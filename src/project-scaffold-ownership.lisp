;;;; Verify generated scaffolds before allowing their replacement.

(defpackage #:cl-mcp/src/project-scaffold-ownership
  (:use #:cl)
  (:import-from #:ironclad)
  (:import-from #:cl-mcp/src/project-scaffold-core
                #:*scaffold-marker-file*
                #:scaffold-marker-content)
  (:import-from #:cl-mcp/src/utils/paths
                #:native-path-namestring)
  (:export #:scaffold-entry-kind
           #:signed-scaffold-marker
           #:discard-scaffold-backup
           #:scaffold-replacement-problem))

(in-package #:cl-mcp/src/project-scaffold-ownership)

(defvar *scaffold-signing-key* nil
  "Private key for ownership receipts issued by this server process.")

(defconstant +marker-limit+ 16384)

(defparameter *signature-prefix* ";;;; cl-mcp scaffold HMAC-SHA256 ")

(defun scaffold-entry-kind (path)
  "Return :DIRECTORY, :FILE, :OTHER or NIL for PATH without following its last entry."
  (let ((native (if (pathnamep path) (native-path-namestring path) path)))
    (handler-case
        (let* ((stat (sb-posix:lstat (string-right-trim "/" native)))
               (type (logand (sb-posix:stat-mode stat) sb-posix:s-ifmt)))
          (cond ((= type sb-posix:s-ifdir) :directory)
                ((= type sb-posix:s-ifreg) :file)
                (t :other)))
      (sb-posix:syscall-error (condition)
        (unless (= (sb-posix:syscall-errno condition) sb-posix:enoent)
          (error condition))))))

(defun %file-evidence (native &optional expected-size)
  "Return the byte count and SHA-256 of a generated file. Skip hashing other sizes."
  (with-open-file (input (uiop:parse-native-namestring native)
                        :element-type '(unsigned-byte 8))
    (let ((size (file-length input)))
      (when (or (null expected-size) (= size expected-size))
        (list size (ironclad:byte-array-to-hex-string
                    (ironclad:digest-stream :sha256 input)))))))

(defun %signature (directory location manifest)
  "Authenticate MANIFEST for DIRECTORY and its original physical LOCATION."
  (let* ((stat (sb-posix:lstat (string-right-trim "/" (native-path-namestring directory))))
         (hmac (ironclad:make-hmac *scaffold-signing-key* :sha256))
         (identity (format nil "~D/~D" (sb-posix:stat-dev stat) (sb-posix:stat-ino stat)))
         (separator (string (code-char 0))))
    (ironclad:update-hmac
     hmac (sb-ext:string-to-octets
           (concatenate 'string identity separator location separator manifest)
           :external-format :utf-8))
    (ironclad:hmac-digest hmac)))

(defun signed-scaffold-marker (directory location name files)
  "Issue a receipt for FILES written to DIRECTORY and destined for LOCATION."
  ;; The caller serializes scaffold generation, including key initialization.
  (unless *scaffold-signing-key*
    (setf *scaffold-signing-key* (ironclad:random-data 32)))
  (let* ((root (native-path-namestring directory))
         (entries (loop for file in files
                        collect (cons file (%file-evidence (concatenate 'string root file)))))
         (manifest (scaffold-marker-content name files entries)))
    (format nil "~A~A~%~A" *signature-prefix*
            (ironclad:byte-array-to-hex-string (%signature directory location manifest)) manifest)))

(defun %verified-manifest (directory location)
  "Read the manifest at DIRECTORY only after authenticating its text for LOCATION."
  (let ((marker (merge-pathnames *scaffold-marker-file* directory)))
    (when (and *scaffold-signing-key* (eq :file (scaffold-entry-kind marker)))
      (with-open-file (input marker :external-format :utf-8)
        (let* ((buffer (make-string (1+ +marker-limit+)))
               (end (read-sequence buffer input))
               (newline (position #\Newline buffer :end end)))
          (when (and (< end (length buffer))
                     (eql newline (+ 64 (length *signature-prefix*)))
                     (uiop:string-prefix-p *signature-prefix* buffer))
            (let ((signature (ignore-errors
                               (ironclad:hex-string-to-byte-array
                                (subseq buffer (length *signature-prefix*) newline))))
                  (manifest (subseq buffer (1+ newline) end)))
              (when (and signature
                         (ironclad:constant-time-equal
                          signature (%signature directory location manifest)))
                (with-standard-io-syntax
                  (let ((*read-eval* nil))
                    (read-from-string manifest)))))))))))

(defun %directory-names (native)
  "List the raw entry names of NATIVE, including hidden files and links."
  (let ((directory (sb-posix:opendir native)))
    (unwind-protect
         (loop with names = nil
               for entry = (sb-posix:readdir directory)
               until (sb-alien:null-alien entry)
               do (let ((name (sb-posix:dirent-name entry)))
                    (unless (member name '("." "..") :test #'string=)
                      (push name names)))
               finally (return names))
      (sb-posix:closedir directory))))

(defun %expected-entries (manifest)
  "Index the generated files, their containing directories, and the receipt."
  (let ((pending (make-hash-table :test #'equal)))
    (setf (gethash *scaffold-marker-file* pending) :marker)
    (dolist (entry (getf manifest :entries))
      (let ((file (car entry)))
        (setf (gethash file pending) (cdr entry))
        (loop for slash = (position #\/ file) then (position #\/ file :start (1+ slash))
              while slash
              do (setf (gethash (subseq file 0 slash) pending) :directory))))
    pending))

(defun %changed-entries (directory manifest)
  "List missing, added, or changed entries. Visit only expected real directories."
  (let ((pending (%expected-entries manifest))
        (root (native-path-namestring directory))
        (changed nil))
    (labels ((walk (prefix)
               (dolist (name (%directory-names (concatenate 'string root prefix)))
                 (let* ((relative (concatenate 'string prefix name))
                        (expected (gethash relative pending))
                        (native (concatenate 'string root relative)))
                   (remhash relative pending)
                   (cond
                     ((null expected) (push relative changed))
                     ((eq expected :directory)
                      (if (eq :directory (scaffold-entry-kind native))
                          (walk (concatenate 'string relative "/"))
                          (push relative changed)))
                     ((not (eq :file (scaffold-entry-kind native)))
                      (push relative changed))
                     ((and (listp expected)
                           (not (equal expected (%file-evidence native (first expected)))))
                      (push relative changed)))))))
      (walk ""))
    (maphash (lambda (name evidence)
               (declare (ignore evidence))
               (push name changed))
             pending)
    (sort changed #'string<)))

(defun scaffold-replacement-problem (directory &optional location)
  "Return why DIRECTORY cannot be replaced, or NIL if it is an unchanged scaffold.
LOCATION is its original physical namestring when DIRECTORY has been moved aside."
  (handler-case
      (if (eq :directory (scaffold-entry-kind directory))
          (let* ((location (or location (native-path-namestring (truename directory))))
                 (manifest (%verified-manifest directory location)))
            (if manifest
                (let ((changed (%changed-entries directory manifest)))
                  (when changed
                    (format nil "scaffold has added, missing, or changed entries (~{~S~^, ~})"
                            changed)))
                "this server cannot verify ownership. Use a new scaffold name."))
          "the existing target is not a real directory")
    ((or file-error stream-error sb-posix:syscall-error) ()
      "the existing scaffold could not be inspected safely")))

(defun discard-scaffold-backup (directory location)
  "Remove verified generated entries from DIRECTORY. Return true once it is gone.
Remove directories only when empty, so a new entry that appears during
cleanup is kept and the caller can report the backup."
  (let ((manifest (%verified-manifest directory location)))
    (when (and manifest (null (%changed-entries directory manifest)))
      (let ((root (native-path-namestring directory))
            (folders nil))
        (maphash
         (lambda (relative expected)
           (let ((native (concatenate 'string root relative)))
             (cond ((eq expected :directory) (push native folders))
                   ((listp expected)
                    (unless (and (eq :file (scaffold-entry-kind native))
                                 (equal expected (%file-evidence native (first expected))))
                      (return-from discard-scaffold-backup nil))
                    (sb-posix:unlink native)))))
         (%expected-entries manifest))
        (dolist (folder (sort folders #'> :key #'length))
          (sb-posix:rmdir folder))
        (sb-posix:unlink (concatenate 'string root *scaffold-marker-file*))
        (sb-posix:rmdir root)
        t))))
