;;;; Verify generated scaffolds before allowing their replacement.

(defpackage #:cl-mcp/src/project-scaffold-ownership
  (:use #:cl)
  (:import-from #:ironclad)
  (:import-from #:cl-mcp/src/fs
                #:*fs-read-max-bytes*
                #:fs-read-file
                #:fs-read-source-octets)
  (:import-from #:cl-mcp/src/project-scaffold-core
                #:*scaffold-marker-file*
                #:scaffold-marker-content)
  (:import-from #:cl-mcp/src/utils/paths
                #:native-path-namestring)
  (:export #:scaffold-entry-kind
           #:signed-scaffold-marker
           #:scaffold-replacement-problem))

(in-package #:cl-mcp/src/project-scaffold-ownership)

(defvar *scaffold-signing-key* nil
  "Private key for ownership receipts issued by this server process.")

(defconstant +marker-limit+ 16384)

(defparameter *signature-prefix* ";;;; cl-mcp scaffold HMAC-SHA256 ")

(defun scaffold-entry-kind (path)
  "Classify PATH without following its last entry.
:FILE means a regular file with one link. :OTHER covers links and special files."
  (let ((native (if (pathnamep path) (native-path-namestring path) path)))
    (handler-case
        (let* ((stat (sb-posix:lstat (string-right-trim "/" native)))
               (type (logand (sb-posix:stat-mode stat) sb-posix:s-ifmt)))
          (cond ((= type sb-posix:s-ifdir) :directory)
                ((and (= type sb-posix:s-ifreg) (= 1 (sb-posix:stat-nlink stat))) :file)
                (t :other)))
      (sb-posix:syscall-error (condition)
        (unless (= (sb-posix:syscall-errno condition) sb-posix:enoent)
          (error condition))))))

(defun %file-evidence (native &optional expected-size)
  "Return the byte count and SHA-256 of a generated file. Skip hashing other sizes."
  (when (or (null expected-size)
            (= expected-size (sb-posix:stat-size (sb-posix:lstat native))))
    ;; Use the file tools' read policy while retaining exact bytes for the digest.
    (let ((bytes (fs-read-source-octets (uiop:parse-native-namestring native))))
      (when (or (null expected-size) (= (length bytes) expected-size))
        (list (length bytes)
              (ironclad:byte-array-to-hex-string
               (ironclad:digest-sequence :sha256 bytes)))))))

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
      (multiple-value-bind (text truncated size remaining)
          (fs-read-file marker :limit (min (1+ +marker-limit+) *fs-read-max-bytes*))
        (declare (ignore truncated size))
        (let ((newline (position #\Newline text)))
          (when (and (not remaining)
                     (<= (length text) +marker-limit+)
                     (eql newline (+ 64 (length *signature-prefix*)))
                     (uiop:string-prefix-p *signature-prefix* text))
            (let ((signature (ignore-errors
                               (ironclad:hex-string-to-byte-array
                                (subseq text (length *signature-prefix*) newline))))
                  (manifest (subseq text (1+ newline))))
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
    ((or file-error stream-error simple-error sb-posix:syscall-error) ()
      "the existing scaffold could not be inspected safely")))
