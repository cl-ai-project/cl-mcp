;;;; specs/path-fixtures.lisp
;;;;
;;;; Scratch filesystems and read-policy checks for the read-path properties
;;;; (specs/paths.lisp) and their fixed cases (tests/path-specs-test.lisp).
;;;; Needs no cl-spec, so the fixed cases run in the default suite as well.
;;;;
;;;; Descriptors are plain, printable data -- regions, name parts, spellings,
;;;; link topologies -- and drawing one touches nothing: no filesystem, no
;;;; ASDF, no random state but CL:RANDOM, which cl-spec binds from the run's
;;;; seed.  CALL-WITH-READ-FIXTURE builds a fresh scratch tree from
;;;; descriptors, binds *PROJECT-ROOT* around its body only, may register one
;;;; ASDF system of its own, and removes everything it created on the way out:
;;;;
;;;;   <tmp>/cl-mcp-read-spec-<pid>-<serial>-<time>/
;;;;     project/            the project root, or the target of project-alias
;;;;     project-alias       a symlink to project/, when a case asks for one
;;;;     dependency/         with its own .asd; registered only when asked
;;;;     outside/            outside the project, never registered
;;;;     project-other/      shares project/'s name as a string prefix only
;;;;
;;;; Temporary names come from the process id, a counter and the clock, never
;;;; from CL:RANDOM, so they neither depend on nor disturb a run's seed.  The
;;;; same seed therefore rebuilds the same descriptors and topology, under a
;;;; different scratch path; it does not restore inodes or absolute paths.
;;;;
;;;; Cleanup removes exactly what was created, newest first, by unlinking files
;;;; and links and removing directories one by one.  It never recurses and never
;;;; follows a link.  A cleanup failure after a normal exit signals
;;;; READ-FIXTURE-CLEANUP-ERROR; after an error it is reported on
;;;; *ERROR-OUTPUT* and the original error goes on.

(defpackage #:cl-mcp/specs/path-fixtures
  (:use #:cl)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/utils/paths
                #:allowed-read-path
                #:resolve-readable-path)
  (:export #:draw-place
           #:draw-project-read-case
           #:draw-dependency-read-case
           #:draw-denied-read-case
           #:draw-link-read-case
           #:read-fixture
           #:read-fixture-scratch
           #:read-fixture-system-name
           #:read-fixture-registered
           #:read-fixture-root
           #:call-with-read-fixture
           #:with-read-fixture
           #:region-native
           #:place-native
           #:access-argument
           #:expected-target
           #:fixture-symlink
           #:register-dependency
           #:unregister-dependency
           #:expected-read-decision
           #:read-denial-p
           #:call-denied-p
           #:same-target-p
           #:both-allow-as-p
           #:both-deny-p
           #:environment-problems
           #:read-fixture-environment-error
           #:read-fixture-cleanup-error
           #:read-fixture-cleanup-error-failures))

(in-package #:cl-mcp/specs/path-fixtures)

;;; ------------------------------------------------------------------------
;;; Descriptors

(defparameter *regions* '(:project :dependency :outside :project-other)
  "The four regions of a scratch tree.")

(defparameter *name-parts*
  (list "src" "doc" "a b" "日本語" "x[1]" "v[old] 2" "データ.d")
  "Directory and file name stems: plain, with a space, Japanese, with a dot,
and with brackets, which the Common Lisp pathname reader -- not the filesystem
-- would read as wild.  Arguments are built natively, never through that
reader.  No stem starts with \"link-\" or is \"decoy\", so none can collide with
a link or a decoy file.")

(defparameter *file-types* (list "lisp" "txt" nil)
  "File types; NIL makes a file without one.")

(defparameter *link-names* (list "link-a" "link b" "link-リンク" "link-[x]")
  "Symlink names.")

(defparameter *spellings* '(:relative :absolute :pathname :detour)
  "How an argument names its target: a string relative to the project root, an
absolute native string, a pathname parsed natively, or a relative string with
./ and a d/../d detour through a real directory.  A detour never passes
through a symlink: after a link, .. means something else to the OS than to a
lexical resolver, and these cases would be claiming they name one file.")

(defun %pick (sequence)
  "Return a random element of SEQUENCE."
  (elt sequence (random (length sequence))))

(defun %chance (percent)
  "Return true with probability PERCENT/100."
  (< (random 100) percent))

(defun draw-place (region &key (directory-percent 25))
  "Return a place in REGION: (:REGION R :DIRS D :NAME N :TYPE T).  With NAME NIL
the place is the directory DIRS itself, the region's root when DIRS is empty;
otherwise it is a regular file in DIRS."
  (let ((dirs (loop repeat (random 3) collect (%pick *name-parts*))))
    (if (%chance directory-percent)
        (list :region region :dirs dirs :name nil :type nil)
        (list :region region :dirs dirs :name (%pick *name-parts*)
              :type (%pick *file-types*)))))

(defun draw-project-read-case ()
  "Return a case reading a file or directory inside the project."
  (list :target (draw-place :project)
        :spelling (%pick *spellings*)
        :root-alias (%chance 30)
        :dependency-registered (%chance 50)
        :must-exist (%chance 70)))

(defun draw-dependency-read-case ()
  "Return a case reading a place in the dependency directory, with a project
file and an unlisted file as controls."
  (list :target (draw-place :dependency)
        :spelling (%pick *spellings*)
        :project-control (draw-place :project :directory-percent 0)
        :outside-control (draw-place (%pick '(:outside :project-other)) :directory-percent 0)
        :must-exist (%chance 70)))

(defun draw-denied-read-case ()
  "Return a case reading a place outside both the project and the dependency."
  (list :target (draw-place (%pick '(:outside :project-other)))
        :spelling (%pick *spellings*)
        :root-alias (%chance 30)
        :dependency-registered (%chance 50)
        :must-exist (%chance 70)))

(defun draw-link-read-case ()
  "Return a case reading a file through a symlink in the project or the
dependency: to the file itself (:FILE) or to its directory (:DIRECTORY).  A
directory link never points at the region it sits in, so no link is its own
ancestor and the tree stays acyclic."
  ;; LET draws in order, so a seed replays the same link.
  (let ((link-region (if (%chance 70) :project :dependency))
        (target (draw-place (%pick *regions*) :directory-percent 0))
        (kind (if (%chance 50) :file :directory)))
    (when (and (eq kind :directory)
               (eq (getf target :region) link-region)
               (null (getf target :dirs)))
      (setf kind :file))
    (list :link (list :region link-region :kind kind :name (%pick *link-names*)
                      :target target)
          :spelling (%pick '(:relative :absolute :pathname))
          :root-alias (%chance 30)
          :dependency-registered (%chance 75)
          :must-exist (%chance 70))))

;;; ------------------------------------------------------------------------
;;; The policy, stated on descriptors alone

(defun %thing-target (thing)
  "Return the place THING reaches: a link's target, or THING itself."
  (if (getf thing :kind) (getf thing :target) thing))

(defun expected-read-decision (thing dependency-registered-p)
  "Return :ALLOWED or :DENIED for reading THING, a place or a link, from where
its descriptor puts the file it reaches -- never from the functions under test.
Inside the project: allowed.  Inside the dependency directory: allowed exactly
while its system is registered.  Anywhere else: denied.  Where a link sits
does not matter; where it leads does."
  (ecase (getf (%thing-target thing) :region)
    (:project :allowed)
    (:dependency (if dependency-registered-p :allowed :denied))
    ((:outside :project-other) :denied)))

;;; ------------------------------------------------------------------------
;;; Fixtures

(defstruct (read-fixture (:constructor %make-read-fixture (scratch system-name)))
  "One scratch tree.  CREATED lists (KIND NATIVE-PATH) newest first."
  (scratch nil :read-only t)
  (system-name nil :read-only t)
  (created '())
  (registered nil)
  (root nil))

(define-condition read-fixture-environment-error (error)
  ((systems :initarg :systems :reader read-fixture-environment-error-systems))
  (:report (lambda (condition stream)
             (format stream "The scratch parent lies inside the source directory of ~
                             registered ASDF system~P ~{~A~^, ~}, so an \"outside\" case ~
                             would be readable for a reason the case does not model."
                     (length (read-fixture-environment-error-systems condition))
                     (read-fixture-environment-error-systems condition))))
  (:documentation "The environment cannot host a read fixture.  Not a verdict
about the functions under test."))

(define-condition read-fixture-cleanup-error (error)
  ((failures :initarg :failures :reader read-fixture-cleanup-error-failures))
  (:report (lambda (condition stream)
             (format stream "Read fixture cleanup failed: ~S"
                     (read-fixture-cleanup-error-failures condition))))
  (:documentation "Something a fixture created could not be removed."))

(defvar *fixture-serial* (list 0)
  "Counter behind fixture names; a cons so SB-EXT:ATOMIC-INCF can bump it.")

(defun %native (pathname)
  "Return PATHNAME's native namestring."
  (uiop:native-namestring pathname))

(defun %directory-pathname (native)
  "Parse the native directory path NATIVE without the pathname reader."
  (uiop:parse-native-namestring native :ensure-directory t))

(defun environment-problems (scratch-parent source-directories)
  "Return the names in SOURCE-DIRECTORIES, a list of (NAME . DIRECTORY), whose
directory contains SCRATCH-PARENT.  Such a system would make every region of a
scratch tree readable, outside/ included."
  (loop for (name . directory) in source-directories
        when (uiop:subpathp scratch-parent directory)
          collect name))

(defun %registered-source-directories ()
  "Return (NAME . DIRECTORY) for every registered ASDF system with a source
directory, resolved when it exists.  Read through ASDF's registry, without
FIND-SYSTEM, so no .asd is reloaded."
  (loop for name in (asdf:registered-systems)
        for system = (asdf:registered-system name)
        for directory = (and system (asdf:system-source-directory system))
        when directory
          collect (cons name (handler-case (truename directory)
                               (file-error () directory)))))

(defun %check-environment ()
  "Signal READ-FIXTURE-ENVIRONMENT-ERROR unless the temporary directory lies
outside every registered system's source directory."
  (let ((problems (environment-problems (truename (uiop:temporary-directory))
                                        (%registered-source-directories))))
    (when problems
      (error 'read-fixture-environment-error :systems problems))))

(defun region-native (fixture region)
  "Return the native directory path of REGION in FIXTURE, ending in /."
  (concatenate 'string (read-fixture-scratch fixture)
               (ecase region
                 (:project "project/")
                 (:dependency "dependency/")
                 (:outside "outside/")
                 (:project-other "project-other/"))))

(defun %place-below-region (place)
  "Return PLACE's path below its region directory, natively: a directory place
ends in /, and the region's root is the empty string."
  (destructuring-bind (&key region dirs name type) place
    (declare (ignore region))
    (format nil "~{~A/~}~@[~A~]~@[.~A~]" dirs name (and name type))))

(defun place-native (fixture place)
  "Return the native absolute path of PLACE in FIXTURE."
  (concatenate 'string (region-native fixture (getf place :region))
               (%place-below-region place)))

(defun %link-native (fixture link)
  "Return the native absolute path of the symlink LINK itself."
  (concatenate 'string (region-native fixture (getf link :region)) (getf link :name)))

(defun %leaf (place)
  "Return the file name of the file PLACE, with its type."
  (format nil "~A~@[.~A~]" (getf place :name) (getf place :type)))

(defun %thing-native (fixture thing)
  "Return the native absolute path an argument uses to reach THING: a place's
own path, or a path through a link.  Directory places lose their trailing
slash, so the functions see a directory named like a file."
  (if (getf thing :kind)
      (let ((link (%link-native fixture thing)))
        (ecase (getf thing :kind)
          (:file link)
          (:directory (format nil "~A/~A" link (%leaf (getf thing :target))))))
      (string-right-trim "/" (place-native fixture thing))))

(defun %from-project (fixture native)
  "Return NATIVE, a path inside the scratch tree, relative to project/."
  (let* ((scratch (read-fixture-scratch fixture))
         (below (subseq native (length scratch))))
    (cond ((string= below "project") ".")
          ((uiop:string-prefix-p "project/" below) (subseq below (length "project/")))
          (t (concatenate 'string "../" below)))))

(defun %detour (relative)
  "Return RELATIVE with ./ in front and its first real directory D visited
twice, as D/../D.  Only for place paths, which pass through no symlink."
  (let* ((segments (uiop:split-string relative :separator "/"))
         (position (position-if (lambda (segment)
                                  (not (member segment '("." "..") :test #'string=)))
                                segments :end (max 0 (1- (length segments))))))
    (format nil "./~{~A~^/~}"
            (if position
                (append (subseq segments 0 position)
                        (list (nth position segments) ".." (nth position segments))
                        (subseq segments (1+ position)))
                segments))))

(defun access-argument (fixture thing spelling)
  "Return a fresh argument naming THING, a place or a link, as SPELLING says:
a string relative to the project root, an absolute native string, a natively
parsed pathname, or a relative string with a detour (places only)."
  (let ((native (%thing-native fixture thing)))
    (ecase spelling
      (:absolute (copy-seq native))
      (:pathname (uiop:parse-native-namestring native))
      (:relative (%from-project fixture native))
      (:detour (if (getf thing :kind)
                   (error "A detour through a link is outside this domain.")
                   (%detour (%from-project fixture native)))))))

(defun expected-target (fixture thing)
  "Return the truename of the file or directory THING reaches, as FIXTURE
created it.  Used to check which object came back, never whether it may."
  (let* ((place (%thing-target thing))
         (native (place-native fixture place)))
    (truename (if (getf place :name)
                  (uiop:parse-native-namestring native)
                  (%directory-pathname native)))))

(defun %record (fixture kind native)
  "Record that FIXTURE created the object KIND at NATIVE."
  (push (list kind native) (read-fixture-created fixture)))

(defun %created-p (fixture native)
  "True when FIXTURE already created NATIVE."
  (find native (read-fixture-created fixture) :key #'second :test #'string=))

(defun %make-directory (fixture native)
  "Create the directory NATIVE unless FIXTURE already has."
  (unless (%created-p fixture native)
    (sb-posix:mkdir native #o700)
    (%record fixture :directory native)))

(defun %make-file (fixture native contents)
  "Create the file NATIVE holding CONTENTS; it must not exist yet."
  (with-open-file (out (uiop:parse-native-namestring native) :direction :output
                                                            :if-exists :error
                                                            :external-format :utf-8)
    (%record fixture :file native)
    (write-string contents out)))

(defun fixture-symlink (fixture link-native target-native)
  "Create a symlink at LINK-NATIVE pointing to TARGET-NATIVE and record it, so
cleanup unlinks the link itself and never what it points to."
  (sb-posix:symlink target-native link-native)
  (%record fixture :link link-native))

(defun %make-place (fixture place)
  "Create PLACE, the directories above it, and a decoy file beside it."
  (let ((directory (region-native fixture (getf place :region))))
    (dolist (part (getf place :dirs))
      (setf directory (concatenate 'string directory part "/"))
      (%make-directory fixture directory))
    (let ((decoy (concatenate 'string directory "decoy.txt")))
      (unless (%created-p fixture decoy)
        (%make-file fixture decoy "decoy")))
    (when (getf place :name)
      (let ((native (place-native fixture place)))
        (unless (%created-p fixture native)
          (%make-file fixture native (%place-below-region place)))))))

(defun %make-link (fixture link)
  "Create LINK: a symlink to its target file, or to that file's directory."
  (let* ((target (getf link :target))
         (destination (ecase (getf link :kind)
                        (:file (place-native fixture target))
                        (:directory (string-right-trim
                                     "/" (place-native fixture
                                                       (list :region (getf target :region)
                                                             :dirs (getf target :dirs)
                                                             :name nil :type nil)))))))
    (fixture-symlink fixture (%link-native fixture link) destination)))

(defun %asd-native (fixture)
  "Return the native path of FIXTURE's .asd, in dependency/ itself."
  (format nil "~A~A.asd" (region-native fixture :dependency)
          (read-fixture-system-name fixture)))

(defun %materialize (fixture places links root-alias)
  "Create FIXTURE's tree: regions, PLACES, LINKS, the dependency's .asd, and the
project-alias link when ROOT-ALIAS; set the root the body will see."
  (%make-directory fixture (read-fixture-scratch fixture))
  (dolist (region *regions*)
    (%make-directory fixture (region-native fixture region))
    (%make-file fixture (concatenate 'string (region-native fixture region) "decoy.txt")
                "decoy"))
  (%make-file fixture (%asd-native fixture)
              (format nil "(asdf:defsystem ~S)~%" (read-fixture-system-name fixture)))
  (dolist (place places)
    (%make-place fixture place))
  (dolist (link links)
    (%make-link fixture link))
  (setf (read-fixture-root fixture)
        (if root-alias
            (let ((alias (concatenate 'string (read-fixture-scratch fixture) "project-alias")))
              (fixture-symlink fixture alias
                               (string-right-trim "/" (region-native fixture :project)))
              (%directory-pathname (concatenate 'string alias "/")))
            (%directory-pathname (region-native fixture :project)))))

(defun register-dependency (fixture)
  "Register FIXTURE's own ASDF system, whose source directory is dependency/,
and check that ASDF now reports exactly that directory."
  (let ((name (read-fixture-system-name fixture)))
    (when (asdf:registered-system name)
      (error "Fixture system ~A is registered already." name))
    (asdf:load-asd (uiop:parse-native-namestring (%asd-native fixture)))
    (setf (read-fixture-registered fixture) t)
    (let ((system (asdf:registered-system name)))
      (unless (and system
                   (string= (%native (truename (asdf:system-source-directory system)))
                            (%native (truename (%directory-pathname
                                                (region-native fixture :dependency))))))
        (error "Fixture system ~A did not register dependency/ as its source directory."
               name)))))

(defun unregister-dependency (fixture)
  "Remove FIXTURE's own ASDF system from the registry, and nothing else."
  (let ((name (read-fixture-system-name fixture)))
    (asdf:clear-system name)
    (when (asdf:registered-system name)
      (error "Fixture system ~A is still registered." name))
    (setf (read-fixture-registered fixture) nil)))

(defun %cleanup (fixture)
  "Unregister FIXTURE's system and remove what it created, newest first, one
object at a time.  Return the failures; an empty list means everything went."
  (let ((failures '()))
    (when (read-fixture-registered fixture)
      (handler-case (unregister-dependency fixture)
        (error (condition) (push (list :unregister (princ-to-string condition)) failures))))
    (loop for (kind native) in (read-fixture-created fixture)
          do (handler-case (ecase kind
                             ((:file :link) (sb-posix:unlink native))
                             (:directory (sb-posix:rmdir native)))
               (error (condition)
                 (push (list kind native (princ-to-string condition)) failures))))
    (setf (read-fixture-created fixture) '())
    (when (probe-file (%directory-pathname (read-fixture-scratch fixture)))
      (push (list :scratch-remains (read-fixture-scratch fixture)) failures))
    (nreverse failures)))

(defun %fresh-fixture ()
  "Return a fixture with a scratch path and a system name nothing uses yet."
  (let ((serial (sb-ext:atomic-incf (car *fixture-serial*)))
        (pid (sb-posix:getpid)))
    (%make-read-fixture
     (format nil "~Acl-mcp-read-spec-~D-~D-~D/" (%native (uiop:temporary-directory))
             pid serial (get-universal-time))
     (format nil "cl-mcp-read-fixture-~D-~D" pid serial))))

(defun call-with-read-fixture (thunk &key places links root-alias register-dependency)
  "Build a scratch tree holding PLACES and LINKS, register its dependency system
when REGISTER-DEPENDENCY, and call THUNK with the fixture while *PROJECT-ROOT*
is bound to project/ -- or to project-alias, a symlink to it, when ROOT-ALIAS.
Everything created is removed afterwards, however THUNK exits.  Signals
READ-FIXTURE-ENVIRONMENT-ERROR, before creating anything, when a registered
system's source directory contains the temporary directory."
  (%check-environment)
  (let ((fixture (%fresh-fixture))
        (normal-exit nil))
    (unwind-protect
         (multiple-value-prog1
             (progn
               (%materialize fixture places links root-alias)
               (when register-dependency
                 (register-dependency fixture))
               (let ((*project-root* (read-fixture-root fixture)))
                 (funcall thunk fixture)))
           (setf normal-exit t))
      (let ((failures (%cleanup fixture)))
        (when failures
          (if normal-exit
              (error 'read-fixture-cleanup-error :failures failures)
              (format *error-output* "~&;; read fixture cleanup failed while unwinding: ~S~%"
                      failures)))))))

(defmacro with-read-fixture ((fixture &rest options) &body body)
  "Run BODY with FIXTURE bound to a fresh read fixture (see CALL-WITH-READ-FIXTURE)."
  `(call-with-read-fixture (lambda (,fixture) ,@body) ,@options))

;;; ------------------------------------------------------------------------
;;; Checks on the functions under test

(defun read-denial-p (condition)
  "True when CONDITION is RESOLVE-READABLE-PATH's refusal: a SIMPLE-ERROR whose
format control says the path is outside the project root.  Matches that phrase
of the control string, not the rendered message, which embeds the paths."
  (and (typep condition 'simple-error)
       (let ((control (simple-condition-format-control condition)))
         (and (stringp control)
              (search "outside project root" control)
              t))))

(defun call-denied-p (thunk)
  "Call THUNK and return true when it signalled a read refusal, NIL when it
returned.  Handles that refusal only; any other condition goes on."
  (block denied
    (handler-bind ((simple-error (lambda (condition)
                                   (when (read-denial-p condition)
                                     (return-from denied t)))))
      (funcall thunk))
    nil))

(defun same-target-p (result expected)
  "True when RESULT is an absolute pathname naming the same file as EXPECTED."
  (and (pathnamep result)
       (uiop:absolute-pathname-p result)
       (string= (%native result) (%native expected))))

(defun both-allow-as-p (fixture thing spelling &key (must-exist t))
  "True when ALLOWED-READ-PATH and RESOLVE-READABLE-PATH, each given a fresh
argument naming THING, both return the object THING reaches.  Each is compared
with the expected target separately."
  (let ((expected (expected-target fixture thing)))
    (and (same-target-p (allowed-read-path (access-argument fixture thing spelling))
                        expected)
         (same-target-p (resolve-readable-path (access-argument fixture thing spelling)
                                               :must-exist must-exist)
                        expected))))

(defun both-deny-p (fixture thing spelling &key (must-exist t))
  "True when ALLOWED-READ-PATH returns NIL for THING and RESOLVE-READABLE-PATH
signals its read refusal.  Only that refusal counts; any other condition,
from the call or elsewhere, propagates."
  (and (null (allowed-read-path (access-argument fixture thing spelling)))
       (call-denied-p (lambda ()
                        (resolve-readable-path (access-argument fixture thing spelling)
                                               :must-exist must-exist)))))
