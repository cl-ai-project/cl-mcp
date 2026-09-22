;;;; specs/write-fixtures.lisp
;;;;
;;;; Write cases, the write policy stated on them, and observed checks of
;;;; ENSURE-WRITE-PATH and FS-WRITE-FILE, for the write-path properties
;;;; (specs/write-paths.lisp) and their fixed cases
;;;; (tests/write-path-specs-test.lisp).  Needs no cl-spec, so the fixed cases
;;;; run in the default suite as well.
;;;;
;;;; Built on specs/path-fixtures.lisp: the same scratch tree, regions, names
;;;; and links.  A write target says where a write should land:
;;;;
;;;;   (:REGION R :DIRS D :NEW-DIRS N :NAME NAME :TYPE TYPE :LEAF-EXISTS B)
;;;;
;;;; The directories D below region R exist before the call; the directories N
;;;; below them do not, and neither does the file NAME.TYPE unless B, which
;;;; only a target without N can be.  A case reaches its target directly, or
;;;; through one link at the root of project/ or outside/: a directory link to
;;;; R/D, or a file link to the existing file itself.
;;;;
;;;; The policy, decided from the descriptors alone:
;;;;
;;;;   an absolute argument                             -> refused
;;;;   a target outside project/                        -> refused, whether or
;;;;                                                       not the dependency is
;;;;                                                       registered
;;;;   a .. right after a link or a name that does not  -> refused: the OS and
;;;;   exist yet                                           a lexical reading
;;;;                                                       disagree on it
;;;;   anything else                                    -> allowed, as the real
;;;;                                                       path of the target
;;;;
;;;; A check makes one call and lists the whole scratch tree, without following
;;;; any link, just before and just after it; it judges from those two listings
;;;; while the fixture still exists.  Fixtures built here adopt new entries, so
;;;; whatever a wrong implementation writes inside the tree is removed on the
;;;; way out, after it has been observed.

(defpackage #:cl-mcp/specs/write-fixtures
  (:use #:cl)
  (:import-from #:cl-mcp/src/log
                #:*log-level*)
  (:import-from #:cl-mcp/src/utils/paths
                #:ensure-write-path
                #:write-path-refused
                #:write-path-refused-reason)
  (:import-from #:cl-mcp/src/fs
                #:fs-write-file)
  (:import-from #:cl-mcp/specs/path-fixtures
                #:*regions*
                #:*name-parts*
                #:*file-types*
                #:*link-names*
                #:read-fixture-scratch
                #:call-with-read-fixture
                #:place-native
                #:detour-spelling
                #:scratch-snapshot
                #:snapshot-changes)
  (:export #:*write-spellings*
           #:*write-contents*
           #:draw-write-target
           #:draw-project-write-case
           #:draw-refused-write-case
           #:draw-link-write-case
           #:draw-write-case
           #:write-target
           #:write-case
           #:write-case-places
           #:call-with-write-fixture
           #:with-write-fixture
           #:write-argument
           #:expected-write-decision
           #:expected-write-native
           #:expected-write-entries
           #:observe-validator
           #:observe-writer
           #:no-changes-p
           #:same-entries-p
           #:validator-allows-as-p
           #:validator-refuses-p
           #:validator-agrees-p
           #:writer-agrees-p
           #:*safe-spellings*
           #:draw-safe-spelling-case
           #:safe-spelling-variants
           #:observe-spellings
           #:safe-spelling-coverage-p
           #:explain-safe-spellings))

(in-package #:cl-mcp/specs/write-fixtures)

;;; ------------------------------------------------------------------------
;;; Descriptors

(defparameter *write-spellings* '(:relative :relative-pathname :detour :absolute)
  "How an argument names its target: a string relative to the project root, the
same parsed natively into a pathname, the same with ./ in front and its first
directory D visited as D/../D, or an absolute native string.")

(defparameter *write-contents* (format nil "written by a write check~%")
  "What the writer checks write.  ASCII, so its bytes do not depend on the
default external format, and never equal to what a fixture puts in a file.")

(defun %pick (sequence)
  "Return a random element of SEQUENCE."
  (elt sequence (random (length sequence))))

(defun %chance (percent)
  "Return true with probability PERCENT/100."
  (< (random 100) percent))

(defun write-target (region dirs new-dirs name &optional type leaf-exists)
  "Return a write target; see the file header."
  (list :region region :dirs dirs :new-dirs new-dirs :name name :type type
        :leaf-exists (and leaf-exists (null new-dirs))))

(defun write-case (target &key link (spelling :relative) root-alias dependency-registered)
  "Return a write case: TARGET, reached through LINK, a list (:REGION R :KIND
K :NAME N) with R :PROJECT or :OUTSIDE and K :DIRECTORY or :FILE, when given."
  (list :target target :link link :spelling spelling :root-alias root-alias
        :dependency-registered dependency-registered))

(defun draw-write-target (region &key (new-dirs-percent 40) (existing-percent 30))
  "Return a write target in REGION: zero to two existing directories, then, at
NEW-DIRS-PERCENT, one or two that do not exist; the leaf exists at
EXISTING-PERCENT when no new directory comes before it."
  ;; LET* draws in order, so a seed replays the same target.
  (let* ((dirs (loop repeat (random 3) collect (%pick *name-parts*)))
         (new-dirs (when (%chance new-dirs-percent)
                     (loop repeat (1+ (random 2)) collect (%pick *name-parts*))))
         (name (%pick *name-parts*))
         (type (%pick *file-types*))
         (leaf-exists (and (null new-dirs) (%chance existing-percent))))
    (write-target region dirs new-dirs name type leaf-exists)))

(defun draw-project-write-case ()
  "Return a case writing inside the project, reached directly."
  ;; LET evaluates its forms in order too, so a seed replays the same case.
  (let ((target (draw-write-target :project))
        (spelling (%pick *write-spellings*))
        (root-alias (%chance 30))
        (registered (%chance 50)))
    (write-case target :spelling spelling :root-alias root-alias
                       :dependency-registered registered)))

(defun draw-refused-write-case ()
  "Return a case writing into the dependency, outside/ or project-other/,
reached directly, with a project target as a control."
  (let ((target (draw-write-target (%pick '(:dependency :outside :project-other))))
        (spelling (%pick *write-spellings*))
        (root-alias (%chance 30))
        (control (draw-write-target :project)))
    (append (write-case target :spelling spelling :root-alias root-alias)
            (list :project-control control))))

(defun draw-link-write-case ()
  "Return a case writing through one link at the root of project/ or outside/:
a directory link to the target's directory, followed by any new directories
and the leaf, or a file link to an existing target file."
  (let* ((link-region (if (%chance 75) :project :outside))
         (kind (if (%chance 25) :file :directory))
         (target (if (eq kind :file)
                     (draw-write-target (%pick *regions*)
                                        :new-dirs-percent 0 :existing-percent 100)
                     (draw-write-target (%pick *regions*))))
         (name (%pick *link-names*))
         (spelling (%pick *write-spellings*))
         (root-alias (%chance 30))
         (registered (%chance 50)))
    (write-case target :link (list :region link-region :kind kind :name name)
                       :spelling spelling :root-alias root-alias
                       :dependency-registered registered)))

(defun draw-write-case ()
  "Return a direct or a linked case, in any region."
  (let ((shape (random 3)))
    (ecase shape
      (0 (draw-project-write-case))
      (1 (draw-refused-write-case))
      (2 (draw-link-write-case)))))

;;; ------------------------------------------------------------------------
;;; The argument, and the policy stated on descriptors alone

(defun %region-directory (region)
  "Return the name of REGION's directory in a scratch tree."
  (ecase region
    (:project "project")
    (:dependency "dependency")
    (:outside "outside")
    (:project-other "project-other")))

(defun %leaf (target)
  "Return the file name TARGET writes, with its type."
  (format nil "~A~@[.~A~]" (getf target :name) (getf target :type)))

(defun %from-project (region)
  "Return the segments leading from project/ to REGION's directory."
  (if (eq region :project)
      '()
      (list (cons ".." :up) (cons (%region-directory region) :existing))))

(defun write-segments (case)
  "Return the segments of CASE's relative argument, from project/ on, as
\(NAME . KIND): KIND is :UP for .., :EXISTING for a real directory, :LINK for the
directory link, :NEW for a directory that does not exist yet and :LEAF for the
last segment, which is the file link itself when the case has one."
  (let* ((target (getf case :target))
         (link (getf case :link))
         (new (mapcar (lambda (name) (cons name :new)) (getf target :new-dirs)))
         (leaf (list (cons (%leaf target) :leaf))))
    (cond ((null link)
           (append (%from-project (getf target :region))
                   (mapcar (lambda (name) (cons name :existing)) (getf target :dirs))
                   new leaf))
          ((eq (getf link :kind) :file)
           (append (%from-project (getf link :region))
                   (list (cons (getf link :name) :leaf))))
          (t
           (append (%from-project (getf link :region))
                   (list (cons (getf link :name) :link))
                   new leaf)))))

(defun %detour-kind (segments)
  "Return the kind of the segment DETOUR-SPELLING visits twice: the first that is
not .., the last segment excepted.  NIL when there is none."
  (cdr (find :up (butlast segments) :key #'cdr :test-not #'eq)))

(defun expected-write-decision (case)
  "Return :ALLOWED or :REFUSED for CASE, from its descriptors alone -- never
from the functions under test.  An absolute argument is refused.  A target
outside project/ is refused, however it is reached and whether or not the
dependency is registered.  A detour that puts .. right after a link or after a
directory that does not exist yet is refused.  Everything else is allowed."
  (cond ((eq (getf case :spelling) :absolute) :refused)
        ((not (eq (getf (getf case :target) :region) :project)) :refused)
        ((and (eq (getf case :spelling) :detour)
              (member (%detour-kind (write-segments case)) '(:link :new)))
         :refused)
        (t :allowed)))

(defun write-argument (fixture case)
  "Return a fresh argument for CASE in FIXTURE, spelled as CASE says."
  (let* ((segments (write-segments case))
         (relative (format nil "~{~A~^/~}" (mapcar #'car segments))))
    (ecase (getf case :spelling)
      (:relative relative)
      (:relative-pathname (uiop:parse-native-namestring relative))
      (:detour (detour-spelling relative))
      (:absolute
       ;; Only a leading .. can occur, so dropping the last directory on each
       ;; one gives the path without any.
       (let ((below (list "project")))
         (loop for (name . kind) in segments
               do (if (eq kind :up)
                      (setf below (butlast below))
                      (setf below (append below (list name)))))
         (format nil "~A~{~A~^/~}" (read-fixture-scratch fixture) below))))))

(defun %directory-place (target)
  "Return the place of TARGET's existing directory."
  (list :region (getf target :region) :dirs (getf target :dirs) :name nil :type nil))

(defun %file-place (target)
  "Return the place of TARGET's file, as if it existed."
  (list :region (getf target :region) :dirs (getf target :dirs)
        :name (getf target :name) :type (getf target :type)))

(defun expected-write-native (fixture case)
  "Return the native path an allowed write for CASE must land on: the truename
of the target's existing directory, then its new directories and its leaf."
  (let ((target (getf case :target)))
    (format nil "~A~{~A/~}~A"
            (uiop:native-namestring
             (truename (uiop:parse-native-namestring
                        (place-native fixture (%directory-place target))
                        :ensure-directory t)))
            (getf target :new-dirs)
            (%leaf target))))

(defun expected-write-entries (case contents)
  "Return two values for an allowed write of CONTENTS for CASE, as snapshot
entries relative to the scratch root: the entries it must add -- each new
directory, then the file unless it existed -- and the entries it must change,
the file with its new bytes when it existed."
  (let* ((target (getf case :target))
         (octets (sb-ext:string-to-octets contents :external-format :utf-8))
         (directory (format nil "~A/~{~A/~}" (%region-directory (getf target :region))
                            (getf target :dirs)))
         (added '()))
    (dolist (name (getf target :new-dirs))
      (setf directory (concatenate 'string directory name "/"))
      (push (list directory :directory nil) added))
    (let ((file (list (concatenate 'string directory (%leaf target)) :file octets)))
      (if (getf target :leaf-exists)
          (values (nreverse added) (list file))
          (values (nreverse (cons file added)) '())))))

;;; ------------------------------------------------------------------------
;;; Fixtures

(defun write-case-places (target)
  "Return the places a fixture creates for TARGET: its existing directory, and
its file when that exists."
  (if (getf target :leaf-exists)
      (list (%directory-place target) (%file-place target))
      (list (%directory-place target))))

(defun call-with-write-fixture (thunk case &key extra-targets)
  "Call THUNK with a read fixture (specs/path-fixtures.lisp) holding CASE's
target, its link and EXTRA-TARGETS, with CASE's root alias and registration,
that adopts whatever the body writes into its tree."
  (let ((target (getf case :target))
        (link (getf case :link)))
    (call-with-read-fixture
     thunk
     :places (loop for each in (cons target extra-targets)
                   append (write-case-places each))
     :links (when link
              (list (list :region (getf link :region) :kind (getf link :kind)
                          :name (getf link :name) :target (%file-place target))))
     :root-alias (getf case :root-alias)
     :register-dependency (getf case :dependency-registered)
     :adopt-new-entries t)))

(defmacro with-write-fixture ((fixture case &rest options) &body body)
  "Run BODY with FIXTURE bound to a fresh fixture for CASE (see
CALL-WITH-WRITE-FIXTURE)."
  `(call-with-write-fixture (lambda (,fixture) ,@body) ,case ,@options))

;;; ------------------------------------------------------------------------
;;; Observed checks

(defun observe-validator (fixture argument)
  "Call ENSURE-WRITE-PATH on ARGUMENT once, listing FIXTURE's tree just before
and just after.  Return (:RETURNED PATHNAME :CHANGES C) or (:REFUSED REASON
:CHANGES C), C being SNAPSHOT-CHANGES's report.  Only WRITE-PATH-REFUSED from
that one call is handled; any other condition goes on."
  ;; LET evaluates its forms in order: before, the call, after.
  (let ((before (scratch-snapshot fixture))
        (outcome (handler-case (list :returned (ensure-write-path argument))
                   (write-path-refused (condition)
                     (list :refused (write-path-refused-reason condition)))))
        (after (scratch-snapshot fixture)))
    (append outcome (list :changes (snapshot-changes before after)))))

(defun observe-writer (fixture argument contents)
  "Call FS-WRITE-FILE on ARGUMENT and CONTENTS once, listing FIXTURE's tree just
before and just after.  Return (:WROTE T :CHANGES C) or (:REFUSED REASON
:CHANGES C).  Only WRITE-PATH-REFUSED from that one call is handled; any other
condition goes on.  Debug logging is off for the call, which does not change
what it writes."
  ;; LET evaluates its forms in order: before, the call, after.
  (let ((before (scratch-snapshot fixture))
        (outcome (handler-case (let ((*log-level* :warn))
                                 (fs-write-file argument contents)
                                 (list :wrote t))
                   (write-path-refused (condition)
                     (list :refused (write-path-refused-reason condition)))))
        (after (scratch-snapshot fixture)))
    (append outcome (list :changes (snapshot-changes before after)))))

(defun no-changes-p (changes)
  "True when SNAPSHOT-CHANGES's report CHANGES lists nothing."
  (and (null (getf changes :added))
       (null (getf changes :removed))
       (null (getf changes :changed))))

(defun validator-allows-as-p (fixture case)
  "True when ENSURE-WRITE-PATH returns, for a fresh argument for CASE, an
absolute pathname naming exactly EXPECTED-WRITE-NATIVE, and creates nothing."
  (let* ((observed (observe-validator fixture (write-argument fixture case)))
         (result (getf observed :returned)))
    (and (pathnamep result)
         (uiop:absolute-pathname-p result)
         (string= (uiop:native-namestring result) (expected-write-native fixture case))
         (no-changes-p (getf observed :changes)))))

(defun validator-refuses-p (fixture case)
  "True when ENSURE-WRITE-PATH signals WRITE-PATH-REFUSED for a fresh argument
for CASE and creates nothing."
  (let ((observed (observe-validator fixture (write-argument fixture case))))
    (and (getf observed :refused)
         (no-changes-p (getf observed :changes))
         t)))

(defun validator-agrees-p (fixture case)
  "True when ENSURE-WRITE-PATH does for CASE what EXPECTED-WRITE-DECISION says."
  (ecase (expected-write-decision case)
    (:allowed (validator-allows-as-p fixture case))
    (:refused (validator-refuses-p fixture case))))

(defun same-entries-p (actual expected)
  "True when the snapshot entries ACTUAL and EXPECTED hold the same paths, kinds
and details, in any order."
  (and (= (length actual) (length expected))
       (every (lambda (entry)
                (let ((match (find (first entry) actual :key #'first :test #'string=)))
                  (and match
                       (eq (second match) (second entry))
                       (equalp (third match) (third entry)))))
              expected)))

(defun writer-agrees-p (fixture case &optional (contents *write-contents*))
  "True when FS-WRITE-FILE, given a fresh argument for CASE and CONTENTS, does
what EXPECTED-WRITE-DECISION says: an allowed write adds exactly the expected
directories and file, or changes exactly the existing file, and nothing else in
the whole tree; a refused one signals WRITE-PATH-REFUSED and changes nothing."
  (let* ((observed (observe-writer fixture (write-argument fixture case) contents))
         (changes (getf observed :changes)))
    (ecase (expected-write-decision case)
      (:allowed
       (multiple-value-bind (added changed) (expected-write-entries case contents)
         (and (getf observed :wrote)
              (same-entries-p (getf changes :added) added)
              (null (getf changes :removed))
              (same-entries-p (mapcar #'second (getf changes :changed)) changed))))
      (:refused
       (and (getf observed :refused)
            (no-changes-p changes)
            t)))))

;;; ------------------------------------------------------------------------
;;; Safe spellings of one allowed target
;;;
;;; For WRITE-PRESERVES-SAFE-SPELLINGS: the same allowed case, spelled in ways
;;; that name the same file, must come back as the same real path.  The base
;;; spelling is checked against EXPECTED-WRITE-NATIVE, so a function that
;;; refuses everything, or sends every spelling to one wrong file, cannot pass
;;; by agreeing with itself.

(defparameter *safe-spellings*
  '(:relative-string :relative-pathname :leading-dot :repeated-separator)
  "The spellings WRITE-PRESERVES-SAFE-SPELLINGS compares, base first: the
relative native string S; S parsed natively into a relative pathname; ./ and
then S; and S with one separator doubled, or .// and then S when S has none.
POSIX pathname resolution ignores a . segment and a repeated separator
anywhere but at the very start, so all four name one file.  A leading / or //,
a trailing /, and .. are not among them.")

(defun draw-safe-spelling-case ()
  "Return a case the policy allows in every safe spelling: a target in project/
-- existing directories, then new ones, then a new or existing file -- reached
directly or through one directory link at the root of project/ that leads to
the target's existing directory, under the project root or its alias.  Its
relative argument has no .. and no trailing slash.  :DOUBLED-SEPARATOR says
which separator of that argument the repeated-separator spelling doubles,
counting from 0, or is NIL when the argument has none."
  ;; LET* draws in order, so a seed replays the same case.
  (let* ((target (draw-write-target :project))
         (link (when (%chance 40)
                 (list :region :project :kind :directory :name (%pick *link-names*))))
         (root-alias (%chance 30))
         (case (write-case target :link link :root-alias root-alias))
         (separators (1- (length (write-segments case)))))
    (append case (list :doubled-separator (and (plusp separators) (random separators))))))

(defun %double-separator (base index)
  "Return a fresh copy of BASE with its separator number INDEX, counting from 0,
doubled; or .// and then BASE when INDEX is NIL."
  (if index
      (let ((position (loop for i from 0 below (length base)
                            count (char= (char base i) #\/) into seen
                            when (and (char= (char base i) #\/) (= seen (1+ index)))
                              return i)))
        (assert position () "~S has no separator number ~D." base index)
        (concatenate 'string (subseq base 0 position) "/" (subseq base position)))
      (concatenate 'string ".//" base)))

(defun safe-spelling-variants (case)
  "Return ((SPELLING . ARGUMENT) ...) for CASE, one per *SAFE-SPELLINGS*, base
first.  The base is CASE's relative argument.  Every other argument is built
from it by string operations or a native parse only, never by resolving it,
so a difference the function under test must cope with is still there when it
is called.  Each argument is a fresh object.  Signals unless the base is
relative, ends in a file name, and has no empty, . or .. segment."
  (let* ((base (write-argument nil (list :target (getf case :target) :link (getf case :link)
                                         :spelling :relative)))
         (segments (uiop:split-string base :separator "/")))
    (unless (and (plusp (length base))
                 (notany (lambda (segment) (member segment '("" "." "..") :test #'string=))
                         segments))
      (error "~S is not a safe base spelling." base))
    (list (cons :relative-string base)
          (cons :relative-pathname (uiop:parse-native-namestring base))
          (cons :leading-dot (concatenate 'string "./" base))
          (cons :repeated-separator
                (%double-separator base (getf case :doubled-separator))))))

(defun observe-spellings (fixture variants)
  "Call ENSURE-WRITE-PATH once for each of VARIANTS, in order, observing each
call as OBSERVE-VALIDATOR does, and return one record per call, made right
after it:

  (:SPELLING S :INPUT-TYPE :STRING or :PATHNAME :INPUT the argument, native
   :RETURNED the result's native namestring or NIL :ABSOLUTE its absoluteness
   :REFUSED the refusal's reason or NIL :CHANGES the tree's changes)

The result is kept as a native string, so nothing a later call does to a
shared object can change it.  Stops after the first call that changed the tree:
a later call would run against a tree the case does not describe.  Only
WRITE-PATH-REFUSED counts as a refusal; any other condition goes on."
  (let ((records '()))
    (loop for (spelling . argument) in variants
          do (let* ((pathname-p (pathnamep argument))
                    (input (if pathname-p (uiop:native-namestring argument) (copy-seq argument)))
                    (observed (observe-validator fixture argument))
                    (result (getf observed :returned)))
               (push (list :spelling spelling
                           :input-type (if pathname-p :pathname :string)
                           :input input
                           :returned (and (pathnamep result) (uiop:native-namestring result))
                           :absolute (and (pathnamep result) (uiop:absolute-pathname-p result) t)
                           :refused (getf observed :refused)
                           :changes (getf observed :changes))
                     records)
               (unless (no-changes-p (getf observed :changes))
                 (loop-finish))))
    (nreverse records)))

(defun safe-spelling-coverage-p (records)
  "True when RECORDS observe every spelling of *SAFE-SPELLINGS*, in that order,
one call each, a pathname and at least two strings among them.  An empty list,
the base alone, or strings alone do not cover the relation."
  (flet ((input-type (record) (getf record :input-type)))
    (and (equal (mapcar (lambda (record) (getf record :spelling)) records) *safe-spellings*)
         (find :pathname records :key #'input-type)
         (>= (count :string records :key #'input-type) 2)
         t)))

(defun explain-safe-spellings (case)
  "Build CASE in a fresh fixture and return what WRITE-PRESERVES-SAFE-SPELLINGS
sees for it, as (:EXPECTED native :RECORDS records), for reading a
counterexample.  The calls are made again; nothing from a failed run is
replayed."
  (with-write-fixture (fixture case)
    (list :expected (expected-write-native fixture case)
          :records (observe-spellings fixture (safe-spelling-variants case)))))
