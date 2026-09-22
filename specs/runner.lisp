;;;; specs/runner.lisp
;;;;
;;;; Direct runner for the cl-mcp/specs bundle, on cl-spec's public Lisp API.
;;;;
;;;; This is what CI judges by.  It calls CL-SPEC:RUN-PROPERTY,
;;;; CL-SPEC:CHECK-FUNCTION and CL-SPEC:CHECK-CALL and reads their results
;;;; through CL-SPEC:RESULT-DATA; it never goes through the MCP adapter or parses
;;;; spec-check's text.  MCP-side verification gaps such as
;;;; input-coverage-unmeasured are the adapter's report and play no part here.
;;;;
;;;; A run passes only when it selected at least one target, every target is
;;;; registered, every generated run came back :PASSED with at least one trial
;;;; that reached the function, every declared case of a contract was called,
;;;; and every concrete example passed.  Anything else -- a failure, an error, a
;;;; skipped run, a condition, a timeout, an undeclared profile, a result this
;;;; runner cannot read -- fails the run.  Nothing unknown defaults to success.

(defpackage #:cl-mcp/specs/runner
  (:use #:cl)
  (:import-from #:cl-spec/main
                #:*registry*
                #:*generator-backend*
                #:make-hash-table-registry
                #:find-property
                #:find-function-spec
                #:property-trials
                #:property-targets
                #:list-properties
                #:list-function-specs
                #:list-specs
                #:list-generators
                #:run-property
                #:check-function
                #:check-call
                #:result-data
                #:property-result-condition
                #:function-spec-data
                #:call-check-result-status
                #:call-check-result-failure-phase
                #:call-check-data)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/utils/paths
                #:allowed-read-path
                #:ensure-write-path
                #:write-path-refused
                #:canonical-path
                #:path-inside-p)
  (:import-from #:cl-mcp/src/spec-core-record
                #:field-availability
                #:project-record
                #:project-core-record
                #:*projection-max-length*)
  ;; Internal symbols, imported for the negative control only: it swaps these
  ;; functions' definitions, and nothing here calls them.
  (:import-from #:cl-mcp/src/spec-adapter-report
                #:%counts
                #:%contract-plist
                #:%verified-p
                #:%verification-gaps
                #:%select-properties
                #:%trials-budget
                #:%definition-match
                #:list-report
                #:%property-listing
                #:%describe-function-spec)
  (:import-from #:cl-mcp/src/tools/spec-entry
                #:parse-seed-string)
  (:import-from #:cl-mcp/src/tools/spec-response-builders
                #:build-spec-list-response
                #:build-spec-check-response)
  (:import-from #:cl-mcp/src/spec-adapter-core
                #:api-fn
                #:definition-digest)
  (:import-from #:cl-mcp/specs
                #:register-specifications
                #:contract-names
                #:property-names
                #:spec-names
                #:generator-names
                #:call-examples)
  (:export #:*default-seeds*
           #:*default-profile*
           #:*default-trials*
           #:*default-timeout-seconds*
           #:bundle-targets
           #:run-checks
           #:run-bundle
           #:run-negative-control
           #:judge-entry
           #:judge-example
           #:bundle-consistency-problems
           #:covered-functions
           #:report-ok-p
           #:exit-code
           #:git-state
           #:print-report
           #:write-report
           #:main))

(in-package #:cl-mcp/specs/runner)

(defparameter *default-seeds* '(20260922 1 7777777)
  "Fixed seeds of a bundle run.  These are Lisp integers; an MCP spec-check
call takes the same seed as a decimal string.")

(defparameter *default-profile* :normal
  "Profile of every property run.  Each property's own :TRIALS table turns it
into a budget; a property whose table lacks it is refused, not run.")

(defparameter *default-trials* 200
  "Trial budget of every Function Spec run.  A contract has no profile table.")

(defparameter *default-timeout-seconds* 120
  "Deadline for one target under one seed.")

;;; ------------------------------------------------------------------------
;;; Printing values safely

(defun %qualified-name (symbol)
  "Return SYMBOL's name with its package, however *PACKAGE* is bound."
  (let ((*package* (find-package "KEYWORD")))
    (prin1-to-string symbol)))

(defun %safe-text (string &key (limit 600))
  "Return STRING with every control character, DEL, C1 control, surrogate and
character above U+FFFF written as <U+XXXX>, cut at LIMIT characters.  A report
goes to CI logs and terminals, where a raw ESC would be interpreted."
  (let ((text (with-output-to-string (out)
                (loop for character across string
                      for code = (char-code character)
                      do (if (or (< code 32) (<= 127 code 159) (<= #xD800 code #xDFFF)
                                 (> code #xFFFF))
                             (format out "<U+~4,'0X>" code)
                             (write-char character out))))))
    (if (> (length text) limit)
        (format nil "~A...(~D chars)" (subseq text 0 limit) (length text))
        text)))

(defun %render (value &key (limit 600))
  "Return VALUE printed on one line with bounded depth and length, made safe by
%SAFE-TEXT and cut at LIMIT characters.  Symbols outside COMMON-LISP print
with their package."
  (%safe-text (let ((*print-length* 20) (*print-level* 6) (*print-readably* nil)
                    (*print-pretty* nil) (*package* (find-package "CL-USER")))
                (prin1-to-string value))
              :limit limit))

(defun %condition-record (condition)
  "Return CONDITION's type and report as strings."
  (list :type (%qualified-name (type-of condition))
        :report (%safe-text (handler-case (princ-to-string condition)
                              (error () "<report signalled>")))))

;;; ------------------------------------------------------------------------
;;; Running one target

(defun %call-with-deadline (seconds thunk)
  "Call THUNK.  Return its value and NIL, or NIL and a record of why there is no
value: (:STATUS :TIMEOUT) past SECONDS, (:STATUS :SIGNALLED ...) on an error."
  (handler-case (values (sb-ext:with-timeout seconds (funcall thunk)) nil)
    (sb-ext:timeout ()
      (values nil (list :status :timeout :timeout-seconds seconds)))
    (error (condition)
      (values nil (list :status :signalled :condition (%condition-record condition))))))

(defun %call-capturing-error-output (thunk)
  "Call THUNK with *ERROR-OUTPUT* captured and its warnings recorded, and return
THUNK's two values and a record of both, or NIL when there was neither.

Output: (:ERROR-OUTPUT-LINES N :ERROR-OUTPUT-SAMPLE FIRST-LINE); the text beyond
its first line is not kept.  The read-path properties make ASDF reload .asd
files and warn about each one; counted, that noise no longer buries the report.

Warnings: (:WARNINGS ((:TYPE NAME :COUNT N :REPORTS (TEXT ...)) ...)), one row
per condition type in order of first appearance, with up to three distinct
reports each, kept whole up to 2000 characters.  A warning is recorded, never
muffled, so it is printed as before.  This is where a failure that must not be
lost next to that noise goes -- a read fixture's cleanup failing while a
property's own condition unwinds, say.

Neither plays any part in a verdict."
  (let ((captured (make-string-output-stream))
        (rows '()))
    (flet ((record (warning)
             (let* ((type (%qualified-name (type-of warning)))
                    (row (or (find type rows :key (lambda (row) (getf row :type))
                                             :test #'string=)
                             (let ((new (list :type type :count 0 :reports '())))
                               (push new rows)
                               new)))
                    (text (%safe-text (handler-case (princ-to-string warning)
                                        (error () "<report signalled>"))
                                      :limit 2000)))
               (incf (getf row :count))
               (when (and (< (length (getf row :reports)) 3)
                          (not (member text (getf row :reports) :test #'string=)))
                 (setf (getf row :reports) (append (getf row :reports) (list text)))))))
      (multiple-value-bind (value failure)
          (handler-bind ((warning #'record))
            (let ((*error-output* captured))
              (funcall thunk)))
        (let ((text (get-output-stream-string captured)))
          (values value failure
                  (append
                   (when (plusp (length text))
                     (list :error-output-lines (max 1 (count #\Newline text))
                           :error-output-sample
                           (%safe-text (subseq text 0 (or (position #\Newline text)
                                                          (length text)))
                                       :limit 200)))
                   (when rows
                     (list :warnings (reverse rows))))))))))

(defun %result-record (result)
  "Return what the runner keeps of a cl-spec RESULT, read from its version 1
RESULT-DATA record.  Any other schema version is :UNSUPPORTED-RESULT."
  (let ((data (result-data result)))
    (if (not (eql 1 (getf data :schema-version)))
        (list :status :unsupported-result :schema-version (getf data :schema-version))
        (list :status (getf data :status)
              :trials (getf data :trials)
              :budget (getf data :budget)
              :rejected (getf data :rejected)
              :result-seed (getf data :seed)
              :result-profile (getf data :profile)
              :digest (getf data :definition-digest)
              :digest-complete (getf data :definition-digest-complete)
              :digest-omissions (getf data :digest-omissions)
              :capabilities (getf data :capabilities)
              :case-report (getf data :case-report)
              :counterexample (getf data :counterexample)
              :shrunk-counterexample (getf data :shrunk-counterexample)
              :shrunk-outcome (getf data :shrunk-outcome)
              :failure-phase (getf data :failure-phase)
              :failure-reason (getf data :failure-reason)
              :condition (let ((condition (property-result-condition result)))
                           (and condition (%condition-record condition)))
              :elapsed (getf data :elapsed)))))

(defun %profile-declared-p (property profile)
  "True when PROFILE is a key of PROPERTY's :TRIALS table."
  (loop for key in (property-trials property) by #'cddr
        thereis (eq key profile)))

(defun %run-property-target (name registry seed profile timeout-seconds)
  "Run property NAME under SEED and PROFILE; return its entry."
  (let ((base (list :kind :property :name name :seed seed :profile profile)))
    (multiple-value-bind (property found) (find-property name registry)
      (cond ((not found)
             (append base (list :status :not-registered)))
            ((not (%profile-declared-p property profile))
             (append base (list :status :unknown-profile
                                :declared-trials (property-trials property))))
            (t
             (multiple-value-bind (result failure output)
                 (%call-capturing-error-output
                  (lambda ()
                    (%call-with-deadline timeout-seconds
                                         (lambda ()
                                           (run-property name :profile profile :seed seed
                                                              :registry registry)))))
               (append base (or failure (%result-record result)) output)))))))

(defun %declared-cases (name registry)
  "Return the case names the Function Spec NAME declares, in order."
  (mapcar (lambda (case) (getf case :name))
          (getf (function-spec-data name :registry registry) :cases)))

(defun %run-function-target (name registry seed trials timeout-seconds)
  "Run the Function Spec NAME under SEED with TRIALS; return its entry."
  (let ((base (list :kind :function-spec :name name :seed seed :requested-trials trials)))
    (if (not (nth-value 1 (find-function-spec name registry)))
        (append base (list :status :not-registered))
        (multiple-value-bind (result failure output)
            (%call-capturing-error-output
             (lambda ()
               (%call-with-deadline timeout-seconds
                                    (lambda ()
                                      (check-function name :trials trials :seed seed
                                                           :registry registry)))))
          (append base
                  (list :declared-cases (%declared-cases name registry))
                  (or failure (%result-record result))
                  output)))))

(defun %run-example (example registry timeout-seconds)
  "Check one concrete call (FUNCTION ARGUMENTS [CASE]) with CHECK-CALL."
  (destructuring-bind (name arguments &optional expected-case) example
    (let ((base (list :kind :example :name name :arguments arguments
                      :expected-case expected-case)))
      (multiple-value-bind (result failure)
          (%call-with-deadline timeout-seconds
                               (lambda () (check-call name arguments :registry registry)))
        (append base
                (cond (failure failure)
                      ((null result) (list :status :no-observation))
                      (t (let ((observation (getf (call-check-data result) :observation)))
                           (list :status (call-check-result-status result)
                                 :case (getf observation :case)
                                 :failure-phase (call-check-result-failure-phase result)
                                 :condition-report (getf observation :condition-report))))))))))

;;; ------------------------------------------------------------------------
;;; Judging

(defun %case-problems (declared report)
  "Return the problems of a contract whose cases are DECLARED, given the case
REPORT of its run: unmeasured, a failed selection or capture, or a declared
case no trial called."
  (if (not (consp report))
      (list (list :case-report-unmeasured report))
      (append
       (unless (eql 0 (getf report :case-selection-errors))
         (list (list :case-selection-errors (getf report :case-selection-errors))))
       (unless (eql 0 (getf report :capture-errors))
         (list (list :capture-errors (getf report :capture-errors))))
       (when (getf report :never-called)
         (list (list :cases-never-called (getf report :never-called))))
       (loop for case in declared
             for called = (getf (find case (getf report :cases)
                                      :key (lambda (row) (getf row :name)))
                                :called)
             unless (and (integerp called) (plusp called))
               collect (list :case-not-called case)))))

(defun judge-entry (entry)
  "Return the problems of the target run ENTRY; NIL means it passed.
Only :PASSED passes, and only with a positive trial count, a measured rejection
count from zero up to but not including it, and -- for a contract with :CASES -- a measured case report in
which every declared case was called and no selection or capture failed."
  (let ((status (getf entry :status))
        (trials (getf entry :trials))
        (rejected (getf entry :rejected))
        (declared (getf entry :declared-cases)))
    (cond ((not (eq status :passed))
           (list (list :status status)))
          ((not (and (integerp trials) (plusp trials)))
           (list (list :no-trials trials)))
          ((not (integerp rejected))
           (list (list :rejections-unmeasured rejected)))
          ((not (<= 0 rejected trials))
           (list (list :rejections-out-of-range trials rejected)))
          ((not (plusp (- trials rejected)))
           (list (list :no-effective-trials trials rejected)))
          (declared
           (%case-problems declared (getf entry :case-report)))
          (t nil))))

(defun judge-example (entry)
  "Return the problems of the example ENTRY; NIL means it passed: CHECK-CALL
answered :PASSED and, when the example names a case, that case was selected."
  (let ((expected (getf entry :expected-case)))
    (append (unless (eq :passed (getf entry :status))
              (list (list :status (getf entry :status))))
            (when (and expected (not (eq expected (getf entry :case))))
              (list (list :case (getf entry :case) :expected expected))))))

(defun report-ok-p (report)
  "True when REPORT recorded no problem."
  (and (getf report :ok) t))

(defun exit-code (report)
  "Return 0 for a REPORT that passed and 1 for any other."
  (if (report-ok-p report) 0 1))

;;; ------------------------------------------------------------------------
;;; Running a selection

(defun bundle-targets ()
  "Return every target this bundle owns, as (:FUNCTION-SPEC NAME) and
\(:PROPERTY NAME) designators."
  (append (mapcar (lambda (name) (list :function-spec name)) (contract-names))
          (mapcar (lambda (name) (list :property name)) (property-names))))

(defun run-checks (targets &key (registry *registry*) (seeds *default-seeds*)
                             (profile *default-profile*) (trials *default-trials*)
                             (timeout-seconds *default-timeout-seconds*) examples)
  "Run each of TARGETS under each of SEEDS in REGISTRY, check each of EXAMPLES
once, and return a report plist.  Property runs use PROFILE and Function Spec
runs use TRIALS; neither setting is applied to the other kind.  Generated runs
and examples are reported apart, and an example counts as no trial."
  (let* ((entries
           (loop for seed in seeds
                 append (loop for (kind name) in targets
                              collect (ecase kind
                                        (:property
                                         (%run-property-target name registry seed profile
                                                               timeout-seconds))
                                        (:function-spec
                                         (%run-function-target name registry seed trials
                                                               timeout-seconds))))))
         (example-entries (loop for example in examples
                                collect (%run-example example registry timeout-seconds)))
         (problems
           (append (unless targets (list (list :empty-selection)))
                   (unless seeds (list (list :no-seeds)))
                   (loop for entry in entries
                         for found = (judge-entry entry)
                         when found
                           collect (list :target (getf entry :kind) (getf entry :name)
                                         :seed (getf entry :seed) :problems found))
                   (loop for entry in example-entries
                         for found = (judge-example entry)
                         when found
                           collect (list :example (getf entry :name) (getf entry :arguments)
                                         :problems found)))))
    (list :ok (null problems)
          :problems problems
          :selection (list :targets targets :seeds seeds :profile profile :trials trials
                           :timeout-seconds timeout-seconds :examples (length examples))
          :entries entries
          :examples example-entries)))

(defun bundle-consistency-problems (registry)
  "Compare what REGISTRY holds with what the bundle lists.  Run on a registry
that holds this bundle alone, any difference is a definition missing from the
listing or a listed name nothing registered."
  (loop for (kind listed registered)
          in (list (list :function-specs (contract-names) (list-function-specs registry))
                   (list :properties (property-names) (list-properties registry))
                   (list :specs (spec-names) (list-specs registry))
                   (list :generators (generator-names) (list-generators registry)))
        for missing = (set-difference listed registered)
        for unlisted = (set-difference registered listed)
        when (/= (length listed) (length (remove-duplicates listed)))
          collect (list :duplicate-names kind listed)
        when missing
          collect (list :unregistered kind missing)
        when unlisted
          collect (list :unlisted kind unlisted)))

;;; ------------------------------------------------------------------------
;;; Where the code under check came from

(defun %definition-files (symbol)
  "Return the source files SBCL recorded for the function SYMBOL names."
  (require :sb-introspect)
  (loop for source in (uiop:symbol-call :sb-introspect :find-definition-sources-by-name
                                        symbol :function)
        for pathname = (uiop:symbol-call :sb-introspect :definition-source-pathname source)
        when pathname
          collect pathname))

(defun %native (pathname)
  "Return PATHNAME as a native namestring, or NIL.  A logical pathname -- SBCL
records one for its own functions, SYS:SRC;... -- is translated first, and
kept as its logical namestring when it has no translation."
  (when pathname
    (handler-case (uiop:native-namestring (translate-logical-pathname pathname))
      (error () (namestring pathname)))))

(defun covered-functions (registry)
  "Return every function the bundle makes a claim about, as registered in
REGISTRY: those with a Function Spec, and each fbound (:about ...) target of its
properties.  A function checked only by properties is covered too."
  (remove-duplicates
   (append (contract-names)
           (loop for name in (property-names)
                 for property = (find-property name registry)
                 when property
                   append (remove-if-not #'fboundp (property-targets property))))
   :from-end t))

(defun source-record (functions)
  "Return, for each of FUNCTIONS, the files its loaded definition came from."
  (loop for name in functions
        collect (list :name name :files (mapcar #'%native (%definition-files name)))))

(defun source-problems (expected-root functions)
  "Return problems when the cl-mcp under check was not loaded from
EXPECTED-ROOT: the cl-mcp system's directory differs, or the definition of one
of FUNCTIONS lives elsewhere.  NIL when EXPECTED-ROOT is NIL."
  (when expected-root
    (let* ((root (truename expected-root))
           (system-directory (asdf:system-source-directory "cl-mcp"))
           (problems (unless (and system-directory (equal (truename system-directory) root))
                       (list (list :cl-mcp-loaded-from (%native system-directory)
                                   :expected (%native root))))))
      (dolist (name functions)
        (let ((files (%definition-files name)))
          (if (null files)
              (push (list :source-unknown name) problems)
              (dolist (file files)
                (let ((truename (ignore-errors (truename file))))
                  (unless (and truename (uiop:subpathp truename root))
                    (push (list :source-outside-checkout name (%native file)) problems)))))))
      (nreverse problems))))

(defun %git (directory arguments &key (strip t) input)
  "Run git with ARGUMENTS in DIRECTORY and return its output, or NIL when git is
missing or fails.  STRIP drops trailing whitespace; INPUT is a string fed to
git's standard input."
  (handler-case
      (multiple-value-bind (output error-output code)
          (uiop:run-program (list* "git" "-C" (%native directory) arguments)
                            :output (if strip '(:string :stripped t) :string)
                            :error-output nil
                            :input (and input (make-string-input-stream input))
                            :ignore-error-status t)
        (declare (ignore error-output))
        (and (eql code 0) output))
    (error () nil)))

(defun %lisp-source-changes (directory)
  "Return (STATUS PATH BLOB) for every .lisp and .asd file of DIRECTORY's
repository that differs from HEAD, untracked ones included (STATUS \"??\").
PATH is relative to the repository's top level and BLOB is git's hash of the
file as it is now, or :DELETED.  NIL when nothing differs, :UNKNOWN when git
cannot say."
  (let ((top (%git directory '("rev-parse" "--show-toplevel")))
        (status (%git directory '("status" "--porcelain" "-z" "--untracked-files=all"
                                  "--" "*.lisp" "*.asd")
                      :strip nil)))
    (if (not (and top status))
        :unknown
        (let ((fields (uiop:split-string status :separator (list (code-char 0))))
              (entries '()))
          (loop while fields
                do (let ((field (pop fields)))
                     (when (> (length field) 3)
                       ;; A rename or copy is followed by its source path.
                       (when (find-if (lambda (code) (find code "RC")) (subseq field 0 2))
                         (pop fields))
                       (push (list (subseq field 0 2) (subseq field 3)) entries))))
          (let* ((entries (sort entries #'string< :key #'second))
                 (present (remove-if (lambda (entry) (find #\D (first entry))) entries))
                 (blobs (and present
                             (uiop:split-string
                              (or (%git top (list* "hash-object" "--"
                                                   (mapcar #'second present)))
                                  "")
                              :separator '(#\Newline)))))
            (if (and present (/= (length blobs) (length present)))
                :unknown
                (mapcar (lambda (entry)
                          (list (first entry) (second entry)
                                (if (member entry present)
                                    (nth (position entry present) blobs)
                                    :deleted)))
                        entries)))))))

(defun git-state (directory)
  "Return what git says about DIRECTORY's repository, as a plist:
  :REVISION           HEAD, or :UNKNOWN
  :CHANGES            `git status --porcelain' lines, or :UNKNOWN
  :LISP-CHANGES       (STATUS PATH BLOB) per .lisp/.asd file differing from HEAD
  :LISP-FINGERPRINT   git's hash of those rows, or NIL when there are none
Two runs at one HEAD with the same status lines can still have checked
different code; their fingerprints differ whenever a changed Lisp file's
contents do, untracked files included."
  (if (null directory)
      (list :revision :unknown)
      (let ((revision (%git directory '("rev-parse" "HEAD")))
            (status (%git directory '("status" "--porcelain")))
            (lisp-changes (%lisp-source-changes directory)))
        (list :revision (or revision :unknown)
              :changes (if status
                           (remove "" (uiop:split-string status :separator '(#\Newline))
                                   :test #'string=)
                           :unknown)
              :lisp-changes lisp-changes
              :lisp-fingerprint
              (and (consp lisp-changes)
                   (%git directory '("hash-object" "--stdin")
                         :input (format nil "~:{~A ~A ~A~%~}" lisp-changes)))))))

(defun %system-record (name)
  "Return the version, directory and git state of the ASDF system NAME."
  (let* ((system (asdf:find-system name nil))
         (directory (and system (asdf:system-source-directory system))))
    (list :version (and system (asdf:component-version system))
          :directory (%native directory)
          :git (git-state directory))))

(defun environment-record (&optional expected-root)
  "Return the Lisp, ASDF, cl-spec backend and the revisions of cl-mcp, cl-spec
and check-it this run used, and the checkout it was expected to use."
  (list :lisp (format nil "~A ~A" (lisp-implementation-type) (lisp-implementation-version))
        :asdf (asdf:asdf-version)
        :backend (%qualified-name (type-of *generator-backend*))
        :cl-mcp (%system-record "cl-mcp")
        :cl-spec (%system-record "cl-spec")
        :check-it (%system-record "check-it")
        :expected-root (%native expected-root)))

(defun run-bundle (&key (seeds *default-seeds*) (profile *default-profile*)
                     (trials *default-trials*)
                     (timeout-seconds *default-timeout-seconds*) expected-root)
  "Register the bundle into a fresh registry, check that registry against the
bundle's own listing, run every target under every seed and every example, and
return the report.  When EXPECTED-ROOT is given, a cl-mcp loaded from anywhere
else fails the run.  Runs in a registry of its own, so CL-SPEC:*REGISTRY* and
whatever else is registered there are neither read nor changed."
  (let ((registry (make-hash-table-registry)))
    (register-specifications registry)
    (let* ((functions (covered-functions registry))
           (report (run-checks (bundle-targets) :registry registry :seeds seeds
                                                :profile profile :trials trials
                                                :timeout-seconds timeout-seconds
                                                :examples (call-examples)))
           (problems (append (getf report :problems)
                             (bundle-consistency-problems registry)
                             (source-problems expected-root functions))))
      (setf (getf report :problems) problems
            (getf report :ok) (null problems))
      (list* :mode :check
             :environment (environment-record expected-root)
             :sources (source-record functions)
             report))))

;;; ------------------------------------------------------------------------
;;; Negative control

(defun %bundle-name (kind name)
  "Return the bundle's symbol named NAME among its KIND names, or signal."
  (or (find name (ecase kind
                   (:property (property-names))
                   (:function-spec (contract-names)))
            :key #'symbol-name :test #'string=)
      (error "The bundle has no ~(~A~) named ~A." kind name)))

(defun %read-path-ignoring-dependencies (path)
  "A wrong ALLOWED-READ-PATH, for the negative control: resolves PATH as the
real one does but allows the project only, as if no ASDF system were
registered."
  (let* ((absolute (canonical-path path))
         (resolved (or (ignore-errors (truename absolute)) absolute))
         (normalized (if (uiop:directory-exists-p resolved)
                         (uiop:ensure-directory-pathname resolved)
                         resolved)))
    (when (path-inside-p normalized
                         (truename (uiop:ensure-directory-pathname *project-root*)))
      normalized)))

(defun %read-path-trusting-spelling (real)
  "Return a wrong ALLOWED-READ-PATH, for the negative control: a path spelled
under the project root is allowed as whatever it resolves to, without asking
where that is; any other path goes to REAL, the real function.  It differs from
REAL only where a path written inside the project leads outside it, so only a
denial can catch it."
  (lambda (path)
    (let ((absolute (canonical-path path)))
      (if (path-inside-p absolute (uiop:ensure-directory-pathname *project-root*))
          (let ((resolved (or (ignore-errors (truename absolute)) absolute)))
            (if (uiop:directory-exists-p resolved)
                (uiop:ensure-directory-pathname resolved)
                resolved))
          (funcall real path)))))

(defun %read-path-by-string-prefix (real)
  "Return a wrong ALLOWED-READ-PATH, for the negative control: a resolved path
whose native string merely starts with the project root's -- project-other/
beside project/, say -- is allowed; any other path goes to REAL.  Only a
denial of a prefix sibling can catch it."
  (lambda (path)
    (let* ((absolute (canonical-path path))
           (resolved (or (ignore-errors (truename absolute)) absolute))
           (root (string-right-trim
                  "/" (uiop:native-namestring
                       (truename (uiop:ensure-directory-pathname *project-root*))))))
      (if (uiop:string-prefix-p root (uiop:native-namestring resolved))
          resolved
          (funcall real path)))))

(defun %refuse-write-for-control (path)
  "Signal the refusal the wrong ENSURE-WRITE-PATHs below give."
  (error 'write-path-refused :path path :reason :outside-project
                             :format-control "Write path ~A is outside project root"
                             :format-arguments (list path)))

(defun %write-path-lexically (path)
  "A wrong ENSURE-WRITE-PATH, for the negative control: the implementation
before the write boundary was fixed.  It merges PATH onto the project root
lexically and resolves it with TRUENAME only when the whole path exists, so a
new file or directory below a symlink to outside the project is allowed as the
link's own spelling.  Every path it is given in a check lies in the check's
own scratch tree, so what it lets through is written there and nowhere else."
  (let ((pn (uiop:ensure-pathname path)))
    (when (uiop:absolute-pathname-p pn)
      (%refuse-write-for-control path))
    (let ((real (or (ignore-errors (truename (canonical-path pn))) (canonical-path pn)))
          (root (let ((directory (uiop:ensure-directory-pathname *project-root*)))
                  (or (ignore-errors (uiop:ensure-directory-pathname (truename directory)))
                      directory))))
      (unless (path-inside-p real root)
        (%refuse-write-for-control path))
      real)))

(defun %write-path-by-read-policy (path)
  "A wrong ENSURE-WRITE-PATH, for the negative control: allows whatever
ALLOWED-READ-PATH allows, so an absolute path in the project and a registered
dependency's directory become writable.  ALLOWED-READ-PATH is not swapped in
this control, so this is the real read decision."
  (or (allowed-read-path path)
      (%refuse-write-for-control path)))

(defun %write-path-misreading-doubled-separators (real)
  "Return a wrong ENSURE-WRITE-PATH, for the negative control: a string with a
doubled separator comes back as the file beside the right one, its name with
-other appended, still inside the project; anything else goes to REAL, the
real function.  Every answer is allowed and nothing escapes, so only a
comparison of spellings that include a doubled separator can tell."
  (lambda (path)
    (let ((result (funcall real path)))
      (if (and (stringp path) (search "//" path))
          (make-pathname :name (format nil "~A-other" (pathname-name result))
                         :defaults result)
          result))))

(defun %availability-reading-nil-as-absent (real)
  "Return a wrong FIELD-AVAILABILITY, for the negative control: a key present
with NIL reads as :ABSENT, as GETF would answer; anything else goes to REAL."
  (lambda (record key)
    (if (null (getf record key))
        :absent
        (funcall real record key))))

(defun %project-record-with-numeric-seeds (real)
  "Return a wrong PROJECT-RECORD, for the negative control: REAL's projection,
with a top-level seed turned back into a JSON number."
  (lambda (value descriptor &rest options)
    (multiple-value-bind (node issues unknown) (apply real value descriptor options)
      (values (if (and (consp node) (eq :object (first node)))
                  (list :object
                        (loop for (key . child) in (second node)
                              collect (if (and (equal "seed" key)
                                               (stringp (second child)))
                                          (cons key (list :scalar
                                                          (parse-integer (second child))))
                                          (cons key child))))
                  node)
              issues
              unknown))))

(defun %core-record-claiming-completeness (real)
  "Return a wrong PROJECT-CORE-RECORD, for the negative control: REAL's report,
with PROJECTION.COMPLETE true even when its issues say something was cut."
  (lambda (record shape-name &rest options)
    (multiple-value-bind (report status reason) (apply real record shape-name options)
      (values (and report
                   (let ((copy (copy-list report)))
                     (setf (getf copy :projection)
                           (list :complete t
                                 :issues (getf (getf report :projection) :issues)))
                     copy))
              status
              reason))))

(defun %project-record-keeping-tails (real)
  "Return a wrong PROJECT-RECORD, for the negative control: a record's
top-level list longer than *PROJECTION-MAX-LENGTH* keeps its last items instead
of its first.  REAL still makes the cut, so the issue, its path, the omitted
count and PROJECTION.COMPLETE are exactly what they should be; only the kept
items are wrong."
  (lambda (value descriptor &rest options)
    (let ((limit *projection-max-length*))
      (apply real
             (if (and (consp value) (keywordp (first value)))
                 (loop for (key item) on value by #'cddr
                       append (list key
                                    (if (and (consp item) (> (length item) limit))
                                        ;; The last LIMIT items first, so the
                                        ;; head REAL keeps is the old tail.
                                        (append (last item limit) (butlast item limit))
                                        item)))
                 value)
             descriptor
             options))))

(defun %counts-dropping-unnamed-statuses (real)
  "Return a wrong %COUNTS, for the negative control: REAL's tally with every
status that has no field of its own dropped from OTHER and BY-STATUS, as the
named fields alone once did.  SELECTED still counts them."
  (lambda (results)
    (let ((counts (copy-list (funcall real results))))
      (setf (getf counts :other) 0
            (getf counts :by-status)
            (remove-if-not (lambda (entry)
                             (member (car entry) '(:passed :failed :error :timeout :not-run)))
                           (getf counts :by-status)))
      counts)))

(defun %contract-plist-falling-back-to-raw-trials (real)
  "Return a wrong %CONTRACT-PLIST, for the negative control: REAL's contract
half, with the raw trial count published as effective trials whenever the
refusal count cannot be subtracted with."
  (lambda (api result executed max-value-chars source &optional (precondition-p :unknown))
    (let ((half (copy-list (funcall real api result executed max-value-chars source
                                    precondition-p))))
      (when (and (null (getf half :effective-trials)) (integerp executed))
        (setf (getf half :effective-trials) executed))
      half)))

(defun %verified-when-nothing-ran (real)
  "Return a wrong %VERIFIED-P, for the negative control: an empty list of
results is verified; any other list goes to REAL."
  (lambda (results)
    (or (null results) (funcall real results))))

(defun %claiming-every-case-reached (result)
  "Return RESULT, or a copy of it whose case report lists no declared case as
never called.  The copy shares nothing it changes with RESULT."
  (let* ((core (getf result :core-record))
         (source (getf core :source))
         (report (getf source :case-report)))
    (if (and (consp report) (getf report :never-called))
        (let ((report (copy-list report))
              (source (copy-list source))
              (core (copy-list core))
              (result (copy-list result)))
          (setf (getf report :never-called) nil
                (getf source :case-report) report
                (getf core :source) source
                (getf result :core-record) core)
          result)
        result)))

(defun %verified-ignoring-unreached-cases (real)
  "Return a wrong %VERIFIED-P, for the negative control: REAL's verdict over
the results as if every declared case had been reached."
  (lambda (results)
    (funcall real (mapcar #'%claiming-every-case-reached results))))

(defun %gaps-measuring-rejections-from-any-contract (real)
  "Return a wrong %VERIFICATION-GAPS, for the negative control: REAL's gaps,
without rejection-counts-unmeasured whenever any one result, rather than every
one, is a contract run whose refusal count is usable."
  (lambda (results &optional selection)
    (let ((gaps (funcall real results selection)))
      (if (some (lambda (result) (getf (getf result :contract) :rejected-usable))
                results)
          (remove :rejection-counts-unmeasured gaps)
          gaps))))

(defun %selection-adding-the-subject (real)
  "Return a wrong %SELECT-PROPERTIES, for the negative control: an :about
selection that also runs the symbol's own contract or same-named property,
which the real one reports as not run.  Everything else goes to REAL."
  (lambda (api property symbol function package registry)
    (multiple-value-bind (names selection error kind data)
        (funcall real api property symbol function package registry)
      (let ((subject (and symbol (null error)
                          (or (getf selection :own-property-not-run)
                              (getf selection :contract-not-run)))))
        (values (if subject
                    (append names
                            (list (find-symbol (getf subject :name) (getf subject :package))))
                    names)
                selection error kind data)))))

(defun %budget-from-the-backend-only (real)
  "Return a wrong %TRIALS-BUDGET, for the negative control: REAL's answer as if
the property had no :TRIALS table and the caller had asked for no trials, so
every budget is the backend default."
  (lambda (api facts profile backend &optional requested)
    (declare (ignore requested))
    (funcall real api (list* :trials-table nil facts) profile backend nil)))

(defun %seed-through-a-double (real)
  "Return a wrong PARSE-SEED-STRING, for the negative control: REAL's seed
taken through a binary64 double, as a JSON consumer would, so a seed past 2^53
comes back rounded."
  (lambda (text)
    (multiple-value-bind (seed message) (funcall real text)
      (values (and seed (round (coerce seed 'double-float))) message))))

(defun %incomplete-digest-matching (real)
  "Return a wrong %DEFINITION-MATCH, for the negative control: an incomplete
digest whose text equals the expected one counts as a match.  Everything else
goes to REAL."
  (lambda (digest expected)
    (let ((answer (funcall real digest expected))
          (value (getf digest :value)))
      (if (and (eq answer :unknown) value expected (string= value expected))
          :true
          answer))))

(defun %listing-counting-what-it-did-not-look-at (real)
  "Return a wrong LIST-REPORT, for the negative control: every count that is
absent -- because the kind was not asked for, or cannot be listed -- is
reported as 0, which says the registry holds none of them."
  (lambda (api api-status &rest arguments)
    (let ((report (copy-list (apply real api api-status arguments))))
      (when (getf report :counts)
        (setf (getf report :counts)
              (loop for (key value) on (getf report :counts) by #'cddr
                    append (list key (or value 0)))))
      report)))

(defun %listing-reading-another-registry (real)
  "Return a wrong %PROPERTY-LISTING, for the negative control: the row is read
without the registry the listing is about, so the reader answers out of
whatever registry the image holds.

The rows still come back right here, because the stub answers from the
descriptor it closes over however it is called.  Only the recorded call shows
the registry went missing, which is why the check has to read it."
  (lambda (api name registry)
    (declare (ignore registry))
    (funcall real api name nil)))

(defun %response-counting-what-nobody-looked-at (real)
  "Return a wrong BUILD-SPEC-LIST-RESPONSE, for the negative control: a count
that is null -- the kind nobody could look at -- is published as 0, which says
the registry holds none of them."
  (lambda (report)
    (let* ((response (funcall real report))
           (counts (gethash "counts" response)))
      (when (hash-table-p counts)
        (maphash (lambda (key value)
                   (unless value (setf (gethash key counts) 0)))
                 counts))
      response)))

(defun %response-with-a-false-turned-null (real)
  "Return a wrong BUILD-SPEC-CHECK-RESPONSE, for the negative control: a
verified of false is published as null.

Not an invented fault: this is what the worker's own JSON round trip does to
every false cl-mcp produces, because the parent parses the worker's answer
with YASON:PARSE and no arguments and re-encodes the NIL it gets.  Here the
builder is made to do it directly, so a check that reads the document can say
whether it would notice."
  (lambda (report)
    (let ((response (funcall real report)))
      (when (eq yason:false (gethash "verified" response))
        (setf (gethash "verified" response) nil))
      response)))

(defun %response-with-a-verified-headline (real)
  "Return a wrong BUILD-SPEC-CHECK-RESPONSE, for the negative control: the
first line says VERIFIED whatever the run did, while every field beside it
stays right.  Only a check that reads the text can see it."
  (lambda (report)
    (let* ((response (funcall real report))
           (content (gethash "content" response)))
      (when (and (vectorp content) (plusp (length content)))
        (let* ((part (aref content 0))
               (text (gethash "text" part))
               (break (position #\Newline text)))
          (setf (gethash "text" part)
                (concatenate 'string "✓ VERIFIED"
                             (if break (subseq text break) "")))))
      response)))

(defun %response-replaying-a-contract-as-a-property (real)
  "Return a wrong BUILD-SPEC-CHECK-RESPONSE, for the negative control: a
contract's replay line asks for property=, which names a property that does
not exist.  The line still looks like an instruction, and following it runs
nothing."
  (let ((from "spec-check function=")
        (to "spec-check property="))
    (lambda (report)
      (let* ((response (funcall real report))
             (content (gethash "content" response)))
        (when (and (vectorp content) (plusp (length content)))
          (let* ((part (aref content 0))
                 (text (gethash "text" part))
                 (at (search from text)))
            (when at
              (setf (gethash "text" part)
                    (concatenate 'string (subseq text 0 at) to
                                 (subseq text (+ at (length from))))))))
        response))))

(defun %declaration-with-required-arguments (real)
  "Return a wrong %DESCRIBE-FUNCTION-SPEC, for the negative control: every
argument is reported as required, which is what version 1's omitted :KIND is
normalized to -- and what an :OPTIONAL or :KEY argument is not."
  (lambda (api name registry max-chars)
    (let ((description (copy-list (funcall real api name registry max-chars))))
      (when (getf description :arguments)
        (setf (getf description :arguments)
              (loop for argument in (getf description :arguments)
                    collect (let ((copy (copy-list argument)))
                              (setf (getf copy :kind) :required)
                              copy))))
      description)))

(defun %declaration-claiming-whole-clauses (real)
  "Return a wrong %DESCRIBE-FUNCTION-SPEC, for the negative control: a
precondition that was cut is reported complete, so a reader takes the part it
can see for the whole condition."
  (lambda (api name registry max-chars)
    (let ((description (copy-list (funcall real api name registry max-chars))))
      (when (getf description :preconditions)
        (setf (getf description :preconditions-complete) t))
      description)))

(defun %digest-falling-back-from-a-refused-record (real)
  "Return a wrong DEFINITION-DIGEST, for the negative control: a record whose
own digest cannot be used -- incomplete, missing, or of a version this cl-mcp
does not know -- is digested from the readers instead, by handing REAL the
same record with its versioned metadata stripped off.  That reports a
definition as unchanged on the strength of a record that was refused."
  (lambda (api name registry &rest arguments)
    (multiple-value-bind (digest complete) (apply real api name registry arguments)
      (if digest
          (values digest complete)
          (let* ((key (getf arguments :data-key :property-data))
                 (data (ignore-errors (funcall (api-fn api key) name :registry registry)))
                 (stripped (loop for (indicator value) on data by #'cddr
                                 unless (member indicator
                                                '(:schema-version :record-kind :entity-kind
                                                  :definition-digest
                                                  :definition-digest-complete
                                                  :definition-digest-covers :capabilities))
                                   append (list indicator value))))
            (if stripped
                (funcall real api name registry :property stripped :data-key key)
                (values nil nil)))))))

(defun %negative-controls ()
  "Return the deliberately wrong implementations the negative control swaps in:
each names the function, its replacement, the targets to run, and the targets
that must answer :FAILED with a counterexample against it.  Two of them
overwrite their argument and return it, which a check comparing the result with
the argument as it is after the call cannot see."
  (let ((newline (%bundle-name :function-spec "ENSURE-TRAILING-NEWLINE"))
        (terminated (%bundle-name :property "ENSURE-TRAILING-NEWLINE-KEEPS-TERMINATED-TEXT"))
        (sanitize (%bundle-name :function-spec "SANITIZE-FOR-JSON"))
        (allowed (%bundle-name :property "SANITIZE-FOR-JSON-KEEPS-ALLOWED-TEXT"))
        (idempotent (%bundle-name :property "SANITIZE-FOR-JSON-IS-IDEMPOTENT"))
        (unmodified (%bundle-name :property "SANITIZE-FOR-JSON-LEAVES-ITS-ARGUMENT-UNMODIFIED"))
        (project-reads (%bundle-name :property "READ-ALLOWS-PROJECT-FILES-AS-THEMSELVES"))
        (dependency-reads (%bundle-name :property "READ-FOLLOWS-DEPENDENCY-REGISTRATION"))
        (link-reads (%bundle-name :property "READ-JUDGES-SYMLINKS-BY-THEIR-TARGET"))
        (denied-reads (%bundle-name :property "READ-DENIES-UNLISTED-REGIONS"))
        (project-writes (%bundle-name :property "WRITE-RESOLVES-PROJECT-TARGETS-WITHOUT-CREATING"))
        (refused-writes (%bundle-name :property "WRITE-REFUSES-OUTSIDE-AND-ABSOLUTE"))
        (link-writes (%bundle-name :property "WRITE-FOLLOWS-EXISTING-LINKS"))
        (writes (%bundle-name :property "WRITER-CHANGES-ONLY-THE-EXPECTED-ENTRIES"))
        (spellings (%bundle-name :property "WRITE-PRESERVES-SAFE-SPELLINGS"))
        (availability (%bundle-name :property
                                    "CORE-RECORD-AVAILABILITY-SEPARATES-ABSENCE-FROM-NIL"))
        (seeds (%bundle-name :property "CORE-RECORD-SEEDS-STAY-DECIMAL-TEXT"))
        (cuts (%bundle-name :property "CORE-RECORD-REPORTS-EVERY-CUT"))
        (counts (%bundle-name :property "CHECK-VERDICT-COUNTS-KEEP-EVERY-STATUS"))
        (effective (%bundle-name :property
                                 "CHECK-VERDICT-EFFECTIVE-TRIALS-ONLY-FROM-A-USABLE-COUNT"))
        (verified (%bundle-name :property
                                "CHECK-VERDICT-VERIFIED-NEEDS-EVIDENCE-FROM-EVERY-RESULT"))
        (gaps (%bundle-name :property
                            "CHECK-VERDICT-GAPS-NAME-EACH-SHORTFALL-AND-NOTHING-ELSE"))
        (selection (%bundle-name :property "CHECK-ROUTING-SELECTION-NAMES-ONLY-WHAT-WAS-ASKED"))
        (budget (%bundle-name :property "CHECK-ROUTING-BUDGET-COMES-FROM-ITS-STATED-SOURCE"))
        (seed-text (%bundle-name :property "CHECK-ROUTING-SEED-TEXT-KEEPS-EVERY-DIGIT"))
        (digest (%bundle-name :property "CHECK-ROUTING-DIGEST-COMPARISON-HAS-FOUR-ANSWERS"))
        (listing (%bundle-name :property
                               "SPEC-INSPECTION-LISTING-SEPARATES-CAPABILITY-FROM-COUNT"))
        (declaration (%bundle-name :property
                                   "SPEC-INSPECTION-CONTRACT-DECLARATION-SURVIVES-DESCRIBE"))
        (digest-source (%bundle-name :property
                                     "SPEC-INSPECTION-DIGEST-COMES-FROM-THE-RECORD-OR-THE-READERS"))
        (response-list (%bundle-name :property
                                     "SPEC-LIST-RESPONSE-SAYS-WHY-A-KIND-HAS-NO-NAMES"))
        (response-check
          (%bundle-name :property
                        "SPEC-CHECK-RESPONSE-CARRIES-THE-VERDICT-AND-ITS-RESERVATIONS"))
        (response-replay
          (%bundle-name :property "SPEC-CHECK-REPLAY-LINE-ASKS-FOR-THE-RUN-IT-REPORTS"))
        ;; Taken before any swap, so a wrong implementation can defer to it.
        (real-read (fdefinition 'allowed-read-path))
        (real-write (fdefinition 'ensure-write-path))
        (real-availability (fdefinition 'field-availability))
        (real-projection (fdefinition 'project-record))
        (real-core-record (fdefinition 'project-core-record))
        (real-counts (fdefinition '%counts))
        (real-contract-plist (fdefinition '%contract-plist))
        (real-verified (fdefinition '%verified-p))
        (real-gaps (fdefinition '%verification-gaps))
        (real-selection (fdefinition '%select-properties))
        (real-budget (fdefinition '%trials-budget))
        (real-seed (fdefinition 'parse-seed-string))
        (real-match (fdefinition '%definition-match))
        (real-listing (fdefinition 'list-report))
        (real-row (fdefinition '%property-listing))
        (real-list-response (fdefinition 'build-spec-list-response))
        (real-check-response (fdefinition 'build-spec-check-response))
        (real-declaration (fdefinition '%describe-function-spec))
        (real-digest (fdefinition 'definition-digest)))
    (list
     (list :function newline
           :description "returns its argument, never adding a newline"
           :replacement #'identity
           :targets (list (list :function-spec newline) (list :property terminated))
           :must-fail (list (list :function-spec newline)))
     (list :function newline
           :description "overwrites its argument with newlines and returns it"
           :replacement (lambda (text)
                          (if (zerop (length text))
                              (string #\Newline)
                              (fill text #\Newline)))
           :targets (list (list :function-spec newline) (list :property terminated))
           :must-fail (list (list :function-spec newline) (list :property terminated)))
     (list :function sanitize
           :description "returns the empty string for every argument but NIL"
           :replacement (lambda (value) (and value (make-string 0)))
           :targets (list (list :property allowed) (list :property idempotent))
           :must-fail (list (list :property allowed)))
     (list :function sanitize
           :description "overwrites a string argument with a's and returns it"
           :replacement (lambda (value) (if (stringp value) (fill value #\a) value))
           :targets (list (list :property allowed) (list :property unmodified))
           :must-fail (list (list :property allowed) (list :property unmodified)))
     ;; Read paths.  Only ALLOWED-READ-PATH is replaced: RESOLVE-READABLE-PATH
     ;; calls it for its decision.  Nothing is ever written to a denied path.
     (list :function 'allowed-read-path
           :description "returns NIL for every path"
           :replacement (lambda (path) (declare (ignore path)) nil)
           :targets (list (list :property project-reads) (list :property dependency-reads))
           :must-fail (list (list :property project-reads)
                            (list :property dependency-reads)))
     (list :function 'allowed-read-path
           :description "allows the project only, ignoring registered ASDF systems"
           :replacement #'%read-path-ignoring-dependencies
           :targets (list (list :property project-reads) (list :property dependency-reads))
           :must-fail (list (list :property dependency-reads)))
     (list :function 'allowed-read-path
           :description "allows any path written inside the project, wherever it leads"
           :replacement (%read-path-trusting-spelling real-read)
           :targets (list (list :property link-reads))
           :must-fail (list (list :property link-reads)))
     (list :function 'allowed-read-path
           :description "treats a string prefix of the project root as containment"
           :replacement (%read-path-by-string-prefix real-read)
           :targets (list (list :property denied-reads))
           :must-fail (list (list :property denied-reads)))
     ;; Write paths.  Only ENSURE-WRITE-PATH is replaced: FS-WRITE-FILE calls it
     ;; for its decision.  Every argument a check passes lies in the check's own
     ;; scratch tree, so a wrong implementation that lets a write through
     ;; writes there, and the fixture removes it after the check has seen it.
     (list :function 'ensure-write-path
           :description "refuses every path"
           :replacement #'%refuse-write-for-control
           :targets (list (list :property project-writes) (list :property writes)
                          (list :property spellings))
           :must-fail (list (list :property project-writes) (list :property writes)
                            (list :property spellings)))
     (list :function 'ensure-write-path
           :description "resolves lexically, as before the fix, trusting new names below a link"
           :replacement #'%write-path-lexically
           :targets (list (list :property link-writes) (list :property writes))
           :must-fail (list (list :property link-writes) (list :property writes)))
     (list :function 'ensure-write-path
           :description "allows whatever the read policy allows"
           :replacement #'%write-path-by-read-policy
           :targets (list (list :property refused-writes))
           :must-fail (list (list :property refused-writes)))
     ;; The project property runs too, to show whether the existing generated
     ;; checks see this fault: no existing generator spells a doubled separator.
     (list :function 'ensure-write-path
           :description "sends a spelling with a doubled separator to the file beside the right one"
           :replacement (%write-path-misreading-doubled-separators real-write)
           :targets (list (list :property spellings) (list :property project-writes))
           :must-fail (list (list :property spellings)))
     ;; Record fidelity.  Each wrong function calls the real one and bends one
     ;; rule of its answer, so the fault is that rule and nothing else.  The
     ;; properties call these functions on records they build; nothing reaches
     ;; the filesystem or the MCP server.
     (list :function 'field-availability
           :description "reads a key present with NIL as absent"
           :replacement (%availability-reading-nil-as-absent real-availability)
           :targets (list (list :property availability))
           :must-fail (list (list :property availability)))
     (list :function 'project-record
           :description "turns a record's seed back into a JSON number"
           :replacement (%project-record-with-numeric-seeds real-projection)
           :targets (list (list :property seeds))
           :must-fail (list (list :property seeds)))
     (list :function 'project-core-record
           :description "claims a complete projection whatever its issues say"
           :replacement (%core-record-claiming-completeness real-core-record)
           :targets (list (list :property cuts))
           :must-fail (list (list :property cuts)))
     (list :function 'project-record
           :description "keeps the tail of an over-long list, reporting the cut correctly"
           :replacement (%project-record-keeping-tails real-projection)
           :targets (list (list :property cuts))
           :must-fail (list (list :property cuts)))
     ;; Verdicts.  The same shape: each wrong function calls the real one and
     ;; bends one rule of its answer.  The properties call these internal
     ;; functions on results they build, on their own thread; nothing goes
     ;; through CHECK-REPORT, a worker or the MCP server.
     (list :function '%counts
           :description "drops statuses without a field of their own from other and by-status"
           :replacement (%counts-dropping-unnamed-statuses real-counts)
           :targets (list (list :property counts))
           :must-fail (list (list :property counts)))
     (list :function '%contract-plist
           :description "publishes raw trials as effective trials when the count is unusable"
           :replacement (%contract-plist-falling-back-to-raw-trials real-contract-plist)
           :targets (list (list :property effective))
           :must-fail (list (list :property effective)))
     (list :function '%verified-p
           :description "verifies an empty list of results"
           :replacement (%verified-when-nothing-ran real-verified)
           :targets (list (list :property verified))
           :must-fail (list (list :property verified)))
     (list :function '%verified-p
           :description "verifies a contract whose declared case was never reached"
           :replacement (%verified-ignoring-unreached-cases real-verified)
           :targets (list (list :property verified))
           :must-fail (list (list :property verified)))
     (list :function '%verification-gaps
           :description "measures refusals when any one result, not every one, has a usable count"
           :replacement (%gaps-measuring-rejections-from-any-contract real-gaps)
           :targets (list (list :property gaps))
           :must-fail (list (list :property gaps)))
     ;; Routing.  The same shape again.  Each fault lies in a state every
     ;; trial of its property runs: symbol= against a registry with a contract
     ;; and a same-named property, a profile entry beside a different
     ;; default, the seed 2^53+1, an incomplete digest equal to the expected.
     (list :function '%select-properties
           :description "runs a symbol's own contract or same-named property on symbol="
           :replacement (%selection-adding-the-subject real-selection)
           :targets (list (list :property selection))
           :must-fail (list (list :property selection)))
     (list :function '%trials-budget
           :description "ignores the profile entry and explicit trials for the backend default"
           :replacement (%budget-from-the-backend-only real-budget)
           :targets (list (list :property budget))
           :must-fail (list (list :property budget)))
     (list :function 'parse-seed-string
           :description "takes the seed through a double, rounding it past 2^53"
           :replacement (%seed-through-a-double real-seed)
           :targets (list (list :property seed-text))
           :must-fail (list (list :property seed-text)))
     (list :function '%definition-match
           :description "counts an incomplete digest equal to the expected one as a match"
           :replacement (%incomplete-digest-matching real-match)
           :targets (list (list :property digest))
           :must-fail (list (list :property digest)))
     ;; Inspection.  Each fault is a false negative or a false completeness:
     ;; something unread reported as something known.
     (list :function 'list-report
           :description "reports a count it never looked for as zero"
           :replacement (%listing-counting-what-it-did-not-look-at real-listing)
           :targets (list (list :property listing))
           :must-fail (list (list :property listing)))
     (list :function '%property-listing
           :description "reads a listing row without the registry it was given"
           :replacement (%listing-reading-another-registry real-row)
           :targets (list (list :property listing))
           :must-fail (list (list :property listing)))
     (list :function '%describe-function-spec
           :description "reports every contract argument as required"
           :replacement (%declaration-with-required-arguments real-declaration)
           :targets (list (list :property declaration))
           :must-fail (list (list :property declaration)))
     (list :function '%describe-function-spec
           :description "reports a precondition that was cut as complete"
           :replacement (%declaration-claiming-whole-clauses real-declaration)
           :targets (list (list :property declaration))
           :must-fail (list (list :property declaration)))
     ;; Responses.  Each fault is one a caller acts on: a count nobody took
     ;; published as none, a measurement published as absence, a headline that
     ;; disagrees with its own payload, and an instruction that runs nothing.
     (list :function 'build-spec-list-response
           :description "publishes a count nobody took as zero"
           :replacement (%response-counting-what-nobody-looked-at real-list-response)
           :targets (list (list :property response-list))
           :must-fail (list (list :property response-list)))
     (list :function 'build-spec-check-response
           :description "publishes a verified of false as null"
           :replacement (%response-with-a-false-turned-null real-check-response)
           :targets (list (list :property response-check))
           :must-fail (list (list :property response-check)))
     (list :function 'build-spec-check-response
           :description "opens every headline with VERIFIED"
           :replacement (%response-with-a-verified-headline real-check-response)
           :targets (list (list :property response-check))
           :must-fail (list (list :property response-check)))
     (list :function 'build-spec-check-response
           :description "replays a contract as property="
           :replacement (%response-replaying-a-contract-as-a-property real-check-response)
           :targets (list (list :property response-replay))
           :must-fail (list (list :property response-replay)))
     (list :function 'definition-digest
           :description "digests from the readers when the record's own digest was refused"
           :replacement (%digest-falling-back-from-a-refused-record real-digest)
           :targets (list (list :property digest-source))
           :must-fail (list (list :property digest-source))))))

(defun %call-with-replaced-function (symbol replacement thunk)
  "Call THUNK with SYMBOL's global function replaced by REPLACEMENT, and put
the original back however THUNK exits."
  (let ((original (fdefinition symbol)))
    (unwind-protect
         (progn (setf (fdefinition symbol) replacement)
                (funcall thunk))
      (setf (fdefinition symbol) original))))

(defun %entry-for (report kind name)
  "Return REPORT's entry for the target KIND NAME."
  (find-if (lambda (entry) (and (eq kind (getf entry :kind)) (eq name (getf entry :name))))
           (getf report :entries)))

(defun %negative-control-outcome (control run)
  "Run CONTROL's targets with RUN against its wrong implementation, then against
the real one, and return what each run found."
  (destructuring-bind (&key function description replacement targets must-fail) control
    (let ((wrong (%call-with-replaced-function function replacement
                                               (lambda () (funcall run targets))))
          (right (funcall run targets)))
      (flet ((field (report target key)
               (getf (%entry-for report (first target) (second target)) key)))
        (list :function function
              :description description
              :must-fail must-fail
              :wrong wrong
              :right right
              :detected (every (lambda (target) (eq :failed (field wrong target :status)))
                               must-fail)
              :same-digests (every (lambda (target)
                                     (let ((digest (field wrong target :digest)))
                                       (and digest
                                            (equal digest (field right target :digest)))))
                                   targets))))))

(defun run-negative-control (&key (seed (first *default-seeds*))
                               (profile *default-profile*) (trials *default-trials*)
                               (timeout-seconds *default-timeout-seconds*) expected-root)
  "Check that the bundle's own contracts and properties catch deliberately wrong
implementations, then that they pass again once the real one is back.

RUN THIS ONLY IN A PROCESS OF ITS OWN: it replaces production functions'
global definitions while it runs, and although it restores them on every exit,
any other thread in the image would see the wrong ones meanwhile.
scripts/check-specs.lisp runs it as a separate process.

Passes only when every must-fail target answered :FAILED -- a counterexample,
not an error -- against the wrong implementation, every target passed against
the real one, and both runs used definitions with the same digests."
  (let ((registry (make-hash-table-registry)))
    (register-specifications registry)
    (let* ((run (lambda (targets)
                  (run-checks targets :registry registry :seeds (list seed)
                                      :profile profile :trials trials
                                      :timeout-seconds timeout-seconds)))
           (functions (covered-functions registry))
           (outcomes (loop for control in (%negative-controls)
                           collect (%negative-control-outcome control run)))
           (problems (source-problems expected-root functions)))
      (list :mode :negative-control
            :ok (and (null problems)
                     (every (lambda (outcome)
                              (and (getf outcome :detected)
                                   (report-ok-p (getf outcome :right))
                                   (getf outcome :same-digests)))
                            outcomes))
            :environment (environment-record expected-root)
            :sources (source-record functions)
            :problems problems
            :outcomes outcomes))))

;;; ------------------------------------------------------------------------
;;; Reporting

(defun %git-summary (record)
  "Return a one-line summary of a %SYSTEM-RECORD."
  (destructuring-bind (&key version directory git) record
    (let ((changes (getf git :changes))
          (lisp-changes (getf git :lisp-changes)))
      (format nil "~@[~A ~]~A rev ~A (~A~@[; ~A~])" version directory (getf git :revision)
              (cond ((eq changes :unknown) "local changes unknown")
                    ((null changes) "no local changes")
                    (t (format nil "~D local change~:P" (length changes))))
              (cond ((eq lisp-changes :unknown) "Lisp source contents unknown")
                    ((consp lisp-changes)
                     (format nil "~D Lisp file~:P differ from HEAD, fingerprint ~A"
                             (length lisp-changes) (getf git :lisp-fingerprint))))))))

(defun %print-environment (report stream)
  "Print REPORT's environment and source records to STREAM."
  (let ((environment (getf report :environment)))
    (format stream "Lisp: ~A   ASDF: ~A~%Backend: ~A~%"
            (getf environment :lisp) (getf environment :asdf) (getf environment :backend))
    (dolist (system '(:cl-mcp :cl-spec :check-it))
      (let ((changes (getf (getf (getf environment system) :git) :changes)))
        (format stream "~(~A~): ~A~%" system (%git-summary (getf environment system)))
        (when (listp changes)
          (dolist (change (subseq changes 0 (min 20 (length changes))))
            (format stream "    ~A~%" (%safe-text change))))
        (let ((lisp-changes (getf (getf (getf environment system) :git) :lisp-changes)))
          (when (consp lisp-changes)
            (dolist (row (subseq lisp-changes 0 (min 40 (length lisp-changes))))
              (format stream "    ~A ~A ~A~%" (first row) (%safe-text (second row))
                      (if (stringp (third row)) (subseq (third row) 0 12) (third row))))))))
    (format stream "Expected checkout: ~A~%"
            (or (getf environment :expected-root) "not checked"))
    (dolist (source (getf report :sources))
      (format stream "  ~A loaded from ~{~A~^, ~}~%"
              (%qualified-name (getf source :name)) (getf source :files)))))

(defun %replay-lines (entry stream)
  "Print how to replay ENTRY from Lisp and from an MCP client."
  (let ((name (%qualified-name (getf entry :name)))
        (seed (getf entry :seed))
        (digest (getf entry :digest)))
    (ecase (getf entry :kind)
      (:property
       (format stream "      replay (Lisp): (cl-spec:run-property '~A :profile ~S :seed ~D)~%"
               name (getf entry :profile) seed)
       (format stream "      replay (MCP):  spec-check property=~A profile=~(~A~) seed=\"~D\"~
                       ~@[ expect_definition_digest=~A~]~%"
               name (getf entry :profile) seed digest))
      (:function-spec
       (format stream "      replay (Lisp): (cl-spec:check-function '~A :trials ~D :seed ~D)~%"
               name (getf entry :requested-trials) seed)
       (format stream "      replay (MCP):  spec-check function=~A trials=~D seed=\"~D\"~
                       ~@[ expect_definition_digest=~A~]~%"
               name (getf entry :requested-trials) seed digest)))))

(defun %print-entry (entry stream)
  "Print one target run ENTRY."
  (let ((problems (judge-entry entry))
        (report (getf entry :case-report)))
    (format stream "[~:[PASS~;FAIL~]] ~(~A~) ~A seed ~D ~A~%"
            problems (getf entry :kind) (%qualified-name (getf entry :name)) (getf entry :seed)
            (if (eq :property (getf entry :kind))
                (format nil "profile ~S" (getf entry :profile))
                (format nil "trials ~D" (getf entry :requested-trials))))
    (format stream "      status ~S  trials ~S  budget ~S  rejected ~S  shrinking ~S~%"
            (getf entry :status) (getf entry :trials) (getf entry :budget)
            (getf entry :rejected) (getf (getf entry :capabilities) :shrinking))
    (when (getf entry :digest)
      (format stream "      digest ~A (~:[incomplete~;complete~])~%"
              (getf entry :digest) (getf entry :digest-complete)))
    (when (getf entry :error-output-lines)
      (format stream "      stderr ~D line~:P captured (first line only kept): ~A~%"
              (getf entry :error-output-lines) (getf entry :error-output-sample)))
    (dolist (row (getf entry :warnings))
      (format stream "      warning ~A x~D~%" (getf row :type) (getf row :count))
      (dolist (text (getf row :reports))
        ;; Whole for a failed run, where a warning may explain it; a line otherwise.
        (format stream "        ~A~%" (%safe-text text :limit (if problems 2000 160)))))
    (when (getf entry :declared-cases)
      (format stream "      cases ~{~A~^, ~}~%"
              (if (consp report)
                  (loop for row in (getf report :cases)
                        collect (format nil "~S called ~S" (getf row :name) (getf row :called)))
                  (list (%render report)))))
    (when problems
      (format stream "      problems ~A~%" (%render problems))
      (dolist (key '(:counterexample :shrunk-counterexample :shrunk-outcome :failure-phase
                     :failure-reason :condition :declared-trials))
        (when (getf entry key)
          (format stream "      ~(~A~) ~A~%" key (%render (getf entry key)))))
      (%replay-lines entry stream))))

(defun %print-check-report (report stream)
  "Print a :CHECK REPORT."
  (let ((selection (getf report :selection)))
    (format stream "Selection: the bundle's ~D target~:P under seeds ~{~D~^ ~}~%"
            (length (getf selection :targets)) (getf selection :seeds))
    (format stream "  properties: profile ~S, from each property's :trials table~%"
            (getf selection :profile))
    (format stream "  function specs: ~D trials each; deadline ~Ds per target~%"
            (getf selection :trials) (getf selection :timeout-seconds))
    (dolist (entry (getf report :entries))
      (%print-entry entry stream))
    (format stream "Examples (CHECK-CALL, one call each, not counted as trials):~%")
    (dolist (entry (getf report :examples))
      (let ((problems (judge-example entry)))
        (format stream "[~:[PASS~;FAIL~]] ~A ~A~@[ case ~S~]~@[  problems ~A~]~%"
                problems (%qualified-name (getf entry :name))
                (%render (getf entry :arguments) :limit 80) (getf entry :case)
                (and problems (%render problems)))))))

(defun %print-negative-control-report (report stream)
  "Print a :NEGATIVE-CONTROL REPORT."
  (dolist (outcome (getf report :outcomes))
    (format stream "~&Wrong ~A: ~A~%" (%qualified-name (getf outcome :function))
            (getf outcome :description))
    (format stream "  detected by ~{~A~^, ~}: ~:[NO~;yes~]~%"
            (mapcar (lambda (target) (%qualified-name (second target)))
                    (getf outcome :must-fail))
            (getf outcome :detected))
    (format stream "  same digests in both runs: ~:[NO~;yes~]~%" (getf outcome :same-digests))
    (format stream "  -- against the wrong implementation:~%")
    (dolist (entry (getf (getf outcome :wrong) :entries))
      (%print-entry entry stream))
    (format stream "  -- against the real implementation (must pass):~%")
    (dolist (entry (getf (getf outcome :right) :entries))
      (%print-entry entry stream))))

(defun print-report (report &optional (stream *standard-output*))
  "Print REPORT, from RUN-BUNDLE or RUN-NEGATIVE-CONTROL, to STREAM."
  (format stream "~&cl-mcp/specs ~(~A~): ~:[FAILED~;PASSED~]~%"
          (getf report :mode) (report-ok-p report))
  (%print-environment report stream)
  (ecase (getf report :mode)
    (:check (%print-check-report report stream))
    (:negative-control (%print-negative-control-report report stream)))
  (format stream "Problems:~:[ none~;~]~%" (getf report :problems))
  (dolist (problem (getf report :problems))
    (format stream "  ~A~%" (%render problem)))
  (format stream "Result: ~:[FAILED~;PASSED~]~%" (report-ok-p report))
  (finish-output stream))

(defun %printable (value)
  "Return VALUE with symbols as qualified names and strings made safe, so the
report file reads back without any package of this bundle."
  (typecase value
    (null nil)
    (keyword value)
    (symbol (if (eq value t) t (%qualified-name value)))
    (string (%safe-text value :limit 2000))
    (cons (cons (%printable (car value)) (%printable (cdr value))))
    ((or number character) value)
    (t (%render value))))

(defun write-report (report pathname)
  "Write REPORT to PATHNAME as one readable form."
  (with-open-file (out pathname :direction :output :if-exists :supersede
                                :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*print-readably* nil))
        (prin1 (%printable report) out)
        (terpri out)))))

(defun main (&key (mode :check) expected-root report-pathname)
  "Run MODE -- :CHECK or :NEGATIVE-CONTROL -- print its report, write it to
REPORT-PATHNAME when given, and return the process exit code: 0 only when the
report passed."
  (let ((report (ecase mode
                  (:check (run-bundle :expected-root expected-root))
                  (:negative-control (run-negative-control :expected-root expected-root)))))
    (print-report report)
    (when report-pathname
      (write-report report report-pathname))
    (exit-code report)))
