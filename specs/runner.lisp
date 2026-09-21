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
           #:report-ok-p
           #:exit-code
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
             (multiple-value-bind (result failure)
                 (%call-with-deadline timeout-seconds
                                      (lambda ()
                                        (run-property name :profile profile :seed seed
                                                           :registry registry)))
               (append base (or failure (%result-record result)))))))))

(defun %declared-cases (name registry)
  "Return the case names the Function Spec NAME declares, in order."
  (mapcar (lambda (case) (getf case :name))
          (getf (function-spec-data name :registry registry) :cases)))

(defun %run-function-target (name registry seed trials timeout-seconds)
  "Run the Function Spec NAME under SEED with TRIALS; return its entry."
  (let ((base (list :kind :function-spec :name name :seed seed :requested-trials trials)))
    (if (not (nth-value 1 (find-function-spec name registry)))
        (append base (list :status :not-registered))
        (multiple-value-bind (result failure)
            (%call-with-deadline timeout-seconds
                                 (lambda ()
                                   (check-function name :trials trials :seed seed
                                                        :registry registry)))
          (append base
                  (list :declared-cases (%declared-cases name registry))
                  (or failure (%result-record result)))))))

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
count below it, and -- for a contract with :CASES -- a measured case report in
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

(defun source-record ()
  "Return, for each function the bundle contracts, the files its loaded
definition came from."
  (loop for name in (contract-names)
        collect (list :name name :files (mapcar #'%native (%definition-files name)))))

(defun source-problems (expected-root)
  "Return problems when the cl-mcp under check was not loaded from
EXPECTED-ROOT: the cl-mcp system's directory differs, or a contracted
function's definition lives elsewhere.  NIL when EXPECTED-ROOT is NIL."
  (when expected-root
    (let* ((root (truename expected-root))
           (system-directory (asdf:system-source-directory "cl-mcp"))
           (problems (unless (and system-directory (equal (truename system-directory) root))
                       (list (list :cl-mcp-loaded-from (%native system-directory)
                                   :expected (%native root))))))
      (dolist (name (contract-names))
        (let ((files (%definition-files name)))
          (if (null files)
              (push (list :source-unknown name) problems)
              (dolist (file files)
                (let ((truename (ignore-errors (truename file))))
                  (unless (and truename (uiop:subpathp truename root))
                    (push (list :source-outside-checkout name (%native file)) problems)))))))
      (nreverse problems))))

(defun %git-state (directory)
  "Return the git revision of DIRECTORY and its uncommitted changes, or :UNKNOWN."
  (flet ((git (&rest arguments)
           (handler-case
               (multiple-value-bind (output error-output code)
                   (uiop:run-program (list* "git" "-C" (%native directory) arguments)
                                     :output '(:string :stripped t)
                                     :error-output nil
                                     :ignore-error-status t)
                 (declare (ignore error-output))
                 (and (eql code 0) output))
             (error () nil))))
    (if (null directory)
        (list :revision :unknown)
        (let ((revision (git "rev-parse" "HEAD"))
              (status (git "status" "--porcelain")))
          (list :revision (or revision :unknown)
                :changes (if status
                             (remove "" (uiop:split-string status :separator '(#\Newline))
                                     :test #'string=)
                             :unknown))))))

(defun %system-record (name)
  "Return the version, directory and git state of the ASDF system NAME."
  (let* ((system (asdf:find-system name nil))
         (directory (and system (asdf:system-source-directory system))))
    (list :version (and system (asdf:component-version system))
          :directory (%native directory)
          :git (%git-state directory))))

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
    (let* ((report (run-checks (bundle-targets) :registry registry :seeds seeds
                                                :profile profile :trials trials
                                                :timeout-seconds timeout-seconds
                                                :examples (call-examples)))
           (problems (append (getf report :problems)
                             (bundle-consistency-problems registry)
                             (source-problems expected-root))))
      (setf (getf report :problems) problems
            (getf report :ok) (null problems))
      (list* :mode :check
             :environment (environment-record expected-root)
             :sources (source-record)
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

(defun %negative-controls ()
  "Return the deliberately wrong implementations the negative control swaps in:
each names the function, its replacement, the targets to run, and the targets
that must answer :FAILED with a counterexample against it."
  (let ((newline (%bundle-name :function-spec "ENSURE-TRAILING-NEWLINE"))
        (terminated (%bundle-name :property "ENSURE-TRAILING-NEWLINE-KEEPS-TERMINATED-TEXT"))
        (sanitize (%bundle-name :function-spec "SANITIZE-FOR-JSON"))
        (allowed (%bundle-name :property "SANITIZE-FOR-JSON-KEEPS-ALLOWED-TEXT"))
        (idempotent (%bundle-name :property "SANITIZE-FOR-JSON-IS-IDEMPOTENT")))
    (list
     (list :function newline
           :description "returns its argument, never adding a newline"
           :replacement #'identity
           :targets (list (list :function-spec newline) (list :property terminated))
           :must-fail (list (list :function-spec newline)))
     (list :function sanitize
           :description "returns the empty string for every argument but NIL"
           :replacement (lambda (value) (and value (make-string 0)))
           :targets (list (list :property allowed) (list :property idempotent))
           :must-fail (list (list :property allowed))))))

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
           (outcomes (loop for control in (%negative-controls)
                           collect (%negative-control-outcome control run)))
           (problems (source-problems expected-root)))
      (list :mode :negative-control
            :ok (and (null problems)
                     (every (lambda (outcome)
                              (and (getf outcome :detected)
                                   (report-ok-p (getf outcome :right))
                                   (getf outcome :same-digests)))
                            outcomes))
            :environment (environment-record expected-root)
            :sources (source-record)
            :problems problems
            :outcomes outcomes))))

;;; ------------------------------------------------------------------------
;;; Reporting

(defun %git-summary (record)
  "Return a one-line summary of a %SYSTEM-RECORD."
  (destructuring-bind (&key version directory git) record
    (let ((changes (getf git :changes)))
      (format nil "~@[~A ~]~A rev ~A (~A)" version directory (getf git :revision)
              (cond ((eq changes :unknown) "local changes unknown")
                    ((null changes) "no local changes")
                    (t (format nil "~D local change~:P" (length changes))))))))

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
            (format stream "    ~A~%" (%safe-text change))))))
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
