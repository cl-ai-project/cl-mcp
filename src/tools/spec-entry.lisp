;;;; src/tools/spec-entry.lisp
;;;;
;;;; One place where a request becomes a response: resolve the cl-spec API,
;;;; build the report, project it into a hash-table.
;;;;
;;;; Separate from SRC/TOOLS/SPEC-TOOLS.LISP because the worker handlers need
;;;; the same three steps and must not drag the proxy and the tool registry
;;;; into the worker image to get them.

(defpackage #:cl-mcp/src/tools/spec-entry
  (:use #:cl)
  (:import-from #:cl-mcp/src/spec-adapter-core
                #:resolve-cl-spec-api)
  (:import-from #:cl-mcp/src/spec-adapter-report
                #:list-report
                #:symbol-report
                #:describe-report
                #:check-report)
  (:import-from #:cl-mcp/src/tools/spec-response-builders
                #:build-spec-list-response
                #:build-spec-symbol-response
                #:build-spec-describe-response
                #:build-spec-check-response)
  (:import-from #:cl-mcp/src/utils/deadline
                #:call-with-deadline-thread)
  (:export #:spec-list-response
           #:spec-symbol-response
           #:spec-describe-response
           #:spec-check-response
           #:parse-seed-string
           #:*default-introspection-timeout-seconds*))

(in-package #:cl-mcp/src/tools/spec-entry)

(defun parse-seed-string (text)
  "Return (values SEED NIL) for a decimal seed TEXT, or (values NIL MESSAGE).

A seed arrives as text and never as a JSON number.  cl-spec draws seeds below
2 to the 62nd, and a JSON number that large has already lost digits by the
time it reaches here -- accepting one would mean accepting a seed that cannot
reproduce anything and reporting it as if it could.

PARSE-INTEGER rather than the reader: this is a tool argument from outside the
image, and nothing about a seed calls for reader macros."
  (cond
    ((null text) (values nil nil))
    ((not (stringp text))
     (values nil "seed must be a string of decimal digits"))
    ((zerop (length text))
     (values nil "seed must not be empty"))
    ((notevery #'digit-char-p text)
     (values nil (format nil "seed must be decimal digits only, got ~S. A ~
cl-spec seed can exceed what JSON holds exactly as a number, so it travels as ~
text." text)))
    (t (handler-case (values (parse-integer text) nil)
         (error () (values nil (format nil "seed ~S is not an integer" text)))))))

(defun %string-arg (params name)
  "Return the string argument NAME from PARAMS, or NIL when absent or empty."
  (let ((value (and params (gethash name params))))
    (when (and (stringp value) (plusp (length value))) value)))

(defun %positive-integer-arg (params name default)
  "Return (values N NIL) for a positive integer argument, or (values NIL MESSAGE).

An absent argument takes DEFAULT.  Validated here rather than left to the
consumer: a negative character budget reached SUBSEQ as an end index and
signalled a type error, which the layer above then reported as \"this name is
not registered\"."
  (let ((value (and params (gethash name params))))
    (cond
      ((null value) (values default nil))
      ((and (integerp value) (plusp value)) (values value nil))
      (t (values nil (format nil "~A must be a positive integer, got ~S"
                             name value))))))

(defvar *maximum-trials* 1000000
  "Largest trial count spec-check will pass to a contract run.

An upper bound because the run happens on a deadline thread that cannot always
be stopped: a budget of a hundred million outlives its timeout, keeps calling
the target, and does it inside the worker this session's repl-eval and
load-system share.  The deadline bounds how long the caller waits; only this
bounds what the worker is left doing afterwards.  PROFILE never offered a
caller-supplied number, so TRIALS is the first argument on this path that
needed one.")

(defun %bounded-integer-arg (params name maximum)
  "Return (values N NIL) for a positive integer at most MAXIMUM, else an error."
  (multiple-value-bind (value message) (%positive-integer-arg params name nil)
    (cond
      (message (values nil message))
      ((and value (> value maximum))
       (values nil (format nil "~A must be at most ~:D; a larger budget can ~
outlive its timeout and go on calling the function in this worker"
                           name maximum)))
      (t (values value nil)))))

(defun %argument-error-response (message builder)
  "Return BUILDER's response for an argument MESSAGE, before cl-spec is asked."
  (funcall builder
           (list :status :invalid-arguments :verified nil :message message
                 :environment (%environment-stub))))

(defun %environment-stub ()
  "Return the environment plist for an answer given before cl-spec was asked.

An argument that is wrong is wrong whatever cl-spec is doing, and resolving
the API only to report a bad seed would put a version and a backend name on a
response that never looked at either."
  (list :cl-spec-loaded nil
        :cl-spec-status :not-consulted
        :lisp (format nil "~A ~A"
                      (lisp-implementation-type)
                      (lisp-implementation-version))))

(defvar *default-introspection-timeout-seconds* 30
  "Budget for spec-symbol and spec-describe when the caller names none.

They read rather than run, but reading is not free: a listing digests every
property registered about a symbol, and a digest walks that property's whole
transitive spec closure and prints it.  Without a deadline a large registry
could hold the worker past the proxy's own ceiling, and a proxy timeout is not
a timeout report -- it kills the worker and resets the session's Lisp state.")

(defun %within-deadline (params builder thunk)
  "Run THUNK under the request's deadline and build its answer.

CALL-WITH-DEADLINE-THREAD answers :OK, :TIMEOUT or :ERROR, and the three are
kept apart here.  Folding :ERROR into the timeout report told a caller its
registry was too large to read when what actually happened was a bug in this
adapter -- the same mislabelling DESCRIBE-REPORT's blanket handler used to
make, in a second place."
  (multiple-value-bind (seconds message)
      (%positive-integer-arg params "timeout_seconds"
                             *default-introspection-timeout-seconds*)
    (if message
        (%argument-error-response message builder)
        (multiple-value-bind (value status leaked)
            (call-with-deadline-thread thunk seconds :name "mcp-spec-read")
          (ecase status
            (:ok (first value))
            (:timeout
             (funcall builder
                      (list :status :timeout
                            :verified nil
                            :message
                            (format nil "reading the registry exceeded its ~
~A second deadline~:[ and its thread was stopped~; and its thread could not be ~
stopped, so this worker should be replaced~]. Nothing was read; raise ~
timeout_seconds or narrow the request."
                                    seconds leaked)
                            :environment (%environment-stub))))
            (:error
             (funcall builder
                      (list :status :internal-error
                            :verified nil
                            :message
                            (format nil "reading the registry failed in ~
cl-mcp: ~A" value)
                            :environment (%environment-stub)))))))))

(defun spec-list-response (params)
  "Return the spec-list response hash-table for PARAMS."
  (multiple-value-bind (limit message)
      (%positive-integer-arg params "limit" 200)
    (if message
        (%argument-error-response message #'build-spec-list-response)
        (%within-deadline
         params #'build-spec-list-response
         (lambda ()
           (multiple-value-bind (api status) (resolve-cl-spec-api)
             (build-spec-list-response
              (list-report api status
                           :kind (or (%string-arg params "kind") "both")
                           :package (%string-arg params "package")
                           :tag (%string-arg params "tag")
                           :limit limit))))))))

(defun spec-symbol-response (params)
  "Return the spec-symbol response hash-table for PARAMS."
  (%within-deadline
   params #'build-spec-symbol-response
   (lambda ()
     (multiple-value-bind (api status) (resolve-cl-spec-api)
       (build-spec-symbol-response
        (symbol-report api status (gethash "symbol" params)
                       :package (%string-arg params "package")
                       :include-runtime (multiple-value-bind (value present)
                                            (gethash "include_runtime" params)
                                          (if present value t))))))))

(defun spec-describe-response (params)
  "Return the spec-describe response hash-table for PARAMS."
  (multiple-value-bind (max-chars message)
      (%positive-integer-arg params "max_chars" 8000)
    (if message
        (%argument-error-response message #'build-spec-describe-response)
        (%within-deadline
         params #'build-spec-describe-response
         (lambda ()
           (multiple-value-bind (api status) (resolve-cl-spec-api)
             (build-spec-describe-response
              (describe-report api status
                               (gethash "kind" params)
                               (gethash "name" params)
                               :package (%string-arg params "package")
                               :max-chars max-chars))))))))

(defun spec-check-response (params)
  "Return the spec-check response hash-table for PARAMS.

An unusable seed is answered here rather than passed on: a run started with a
seed the caller did not mean is a run whose result means nothing.  TRIALS is
checked in the same place and for the same reason: the tool schema's :INTEGER
admits 0 and negative numbers, and cl-spec runs (loop for trial from 1 to -5)
without complaint -- zero trials, reported as \"-5 executed of -5 budget\"."
  ;; The raw value, not %STRING-ARG's: that filter turned a JSON number or an
  ;; empty string into NIL, and NIL means "no seed given" -- so the run went
  ;; ahead with a fresh random seed and reported it as though it were the
  ;; caller's.  A seed that cannot be honoured has to be refused, which is
  ;; the whole reason it travels as text.
  (multiple-value-bind (seed seed-error)
      (parse-seed-string (and params (gethash "seed" params)))
    (multiple-value-bind (max-value-chars chars-error)
        (%positive-integer-arg params "max_value_chars" 2000)
      ;; A default of NIL, so an absent trials stays absent: the budget then
      ;; comes from the property's own table or the backend, which is what
      ;; CHECK-REPORT expects to see.
      (multiple-value-bind (trials trials-error)
          (%bounded-integer-arg params "trials" *maximum-trials*)
        (let ((message (or seed-error chars-error trials-error)))
          (if message
              (%argument-error-response message #'build-spec-check-response)
              (multiple-value-bind (api status) (resolve-cl-spec-api)
                (build-spec-check-response
                 (check-report api status
                               :property (%string-arg params "property")
                               :symbol (%string-arg params "symbol")
                               :function (%string-arg params "function")
                               :trials trials
                               :package (%string-arg params "package")
                               :profile (%string-arg params "profile")
                               :seed seed
                               :expect-definition-digest
                               (%string-arg params "expect_definition_digest")
                               :timeout-seconds (gethash "timeout_seconds" params)
                               :max-value-chars max-value-chars)))))))))
