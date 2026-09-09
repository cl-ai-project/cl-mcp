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
                #:symbol-report
                #:describe-report
                #:check-report)
  (:import-from #:cl-mcp/src/tools/spec-response-builders
                #:build-spec-symbol-response
                #:build-spec-describe-response
                #:build-spec-check-response)
  (:export #:spec-symbol-response
           #:spec-describe-response
           #:spec-check-response
           #:parse-seed-string))

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

(defun spec-symbol-response (params)
  "Return the spec-symbol response hash-table for PARAMS."
  (multiple-value-bind (api status) (resolve-cl-spec-api)
    (build-spec-symbol-response
     (symbol-report api status (gethash "symbol" params)
                    :package (%string-arg params "package")
                    :include-runtime (multiple-value-bind (value present)
                                         (gethash "include_runtime" params)
                                       (if present value t))))))

(defun spec-describe-response (params)
  "Return the spec-describe response hash-table for PARAMS."
  (multiple-value-bind (api status) (resolve-cl-spec-api)
    (build-spec-describe-response
     (describe-report api status
                      (gethash "kind" params)
                      (gethash "name" params)
                      :package (%string-arg params "package")
                      :max-chars (or (gethash "max_chars" params) 8000)))))

(defun spec-check-response (params)
  "Return the spec-check response hash-table for PARAMS.

An unusable seed is answered here rather than passed on: a run started with a
seed the caller did not mean is a run whose result means nothing."
  (multiple-value-bind (seed seed-error)
      (parse-seed-string (%string-arg params "seed"))
    (if seed-error
        (build-spec-check-response
         (list :status :invalid-arguments :verified nil :message seed-error
               :environment (%environment-stub)))
        (multiple-value-bind (api status) (resolve-cl-spec-api)
          (build-spec-check-response
           (check-report api status
                         :property (%string-arg params "property")
                         :symbol (%string-arg params "symbol")
                         :package (%string-arg params "package")
                         :profile (%string-arg params "profile")
                         :seed seed
                         :expect-definition-digest
                         (%string-arg params "expect_definition_digest")
                         :timeout-seconds (gethash "timeout_seconds" params)
                         :max-value-chars (or (gethash "max_value_chars" params)
                                              2000)))))))
