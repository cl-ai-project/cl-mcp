;;;; tests/spec-tools-test.lisp
;;;;
;;;; The cl-spec tools driven the way a client drives them: a JSON-RPC line in,
;;;; a JSON-RPC line out, with the worker pool disabled so the call runs in
;;;; this image.  These checks are about the tool surface -- schema, argument
;;;; validation, and the shape of an answer that never reaches cl-spec -- not
;;;; about cl-spec itself, which tests/spec-adapter-report-test.lisp covers
;;;; with stubs and tests/spec-integration-test.lisp covers with the real
;;;; thing.

(defpackage #:cl-mcp/tests/spec-tools-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/protocol #:process-json-line)
  (:import-from #:cl-mcp/src/proxy #:*use-worker-pool*)
  (:import-from #:cl-mcp/src/tools/spec-entry #:parse-seed-string)
  (:import-from #:cl-mcp/src/spec-adapter-report
                #:+result-statuses+
                #:+call-statuses+
                #:+verification-gap-values+)
  (:import-from #:cl-mcp/src/tools/registry
                #:*enabled-tool-groups*
                #:disabled-tool-group)
  (:import-from #:yason #:parse))

(in-package #:cl-mcp/tests/spec-tools-test)

(defvar *tools-loaded* nil
  "True once cl-mcp/main has been loaded into this image.")

(defun %ensure-tools ()
  "Load the tool definitions so process-json-line can dispatch to them."
  (unless *tools-loaded*
    (asdf:load-system "cl-mcp/main")
    (setf *tools-loaded* t)))

(defmacro with-cl-spec-group (&body body)
  "Run BODY with the optional :CL-SPEC tool group switched on.

The group is off by default, which is what most of these tests are about; the
ones that drive a spec tool have to turn it on the way a deployment would."
  `(let ((*enabled-tool-groups* (list "CL-SPEC")))
     ,@body))

(defun %call (name arguments-json)
  "Call tool NAME with ARGUMENTS-JSON and return the parsed JSON-RPC object.

The whole object, not its result: argument validation on a protocol version
older than 2025-11-25 answers with a JSON-RPC error rather than a result, and
a helper that returned only the result would hide exactly those messages."
  (%ensure-tools)
  (let ((*use-worker-pool* nil)
        (*enabled-tool-groups* (list "CL-SPEC"))
        (line (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",~
\"params\":{\"name\":\"~A\",\"arguments\":~A}}" name arguments-json)))
    (parse (process-json-line line))))

(defun %text (object)
  "Return the caller-visible text of a JSON-RPC OBJECT.

A tool answers either with a result whose content carries text, or with a
JSON-RPC error carrying a message.  Both are what the caller reads, so both
are what a test about the message has to look at."
  (let ((result (gethash "result" object))
        (failure (gethash "error" object)))
    (cond
      (failure (or (gethash "message" failure) ""))
      (result (let ((content (gethash "content" result)))
                (or (when (and (vectorp content) (plusp (length content)))
                      (gethash "text" (aref content 0)))
                    "")))
      (t ""))))

(deftest spec-tools-are-hidden-until-the-group-is-enabled
  (testing "the optional group is off by default"
    (%ensure-tools)
    (let* ((*use-worker-pool* nil)
           (response (process-json-line
                      "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}"))
           (tools (gethash "tools" (gethash "result" (parse response))))
           (names (map 'list (lambda (tool) (gethash "name" tool)) tools)))
      (dolist (name '("spec-list" "spec-symbol" "spec-describe" "spec-check"))
        (ok (not (member name names :test #'string=))))
      (testing "and the tools that are not optional are still there"
        (ok (member "repl-eval" names :test #'string=)))))
  (testing "calling one says which group to enable, not that it does not exist"
    (%ensure-tools)
    (let* ((*use-worker-pool* nil)
           (response (process-json-line
                      "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":{\"name\":\"spec-check\",\"arguments\":{}}}"))
           (message (gethash "message" (gethash "error" (parse response)))))
      (ok (search "cl-spec" message))
      (ok (search "MCP_ENABLE_TOOL_GROUPS" message))
      (ok (not (search "not found" message)))))
  (testing "an unknown tool is still reported as unknown"
    (%ensure-tools)
    (let* ((*use-worker-pool* nil)
           (response (process-json-line
                      "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/call\",\"params\":{\"name\":\"no-such-tool\",\"arguments\":{}}}"))
           (message (gethash "message" (gethash "error" (parse response)))))
      (ok (search "not found" message))))
  (testing "disabled-tool-group names the group for a registered tool only"
    (%ensure-tools)
    (ok (eq :cl-spec (disabled-tool-group "spec-check")))
    (ok (null (disabled-tool-group "repl-eval")))
    (ok (null (disabled-tool-group "no-such-tool")))
    (with-cl-spec-group
      (ok (null (disabled-tool-group "spec-check"))))))

(deftest spec-tools-are-registered
  (testing "all four tools appear in tools/list once the group is on"
    (%ensure-tools)
    (let* ((*use-worker-pool* nil)
           (*enabled-tool-groups* (list "CL-SPEC"))
           (response (process-json-line
                      "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}"))
           (tools (gethash "tools" (gethash "result" (parse response))))
           (names (map 'list (lambda (tool) (gethash "name" tool)) tools)))
      (dolist (name '("spec-list" "spec-symbol" "spec-describe" "spec-check"))
        (ok (member name names :test #'string=)))
      (testing "and each carries an inputSchema"
        (loop for tool across tools
              when (member (gethash "name" tool)
                           '("spec-list" "spec-symbol" "spec-describe"
                             "spec-check")
                           :test #'string=)
                do (ok (hash-table-p (gethash "inputSchema" tool))))))))

(deftest spec-symbol-requires-a-symbol
  (testing "a missing symbol argument is a validation error, not a crash"
    (let ((response (%call "spec-symbol" "{}")))
      (ok (search "symbol" (%text response)))
      (testing "and nothing was answered as if it had succeeded"
        (ok (null (gethash "result" response)))))))

(deftest spec-describe-rejects-an-unknown-kind
  (testing "kind is constrained and the message says what is allowed"
    (let ((result (%call "spec-describe"
                         "{\"kind\":\"generator\",\"name\":\"cl:car\"}")))
      (ok (search "property" (%text result)))
      (ok (search "spec" (%text result))))))

(deftest spec-check-refuses-more-than-one-target
  (testing "property and symbol together is refused with a usable message"
    (let ((result (%call "spec-check"
                         "{\"property\":\"cl:car\",\"symbol\":\"cl:cdr\"}")))
      (ok (search "exactly one" (string-downcase (%text result))))))
  (testing "so is a function alongside one of them"
    (let ((result (%call "spec-check"
                         "{\"function\":\"cl:car\",\"symbol\":\"cl:cdr\"}")))
      (ok (search "exactly one" (string-downcase (%text result))))))
  (testing "and naming none of the three says all three"
    (let ((result (%call "spec-check" "{}")))
      (ok (search "property, symbol or function"
                  (string-downcase (%text result)))))))

(deftest spec-check-refuses-a-non-numeric-seed
  (testing "a seed that is not decimal digits is rejected before any run"
    (let ((result (%call "spec-check"
                         "{\"property\":\"cl:car\",\"seed\":\"not-a-number\"}")))
      (ok (search "seed" (string-downcase (%text result)))))))

(deftest spec-check-refuses-a-numeric-seed
  (testing "a JSON number is refused at the schema, before any run"
    (let ((response (%call "spec-check"
                           "{\"property\":\"cl:car\",\"seed\":12345}")))
      (ok (search "seed" (string-downcase (%text response))))
      (ok (search "string" (string-downcase (%text response))))))
  (testing "and the entry point refuses one too, for callers below the schema"
    ;; SPEC-CHECK-RESPONSE is exported and the worker handler calls it with a
    ;; params table directly, so the refusal cannot live only in the schema.
    (%ensure-tools)
    (let* ((params (make-hash-table :test #'equal))
           (response (progn (setf (gethash "property" params) "cl:car"
                                  (gethash "seed" params) 12345)
                            (funcall (find-symbol "SPEC-CHECK-RESPONSE"
                                                  "CL-MCP/SRC/TOOLS/SPEC-ENTRY")
                                     params))))
      (ok (string= "invalid-arguments" (gethash "status" response)))
      (ok (search "digits" (string-downcase (gethash "message" response)))))))

(deftest spec-check-refuses-an-empty-seed
  (testing "an empty seed is refused, not treated as absent"
    (let ((response (%call "spec-check"
                           "{\"property\":\"cl:car\",\"seed\":\"\"}")))
      (ok (search "seed" (string-downcase (%text response)))))))

(deftest spec-describe-refuses-a-negative-budget
  (testing "max_chars is validated before anything reads the registry"
    (let ((response (%call "spec-describe"
                           "{\"kind\":\"property\",\"name\":\"cl:car\",\"max_chars\":-1}")))
      (ok (search "max_chars" (%text response)))
      (ok (search "positive" (string-downcase (%text response)))))))

(deftest spec-check-refuses-a-non-positive-trials
  (testing "a negative trial count is refused before anything runs"
    ;; The schema's :INTEGER admits 0 and negatives, and cl-spec runs
    ;; (loop for trial from 1 to -5) without complaint: zero trials, which the
    ;; response then asserts as "-5 executed of -5 budget (requested)".
    (let ((response (%call "spec-check" "{\"function\":\"cl:car\",\"trials\":-5}")))
      (ok (search "trials" (%text response)))
      (ok (search "positive" (string-downcase (%text response))))))
  (testing "and so is zero"
    (let ((response (%call "spec-check" "{\"function\":\"cl:car\",\"trials\":0}")))
      (ok (search "positive" (string-downcase (%text response))))))
  (testing "while an absent trials stays absent rather than becoming a budget"
    ;; Asserted on the value, not on a status that would be something else
    ;; anyway.  A numeric default here would send cl-spec a budget the caller
    ;; never asked for, and "the call was not refused" cannot see that: the
    ;; status is cl-spec-not-loaded or not-registered either way.
    (%ensure-tools)
    (let ((params (make-hash-table :test #'equal))
          (read-arg (find-symbol "%POSITIVE-INTEGER-ARG"
                                 "CL-MCP/SRC/TOOLS/SPEC-ENTRY")))
      (multiple-value-bind (value message) (funcall read-arg params "trials" nil)
        (ok (null value))
        (ok (null message)))
      (testing "and a budget past the ceiling is refused with the reason"
        ;; The ceiling bounds what a leaked deadline thread is left doing in
        ;; the worker, so it has to be reachable through the entry point the
        ;; worker handler calls, not only through the schema.
        (let ((response (%call "spec-check"
                               "{\"function\":\"cl:car\",\"trials\":5000000}")))
          (ok (search "at most" (%text response)))
          (ok (search "1,000,000" (%text response)))))
      (testing "and a value that is there still has to be positive"
        (setf (gethash "trials" params) 0)
        (multiple-value-bind (value message)
            (funcall read-arg params "trials" nil)
          (ok (null value))
          (ok (search "positive" message)))))))

(deftest read-tools-accept-a-timeout
  (testing "spec-symbol and spec-describe take timeout_seconds"
    (%ensure-tools)
    (let* ((*use-worker-pool* nil)
           (*enabled-tool-groups* (list "CL-SPEC"))
           (response (process-json-line
                      "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}"))
           (tools (gethash "tools" (gethash "result" (parse response)))))
      (loop for tool across tools
            when (member (gethash "name" tool) '("spec-symbol" "spec-describe")
                         :test #'string=)
              do (let ((properties (gethash "properties"
                                            (gethash "inputSchema" tool))))
                   (ok (nth-value 1 (gethash "timeout_seconds" properties))))))))

(deftest parse-seed-string-round-trips-a-big-seed
  (testing "a seed beyond JSON's safe integer parses exactly"
    (multiple-value-bind (value message)
        (parse-seed-string "3963993791726803706")
      (ok (null message))
      (ok (= 3963993791726803706 value))))
  (testing "and anything else is refused rather than coerced"
    (dolist (bad '("-1" "1.5" "" "12a" "0x10"))
      (multiple-value-bind (value message) (parse-seed-string bad)
        (ok (null value))
        (ok (stringp message))))))

(deftest spec-check-description-names-every-status-it-can-answer-with
  (testing "the description does not fall behind the statuses the code emits"
    ;; The tool description is the only documentation a model ever sees, and
    ;; it silently drifted behind three rounds of changes. Checked against the
    ;; code's own list so the next status cannot be added without saying so.
    (%ensure-tools)
    (let* ((*use-worker-pool* nil)
           (*enabled-tool-groups* (list "CL-SPEC"))
           (response (process-json-line
                      "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}"))
           (tools (gethash "tools" (gethash "result" (parse response))))
           (description (loop for tool across tools
                              when (string= "spec-check" (gethash "name" tool))
                                return (gethash "description" tool))))
      (ok (stringp description))
      (dolist (status +result-statuses+)
        (ok (search (string-downcase (symbol-name status)) description)
            (format nil "spec-check description must name the ~(~A~) status"
                    status)))
      (testing "and the whole-call statuses too"
        ;; Searched inside the whole-call section, not the whole string: a
        ;; status documented only under results[].status satisfied a search
        ;; over the description and the guard reported coverage it did not
        ;; have -- which is how undefined-function reached +CALL-STATUSES+
        ;; without ever being defined for a caller reading about a call.
        (let* ((start (search "For the whole call, status:" description))
               (section (subseq description (or start 0))))
          (ok start "the description must have a whole-call status section")
          (dolist (status +call-statuses+)
            (ok (search (string-downcase (symbol-name status)) section)
                (format nil "the whole-call section must name ~(~A~)"
                        status)))))
      (testing "and every verification gap it can report"
        ;; The gap set grew four times in one branch while the description
        ;; explained two of the values.  Checked from the code's own list for
        ;; the same reason the statuses are.
        (dolist (gap +verification-gap-values+)
          (ok (search (string-downcase (symbol-name gap)) description)
              (format nil "spec-check description must name the ~(~A~) gap"
                      gap)))))))

(deftest spec-check-description-states-the-current-rules
  (testing "verified's third condition and the four-valued match are stated"
    (%ensure-tools)
    (let* ((*use-worker-pool* nil)
           (*enabled-tool-groups* (list "CL-SPEC"))
           (response (process-json-line
                      "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}"))
           (tools (gethash "tools" (gethash "result" (parse response))))
           (description (loop for tool across tools
                              when (string= "spec-check" (gethash "name" tool))
                                return (gethash "description" tool))))
      (ok (search "at least one trial" description))
      (ok (search "four-valued" description))
      (ok (search "verification_gaps" description))
      (ok (search "by_status" description))
      (ok (search "worker_reuse" description)))))

(deftest spec-list-refuses-a-non-positive-limit
  (testing "limit is validated before anything reads the registry"
    (let ((response (%call "spec-list" "{\"limit\":0}")))
      (ok (search "limit" (%text response)))
      (ok (search "positive" (string-downcase (%text response)))))))

(deftest spec-list-is-in-the-optional-group
  (testing "the listing tool is opt-in like the other three"
    (%ensure-tools)
    (ok (eq :cl-spec (disabled-tool-group "spec-list")))
    (with-cl-spec-group
      (ok (null (disabled-tool-group "spec-list"))))))
