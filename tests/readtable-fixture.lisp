(defpackage #:cl-mcp/tests/readtable-fixture
  (:use #:cl)
  (:import-from #:named-readtables #:register-readtable #:unregister-readtable)
  (:import-from #:cl-mcp/src/protocol #:process-json-line)
  (:import-from #:cl-mcp/src/state #:make-state #:protocol-version)
  (:import-from #:cl-mcp/src/tools/helpers #:make-ht)
  (:import-from #:yason #:encode #:parse)
  (:export #:call-with-qualified-readtable #:call-readtable-tool #:fixture-syntax))

(in-package #:cl-mcp/tests/readtable-fixture)

(defun call-with-qualified-readtable (thunk)
  "Run THUNK with a symbol and a qualified string naming a temporary readtable."
  (let ((name 'fixture-syntax)
        (readtable (copy-readtable nil)))
    (set-dispatch-macro-character
     #\# #\?
     (lambda (stream char argument)
       (declare (ignore char argument))
       (read stream t nil t))
     readtable)
    (register-readtable name readtable)
    (unwind-protect
         (funcall thunk name "cl-mcp/tests/readtable-fixture:fixture-syntax")
      (unregister-readtable name))))

(defun call-readtable-tool (tool arguments)
  "Send a JSON tools/call for TOOL and return its successful result."
  (let ((state (make-state))
        (request (make-ht "jsonrpc" "2.0" "id" 1 "method" "tools/call"
                          "params" (make-ht "name" tool "arguments" arguments))))
    (setf (protocol-version state) "2025-11-25")
    (let* ((line (with-output-to-string (stream) (encode request stream)))
           (response (parse (process-json-line line state)))
           (result (gethash "result" response)))
      (unless (and (equal (gethash "jsonrpc" response) "2.0")
                   (eql (gethash "id" response) 1)
                   (not (gethash "error" response))
                   (hash-table-p result)
                   (not (gethash "isError" result)))
        (error "~A returned an unexpected JSON response: ~A"
               tool (with-output-to-string (stream) (encode response stream))))
      result)))
