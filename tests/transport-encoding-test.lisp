;;;; tests/transport-encoding-test.lisp
;;;;
;;;; Every line cl-mcp sends goes through ENCODE-JSON-LINE (or %ENCODE-JSON),
;;;; whatever yason version encodes it (#342, #344).  CI runs yason 0.8, which
;;;; escapes control characters itself, so these tests make yason write them
;;;; raw, as 0.7.x does, and look at what each send site puts on the wire.

(defpackage #:cl-mcp/tests/transport-encoding-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:encode-json-line)
  (:import-from #:cl-mcp/src/log
                #:log-event #:*log-stream* #:*log-level* #:*log-context*)
  (:import-from #:cl-mcp/src/protocol)
  (:import-from #:cl-mcp/src/worker-client)
  (:import-from #:cl-mcp/src/worker/server)
  (:import-from #:cl-mcp/src/worker/main)
  (:import-from #:cl-mcp/src/tcp)
  (:import-from #:cl-mcp/src/http))

(in-package #:cl-mcp/tests/transport-encoding-test)

(defun %raw-control-replacements ()
  "Return a copy of yason's escape table that writes every control character
yason 0.7.x writes raw -- all below U+0020 but \\b \\t \\n \\f \\r -- as itself."
  (let ((table (make-hash-table :test (hash-table-test yason::*char-replacements*))))
    (maphash (lambda (key value) (setf (gethash key table) value))
             yason::*char-replacements*)
    (loop for code from 0 below #x20
          unless (member code '(8 9 10 12 13))
            do (setf (gethash (code-char code) table) (string (code-char code))))
    table))

(defmacro with-yason-0.7-escaping (&body body)
  "Run BODY with yason writing control characters raw, as 0.7.x does."
  `(let ((yason::*char-replacements* (%raw-control-replacements)))
     ,@body))

(defun %raw-value ()
  "A string holding control characters 0.7.x writes raw and a lone surrogate."
  (coerce (list #\a (code-char 0) (code-char 1) (code-char #x0B) (code-char #x1F)
                (code-char #xDC00) #\z)
          'string))

(defun %sendable-p (line)
  "True when LINE holds no character a transport cannot carry."
  (notany (lambda (char)
            (let ((code (char-code char)))
              (or (<= #xD800 code #xDFFF)
                  (and (< code #x20) (/= code 9) (/= code 10) (/= code 13)))))
          line))

(defun %expected-value ()
  "What a client parses back from %RAW-VALUE: the controls as they were, the
lone surrogate as U+FFFD."
  (substitute (code-char #xFFFD) (code-char #xDC00) (%raw-value)))

(defun %check-line (line value-of)
  "Check LINE, one line sent, is sendable and that VALUE-OF finds the raw value
in its parse unchanged but for the surrogate."
  (ok (%sendable-p line) "nothing on the line breaks the transport")
  (ok (not (find #\Newline line)) "one line")
  (ok (equal (%expected-value) (funcall value-of (yason:parse line)))
      "the parse gives back the value"))

(deftest the-simulation-writes-control-characters-raw
  ;; Without this the tests below would pass on 0.8 whether or not a send site
  ;; is safe: the simulation must really make yason write a NUL raw.
  (with-yason-0.7-escaping
    (ok (find (code-char 0)
              (with-output-to-string (s) (yason:encode (string (code-char 0)) s)))
        "yason writes the NUL as itself")))

(deftest encode-json-line-is-safe-on-yason-0.7
  (with-yason-0.7-escaping
    (let ((object (make-hash-table :test 'equal)))
      (setf (gethash "v" object) (%raw-value))
      (%check-line (encode-json-line object) (lambda (json) (gethash "v" json))))))

(deftest every-send-site-is-safe-on-yason-0.7
  (with-yason-0.7-escaping
    (testing "the parent's answer to a client (%encode-json)"
      (let ((response (make-hash-table :test 'equal)))
        (setf (gethash "jsonrpc" response) "2.0"
              (gethash "id" response) 1
              (gethash "result" response) (%raw-value))
        (%check-line (cl-mcp/src/protocol::%encode-json response)
                     (lambda (json) (gethash "result" json)))))
    (testing "the worker's answer to the parent (%encode-response)"
      (let ((response (cl-mcp/src/worker/server::%make-result 3 (%raw-value))))
        (%check-line (cl-mcp/src/worker/server::%encode-response response)
                     (lambda (json) (gethash "result" json)))))
    (testing "the parent's request to a worker (%send-json-rpc)"
      (let ((params (make-hash-table :test 'equal)))
        (setf (gethash "code" params) (%raw-value))
        (let ((line (string-right-trim
                     '(#\Newline)
                     (with-output-to-string (stream)
                       (cl-mcp/src/worker-client::%send-json-rpc
                        stream 4 "worker/eval" params)))))
          (%check-line line (lambda (json) (gethash "code" (gethash "params" json)))))))
    (testing "a log line (log-event)"
      (let ((line (string-right-trim
                   '(#\Newline)
                   (let ((*log-stream* (make-string-output-stream))
                         (*log-level* :debug)
                         (*log-context* nil))
                     (log-event :info "transport.test" "value" (%raw-value))
                     (get-output-stream-string *log-stream*)))))
        (%check-line line (lambda (json) (gethash "value" json)))))
    (testing "an HTTP error body (%http-error-json)"
      (%check-line (cl-mcp/src/http::%http-error-json -32600 (%raw-value))
                   (lambda (json) (gethash "message" (gethash "error" json)))))
    (testing "a TCP error line (%error-response-line)"
      (%check-line (cl-mcp/src/tcp::%error-response-line -32600 (%raw-value))
                   (lambda (json) (gethash "message" (gethash "error" json)))))
    (testing "the worker's handshake (%output-handshake)"
      (let ((line (with-output-to-string (stream)
                    (cl-mcp/src/worker/main::%output-handshake 1234 nil stream))))
        (ok (%sendable-p line))
        (ok (eql 1234 (gethash "tcp_port" (yason:parse line))))))))

(deftest an-oversized-worker-answer-becomes-an-error-answer
  ;; Escaping makes a control character six, so a result can outgrow the line
  ;; the parent reads in encoding alone; refused in the middle of the line, the
  ;; parent took the worker for broken (#344).
  (let* ((response (cl-mcp/src/worker/server::%make-result
                    7 (make-string 20 :initial-element (code-char 0))))
         (line (cl-mcp/src/worker/server::%encode-response response 100))
         (json (yason:parse line)))
    (ok (< (length line) 1000) "the error answer is small")
    (ok (eql 7 (gethash "id" json)) "it answers the same request")
    (ok (null (gethash "result" json)) "the result is gone")
    (ok (eql -32603 (gethash "code" (gethash "error" json))))
    (ok (search "over the 100" (gethash "message" (gethash "error" json)))
        (or (gethash "message" (gethash "error" json)) "no message")))
  (testing "an answer within the limit is sent as it is"
    (let ((response (cl-mcp/src/worker/server::%make-result 8 "small")))
      (ok (equal "small"
                 (gethash "result"
                          (yason:parse (cl-mcp/src/worker/server::%encode-response
                                        response 1000))))))))
