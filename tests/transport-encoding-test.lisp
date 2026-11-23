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

(deftest encoding-ignores-the-printer-settings-it-meets
  ;; A test's (setf *print-base* 16) changes the global value every thread
  ;; sees; yason writes an integer with PRINC, so the worker's answer held
  ;; "2A" and the parent killed the worker for it (#416).
  (let ((object (make-hash-table :test 'equal)))
    (setf (gethash "n" object) 255)
    (let ((*print-base* 16) (*print-radix* t))
      (ok (equal "{\"n\":255}" (encode-json-line object)) "a line sent between processes")
      (ok (equal "{\"n\":255}" (cl-mcp/src/protocol::%encode-json object))
          "and an answer to a client"))
    (testing "nor a pprint dispatch entry for integers"
      (setf (gethash "n" object) 1234567)
      (let ((*print-pprint-dispatch* (copy-pprint-dispatch))
            (*print-pretty* t))
        (set-pprint-dispatch 'integer
                             (lambda (stream n)
                               (let ((*print-pretty* nil)) (format stream "~:D" n))))
        (ok (equal "{\"n\":1234567}" (encode-json-line object)) (encode-json-line object))
        (ok (equal "{\"n\":1234567}" (cl-mcp/src/protocol::%encode-json object))
            "an answer to a client too")
        ;; yason writes a non-BMP character as \u escapes of its own, and a
        ;; yason that leaves control characters raw has them escaped by
        ;; TRANSPORT-SAFE-JSON, which wrote its hex digits with FORMAT ~X.
        (let ((emoji (string (code-char #x1F600))))
          (ok (equal (list emoji)
                     (coerce (yason:parse (encode-json-line (list emoji))) 'list))
              "nor a string's escapes"))
        (ok (equal "[\"\\u001B\"]"
                   (cl-mcp/src/utils/sanitize:transport-safe-json
                    (format nil "[\"~C\"]" (code-char 27))))
            "nor the escape of a raw control character")
        ;; The last fallback, for an answer yason cannot encode, escapes a
        ;; string id by hand.
        (let ((response (make-hash-table :test 'equal)))
          (setf (gethash "jsonrpc" response) "2.0"
                (gethash "id" response) (format nil "a~Cz" (code-char 27))
                (gethash "result" response) #c(1 2))
          (ok (search "\"id\":\"a\\u001Bz\""
                      (cl-mcp/src/protocol::%encode-json response))
              "nor the escape in the fallback's id"))))))

(deftest wire-decoding-ignores-the-read-base
  (let ((server (cl-mcp/src/worker/server::%make-worker-server :authenticated-p t))
        (ping "{\"jsonrpc\":\"2.0\",\"id\":10,\"method\":\"worker/ping\"}")
        (reply "{\"jsonrpc\":\"2.0\",\"id\":10,\"result\":{\"n\":1e5}}")
        (handshake "{\"tcp_port\":12345,\"swank_port\":null,\"pid\":12346}"))
    (cl-mcp/src/worker/server:register-method
     server "worker/ping" (lambda (params) (declare (ignore params)) (make-hash-table)))
    (testing "requests to the worker keep their decimal ID"
      (let ((line (let ((*read-base* 16))
                    (cl-mcp/src/worker/server::%process-line server ping))))
        (ok (eql 10 (gethash "id" (yason:parse line))) line)))
    (testing "responses from the worker match the ID and parse decimal exponents"
      (multiple-value-bind (result condition)
          (let ((*read-base* 16))
            (ignore-errors
              (with-input-from-string (stream reply)
                (cl-mcp/src/worker-client::%read-json-rpc-response stream 10 nil))))
        (ok (and (hash-table-p result) (= 100000 (gethash "n" result)))
            (and condition (princ-to-string condition)))))
    (testing "the worker's handshake keeps its decimal port and PID"
      (ok (equal '(12345 nil 12346)
                 (let ((*read-base* 16))
                   (with-input-from-string (stream handshake)
                     (multiple-value-list
                      (cl-mcp/src/worker-client::%parse-handshake-from-stream stream)))))))))

(deftest wire-decoding-uses-cl-mcps-json-types
  (let* ((state (cl-mcp/src/protocol:make-state))
         (batch "[{\"jsonrpc\":\"2.0\",\"id\":10,\"method\":\"ping\"}]")
         (reply (let ((yason:*parse-json-arrays-as-vectors* nil)
                      (yason:*parse-object-key-fn* #'string-upcase))
                  (setf (cl-mcp/src/protocol:protocol-version state) "2025-03-26")
                  (cl-mcp/src/protocol:process-json-line batch state))))
    (let ((parsed (yason:parse reply :json-arrays-as-vectors t)))
      (ok (and (vectorp parsed) (= 1 (length parsed))
               (eql 10 (gethash "id" (aref parsed 0))))
          reply)))
  (flet ((read-result (preserve-json-types)
           (let ((yason:*parse-json-booleans-as-symbols* t)
                 (yason:*parse-json-null-as-keyword* t)
                 (yason:*parse-object-as-alist* t))
             (ignore-errors
               (with-input-from-string
                   (stream "{\"id\":10,\"result\":{\"flag\":false,\"value\":null,\"items\":[]}}")
                 (cl-mcp/src/worker-client::%read-json-rpc-response
                  stream 10 nil :preserve-json-types preserve-json-types))))))
    (let ((result (read-result nil))
          (relay (read-result t)))
      (ok (and (hash-table-p result)
               (null (gethash "flag" result))
               (null (gethash "value" result))
               (vectorp (gethash "items" result))))
      (ok (and (hash-table-p relay)
               (eq 'yason:false (gethash "flag" relay))
               (eq :null (gethash "value" relay))
               (vectorp (gethash "items" relay)))))))

(deftest wire-encoding-ignores-yason-customizations
  (let ((list-encoder (find-symbol "*LIST-ENCODER*" :yason))
        (symbol-encoder (find-symbol "*SYMBOL-ENCODER*" :yason))
        (key-encoder (find-symbol "*SYMBOL-KEY-ENCODER*" :yason))
        (response (make-hash-table :test 'equal)))
    (setf (gethash "elements" response) (list "one" 2))
    (ok (equal "{\"elements\":[\"one\",2]}" (encode-json-line response)))
    ;; Older Yason releases use fixed encoders and have no such settings.
    (when (and list-encoder symbol-encoder key-encoder
               (every #'boundp (list list-encoder symbol-encoder key-encoder)))
      (flet ((encode-symbol (symbol &optional stream)
               (when stream (yason:encode (string symbol) stream))
               (string symbol)))
        (progv (list list-encoder symbol-encoder key-encoder)
            (list 'yason:encode-plist #'encode-symbol #'encode-symbol)
          (ok (equal "{\"elements\":[\"one\",2]}" (encode-json-line response)))
          (ok (eq 'yason:encode-plist (symbol-value list-encoder)))
          (ok (handler-case (progn (encode-json-line :symbol-value) nil)
                (cl-mcp/src/utils/sanitize:json-encoding-error () t)))
          (clrhash response)
          (setf (gethash :symbol-key response) 1)
          (ok (handler-case (progn (encode-json-line response) nil)
                (cl-mcp/src/utils/sanitize:json-encoding-error () t))))))))

(deftest wire-boolean-literals-ignore-yason-aliases
  (progv '(yason:true yason:false) '(:application-true nil)
    (let ((response (cl-mcp/src/tools/helpers:make-ht
                     "flag" (cl-mcp/src/tools/helpers:json-bool nil)
                     "ok" 'yason:true)))
      (ok (equal "{\"flag\":false,\"ok\":true}" (encode-json-line response))))
    (ok (handler-case (progn (encode-json-line :application-true) nil)
          (cl-mcp/src/utils/sanitize:json-encoding-error () t)))
    (ok (eq :application-true (symbol-value 'yason:true)))
    (ok (null (symbol-value 'yason:false)))))

(defvar *inside-encoding* nil
  "True while %UNENCODABLE-PROBE's encode method runs.")

(define-condition %encoding-probe-error (error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "probe failed, inside=~A" *inside-encoding*)))
  (:documentation "Signalled inside yason's encoding; its report says whether it is still inside."))

(defstruct %unencodable-probe
  "A value whose yason:encode method signals while *INSIDE-ENCODING* is true.")

(defmethod yason:encode ((probe %unencodable-probe) &optional stream)
  (declare (ignore probe stream))
  (let ((*inside-encoding* t))
    (error '%encoding-probe-error)))

(deftest an-unencodable-worker-answer-becomes-an-error-answer
  ;; yason's error held the stack-allocated string stream it was writing to,
  ;; and printing it once that stream was gone was a memory fault that ended
  ;; the worker instead of answering (#417).
  (testing "the error's text is made while the encoding is still in progress"
    ;; Made after unwinding, as before, the dead stream usually still printed,
    ;; so only this tells the two apart.
    (let ((message (handler-case (encode-json-line (make-%unencodable-probe))
                     (cl-mcp/src/utils/sanitize:json-encoding-error (condition)
                       (princ-to-string condition)))))
      (ok (equal "JSON encoding failed: probe failed, inside=T" message) message)))
  (testing "the worker answers with an error instead of ending"
    (let ((server (cl-mcp/src/worker/server::%make-worker-server))
          (payload (make-hash-table :test 'equal))
          ;; A thread an earlier suite's deadline left running must not
          ;; retire this process.
          (cl-mcp/src/worker/server::*retire-action* (lambda (leaked)
                                                       (declare (ignore leaked)))))
      (setf (cl-mcp/src/worker/server::worker-server-authenticated-p server) t
            (gethash "x" payload) #c(1 2)
            (gethash "m" (cl-mcp/src/worker/server::worker-server-methods server))
            (lambda (params) (declare (ignore params)) payload))
      (let* ((line (handler-case
                       (cl-mcp/src/worker/server::%dispatch-request server 9 "m" nil)
                     (serious-condition (condition) (type-of condition))))
             (json (and (stringp line) (yason:parse line))))
        (ok (stringp line) (format nil "the worker answers: ~S" line))
        (when json
          (ok (eql 9 (gethash "id" json)) "the same request")
          (ok (eql -32603 (gethash "code" (gethash "error" json))) "with an error")
          (ok (search "JSON encoding failed" (gethash "message" (gethash "error" json)))
              (gethash "message" (gethash "error" json))))))))
