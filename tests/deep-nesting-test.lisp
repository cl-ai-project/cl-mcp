;;;; tests/deep-nesting-test.lisp
;;;;
;;;; 2026-08-01 監査の Critical の回帰テスト。
;;;; 深さ 20,000 のネストが接続スレッドを永久停止させた。

(defpackage #:cl-mcp/tests/deep-nesting-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/validate
                #:lisp-check-parens)
  (:import-from #:cl-mcp/src/utils/paren-scan
                #:*max-nesting-depth*)
  (:import-from #:cl-mcp/src/cst
                #:parse-top-level-forms
                #:nesting-too-deep)
  (:import-from #:cl-mcp/src/tcp
                #:serve-tcp)
  (:import-from #:cl-mcp/src/utils/serving
                #:call-without-debugger))

(in-package #:cl-mcp/tests/deep-nesting-test)

(defun nested-source (depth)
  "DEPTH 重にネストした、構文的に正しいフォームの文字列を返す。"
  (concatenate 'string
               (make-string depth :initial-element #\()
               ":deep"
               (make-string depth :initial-element #\))))

(deftest check-parens-accepts-depth-at-the-limit
  (testing "exactly the limit is still accepted"
    (let ((res (lisp-check-parens :code (nested-source *max-nesting-depth*))))
      (ok (eq t (gethash "ok" res))
          "a form at the limit must not be rejected"))))

(deftest check-parens-rejects-depth-over-the-limit
  (testing "one past the limit is rejected as too-deep, not as a hang"
    (let ((res (lisp-check-parens :code (nested-source (1+ *max-nesting-depth*)))))
      (ok (null (gethash "ok" res)))
      (ok (string= "too-deep" (gethash "kind" res))))))

(deftest check-parens-survives-the-audit-reproduction
  (testing "depth 20000 returns an error instead of exhausting the stack"
    (let ((res (lisp-check-parens :code (nested-source 20000))))
      (ok (string= "too-deep" (gethash "kind" res))))))

(deftest check-parens-does-not-count-string-parens
  (testing "a long string literal of open parens is not too deep"
    (let* ((source (format nil "(f ~S)" (make-string 30000 :initial-element #\()))
           (res (lisp-check-parens :code source)))
      (ok (eq t (gethash "ok" res))
          "parens inside a string literal must not trip the depth limit"))))

(deftest check-parens-shallow-mismatch-does-not-let-deep-nesting-reach-the-reader
  (testing "a shallow bracket mismatch followed by deep legitimate nesting returns, not crashes"
    ;; Reproduction: scan-parens stops at the FIRST delimiter error it finds,
    ;; so its :max-depth only describes the text up to that point. A shallow
    ;; mismatch (here, an extra "]" at depth 1) must not let 20,000 levels of
    ;; real nesting after it reach the standard reader, which treats "]" as
    ;; an ordinary constituent character rather than an error.
    (let* ((source (concatenate 'string
                                 "(]"
                                 (make-string 20000 :initial-element #\()
                                 ":x"
                                 (make-string 20000 :initial-element #\))
                                 ")"))
           (res (lisp-check-parens :code source)))
      (ok (null (gethash "ok" res))
          "the shallow mismatch must still be reported as an error")
      (ok (string= "mismatch" (gethash "kind" res))
          "the mismatch, not a hang or crash, must be what is reported"))))

(deftest check-parens-quote-run-does-not-let-prefix-macros-bypass-the-guard
  (testing "a long run of quote characters with no parens is caught as too-deep"
    ;; Reproduction: ', `, ,, ,@ and #' make the standard reader recurse
    ;; just as parens do (CLHS 2.4.5, 2.4.6), but scan-parens originally
    ;; only tracked bracket depth -- so 20,000 consecutive quote characters
    ;; with no parens at all looked like depth 0 and sailed past the guard
    ;; straight into the reader, reproducing the same control-stack
    ;; exhaustion the guard exists to prevent.
    (let* ((source (concatenate 'string
                                 (make-string 20000 :initial-element #\')
                                 ":x"))
           (res (lisp-check-parens :code source)))
      (ok (null (gethash "ok" res))
          "the deep quote run must be rejected, not read")
      (ok (string= "too-deep" (gethash "kind" res))
          "it is a depth rejection, matching scan-parens's :max-depth"))))

(deftest check-parens-sharp-plus-chain-does-not-let-feature-conditionals-bypass-the-guard
  (testing "a long chain of #+(and) blocks is caught as too-deep"
    ;; Reproduction: #+/#- (CLHS 1.5.2, 2.4.8.16/17) issue two independent
    ;; recursive reads -- the feature-expression, then the guarded form --
    ;; neither gated by *read-eval*. Chaining "#+(and)" as each block's
    ;; guarded form (rather than a bare atom) makes each block's own
    ;; (and)/(or) close its own bracket depth back to 0, so a bracket-only
    ;; scan sees only shallow, independent pairs and misses that the real
    ;; reader recurses once per block for the pending guarded-form read,
    ;; same hazard class as the quote-chain and shallow-mismatch cases
    ;; above.
    (let* ((prefix (with-output-to-string (s)
                     (dotimes (i 20000)
                       (write-string "#+(and)" s))))
           (source (concatenate 'string prefix ":x"))
           (res (lisp-check-parens :code source)))
      (ok (null (gethash "ok" res))
          "the deep #+ chain must be rejected, not read")
      (ok (string= "too-deep" (gethash "kind" res))
          "it is a depth rejection, matching scan-parens's :max-depth"))))

(deftest check-parens-sharp-plus-ordinary-spelling-chain-does-not-let-feature-conditionals-bypass-the-guard
  (testing "a long chain of #+sbcl (ordinary multi-character feature name) blocks is caught as too-deep"
    ;; Reproduction of the Critical this branch's own review found: #+(and)
    ;; above uses a bracketed feature-expression, which goes through the
    ;; open/close-bracket branches of %SCAN-HANDLE-NORMAL and was never at
    ;; risk. #+sbcl -- the ordinary, ubiquitous spelling, with no brackets
    ;; at all -- resolved its pending marker once per character of "sbcl"
    ;; instead of once per token, popping the marker before the guarded
    ;; form was even reached. scan-parens reported max-depth 1 for this
    ;; exact shape no matter how long the chain, so lisp-check-parens
    ;; scanned it clean and a real (READ) on it exhausts the control stack
    ;; -- silently reopening the audit's Critical on the tool that exists
    ;; to catch it.
    (let* ((prefix (with-output-to-string (s)
                     (dotimes (i 20000)
                       (write-string "#+sbcl " s))))
           (source (concatenate 'string prefix ":x"))
           (res (lisp-check-parens :code source)))
      (ok (null (gethash "ok" res))
          "the deep #+sbcl chain must be rejected, not read")
      (ok (string= "too-deep" (gethash "kind" res))
          "it is a depth rejection, matching scan-parens's :max-depth"))))

(deftest cst-rejects-depth-over-the-limit
  (testing "the CST path signals instead of exhausting the stack"
    (ok (handler-case
            (progn (parse-top-level-forms (nested-source 20000)) nil)
          (nesting-too-deep () t))
        "depth 20000 must signal nesting-too-deep")))

(deftest cst-accepts-real-source
  (testing "the deepest file in this repo still parses"
    ;; src/proxy.lisp は実測でネスト深さ 20、このリポジトリの最大。
    (let ((text (uiop:read-file-string
                 (asdf:system-relative-pathname :cl-mcp "src/proxy.lisp"))))
      (ok (parse-top-level-forms text)
          "ordinary source must be unaffected by the depth limit"))))

(deftest serious-condition-does-not-park-the-connection-thread
  (testing "a serious condition closes one connection and leaves the server serving"
    (let* ((port nil)
           (ready (bordeaux-threads:make-semaphore))
           (server (bordeaux-threads:make-thread
                    (lambda ()
                      (serve-tcp :host "127.0.0.1" :port 0 :accept-once nil
                                 :on-listening
                                 (lambda (p)
                                   (setf port p)
                                   (bordeaux-threads:signal-semaphore ready)))))))
      (unwind-protect
           (progn
             (ok (bordeaux-threads:wait-on-semaphore ready :timeout 10)
                 "server must come up")
             ;; 壊れた JSON-RPC を送りつけた後、別接続が生きていることを確かめる。
             ;; 目的はサーバが応答し続けることであって、この行が何を返すかではない。
             (let ((socket (usocket:socket-connect "127.0.0.1" port)))
               (unwind-protect
                    (progn
                      (format (usocket:socket-stream socket) "~A~%" "{")
                      (force-output (usocket:socket-stream socket)))
                 (ignore-errors (usocket:socket-close socket))))
             (let ((socket (usocket:socket-connect "127.0.0.1" port
                                                   :timeout 10)))
               (unwind-protect
                    (ok socket "the server must still accept a new connection")
                 (ignore-errors (usocket:socket-close socket)))))
        (ignore-errors (bordeaux-threads:destroy-thread server))))))

(deftest call-without-debugger-escapes-a-debugger-entry
  (testing "invoke-debugger inside the thunk returns :debugger-suppressed, not the thunk's own value"
    ;; This is the direct unit test the connection-thread test above cannot
    ;; be: that test sends "{", which process-json-line catches as an
    ;; ordinary parse error and answers normally -- the debugger hook is
    ;; never approached, so deleting CALL-WITHOUT-DEBUGGER entirely would
    ;; still leave that test, and the rest of this suite, green.
    ;;
    ;; INVOKE-DEBUGGER is called directly here rather than via ERROR/SIGNAL.
    ;; A plain (error "boom") inside the thunk gets intercepted by whatever
    ;; HANDLER-CASE the calling harness itself installs around a test body
    ;; (verified empirically: both repl-eval's error-context capture and,
    ;; separately, evaluating this same shape via Rove both catch a plain
    ;; ERROR before *INVOKE-DEBUGGER-HOOK* is ever consulted, since
    ;; HANDLER-CASE intercepts at the SIGNAL call site, upstream of
    ;; INVOKE-DEBUGGER) -- which would make the assertion below pass or
    ;; fail for the wrong reason regardless of whether the hook works.
    ;; Calling INVOKE-DEBUGGER directly reaches *INVOKE-DEBUGGER-HOOK* the
    ;; same way a genuinely unhandled condition does in production,
    ;; independent of any such wrapping.
    (ok (eq :debugger-suppressed
            (call-without-debugger
             "unit-test"
             (lambda ()
               (invoke-debugger (make-condition 'simple-error
                                                 :format-control "boom"))
               :thunk-returned)))
        "the hook must escape via THROW, not let the thunk's own return value win")))
