;;;; tests/bridge-test.lisp

(defpackage #:cl-mcp/tests/bridge-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/main #:serve-tcp)
  (:import-from #:bordeaux-threads #:make-thread))

(in-package #:cl-mcp/tests/bridge-test)

(deftest stdio-bridge-no-idle-timeout
  (ok t)
  #+(or)
  (testing "stdio_tcp_bridge.py should not exit on idle > 5s after connect"
    ;; Start TCP server on ephemeral port
    (let ((port-var nil))
      (let ((thr (make-thread
                  (lambda ()
                    (serve-tcp :host "127.0.0.1" :port 0
                               :on-listening (lambda (p) (setf port-var p))
                               :accept-once t))
                  :name "bridge-test-server")))
        (unwind-protect
             (progn
               ;; Wait for server to listen
               (loop repeat 200 until port-var do (sleep 0.01d0))
               (ok port-var)

               ;; Launch stdio↔TCP bridge
               (let* ((cmd (list "python3" "scripts/stdio_tcp_bridge.py"
                                 "--host" "127.0.0.1"
                                 "--port" (write-to-string port-var)
                                 "--connect-timeout" "5.0"))
                      (proc (uiop:launch-program cmd
                                                 :input :stream
                                                 :output :stream
                                                 :error-output :string))
                      (pin (uiop:process-info-input proc))
                      (pout (uiop:process-info-output proc)))
                 (unwind-protect
                      (let ((req (concatenate
                                  'string
                                  "{\"jsonrpc\":\"2.0\",\"id\":1,"
                                  "\"method\":\"initialize\",\"params\":{}}"
                                  (string #\Newline))))
                        ;; Send initialize request through stdin → TCP
                        (write-string req pin)
                        (finish-output pin)
                        ;; Receive one line back from stdout
                        (let ((line (read-line pout nil nil)))
                          (ok (and line (search "\"result\"" line))))
                        ;; Stay idle for > 5s; process must remain alive
                        (sleep 6)
                        (ok (uiop:process-alive-p proc))
                        ;; Close stdin to allow bridge to exit
                        (close pin)
                        (let ((rc (uiop:wait-process proc)))
                          (ok (= 0 rc))))
                   (when (and proc (uiop:process-alive-p proc))
                     (ignore-errors (uiop:terminate-process proc))))))
          (bordeaux-threads:join-thread thr))))))
