;;;; Run in a clean parent process, from the install-path worker launch test.
(require :asdf)
(asdf:load-system "cl-mcp/src/worker-client")

(let ((install (uiop:getenv "CL_MCP_TEST_INSTALL"))
      (clean (uiop:getenv "CL_MCP_TEST_CLEAN_SBCL")))
  (assert (equal install (uiop:native-namestring (asdf:system-source-directory :cl-mcp))))
  (when clean
    (assert (not (find-package :ql)))
    (setf cl-mcp/src/worker-client::*cached-sbcl-path* clean))
  (let ((worker (cl-mcp/src/worker-client:spawn-worker))
        (params (make-hash-table :test 'equal)))
    (uiop:quit
     (unwind-protect
          (progn
            (setf (gethash "code" params)
                  (if clean "(if (find-package :ql) 0 (+ 40 2))" "(+ 40 2)"))
            (let* ((reply (cl-mcp/src/worker-client:worker-rpc worker "worker/eval" params))
                   (text (gethash "text" (aref (gethash "content" reply) 0))))
              (parse-integer text)))
       (cl-mcp/src/worker-client:kill-worker worker)))))
