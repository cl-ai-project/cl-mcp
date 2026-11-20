;;;; Run in a clean parent process, from the install-path worker launch test.
(require :asdf)
(asdf:load-system "cl-mcp/src/worker-client")

(let ((install (uiop:getenv "CL_MCP_TEST_INSTALL")))
  (assert (equal install (uiop:native-namestring (asdf:system-source-directory :cl-mcp))))
  (when (uiop:getenv "CL_MCP_TEST_CLEAN_SBCL")
    (assert (not (find-package :ql)))
    (setf cl-mcp/src/worker-client::*cached-sbcl-path* (uiop:getenv "CL_MCP_TEST_CLEAN_SBCL")))
  (let ((worker (cl-mcp/src/worker-client:spawn-worker))
        (params (make-hash-table :test 'equal)))
    (unwind-protect
         (progn
           (setf (gethash "code" params) "(if (find-package :ql) :quicklisp-child :asdf-child)")
           (let* ((reply (cl-mcp/src/worker-client:worker-rpc worker "worker/eval" params))
                  (text (gethash "text" (aref (gethash "content" reply) 0))))
             (format t "~&AUTHENTICATED ~A~%" text)))
      (cl-mcp/src/worker-client:kill-worker worker))))
