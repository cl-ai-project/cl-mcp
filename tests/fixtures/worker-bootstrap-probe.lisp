;;;; Appended to the fixture worker's server.lisp by the cold-cache test.

(eval-when (:compile-toplevel)
  (let* ((dir (uiop:getenv-pathname "CL_MCP_BOOT_PROBE_DIR" :ensure-directory t))
         (claim (merge-pathnames "compiling" dir)))
    (when (probe-file (merge-pathnames "abort" dir))
      (write-line "Aborting the bootstrap compiler" *error-output*)
      (finish-output *error-output*)
      (sb-ext:exit :code 42 :abort t))
    ;; Stretch the window in which two ASDFs would compile the same output.
    (with-open-file (owner claim :direction :output :if-exists nil)
      (unless owner (error "Concurrent compilation of worker/server in the shared cache"))
      (unwind-protect (sleep 2)
        (delete-file claim)))))

(eval-when (:load-toplevel)
  (let ((dir (uiop:getenv-pathname "CL_MCP_BOOT_PROBE_DIR" :ensure-directory t)))
    (when (probe-file (merge-pathnames "warm" dir))
      ;; A pair of warm readers must both enter the real ASDF load. A lock
      ;; held exclusively all the way through every warm startup deadlocks here.
      (with-open-file (out (merge-pathnames (format nil "reader-~D" (sb-posix:getpid)) dir)
                           :direction :output)
        (write-line "loading" out))
      (unless (loop repeat 100
                    when (= 2 (length (directory (merge-pathnames "reader-*" dir))))
                      return t
                    do (sleep 0.05))
        (error "Warm worker loads did not overlap")))))
