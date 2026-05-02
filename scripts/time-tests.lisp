;;;; scripts/time-tests.lisp
;;;;
;;;; Profile cl-mcp test suite by timing each sub-system in one SBCL.
;;;;
;;;; Usage (from project root):
;;;;   sbcl --non-interactive \
;;;;        --load ~/.config/quicklisp/setup.lisp \
;;;;        --load scripts/time-tests.lisp
;;;;
;;;; Output: per-system wall time on *error-output* as each test runs,
;;;; followed by a sorted summary on *standard-output*. Captures the
;;;; baseline cost picture used to decide where to focus optimization
;;;; work next (pool-test consolidation, fast/heavy tier split, etc.).

(in-package #:cl-user)

(require :asdf)

(let ((cwd (truename ".")))
  (pushnew cwd asdf:*central-registry* :test #'equal))

(format *error-output* "~&Loading cl-mcp/tests...~%")
(let ((load-start (get-internal-real-time)))
  (asdf:load-system "cl-mcp/tests")
  (format *error-output* "  done in ~,2Fs~%~%"
          (/ (float (- (get-internal-real-time) load-start))
             internal-time-units-per-second)))

(defun discover-test-systems ()
  "Return a sorted list of cl-mcp test sub-system designators."
  (let ((systems
          (loop for f in (directory "tests/*-test.lisp")
                for name = (pathname-name f)
                ;; Skip helper-only files; keep only -test.lisp suites.
                when (and (search "-test" name :from-end t)
                          (not (search "test-helpers" name)))
                  collect (concatenate 'string "cl-mcp/tests/" name))))
    (sort systems #'string<)))

(defun time-one-system (system-string)
  "Run SYSTEM-STRING via rove:run; return wall-clock seconds."
  (let ((start (get-internal-real-time))
        (designator (intern (string-upcase system-string) :keyword)))
    (handler-case
        (let ((rove-run (or (find-symbol "RUN" :rove)
                            (error "rove:run not found"))))
          ;; Suppress rove's chatter; we only want timing.
          (let ((*standard-output* (make-broadcast-stream))
                (*error-output*    (make-broadcast-stream)))
            (funcall rove-run designator)))
      (error (c)
        (format *error-output* "~&  ! ~A errored: ~A~%" system-string c)))
    (/ (float (- (get-internal-real-time) start))
       internal-time-units-per-second)))

(defun format-row (secs system stream)
  (format stream "~8,2Fs  ~A~%" secs system))

(let* ((systems (discover-test-systems))
       (suite-start (get-internal-real-time))
       (results
         (loop for sys in systems
               for elapsed = (time-one-system sys)
               do (format-row elapsed sys *error-output*)
               collect (cons sys elapsed)))
       (suite-elapsed
         (/ (float (- (get-internal-real-time) suite-start))
            internal-time-units-per-second)))
  (format t "~&~%=== Per-system wall time, sorted descending ===~%")
  (dolist (row (sort (copy-list results) #'> :key #'cdr))
    (format-row (cdr row) (car row) *standard-output*))
  (let ((sum (reduce #'+ results :key #'cdr :initial-value 0.0)))
    (format t "~%Sum of per-system times: ~,2Fs (~,1F min)~%"
            sum (/ sum 60))
    (format t "Wall time (incl. overhead): ~,2Fs (~,1F min)~%"
            suite-elapsed (/ suite-elapsed 60))))
