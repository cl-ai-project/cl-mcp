;;;; src/frame-inspector.lisp
;;;;
;;;; Structured error context capture with SBCL frame inspection.

(defpackage #:cl-mcp/src/frame-inspector
  (:use #:cl)
  (:export #:capture-error-context))

(in-package #:cl-mcp/src/frame-inspector)

(defun %safe-format-value (value print-level print-length)
  "Safely format VALUE to string, handling errors during printing."
  (handler-case
      (let ((*print-level* print-level)
            (*print-length* print-length)
            (*print-readably* nil)
            (*print-circle* t))
        (prin1-to-string value))
    (error (e)
      (format nil "#<error printing: ~A>" (type-of e)))))

(defun %collect-restarts ()
  "Return list of available restarts with names and descriptions."
  (loop for restart in (compute-restarts)
        collect (list :name (string (restart-name restart))
                      :description (or (ignore-errors
                                         (princ-to-string restart))
                                       ""))))

#+sbcl
(defun %frame-locals (frame print-level print-length)
  "Extract local variable names and values from FRAME."
  (let ((debug-fun (sb-di:frame-debug-fun frame))
        (locals '()))
    (handler-case
        (sb-di:do-debug-fun-vars (var debug-fun)
          (when (eq (sb-di:debug-var-validity var (sb-di:frame-code-location frame))
                    :valid)
            (handler-case
                (let* ((sym (sb-di:debug-var-symbol var))
                       (val (sb-di:debug-var-value var frame)))
                  (push (list :name (symbol-name sym)
                              :value (%safe-format-value val print-level print-length))
                        locals))
              (error () nil))))
      (error () nil))
    (nreverse locals)))

#+sbcl
(defun %frame-source-location (frame)
  "Extract source file and line from FRAME, or NIL if unavailable."
  (handler-case
      (let ((code-location (sb-di:frame-code-location frame)))
        (when code-location
          (let ((debug-source (sb-di:code-location-debug-source code-location)))
            (when debug-source
              (let ((namestring (sb-di:debug-source-namestring debug-source)))
                (when namestring
                  (list :file namestring
                        :line (ignore-errors
                                (sb-di:code-location-toplevel-form-offset code-location)))))))))
    (error () nil)))

#+sbcl
(defun %frame-function-name (frame)
  "Extract function name from FRAME as a string."
  (handler-case
      (let* ((debug-fun (sb-di:frame-debug-fun frame))
             (name (sb-di:debug-fun-name debug-fun)))
        (if name
            (prin1-to-string name)
            "<anonymous>"))
    (error () "<unknown>")))

#+sbcl
(defun %collect-frames (max-frames print-level print-length)
  "Walk stack and collect frame information using SBCL backtrace API."
  (let ((frames '())
        (index 0))
    (handler-case
        ;; Use funcall with intern to avoid reader errors for internal symbols
        (let ((map-fn (ignore-errors
                        (fdefinition (find-symbol "MAP-BACKTRACE" "SB-DEBUG")))))
          (when map-fn
            (funcall map-fn
                     (lambda (frame)
                       (when (< index max-frames)
                         (let ((source-loc (%frame-source-location frame)))
                           (push (list :index index
                                       :function (%frame-function-name frame)
                                       :source-file (getf source-loc :file)
                                       :source-line (getf source-loc :line)
                                       :locals (%frame-locals frame print-level print-length))
                                 frames))
                         (incf index))))))
      (error () nil))
    (nreverse frames)))

#-sbcl
(defun %collect-frames (max-frames print-level print-length)
  "Fallback for non-SBCL: return empty frame list."
  (declare (ignore max-frames print-level print-length))
  nil)

(defun capture-error-context (condition &key (max-frames 20)
                                             (print-level 3)
                                             (print-length 10))
  "Capture structured error context including frames and locals.

Returns a plist with:
  :error t
  :condition-type - string name of condition type
  :message - formatted error message
  :restarts - list of (:name STRING :description STRING)
  :frames - list of frame plists (SBCL only, NIL on other implementations)"
  (list :error t
        :condition-type (prin1-to-string (type-of condition))
        :message (handler-case
                     (princ-to-string condition)
                   (error () "<error formatting condition>"))
        :restarts (%collect-restarts)
        :frames (%collect-frames max-frames print-level print-length)))
