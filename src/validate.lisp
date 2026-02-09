;;;; src/validate.lisp

(defpackage #:cl-mcp/src/validate
  (:use #:cl)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-file)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:text-content
                #:arg-validation-error)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:export #:lisp-check-parens
           #:*check-parens-max-bytes*))

(in-package #:cl-mcp/src/validate)

(defparameter *check-parens-max-bytes* (* 2 1024 1024)
  "Maximum number of characters lisp-check-parens will scan in one call.")

(defun %closing (opener)
  (ecase opener
    (#\( #\))
    (#\[ #\])
    (#\{ #\})))

(defun %scan-parens-push-open (stack line col base-offset ch idx)
  (cons (list ch line col (+ base-offset idx)) stack))

(defun %scan-parens-pop-open (stack line col base-offset ch idx)
  (if (null stack)
      (values stack
              (list :ok nil
                    :kind "extra-close"
                    :expected nil
                    :found (string ch)
                    :offset (+ base-offset idx)
                    :line line
                    :column col))
      (destructuring-bind (top-ch top-line top-col top-off) (car stack)
        (declare (ignore top-line top-col top-off))
        (let ((expected (%closing top-ch)))
          (if (char= expected ch)
              (values (cdr stack) nil)
              (values stack
                      (list :ok nil
                            :kind "mismatch"
                            :expected (string expected)
                            :found (string ch)
                            :offset (+ base-offset idx)
                            :line line
                            :column col)))))))

(defstruct scan-state
  (line 1 :type fixnum)
  (col 1 :type fixnum)
  (stack '() :type list)
  (in-string nil :type boolean)
  (escape nil :type boolean)
  (line-comment nil :type boolean)
  (block-depth 0 :type fixnum))

(defun %scan-handle-line-comment (state ch)
  (when (char= ch #\Newline)
    (setf (scan-state-line-comment state) nil)))

(defun %scan-handle-string (state ch)
  (cond
    ((scan-state-escape state)
     (setf (scan-state-escape state) nil))
    ((char= ch #\\)
     (setf (scan-state-escape state) t))
    ((char= ch #\")
     (setf (scan-state-in-string state) nil))))

(defun %scan-handle-block-comment (state ch next)
  (when (and (char= ch #\|) next (char= next #\#))
    (decf (scan-state-block-depth state))
    t))

(defun %scan-handle-normal (state ch next idx base-offset)
  (cond
    ((char= ch #\;)
     (setf (scan-state-line-comment state) t)
     (values nil nil))
    ((char= ch #\")
     (setf (scan-state-in-string state) t)
     (values nil nil))
    ((and (char= ch #\#) next (char= next #\|))
     (incf (scan-state-block-depth state))
     (values nil t))
    ((or (char= ch #\() (char= ch #\[) (char= ch #\{))
     (setf (scan-state-stack state)
           (%scan-parens-push-open (scan-state-stack state)
                                   (scan-state-line state)
                                   (scan-state-col state)
                                   base-offset
                                   ch
                                   idx))
     (values nil nil))
    ((or (char= ch #\)) (char= ch #\]) (char= ch #\}))
     (multiple-value-bind (new-stack err)
         (%scan-parens-pop-open (scan-state-stack state)
                                (scan-state-line state)
                                (scan-state-col state)
                                base-offset
                                ch
                                idx)
       (setf (scan-state-stack state) new-stack)
       (values err nil)))
    (t (values nil nil))))

(defun %scan-advance-position (state ch)
  (cond
    ((char= ch #\Newline)
     (incf (scan-state-line state))
     (setf (scan-state-col state) 1))
    (t
     (incf (scan-state-col state)))))

(defun %scan-parens (text &key (base-offset 0))
  "Return a plist describing balance of delimiters in TEXT.
Keys: :ok (boolean), :kind (string|nil), :expected, :found, :offset, :line, :column."
  (let ((state (make-scan-state)))
    (loop for idx from 0 below (length text)
          for ch = (char text idx)
          for next = (and (< (1+ idx) (length text))
                          (char text (1+ idx))) do
            (cond
              ((scan-state-line-comment state)
               (%scan-handle-line-comment state ch))
              ((scan-state-in-string state)
               (%scan-handle-string state ch))
              ((plusp (scan-state-block-depth state))
               (when (%scan-handle-block-comment state ch next)
                 (incf idx)
                 (incf (scan-state-col state))))
              (t
               (multiple-value-bind (err consumed)
                   (%scan-handle-normal state ch next idx base-offset)
                 (when err
                   (return-from %scan-parens err))
                 (when consumed
                   (incf idx)
                   (incf (scan-state-col state))))))
            (%scan-advance-position state ch))
    (when (scan-state-stack state)
      (destructuring-bind (ch l c off) (pop (scan-state-stack state))
        (return-from %scan-parens
          (list :ok nil
                :kind "unclosed"
                :expected (string (%closing ch))
                :found nil
                :offset off
                :line l
                :column c))))
    (list :ok t)))

(defun %maybe-add-lisp-edit-guidance (result kind)
  "Attach machine-readable remediation hints for broken Lisp delimiters."
  (when (member kind '("extra-close" "mismatch" "unclosed") :test #'string=)
    (setf (gethash "fix_code" result) "use_lisp_edit_form"
          (gethash "next_tool" result) "lisp-edit-form"
          (gethash "required_args" result)
          (vector "file_path" "form_type" "form_name" "operation" "content")))
  result)



(defun lisp-check-parens (&key path code offset limit)
  "Check balanced parentheses/brackets in CODE or PATH slice.
Returns a hash table with keys \"ok\" and, when not ok, \"kind\", \"expected\",
\"found\", and \"position\"."
  (when (and path code)
    (error "Provide either PATH or CODE, not both"))
  (when (and (null path) (null code))
    (error "Either PATH or CODE is required"))
  (when (and offset (< offset 0))
    (error "offset must be non-negative"))
  (when (and limit (< limit 0))
    (error "limit must be non-negative"))
  (let ((text (or code (fs-read-file path :offset offset :limit limit)))
        (base-off (or offset 0)))
    (when (> (length text) *check-parens-max-bytes*)
      (let ((h (make-hash-table :test #'equal)))
        (setf (gethash "ok" h) nil
              (gethash "kind" h) "too-large"
              (gethash "expected" h) nil
              (gethash "found" h) nil)
        (let ((pos (make-hash-table :test #'equal)))
          (setf (gethash "offset" pos) base-off
                (gethash "line" pos) 1
                (gethash "column" pos) 1)
          (setf (gethash "position" h) pos))
        (return-from lisp-check-parens h)))
    (destructuring-bind (&key ok kind expected found offset line column)
        (%scan-parens text :base-offset base-off)
      (let ((h (make-hash-table :test #'equal)))
        (setf (gethash "ok" h) (and ok t))
        (unless ok
          (setf (gethash "kind" h) kind
                (gethash "expected" h) expected
                (gethash "found" h) found)
          (let ((pos (make-hash-table :test #'equal)))
            (setf (gethash "offset" pos) offset
                  (gethash "line" pos) line
                  (gethash "column" pos) column)
            (setf (gethash "position" h) pos))
          (%maybe-add-lisp-edit-guidance h kind))
        h))))

(define-tool "lisp-check-parens"
  :description "Check balanced parentheses/brackets in a file slice or provided code.
Use this to DIAGNOSE syntax errors in existing files or validate code snippets
before/after editing. Returns the first mismatch position if unbalanced, or
success if balanced. Unbalanced delimiter results include guidance to use
lisp-edit-form for existing Lisp files."
  :args ((path :type :string
               :description "Absolute path inside project or registered ASDF system
(mutually exclusive with code)")
         (code :type :string
               :description "Raw code string to check (mutually exclusive with path)")
         (offset :type :integer
                 :description "0-based character offset when reading from path")
         (limit :type :integer
                :description "Maximum characters to read from path"))
  :body
  (progn
    (when (and path code)
      (error 'arg-validation-error
             :arg-name "path/code"
             :message "Provide either path or code, not both"))
    (when (and (null path) (null code))
      (error 'arg-validation-error
             :arg-name "path/code"
             :message "Either path or code is required"))
    (let* ((check-result (lisp-check-parens :path path
                                            :code code
                                            :offset offset
                                            :limit limit))
           (ok (gethash "ok" check-result))
           (next-tool (gethash "next_tool" check-result))
           (summary
            (if ok
                "Parentheses are balanced"
                (let* ((kind (gethash "kind" check-result))
                       (expected (gethash "expected" check-result))
                       (found (gethash "found" check-result))
                       (pos (gethash "position" check-result))
                       (line (and pos (gethash "line" pos)))
                       (col (and pos (gethash "column" pos))))
                  (format nil
                          "Unbalanced parentheses: ~A~@[ (expected ~A, found ~A)~] at line ~D, column ~D~A"
                          kind expected found line col
                          (if next-tool
                              " Use lisp-edit-form for existing Lisp files."
                              ""))))))
      (let* ((payload
               (make-ht "content" (text-content summary)
                        "ok" ok
                        "kind" (gethash "kind" check-result)
                        "expected" (gethash "expected" check-result)
                        "found" (gethash "found" check-result)
                        "position" (gethash "position" check-result)))
             (fix-code (gethash "fix_code" check-result))
             (required-args (gethash "required_args" check-result)))
        (when fix-code
          (setf (gethash "fix_code" payload) fix-code))
        (when next-tool
          (setf (gethash "next_tool" payload) next-tool))
        (when required-args
          (setf (gethash "required_args" payload) required-args))
        (result id payload)))))
