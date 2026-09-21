;;;; tests/repl-error-context-test.lisp
;;;;
;;;; Tests for structured error context in repl-eval.

(defpackage #:cl-mcp/tests/repl-error-context-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/repl
                #:repl-eval)
  (:import-from #:cl-mcp/src/utils/request-debugger-boundary
                #:*request-debugger-boundary-active*))

(in-package #:cl-mcp/tests/repl-error-context-test)

(define-condition repl-boundary-condition (condition)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "repl debugger snapshot" stream))))

(defun repl-boundary-frame ()
  "Enter the debugger with a user frame and restart still live."
  (declare (optimize (debug 3) (speed 0)))
  (restart-case (invoke-debugger (make-condition 'repl-boundary-condition))
    (repl-boundary-restart () :recovered)))

(deftest repl-debugger-escape-preserves-pre-unwind-context
  (let ((*request-debugger-boundary-active* t))
    (multiple-value-bind (printed raw stdout stderr error-context)
        (repl-eval "(repl-boundary-frame)"
                   :package "CL-MCP/TESTS/REPL-ERROR-CONTEXT-TEST"
                   :timeout-seconds 2)
      (declare (ignore stdout stderr))
      (ok (equal printed raw))
      (ok (search "repl debugger snapshot" printed))
      (ok (search "REPL-BOUNDARY-CONDITION" (getf error-context :condition-type)))
      (ok (equal "repl debugger snapshot" (getf error-context :message)))
      (ok (find-if (lambda (frame)
                     (search "REPL-BOUNDARY-FRAME"
                             (or (getf frame :function) "")))
                   (getf error-context :frames)))
      (ok (find "REPL-BOUNDARY-RESTART" (getf error-context :restarts)
                :key (lambda (restart) (getf restart :name))
                :test #'search)))))

(deftest repl-debugger-output-keeps-bounds-and-sanitization
  (let ((*request-debugger-boundary-active* t))
    (dolist (limit '(4 0))
      (multiple-value-bind (printed raw stdout stderr context)
          (repl-eval
           "(write-string \"A\")
            (write-char (code-char 0))
            (write-string \"BCDE\")
            (write-string \"stderr\" *error-output*)
            (invoke-debugger (make-condition 'repl-boundary-condition))"
           :package "CL-MCP/TESTS/REPL-ERROR-CONTEXT-TEST"
           :timeout-seconds 2 :max-output-length limit)
        (declare (ignore printed raw))
        (ok (search "REPL-BOUNDARY-CONDITION" (getf context :condition-type)))
        (ok (equal (if (zerop limit)
                       "... (truncated, 6 total chars)"
                       (format nil "ABC~%... (truncated, 6 total chars)"))
                   stdout))
        ;; SBCL can append compilation-unit abort diagnostics on stderr
        ;; during unwind. Only the first LIMIT characters may be retained.
        (ok (eql 0 (search (if (zerop limit)
                              "... (truncated, "
                              (format nil "stde~%... (truncated, "))
                          stderr)))
        (ok (search " total chars)" stderr))))))

(deftest repl-debugger-output-snapshot-failure-preserves-original-context
  (let* ((*request-debugger-boundary-active* t)
         (name 'cl-mcp/src/repl-core::%captured-output)
         (original (fdefinition name))
         (calls 0))
    (unwind-protect
         (progn
           (setf (fdefinition name)
                 (lambda (stream)
                   (if (= 1 (incf calls))
                       (error "unexpected output snapshot failure")
                       (funcall original stream))))
           (multiple-value-bind (printed raw stdout stderr context)
               (repl-eval
                "(write-string \"stdout\")
                 (write-string \"stderr\" *error-output*)
                 (invoke-debugger (make-condition 'repl-boundary-condition))"
                :package "CL-MCP/TESTS/REPL-ERROR-CONTEXT-TEST" :timeout-seconds 2)
             (declare (ignore printed raw))
             (ok (= 2 calls) "each bounded capture gets one independent snapshot attempt")
             (ok (equal "" stdout))
             (ok (eql 0 (search "stderr" stderr)))
             (ok (equal "repl debugger snapshot" (getf context :message)))
             (ok (search "REPL-BOUNDARY-CONDITION" (getf context :condition-type)))))
      (setf (fdefinition name) original))))

(defvar *repl-config-locals* nil)

(defun repl-config-frame (structured)
  (declare (optimize (debug 3) (speed 0)))
  (unwind-protect
       (invoke-debugger (make-condition 'repl-boundary-condition))
    (setf *repl-config-locals* structured)))

(deftest repl-debugger-settings-reach-nested-deadline-child
  (let ((*request-debugger-boundary-active* t))
    (multiple-value-bind (printed context stdout stderr error-context)
        (repl-eval
         "(multiple-value-bind (escaped status leaked)
              (cl-mcp/src/utils/deadline:call-with-deadline-thread
               (lambda () (repl-config-frame #(#(1 2 3) #(4 5 6) #(7 8 9))))
               1)
            (assert (and (eq status :error) (not leaked)))
            (cl-mcp/src/utils/request-debugger-boundary:request-debugger-escape-error-context
             escaped))"
         :package "CL-MCP/TESTS/REPL-ERROR-CONTEXT-TEST" :timeout-seconds 3
         :print-level 1 :print-length 2 :locals-preview-frames 1
         :locals-preview-max-depth 2 :locals-preview-max-elements 2)
      (declare (ignore printed stdout stderr))
      (ok (null error-context) "the outer child normally returns the inner saved context")
      (let* ((frame (find "REPL-CONFIG-FRAME" (getf context :frames)
                          :key (lambda (frame) (getf frame :function)) :test #'search))
             (local (find "STRUCTURED" (getf frame :locals)
                          :key (lambda (local) (getf local :name)) :test #'equal))
             (preview (getf local :preview)))
        (ok (hash-table-p preview) "nested managed child inherits requested preview settings")
        (ok (equal "#(# # ...)" (getf local :value)))
        (when preview
          (let* ((elements (gethash "elements" preview))
                 (child (first elements)))
            (ok (= 2 (length elements)))
            (ok (equal "array" (gethash "kind" child)))
            (ok (= 2 (length (gethash "elements" child))))))))))

(deftest repl-eval-returns-error-context
  (testing "repl-eval returns structured error context on error"
    (multiple-value-bind (printed raw stdout stderr error-context)
        (repl-eval "(error \"Test error\")" :package "CL-USER")
      (declare (ignore raw stdout stderr))
      ;; The printed output should contain the error message
      (ok (search "Test error" printed))
      ;; Error context should be a plist with :error t
      (ok error-context)
      (ok (getf error-context :error))
      (ok (stringp (getf error-context :condition-type)))
      (ok (stringp (getf error-context :message)))
      (ok (listp (getf error-context :restarts)))
      (ok (listp (getf error-context :frames))))))

(deftest repl-eval-no-error-context-on-success
  (testing "repl-eval returns nil error-context on success"
    (multiple-value-bind (printed raw stdout stderr error-context)
        (repl-eval "(+ 1 2)" :package "CL-USER")
      (declare (ignore printed raw stdout stderr))
      (ok (null error-context)))))

(deftest repl-eval-error-context-has-frames
  (testing "error context includes stack frames on SBCL"
    (multiple-value-bind (printed raw stdout stderr error-context)
        (repl-eval "(labels ((foo () (bar))
                            (bar () (error \"deep error\")))
                     (foo))"
                   :package "CL-USER")
      (declare (ignore printed raw stdout stderr))
      (ok error-context)
      #+sbcl
      (progn
        (ok (> (length (getf error-context :frames)) 0))
        ;; Frames should have function names
        (let ((first-frame (first (getf error-context :frames))))
          (ok (stringp (getf first-frame :function))))))))

(deftest repl-eval-error-context-respects-print-limits
  (testing "error context uses print-level and print-length"
    (multiple-value-bind (printed raw stdout stderr error-context)
        (repl-eval "(let ((x '((((deeply nested))))))
                     (error \"with deep local\"))"
                   :package "CL-USER"
                   :print-level 2
                   :print-length 3)
      (declare (ignore printed raw stdout stderr))
      (ok error-context)
      ;; Should complete without error even with complex locals
      (ok (getf error-context :error)))))

(deftest repl-eval-timeout-no-error-context
  (testing "timeout returns nil error-context"
    (multiple-value-bind (printed raw stdout stderr error-context)
        (repl-eval "(loop)" :package "CL-USER" :timeout-seconds 0.1)
      (declare (ignore printed stdout stderr))
      (ok (eq raw :timeout))
      (ok (null error-context)))))

(deftest backtrace-falls-back-when-all-frames-internal
  (testing
   "build-eval-response shows backtrace even when every frame is internal"
   (let* ((ctx
           (list :error t
                 :condition-type "SIMPLE-ERROR"
                 :message "test"
                 :restarts nil
                 :frames
                 (list (list :index 0 :function "SB-KERNEL::ERROR"
                             :source-file nil :source-line nil :locals nil)
                       (list :index 1 :function "SB-INT::FOO"
                             :source-file nil :source-line nil :locals nil))))
          (resp (cl-mcp/src/tools/response-builders:build-eval-response
                 "" nil "" "" ctx))
          (content (gethash "content" resp))
          (text (when (and (vectorp content) (plusp (length content)))
                  (gethash "text" (aref content 0)))))
     (ok text)
     (ok (search "Backtrace:" text)
         "Backtrace header appears despite all frames being internal")
     (ok (search "SB-KERNEL::ERROR" text)
         "fallback includes the first would-be-filtered frame"))))

(defun %response-text (error-context &key max-output-length)
  "Return the content text BUILD-EVAL-RESPONSE renders for ERROR-CONTEXT.
MAX-OUTPUT-LENGTH is passed through, so a test can put the text under the same
budget pressure a real response is under."
  (let* ((resp (cl-mcp/src/tools/response-builders:build-eval-response
                "" nil "" "" error-context
                :max-output-length max-output-length))
         (content (gethash "content" resp)))
    (when (and (vectorp content) (plusp (length content)))
      (gethash "text" (aref content 0)))))

(deftest backtrace-text-carries-each-frames-locals
  (testing "a frame's locals reach content[].text, not only the JSON"
    ;; They used to reach the response as error_context.frames[].locals alone,
    ;; which a client rendering content[].text never sees -- so
    ;; locals_preview_frames and its companions produced nothing observable.
    (let ((text (%response-text
                 (list :error t
                       :condition-type "SIMPLE-ERROR"
                       :message "test"
                       :restarts nil
                       :frames
                       (list (list :index 0 :function "MY-APP::CRUNCH"
                                   :source-file "src/a.lisp" :source-line 7
                                   :locals (list (list :name "COUNT" :value "42")
                                                 (list :name "LABEL"
                                                       :value "\"trouble\"")
                                                 (list :name "TABLE"
                                                       :value "#<HASH-TABLE>"
                                                       :object-id 99))))))))
      (ok (search "locals:" text) "the section is labelled")
      (ok (search "COUNT = 42" text))
      (ok (search "LABEL = \"trouble\"" text)
          "a string local keeps the quotes its printed value carries")
      (ok (search "TABLE = #<HASH-TABLE>  [object-id: 99]" text)
          "a non-primitive local names the id inspect-object drills into"))))

(deftest backtrace-text-omits-the-locals-label-for-a-frame-with-none
  (testing "a frame with no locals gets no empty locals: heading"
    (let ((text (%response-text
                 (list :error t :condition-type "SIMPLE-ERROR" :message "test"
                       :restarts nil
                       :frames (list (list :index 0 :function "MY-APP::F"
                                           :source-file nil :source-line nil
                                           :locals nil))))))
      (ok (search "MY-APP::F" text))
      (ok (not (search "locals:" text))))))

(deftest backtrace-text-caps-the-locals-it-lists
  (testing "a frame with many locals is cut, and says how many are left"
    (let* ((cap cl-mcp/src/tools/response-builders::*locals-shown-per-frame*)
           (extra 3)
           (locals (loop for i from 1 to (+ cap extra)
                         collect (list :name (format nil "V~D" i)
                                       :value (princ-to-string i))))
           (text (%response-text
                  (list :error t :condition-type "SIMPLE-ERROR" :message "test"
                        :restarts nil
                        :frames (list (list :index 0 :function "MY-APP::WIDE"
                                            :source-file nil :source-line nil
                                            :locals locals))))))
      (ok (search (format nil "V~D = ~D" cap cap) text)
          "the last local within the cap is listed")
      (ok (not (search (format nil "V~D = " (1+ cap)) text))
          "the first one past it is not")
      (ok (search (format nil "... and ~D more" extra) text)
          "and the remainder is counted rather than dropped silently"))))

(deftest locals-preview-frames-is-visible-in-the-text
  (testing "the argument expands a non-primitive local in place, and only when asked"
    ;; End to end: a real function compiled at (debug 3), through repl-eval and
    ;; the response builder, because the defect was that nothing the argument
    ;; produced ever reached the text.
    (let ((code "(defun %locals-probe-crunch (words)
  (declare (optimize (debug 3)))
  (let ((table (make-hash-table :test #'equal)))
    (dolist (w words) (incf (gethash w table 0)))
    (error \"crunch failed\")))
(%locals-probe-crunch (list \"a\" \"b\" \"a\"))"))
      (flet ((text-for (frames)
               (multiple-value-bind (printed raw stdout stderr ctx)
                   (apply #'repl-eval code :package "CL-USER"
                          (when frames (list :locals-preview-frames frames)))
                 (declare (ignore printed raw stdout stderr))
                 (%response-text ctx))))
        (let ((without (text-for nil))
              (with (text-for 3)))
          #+sbcl
          (progn
            (ok (search "TABLE = " without)
                "the local is listed whether or not a preview was asked for")
            (ok (not (search "Entries (" without))
                "without the argument nothing is expanded under it")
            (ok (search "TABLE = " with))
            (ok (search "Entries (" with)
                "with it the hash-table's entries are expanded in place")
            (ok (search "a => 2" with)
                "and those entries are the real contents")))))))

(deftest one-huge-local-does-not-evict-the-frames-below-it
  (testing "a long value is cut, so later locals and caller frames survive"
    ;; print_level and print_length bound a printed structure's depth and its
    ;; element count; neither applies to a string, so a local holding one
    ;; prints in full.  The text is truncated whole at max_output_length
    ;; afterwards, so without a per-value cut the first such local pushed its
    ;; own siblings and every caller frame below it off the end -- taking away
    ;; frames that were visible before locals were written here at all.
    (let* ((big (make-string 3000 :initial-element #\x))
           (ctx (list :error t :condition-type "SIMPLE-ERROR" :message "test"
                      :restarts nil
                      :frames
                      (list (list :index 0 :function "MY-APP::VICTIM"
                                  :source-file nil :source-line nil
                                  :locals (list (list :name "BODY" :value big)
                                                (list :name "COUNT" :value "42")))
                            (list :index 1 :function "MY-APP::CALLER"
                                  :source-file nil :source-line nil
                                  :locals (list (list :name "N" :value "3000"))))))
           (text (%response-text ctx :max-output-length 900)))
      (ok (search "[cut, 3000 chars]" text)
          "the value says it was cut, and how big it was")
      (ok (search "COUNT = 42" text)
          "the local declared after it is still there")
      (ok (search "MY-APP::CALLER" text)
          "and so is the caller frame, which is what used to be lost")
      (ok (search "N = 3000" text) "with its own locals"))))
