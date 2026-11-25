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

(defun repl-break-frame ()
  "Call BREAK from a user frame, the way code stops to be looked at."
  (declare (optimize (debug 3) (speed 0)))
  (break "repl break ~A" :here)
  :resumed)

(deftest backtrace-of-a-break-starts-at-the-caller
  (testing "BREAK's own frames are signalling machinery, like ERROR's"
    (let ((*request-debugger-boundary-active* t))
      (multiple-value-bind (printed raw stdout stderr error-context)
          (repl-eval "(repl-break-frame)"
                     :package "CL-MCP/TESTS/REPL-ERROR-CONTEXT-TEST"
                     :timeout-seconds 2)
        (declare (ignore printed raw stdout stderr))
        #+sbcl
        (let ((names (mapcar (lambda (frame) (or (getf frame :function) ""))
                             (getf error-context :frames))))
          (ok (search "repl break HERE" (getf error-context :message))
              "the break's message is kept")
          (ok (and names (search "REPL-BREAK-FRAME" (first names)))
              (format nil "the frame that called BREAK is the first: ~S" names))
          (ok (notany (lambda (name) (search "BREAK" name :test #'char-equal))
                      (remove-if (lambda (name) (search "REPL-BREAK-FRAME" name))
                                 names))
              (format nil "neither BREAK nor SB-INT:%BREAK is listed: ~S" names)))))))

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

(deftest backtrace-text-says-what-it-leaves-out-and-keeps-the-outermost-frame
  ;; Five frames were listed and the text ended there, with no word of the
  ;; rest.  A recursive LABELS fills all five, so the function the user called
  ;; -- the outermost frame -- never appeared.
  (testing "a recursion is one entry: its first and last calls, and how many between"
    (let ((text (%response-text
                 (list :error t :condition-type "SIMPLE-ERROR" :message "bottom"
                       :restarts nil :frames-total 10
                       :frames (append
                                (loop for index from 0 below 9
                                      collect (list :index index
                                                    :function "(LABELS APP::DOWN :IN APP::OUTER)"
                                                    :locals nil))
                                (list (list :index 9 :function "APP::OUTER" :locals nil)))))))
      (ok (search "0: (LABELS APP::DOWN" text) (format nil "the innermost call: ~A" text))
      (ok (search "1-7: 7 more calls of the same function" text) "the ones between, counted")
      (ok (search "8: (LABELS APP::DOWN" text) "the outermost call")
      (ok (search "9: APP::OUTER" text) "and the function the user called")))
  (flet ((context (truncated total)
           (list :error t :condition-type "SIMPLE-ERROR" :message "bottom"
                 :restarts nil :frames-truncated truncated :frames-total total
                 :frames (append
                          (loop for index from 0 below 7
                                collect (list :index index
                                              :function (format nil "APP::F~D" index)
                                              :locals nil))
                          (list (list :index (1- total)
                                      :function "APP::OUTER" :locals nil))))))
    (testing "past the limit the outermost frame is kept and the rest counted"
      (let ((text (%response-text (context nil 8))))
        (ok (search "7: APP::OUTER" text)
            (format nil "the outermost frame is shown: ~A" text))
        (ok (search "3 more frames left out here" text) "the ones left out are counted")
        (ok (search "error_context.frames has all 8 of them" text)
            "with where to find them")
        (ok (null (search "APP::F4" text)) "the middle of the run is what is left out")))
    (testing "a capture its cap cut short says how many the stack held"
      (let ((text (%response-text (context t 30))))
        (ok (search "29: APP::OUTER" text)
            (format nil "the outermost frame is still shown: ~A" text))
        (ok (search "error_context.frames has 8 of the 30" text)
            "and the count says what was not recorded")))))

(deftest backtrace-text-names-sbcl-frames-without-their-locals
  ;; SBCL's own frames are named, so nothing is hidden, but their locals --
  ;; SBCL's internals -- stay in error_context.frames.
  (let* ((context
           (list :error t :condition-type "SIMPLE-ERROR" :message "x"
                 :restarts nil :frames-total 4
                 :frames (list (list :index 0 :function "(LAMBDA (X) :IN \"repl-eval\")"
                                     :locals (list (list :name "X" :value "41")))
                               (list :index 1 :function "SB-IMPL::MAP1" :implementation t
                                     :locals (list (list :name "SB-IMPL::LISTS"
                                                         :value "((41))")))
                               (list :index 2 :function "SB-IMPL::OTHER" :implementation t
                                     :locals nil)
                               (list :index 3 :function "MAPCAR" :locals nil))))
         (text (%response-text context))
         (json (gethash "frames"
                        (gethash "error_context"
                                 (cl-mcp/src/tools/response-builders:build-eval-response
                                  "" nil "" "" context)))))
    (ok (search "1-2: SB-IMPL::MAP1, SB-IMPL::OTHER (SBCL)" text)
        (format nil "consecutive SBCL frames share one line: ~A" text))
    (ok (null (search "SB-IMPL::LISTS" text)) "without their locals")
    (ok (search "X = 41" text) "the user's frames keep theirs")
    (ok (search "3: MAPCAR" text) "and a standard function is shown as itself")
    (ok (eq t (gethash "implementation" (aref json 1)))
        "error_context.frames marks SBCL's frames")
    (ok (gethash "locals" (aref json 1)) "and keeps their locals")))

(deftest backtrace-text-counts-a-real-recursion-truthfully
  ;; Through repl-eval's own path: every stack ends in the thread's frames, so
  ;; a flag set by any frame past the cap said every error's walk was cut short.
  (cl-mcp/src/repl:repl-eval
   "(defun rec-probe-outer (n)
      (declare (optimize (debug 3)))
      (labels ((rec-probe-down (k)
                 (if (zerop k) (error \"bottom\") (1+ (rec-probe-down (1- k))))))
        (rec-probe-down n)))")
  (flet ((text-for (n)
           ;; With a deadline, as the tool calls it: the evaluation then runs on
           ;; a thread of its own, so this test's frames are not on its stack.
           (multiple-value-bind (printed value stdout stderr context)
               (cl-mcp/src/repl:repl-eval (format nil "(rec-probe-outer ~D)" n)
                                          :timeout-seconds 60)
             (gethash "text"
                      (aref (gethash "content"
                                     (cl-mcp/src/tools/response-builders:build-eval-response
                                      printed value stdout stderr context))
                            0)))))
    (let ((text (text-for 8)))
      (ok (search "1-7: 7 more calls of the same function" text)
          (format nil "the recursion is counted: ~A" text))
      (ok (search "9: REC-PROBE-OUTER (repl-eval)" text) "and the function called is shown")
      (ok (search "N = 8" text) "with its locals"))
    (let ((text (text-for 40)))
      (ok (search "41: REC-PROBE-OUTER (repl-eval)" text)
          (format nil "past the cap the function called is still shown: ~A" text))
      (ok (search "frames not recorded here" text) "where frames were not recorded is said")
      (ok (search "error_context.frames has 20 of the 42" text) "and how many"))))

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
            (ok (search "\"a\" => 2" with)
                "and those entries are the real contents")))))))

#+sbcl
(defun repl-preview-spelling-frame (items stop)
  "Keep ITEMS live when STOP is called so it can be previewed."
  (declare (optimize (debug 3) (speed 0)))
  (funcall stop)
  items)

#+sbcl
(deftest locals-preview-keeps-lisp-spelling
  (let ((*request-debugger-boundary-active* t))
    (loop for (stop condition-type) in
          '(("(error \"preview probe\")" "SIMPLE-ERROR")
            ("(invoke-debugger (make-condition 'repl-boundary-condition))"
             "REPL-BOUNDARY-CONDITION"))
          do (multiple-value-bind (printed raw stdout stderr context)
                 (repl-eval
                  (format nil "(repl-preview-spelling-frame (list :k \"s\" #\\A 2)
                                 (lambda () ~A))" stop)
                  :package "CL-MCP/TESTS/REPL-ERROR-CONTEXT-TEST"
                  :locals-preview-frames 3 :timeout-seconds 3)
               (let* ((response (cl-mcp/src/tools/response-builders:build-eval-response
                                 printed raw stdout stderr context))
                      (error-context (gethash "error_context" response))
                      (frame (find "REPL-PREVIEW-SPELLING-FRAME" (gethash "frames" error-context)
                                   :key (lambda (frame) (gethash "function" frame))
                                   :test #'search))
                      (local (find "ITEMS" (gethash "locals" frame)
                                   :key (lambda (local) (gethash "name" local)) :test #'equal))
                      (elements (gethash "elements" (gethash "preview" local)))
                      (text (gethash "text" (aref (gethash "content" response) 0))))
                 (ok (equal condition-type (gethash "condition_type" error-context)))
                 (ok (equal '(("K" ":K") ("s" "\"s\"") ("A" "#\\A") (2 "2"))
                            (loop for element in elements
                                  collect (list (gethash "value" element)
                                                (gethash "printed" element))))
                     condition-type)
                 (ok (every (lambda (row) (search row text))
                            '("[0] :K" "[1] \"s\"" "[2] #\\A" "[3] 2"))
                     condition-type))))))

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

(deftest backtrace-of-a-top-level-form-starts-at-the-form
  (testing "a top-level form's error shows the form's frame first, with its locals preview"
    (multiple-value-bind (printed raw stdout stderr error-context)
        (repl-eval "(locally (declare (optimize (debug 3)))
                      (let ((sample-ht (make-hash-table :test #'equal)))
                        (setf (gethash \"status\" sample-ht) :testing)
                        (error \"top-level boom ~A\" sample-ht)))"
                   :package "CL-USER"
                   :locals-preview-frames 2)
      (let* ((resp (cl-mcp/src/tools/response-builders:build-eval-response
                    printed raw stdout stderr error-context))
             (text (gethash "text" (aref (gethash "content" resp) 0)))
             (json-ctx (gethash "error_context" resp)))
        #+sbcl
        (progn
          (ok (search "0: (LAMBDA () :IN \"repl-eval\")" text)
              (format nil "top-level repl-eval frame is the first shown: ~A" text))
          (ok (search "SAMPLE-HT" text)
              (format nil "top-level local variable is shown: ~A" text))
          (ok (search "\"status\" => :TESTING" text)
              (format nil "top-level local preview is expanded: ~A" text))
          (ok (null (search "%COLLECT-FRAMES" text))
              "capture harness frame is omitted from text")
          (ok (null (search "CAPTURE-ERROR-CONTEXT" text))
              "capture-error-context frame is omitted from text")
          (ok (null (search "NOTE-ERROR" text))
              "note-error frame is omitted from text")
          (ok (null (search "%EVAL-FORMS" text))
              "and so is the evaluator below the form")
          (ok (= 1 (length (gethash "frames" json-ctx)))
              "error_context.frames holds the form's frame alone"))))))

(deftest backtrace-of-a-compile-error-shows-the-form-alone
  (testing "a top-level COMPILED-PROGRAM-ERROR shows the form's frame and nothing of cl-mcp's"
    (multiple-value-bind (printed raw stdout stderr error-context)
        (repl-eval "(return-from nil 1)" :package "CL-USER")
      (let* ((resp (cl-mcp/src/tools/response-builders:build-eval-response
                    printed raw stdout stderr error-context))
             (text (gethash "text" (aref (gethash "content" resp) 0)))
             (json-ctx (gethash "error_context" resp))
             (json-frames (gethash "frames" json-ctx)))
        #+sbcl
        (progn
          (ok (search "0: (LAMBDA () :IN \"repl-eval\")" text)
              (format nil "the form's frame is shown: ~A" text))
          (ok (null (search "%COLLECT-FRAMES" text))
              "does not dump capture harness frames")
          (ok (null (search "FILTER-INTERNAL" text))
              "does not dump capture harness locals")
          (ok (and (vectorp json-frames) (= 1 (length json-frames)))
              "error_context.frames holds that frame alone")
          (ok (null (gethash "frames_truncated" json-ctx))
              "and nothing was cut"))))))

(deftest backtrace-says-so-when-the-form-itself-signalled
  (testing "an ERROR called by the evaluated form directly leaves no frame of the code"
    (multiple-value-bind (printed raw stdout stderr error-context)
        (repl-eval "(error \"plain ~A\" 1)" :package "CL-USER")
      (let* ((resp (cl-mcp/src/tools/response-builders:build-eval-response
                    printed raw stdout stderr error-context))
             (text (gethash "text" (aref (gethash "content" resp) 0)))
             (json-frames (gethash "frames" (gethash "error_context" resp))))
        #+sbcl
        (progn
          (ok (search "Backtrace: none -- the form evaluated signalled it itself" text)
              (format nil "the text says why there is no frame: ~A" text))
          (ok (zerop (length json-frames)) "error_context.frames is empty"))))))

(deftest backtrace-shows-a-cl-mcp-frame-like-any-other
  ;; cl-mcp is developed with its own tools: a cl-mcp function called from
  ;; repl-eval is the code being debugged, and a client renders only the text.
  (testing "a cl-mcp function called in repl-eval is shown with its locals"
    (let ((text (multiple-value-bind (printed raw stdout stderr context)
                    (repl-eval "(cl-mcp/src/frame-inspector::%sbcl-package-p 5)"
                               :package "CL-USER")
                  (gethash "text"
                           (aref (gethash "content"
                                          (cl-mcp/src/tools/response-builders:build-eval-response
                                           printed raw stdout stderr context))
                                 0)))))
      #+sbcl
      (progn
        (ok (search "CL-MCP/SRC/FRAME-INSPECTOR::%SBCL-PACKAGE-P" text)
            (format nil "the cl-mcp function's frame is shown: ~A" text))
        (ok (search "PACKAGE = 5" text) "with its locals")
        (ok (null (search "%EVAL-FORMS" text)) "and the evaluator below it is not")))))

(deftest backtrace-keeps-the-frames-under-a-users-own-eval
  ;; Only the evaluator frames next to the boundary are repl-eval's; a user's
  ;; function that calls EVAL itself is the user's code, and so is what it ran.
  (testing "a function that calls EVAL, and the function EVAL ran, are both shown"
    (let ((text (multiple-value-bind (printed raw stdout stderr context)
                    (repl-eval "(defun %eval-probe-inner (x)
  (declare (optimize (debug 3)))
  (error \"inner eval boom ~A\" x))
(defun %eval-probe-runner (form)
  (declare (optimize (debug 3)))
  (list (eval form)))
(%eval-probe-runner '(%eval-probe-inner 3))"
                               :package "CL-USER")
                  (gethash "text"
                           (aref (gethash "content"
                                          (cl-mcp/src/tools/response-builders:build-eval-response
                                           printed raw stdout stderr context))
                                 0)))))
      #+sbcl
      (progn
        (ok (search "%EVAL-PROBE-INNER" text)
            (format nil "the function EVAL ran is shown: ~A" text))
        (ok (search "%EVAL-PROBE-RUNNER" text) "and so is the user's function that called EVAL")
        (ok (null (search "%EVAL-FORMS" text)) "repl-eval's own evaluator is not")))))

(deftest backtrace-skip-internal-false-still-lists-your-frames
  ;; locals_preview_skip_internal only says which frames are counted for a
  ;; preview.  It once also turned the filtering off, and the text then listed
  ;; the capture's own five frames and stopped before the user's.
  (testing "locals_preview_skip_internal=nil changes preview counting, not which frames are shown"
    (multiple-value-bind (printed raw stdout stderr error-context)
        (repl-eval "(defun %skip-internal-probe-inner (x)
  (declare (optimize (debug 3)))
  (let ((doubled (* x 2)))
    (error \"inner ~A\" doubled)))
(defun %skip-internal-probe-outer (n)
  (declare (optimize (debug 3)))
  (%skip-internal-probe-inner (1+ n)))
(%skip-internal-probe-outer 1)"
                   :package "CL-USER"
                   :locals-preview-frames 5
                   :locals-preview-skip-internal nil)
      (let* ((resp (cl-mcp/src/tools/response-builders:build-eval-response
                    printed raw stdout stderr error-context))
             (text (gethash "text" (aref (gethash "content" resp) 0)))
             (frames (gethash "frames" (gethash "error_context" resp))))
        #+sbcl
        (progn
          (ok (search "%SKIP-INTERNAL-PROBE-INNER" text)
              (format nil "the frame that signalled is in the text: ~A" text))
          (ok (search "DOUBLED = 4" text) "with its locals")
          (ok (search "%SKIP-INTERNAL-PROBE-OUTER" text) "and so is its caller")
          (ok (null (search "%COLLECT-FRAMES" text)) "the capture's frames are not")
          (ok (null (search "HANDLER-CLUSTERS" text)) "nor the signalling plumbing's locals")
          (ok (search "%SKIP-INTERNAL-PROBE-INNER" (gethash "function" (aref frames 0)))
              "error_context.frames starts at the user's frame too"))))))

(deftest backtrace-keeps-the-outermost-of-a-top-level-labels
  ;; A LABELS written in repl-eval is the user's code as much as a defun is: a
  ;; recursion in it fills the first frames, so the outermost is kept and those
  ;; between are counted, as they are for a named function.
  (testing "a top-level recursive LABELS keeps its outermost call and says what it left out"
    (multiple-value-bind (printed raw stdout stderr error-context)
        (repl-eval "(locally (declare (optimize (debug 3)))
  (labels ((walk (n) (if (zerop n) (error \"bottom\") (walk (1- n)))))
    (walk 8)))"
                   :package "CL-USER")
      (let ((text (gethash "text"
                           (aref (gethash "content"
                                          (cl-mcp/src/tools/response-builders:build-eval-response
                                           printed raw stdout stderr error-context))
                                 0))))
        #+sbcl
        (progn
          (ok (search "N = 0" text) (format nil "the innermost call is shown: ~A" text))
          (ok (search "N = 8" text) "and so is the outermost")
          (ok (search "1-7: 7 more calls of the same function" text)
              "with the ones between counted"))))))

(deftest backtrace-keeps-a-top-level-lambda-under-a-standard-function
  ;; A CL function such as MAPCAR counts as a frame of the user's, and once
  ;; there was one the lambda written in repl-eval -- the code that signalled,
  ;; holding the local that explains it -- was dropped as internal.
  (testing "a lambda written in repl-eval is shown with its locals beside MAPCAR"
    (multiple-value-bind (printed raw stdout stderr error-context)
        (repl-eval "(mapcar (lambda (x) (declare (optimize (debug 3))) (error \"m ~A\" x)) '(41))"
                   :package "CL-USER")
      (let ((text (gethash "text"
                           (aref (gethash "content"
                                          (cl-mcp/src/tools/response-builders:build-eval-response
                                           printed raw stdout stderr error-context))
                                 0))))
        #+sbcl
        (progn
          (ok (search "(LAMBDA (X) :IN \"repl-eval\")" text)
              (format nil "the lambda's frame is shown: ~A" text))
          (ok (search "X = 41" text) "with its local")
          (ok (search "MAPCAR" text) "and the standard function stays"))))))
