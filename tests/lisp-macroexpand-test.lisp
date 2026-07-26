;;;; tests/lisp-macroexpand-test.lisp
;;;;
;;;; Tests for the macro-expansion tool: the pure expansion core
;;;; (src/macroexpand-core.lisp) and the parent-side form addressing
;;;; (src/lisp-macroexpand.lisp).  Expansion runs in this image, which is
;;;; the inline (no worker pool) path; the worker path is covered by
;;;; tests/worker-test.lisp.

(defpackage #:cl-mcp/tests/lisp-macroexpand-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:skip)
  (:import-from #:cl-mcp/src/macroexpand-core
                #:macroexpand-source
                #:macroexpand-forms
                #:macroexpand-package-error)
  (:import-from #:cl-mcp/src/lisp-macroexpand
                #:lisp-macroexpand)
  (:import-from #:cl-mcp/src/proxy
                #:*use-worker-pool*)
  (:import-from #:cl-mcp/src/state
                #:make-state
                #:protocol-version)
  ;; *PROJECT-ROOT* must be the real special, not a same-named symbol interned
  ;; here: the file-mode tests LET-bind it, and a lexical binding of a fresh
  ;; symbol would leave the guardrail in %NORMALIZE-PATHS looking at whatever
  ;; the test process happened to have.
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*))

(in-package #:cl-mcp/tests/lisp-macroexpand-test)

;;; ---------------------------------------------------------------------------
;;; Fixture macros.  They live in this package so the tests can name it as the
;;; expansion package without touching any production source.
;;; ---------------------------------------------------------------------------

(defmacro double-it (x)
  "Test macro: expands to (* 2 X)."
  `(* 2 ,x))

(defmacro double-it-twice (x)
  "Test macro: expands into DOUBLE-IT, which expands again."
  `(double-it (double-it ,x)))

(defmacro shared-literal-macro ()
  "Test macro whose expansion shares one CONS in two places.
Under *PRINT-CIRCLE* the sharing would surface as #1= / #1# markers.  The
shared object must be a cons, not a string: %CIRCULAR-P only traverses
conses, so a shared string would never reach its memoization path and the
test would pass without exercising anything."
  (let ((shared (list 'quote (list 1 2))))
    (list 'list shared shared)))

(defmacro big-list-macro ()
  "Test macro whose expansion is deliberately long, for truncation tests."
  `(list ,@(loop for i from 0 below 200 collect i)))

(defmacro cyclic-expansion-macro ()
  "Test macro whose expansion is a self-referential list.
Exercises the circularity check that decides whether to switch
*PRINT-CIRCLE* on.  Without it SBCL's pretty printer exhausts the control
stack on this input and takes the whole process down."
  (let ((cell (list 'quote nil)))
    (setf (second cell) cell)
    cell))

(defmacro define-thing (name value)
  "Test macro that expands into a DEFPARAMETER, for top-level addressing tests."
  `(defparameter ,name ,value))

(defmacro exploding-macro ()
  "Test macro whose expander signals, to check per-entry error reporting."
  (error "expander failed on purpose"))

(defmacro circular-expansion-under-list ()
  "Test macro whose expansion is circular under a LIST head, not a QUOTE.
MACROEXPAND-ALL refuses to descend into QUOTE, so a cycle hidden there is
survivable by accident; this one is not, and exercises the
STORAGE-CONDITION clause rather than the input guard."
  (let ((cell (list 'list 1)))
    (setf (cddr cell) cell)
    cell))

(defmacro self-reproducing-macro ()
  "Test macro that expands into itself, to exercise the expansion cap."
  '(self-reproducing-macro))

(defparameter *fixture-package* "CL-MCP/TESTS/LISP-MACROEXPAND-TEST"
  "Name of this test package, used as the expansion package.")

;;; ---------------------------------------------------------------------------
;;; macroexpand-core
;;; ---------------------------------------------------------------------------

(deftest macroexpand-source-expands-one-step
  (testing "level once expands the head macro exactly one step"
    (multiple-value-bind (printed expanded-p steps truncated-p)
        (macroexpand-source "(double-it 21)" :package *fixture-package*)
      (ok (string= printed "(* 2 21)"))
      (ok expanded-p)
      (ok (= steps 1))
      (ok (null truncated-p)))))

(deftest macroexpand-source-reports-non-macro
  (testing "a form whose head is not a macro reports expanded-p NIL"
    (multiple-value-bind (printed expanded-p steps)
        (macroexpand-source "(+ 1 2)" :package "CL-USER")
      (ok (string= printed "(+ 1 2)"))
      (ok (null expanded-p) "must not silently claim an expansion happened")
      (ok (= steps 0)))))

(deftest macroexpand-source-signals-on-missing-package
  (testing "an absent package produces an actionable error, not a silent no-op"
    (ok (handler-case
            (progn (macroexpand-source "(double-it 1)"
                                       :package "NO-SUCH-PACKAGE-XYZZY")
                   nil)
          (macroexpand-package-error (e)
            (and (search "load-system" (princ-to-string e)) t)))
        "the error message should tell the caller to load the system")))

(deftest macroexpand-source-full-repeats-until-fixpoint
  (testing "level full keeps expanding while the head is a macro"
    (multiple-value-bind (printed expanded-p steps)
        (macroexpand-source "(double-it-twice 3)"
                            :package *fixture-package* :level "full")
      (ok expanded-p)
      (ok (= steps 2) "double-it-twice -> double-it -> (* 2 ...)")
      (ok (string= printed "(* 2 (double-it 3))")))))

(deftest macroexpand-source-all-walks-nested-forms
  (testing "level all expands nested macro calls too"
    (multiple-value-bind (printed expanded-p)
        (macroexpand-source "(double-it-twice 3)"
                            :package *fixture-package* :level "all")
      (ok expanded-p)
      (ok (null (search "double-it" printed))
          "no macro call should remain after a full code walk"))))

(deftest macroexpand-source-prints-downcased-without-circle-markers
  (testing "output is lower case and free of #N= sharing markers"
    (let ((printed (macroexpand-source "(shared-literal-macro)"
                                       :package *fixture-package*)))
      (ok (null (search "#1=" printed)) "no *print-circle* sharing markers")
      (ok (null (find-if #'upper-case-p printed))
          "symbols print in lower case"))))

(deftest macroexpand-source-does-not-crash-on-cyclic-expansion
  (testing "a self-referential expansion prints with circle markers instead of crashing"
    (multiple-value-bind (printed expanded-p)
        (macroexpand-source "(cyclic-expansion-macro)"
                            :package *fixture-package* :print-level 5)
      (ok expanded-p)
      (ok (search "#1=" printed)
          "circle notation is switched on for a genuinely circular form")
      (ok (< (length printed) 200) "output must be bounded"))))

(deftest macroexpand-source-truncates-long-output
  (testing "output longer than max-output-length is cut and flagged"
    (multiple-value-bind (printed expanded-p steps truncated-p)
        (macroexpand-source "(big-list-macro)"
                            :package *fixture-package* :max-output-length 50)
      (declare (ignore expanded-p steps))
      (ok truncated-p)
      (ok (search "...(truncated)" printed)))))

(deftest macroexpand-source-rejects-unknown-level
  (testing "an unknown level is rejected instead of silently defaulting"
    (ok (handler-case
            (progn (macroexpand-source "(double-it 1)"
                                       :package *fixture-package*
                                       :level "everything")
                   nil)
          (error (e) (and (search "once" (princ-to-string e)) t)))
        "the error should list the accepted level values")))

(deftest macroexpand-forms-keeps-going-after-one-entry-fails
  (testing "a bad entry is reported in place without aborting the batch"
    (let ((results (macroexpand-forms (list (cons "good" "(double-it 1)")
                                            (cons "bad" "(unclosed")
                                            (cons "also-good" "(double-it 2)"))
                                      :package *fixture-package*)))
      (ok (= 3 (length results)) "every entry yields a result")
      (ok (null (getf (first results) :error)))
      (ok (getf (second results) :error) "the unreadable entry carries an error")
      (ok (string= "(* 2 2)" (getf (third results) :printed))
          "the entry after the failure is still expanded"))))

(deftest macroexpand-forms-reports-expander-errors
  (testing "an error signaled by the macro expander is reported, not swallowed"
    (let ((results (macroexpand-forms (list (cons "boom" "(exploding-macro)"))
                                      :package *fixture-package*)))
      (ok (getf (first results) :error)
          "the failure is recorded on the entry")
      (ok (search "on purpose" (getf (first results) :error))
          "the expander's own message reaches the caller"))))

(deftest circular-check-survives-a-long-flat-list
  (testing "a long quoted literal does not exhaust the control stack"
    (let ((source (with-output-to-string (out)
                    (write-string "(quote (" out)
                    (dotimes (i 60000)
                      (format out "~D " i))
                    (write-string "))" out))))
      (multiple-value-bind (printed expanded-p steps truncated-p)
          (macroexpand-source source :package "CL-USER"
                                     :max-output-length 200)
        (declare (ignore expanded-p steps))
        (ok (stringp printed) "a 60000-element literal is ordinary input")
        (ok truncated-p)))))

(deftest macroexpand-full-reports-when-it-hits-the-cap
  (testing "a self-reproducing macro is capped and says so"
    (multiple-value-bind (printed expanded-p steps truncated-p capped-p)
        (macroexpand-source "(self-reproducing-macro)"
                            :package *fixture-package* :level "full")
      (declare (ignore printed truncated-p))
      (ok expanded-p)
      (ok (= steps cl-mcp/src/macroexpand-core:*max-expansion-steps*))
      (ok capped-p
          "the result is still a macro call and must not read as a fixpoint"))))

(deftest macroexpand-all-refuses-circular-source
  (testing "level all rejects circular source text instead of exhausting the stack"
    (let ((results (macroexpand-forms
                    (list (cons "circular" "#1=(list 1 . #1#)"))
                    :package "CL-USER" :level "all")))
      (ok (getf (first results) :error)
          "the failure is reported on the entry, not raised at the caller")
      (ok (search "circular" (getf (first results) :error))))))

(deftest macroexpand-once-accepts-circular-source
  (testing "once and full accept circular source, since they only look at the head"
    (multiple-value-bind (printed expanded-p)
        (macroexpand-source "#1=(list 1 . #1#)" :package "CL-USER")
      (declare (ignore expanded-p))
      (ok (search "#1=" printed)
          "printed with circle notation; nothing hangs or crashes"))))

(deftest macroexpand-all-survives-a-runaway-macro
  (testing "a self-reproducing macro at level all is reported, not fatal"
    (let ((results (macroexpand-forms
                    (list (cons "runaway" "(self-reproducing-macro)")
                          (cons "sane" "(double-it 4)"))
                    :package *fixture-package* :level "all")))
      (ok (getf (first results) :error)
          "stack exhaustion is a STORAGE-CONDITION and must still be caught")
      (ok (string= "(* 2 4)" (getf (second results) :printed))
          "the entry after the runaway is still expanded"))))

(deftest macroexpand-all-survives-a-circular-expansion
  (testing "a cycle built by the expander is reported, not fatal"
    (let ((results (macroexpand-forms
                    (list (cons "cyclic" "(circular-expansion-under-list)")
                          (cons "sane" "(double-it 4)"))
                    :package *fixture-package* :level "all")))
      (ok (getf (first results) :error)
          "the input guard cannot catch this; the storage-condition clause must")
      (ok (string= "(* 2 4)" (getf (second results) :printed))
          "the batch continues after it"))))

(deftest macroexpand-source-rejects-multiple-forms
  (testing "more than one form in SOURCE is an error, not a silent truncation"
    (ok (handler-case
            (progn (macroexpand-source "(double-it 1) (double-it 2)"
                                       :package *fixture-package*)
                   nil)
          (error (e) (and (search "more than one form"
                                  (princ-to-string e))
                          t))))))

(deftest macroexpand-rejects-an-unresolvable-readtable
  (testing "an unresolvable readtable designator produces an actionable error"
    (ok (handler-case
            (progn (macroexpand-source "(double-it 1)"
                                       :package *fixture-package*
                                       :readtable "no-such-readtable-xyzzy")
                   nil)
          (error (e)
            (let ((message (princ-to-string e)))
              ;; Which branch reports this depends on whether named-readtables
              ;; happens to be loaded, and that varies with suite order -- an
              ;; earlier version of this test asserted one branch's wording and
              ;; was green alone but red in the full suite.  What must hold
              ;; either way is that the message names what could not be
              ;; resolved.  The branches themselves are covered directly by
              ;; PARSE-READTABLE-NAME-HANDLES-EVERY-DESIGNATOR-FORM.
              (and (or (search "no-such-readtable-xyzzy" message)
                       (search "named-readtables" message))
                   t))))
        "the message must identify what could not be resolved")))

(deftest parse-readtable-name-handles-every-designator-form
  (testing "the designator shapes resolve without interning anything"
    (flet ((parse (designator)
             (cl-mcp/src/macroexpand-core::%parse-readtable-name designator)))
      (ok (eq :standard (parse "standard")))
      (ok (eq :standard (parse ":standard")))
      (ok (eq :standard (parse "  :standard  "))
          "surrounding whitespace is trimmed")
      (ok (eq 'cl-user::probe-symbol (parse "cl-user::probe-symbol")))
      (ok (handler-case (progn (parse "no-such-pkg:x") nil)
            (error (e) (and (search "no-such-pkg" (princ-to-string e)) t)))
          "an absent package is named in the error"))))

;;; ---------------------------------------------------------------------------
;;; Parent-side addressing.  Files are written under tests/tmp/ (gitignored)
;;; and removed afterwards; tests/fixtures/ is untracked and unused.
;;; ---------------------------------------------------------------------------

(defun project-path (relative)
  "Return an absolute namestring under the cl-mcp project for RELATIVE."
  (uiop:native-namestring
   (uiop:merge-pathnames* relative (asdf:system-source-directory :cl-mcp))))

(defun call-with-temp-source (relative content thunk)
  "Write CONTENT to RELATIVE under the project, call THUNK with the
absolute path, then delete the file."
  (let ((absolute (project-path relative)))
    (ensure-directories-exist absolute)
    (with-open-file (out absolute :direction :output :if-exists :supersede)
      (write-string content out))
    (unwind-protect (funcall thunk absolute)
      (ignore-errors (delete-file absolute)))))

(defun response-text (payload)
  "Return the content[0].text string of a tool response PAYLOAD."
  (gethash "text" (aref (gethash "content" payload) 0)))

(defun call-macroexpand-tool (args-plist &key protocol-version use-worker-pool)
  "Invoke the generated lisp-macroexpand tool handler and return its response.

ARGS-PLIST holds the JSON argument names and values.  These tests go
through the handler rather than through LISP-MACROEXPAND because the
behaviour under test — which failures are argument validation and which
are operational — lives in the DEFINE-TOOL body, not in the function.
The worker pool is disabled by default so the inline branch runs; pass
USE-WORKER-POOL to take the proxy branch instead."
  (let ((state (make-state))
        (args (make-hash-table :test #'equal))
        (*use-worker-pool* use-worker-pool))
    (loop for (key value) on args-plist by #'cddr
          do (setf (gethash key args) value))
    (when protocol-version
      (setf (protocol-version state) protocol-version))
    (cl-mcp/src/lisp-macroexpand::lisp-macroexpand-handler state 1 args)))

(defun %try-load (system)
  "Attempt to load SYSTEM via Quicklisp or ASDF. Returns T on success, NIL on failure."
  (handler-case
      (cond
        ((find-package :ql)
         (funcall (find-symbol "QUICKLOAD" :ql) system :silent t)
         t)
        ((asdf:find-system system nil)
         (asdf:load-system system)
         t)
        (t nil))
    (error () nil)))

(deftest lisp-macroexpand-file-mode-top-level
  (testing "the addressed top-level form is expanded when sub_form is omitted"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (call-with-temp-source
       "tests/tmp/macroexpand-toplevel.lisp"
       "(in-package #:cl-mcp/tests/lisp-macroexpand-test)

(define-thing *thing-a* 1)

(define-thing *thing-b* 2)
"
       (lambda (path)
         (let* ((payload (lisp-macroexpand :path path
                                           :form-type "define-thing"
                                           :form-name "*thing-b*"))
                (text (response-text payload)))
           (ok (= 1 (gethash "count" payload)) "exactly one form addressed")
           (ok (search "defparameter" text) "the macro expanded")
           (ok (search "*thing-b*" text) "the second definition was selected")
           (ok (null (search "*thing-a*" text))
               "the first definition was not selected")))))))

(deftest lisp-macroexpand-file-mode-sub-form
  (testing "sub_form expands a macro call nested inside the addressed form"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (call-with-temp-source
       "tests/tmp/macroexpand-subform.lisp"
       "(in-package #:cl-mcp/tests/lisp-macroexpand-test)

(defun uses-double (n)
  (let ((base n))
    (double-it base)))
"
       (lambda (path)
         (let* ((payload (lisp-macroexpand :path path
                                           :form-type "defun"
                                           :form-name "uses-double"
                                           :sub-form "double-it"))
                (text (response-text payload)))
           (ok (= 1 (gethash "count" payload)))
           (ok (search "(* 2 base)" text)
               "the nested call was expanded, not the enclosing defun")))))))

(deftest lisp-macroexpand-sub-form-multiple-matches
  (testing "every matching sub-form is expanded and labelled with its position"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (call-with-temp-source
       "tests/tmp/macroexpand-multi.lisp"
       "(in-package #:cl-mcp/tests/lisp-macroexpand-test)

(defun uses-double-twice (a b)
  (list (double-it a)
        (double-it b)))
"
       (lambda (path)
         (let* ((payload (lisp-macroexpand :path path
                                           :form-type "defun"
                                           :form-name "uses-double-twice"
                                           :sub-form "double-it"))
                (text (response-text payload)))
           (ok (= 2 (gethash "count" payload)) "both calls were expanded")
           (ok (search "(* 2 a)" text))
           (ok (search "(* 2 b)" text))
           (ok (search "[1/2]" text)
               "labels carry the position out of the total")))))))

(deftest lisp-macroexpand-rejects-sub-form-with-readtable
  (testing "sub_form combined with readtable fails with an actionable message"
    (ok (handler-case
            (progn (lisp-macroexpand :path "irrelevant.lisp"
                                     :form-type "defun"
                                     :form-name "f"
                                     :sub-form "g"
                                     :readtable "some-syntax")
                   nil)
          (error (e) (and (search "sub_form" (princ-to-string e)) t)))
        "the error should name the offending argument")))

(deftest lisp-macroexpand-requires-exactly-one-mode
  (testing "path and code are mutually exclusive, and one of them is required"
    ;; Assert the MESSAGE, not merely that something was signalled: with the
    ;; (and (null path) (null code)) guard deleted this call still errors, but
    ;; with "form_type is required when 'path' is given" -- telling the caller
    ;; about a path they never supplied, while a bare (error () t) stays green.
    (ok (handler-case (progn (lisp-macroexpand) nil)
          (error (e) (and (search "or 'code'" (princ-to-string e)) t)))
        "neither path nor code names both alternatives in the message")
    (ok (handler-case (progn (lisp-macroexpand :path "a.lisp" :code "(f)") nil)
          (error () t))
        "both path and code is rejected")))

(deftest lisp-macroexpand-code-mode
  (testing "code mode expands a caller-supplied form without touching disk"
    (let* ((payload (lisp-macroexpand :code "(double-it 5)"
                                      :package *fixture-package*))
           (text (response-text payload)))
      (ok (search "(* 2 5)" text)))))

(deftest lisp-macroexpand-rejects-an-unknown-level
  (testing "an unknown level is a validation error, not a generic internal error"
    (ok (handler-case
            (progn (lisp-macroexpand :code "(double-it 1)"
                                     :package *fixture-package*
                                     :level "sideways")
                   nil)
          (cl-mcp/src/tools/helpers:arg-validation-error (e)
            (and (search "once" (princ-to-string e)) t)))
        "the message should list the accepted levels")))

(deftest lisp-macroexpand-missing-package-matches-the-worker-shape
  (testing "an absent package returns the same minimal payload the worker returns"
    (let ((payload (lisp-macroexpand :code "(double-it 1)"
                                     :package "NO-SUCH-PACKAGE-XYZZY")))
      (ok (gethash "isError" payload))
      (ok (search "load-system" (response-text payload))
          "the actionable message survives instead of being wrapped")
      (ok (null (gethash "expansions" payload))
          "minimal shape: no expansions key, matching %handle-macroexpand"))))

(deftest lisp-macroexpand-rejects-sub-form-with-code
  (testing "sub_form in code mode is rejected instead of silently ignored"
    (ok (handler-case
            (progn (lisp-macroexpand :code "(list (double-it 1))"
                                     :package *fixture-package*
                                     :sub-form "double-it")
                   nil)
          (cl-mcp/src/tools/helpers:arg-validation-error (e)
            (and (search "sub_form" (princ-to-string e)) t)))
        "the error should name the offending argument")
    (ok (handler-case
            (progn (lisp-macroexpand :path "a.lisp" :code "(f)"
                                     :sub-form "double-it")
                   nil)
          (cl-mcp/src/tools/helpers:arg-validation-error (e)
            (and (search "not both" (princ-to-string e)) t)))
        "the more fundamental path/code conflict is reported first")))

(deftest lisp-macroexpand-finds-a-sub-form-behind-a-comment
  (testing "a comment between the open paren and the operator does not hide the call"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (call-with-temp-source
       "tests/tmp/macroexpand-comment-head.lisp"
       "(in-package #:cl-mcp/tests/lisp-macroexpand-test)

(defun uses-double-after-comment (n)
  ( ;; deliberately awkward formatting
   double-it n))
"
       (lambda (path)
         (let* ((payload (lisp-macroexpand :path path
                                           :form-type "defun"
                                           :form-name "uses-double-after-comment"
                                           :sub-form "double-it"))
                (text (response-text payload)))
           (ok (= 1 (gethash "count" payload))
               "the call is found rather than silently missed")
           (ok (search "(* 2 n)" text))))))))

(deftest lisp-macroexpand-sub-form-skips-quoted-and-function-positions
  (testing "quote and #' subtrees are not searched, and a template match is labelled"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (call-with-temp-source
       "tests/tmp/macroexpand-positions.lisp"
       "(in-package #:cl-mcp/tests/lisp-macroexpand-test)

(defun uses-double-in-many-positions (x)
  (list '(double-it a)
        (quote (double-it b))
        #'double-it
        `(double-it ,x)
        (double-it 3)))
"
       (lambda (path)
         (let* ((payload (lisp-macroexpand
                          :path path
                          :form-type "defun"
                          :form-name "uses-double-in-many-positions"
                          :sub-form "double-it"))
                (text (response-text payload)))
           (ok (= 2 (gethash "count" payload))
               "only the template match and the real call are reported")
           (ok (null (search "(* 2 a)" text))
               "'(double-it a) is data, not a call")
           (ok (null (search "(* 2 b)" text))
               "(quote (double-it b)) is data, not a call")
           (ok (search "(* 2 3)" text)
               "the real call is still found")
           (ok (search "(inside a backquote template)" text)
               "the template match is labelled rather than silently dropped")))))))

(deftest lisp-macroexpand-sub-form-is-positional-blind
  (testing "a binding position is reported like a call: a documented limitation"
    ;; Pins the documented behaviour rather than endorsing it.  Telling a
    ;; binding pair from a call needs a full code walker; until then the
    ;; docstring and the tool description say so, and this test makes the
    ;; behaviour deliberate instead of accidental.
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (call-with-temp-source
       "tests/tmp/macroexpand-binding-position.lisp"
       "(in-package #:cl-mcp/tests/lisp-macroexpand-test)

(defun binds-a-name-like-the-macro (n)
  (let ((double-it n))
    double-it))
"
       (lambda (path)
         (let ((payload (lisp-macroexpand
                         :path path
                         :form-type "defun"
                         :form-name "binds-a-name-like-the-macro"
                         :sub-form "double-it")))
           (ok (= 1 (gethash "count" payload))
               "the let binding pair still matches")))))))

(deftest lisp-macroexpand-not-expanded-names-positional-blindness
  (testing "a NOT EXPANDED sub_form match names the likeliest real cause"
    ;; The stock advice -- load the system -- is misleading when what matched
    ;; was a LET binding pair, which is what positional-blind matching makes
    ;; likely.  Only a sub_form request can hit that, so only a sub_form
    ;; request says it.
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (call-with-temp-source
       "tests/tmp/macroexpand-not-expanded.lisp"
       "(in-package #:cl-mcp/tests/lisp-macroexpand-test)

(defun binds-a-non-macro-name (n)
  (let ((not-a-macro-xyzzy n))
    not-a-macro-xyzzy))
"
       (lambda (path)
         (let ((text (response-text
                      (lisp-macroexpand :path path
                                        :form-type "defun"
                                        :form-name "binds-a-non-macro-name"
                                        :sub-form "not-a-macro-xyzzy"))))
           (ok (search "NOT EXPANDED" text))
           (ok (search "positional-blind" text)
               "the matching rule is named, not just 'load-system'")
           (ok (search "binding position" text)
               "and a binding pair is offered as the likely cause")))))
    ;; The negative case goes through code mode: a whole-form file match cannot
    ;; easily be NOT EXPANDED, since DEFUN and friends are themselves macros.
    (let ((text (response-text
                 (lisp-macroexpand :code "(+ 1 2)" :package *fixture-package*))))
      (ok (search "NOT EXPANDED" text)
          "a non-macro head reports the same status outside sub_form")
      (ok (null (search "positional-blind" text))
          "but only a sub_form match can have landed on a binding position"))))

(deftest lisp-macroexpand-template-slice-error-does-not-leak-the-stream
  (testing "an unreadable template slice reports the diagnosis, not a stream object"
    ;; Retaining backquote matches makes this path reachable: the slice is
    ;; text cut out of a template, so its comma has no enclosing backquote and
    ;; it cannot be read on its own.  That is worth saying; the internal
    ;; #<STRING-INPUT-STREAM ...> the default report appends is not.
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (call-with-temp-source
       "tests/tmp/macroexpand-template-error.lisp"
       "(in-package #:cl-mcp/tests/lisp-macroexpand-test)

(defmacro wraps-double (x)
  `(double-it ,x))
"
       (lambda (path)
         (let* ((payload (lisp-macroexpand :path path
                                           :form-type "defmacro"
                                           :form-name "wraps-double"
                                           :sub-form "double-it"))
                (message (gethash "error" (aref (gethash "expansions" payload) 0)))
                (text (response-text payload)))
           (ok (search "(inside a backquote template)" text)
               "the label still says where the slice came from")
           (ok (search "Comma not inside a backquote" message)
               "the diagnosis survives")
           (ok (null (search "STRING-INPUT-STREAM" message))
               "the internal stream object does not")
           (ok (null (search "STRING-INPUT-STREAM" text))
               "and does not reach the content text either")))))))

(deftest lisp-macroexpand-template-slice-error-explains-itself
  (testing "an unreadable template slice says why it cannot be read on its own"
    ;; The reader reports "Comma not inside a backquote" and the label says the
    ;; match came out of a template; joining those two facts is the tool's job,
    ;; not the reader's.  The explanation belongs on the failing path only: a
    ;; template fragment containing no comma reads and expands perfectly well,
    ;; so putting it in the label would be wrong for those.
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (call-with-temp-source
       "tests/tmp/macroexpand-template-explanation.lisp"
       "(in-package #:cl-mcp/tests/lisp-macroexpand-test)

(defmacro wraps-double-with-comma (x)
  `(double-it ,x))

(defmacro wraps-double-without-comma ()
  `(double-it 7))
"
       (lambda (path)
         (let ((text (response-text
                      (lisp-macroexpand :path path
                                        :form-type "defmacro"
                                        :form-name "wraps-double-with-comma"
                                        :sub-form "double-it"))))
           (ok (search "Comma not inside a backquote" text)
               "the reader's own diagnosis still reaches the caller")
           (ok (search "fragment of a backquote template" text)
               "and the tool spells out what that means here")
           (ok (search "Expand the enclosing macro instead" text)
               "together with the action to take"))
         (let ((text (response-text
                      (lisp-macroexpand :path path
                                        :form-type "defmacro"
                                        :form-name "wraps-double-without-comma"
                                        :sub-form "double-it"))))
           (ok (search "(* 2 7)" text)
               "a comma-free template fragment reads and expands")
           (ok (null (search "fragment of a backquote template" text))
               "so a successful expansion must not carry the explanation")))))))

(deftest lisp-macroexpand-rejects-sub-form-on-a-file-with-in-readtable
  (testing "the file's own in-readtable is named, instead of a false 'not found'"
    ;; named-readtables must really be loaded: without it %TRY-SWITCH-READTABLE
    ;; finds nothing, no reader switch happens, children survive, and the call
    ;; IS found -- so the test would pass without exercising the guard at all.
    (if (not (%try-load :named-readtables))
        (skip "named-readtables not available")
        (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
          (call-with-temp-source
           "tests/tmp/macroexpand-in-readtable.lisp"
           "(in-package #:cl-mcp/tests/lisp-macroexpand-test)
(named-readtables:in-readtable :standard)

(defun uses-double-under-readtable (n)
  (double-it n))
"
           (lambda (path)
             (ok (handler-case
                     (progn (lisp-macroexpand
                             :path path
                             :form-type "defun"
                             :form-name "uses-double-under-readtable"
                             :sub-form "double-it")
                            nil)
                   (cl-mcp/src/tools/helpers:arg-validation-error (e)
                     (and (search "in-readtable" (princ-to-string e)) t)))
                 "the message names the in-readtable declaration as the cause")
             (let ((payload (lisp-macroexpand
                             :path path
                             :form-type "defun"
                             :form-name "uses-double-under-readtable")))
               (ok (= 1 (gethash "count" payload))
                   "addressing the whole form is unaffected"))))))))

(deftest lisp-macroexpand-tool-reports-a-missing-form-without-a-wrapper
  (testing "a wrong form_name reads like lisp-edit-form's, not like an internal error"
    (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
      (call-with-temp-source
       "tests/tmp/macroexpand-missing-form.lisp"
       "(in-package #:cl-mcp/tests/lisp-macroexpand-test)

(define-thing *thing-a* 1)
"
       (lambda (path)
         (let* ((response (call-macroexpand-tool
                           (list "path" path
                                 "form_type" "define-thing"
                                 "form_name" "no-such-thing-xyzzy")))
                (message (gethash "message" (gethash "error" response))))
           (ok (search "not found" message)
               "the actionable message survives")
           (ok (null (search "Internal error during" message))
               "and is not wrapped as an internal error"))
         (let* ((response (call-macroexpand-tool
                           (list "path" path
                                 "form_type" "define-thing"
                                 "form_name" "no-such-thing-xyzzy")
                           :protocol-version "2025-11-25"))
                (payload (gethash "result" response))
                (text (response-text payload)))
           (ok (gethash "isError" payload)
               "newer protocols get a tool-execution error, not a protocol error")
           (ok (search "not found" text))
           (ok (null (search "Internal error during" text)))))))))

(deftest lisp-macroexpand-tool-keeps-validation-errors-distinct
  (testing "the generic error clause must not swallow ARG-VALIDATION-ERROR"
    ;; ARG-VALIDATION-ERROR is a subtype of ERROR, so a lone (error ...) clause
    ;; in the tool body would catch it first and report a validation failure as
    ;; an operational one.  The two paths use different JSON-RPC codes, which
    ;; is what distinguishes them here: -32602 is validation, -32603 is the
    ;; generic clause.
    (let* ((response (call-macroexpand-tool
                      (list "code" "(double-it 1)"
                            "package" *fixture-package*
                            "level" "sideways")))
           (error-object (gethash "error" response)))
      (ok error-object "a validation failure is reported as a JSON-RPC error")
      (ok (= -32602 (gethash "code" error-object))
          "-32602, not the generic clause's -32603")
      (ok (search "once" (gethash "message" error-object))
          "the accepted levels are listed"))
    (let* ((response (call-macroexpand-tool
                      (list "code" "(double-it 1)"
                            "package" *fixture-package*
                            "level" "sideways")
                      :protocol-version "2025-11-25"))
           (payload (gethash "result" response)))
      (ok (gethash "isError" payload))
      (ok (search "once" (response-text payload))))))

(deftest lisp-macroexpand-validation-messages-render-cleanly
  (testing "no validation message reaches the caller with a literal tilde"
    ;; ARG-VALIDATION-ERROR's report prints :MESSAGE verbatim through ~A, so a
    ;; "~" line continuation written in a plain string literal is never
    ;; processed and shows up in agent-facing text.  Only a FORMAT NIL control
    ;; string gets the continuation it looks like it has.  None of these calls
    ;; touch the disk: every one is rejected before path resolution.
    (dolist (arguments (list (list :path "a.lisp" :code "(f)")
                             (list)
                             (list :code "(f)" :sub-form "double-it")
                             (list :path "a.lisp")
                             (list :path "a.lisp" :form-type "defun")
                             (list :path "a.lisp" :form-type "defun"
                                   :form-name "f" :sub-form "g"
                                   :readtable "some-syntax")))
      (let ((message (handler-case (progn (apply #'lisp-macroexpand arguments)
                                          nil)
                       (cl-mcp/src/tools/helpers:arg-validation-error (e)
                         (princ-to-string e)))))
        (ok (stringp message)
            (format nil "rejected: ~S" arguments))
        (ok (null (find #\~ message))
            (format nil "no literal tilde in: ~A" message))))))

(deftest lisp-macroexpand-proxy-params-match-what-the-worker-reads
  (testing "the parent sends exactly the keys %handle-macroexpand reads"
    ;; Nothing else covers this seam: tests/worker-test.lisp builds its params
    ;; by hand and only ever sends forms/package/level, so a typo in any of the
    ;; other key names would be caught by no test at all -- the worker would
    ;; silently fall back to NIL for that argument.
    (let ((captured nil)
          (original (fdefinition 'cl-mcp/src/proxy:proxy-to-worker)))
      (unwind-protect
           (progn
             (setf (fdefinition 'cl-mcp/src/proxy:proxy-to-worker)
                   (lambda (id method params)
                     (declare (ignore id method))
                     (setf captured params)
                     (make-hash-table :test #'equal)))
             (call-macroexpand-tool
              (list "code" "(double-it 1)"
                    "package" *fixture-package*
                    "level" "full"
                    "readtable" "standard"
                    "print_level" 3
                    "print_length" 4
                    "max_output_length" 5)
              :use-worker-pool t))
        (setf (fdefinition 'cl-mcp/src/proxy:proxy-to-worker) original))
      (ok (hash-table-p captured)
          "the stub was reached, so the proxy branch really ran")
      (ok (equal '("forms" "level" "max_output_length" "note" "package"
                   "print_length" "print_level" "readtable" "sub_form")
                 (sort (loop for key being the hash-keys of captured
                             collect key)
                       #'string<))
          "key set matches the gethash calls in %handle-macroexpand"))))
