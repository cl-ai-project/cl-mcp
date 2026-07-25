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
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/macroexpand-core
                #:macroexpand-source
                #:macroexpand-forms
                #:macroexpand-package-error))

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
