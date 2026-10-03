;;;; tests/frame-inspector-test.lisp

(defpackage #:cl-mcp/tests/frame-inspector-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/frame-inspector
                #:capture-error-context
                #:capture-debugger-error-context))

(in-package #:cl-mcp/tests/frame-inspector-test)

(define-condition diagnostic-capture-report-error (condition) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition stream))
     (error "diagnostic report failed"))))

(deftest capture-debugger-error-context-keeps-live-restarts
  (let (context)
    (handler-bind
        ((error
           (lambda (condition)
             (setf context
                   (capture-debugger-error-context
                    condition
                    (lambda (secondary)
                      (declare (ignore secondary))
                      (error "unexpected diagnostic callback"))
                    :max-frames 0))
             (invoke-restart 'retry-snapshot))))
      (restart-case
          (error "snapshot source")
        (retry-snapshot () :report "Retry the snapshot" nil)))
    (ok (getf context :error))
    (ok (search "SIMPLE-ERROR" (getf context :condition-type)))
    (ok (search "snapshot source" (getf context :message)))
    (ok (find "RETRY-SNAPSHOT" (getf context :restarts)
              :key (lambda (restart) (getf restart :name))
              :test #'search))))

(deftest capture-debugger-error-context-transfers-on-secondary-condition
  (let ((tag (list :diagnostic-transfer)))
    (handler-case
        (error 'diagnostic-capture-report-error)
      (condition (original)
        (let ((result
                (catch tag
                  (capture-debugger-error-context
                   original
                   (lambda (secondary)
                     (throw tag (list :secondary (type-of secondary))))))))
          (ok (equal '(:secondary simple-error) result)
              "the report's ERROR reaches the caller's handler-bind exit"))))))

#+sbcl
(defclass diagnostic-preview-printer () ())

#+sbcl
(defmethod print-object ((object diagnostic-preview-printer) stream)
  (declare (ignore object stream))
  (error "secondary preview printer failure"))

#+sbcl
(defclass diagnostic-preview-inspector ()
  ((payload :initform 73)))

#+sbcl
(defmethod sb-mop:slot-value-using-class :before
    ((class standard-class) (object diagnostic-preview-inspector) slot)
  (declare (ignore class object slot))
  (error "secondary preview inspector failure"))

#+sbcl
(defun capture-preview-local (object callback &optional ordinary-p)
  (declare (optimize (debug 3) (speed 0)))
  ;; The hash table's own printer does not print its contents. A failing
  ;; nested printer therefore identifies preview collection, not :VALUE.
  ;; Rove's whole-package runner otherwise prints imported capture functions
  ;; unqualified, which the existing frame filter cannot identify as internal.
  (let ((*package* (find-package '#:cl-user)))
    (let ((context
            (if ordinary-p
                (capture-error-context
                 (make-condition 'simple-error :format-control "preview source")
                 :max-frames 1 :filter-internal t :locals-preview-frames 1
                 :preview-max-depth 2)
                (capture-debugger-error-context
                 (make-condition 'simple-error :format-control "preview source") callback
                 :max-frames 1 :filter-internal t :locals-preview-frames 1
                 :preview-max-depth 2))))
      (values context object))))

#+sbcl
(deftest debugger-positive-preview-transfers-secondary-conditions
  (dolist (fixture '(diagnostic-preview-printer diagnostic-preview-inspector))
    (let ((object (make-hash-table))
          (tag (list :preview-transfer)))
      (setf (gethash :nested object) (make-instance fixture))
      (let ((result
              (catch tag
                (capture-preview-local
                 object
                 (lambda (secondary)
                   (throw tag (princ-to-string secondary)))))))
        (ok (equal (if (eq fixture 'diagnostic-preview-printer)
                       "secondary preview printer failure"
                       "secondary preview inspector failure")
                   result)
            "a positive locals preview must expose the original secondary condition")))))

#+sbcl
(deftest ordinary-positive-preview-keeps-recovery-fallbacks
  (dolist (fixture '(diagnostic-preview-printer diagnostic-preview-inspector))
    (let ((object (make-hash-table)))
      (setf (gethash :nested object) (make-instance fixture))
      (let* ((context (capture-preview-local object nil t))
             (local (find "OBJECT" (getf (first (getf context :frames)) :locals)
                          :key (lambda (local) (getf local :name)) :test #'equal)))
        (ok (equal "preview source" (getf context :message)))
        (ok (hash-table-p (getf local :preview))
            "ordinary capture still returns the recoverable preview")))))

#+sbcl
(defstruct debugger-preview-record payload)

#+sbcl
(deftest debugger-positive-preview-preserves-bounds-and-references
  (let ((object (make-hash-table)))
    (setf (gethash :items object) (vector '(nested) 20 #\A 40 50 60 70)
          (gethash :record object) (make-debugger-preview-record :payload 73)
          (gethash :self object) object)
    (let* ((context (capture-preview-local object #'error))
           (local (find "OBJECT" (getf (first (getf context :frames)) :locals)
                        :key (lambda (local) (getf local :name)) :test #'equal))
           (preview (getf local :preview)))
      (ok (hash-table-p preview))
      (when (hash-table-p preview)
        (flet ((entry (key)
                 (gethash "value"
                          (find key (gethash "entries" preview)
                                :key (lambda (entry)
                                       (gethash "value" (gethash "key" entry)))
                                :test #'equal))))
          (let* ((items (entry "ITEMS"))
                 (elements (gethash "elements" items))
                 (record (entry "RECORD"))
                 (self (entry "SELF")))
            (ok (equal "hash-table" (gethash "kind" preview)))
            (ok (eql (getf local :object-id) (gethash "id" preview)))
            (ok (= 5 (length elements)) "the element limit is retained")
            (ok (gethash "truncated" (gethash "meta" items)))
            (ok (equal "object-ref" (gethash "kind" (first elements)))
                "nested objects at the depth limit keep an inspection handle")
            (ok (stringp (gethash "id" (first elements))))
            (ok (equal "CHARACTER" (gethash "type" (third elements))))
            (ok (equal "A" (gethash "value" (third elements))))
            (ok (equal "structure" (gethash "kind" record)))
            (ok (eql 73 (gethash "value" (gethash "value"
                                                 (first (gethash "slots" record))))))
            (ok (equal "circular-ref" (gethash "kind" self)))
            (ok (equal (gethash "id" preview) (gethash "ref_id" self)))))))))

(deftest capture-error-context-basic
  (testing "captures condition type and message"
    (let ((ctx (handler-case
                   (error "Test error message")
                 (error (e)
                   (capture-error-context e)))))
      (ok (getf ctx :error))
      (ok (stringp (getf ctx :condition-type)))
      (ok (search "SIMPLE-ERROR" (getf ctx :condition-type)))
      (ok (stringp (getf ctx :message)))
      (ok (search "Test error message" (getf ctx :message))))))

(deftest capture-error-context-restarts
  (testing "captures available restarts"
    (let ((ctx (handler-case
                   (restart-case
                       (error "Error with restarts")
                     (retry () :report "Retry the operation" nil)
                     (skip () :report "Skip this item" nil))
                 (error (e)
                   (capture-error-context e)))))
      (ok (listp (getf ctx :restarts)))
      (ok (> (length (getf ctx :restarts)) 0))
      ;; Each restart should have :name and :description
      (let ((first-restart (first (getf ctx :restarts))))
        (ok (stringp (getf first-restart :name)))
        (ok (stringp (getf first-restart :description)))))))

(defun %probe-restart-names (package)
  "Return the captured restart names for a probe signalled with *PACKAGE* bound.
HANDLER-BIND rather than HANDLER-CASE: the restarts have to still be
established when the context is captured, and HANDLER-CASE unwinds first."
  (let (context)
    (ignore-errors
     (let ((*package* package))
       (handler-bind ((error (lambda (condition)
                               (setf context (capture-error-context condition)))))
         (restart-case (error "restart naming probe")
           (:keyword-named () :report "keyword" nil)
           (symbol-named () :report "symbol" nil)))))
    (mapcar (lambda (restart) (getf restart :name))
            (getf context :restarts))))

(deftest capture-error-context-restart-names-are-readable
  ;; INVOKE-RESTART compares restart names with EQ, and cl-mcp's own prompts
  ;; tell library authors to name restarts with keywords for exactly that
  ;; reason.  Printing the name with ~A rendered :KEYWORD-NAMED and
  ;; KEYWORD-NAMED as the same text, so the field an agent reads to decide
  ;; which spelling to type could not tell them apart.
  (let ((names (%probe-restart-names
                (find-package '#:cl-mcp/tests/frame-inspector-test))))
    (testing "a keyword-named restart keeps its colon"
      (ok (member ":KEYWORD-NAMED" names :test #'string=)))
    (testing "a symbol named in the current package carries no qualifier"
      (ok (member "SYMBOL-NAMED" names :test #'string=)))
    (testing "the two spellings are no longer the same string"
      (ok (not (member ":SYMBOL-NAMED" names :test #'string=)))
      (ok (not (member "KEYWORD-NAMED" names :test #'string=))))))

(deftest capture-error-context-restart-names-qualify-foreign-symbols
  ;; The names are printed relative to *PACKAGE*, which during repl-eval is the
  ;; package the caller asked for -- so what comes back is what they would have
  ;; to type there. From a package that does not home the symbol, that means a
  ;; qualifier.
  (let ((names (%probe-restart-names (find-package '#:cl-user))))
    (testing "a symbol from another package comes back qualified"
      (ok (find-if (lambda (name)
                     (and (search "SYMBOL-NAMED" name)
                          (search "::" name)))
                   names)))
    (testing "a keyword needs no qualifier and gets none"
      (ok (member ":KEYWORD-NAMED" names :test #'string=)))))

(deftest capture-error-context-frames
  (testing "captures stack frames (SBCL specific)"
    (let ((ctx (handler-case
                   (error "Frame test")
                 (error (e)
                   (capture-error-context e)))))
      ;; Frames should be a list (may be empty on non-SBCL)
      (ok (listp (getf ctx :frames)))
      #+sbcl
      (progn
        ;; On SBCL, we should have at least some frames
        (ok (> (length (getf ctx :frames)) 0))
        ;; Each frame should have expected keys
        (let ((first-frame (first (getf ctx :frames))))
          (ok (integerp (getf first-frame :index)))
          (ok (stringp (getf first-frame :function))))))))

(deftest capture-error-context-max-frames
  (testing "respects max-frames limit"
    (labels ((deep-call (n)
               (if (zerop n)
                   (error "Deep error")
                   (deep-call (1- n)))))
      (let ((ctx (handler-case
                     (deep-call 50)
                   (error (e)
                     (capture-error-context e :max-frames 5)))))
        #+sbcl
        (ok (<= (length (getf ctx :frames)) 5))))))

(defparameter *type-error-bad-arg* "not a number"
  "Held in a global so SBCL cannot derive its type at the call site below.")

(deftest capture-error-context-type-error
 (testing "captures type error details"
  (let ((ctx
         (handler-case (+ *type-error-bad-arg* 1)
                       (error (e) (capture-error-context e)))))
    (ok (getf ctx :error))
    (ok (stringp (getf ctx :condition-type)))
    (ok (stringp (getf ctx :message))))))

(deftest capture-error-context-unbound-variable
  (testing "captures unbound variable error"
    (let ((ctx (handler-case
                   (eval 'some-undefined-variable-12345)
                 (error (e)
                   (capture-error-context e)))))
      (ok (getf ctx :error))
      (ok (search "UNBOUND" (getf ctx :condition-type))))))

(deftest capture-error-context-print-limits
  (testing "respects print-level and print-length for locals"
    (let ((ctx (handler-case
                   (let ((deep-list '((((a b c d e f g h i j k l m))))))
                     (declare (ignore deep-list))
                     (error "Error with deep local"))
                 (error (e)
                   (capture-error-context e :print-level 2 :print-length 3)))))
      ;; Should complete without error
      (ok (getf ctx :error))
      (ok (stringp (getf ctx :message))))))

(deftest internal-frame-p-prefix-boundary
  (testing "%internal-frame-p matches package prefixes with boundaries only"
    (ok (cl-mcp/src/frame-inspector::%internal-frame-p "CL-MCP/SRC/REPL:REPL-EVAL"))
    (ok (not (cl-mcp/src/frame-inspector::%internal-frame-p
              "CL-MCP/TESTS/REPL-TEST::HELPER")))
    (ok (not (cl-mcp/src/frame-inspector::%internal-frame-p
              "SB-INTROSPECTIVE::FOO")))
    (ok (cl-mcp/src/frame-inspector::%internal-frame-p "SB-INT:FOO"))))

(deftest internal-frame-p-anonymous-and-standard
  (testing "%internal-frame-p marks anonymous and standard signaling frames as internal"
    (ok (cl-mcp/src/frame-inspector::%internal-frame-p "(LAMBDA () ...)"))
    (ok (cl-mcp/src/frame-inspector::%internal-frame-p "(FLET X)"))
    (ok (cl-mcp/src/frame-inspector::%internal-frame-p "ERROR"))
    (ok (cl-mcp/src/frame-inspector::%internal-frame-p "SIGNAL"))
    (ok (not (cl-mcp/src/frame-inspector::%internal-frame-p "MY-APP::PROCESS")))))

(deftest internal-frame-p-clos-method-frames
  (testing
   "%internal-frame-p exempts user CLOS method wrappers but keeps internal ones"
   (ok
    (not (cl-mcp/src/frame-inspector::%internal-frame-p
          "(SB-PCL::FAST-METHOD MY-APP::GREET (STRING))")))
   (ok
    (cl-mcp/src/frame-inspector::%internal-frame-p
     "(SB-PCL::FAST-METHOD SB-INT::FAKE (T))"))
   (ok
    (not (cl-mcp/src/frame-inspector::%internal-frame-p
          "(SB-PCL::SLOW-METHOD MY-APP::FOO (T))")))))

(deftest internal-frame-p-setf-forms
  (testing
   "%internal-frame-p exempts (SETF user:name) but keeps internal SETFs"
   (ok
    (not (cl-mcp/src/frame-inspector::%internal-frame-p
          "(SETF MY-APP::CUSTOM-SETTER)")))
   (ok
    (cl-mcp/src/frame-inspector::%internal-frame-p
     "(SETF SB-INT::STORE)"))))

(defun frame-probe-shadowed-local (pattern)
  "Signal with two live variables named PATTERN in one frame -- the argument and
an inner binding of the same name -- as a let-converted LABELS helper leaves
them in its caller's frame."
  (declare (optimize (debug 3)))
  (let ((pattern (rest pattern)))
    (error "frame probe shadowed ~S" pattern)))

(defun %probe-frame-locals (capture)
  "Return the locals of FRAME-PROBE-SHADOWED-LOCAL's frame, as (NAME . VALUE),
captured by CAPTURE, called with the condition inside the signalling handler."
  (let ((context nil))
    (block caught
      (handler-bind ((error (lambda (e)
                              (setf context (funcall capture e))
                              (return-from caught))))
        (frame-probe-shadowed-local '(:a :b))))
    (let ((frame (find-if (lambda (frame)
                            (search "FRAME-PROBE-SHADOWED-LOCAL" (getf frame :function)))
                          (getf context :frames))))
      (mapcar (lambda (local) (cons (getf local :name) (getf local :value)))
              (getf frame :locals)))))

(deftest same-named-locals-are-told-apart
  ;; Both used to be listed as PATTERN.  SBCL's debugger writes the second as
  ;; PATTERN#1 (its debug-var id), and so do we now.
  (dolist (capture (list (lambda (e) (capture-error-context e :max-frames 30))
                         (lambda (e)
                           (capture-debugger-error-context
                            e (lambda (secondary) (error secondary)) :max-frames 30))))
    (testing "in both the ordinary and the debugger-boundary capture"
      (let ((locals (%probe-frame-locals capture)))
        (ok (equal '("PATTERN" "PATTERN#1")
                   (sort (mapcar #'car (remove-if-not
                                        (lambda (local) (search "PATTERN" (car local)))
                                        locals))
                         #'string<))
            "the two variables have two names")
        (ok (equal '("(:A :B)" "(:B)")
                   (sort (mapcar #'cdr (remove-if-not
                                        (lambda (local) (search "PATTERN" (car local)))
                                        locals))
                         #'string<))
            "and each name carries its own value")))))

(deftest internal-frame-p-local-functions-follow-their-outer-function
  (testing "a local or anonymous function inside a user's function is the user's frame"
    ;; SBCL names it (FLET NAME :IN OUTER), (LABELS NAME :IN OUTER) or
    ;; (LAMBDA LAMBDA-LIST :IN OUTER).  Every such name used to count as
    ;; internal, so the frame an error was signalled in was dropped from the
    ;; backtrace whenever that was a LABELS helper or a lambda.
    (ok (not (cl-mcp/src/frame-inspector::%internal-frame-p
              "(LABELS MY-APP::VISIT :IN MY-APP::TOPOLOGICAL-SORT)")))
    (ok (not (cl-mcp/src/frame-inspector::%internal-frame-p
              "(FLET MY-APP::HELPER :IN MY-APP::RUN)")))
    (ok (not (cl-mcp/src/frame-inspector::%internal-frame-p
              "(LAMBDA (MY-APP::X) :IN MY-APP::RUN)")))
    (ok (not (cl-mcp/src/frame-inspector::%internal-frame-p "(LABELS WALK :IN OUTER)"))
        "names printed relative to the user's own package")
    (ok (not (cl-mcp/src/frame-inspector::%internal-frame-p
              "(FLET MY-APP::INNER :IN (SB-PCL::FAST-METHOD MY-APP::GREET (STRING)))"))
        "an OUTER that is itself a user's method")
    (ok (not (cl-mcp/src/frame-inspector::%internal-frame-p
              "(LAMBDA (&KEY (MY-APP::MODE :IN)) :IN MY-APP::RUN)"))
        "an :IN inside the lambda list is not the one that names OUTER"))
  (testing "a local function inside infrastructure stays internal"
    (ok (cl-mcp/src/frame-inspector::%internal-frame-p
         "(FLET SB-C::WITH-IT :IN SB-C::%WITH-COMPILATION-UNIT)"))
    (ok (cl-mcp/src/frame-inspector::%internal-frame-p
         "(LAMBDA () :IN CL-MCP/SRC/REPL-CORE::%EVAL-FORMS)"))
    (ok (cl-mcp/src/frame-inspector::%internal-frame-p "(LAMBDA (C) :IN ERROR)"))
    (ok (cl-mcp/src/frame-inspector::%internal-frame-p
         "(LAMBDA (&OPTIONAL (X :IN)) :IN SB-IMPL::FOO)"))
    (ok (cl-mcp/src/frame-inspector::%internal-frame-p
         "(FLET MY-APP::INNER :IN (SB-PCL::FAST-METHOD SB-INT::FAKE (T)))")))
  (testing "a top-level form's lambda, named by a source string, stays internal"
    (ok (cl-mcp/src/frame-inspector::%internal-frame-p "(LAMBDA () :IN \"repl-eval\")"))
    (ok (cl-mcp/src/frame-inspector::%internal-frame-p
         "(LAMBDA () :IN \"/tmp/a (b) :IN c.lisp\")"))))

(deftest internal-frame-p-reads-common-lisp-qualified-operators
  ;; Frame names are printed relative to the caller's *PACKAGE*.  One that does
  ;; not use COMMON-LISP qualifies the operators too, and each shape used to
  ;; be misread: a user's local function or SETF function as internal, and a
  ;; standard signalling function as the user's.
  (testing "a qualified FLET, LABELS or LAMBDA follows its OUTER"
    (ok (not (cl-mcp/src/frame-inspector::%internal-frame-p
              "(COMMON-LISP:LABELS MY-APP::VISIT :IN MY-APP::RUN)")))
    (ok (not (cl-mcp/src/frame-inspector::%internal-frame-p
              "(CL:FLET MY-APP::HELPER :IN MY-APP::RUN)")))
    (ok (not (cl-mcp/src/frame-inspector::%internal-frame-p
              "(COMMON-LISP:LAMBDA (MY-APP::X) :IN MY-APP::RUN)")))
    (ok (cl-mcp/src/frame-inspector::%internal-frame-p
         "(COMMON-LISP:FLET SB-C::WITH-IT :IN SB-C::%WITH-COMPILATION-UNIT)"))
    (ok (cl-mcp/src/frame-inspector::%internal-frame-p
         "(COMMON-LISP:LAMBDA () :IN \"repl-eval\")")))
  (testing "a qualified SETF follows its target"
    (ok (not (cl-mcp/src/frame-inspector::%internal-frame-p
              "(COMMON-LISP:SETF MY-APP::CUSTOM-SETTER)")))
    (ok (cl-mcp/src/frame-inspector::%internal-frame-p "(COMMON-LISP:SETF SB-INT::STORE)")))
  (testing "a qualified standard signalling function is internal, a user's namesake is not"
    (ok (cl-mcp/src/frame-inspector::%internal-frame-p "COMMON-LISP:ERROR"))
    (ok (cl-mcp/src/frame-inspector::%internal-frame-p "CL:SIGNAL"))
    (ok (not (cl-mcp/src/frame-inspector::%internal-frame-p "MY-APP::ERROR"))))
  (testing "an operator of the same name from another package is not taken for one"
    (ok (cl-mcp/src/frame-inspector::%internal-frame-p
         "(MY-APP::LABELS MY-APP::VISIT :IN MY-APP::RUN)"))
    (ok (cl-mcp/src/frame-inspector::%internal-frame-p "(MY-APP::SETF MY-APP::X)"))))

(defun frame-probe-walk (item)
  "Signal from inside a LABELS function, for the local-function frame test.
VISIT recurses outside tail position and is called twice, so SBCL keeps it a
function of its own with frames of its own."
  (labels ((visit (depth)
             (if (zerop depth)
                 (error "frame probe local ~S" item)
                 (cons item (visit (1- depth))))))
    (list (visit 1) (visit 2))))

(defun %filtered-probe-frame-names (package)
  "Return the frame names capture-error-context keeps with :filter-internal for
FRAME-PROBE-WALK's error, captured with *PACKAGE* bound to PACKAGE: frame names
are printed relative to it."
  (let ((context nil))
    (block caught
      (handler-bind ((error (lambda (e)
                              (setf context (capture-error-context
                                             e :max-frames 30 :filter-internal t))
                              (return-from caught))))
        (let ((*package* package))
          (frame-probe-walk :x))))
    (mapcar (lambda (frame) (getf frame :function))
            (getf context :frames))))

(deftest filtered-backtrace-keeps-the-labels-frame-that-signalled
  ;; In a package that does not use COMMON-LISP the names read
  ;; (COMMON-LISP:LABELS ... :IN ...): the operator is qualified too.
  (dolist (package (list (find-package '#:cl-mcp/tests/frame-inspector-test)
                         (or (find-package "CL-MCP-FRAME-PROBE-WITHOUT-CL")
                             (make-package "CL-MCP-FRAME-PROBE-WITHOUT-CL" :use '()))))
    (testing (format nil "capture-error-context with :filter-internal keeps a user's ~
LABELS frames, printed relative to ~A" (package-name package))
      (let* ((names (%filtered-probe-frame-names package))
             (probe-frames (remove-if-not (lambda (name) (search "FRAME-PROBE-WALK" name))
                                          names))
             (visit-frames (remove-if-not
                            (lambda (name)
                              (and (search "LABELS " name)
                                   (search "VISIT :IN " name)))
                            probe-frames)))
        (ok probe-frames "the outer function's frame is there")
        (ok (= 2 (length visit-frames))
            "and so are both VISIT frames, the one that signalled included")
        (ok (search "VISIT :IN " (first probe-frames))
            "the probe's innermost frame shown is the one that signalled")))))

(defgeneric frame-probe-generic-function-with-a-long-enough-name (x)
  (:documentation "Signals from a :before method, for the frame name tests."))

(defmethod frame-probe-generic-function-with-a-long-enough-name :before ((x integer))
  (error "frame probe ~D" x))

(defmethod frame-probe-generic-function-with-a-long-enough-name ((x integer))
  x)

(deftest frame-function-names-stay-on-one-line
  (testing "a method frame's name is one line, however the printer is set up"
    ;; The name was printed with the caller's printer settings, so a qualified
    ;; method name wrapped and repl-eval's backtrace header broke over two
    ;; lines, pushing the source location onto the continuation line.  A
    ;; break straight after the operator also defeats the
    ;; "(SB-PCL::FAST-METHOD " prefix %INTERNAL-FRAME-P looks for, and a
    ;; user's *PRINT-LENGTH* cut the name short.
    (let ((context nil))
      (let ((*print-pretty* t)
            (*print-right-margin* 20)
            (*print-length* 2)
            (*print-level* 1)
            (*print-case* :downcase)
            ;; A user's dispatch entry that forces a break after the operator.
            (*print-pprint-dispatch* (copy-pprint-dispatch nil)))
        (set-pprint-dispatch '(cons (member sb-pcl::fast-method lambda))
                             (lambda (stream list)
                               (format stream "(~S~:@_~{ ~S~})" (first list) (rest list))))
        (block caught
          (handler-bind ((error (lambda (e)
                                  (setf context (capture-error-context e :max-frames 30))
                                  (return-from caught))))
            (frame-probe-generic-function-with-a-long-enough-name 1))))
      (let* ((names (mapcar (lambda (frame) (getf frame :function))
                            (getf context :frames)))
             (method-frame (find-if (lambda (name)
                                      (and (search "FAST-METHOD" name)
                                           (search "FRAME-PROBE-GENERIC" name)))
                                    names)))
        (ok names "frames were captured")
        (ok (notany (lambda (name) (find #\Newline name)) names)
            "no frame name holds a line break")
        (ok (notany (lambda (name) (search "..." name)) names)
            "and none is cut short by the caller's *PRINT-LENGTH*")
        (ok method-frame "the :before method's frame is there")
        (when method-frame
          (ok (search ":BEFORE (INTEGER))" method-frame)
              "with its qualifier and specializers")
          (ok (not (cl-mcp/src/frame-inspector::%internal-frame-p method-frame))
              "and is still recognized as the user's method")))))
  (testing "a lambda's empty lambda list reads as SBCL's debugger writes it"
    ;; Printing with *PRINT-PRETTY* off would show (LAMBDA NIL :IN ...).  The
    ;; frames of this deftest's own body and of Rove's runner are such lambdas.
    (let ((context nil))
      (block caught
        (handler-bind ((error (lambda (e)
                                (setf context (capture-error-context e :max-frames 30))
                                (return-from caught))))
          (error "frame probe lambda")))
      (let ((names (mapcar (lambda (frame) (getf frame :function))
                           (getf context :frames))))
        (ok (some (lambda (name) (search "(LAMBDA () :IN" name)) names))
        (ok (notany (lambda (name) (search "(LAMBDA NIL" name)) names))))))

(deftest frame-source-location-returns-real-line-number
 (testing
  "frame :source-line is a real line number, not a small TLF-offset integer"
  (let ((path (format nil "/tmp/cl-mcp-frame-demo-~A.lisp" (random 1000000)))
        (sym-name "CL-MCP-FRAME-DEMO-FN-XYZQ")
        captured)
    (unwind-protect
        (progn
         (with-open-file (s path :direction :output :if-exists :supersede)
           (format s "(in-package :cl-user)~%")
           (dotimes (i 8) (format s ";; padding line ~A~%" i))
           (format s "(defun ~A ()~%" sym-name)
           (format s "  (declare (optimize (debug 3)))~%")
           (format s "  (error \"boom\"))~%"))
         (load path)
         (let ((fn (find-symbol sym-name :cl-user)))
           (block caught
             (handler-bind ((error
                             (lambda (e)
                               (setf captured
                                       (cl-mcp/src/frame-inspector:capture-error-context
                                        e :max-frames 100))
                               (return-from caught))))
               (funcall fn))))
         (let ((demo-frame
                (find-if
                 (lambda (f) (search sym-name (or (getf f :function) "")))
                 (getf captured :frames))))
           (ok demo-frame "demo function frame was captured")
           (when demo-frame
             (let ((line (getf demo-frame :source-line))
                   (file (getf demo-frame :source-file)))
               (cond
                 ;; SBCL builds without DEBUG-SOURCE start-positions accessor
                 ;; cannot resolve the TLF offset to a source line.  In that
                 ;; mode %frame-source-location intentionally returns NIL for
                 ;; :source-line (see its docstring), so treat NIL as a build
                 ;; capability gap rather than a regression.
                 ((null line)
                  (ok t "skipped: SBCL build lacks debug-source start-positions"))
                 (t
                  (ok (integerp line) "source-line is an integer")
                  (when (and (stringp file) (search "cl-mcp-frame-demo" file))
                    (ok (and line (>= line 10))
                     "source-line is the defun line, not a TLF offset"))))))))
      (ignore-errors (delete-file path))
      (let ((sym (find-symbol sym-name :cl-user)))
        (when sym (unintern sym :cl-user)))))))
