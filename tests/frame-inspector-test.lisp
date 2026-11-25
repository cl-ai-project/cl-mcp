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
  ;; No condition is signalled, so the capture starts below its own frames:
  ;; at this function's, whose OBJECT is previewed.
  (let ((*package* (find-package '#:cl-user)))
    (let ((context
            (if ordinary-p
                (capture-error-context
                 (make-condition 'simple-error :format-control "preview source")
                 :max-frames 1 :locals-preview-frames 1
                 :preview-max-depth 2)
                (capture-debugger-error-context
                 (make-condition 'simple-error :format-control "preview source") callback
                 :max-frames 1 :locals-preview-frames 1
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

(defun frame-probe-case-key (items)
  "Signal from inside a CASE, whose key the compiler binds to a variable of its
own, SB-C::X."
  (declare (optimize (debug 3)))
  (case (first items)
    (:a (error "frame probe case ~S" items))
    (t nil)))

(deftest a-compiler-variable-is-named-with-its-package
  ;; Printed by its bare name, the variable CASE binds read as a user variable X
  ;; the function does not have.  SBCL's debugger writes SB-C::X.
  (let ((context nil))
    (block caught
      (handler-bind ((error (lambda (e)
                              (setf context (capture-error-context e :max-frames 30))
                              (return-from caught))))
        (frame-probe-case-key '(:a 1))))
    (let* ((frame (find-if (lambda (frame)
                             (search "FRAME-PROBE-CASE-KEY" (getf frame :function)))
                           (getf context :frames)))
           (names (mapcar (lambda (local) (getf local :name)) (getf frame :locals))))
      (ok (member "ITEMS" names :test #'string=) "the user's variable keeps its bare name")
      (ok (not (member "X" names :test #'string=))
          (format nil "no bare X: ~S" names))
      (ok (member "SB-C::X" names :test #'string=)
          "the compiler's variable is named with its package"))))

(defun frame-probe-dolist (items)
  "Signal inside a DOLIST, which binds the list it walks to a gensym."
  (declare (optimize (debug 3)))
  (dolist (item items)
    (error "frame probe dolist ~S" item)))

(deftest a-gensym-variable-is-written-as-uninterned
  ;; DOLIST's list variable printed as a bare LIST0, read as the user's own.
  (let ((context nil))
    (block caught
      (handler-bind ((error (lambda (e)
                              (setf context (capture-error-context e :max-frames 30))
                              (return-from caught))))
        (frame-probe-dolist '(1 2))))
    (let* ((frame (find-if (lambda (frame)
                             (search "FRAME-PROBE-DOLIST" (getf frame :function)))
                           (getf context :frames)))
           (names (mapcar (lambda (local) (getf local :name)) (getf frame :locals))))
      (ok (member "ITEM" names :test #'string=) "the user's variable keeps its bare name")
      (ok (some (lambda (name) (eql 0 (search "#:" name))) names)
          (format nil "the gensym is written #:NAME: ~S" names)))))

(deftest capture-error-context-says-when-its-cap-cut-the-walk
  (flet ((capture (max-frames)
           (block caught
             (handler-bind ((error (lambda (e)
                                     (return-from caught
                                       (capture-error-context e :max-frames max-frames)))))
               (frame-probe-dolist '(1))))))
    (ok (getf (capture 2) :frames-truncated) "two frames do not reach the outermost")
    (ok (not (getf (capture 10000) :frames-truncated)) "ten thousand do")))

(defun frame-probe-walk (item)
  "Signal from inside a LABELS function, for the local-function frame test.
VISIT recurses outside tail position and is called twice, so SBCL keeps it a
function of its own with frames of its own."
  (labels ((visit (depth)
             (if (zerop depth)
                 (error "frame probe local ~S" item)
                 (cons item (visit (1- depth))))))
    (list (visit 1) (visit 2))))

(defun %probe-frame-names (package)
  "Return the frame names capture-error-context records for FRAME-PROBE-WALK's
error, captured with *PACKAGE* bound to PACKAGE: frame names are printed
relative to it."
  (let ((context nil))
    (block caught
      (handler-bind ((error (lambda (e)
                              (setf context (capture-error-context e :max-frames 30))
                              (return-from caught))))
        (let ((*package* package))
          (frame-probe-walk :x))))
    (mapcar (lambda (frame) (getf frame :function))
            (getf context :frames))))

(deftest backtrace-starts-at-the-labels-frame-that-signalled
  ;; In a package that does not use COMMON-LISP the names read
  ;; (COMMON-LISP:LABELS ... :IN ...): the capture must not depend on that.
  (dolist (package (list (find-package '#:cl-mcp/tests/frame-inspector-test)
                         (or (find-package "CL-MCP-FRAME-PROBE-WITHOUT-CL")
                             (make-package "CL-MCP-FRAME-PROBE-WITHOUT-CL" :use '()))))
    (testing (format nil "capture-error-context records a user's LABELS frames, ~
printed relative to ~A" (package-name package))
      (let* ((names (%probe-frame-names package))
             (visit-frames (remove-if-not
                            (lambda (name)
                              (and (search "LABELS " name)
                                   (search "VISIT :IN " name)))
                            names)))
        (ok (find-if (lambda (name) (search "FRAME-PROBE-WALK" name)) names)
            "the outer function's frame is there")
        (ok (= 2 (length visit-frames))
            "and so are both VISIT frames, the one that signalled included")
        (ok (search "VISIT :IN " (first names))
            "the first frame is the one that signalled")))))

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
    ;; lines, pushing the source location onto the continuation line, and a
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
          (ok (not (getf (find method-frame (getf context :frames)
                               :key (lambda (frame) (getf frame :function))
                               :test #'equal)
                         :implementation))
              "and is not taken for SBCL's own code")))))
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

(deftest implementation-name-p-reads-the-name-sbcl-holds
  ;; The name is SBCL's own object, not its printed text, so no *PACKAGE* or
  ;; printer setting can change the answer.
  (flet ((impl-p (name) (cl-mcp/src/frame-inspector::%implementation-name-p name)))
    (testing "a symbol is SBCL's when its home package is"
      (ok (impl-p 'sb-kernel::%signal))
      (ok (impl-p 'sb-int:simple-eval-in-lexenv))
      (ok (not (impl-p 'car)) "a COMMON-LISP function is the API called, not SBCL's own")
      (ok (not (impl-p 'frame-probe-walk)))
      (ok (not (impl-p 'cl-mcp/src/frame-inspector:capture-error-context))
          "cl-mcp's code is not SBCL's: it is what is debugged when cl-mcp is"))
    (testing "a compound name names its second element"
      (ok (not (impl-p '(setf frame-probe-walk))))
      (ok (impl-p '(setf sb-kernel::%signal)))
      (ok (not (impl-p '(sb-pcl::fast-method frame-probe-walk (integer)))))
      (ok (impl-p '(sb-pcl::fast-method sb-kernel::%signal (t))))
      (ok (not (impl-p '(labels frame-probe-walk :in "repl-eval"))))
      (ok (impl-p '(flet sb-kernel::%signal :in "SYS:SRC;CODE;WARM-ERROR.LISP"))))
    (testing "a local function is its OUTER's, whoever named it"
      ;; WITH-MUTEX, UNWIND-PROTECT and WITHOUT-INTERRUPTS wrap the user's body
      ;; in a local function SBCL names: the body is still the user's code.
      (ok (not (impl-p '(flet sb-kernel::%signal :in frame-probe-walk))))
      (ok (not (impl-p '(flet "CLEANUP-FUN-0" :in frame-probe-walk))))
      (ok (impl-p '(flet "WITHOUT-INTERRUPTS-BODY-1" :in sb-kernel::%signal)))
      (ok (impl-p '(labels sb-kernel::%signal)) "with no OUTER, its own name decides")
      (ok (not (impl-p '(labels frame-probe-walk)))))
    (testing "a lambda is its OUTER's, a source file SBCL's on the SYS: host"
      (ok (not (impl-p '(lambda (x) :in "repl-eval"))))
      (ok (not (impl-p '(lambda () :in "/home/user/app.lisp"))))
      (ok (impl-p '(lambda () :in "SYS:SRC;CODE;LIST.LISP")))
      (ok (not (impl-p '(lambda (x) :in frame-probe-walk))))
      (ok (impl-p '(lambda () :in sb-impl::%simple-eval)))
      (ok (not (impl-p '(lambda (x))))))
    (testing "a bare string is SBCL's name for a foreign or trampoline frame"
      (ok (impl-p "foreign function: call_into_lisp"))
      (ok (impl-p "undefined function")))))

(defun %capture-in-handler (thunk &rest options)
  "Run THUNK and return the capture its first error's handler takes, with OPTIONS."
  (block caught
    (handler-bind ((error (lambda (e)
                            (return-from caught
                              (apply #'capture-error-context e options)))))
      (funcall thunk))))

(deftest capture-starts-at-the-frame-that-signalled
  (testing "the handler's frames and the signalling machinery above it are not captured"
    (let* ((ctx (%capture-in-handler (lambda () (frame-probe-dolist '(1)))))
           (first-frame (first (getf ctx :frames))))
      #+sbcl
      (progn
        (ok (search "FRAME-PROBE-DOLIST" (getf first-frame :function))
            (format nil "the first frame is the one that signalled: ~A"
                    (getf first-frame :function)))
        (ok (eql 0 (getf first-frame :index)) "and is numbered 0"))))
  (testing "a capture after HANDLER-CASE unwound starts below the capture's own frames"
    (let ((ctx (handler-case (frame-probe-dolist '(1))
                 (error (e) (capture-error-context e)))))
      #+sbcl
      (ok (notany (lambda (frame)
                    (search "CAPTURE-ERROR-CONTEXT" (getf frame :function)))
                  (getf ctx :frames))
          "capture-error-context's own frame is not among them"))))

(deftest capture-ends-at-the-user-code-boundary
  (testing "the boundary, the evaluator it called and everything below are not captured"
    (let ((ctx (%capture-in-handler
                (lambda ()
                  (cl-mcp/src/frame-inspector::eval-at-user-code-boundary
                   '(frame-probe-dolist '(1)))))))
      #+sbcl
      (progn
        (ok (equal '("FRAME-PROBE-DOLIST")
                   (mapcar (lambda (frame)
                             (let ((name (getf frame :function)))
                               (subseq name (1+ (or (position #\: name :from-end t) -1)))))
                           (getf ctx :frames)))
            (format nil "only the evaluated code's frame is left: ~S"
                    (mapcar (lambda (frame) (getf frame :function)) (getf ctx :frames))))
        (ok (not (getf ctx :frames-truncated)))
        (ok (eql 1 (getf ctx :frames-total))))))
  (testing "past MAX-FRAMES the outermost frame of the code is still recorded"
    (let* ((ctx (%capture-in-handler
                 (lambda ()
                   (cl-mcp/src/frame-inspector::eval-at-user-code-boundary
                    '(frame-probe-deep 30)))
                 :max-frames 5))
           (frames (getf ctx :frames)))
      #+sbcl
      (progn
        (ok (= 5 (length frames)) "MAX-FRAMES frames are recorded")
        (ok (getf ctx :frames-truncated) "and the capture says the code held more")
        (ok (< 5 (getf ctx :frames-total)) "counting them")
        (ok (= (1- (getf ctx :frames-total)) (getf (car (last frames)) :index))
            "the last one recorded is the code's outermost")
        ;; The evaluator applies FRAME-PROBE-DEEP itself, so its first call is
        ;; the outermost frame of the code under any compile policy.
        (ok (equal "30" (getf (first (getf (car (last frames)) :locals)) :value))
            (format nil "the first call, N = 30: ~S" (car (last frames)))))))
  (testing "a stack deeper than SBCL's default backtrace count still reaches the boundary"
    (let ((ctx (%capture-in-handler
                (lambda ()
                  (cl-mcp/src/frame-inspector::eval-at-user-code-boundary
                   '(frame-probe-deep 1500)))
                :max-frames 5)))
      #+sbcl
      (ok (= 1501 (getf ctx :frames-total))
          (format nil "every call is counted: ~S" (getf ctx :frames-total))))))

(defun frame-probe-deep (n)
  "Recurse N times, then signal; each call stays on the stack."
  (declare (optimize (debug 3) (speed 0)))
  (if (zerop n)
      (error "deep bottom")
      (1+ (frame-probe-deep (1- n)))))

#+sbcl
(defvar *frame-probe-mutex* (sb-thread:make-mutex :name "frame-probe"))

#+sbcl
(defun frame-probe-locked (x)
  "Signal from a WITH-MUTEX body, which SBCL compiles as a local function."
  (declare (optimize (debug 3) (speed 0)))
  (sb-thread:with-mutex (*frame-probe-mutex*)
    (let ((doubled (* 2 x)))
      (error "in mutex ~A" doubled))))

#+sbcl
(deftest a-body-sbcl-wraps-in-a-local-function-is-the-users
  (let* ((ctx (%capture-in-handler
               (lambda ()
                 (cl-mcp/src/frame-inspector::eval-at-user-code-boundary
                  '(frame-probe-locked 21)))))
         (first-frame (first (getf ctx :frames))))
    (ok (search "FRAME-PROBE-LOCKED" (getf first-frame :function))
        (format nil "the body's frame comes first: ~S" (getf first-frame :function)))
    (ok (not (getf first-frame :implementation)) "and is not marked as SBCL's")
    (ok (find "DOUBLED" (getf first-frame :locals)
              :key (lambda (local) (getf local :name)) :test #'equal)
        "and keeps its locals")))

(deftest stack-window-is-decided-by-position
  (flet ((window (&rest names)
           (multiple-value-list
            (cl-mcp/src/frame-inspector::%stack-window (coerce names 'vector)))))
    (let ((walk 'cl-mcp/src/frame-inspector::%map-stack)
          (collect 'cl-mcp/src/frame-inspector::%collect-window)
          (capture 'cl-mcp/src/frame-inspector:capture-error-context)
          (boundary 'cl-mcp/src/frame-inspector::eval-at-user-code-boundary)
          (signal 'sb-kernel::%signal)
          (evaluator 'sb-int:simple-eval-in-lexenv))
      (ok (equal '(0 0) (window)) "an empty walk is an empty window")
      (ok (equal '(6 8)
                 (window walk collect capture 'handler signal 'error
                         'inner 'outer evaluator boundary 'below))
          "a handler's capture: below the signalling run, above the evaluator")
      (ok (equal '(6 10)
                 (window walk collect capture 'handler signal 'error 'inner 'outer
                         'thread-bottom 'foreign))
          "no boundary: the window runs to the bottom")
      (ok (equal '(3 5)
                 (window walk collect capture 'unwound-caller 'outer boundary))
          "after HANDLER-CASE unwound: below the capture's own frames")
      (ok (equal '(2 3)
                 (window 'sb-int:%break 'break 'caller boundary))
          "a walk starting elsewhere: below the leading signalling run")
      (ok (equal '(6 6)
                 (window walk collect capture 'handler signal 'error evaluator boundary))
          "the form signalled itself: an empty window")
      (ok (equal '(6 11)
                 (window walk collect capture 'handler signal 'error
                         'inner signal 'error 'outer 'outermost boundary))
          "signalling frames between the code's own stay in the window"))))
