;;;; tests/clos-describe-integration-test.lisp
;;;;
;;;; End-to-end proof, against a real SBCL image, that clos-describe's
;;;; fail-closed source matching (design spec 3.1-3.6) does what it exists
;;;; for: a source file is written, compiled and loaded, then edited in
;;;; place and reloaded so the running image holds both the old and the new
;;;; definition of several kinds of CLOS form at once -- the exact situation
;;;; a hand-built report (as clos-core-test.lisp, clos-verify-core-test.lisp
;;;; and clos-response-builders-test.lisp use) cannot reproduce, because
;;;; those tests never recompile a form out from under a live method object.
;;;;
;;;; Six deftests:
;;;; - CLOS-DESCRIBE-FAILS-CLOSED-AGAINST-A-RELOADED-IMAGE covers spec 3.5's
;;;;   round trip for five kinds of stale definition (EQL specializer,
;;;;   class specializer, a deleted method, a DEFGENERIC inline method, an
;;;;   accessor rename) plus the positive case: an untouched method stays
;;;;   MATCHED, and its form_type/form_name, passed to LISP-EDIT-FORM for
;;;;   real, changes only that one form.
;;;; - CLOS-DESCRIBE-AGREES-ACROSS-POOL-AND-NO-POOL-PATHS spawns a real
;;;;   worker (skipping cleanly when `ros` is unavailable) and confirms the
;;;;   worker-proxied and in-process paths agree on SOURCE_MATCH and
;;;;   IDENTITY for a bignum EQL datum, a NIL EQL datum, a two-specializer
;;;;   method and a SETF generic function's boolean flag -- the values the
;;;;   project's own memory notes a JSON round trip can render differently
;;;;   (YASON:FALSE in-process vs. plain NIL once decoded from the wire).
;;;; - CLOS-DESCRIBE-HANDS-OUT-AN-EDIT-GUARD-LISP-EDIT-FORM-ACCEPTS-AND-LATER-REFUSES
;;;;   walks a guard through LISP-EDIT-FORM once, then reuses it after the
;;;;   file changed and requires the structured conflict.
;;;; - CLOS-DESCRIBE-DOES-NOT-CONFIRM-AN-ACCESSOR-AGAINST-A-WRITER-ONLY-SLOT
;;;;   rewrites :ACCESSOR to :WRITER under an unchanged file-write-date and
;;;;   requires the entry not to come back MATCHED.
;;;; - CLOS-DESCRIBE-MATCHES-A-QUOTED-EQL-KEYWORD-OVER-THE-WORKER-RPC loads the
;;;;   fixture only in the worker, so a (EQL ':READY) method can only be
;;;;   confirmed from data that crossed the wire.
;;;; - CLOS-DESCRIBE-POINTS-A-CONDITION-READER-OVERRIDE-AT-ITS-OWN-DEFMETHOD
;;;;   loads a condition whose generated reader a hand-written DEFMETHOD
;;;;   replaced -- indistinguishable from a genuine reader in the image -- and
;;;;   requires the edit target to be that DEFMETHOD, while the untouched
;;;;   reader beside it still resolves to the DEFINE-CONDITION.

(defpackage #:cl-mcp/tests/clos-describe-integration-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok #:skip)
  (:import-from #:cl-mcp/src/clos-core
                #:clos-describe-report)
  (:import-from #:cl-mcp/src/clos-verify-core
                #:verify-entries)
  (:import-from #:cl-mcp/src/tools/clos-response-builders
                #:build-clos-describe-response)
  (:import-from #:cl-mcp/src/code-refs-core
                #:sequence->list)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/lisp-edit-form
                #:lisp-edit-form)
  (:import-from #:cl-mcp/src/lisp-edit-form-core
                #:edit-guard-conflict-error
                #:edit-guard-conflict)
  (:import-from #:cl-mcp/src/cst
                #:parse-top-level-forms
                #:cst-node-start
                #:cst-node-end)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht)
  (:import-from #:cl-mcp/src/proxy
                #:*use-worker-pool*
                #:proxy-to-worker)
  (:import-from #:cl-mcp/src/state
                #:*current-session-id*)
  (:import-from #:cl-mcp/tests/test-helpers
                #:spawn-available-p
                #:with-pool)
  (:import-from #:asdf
                #:system-source-directory))

(in-package #:cl-mcp/tests/clos-describe-integration-test)

;;; ---------------------------------------------------------------------------
;;; Shared file helpers
;;; ---------------------------------------------------------------------------

(defun %write-text (path text)
  "Write TEXT to PATH, creating its directory first."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output
                        :if-exists :supersede :if-does-not-exist :create
                        :external-format :utf-8)
    (write-string text out)))

(defun %compile-and-load-path (path)
  "Compile and load the file at PATH, its truename as the source
namestring -- REPL-EVAL's compilation unit would otherwise name the file
\"repl-eval\", which the parent's CST scan could never find on disk again
(clos-core-test.lisp's %LOAD-FIXTURE uses the same convention)."
  (let ((truename (truename path)))
    (with-compilation-unit (:override t :source-namestring (namestring truename))
      (handler-bind ((warning #'muffle-warning))
        (load (compile-file truename :verbose nil :print nil))))))

(defun %delete-fixture (path)
  "Delete PATH and the .fasl %COMPILE-AND-LOAD-PATH left beside it, if any."
  (ignore-errors (delete-file (compile-file-pathname path)))
  (ignore-errors (delete-file path)))

;;; ---------------------------------------------------------------------------
;;; Report helpers, matching clos-core-test.lisp / clos-response-builders-
;;; test.lisp's own naming
;;; ---------------------------------------------------------------------------

(defun %annotated-report (designator &key (limit 50))
  "Return DESIGNATOR's clos-describe report, annotated exactly as
CL-MCP/SRC/CLOS:CLOS-DESCRIBE's pool-disabled path does."
  (build-clos-describe-response
   (clos-describe-report designator :limit limit)
   (lambda (entries) (verify-entries entries))))

(defun %pool-report (designator &key (limit 50))
  "Return DESIGNATOR's clos-describe report the way CL-MCP/SRC/CLOS does
when the worker pool is enabled: both RPCs proxied to a real worker, only
the source-file half of the flow (already exercised by %ANNOTATED-REPORT)
staying local, exactly as spec 3.6 describes."
  (build-clos-describe-response
   (proxy-to-worker 1 "worker/clos-describe"
                     (make-ht "symbol" designator "package" nil "limit" limit))
   (lambda (entries)
     (proxy-to-worker 1 "worker/clos-verify-source" (make-ht "entries" entries)))))

(defun %gfs (report)
  "Return REPORT's generic function entries as a list."
  (sequence->list (gethash "generic_functions" report)))

(defun %methods (entry)
  "Return ENTRY's method objects as a list."
  (sequence->list (gethash "methods" entry)))

(defun %specializer-text (method)
  "Return METHOD's display specializers joined into one searchable string."
  (format nil "~{~A~^ ~}" (sequence->list (gethash "specializers" method))))

(defun %one-matching (methods substring)
  "Return the sole method in METHODS whose display specializers contain
SUBSTRING.  Signals an error naming the count when that is not exactly
one, so a broken assumption about the fixture fails loudly instead of
silently comparing the wrong method."
  (let ((matches (remove-if-not
                  (lambda (m) (search substring (%specializer-text m))) methods)))
    (assert (= 1 (length matches)) ()
            "expected exactly one method matching ~S, found ~D"
            substring (length matches))
    (first matches)))

(defun %unedited-p (method)
  "True when METHOD carries no edit information -- the fail-closed
contract for a MISMATCHED or UNVERIFIED entry (spec 3.1)."
  (and (member (gethash "source_match" method) '("mismatched" "unverified")
               :test #'equal)
       (null (gethash "form_type" method))
       (null (gethash "form_name" method))
       (stringp (gethash "source_match_reason" method))))

(defun %top-level-texts (text)
  "Return the raw text of every top-level form in TEXT, in file order."
  (mapcar (lambda (node) (subseq text (cst-node-start node) (cst-node-end node)))
          (parse-top-level-forms text)))

;;; ---------------------------------------------------------------------------
;;; Deftest 1: a real reload, five stale definitions, one round trip
;;; ---------------------------------------------------------------------------

(defparameter *fixture-path*
  (asdf/system:system-relative-pathname
   :cl-mcp "tests/tmp/clos-describe-integration-fixture.lisp")
  "Scratch file this test writes, compiles, edits in place and recompiles.
Never checked in: tests/tmp/ is gitignored, and %DELETE-FIXTURE removes it
(and its .fasl) again in the test's UNWIND-PROTECT cleanup.")

(defun %fixture-text (&key (shape-specializer "cat") (eql-datum ":old")
                       (doomed-lines (list "(defmethod doomed ((x cat))"
                                           "  :doomed)"))
                       (combine-specializer "integer")
                       (accessor-name "widget-size"))
  "Return the source text of CL-MCP-CLOS-DESCRIBE-INTEGRATION-FIXTURE.
Called with different keyword combinations across three versions the test
writes in sequence: every line outside the six substituted tokens is
byte-identical across all calls, so each later version leaves every
untouched definition's SB-INTROSPECT source line exactly where it was.

SHAPE-SPECIALIZER, EQL-DATUM and DOOMED-LINES are compiled and loaded
twice (version 1, then version 2): a plain DEFMETHOD's specializer only
ever grows a second, coexisting method when its OWN specializer changes
(SBCL never removes the old one), which is exactly the \"stale method
still in the image\" situation clos-describe's matching exists for (design
spec 1, 3.1).

COMBINE-SPECIALIZER and ACCESSOR-NAME are different: a DEFGENERIC's inline
(:method ...) option and a DEFCLASS accessor are both metaobject-protocol-
managed, so SBCL's own class/generic-function redefinition removes the
superseded method as part of reloading -- there is never a moment with
both old and new accessor methods live at once.  The test instead writes a
third version with only these two changed and never recompiles it: the
running image still reports the version-2 identity, while the source scan
now reads the version-3 file, reproducing the equally real \"file edited,
image not yet reloaded\" mismatch (spec 3.1's HEAD note: fail-closed
matching guards both cases, not just a stale live method)."
  (format nil "~
;;;; Written by cl-mcp/tests/clos-describe-integration-test; deleted after.

(defpackage #:cl-mcp-clos-describe-integration-fixture
  (:use #:cl)
  (:export #:cat #:dog #:shape-tag #:eql-tag #:doomed #:combine
           #:widget #:widget-size #:describe-stable))

(in-package #:cl-mcp-clos-describe-integration-fixture)

(defclass cat () ())

(defclass dog () ())

;; shape-tag's specializer class changes between versions.
(defgeneric shape-tag (x))

(defmethod shape-tag ((x ~A))
  :cat-tag)

;; eql-tag's EQL specializer changes between versions.
(defgeneric eql-tag (x))

(defmethod eql-tag ((x (eql ~A)))
  :tagged)

;; doomed's sole method is deleted entirely in version 2.
(defgeneric doomed (x))

~A
~A

(defgeneric combine (a b)
  (:method-combination +)
  (:method + ((a ~A) b) a))

(defmethod combine + ((a number) (b number))
  b)

(defclass widget ()
  ((size :initarg :size :accessor ~A)))

(defmethod describe-stable ((x cat) stream)
  (declare (ignore stream))
  :stable)
"
          shape-specializer eql-datum
          (first doomed-lines) (second doomed-lines)
          combine-specializer accessor-name))

(deftest clos-describe-fails-closed-against-a-reloaded-image
  (unwind-protect
       (progn
         (%write-text *fixture-path* (%fixture-text))
         (%compile-and-load-path *fixture-path*)
         ;; SB-INTROSPECT keeps one DEBUG-SOURCE per (file, second): two
         ;; compiles of the same truename landing in the same wall-clock
         ;; second used to be resolved by picking whichever recorded the
         ;; most forms, which is wrong when the *later* one -- as "doomed"'s
         ;; deletion below does -- has fewer.  CL-MCP/SRC/CODE-CORE:
         ;; %DEBUG-SOURCES-BY-NAMESTRING now refuses to guess when two
         ;; same-second sources aren't one a prefix of the other, so the
         ;; version-1/version-2 race this comment used to sidestep with a
         ;; sleep no longer needs sidestepping: %FORM-START-OFFSET falls
         ;; back to reading the file, which is always the version 2 that is
         ;; on disk by the time %WRITE-TEXT below returns.
         (%write-text *fixture-path*
                      (%fixture-text
                       :shape-specializer "dog"
                       :eql-datum ":new"
                       :doomed-lines
                       (list ";; doomed's defmethod was deleted here on purpose."
                             ";; (kept as a comment so later lines do not shift.)")))
         (%compile-and-load-path *fixture-path*)
         (let ((*project-root* (system-source-directory :cl-mcp)))
           (testing "a class specializer change: the old CAT method is unedited"
             (let* ((report (%annotated-report
                              "cl-mcp-clos-describe-integration-fixture:shape-tag"))
                    (methods (%methods (first (%gfs report))))
                    (old (%one-matching methods ":CAT"))
                    (new (%one-matching methods ":DOG")))
               (ok (= 2 (length methods)) "both the old and the new method remain")
               (ok (%unedited-p old))
               (ok (equal "matched" (gethash "source_match" new)))
               (ok (equal "defmethod" (gethash "form_type" new)))
               (ok (stringp (gethash "form_name" new)))))
           (testing "an EQL specializer change: the old :OLD method is unedited"
             (let* ((report (%annotated-report
                              "cl-mcp-clos-describe-integration-fixture:eql-tag"))
                    (methods (%methods (first (%gfs report))))
                    (old (%one-matching methods ":OLD"))
                    (new (%one-matching methods ":NEW")))
               (ok (= 2 (length methods)) "both the old and the new method remain")
               (ok (%unedited-p old))
               (ok (equal "matched" (gethash "source_match" new)))
               (ok (equal "defmethod" (gethash "form_type" new)))
               (ok (stringp (gethash "form_name" new)))))
           (testing "a deleted method: the old method is unedited, with a clear reason"
             ;; SB-INTROSPECT's recorded offset for a definition points just
             ;; past the *previous* top-level form and is walked forward
             ;; past whitespace and comments (CL-MCP/SRC/CODE-CORE:%OFFSET-
             ;; >LINE) to find "the form it belongs to" -- so replacing
             ;; doomed's defmethod with a same-length comment does not land
             ;; on an empty line; it lands on COMBINE, the next real
             ;; definition, which is exactly as fail-closed: DOOMED's name
             ;; does not match COMBINE's, so this is MISMATCHED, not the
             ;; separate "no form starts here" case a truly empty tail of
             ;; file would produce.
             (let* ((report (%annotated-report
                              "cl-mcp-clos-describe-integration-fixture:doomed"))
                    (methods (%methods (first (%gfs report))))
                    (old (first methods)))
               (ok (= 1 (length methods)) "only the orphaned method remains")
               (ok (%unedited-p old))))
           (testing "an unchanged method stays matched, and editing it changes only itself"
             ;; Queried before the further, uncompiled write below: any
             ;; write to the file bumps its mtime past what this load
             ;; recorded, which would otherwise mark every entry STALE and
             ;; downgrade this one from MATCHED, unrelated to whether
             ;; describe-stable's own text actually changed.
             (let* ((report (%annotated-report
                              "cl-mcp-clos-describe-integration-fixture:describe-stable"))
                    (stable (first (%methods (first (%gfs report))))))
               (ok (equal "matched" (gethash "source_match" stable)))
               (ok (equal "defmethod" (gethash "form_type" stable)))
               (ok (stringp (gethash "form_name" stable)))
               (let ((before-text (uiop:read-file-string *fixture-path*)))
                 (multiple-value-bind (updated-text warning would-change)
                     (lisp-edit-form
                      :file-path (namestring (truename *fixture-path*))
                      :form-type (gethash "form_type" stable)
                      :form-name (gethash "form_name" stable)
                      :operation "replace"
                      :content (format nil "~
(defmethod describe-stable ((x cat) stream)
  (declare (ignore stream))
  :stable-edited)")
                      :dry-run nil)
                   (declare (ignore updated-text warning))
                   (ok would-change "lisp-edit-form reports a real change"))
                 (let* ((after-text (uiop:read-file-string *fixture-path*))
                        (before-forms (%top-level-texts before-text))
                        (after-forms (%top-level-texts after-text))
                        (diff-indices
                         (loop for i from 0
                               for b in before-forms
                               for a in after-forms
                               unless (string= b a) collect i)))
                   (ok (= (length before-forms) (length after-forms))
                       "no top-level form was added or removed")
                   (ok (= 1 (length diff-indices))
                       "exactly one top-level form's text changed")
                   (when diff-indices
                     (ok (search ":stable-edited" (nth (first diff-indices) after-forms))
                         "the changed form is the intended describe-stable method")
                     (ok (search ":stable)" (nth (first diff-indices) before-forms))
                         "...which used to read :stable"))))))
           ;; COMBINE's inline DEFGENERIC (:method ...) option and WIDGET's
           ;; DEFCLASS accessor are both metaobject-protocol-managed: SBCL
           ;; removes the superseded method as part of reloading either
           ;; form, so there is never a moment with both an old and a new
           ;; one live at once (unlike a plain DEFMETHOD, confirmed above).
           ;; A third file version, written but never recompiled, still
           ;; reproduces the fail-closed case that matters in practice: the
           ;; live image's identity (still version 2's) no longer matches
           ;; what is now on disk (version 3) -- an edit made after the last
           ;; reload, which is exactly what STALE and this whole feature
           ;; also guard against.  (Rewriting the whole file here also reverts
           ;; describe-stable's just-verified edit above, harmlessly: nothing
           ;; queries it again.)
           (%write-text *fixture-path*
                        (%fixture-text
                         :shape-specializer "dog"
                         :eql-datum ":new"
                         :doomed-lines
                         (list ";; doomed's defmethod was deleted here on purpose."
                               ";; (kept as a comment so later lines do not shift.)")
                         :combine-specializer "string"
                         :accessor-name "widget-size-2"))
           (testing "a DEFGENERIC inline method edited after the last reload is unedited"
             (let* ((report (%annotated-report
                              "cl-mcp-clos-describe-integration-fixture:combine"))
                    (methods (%methods (first (%gfs report))))
                    (old (%one-matching methods "INTEGER")))
               (ok (= 2 (length methods))
                   "the image still only has the reloaded INTEGER and NUMBER methods")
               (ok (%unedited-p old))))
           (testing "an accessor renamed after the last reload is unedited"
             (let* ((report (%annotated-report
                              "cl-mcp-clos-describe-integration-fixture:widget"))
                    (methods (%methods (gethash "class" report))))
               (ok (= 2 (length methods))
                   "the image still only has the reloaded WIDGET-SIZE accessor")
               (dolist (method methods)
                 (ok (%unedited-p method)))))))
    (%delete-fixture *fixture-path*)))

;;; ---------------------------------------------------------------------------
;;; Deftest 2: pool vs. no-pool agreement on tricky identity values
;;; ---------------------------------------------------------------------------

(defun %falsy-p (value)
  "True when VALUE is JSON false crossing either boundary this project
uses: the in-process sentinel YASON:FALSE, or plain NIL once a real JSON
round trip decodes \"false\" (clos-verify-core.lisp's own %TRUE-P notes the
same difference for the worker-pool boundary)."
  (or (null value) (eq value 'yason:false)))

(defun %truthy-p (value)
  "True when VALUE is JSON true: non-NIL and not the YASON:FALSE sentinel."
  (and value (not (eq value 'yason:false))))

(defun %kind-method (methods kind)
  "Return the sole method in METHODS whose \"kind\" is KIND (\"reader\" or
\"writer\")."
  (find kind methods :key (lambda (m) (gethash "kind" m)) :test #'equal))

(defparameter *agreement-fixture-path*
  (asdf/system:system-relative-pathname
   :cl-mcp "tests/tmp/clos-describe-agreement-fixture.lisp")
  "A second, independent scratch file for the pool/no-pool agreement test,
never edited after its one load -- unlike *FIXTURE-PATH* above, it exists
only to expose a bignum EQL datum, a NIL EQL datum, a two-specializer
method and a SETF generic function to both code paths at once.")

(defparameter *agreement-fixture-text*
  "
;;;; Written by cl-mcp/tests/clos-describe-integration-test; deleted after.

(defpackage #:cl-mcp-clos-describe-agreement-fixture
  (:use #:cl)
  (:export #:wide-tag #:widget #:widget-size #:combo))

(in-package #:cl-mcp-clos-describe-agreement-fixture)

(defgeneric wide-tag (x))

(defmethod wide-tag ((x (eql 123456789012345678901234567890)))
  :big)

(defmethod wide-tag ((x (eql nil)))
  :none)

(defclass widget ()
  ((size :initarg :size :accessor widget-size)))

(defgeneric combo (a b))

(defmethod combo ((a integer) (b string))
  (list a b))
"
  "Loaded once in this process and once in a spawned worker, from the same
absolute path on the shared filesystem, so both images describe the same
definitions.")

(deftest clos-describe-agrees-across-pool-and-no-pool-paths
  (testing "the same tricky identity values mean the same thing either way"
    (unless (spawn-available-p)
      (skip "ros not available"))
    (unwind-protect
         (progn
           (%write-text *agreement-fixture-path* *agreement-fixture-text*)
           (%compile-and-load-path *agreement-fixture-path*)
           (let ((*project-root* (system-source-directory :cl-mcp))
                 (*use-worker-pool* t)
                 (*current-session-id* "clos-describe-integration-agreement"))
             (with-pool ()
               (let* ((truename (truename *agreement-fixture-path*))
                      (load-code
                       (format nil "~
(with-compilation-unit (:override t :source-namestring (namestring (truename ~S)))
  (handler-bind ((warning (function muffle-warning)))
    (load (compile-file (truename ~S) :verbose nil :print nil))))
:loaded"
                               (namestring truename) (namestring truename)))
                      (load-result
                       (proxy-to-worker 1 "worker/eval"
                                         (make-ht "code" load-code "package" "CL-USER"))))
                 (ok (not (gethash "isError" load-result))
                     "the worker compiled and loaded the same fixture file")
                 (let* ((nopool (%annotated-report
                                 "cl-mcp-clos-describe-agreement-fixture:wide-tag"))
                        (pool (%pool-report
                               "cl-mcp-clos-describe-agreement-fixture:wide-tag"))
                        (nopool-methods (%methods (first (%gfs nopool))))
                        (pool-methods (%methods (first (%gfs pool))))
                        (nopool-big (%one-matching nopool-methods "123456789012345678901234567890"))
                        (pool-big (%one-matching pool-methods "123456789012345678901234567890"))
                        (nopool-nil (find-if (lambda (m) (not (eq m nopool-big))) nopool-methods))
                        (pool-nil (find-if (lambda (m) (not (eq m pool-big))) pool-methods)))
                   (testing "a bignum EQL datum: same kind and same decimal text either way"
                     (let ((nopool-datum
                            (gethash "datum" (aref (gethash "specializers"
                                                            (gethash "identity" nopool-big))
                                                    0)))
                           (pool-datum
                            (gethash "datum" (aref (gethash "specializers"
                                                            (gethash "identity" pool-big))
                                                    0))))
                       (ok (equal "matched" (gethash "source_match" nopool-big)))
                       (ok (equal "matched" (gethash "source_match" pool-big)))
                       (ok (equal "integer" (gethash "kind" nopool-datum)))
                       (ok (equal "integer" (gethash "kind" pool-datum)))
                       (ok (equal "123456789012345678901234567890"
                                  (gethash "value" nopool-datum)))
                       (ok (equal "123456789012345678901234567890"
                                  (gethash "value" pool-datum)))))
                   (testing "a NIL EQL datum: tagged boolean text, not absent info, either way"
                     (let ((nopool-datum
                            (gethash "datum" (aref (gethash "specializers"
                                                            (gethash "identity" nopool-nil))
                                                    0)))
                           (pool-datum
                            (gethash "datum" (aref (gethash "specializers"
                                                            (gethash "identity" pool-nil))
                                                    0))))
                       (ok (equal "matched" (gethash "source_match" nopool-nil)))
                       (ok (equal "matched" (gethash "source_match" pool-nil)))
                       (ok (equal "boolean" (gethash "kind" nopool-datum)))
                       (ok (equal "boolean" (gethash "kind" pool-datum)))
                       ;; Spec 3.3 tags a NIL EQL datum as the STRING "NIL"
                       ;; (never a bare JSON boolean), precisely so it is
                       ;; never mistaken for "no value here" -- a missing
                       ;; field would read back as Lisp NIL too, but GETHASH
                       ;; returning the two-letter string "NIL" proves the
                       ;; field is genuinely present and correctly typed,
                       ;; identically whether or not a JSON round trip
                       ;; happened.
                       (ok (equal "NIL" (gethash "value" nopool-datum)))
                       (ok (equal "NIL" (gethash "value" pool-datum))))))
                 (let* ((nopool (%annotated-report
                                 "cl-mcp-clos-describe-agreement-fixture:combo"))
                        (pool (%pool-report
                               "cl-mcp-clos-describe-agreement-fixture:combo"))
                        (nopool-m (first (%methods (first (%gfs nopool)))))
                        (pool-m (first (%methods (first (%gfs pool))))))
                   (testing "a two-specializer method: the same specializer count either way"
                     (ok (equal "matched" (gethash "source_match" nopool-m)))
                     (ok (equal "matched" (gethash "source_match" pool-m)))
                     (ok (= 2 (length (sequence->list
                                       (gethash "specializers" (gethash "identity" nopool-m))))))
                     (ok (= 2 (length (sequence->list
                                       (gethash "specializers" (gethash "identity" pool-m))))))))
                 (let* ((nopool (%annotated-report
                                 "cl-mcp-clos-describe-agreement-fixture:widget"))
                        (pool (%pool-report
                               "cl-mcp-clos-describe-agreement-fixture:widget"))
                        (nopool-methods (%methods (gethash "class" nopool)))
                        (pool-methods (%methods (gethash "class" pool)))
                        (nopool-reader (%kind-method nopool-methods "reader"))
                        (pool-reader (%kind-method pool-methods "reader"))
                        (nopool-writer (%kind-method nopool-methods "writer"))
                        (pool-writer (%kind-method pool-methods "writer")))
                     (testing "a SETF generic function's boolean flag: true and false, either way"
                       (ok (equal "matched" (gethash "source_match" nopool-reader)))
                       (ok (equal "matched" (gethash "source_match" pool-reader)))
                       (ok (equal "matched" (gethash "source_match" nopool-writer)))
                       (ok (equal "matched" (gethash "source_match" pool-writer)))
                       (ok (%falsy-p (gethash "setf" (gethash "generic_function"
                                                              (gethash "identity" nopool-reader))))
                           "a reader's generic function name is not a SETF name, in-process")
                       (ok (%falsy-p (gethash "setf" (gethash "generic_function"
                                                              (gethash "identity" pool-reader))))
                           "...nor worker-proxied")
                       (ok (%truthy-p (gethash "setf" (gethash "generic_function"
                                                               (gethash "identity" nopool-writer))))
                           "a writer's generic function name is a SETF name, in-process")
                       (ok (%truthy-p (gethash "setf" (gethash "generic_function"
                                                               (gethash "identity" pool-writer))))
                           "...worker-proxied too")))))))
      (%delete-fixture *agreement-fixture-path*))))

;;; ---------------------------------------------------------------------------
;;; Deftest 3: edit_guard round trip -- accepted once, then rejected reused
;;; ---------------------------------------------------------------------------

(defparameter *guard-fixture-path*
  (asdf/system:system-relative-pathname
   :cl-mcp "tests/tmp/clos-describe-edit-guard-fixture.lisp")
  "A third, independent scratch file: clos-describe's own edit_guard (design
doc section 4.1) is handed to lisp-edit-form for a real edit, then reused
after that edit already changed the file, to prove both halves of the
guarantee end to end -- a fresh guard round-trips, and a stale one (already
spent on a prior edit) is refused, leaving the file untouched.")

(defparameter *guard-fixture-text*
  "
;;;; Written by cl-mcp/tests/clos-describe-integration-test; deleted after.

(defpackage #:cl-mcp-clos-describe-edit-guard-fixture
  (:use #:cl)
  (:export #:widget #:tag))

(in-package #:cl-mcp-clos-describe-edit-guard-fixture)

(defclass widget () ())

(defgeneric tag (x))

(defmethod tag ((x widget))
  :old)
")

(deftest clos-describe-hands-out-an-edit-guard-lisp-edit-form-accepts-and-later-refuses
  (unwind-protect
       (progn
         (%write-text *guard-fixture-path* *guard-fixture-text*)
         (%compile-and-load-path *guard-fixture-path*)
         (let* ((*project-root* (system-source-directory :cl-mcp))
                (truename (namestring (truename *guard-fixture-path*)))
                (report (%annotated-report "cl-mcp-clos-describe-edit-guard-fixture:tag"))
                (method (first (%methods (first (%gfs report)))))
                (guard (gethash "edit_guard" method))
                (before-text (uiop:read-file-string *guard-fixture-path*)))
           (testing "a matched method carries an edit_guard usable by lisp-edit-form"
             (ok (equal "matched" (gethash "source_match" method)))
             (ok (hash-table-p guard))
             (ok (equal truename (gethash "abs_path" guard))))
           (testing "observe -> edit with the guard -> only the intended form changes"
             (multiple-value-bind (updated-text warning would-change)
                 (lisp-edit-form
                  :file-path truename
                  :form-type (gethash "form_type" method)
                  :form-name (gethash "form_name" method)
                  :operation "replace"
                  :content (format nil "~
(defmethod tag ((x widget))
  :new)")
                  :guard guard
                  :dry-run nil)
               (declare (ignore updated-text warning))
               (ok would-change "lisp-edit-form reports a real change"))
             (let* ((after-text (uiop:read-file-string *guard-fixture-path*))
                    (before-forms (%top-level-texts before-text))
                    (after-forms (%top-level-texts after-text))
                    (diff-indices
                     (loop for i from 0
                           for b in before-forms
                           for a in after-forms
                           unless (string= b a) collect i)))
               (ok (= (length before-forms) (length after-forms))
                   "no top-level form was added or removed")
               (ok (= 1 (length diff-indices))
                   "exactly one top-level form's text changed")
               (when diff-indices
                 (ok (search ":new)" (nth (first diff-indices) after-forms))
                     "the changed form is the intended tag method")
                 (ok (search ":old)" (nth (first diff-indices) before-forms))
                     "...which used to read :old"))
               (testing "reusing the same, now-stale guard is refused and the file is untouched"
                 (let ((text-after-first-edit (uiop:read-file-string *guard-fixture-path*)))
                   (let ((condition
                          (handler-case
                              (progn
                                (lisp-edit-form
                                 :file-path truename
                                 :form-type (gethash "form_type" method)
                                 :form-name (gethash "form_name" method)
                                 :operation "replace"
                                 :content (format nil "~
(defmethod tag ((x widget))
  :second-edit)")
                                 :guard guard
                                 :dry-run nil)
                                nil)
                            (edit-guard-conflict-error (c) c))))
                     (ok (typep condition 'edit-guard-conflict-error)
                         "the stale guard is refused, not silently accepted")
                     (when (typep condition 'edit-guard-conflict-error)
                       (ok (stringp (getf (edit-guard-conflict condition) :reason)))))
                   (ok (string= text-after-first-edit
                                (uiop:read-file-string *guard-fixture-path*))
                       "the refused edit left the file byte-identical")))))))
    (%delete-fixture *guard-fixture-path*)))

;;; ---------------------------------------------------------------------------
;;; Deftest 4: an accessor is confirmed against the slot option that defines it
;;; ---------------------------------------------------------------------------

(defparameter *accessor-fixture-path*
  (asdf/system:system-relative-pathname
   :cl-mcp "tests/tmp/clos-describe-integration-accessor-fixture.lisp")
  "Scratch file the accessor test writes, compiles and then rewrites in
place without letting its write date advance.  Never checked in: tests/tmp/
is gitignored, and %DELETE-FIXTURE removes it (and its .fasl) again.")

(defun %accessor-fixture-text (slot-option)
  "Return the source of CL-MCP-CLOS-ACCESSOR-E2E-FIXTURE with GAUGE's sole
slot carrying SLOT-OPTION -- \":accessor gauge-level\" or \":writer
gauge-level\".  Nothing outside that one option differs between the two
versions, and the DEFCLASS is the file's last form, so no other definition's
recorded offset moves."
  (format nil "~
;;;; Written by cl-mcp/tests/clos-describe-integration-test; deleted after.

(defpackage #:cl-mcp-clos-accessor-e2e-fixture
  (:use #:cl)
  (:export #:gauge #:gauge-level))

(in-package #:cl-mcp-clos-accessor-e2e-fixture)

(defclass gauge ()
  ((level :initarg :level ~A)))
"
          slot-option))

(defun %rewrite-keeping-write-date (path text)
  "Write TEXT to PATH, then restore PATH's access and modification times.
FILE-WRITE-DATE therefore never advances past the date SBCL recorded when it
compiled the old text, so CL-MCP/SRC/CODE-CORE:%SOURCE-STALE-P does not mark
the entries stale -- the cp -p, rsync -a, same-second-edit and clock-skew
case, in which source matching has to be right entirely on its own."
  (require :sb-posix)
  (let* ((stat (sb-posix:stat path))
         (atime (sb-posix:stat-atime stat))
         (mtime (sb-posix:stat-mtime stat)))
    (%write-text path text)
    (sb-posix:utimes path atime mtime)))

(defun %method-of-generic-function (methods generic-function)
  "Return the sole entry in METHODS whose GENERIC_FUNCTION display name is
GENERIC-FUNCTION, signalling an error naming the count when that is not
exactly one.  One :ACCESSOR option creates two generic functions, X and
(SETF X), and this test turns on telling them apart."
  (let ((matches (remove-if-not
                  (lambda (method)
                    (equal generic-function (gethash "generic_function" method)))
                  methods)))
    (assert (= 1 (length matches)) ()
            "expected exactly one method of ~S, found ~D"
            generic-function (length matches))
    (first matches)))

(deftest clos-describe-does-not-confirm-an-accessor-against-a-writer-only-slot
  (testing "a slot rewritten from :accessor x to :writer x stops confirming the
live (SETF X) writer, even when the file's write date never advanced"
    (unwind-protect
         (progn
           (%write-text *accessor-fixture-path*
                        (%accessor-fixture-text ":accessor gauge-level"))
           (%compile-and-load-path *accessor-fixture-path*)
           ;; :writer gauge-level defines the plain function GAUGE-LEVEL and
           ;; no (SETF GAUGE-LEVEL) at all, so the writer method still in the
           ;; image is a different definition from anything this file now
           ;; holds -- and nothing but the accessor's own SETF flag says so.
           (%rewrite-keeping-write-date
            *accessor-fixture-path* (%accessor-fixture-text ":writer gauge-level"))
           (let* ((*project-root* (system-source-directory :cl-mcp))
                  (report (%annotated-report "cl-mcp-clos-accessor-e2e-fixture:gauge"))
                  (class (gethash "class" report))
                  (writer (%method-of-generic-function
                           (%methods class)
                           "(SETF CL-MCP-CLOS-ACCESSOR-E2E-FIXTURE:GAUGE-LEVEL)")))
             (ok (%falsy-p (gethash "stale" class))
                 "the restored write date keeps this out of the stale shortcut")
             (ok (equal "matched" (gethash "source_match" class))
                 "the DEFCLASS is still the same class, so the file really was read")
             (ok (not (equal "matched" (gethash "source_match" writer)))
                 ":writer gauge-level never defined (SETF GAUGE-LEVEL)")
             (ok (%unedited-p writer))
             (ok (null (gethash "edit_guard" writer))
                 "an entry with no edit information carries no guard either")))
      (%delete-fixture *accessor-fixture-path*))))

;;; ---------------------------------------------------------------------------
;;; Deftest 5: a quoted EQL keyword, judged over the real worker RPC
;;; ---------------------------------------------------------------------------

(defparameter *quoted-eql-fixture-path*
  (asdf/system:system-relative-pathname
   :cl-mcp "tests/tmp/clos-describe-quoted-eql-fixture.lisp")
  "A fourth scratch file, loaded only in a spawned worker and never edited.
It exists to put one quoted EQL specializer -- (eql ':ready), whose source
text reads as a symbol while the live specializer object is the keyword
:READY -- through the worker-proxied path CL-MCP/SRC/CLOS:CLOS-DESCRIBE
actually takes, which the in-process unit tests never cross.")

(defparameter *quoted-eql-fixture-text*
  "
;;;; Written by cl-mcp/tests/clos-describe-integration-test; deleted after.

(defpackage #:cl-mcp-clos-quoted-eql-fixture
  (:use #:cl)
  (:export #:advance))

(in-package #:cl-mcp-clos-quoted-eql-fixture)

(defgeneric advance (state))

(defmethod advance ((state (eql ':ready)))
  :running)
"
  "Compiled and loaded in the spawned worker only: CLOS-DESCRIBE-REPORT and
VERIFY-ENTRIES both run there, while the parent half of the flow reads this
file from disk rather than from any image.")

(deftest clos-describe-matches-a-quoted-eql-keyword-over-the-worker-rpc
  (testing "(eql ':ready) in source names the live :READY specializer, worker-proxied"
    (unless (spawn-available-p)
      (skip "ros not available"))
    (unwind-protect
         (progn
           (%write-text *quoted-eql-fixture-path* *quoted-eql-fixture-text*)
           (let ((*project-root* (system-source-directory :cl-mcp))
                 (*use-worker-pool* t)
                 (*current-session-id* "clos-describe-integration-quoted-eql"))
             (with-pool ()
               (let* ((truename (truename *quoted-eql-fixture-path*))
                      (load-code
                       (format nil "~
(with-compilation-unit (:override t :source-namestring (namestring (truename ~S)))
  (handler-bind ((warning (function muffle-warning)))
    (load (compile-file (truename ~S) :verbose nil :print nil))))
:loaded"
                               (namestring truename) (namestring truename)))
                      (load-result
                       (proxy-to-worker 1 "worker/eval"
                                        (make-ht "code" load-code "package" "CL-USER"))))
                 (ok (not (gethash "isError" load-result))
                     "the worker compiled and loaded the quoted-EQL fixture")
                 (let* ((report (%pool-report "cl-mcp-clos-quoted-eql-fixture:advance"))
                        (method (first (%methods (first (%gfs report)))))
                        (datum (gethash "datum"
                                        (aref (gethash "specializers"
                                                       (gethash "identity" method))
                                              0))))
                   (ok (equal "keyword" (gethash "kind" datum)))
                   (ok (equal "READY" (gethash "name" datum))
                       "the one method described is the :READY one")
                   (ok (equal "matched" (gethash "source_match" method))
                       "the quote is read through before the datum's kind is judged")
                   (ok (null (gethash "source_match_reason" method))
                       "a matched entry carries no reason")
                   (ok (equal "defmethod" (gethash "form_type" method)))
                   (ok (stringp (gethash "form_name" method))
                       "a matched entry hands out a form_name a mismatched one never would")
                   (ok (search "advance" (string-downcase
                                          (or (gethash "form_name" method) "")))
                       "and that form_name names this method"))))))
      (%delete-fixture *quoted-eql-fixture-path*))))

;;; ---------------------------------------------------------------------------
;;; Deftest 6: a hand-written method overriding a generated condition reader
;;; ---------------------------------------------------------------------------

(defparameter *reader-override-fixture-path*
  (asdf/system:system-relative-pathname
   :cl-mcp "tests/tmp/clos-describe-reader-override-fixture.lisp")
  "A fifth scratch file: a DEFINE-CONDITION with two readers, one of them
replaced by a hand-written DEFMETHOD.  SBCL leaves a single method on that
generic function and it is a plain STANDARD-METHOD -- precisely what a
genuine condition reader is, since SBCL never makes a DEFINE-CONDITION
reader a STANDARD-ACCESSOR-METHOD -- so nothing in the image says which of
the two definitions the live method came from, and only the source forms on
disk can settle it.")

(defparameter *reader-override-fixture-text*
  "
;;;; Written by cl-mcp/tests/clos-describe-integration-test; deleted after.

(defpackage #:cl-mcp-clos-reader-override-fixture
  (:use #:cl)
  (:export #:probe-failed #:probe-code #:probe-tag))

(in-package #:cl-mcp-clos-reader-override-fixture)

(define-condition probe-failed (error)
  ((code :initarg :code :reader probe-code)
   (tag :initarg :tag :reader probe-tag)))

;; Replaces the generated PROBE-CODE reader above; PROBE-TAG is left alone,
;; so this one file holds both definitions the report has to tell apart.
(defmethod probe-code ((e probe-failed))
  (max 0 (slot-value e 'code)))
"
  "Compiled and loaded once, never edited: this test is about telling two
kinds of definition apart in an unchanged file, not about a stale image.")

(deftest clos-describe-points-a-condition-reader-override-at-its-own-defmethod
  (unwind-protect
       (progn
         (%write-text *reader-override-fixture-path* *reader-override-fixture-text*)
         (%compile-and-load-path *reader-override-fixture-path*)
         (let ((*project-root* (system-source-directory :cl-mcp)))
           (testing "the overriding DEFMETHOD is the edit target, not the DEFINE-CONDITION
whose :reader option it replaced"
             (let* ((report (%annotated-report
                              "cl-mcp-clos-reader-override-fixture:probe-code"))
                    (methods (%methods (first (%gfs report))))
                    (method (first methods)))
               (ok (= 1 (length methods)) "the override replaced the generated reader")
               (ok (equal "reader" (gethash "access" (gethash "identity" method)))
                   "the image still reports it as an accessor; only the source decides")
               (ok (equal "matched" (gethash "source_match" method)))
               (ok (null (gethash "source_match_reason" method)))
               (ok (equal "defmethod" (gethash "form_type" method)))
               (ok (search "probe-code"
                           (string-downcase (or (gethash "form_name" method) "")))
                   "and the form_name names this method, not the condition")
               (ok (null (gethash "edit_unit" method))
                   "a DEFMETHOD is its own edit unit, so editing it changes nothing else")
               (ok (gethash "edit_guard" method) "a matched entry hands out a guard")))
           (testing "the untouched reader beside it still resolves to the DEFINE-CONDITION"
             (let* ((report (%annotated-report
                              "cl-mcp-clos-reader-override-fixture:probe-tag"))
                    (method (first (%methods (first (%gfs report))))))
               (ok (equal "matched" (gethash "source_match" method)))
               (ok (equal "define-condition" (gethash "form_type" method)))
               (ok (equal "define-condition" (gethash "edit_unit" method))
                   "editing there replaces the whole condition, as it always did")))))
    (%delete-fixture *reader-override-fixture-path*)))
