;;;; tests/clos-verify-core-test.lisp
;;;;
;;;; Tests for cl-mcp/src/clos-verify-core:verify-entries, judged against
;;;; real identities (tests/fixtures/clos-fixture.lisp,
;;;; tests/fixtures/clos-identity-fixture.lisp, task 1) and real source
;;;; signatures (cl-mcp/src/code-refs-scan:top-level-forms-at, task 2),
;;;; converted to the JSON candidate shape verify-entries documents.  Every
;;;; entries structure is round-tripped through YASON so booleans and
;;;; sequences arrive exactly as they do over the real worker wire.

(defpackage #:cl-mcp/tests/clos-verify-core-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/clos-verify-core
                #:verify-entries)
  (:import-from #:cl-mcp/src/clos-core
                #:clos-describe-report)
  (:import-from #:cl-mcp/src/code-refs-scan
                #:top-level-forms-at)
  (:import-from #:cl-mcp/src/code-refs-core
                #:sequence->list)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht))

(in-package #:cl-mcp/tests/clos-verify-core-test)

(defpackage #:cl-mcp-verify-shadow-test
  (:use #:cl)
  (:shadow #:defmethod #:quote)
  (:export #:quote))

(defvar *verify-core-side-effect-counter* 0)

;;; ---------------------------------------------------------------------------
;;; Fixture loading (mirrors cl-mcp/tests/clos-core-test's own loaders)
;;; ---------------------------------------------------------------------------

(defparameter *clos-fixture*
  (asdf/system:system-relative-pathname :cl-mcp "tests/fixtures/clos-fixture.lisp"))

(defparameter *identity-fixture*
  (asdf/system:system-relative-pathname :cl-mcp "tests/fixtures/clos-identity-fixture.lisp"))

(defparameter *accessor-fixture*
  (asdf/system:system-relative-pathname :cl-mcp "tests/fixtures/clos-accessor-fixture.lisp"))

(defun %compile-and-load (path)
  "Compile and load the fixture at PATH with its truename as the source
namestring, as cl-mcp/tests/clos-core-test's fixture loaders do."
  (let ((truename (truename path)))
    (uiop:with-temporary-file (:pathname fasl :type "fasl")
      (with-compilation-unit (:override t :source-namestring (namestring truename))
        (handler-bind ((warning #'muffle-warning))
          (load (compile-file truename :output-file fasl :verbose nil :print nil)))))))

(defun %load-clos-fixture ()
  "Compile and load tests/fixtures/clos-fixture.lisp."
  (%compile-and-load *clos-fixture*))

(defun %load-identity-fixture ()
  "Compile and load tests/fixtures/clos-identity-fixture.lisp."
  (%compile-and-load *identity-fixture*))

(defun %load-accessor-fixture ()
  "Compile and load tests/fixtures/clos-accessor-fixture.lisp."
  (%compile-and-load *accessor-fixture*))

(defun %line-of (path needle)
  "Return the 1-based line of the file at PATH on which NEEDLE starts."
  (let ((text (uiop:read-file-string path)))
    (1+ (count #\Newline text :end (search needle text)))))

(defun %report (designator &key (limit 50))
  "Return DESIGNATOR's clos-describe report with the project root bound."
  (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
    (clos-describe-report designator :limit limit)))

(defun %gfs (report)
  "Return REPORT's generic function entries as a list."
  (sequence->list (gethash "generic_functions" report)))

(defun %methods (entry)
  "Return ENTRY's method objects as a list."
  (sequence->list (gethash "methods" entry)))

(defun %identity (entry)
  "Return ENTRY's identity object."
  (gethash "identity" entry))

(defun %method-with-specializer-name (methods name)
  "Return the entry in METHODS whose first specializer is a named class
matching NAME exactly (case-sensitive), found by identity, not display
text."
  (find-if (lambda (method)
             (let ((specializer (first (sequence->list
                                        (gethash "specializers" (%identity method))))))
               (and specializer (equal "class" (gethash "kind" specializer))
                    (equal name (gethash "name" specializer)))))
           methods))

(defun %accessor-identity (report gf-name)
  "Return the identity of the sole method of REPORT's generic function whose
display name is GF-NAME.  X and (SETF X) are two different generic functions
one symbol can name at once, so they are told apart by name here, never by
their position in the report."
  (let ((gf (find gf-name (%gfs report)
                  :key (lambda (entry) (gethash "name" entry)) :test #'equal)))
    (assert gf () "no generic function named ~S in this report" gf-name)
    (%identity (first (%methods gf)))))

;;; ---------------------------------------------------------------------------
;;; Temp files for source signatures task 2's fixtures don't already cover
;;; ---------------------------------------------------------------------------

(defun %write-tmp (name text)
  "Write TEXT to tests/tmp/NAME in the cl-mcp source tree; return its
truename namestring."
  (let ((file (asdf/system:system-relative-pathname :cl-mcp (format nil "tests/tmp/~A" name))))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string text out))
    (namestring (truename file))))

;;; ---------------------------------------------------------------------------
;;; task 2 plist -> the JSON candidate shape verify-entries documents
;;; ---------------------------------------------------------------------------

(defun %json-token (plist)
  "Convert a source token PLIST (:token .. :in-package ..) to JSON."
  (and plist
       (make-ht "token" (getf plist :token) "in_package" (getf plist :in-package))))

(defun %json-tokens (plists)
  "Convert a list of token PLISTS to a JSON array."
  (map 'vector #'%json-token (sequence->list plists)))

(defun %json-name (plist)
  "Convert a source name PLIST (:token .. :setf .. :in-package ..) to JSON."
  (and plist
       (make-ht "token" (getf plist :token)
                "setf" (and (getf plist :setf) t)
                "in_package" (getf plist :in-package))))

(defun %json-names (plists)
  "Convert a list of source name PLISTS to a JSON array; a NIL element stays
NIL, the JSON null a name the scanner refused to guess at arrives as."
  (map 'vector #'%json-name (sequence->list plists)))

(defun %json-eql-datum (plist)
  "Convert a tagged EQL datum PLIST (spec 3.3) to JSON."
  (ecase (getf plist :kind)
    (:keyword (make-ht "kind" "keyword" "name" (getf plist :name)))
    (:integer (make-ht "kind" "integer" "value" (getf plist :value)))
    (:ratio (make-ht "kind" "ratio"
                     "numerator" (getf plist :numerator)
                     "denominator" (getf plist :denominator)))
    (:character (make-ht "kind" "character" "value" (getf plist :value)))
    (:boolean (make-ht "kind" "boolean" "value" (getf plist :value)))
    (:symbol
     (let ((ht (make-ht "kind" "symbol"
                        "token" (getf plist :token)
                        "in_package" (getf plist :in-package)
                        "quoted" (string-downcase (symbol-name (getf plist :quoted))))))
       (when (getf plist :quote-token)
         (setf (gethash "quote_token" ht) (%json-token (getf plist :quote-token))))
       ht))
    (:unverifiable (make-ht "kind" "unverifiable" "reason" (getf plist :reason)))))

(defun %json-specializer (plist)
  "Convert a specializer PLIST (spec 3.2/3.3) to JSON."
  (ecase (getf plist :kind)
    (:class (make-ht "kind" "class"
                     "token" (getf plist :token)
                     "in_package" (getf plist :in-package)))
    (:eql (make-ht "kind" "eql" "datum" (%json-eql-datum (getf plist :datum))))
    (:unverifiable (make-ht "kind" "unverifiable" "reason" (getf plist :reason)))))

(defun %json-specializers (plists)
  "Convert a list of specializer PLISTS to a JSON array, or NIL."
  (and plists (map 'vector #'%json-specializer (sequence->list plists))))

(defun %json-slot (plist)
  "Convert a slot PLIST (:name :readers :writers) to JSON.  Readers and
writers are function names, SETF flag included, not bare tokens."
  (make-ht "name" (%json-token (getf plist :name))
           "readers" (%json-names (getf plist :readers))
           "writers" (%json-names (getf plist :writers))))

(defun %json-method-option (plist)
  "Convert a defgeneric (:method ...) PLIST to JSON."
  (make-ht "qualifiers" (%json-tokens (getf plist :qualifiers))
           "specializers" (%json-specializers (getf plist :specializers))))

(defun %json-signature (plist)
  "Convert %DEFINITION-SOURCE-SIGNATURE's PLIST (spec 3.2), as
code-refs-scan.lisp builds it, to the candidate JSON shape
cl-mcp/src/clos-verify-core:verify-entries documents -- standing in for the
parent-side conversion a later task implements the same way."
  (let* ((kind (getf plist :kind))
         (ht (make-ht "kind" (string-downcase (symbol-name kind)))))
    (when (getf plist :head)
      (setf (gethash "head" ht) (%json-token (getf plist :head))))
    (case kind
      (:defmethod
        (setf (gethash "name" ht) (%json-name (getf plist :name))
              (gethash "qualifiers" ht) (%json-tokens (getf plist :qualifiers))
              (gethash "specializers" ht) (%json-specializers (getf plist :specializers))))
      (:defgeneric
        (setf (gethash "name" ht) (%json-name (getf plist :name))
              (gethash "methods" ht)
              (map 'vector #'%json-method-option (sequence->list (getf plist :methods)))))
      ((:defclass :define-condition)
        (setf (gethash "name" ht) (%json-name (getf plist :name))
              (gethash "slots" ht)
              (and (getf plist :slots)
                   (map 'vector #'%json-slot (sequence->list (getf plist :slots))))))
      (:defstruct
        (setf (gethash "name" ht) (%json-name (getf plist :name)))))
    ht))

(defun %candidates-at (path line)
  "Return the JSON candidates array for every top-level form starting on
LINE of the file at PATH (spec 3.4: the whole line's forms, not just the
first one)."
  (let ((*project-root* (asdf:system-source-directory :cl-mcp)))
    (multiple-value-bind (table failure) (top-level-forms-at path (list line))
      (assert (null failure) () "top-level-forms-at failed: ~A" failure)
      (map 'vector (lambda (form) (%json-signature (getf form :signature)))
           (gethash line table)))))

(defun %candidates-for (path needle)
  "Return the JSON candidates array for the form starting with NEEDLE in the
file at PATH."
  (%candidates-at path (%line-of path needle)))

(defun %first-eql-datum (candidates)
  "Return the tagged EQL datum of the first specializer of CANDIDATES' first
candidate -- the one place these tests reach into a real signature, to
simulate a token whose package this image does not have."
  (gethash "datum" (elt (gethash "specializers" (elt candidates 0)) 0)))

;;; ---------------------------------------------------------------------------
;;; verify-entries call helpers
;;; ---------------------------------------------------------------------------

(defun %entry (id identity candidates)
  "Build one VERIFY-ENTRIES input entry."
  (make-ht "id" id "identity" identity "candidates" candidates))

(defun %through-json (value)
  "Round-trip VALUE through YASON:ENCODE and YASON:PARSE, so a hand-built
test structure crosses the same JSON boundary VERIFY-ENTRIES sees from the
worker's wire in production -- in particular turning YASON:FALSE (as an
in-process CLOS-DESCRIBE-REPORT result carries it) into plain NIL, the
shape a real request carries after crossing the wire once."
  (yason:parse (with-output-to-string (out) (yason:encode value out))))

(defun %verify (entries)
  "Round-trip ENTRIES through JSON and return VERIFY-ENTRIES' results list."
  (sequence->list (gethash "results" (verify-entries (%through-json entries)))))

(defun %verify-raw (entries)
  "Call VERIFY-ENTRIES directly, with no JSON round trip -- for testing the
MCP_NO_WORKER_POOL=1 in-process path, where a CLOS-DESCRIBE-REPORT
identity's booleans may still be YASON:FALSE rather than plain NIL."
  (sequence->list (gethash "results" (verify-entries entries))))

(defun %verify1 (id identity candidates)
  "Call %VERIFY with a single ID/IDENTITY/CANDIDATES entry and return that
one result's \"status\"."
  (gethash "status" (first (%verify (vector (%entry id identity candidates))))))

;;; ---------------------------------------------------------------------------
;;; A plain defmethod
;;; ---------------------------------------------------------------------------

(deftest verify-entries-matches-a-plain-defmethod
  (testing "a defmethod's name, qualifiers and specializers all resolve to the live method"
    (%load-clos-fixture)
    (let* ((gf (first (%gfs (%report "cl-mcp-clos-fixture:area"))))
           (method (third (%methods gf))) ; order: around, eql-unit, circle, square
           (identity (%identity method))
           (path (namestring (truename *clos-fixture*)))
           (candidates (%candidates-for path "(defmethod area ((shape circle))"))
           (results (%verify (vector (%entry "e1" identity candidates)))))
      (ok (= 1 (length results)))
      (ok (equal "matched" (gethash "status" (first results))))
      (ok (null (gethash "reason" (first results))))
      (ok (= 0 (gethash "candidate_index" (first results)))))))

;;; ---------------------------------------------------------------------------
;;; Qualifiers: a keyword (:around) and a symbol (+)
;;; ---------------------------------------------------------------------------

(deftest verify-entries-resolves-qualifiers-by-package-keyword-or-symbol
  (testing "a keyword qualifier :around matches its own method, not the plain one"
    (%load-clos-fixture)
    (let* ((gf (first (%gfs (%report "cl-mcp-clos-fixture:area"))))
           (around-identity (%identity (first (%methods gf))))
           (plain-identity (%identity (third (%methods gf))))
           (path (namestring (truename *clos-fixture*)))
           (around-candidates (%candidates-for path "(defmethod area :around")))
      (ok (equal "matched" (%verify1 "e1" around-identity around-candidates)))
      (ok (equal "mismatched" (%verify1 "e2" plain-identity around-candidates))
          "the plain method's identity has no :around qualifier")))
  (testing "a symbol qualifier + from a method-combination protocol"
    (%load-clos-fixture)
    (let* ((gf (first (%gfs (%report "cl-mcp-clos-fixture:combine"))))
           (identity (%identity (first (%methods gf))))
           (path (namestring (truename *clos-fixture*)))
           (candidates (%candidates-for path "(defmethod combine + ((a integer)")))
      (ok (equal "matched" (%verify1 "e3" identity candidates))))))

;;; ---------------------------------------------------------------------------
;;; Same name, different package
;;; ---------------------------------------------------------------------------

(deftest verify-entries-rejects-a-generic-function-from-another-package
  (testing "package B's ACT does not match package A's ACT identity"
    (%load-identity-fixture)
    (let* ((identity (%identity (first (%gfs (%report "cl-mcp-identity-a:act")))))
           (path (%write-tmp "verify-core-other-package.lisp"
                             (format nil "~{~A~%~}"
                                    (list "(in-package #:cl-mcp-identity-b)"
                                          "(defgeneric act (x))"))))
           (candidates (%candidates-at path 2)))
      (unwind-protect
           (ok (equal "mismatched" (%verify1 "e1" identity candidates)))
        (ignore-errors (delete-file path))))))

;;; ---------------------------------------------------------------------------
;;; Escaped case: |Foo| vs |FOO|
;;; ---------------------------------------------------------------------------

(deftest verify-entries-distinguishes-escaped-symbol-case
  (testing "|Foo| and |FOO| resolve to distinct specializer identities"
    (%load-identity-fixture)
    (let* ((methods (%methods (first (%gfs (%report "cl-mcp-identity-a:act")))))
           (foo-identity (%identity (%method-with-specializer-name methods "Foo")))
           (foo-upper-identity (%identity (%method-with-specializer-name methods "FOO")))
           (path (namestring (truename *identity-fixture*)))
           (foo-candidates (%candidates-for path "(defmethod act ((x |Foo|))"))
           (foo-upper-candidates (%candidates-for path "(defmethod act ((x |FOO|))")))
      (ok foo-identity)
      (ok foo-upper-identity)
      (ok (equal "matched" (%verify1 "e1" foo-identity foo-candidates)))
      (ok (equal "matched" (%verify1 "e2" foo-upper-identity foo-upper-candidates)))
      (ok (equal "mismatched" (%verify1 "e3" foo-identity foo-upper-candidates))
          "the same name in a different case is a different symbol"))))

;;; ---------------------------------------------------------------------------
;;; EQL specializers: every tagged kind, plus quoting styles
;;; ---------------------------------------------------------------------------

(deftest verify-entries-judges-eql-data-by-kind
  (%load-identity-fixture)
  (let ((path (namestring (truename *identity-fixture*)))
        (methods (%methods (first (%gfs (%report "cl-mcp-identity-a:act"))))))
    (flet ((identity-at (needle)
             (%identity (find (%line-of *identity-fixture* needle) methods
                              :key (lambda (m) (gethash "line" m)))))
           (candidates-at (needle)
             (%candidates-for path needle)))
      (testing "a keyword literal matches, an integer identity does not"
        (ok (equal "matched"
                   (%verify1 "k1" (identity-at "(defmethod act ((x (eql :unit)))")
                             (candidates-at "(defmethod act ((x (eql :unit)))"))))
        (ok (equal "mismatched"
                   (%verify1 "k2" (identity-at "(defmethod act ((x (eql 3)))")
                             (candidates-at "(defmethod act ((x (eql :unit)))")))
            "a keyword candidate does not match an integer identity"))
      (testing "an integer literal matches"
        (ok (equal "matched"
                   (%verify1 "i1" (identity-at "(defmethod act ((x (eql 3)))")
                             (candidates-at "(defmethod act ((x (eql 3)))")))))
      (testing "a ratio matches"
        (ok (equal "matched"
                   (%verify1 "r1" (identity-at "(defmethod act ((x (eql 1/3)))")
                             (candidates-at "(defmethod act ((x (eql 1/3)))")))))
      (testing "characters match case-sensitively"
        (ok (equal "matched"
                   (%verify1 "c1" (identity-at "(defmethod act ((x (eql #\\A)))")
                             (candidates-at "(defmethod act ((x (eql #\\A)))"))))
        (ok (equal "matched"
                   (%verify1 "c2" (identity-at "(defmethod act ((x (eql #\\B)))")
                             (candidates-at "(defmethod act ((x (eql #\\B)))")))))
      (testing "T and NIL are boolean, distinct from each other"
        (ok (equal "matched"
                   (%verify1 "t1" (identity-at "(defmethod act ((x (eql t)))")
                             (candidates-at "(defmethod act ((x (eql t)))"))))
        (ok (equal "matched"
                   (%verify1 "t2" (identity-at "(defmethod act ((x (eql nil)))")
                             (candidates-at "(defmethod act ((x (eql nil)))"))))
        (ok (equal "mismatched"
                   (%verify1 "t3" (identity-at "(defmethod act ((x (eql t)))")
                             (candidates-at "(defmethod act ((x (eql nil)))")))))
      (testing "a reader-quoted interned symbol matches"
        (ok (equal "matched"
                   (%verify1 "s1" (identity-at "(defmethod act ((x (eql 'sym)))")
                             (candidates-at "(defmethod act ((x (eql 'sym)))")))))
      (testing "a string literal is unverifiable, never compared by content"
        (ok (equal "unverified"
                   (%verify1 "str1" (identity-at "(defmethod act ((x (eql \"str\")))")
                             (candidates-at "(defmethod act ((x (eql \"str\")))")))))
      (testing "a variable reference's evaluated value is never matched by the reference"
        (ok (equal "unverified"
                   (%verify1 "v1" (identity-at "(defmethod act ((x (eql *probe-var*)))")
                             (candidates-at "(defmethod act ((x (eql *probe-var*)))")))
            "the identity's evaluated value is 7 (an integer); the source form is a variable")))))

(deftest verify-entries-confirms-quote-as-reader-or-operator
  (%load-identity-fixture)
  (let* ((methods (%methods (first (%gfs (%report "cl-mcp-identity-a:act")))))
         (sym-line (%line-of *identity-fixture* "(defmethod act ((x (eql 'sym)))"))
         (sym-identity (%identity (find sym-line methods :key (lambda (m) (gethash "line" m))))))
    (testing "(quote sym), confirmed as CL:QUOTE, matches the same symbol as 'sym"
      (let* ((path (%write-tmp "verify-core-quote-operator.lisp"
                               (format nil "~{~A~%~}"
                                      (list "(in-package #:cl-mcp-identity-a)"
                                            "(defmethod act ((x (eql (quote sym)))) x)"))))
             (candidates (%candidates-at path 2)))
        (unwind-protect
             (ok (equal "matched" (%verify1 "q1" sym-identity candidates)))
          (ignore-errors (delete-file path)))))
    (testing "a shadowed (quote ...) operator cannot be confirmed as CL:QUOTE"
      (let* ((path (%write-tmp "verify-core-quote-shadowed.lisp"
                               (format nil "~{~A~%~}"
                                      (list "(in-package #:cl-mcp-verify-shadow-test)"
                                            "(defmethod act ((x (eql (quote sym)))) x)"))))
             (candidates (%candidates-at path 2)))
        (unwind-protect
             (ok (equal "unverified" (%verify1 "q2" sym-identity candidates)))
          (ignore-errors (delete-file path)))))))

(defun %act-identity (methods needle)
  "Return the identity of the method in METHODS defined on the line of
tests/fixtures/clos-identity-fixture.lisp on which NEEDLE starts."
  (%identity (find (%line-of *identity-fixture* needle) methods
                   :key (lambda (method) (gethash "line" method)))))

(deftest verify-entries-reads-a-quoted-keyword-t-or-nil-as-that-datum
  (%load-identity-fixture)
  (let ((methods (%methods (first (%gfs (%report "cl-mcp-identity-a:act")))))
        (path (%write-tmp "verify-core-quoted-constants.lisp"
                          (format nil "~{~A~%~}"
                                  (list "(in-package #:cl-mcp-identity-a)"
                                        "(defmethod act ((x (eql ':unit))) x)"
                                        "(defmethod act ((x (eql 't))) x)"
                                        "(defmethod act ((x (eql 'nil))) x)"
                                        "(defmethod act ((x (eql ':other))) x)")))))
    (unwind-protect
         (let ((unit (%act-identity methods "(defmethod act ((x (eql :unit)))"))
               (true (%act-identity methods "(defmethod act ((x (eql t)))"))
               (false (%act-identity methods "(defmethod act ((x (eql nil)))"))
               (three (%act-identity methods "(defmethod act ((x (eql 3)))")))
           (testing "a quoted keyword names the same datum as the unquoted spelling"
             (ok (equal "matched" (%verify1 "q1" unit (%candidates-at path 2)))))
           (testing "quoted T and NIL name the boolean methods they resolve to"
             (ok (equal "matched" (%verify1 "q2" true (%candidates-at path 3))))
             (ok (equal "matched" (%verify1 "q3" false (%candidates-at path 4)))))
           (testing "a resolved quoted datum still contradicts a different one"
             (ok (equal "mismatched" (%verify1 "q4" unit (%candidates-at path 5)))
                 "':other is a different keyword from :unit")
             (let ((result (first (%verify (vector (%entry "q5" three
                                                           (%candidates-at path 2)))))))
               (ok (equal "mismatched" (gethash "status" result))
                   "a quoted symbol is never EQL to an integer")
               (ok (equal "a symbol is never EQL to an integer, ratio or character"
                          (gethash "reason" result))
                   "and says so, rather than reusing the generic kind-mismatch reason"))
             (ok (equal "mismatched" (%verify1 "q6" false (%candidates-at path 3)))
                 "'t is not the method specialized on NIL")))
      (ignore-errors (delete-file path)))))

(deftest verify-entries-confirms-a-quoted-keywords-quote-before-judging-it
  (%load-identity-fixture)
  (let ((methods (%methods (first (%gfs (%report "cl-mcp-identity-a:act")))))
        (path (%write-tmp
               "verify-core-quoted-keyword-operator.lisp"
               (format nil "~{~A~%~}"
                       (list "(in-package #:cl-mcp-identity-a)"
                             "(defmethod act ((x (eql (quote :unit)))) x)"
                             (concatenate 'string
                                          "(defmethod act ((x (eql ("
                                          "cl-mcp-verify-shadow-test:quote :unit)))) x)")
                             "(defmethod act ((x (eql ':unit))) x)")))))
    (unwind-protect
         (let ((unit (%act-identity methods "(defmethod act ((x (eql :unit)))")))
           (testing "(quote :unit), confirmed as CL:QUOTE, is the keyword :unit"
             (ok (equal "matched" (%verify1 "o1" unit (%candidates-at path 2)))))
           (testing "a QUOTE shadowed by another package confirms nothing"
             (ok (equal "unverified" (%verify1 "o2" unit (%candidates-at path 3)))))
           (testing "a quoted token this image cannot resolve is unverified, not mismatched"
             (let ((candidates (%candidates-at path 4)))
               (setf (gethash "in_package" (%first-eql-datum candidates))
                     "cl-mcp-no-such-package-for-verify-core")
               (ok (equal "unverified" (%verify1 "o3" unit candidates))))))
      (ignore-errors (delete-file path)))))

;;; ---------------------------------------------------------------------------
;;; A shadowed defmethod head
;;; ---------------------------------------------------------------------------

(deftest verify-entries-treats-a-shadowed-defmethod-head-as-unverified
  (testing "DEFMETHOD spelled with a different package's own symbol is not the standard form"
    (%load-clos-fixture)
    (let* ((gf (first (%gfs (%report "cl-mcp-clos-fixture:describe-shape"))))
           (identity (%identity (first (%methods gf))))
           (path (%write-tmp "verify-core-shadowed-defmethod.lisp"
                             (format nil "~{~A~%~}"
                                    (list "(in-package #:cl-mcp-verify-shadow-test)"
                                          "(defmethod foo ((x integer)) x)"))))
           (candidates (%candidates-at path 2)))
      (unwind-protect
           (ok (equal "unverified" (%verify1 "s1" identity candidates)))
        (ignore-errors (delete-file path))))))

;;; ---------------------------------------------------------------------------
;;; defgeneric's inline (:method ...)
;;; ---------------------------------------------------------------------------

(deftest verify-entries-matches-exactly-one-inline-defgeneric-method
  (testing "the defgeneric's own EQL :unit method resolves through its enclosing form"
    (%load-clos-fixture)
    (let* ((gf (first (%gfs (%report "cl-mcp-clos-fixture:area"))))
           (unit-method (second (%methods gf))) ; around, eql-unit, circle, square
           (identity (%identity unit-method))
           (path (namestring (truename *clos-fixture*)))
           (candidates (%candidates-for path "(defgeneric area (shape)")))
      (ok (equal "matched" (%verify1 "g1" identity candidates)))))
  (testing "two inline methods that both match the identity make the defgeneric ambiguous"
    (%load-clos-fixture)
    (let* ((gf (first (%gfs (%report "cl-mcp-clos-fixture:area"))))
           (unit-method (second (%methods gf)))
           (identity (%identity unit-method))
           (path (%write-tmp "verify-core-defgeneric-ambiguous.lisp"
                             (format nil "~{~A~%~}"
                                    (list "(in-package #:cl-mcp-clos-fixture)"
                                          (concatenate 'string
                                           "(defgeneric area (shape) "
                                           "(:method ((shape (eql :unit))) 1) "
                                           "(:method ((shape (eql :unit))) 2))")))))
           (candidates (%candidates-at path 2)))
      (unwind-protect
           (ok (equal "unverified" (%verify1 "g2" identity candidates)))
        (ignore-errors (delete-file path))))))

;;; ---------------------------------------------------------------------------
;;; A plain defgeneric
;;; ---------------------------------------------------------------------------

(deftest verify-entries-matches-a-plain-defgeneric
  (testing "COMBINE's own DEFGENERIC form matches its generic-function identity"
    (%load-clos-fixture)
    (let* ((identity (%identity (first (%gfs (%report "cl-mcp-clos-fixture:combine")))))
           (path (namestring (truename *clos-fixture*)))
           (candidates (%candidates-for path "(defgeneric combine (a b)")))
      (ok (equal "matched" (%verify1 "gc1" identity candidates))))))

;;; ---------------------------------------------------------------------------
;;; Accessors
;;; ---------------------------------------------------------------------------

(deftest verify-entries-verifies-accessor-class-slot-and-generic-function
  (%load-clos-fixture)
  (let* ((gfs (%gfs (%report "cl-mcp-clos-fixture:radius")))
         (reader-identity (%identity (first (%methods (first gfs)))))
         (writer-identity (%identity (first (%methods (second gfs)))))
         (path (namestring (truename *clos-fixture*)))
         (candidates (%candidates-for path "(defclass circle (shape)")))
    (testing "the reader matches circle's :accessor radius slot"
      (ok (equal "matched" (%verify1 "r1" reader-identity candidates))))
    (testing "the writer matches the same slot's implicit SETF function"
      (ok (equal "matched" (%verify1 "r2" writer-identity candidates))))
    (testing "the same reader identity does not match SQUARE, which has no radius slot"
      (let ((square-candidates (%candidates-for path "(defclass square (shape)")))
        (ok (equal "mismatched" (%verify1 "r3" reader-identity square-candidates)))))))

(deftest verify-entries-matches-a-condition-readers-slot-and-generic-function
  (testing "PROBE-ERROR-CODE is not a standard-accessor-method in this SBCL, but its
identity now carries class/slot/access (task 10), so its own DEFINE-CONDITION
form matches"
    (%load-clos-fixture)
    (let* ((report (%report "cl-mcp-clos-fixture:probe-error-code"))
           (identity (%identity (first (%methods (first (%gfs report))))))
           (path (namestring (truename *clos-fixture*)))
           (candidates (%candidates-for path "(define-condition probe-error")))
      (ok (equal "reader" (gethash "access" identity)))
      (ok (equal "matched" (%verify1 "d1" identity candidates)))))
  (testing "a candidate whose slot is named differently does not match"
    (%load-clos-fixture)
    (let* ((report (%report "cl-mcp-clos-fixture:probe-error-code"))
           (identity (%identity (first (%methods (first (%gfs report))))))
           (path (%write-tmp
                  "verify-core-condition-slot-renamed.lisp"
                  (format nil "~{~A~%~}"
                         (list "(in-package #:cl-mcp-clos-fixture)"
                               (concatenate 'string
                                "(define-condition probe-error (error) "
                                "((other :initarg :code :reader probe-error-code)))")))))
           (candidates (%candidates-at path 2)))
      (unwind-protect
           (ok (equal "mismatched" (%verify1 "d2" identity candidates)))
        (ignore-errors (delete-file path)))))
  (testing "a candidate whose reader names a different generic function does not match"
    (%load-clos-fixture)
    (let* ((report (%report "cl-mcp-clos-fixture:probe-error-code"))
           (identity (%identity (first (%methods (first (%gfs report))))))
           (path (%write-tmp
                  "verify-core-condition-accessor-renamed.lisp"
                  (format nil "~{~A~%~}"
                         (list "(in-package #:cl-mcp-clos-fixture)"
                               (concatenate 'string
                                "(define-condition probe-error (error) "
                                "((code :initarg :code :reader shape-name)))")))))
           (candidates (%candidates-at path 2)))
      (unwind-protect
           (ok (equal "mismatched" (%verify1 "d3" identity candidates)))
        (ignore-errors (delete-file path))))))

(deftest verify-entries-keeps-an-accessors-setf-flag-part-of-its-identity
  (%load-accessor-fixture)
  (let* ((path (namestring (truename *accessor-fixture*)))
         (meter (%report "cl-mcp-clos-accessor-fixture:meter-level"))
         (meter-reader (%accessor-identity
                        meter "CL-MCP-CLOS-ACCESSOR-FIXTURE:METER-LEVEL"))
         (meter-writer (%accessor-identity
                        meter "(SETF CL-MCP-CLOS-ACCESSOR-FIXTURE:METER-LEVEL)"))
         (gauge-writer (%accessor-identity
                        (%report "cl-mcp-clos-accessor-fixture:gauge-level")
                        "CL-MCP-CLOS-ACCESSOR-FIXTURE:GAUGE-LEVEL"))
         (dial-writer (%accessor-identity
                       (%report "cl-mcp-clos-accessor-fixture:dial-level")
                       "(SETF CL-MCP-CLOS-ACCESSOR-FIXTURE:DIAL-LEVEL)")))
    (testing ":accessor x confirms both the plain x reader and the (setf x) writer"
      (let ((candidates (%candidates-for path "(defclass meter ()")))
        (ok (equal "matched" (%verify1 "sa1" meter-reader candidates)))
        (ok (equal "matched" (%verify1 "sa2" meter-writer candidates)))))
    (testing ":writer (setf x) confirms the (setf x) writer it really defines"
      (ok (equal "matched"
                 (%verify1 "sa3" dial-writer (%candidates-for path "(defclass dial ()")))))
    (testing ":writer x does not confirm a live (setf x) writer"
      (let ((candidate-path
              (%write-tmp "verify-core-accessor-plain-writer.lisp"
                          (format nil "~{~A~%~}"
                                 (list "(in-package #:cl-mcp-clos-accessor-fixture)"
                                       (concatenate 'string
                                        "(defclass meter () "
                                        "((level :initarg :level :writer meter-level)))"))))))
        (unwind-protect
             (ok (equal "mismatched"
                        (%verify1 "sa4" meter-writer (%candidates-at candidate-path 2))))
          (ignore-errors (delete-file candidate-path)))))
    (testing ":accessor x does not confirm a live plain x writer"
      (let ((candidate-path
              (%write-tmp "verify-core-accessor-setf-writer.lisp"
                          (format nil "~{~A~%~}"
                                 (list "(in-package #:cl-mcp-clos-accessor-fixture)"
                                       (concatenate 'string
                                        "(defclass gauge () "
                                        "((level :initarg :level :accessor gauge-level)))"))))))
        (unwind-protect
             (ok (equal "mismatched"
                        (%verify1 "sa5" gauge-writer (%candidates-at candidate-path 2))))
          (ignore-errors (delete-file candidate-path)))))
    (testing "a writer name this image cannot resolve is unverified, never mismatched"
      (let ((candidate-path
              (%write-tmp "verify-core-accessor-unknown-package.lisp"
                          (format nil "~{~A~%~}"
                                 (list "(in-package #:cl-mcp-clos-accessor-fixture)"
                                       (concatenate 'string
                                        "(defclass dial () ((level :initarg :level "
                                        ":writer (setf cl-mcp-no-such-pkg:dial-level))))"))))))
        (unwind-protect
             (ok (equal "unverified"
                        (%verify1 "sa6" dial-writer (%candidates-at candidate-path 2))))
          (ignore-errors (delete-file candidate-path)))))
    (testing "every writer resolving and none matching is a real mismatch"
      (let ((candidate-path
              (%write-tmp "verify-core-accessor-all-resolved.lisp"
                          (format nil "~{~A~%~}"
                                 (list "(in-package #:cl-mcp-clos-accessor-fixture)"
                                       (concatenate 'string
                                        "(defclass dial () ((level :initarg :level "
                                        ":writer (setf gauge-level) "
                                        ":writer (setf meter-level))))"))))))
        (unwind-protect
             (ok (equal "mismatched"
                        (%verify1 "sa7" dial-writer (%candidates-at candidate-path 2))))
          (ignore-errors (delete-file candidate-path)))))
    (testing "one unresolvable writer beside a conclusively different one is unverified"
      (let ((candidate-path
              (%write-tmp "verify-core-accessor-mixed-resolution.lisp"
                          (format nil "~{~A~%~}"
                                 (list "(in-package #:cl-mcp-clos-accessor-fixture)"
                                       (concatenate 'string
                                        "(defclass dial () ((level :initarg :level "
                                        ":writer (setf cl-mcp-no-such-pkg:dial-level) "
                                        ":writer (setf gauge-level))))"))))))
        (unwind-protect
             (ok (equal "unverified"
                        (%verify1 "sa8" dial-writer (%candidates-at candidate-path 2)))
                 "the unresolvable option might have been the definition, so nothing is certain")
          (ignore-errors (delete-file candidate-path)))))))

(deftest verify-entries-does-not-treat-a-qualified-accessor-shaped-identity-as-an-accessor
  (testing "an identity that carries class/slot/access AND a qualifier -- as
CLOS-CORE should never produce, but this file must not rely on that -- is judged as
a plain method, never verified against the class's own DEFCLASS form"
    (%load-clos-fixture)
    (let* ((gfs (%gfs (%report "cl-mcp-clos-fixture:radius")))
           (reader-identity (%identity (first (%methods (first gfs)))))
           (qualified-identity (%through-json reader-identity))
           (path (namestring (truename *clos-fixture*)))
           (candidates (%candidates-for path "(defclass circle (shape)")))
      (setf (gethash "qualifiers" qualified-identity)
            (vector (make-ht "package" "KEYWORD" "name" "BEFORE")))
      (ok (equal "reader" (gethash "access" qualified-identity))
          "confirms this identity still looks like an accessor apart from the qualifier")
      (ok (equal "unverified" (%verify1 "qa1" qualified-identity candidates))))))

;;; ---------------------------------------------------------------------------
;;; defstruct
;;; ---------------------------------------------------------------------------

(deftest verify-entries-matches-a-defstruct-class
  (testing "a DEFSTRUCT class identity matches its own form"
    (%load-clos-fixture)
    (let* ((identity (%identity (gethash "class" (%report "cl-mcp-clos-fixture:point"))))
           (path (namestring (truename *clos-fixture*)))
           (candidates (%candidates-for path "(defstruct point")))
      (ok (equal "matched" (%verify1 "st1" identity candidates))))))

;;; ---------------------------------------------------------------------------
;;; Never evaluates an EQL form's side effects
;;; ---------------------------------------------------------------------------

(deftest verify-entries-does-not-evaluate-an-eql-forms-side-effects
  (testing "an (eql (incf counter))-shaped candidate is judged from its tag alone, never run"
    (%load-clos-fixture)
    (setf *verify-core-side-effect-counter* 0)
    (let* ((gf (first (%gfs (%report "cl-mcp-clos-fixture:area"))))
           (identity (%identity (second (%methods gf))))
           (path (%write-tmp
                  "verify-core-eql-side-effect.lisp"
                  (format nil "~{~A~%~}"
                         (list "(in-package #:cl-mcp/tests/clos-verify-core-test)"
                               (concatenate 'string
                                "(defmethod cl-mcp-clos-fixture:area "
                                "((s (eql (incf *verify-core-side-effect-counter*)))) 1)")))))
           (candidates (%candidates-at path 2)))
      (unwind-protect
           (let ((status (%verify1 "n1" identity candidates)))
             (ok (= 0 *verify-core-side-effect-counter*) "the EQL form's side effect never ran")
             (ok (equal "unverified" status)))
        (ignore-errors (delete-file path))))))

;;; ---------------------------------------------------------------------------
;;; Never interns
;;; ---------------------------------------------------------------------------

(deftest verify-entries-never-interns-a-symbol
  (testing "an unknown token is judged without adding it to the package"
    (%load-clos-fixture)
    (let* ((gf (first (%gfs (%report "cl-mcp-clos-fixture:describe-shape"))))
           (identity (%identity (first (%methods gf)))))
      (ok (null (find-symbol "VERIFY-CORE-NEVER-INTERNED-XYZ" "CL-MCP-CLOS-FIXTURE"))
          "not present before")
      (let ((candidate (make-ht "kind" "defmethod"
                                "head" (make-ht "token" "defmethod"
                                                "in_package" "CL-MCP-CLOS-FIXTURE")
                                "name" (make-ht "token" "verify-core-never-interned-xyz"
                                               "setf" nil "in_package" "CL-MCP-CLOS-FIXTURE")
                                "qualifiers" (vector)
                                "specializers"
                                (vector (make-ht "kind" "class" "token" "shape"
                                                 "in_package" "CL-MCP-CLOS-FIXTURE")))))
        (ok (equal "unverified" (%verify1 "u1" identity (vector candidate)))
            "the specializer matches; only the unknown name is unresolved"))
      (ok (null (find-symbol "VERIFY-CORE-NEVER-INTERNED-XYZ" "CL-MCP-CLOS-FIXTURE"))
          "still not present after"))))

;;; ---------------------------------------------------------------------------
;;; Booleans that never cross a JSON wire (MCP_NO_WORKER_POOL=1, src/run.lisp)
;;; ---------------------------------------------------------------------------

(deftest verify-entries-treats-yason-false-as-false-without-a-json-round-trip
  (testing "a real non-setf method's identity carries YASON:FALSE, not NIL, in-process"
    (%load-clos-fixture)
    (let* ((gf (first (%gfs (%report "cl-mcp-clos-fixture:describe-shape"))))
           (identity (%identity (first (%methods gf))))
           (path (namestring (truename *clos-fixture*)))
           (candidates (%candidates-for path "(defmethod describe-shape")))
      (ok (eq 'yason:false (gethash "setf" (gethash "generic_function" identity)))
          "confirms the in-process shape this test relies on")
      (ok (equal "matched"
                 (gethash "status"
                          (first (%verify-raw (vector (%entry "y1" identity candidates))))))
          "a genuine non-setf method still matches, not \"(setf ...) status differs\"")))
  (testing "a real (setf x) generic function's identity carries T, and still matches"
    (%load-clos-fixture)
    (let* ((identity (%identity (first (%gfs (%report "cl-mcp-clos-fixture:label")))))
           (path (namestring (truename *clos-fixture*)))
           (candidates (%candidates-for path "(defgeneric (setf label)")))
      (ok (eq t (gethash "setf" (gethash "generic_function" identity))))
      (ok (equal "matched"
                 (gethash "status"
                          (first (%verify-raw (vector (%entry "y2" identity candidates))))))))))

;;; ---------------------------------------------------------------------------
;;; Malformed "candidates" or ENTRIES never crash the batch
;;; ---------------------------------------------------------------------------

(deftest verify-entries-treats-a-non-array-candidates-field-as-unverified
  (testing "a non-array \"candidates\" field is unverified, not a type-error crash"
    (%load-clos-fixture)
    (let* ((gf (first (%gfs (%report "cl-mcp-clos-fixture:describe-shape"))))
           (identity (%identity (first (%methods gf))))
           (entry (%entry "m1" identity "not-an-array")))
      (ok (equal "unverified" (%verify1 "m1" identity "not-an-array")))
      (ok (equal "unverified" (gethash "status" (first (%verify-raw (vector entry)))))))))

(deftest verify-entries-treats-a-non-array-entries-argument-as-empty
  (testing "a non-array top-level ENTRIES returns no results, without crashing"
    (ok (equalp #() (gethash "results" (verify-entries "not-an-array"))))))

;;; ---------------------------------------------------------------------------
;;; Combination rule (spec 3.1's tail rule, over multiple candidates)
;;; ---------------------------------------------------------------------------

(deftest verify-entries-combines-multiple-candidates-per-spec
  (%load-clos-fixture)
  (let* ((gf (first (%gfs (%report "cl-mcp-clos-fixture:area"))))
         (circle-identity (%identity (third (%methods gf))))
         (path (namestring (truename *clos-fixture*)))
         (circle-candidates (%candidates-for path "(defmethod area ((shape circle))"))
         (square-candidates (%candidates-for path "(defmethod area ((shape square))")))
    (testing "no candidates at all is unverified, not matched"
      (ok (equal "unverified" (%verify1 "e1" circle-identity (vector)))))
    (testing "two candidates that both match are ambiguous, never matched"
      (let ((both (concatenate 'vector circle-candidates circle-candidates)))
        (ok (equal "unverified" (%verify1 "e2" circle-identity both)))))
    (testing "zero matches with a real contradiction is mismatched"
      (ok (equal "mismatched" (%verify1 "e3" circle-identity square-candidates))))))
