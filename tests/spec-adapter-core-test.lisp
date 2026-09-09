;;;; tests/spec-adapter-core-test.lisp
;;;;
;;;; Unit tests for cl-mcp/src/spec-adapter-core.  These must not require
;;;; cl-spec to be loadable: the adapter's whole point is that cl-mcp works
;;;; with and without it, and a suite that needs cl-spec cannot check the
;;;; "not loaded" branch at all.

(defpackage #:cl-mcp/tests/spec-adapter-core-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/spec-adapter-core
                #:resolve-symbol-designator
                #:symbol-data
                #:resolve-cl-spec-api
                #:make-cl-spec-api
                #:cl-spec-api-missing
                #:api-has-p
                #:api-fn
                #:api-class
                #:api-backend-available-p
                #:externalize-value
                #:digest-string
                #:printed-for-digest
                #:definition-digest))

(in-package #:cl-mcp/tests/spec-adapter-core-test)

(defun %ensure-fixture-packages ()
  "Create two packages that both export a symbol named FOO.
Same name, different home package: the pair a resolver must not confuse."
  (dolist (name '("CL-MCP-SPEC-FIXTURE-A" "CL-MCP-SPEC-FIXTURE-B"))
    (let ((package (or (find-package name) (make-package name :use '()))))
      (export (intern "FOO" package) package)
      (intern "HIDDEN" package))))

(deftest resolve-symbol-designator-distinguishes-packages
  (testing "the same symbol name in two packages resolves to two symbols"
    (%ensure-fixture-packages)
    (let ((a (resolve-symbol-designator "CL-MCP-SPEC-FIXTURE-A:FOO"))
          (b (resolve-symbol-designator "CL-MCP-SPEC-FIXTURE-B:FOO")))
      (ok (and a b))
      (ok (not (eq a b)))
      (ok (string= "CL-MCP-SPEC-FIXTURE-A" (package-name (symbol-package a))))
      (ok (string= "CL-MCP-SPEC-FIXTURE-B" (package-name (symbol-package b)))))))

(deftest resolve-symbol-designator-uses-package-argument
  (testing "an unqualified name resolves inside the package argument"
    (%ensure-fixture-packages)
    (let ((a (resolve-symbol-designator "foo" :package "cl-mcp-spec-fixture-a")))
      (ok a)
      (ok (string= "CL-MCP-SPEC-FIXTURE-A" (package-name (symbol-package a)))))))

(deftest resolve-symbol-designator-single-colon-requires-external
  (testing "PKG:SYM refuses an internal symbol and says so"
    (%ensure-fixture-packages)
    (multiple-value-bind (symbol reason)
        (resolve-symbol-designator "CL-MCP-SPEC-FIXTURE-A:HIDDEN")
      (ok (null symbol))
      (ok (eq :not-external (getf reason :reason))))
    (testing "PKG::SYM accepts it"
      (ok (resolve-symbol-designator "CL-MCP-SPEC-FIXTURE-A::HIDDEN")))))

(deftest resolve-symbol-designator-never-interns
  (testing "an unknown name is reported, not created"
    (%ensure-fixture-packages)
    (multiple-value-bind (symbol reason)
        (resolve-symbol-designator "CL-MCP-SPEC-FIXTURE-A::NO-SUCH-SYMBOL-HERE")
      (ok (null symbol))
      (ok (eq :symbol-not-found (getf reason :reason))))
    ;; The point of the test: the image must be unchanged afterwards.
    (ok (null (find-symbol "NO-SUCH-SYMBOL-HERE" "CL-MCP-SPEC-FIXTURE-A")))))

(deftest resolve-symbol-designator-missing-package
  (testing "an unknown package is reported by name"
    (multiple-value-bind (symbol reason)
        (resolve-symbol-designator "NO-SUCH-PACKAGE-CL-MCP:FOO")
      (ok (null symbol))
      (ok (eq :package-not-found (getf reason :reason)))
      (ok (string= "NO-SUCH-PACKAGE-CL-MCP" (getf reason :package))))))

(deftest resolve-symbol-designator-rejects-escapes
  (testing "escaped names are refused rather than mis-parsed"
    (multiple-value-bind (symbol reason)
        (resolve-symbol-designator "|weird name|")
      (ok (null symbol))
      (ok (eq :malformed (getf reason :reason))))))

(deftest symbol-data-shape
  (testing "a symbol externalizes as package, name and qualified name"
    (%ensure-fixture-packages)
    (let ((data (symbol-data (resolve-symbol-designator
                              "CL-MCP-SPEC-FIXTURE-A:FOO"))))
      (ok (string= "CL-MCP-SPEC-FIXTURE-A" (getf data :package)))
      (ok (string= "FOO" (getf data :name)))
      (ok (string= "CL-MCP-SPEC-FIXTURE-A::FOO" (getf data :qualified))))))

(deftest resolve-cl-spec-api-reports-absence
  (testing "a stub API built from lambdas answers api-has-p"
    (let ((api (make-cl-spec-api
                :functions (list :run-property (lambda (&rest ignored)
                                                 (declare (ignore ignored))
                                                 :called)
                                 :generator-backend (lambda () nil)))))
      (ok (api-has-p api :run-property))
      (ok (not (api-has-p api :semantic-data)))
      (ok (eq :called (funcall (api-fn api :run-property) 'x)))
      (ok (null (api-class api :unknown-property)))
      (testing "a NIL backend reads as unavailable, not as an error"
        (ok (not (api-backend-available-p api)))))))

(deftest resolve-cl-spec-api-live-image
  (testing "resolution against this image reports one of three states"
    (multiple-value-bind (api status) (resolve-cl-spec-api)
      (ok (member status '(:ok :not-loaded :incomplete)))
      (if (eq status :not-loaded)
          (ok (null api))
          (progn
            (ok api)
            ;; :INCOMPLETE must say which names were missing; a bare status
            ;; would leave a version mismatch undiagnosable.
            (ok (or (eq status :ok)
                    (plusp (length (cl-spec-api-missing api))))))))))

(deftest externalize-value-keeps-integers-exact
  (testing "a bignum beyond JSON's safe integer survives as text"
    (let ((data (externalize-value 3963993791726803706)))
      (ok (string= "3963993791726803706" (getf data :printed)))
      (ok (getf data :printed-complete))
      (ok (string= "integer" (getf data :type)))
      ;; A primitive is not registered for inspection: there is nothing to
      ;; drill into and an id would only invite a pointless round trip.
      (ok (null (getf data :object-id))))))

(deftest externalize-value-marks-truncation
  (testing "a value past max-chars is marked incomplete and counted"
    (let* ((big (make-list 400 :initial-element :aaaaaaaa))
           (data (externalize-value big :max-chars 40)))
      (ok (not (getf data :printed-complete)))
      (ok (plusp (getf data :omitted-chars)))
      (ok (string= "cons" (getf data :type)))
      (testing "a compound value gets an object id for inspect-object"
        (ok (integerp (getf data :object-id)))))))

(deftest externalize-value-handles-circularity
  (testing "a circular structure prints rather than hanging"
    (let ((cycle (list 1 2 3)))
      (setf (cdr (last cycle)) cycle)
      (let ((data (externalize-value cycle :max-chars 200)))
        (ok (stringp (getf data :printed)))
        (ok (search "#1=" (getf data :printed)))))))

(deftest digest-string-is-stable-and-sensitive
  (testing "the same text digests the same, different text does not"
    (ok (string= (digest-string "abc") (digest-string "abc")))
    (ok (not (string= (digest-string "abc") (digest-string "abd"))))
    (ok (= 16 (length (digest-string "abc"))))
    (ok (every (lambda (c) (find c "0123456789abcdef")) (digest-string "abc")))))

(deftest printed-for-digest-qualifies-symbols
  (testing "symbols print with their package regardless of *package*"
    (%ensure-fixture-packages)
    (let* ((symbol (resolve-symbol-designator "CL-MCP-SPEC-FIXTURE-A:FOO"))
           (in-cl-user (let ((*package* (find-package "COMMON-LISP-USER")))
                         (printed-for-digest (list symbol))))
           (in-fixture (let ((*package* (find-package "CL-MCP-SPEC-FIXTURE-A")))
                         (printed-for-digest (list symbol)))))
      (ok (string= in-cl-user in-fixture))
      (ok (search "CL-MCP-SPEC-FIXTURE-A" in-cl-user)))))

(deftest definition-digest-follows-spec-references
  (testing "a change in a referenced spec changes the property's digest"
    (let* ((property-plist
             (list :name 'prop :kind :invariant :targets '(add)
                   :arguments (list (list :variable 'a
                                          :spec (list :name nil :kind :reference
                                                      :target 'small-int)))
                   :body '((= a a))))
           (spec-v1 (list :name 'small-int :kind :range :min 0 :max 100))
           (spec-v2 (list :name 'small-int :kind :range :min 0 :max 999))
           (api-for (lambda (spec-plist)
                      (make-cl-spec-api
                       :functions
                       (list :property-data
                             (lambda (name &key registry)
                               (declare (ignore name registry))
                               property-plist)
                             :spec-data
                             (lambda (name &key registry)
                               (declare (ignore name registry))
                               spec-plist))))))
      (let ((d1 (definition-digest (funcall api-for spec-v1) 'prop nil))
            (d2 (definition-digest (funcall api-for spec-v2) 'prop nil)))
        (ok (stringp d1))
        (ok (not (string= d1 d2)))
        (testing "and the same inputs digest the same twice"
          (ok (string= d1 (definition-digest (funcall api-for spec-v1)
                                             'prop nil))))))))

(deftest definition-digest-tolerates-unresolved-reference
  (testing "a reference to a spec that is not registered does not signal"
    (let ((api (make-cl-spec-api
                :functions
                (list :property-data
                      (lambda (name &key registry)
                        (declare (ignore name registry))
                        (list :name 'prop
                              :arguments (list (list :variable 'a
                                                     :spec (list :kind :reference
                                                                 :target 'gone)))))
                      :spec-data
                      (lambda (name &key registry)
                        (declare (ignore registry))
                        (error "No spec named ~S is registered." name))))))
      (ok (stringp (definition-digest api 'prop nil))))))
