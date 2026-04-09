;;;; tests/project-scaffold-test.lisp

(defpackage #:cl-mcp/tests/project-scaffold-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/project-scaffold-core)
  (:import-from #:cl-mcp/src/project-scaffold)
  (:import-from #:cl-mcp/src/project-root))

(in-package #:cl-mcp/tests/project-scaffold-test)

(deftest project-scaffold-smoke
  (testing "project-scaffold-core package loads"
    (ok (find-package '#:cl-mcp/src/project-scaffold-core))))

(deftest validate-project-name-valid
  (testing "accepts valid names"
    (ok (cl-mcp/src/project-scaffold-core:validate-project-name "foo-lib"))
    (ok (cl-mcp/src/project-scaffold-core:validate-project-name "a"))
    (ok (cl-mcp/src/project-scaffold-core:validate-project-name "foo-123"))))

(deftest validate-project-name-rejects
  (testing "rejects empty string"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name "")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects uppercase"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name "FooLib")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects leading digit"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name "1foo")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects underscore"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name "foo_lib")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects slash"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name "foo/lib")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects overlong name"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name
                  (make-string 65 :initial-element #\a))
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects non-string"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-project-name :foo)
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error))))

(deftest validate-destination
  (testing "accepts a plain relative directory"
    (ok (cl-mcp/src/project-scaffold-core:validate-destination "scaffolds")))
  (testing "accepts a nested relative directory"
    (ok (cl-mcp/src/project-scaffold-core:validate-destination "work/samples")))
  (testing "rejects absolute path"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-destination "/tmp/foo")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects parent traversal"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-destination "../outside")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects embedded parent traversal"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-destination "scaffolds/../..")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects empty"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-destination "")
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error))))

(deftest validate-text-field
  (testing "accepts normal text"
    (ok (cl-mcp/src/project-scaffold-core:validate-text-field "author" "Ada Lovelace")))
  (testing "accepts empty string"
    (ok (cl-mcp/src/project-scaffold-core:validate-text-field "description" "")))
  (testing "rejects newline"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-text-field
                  "author" (format nil "Ada~%Lovelace"))
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects CR"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-text-field
                  "description" (format nil "foo~Abar" #\Return))
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
  (testing "rejects non-string"
    (ok (signals (cl-mcp/src/project-scaffold-core:validate-text-field "license" 42)
                 'cl-mcp/src/project-scaffold-core:invalid-argument-error))))

(deftest render-template
  (testing "substitutes a single placeholder"
    (ok (equal "hello foo"
               (cl-mcp/src/project-scaffold-core:render-template
                "hello {{name}}" '(("name" . "foo"))))))
  (testing "substitutes multiple placeholders"
    (ok (equal "foo by Ada (MIT)"
               (cl-mcp/src/project-scaffold-core:render-template
                "{{name}} by {{author}} ({{license}})"
                '(("name" . "foo") ("author" . "Ada") ("license" . "MIT"))))))
  (testing "leaves unknown placeholder unchanged"
    (ok (equal "hello {{unknown}}"
               (cl-mcp/src/project-scaffold-core:render-template
                "hello {{unknown}}" '(("name" . "foo"))))))
  (testing "handles repeated placeholder"
    (ok (equal "foo-foo"
               (cl-mcp/src/project-scaffold-core:render-template
                "{{name}}-{{name}}" '(("name" . "foo"))))))
  (testing "passes through regex-sensitive characters in values"
    (ok (equal "value: a.b*c"
               (cl-mcp/src/project-scaffold-core:render-template
                "value: {{v}}" '(("v" . "a.b*c"))))))
  (testing "leaves format directive ~A unchanged in value"
    (ok (equal "value: ~A done"
               (cl-mcp/src/project-scaffold-core:render-template
                "value: {{v}} done" '(("v" . "~A"))))))
  (testing "handles backslash in value"
    (ok (equal "value: a\\b"
               (cl-mcp/src/project-scaffold-core:render-template
                "value: {{v}}" '(("v" . "a\\b"))))))
  (testing "handles dollar sign in value"
    (ok (equal "value: $1.00"
               (cl-mcp/src/project-scaffold-core:render-template
                "value: {{v}}" '(("v" . "$1.00")))))))

(deftest compute-parent-prompts-path
  (testing "scaffolds destination with plain name"
    (ok (equal "../../prompts"
               (cl-mcp/src/project-scaffold-core:compute-parent-prompts-path
                "scaffolds" "foo-lib"))))
  (testing "two-segment destination"
    (ok (equal "../../../prompts"
               (cl-mcp/src/project-scaffold-core:compute-parent-prompts-path
                "work/samples" "foo-lib"))))
  (testing "three-segment destination"
    (ok (equal "../../../../prompts"
               (cl-mcp/src/project-scaffold-core:compute-parent-prompts-path
                "a/b/c" "foo-lib")))))

(deftest plan-scaffold
  (let ((plan (cl-mcp/src/project-scaffold-core:plan-scaffold
               :name "foo-lib"
               :description "A demo library"
               :author "Ada Lovelace"
               :license "MIT"
               :destination "scaffolds")))
    (testing "returns seven entries"
      (ok (= 7 (length plan))))
    (testing "each entry is (relative-path . content) of strings"
      (ok (every (lambda (entry)
                   (and (consp entry)
                        (stringp (car entry))
                        (stringp (cdr entry))))
                 plan)))
    (testing "asd file is keyed by <name>.asd and has substitutions"
      (let ((asd (cdr (assoc "foo-lib.asd" plan :test #'string=))))
        (ok asd)
        (ok (search "foo-lib" asd))
        (ok (search "Ada Lovelace" asd))
        (ok (search "A demo library" asd))
        (ok (search "MIT" asd))))
    (testing "CLAUDE.md has resolved parent-prompts path"
      (let ((md (cdr (assoc "CLAUDE.md" plan :test #'string=))))
        (ok md)
        (ok (search "../../prompts/repl-driven-development.md" md))))
    (testing "main.lisp uses the project-qualified package name"
      (let ((src (cdr (assoc "src/main.lisp" plan :test #'string=))))
        (ok src)
        (ok (search "#:foo-lib/src/main" src))))
    (testing "main-test.lisp imports the greet symbol"
      (let ((test (cdr (assoc "tests/main-test.lisp" plan :test #'string=))))
        (ok test)
        (ok (search "#:foo-lib/src/main" test))
        (ok (search "#:greet" test))))
    (testing "no unresolved placeholders remain"
      (dolist (entry plan)
        (ok (null (search "{{" (cdr entry))))))))

(deftest plan-scaffold-deeper-destination
  (let ((plan (cl-mcp/src/project-scaffold-core:plan-scaffold
               :name "demo"
               :description "d"
               :author "a"
               :license "MIT"
               :destination "work/samples")))
    (testing "parent-prompts resolves with extra ../"
      (let ((md (cdr (assoc "CLAUDE.md" plan :test #'string=))))
        (ok (search "../../../prompts/repl-driven-development.md" md))))))

(defmacro with-temp-project-root ((root-var) &body body)
  "Bind cl-mcp/src/project-root:*project-root* to a fresh temp directory.
Deletes the directory on exit. ROOT-VAR is always declared IGNORABLE so
callers do not need to reference it."
  `(let* ((,root-var
           (uiop:ensure-directory-pathname
            (uiop:merge-pathnames*
             (format nil "cl-mcp-scaffold-test-~A/" (random #xFFFFFFFF))
             (uiop:temporary-directory)))))
     (declare (ignorable ,root-var))
     (ensure-directories-exist ,root-var)
     (unwind-protect
          (let ((cl-mcp/src/project-root:*project-root* ,root-var))
            ,@body)
       (ignore-errors (uiop:delete-directory-tree ,root-var :validate t)))))

(deftest write-scaffold-creates-all-files
  (with-temp-project-root (root)
    (cl-mcp/src/project-scaffold:write-scaffold
     :name "foo-lib"
     :description "demo"
     :author "Ada"
     :license "MIT"
     :destination "scaffolds")
    (let ((target (uiop:merge-pathnames* "scaffolds/foo-lib/" root)))
      (testing "target directory exists"
        (ok (uiop:directory-exists-p target)))
      (testing "all seven files exist"
        (dolist (rel '("foo-lib.asd" "CLAUDE.md" "AGENTS.md" "README.md"
                       ".gitignore" "src/main.lisp" "tests/main-test.lisp"))
          (ok (probe-file (uiop:merge-pathnames* rel target))))))))

(deftest write-scaffold-rejects-existing-target
  (with-temp-project-root (root)
    (let ((target (uiop:merge-pathnames* "scaffolds/foo-lib/" root)))
      (ensure-directories-exist target)
      (testing "generating into existing dir errors"
        (ok (signals
             (cl-mcp/src/project-scaffold:write-scaffold
              :name "foo-lib" :description "d" :author "a" :license "MIT"
              :destination "scaffolds")
             'cl-mcp/src/project-scaffold-core:invalid-argument-error))))))

(deftest write-scaffold-validates-inputs
  (with-temp-project-root (root)
    (testing "invalid name errors"
      (ok (signals
           (cl-mcp/src/project-scaffold:write-scaffold
            :name "BadName" :description "d" :author "a" :license "MIT"
            :destination "scaffolds")
           'cl-mcp/src/project-scaffold-core:invalid-argument-error)))
    (testing "newline in author errors"
      (ok (signals
           (cl-mcp/src/project-scaffold:write-scaffold
            :name "ok" :description "d" :author (format nil "a~%b") :license "MIT"
            :destination "scaffolds")
           'cl-mcp/src/project-scaffold-core:invalid-argument-error)))))

(deftest write-scaffold-no-temp-dir-on-failure
  (with-temp-project-root (root)
    (ignore-errors
     (cl-mcp/src/project-scaffold:write-scaffold
      :name "BadName" :description "d" :author "a" :license "MIT"
      :destination "scaffolds"))
    (let ((scaffolds (uiop:merge-pathnames* "scaffolds/" root)))
      (testing "no .tmp-project-scaffold-* directory remains"
        (let ((remnants
               (when (uiop:directory-exists-p scaffolds)
                 (remove-if-not
                  (lambda (p)
                    (let ((last-seg (car (last (pathname-directory p)))))
                      (and (stringp last-seg)
                           (uiop:string-prefix-p ".tmp-project-scaffold-"
                                                 last-seg))))
                  (uiop:subdirectories scaffolds)))))
          (ok (null remnants)))))))

(deftest scaffold-e2e-load-and-test
  (with-temp-project-root (root)
    ;; The scaffold's CLAUDE.md uses ../../prompts/... which, relative to
    ;; root/scaffolds/foo-lib-e2e/, resolves to root/prompts/. Create stubs
    ;; so file-based @-includes would resolve if anything ever opened them.
    (let ((prompts (uiop:ensure-directory-pathname
                    (merge-pathnames "prompts/" root))))
      (ensure-directories-exist prompts)
      (with-open-file (s (merge-pathnames "repl-driven-development.md" prompts)
                         :direction :output :if-exists :supersede)
        (write-string "stub" s))
      (with-open-file (s (merge-pathnames "common-lisp-expert.md" prompts)
                         :direction :output :if-exists :supersede)
        (write-string "stub" s)))
    (cl-mcp/src/project-scaffold:write-scaffold
     :name "foo-lib-e2e"
     :description "e2e demo"
     :author "Test"
     :license "MIT"
     :destination "scaffolds")
    (let* ((asd-path (merge-pathnames "scaffolds/foo-lib-e2e/foo-lib-e2e.asd" root))
           (system-name "foo-lib-e2e")
           (test-system-name "foo-lib-e2e/tests"))
      (unwind-protect
           (progn
             (testing "asd is loadable"
               (ok (asdf:load-asd asd-path)))
             (testing "system loads cleanly"
               (ok (asdf:load-system system-name)))
             (testing "bundled test system loads"
               (ok (asdf:load-system test-system-name)))
             (testing "generated greet function works"
               (ok (equal "Hello, world!"
                          (funcall (find-symbol "GREET" "FOO-LIB-E2E/SRC/MAIN")
                                   "world")))))
        (ignore-errors (asdf:clear-system test-system-name))
        (ignore-errors (asdf:clear-system system-name))))))
