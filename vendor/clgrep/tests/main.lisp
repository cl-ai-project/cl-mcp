(defpackage clgrep/tests/main
  (:use :cl
        :clgrep
        :rove))
(in-package :clgrep/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :clgrep)' in your Lisp.

;;; Test data content for grep-file tests
(defparameter *test-data-content*
  "This is a test file
Hello world
Some other content
More lines here
Hello again
Testing patterns
Testing 123
Final line
")

(defmacro with-test-file ((var) &body body)
  "Create a temporary test file, bind its path to VAR, execute BODY, then clean up."
  `(let ((,var (merge-pathnames
                (format nil "clgrep-test-~A.txt" (get-universal-time))
                (uiop:temporary-directory))))
     (unwind-protect
         (progn
           (with-open-file (out ,var :direction :output :if-exists :supersede)
             (write-string *test-data-content* out))
           ,@body)
       (when (probe-file ,var)
         (delete-file ,var)))))

(deftest test-grep-file-basic-match
  (testing "should match simple string patterns"
    (with-test-file (test-file)
      (let ((result (with-output-to-string (*standard-output*)
                      (clgrep:grep-file "Hello" (namestring test-file)))))
        (ok (search "Hello world" result))
        (ok (search "Hello again" result))
        (ok (search ":2:" result))
        (ok (search ":5:" result))))))

(deftest test-grep-file-regex-pattern
  (testing "should match regex patterns"
    (with-test-file (test-file)
      (let ((result (with-output-to-string (*standard-output*)
                      (clgrep:grep-file "test.*file" (namestring test-file)))))
        (ok (search "This is a test file" result))
        (ok (search ":1:" result))))))

(deftest test-grep-file-digit-pattern
  (testing "should match digit patterns"
    (with-test-file (test-file)
      (let ((result (with-output-to-string (*standard-output*)
                      (clgrep:grep-file "\\d+" (namestring test-file)))))
        (ok (search "Testing 123" result))
        (ok (search ":7:" result))))))

(deftest test-grep-file-match-count
  (testing "should return correct match count"
    (with-test-file (test-file)
      (ok (= 2 (clgrep:grep-file "Hello" (namestring test-file))))
      (ok (= 2 (clgrep:grep-file "Testing" (namestring test-file))))
      (ok (= 1 (clgrep:grep-file "\\d+" (namestring test-file)))))))

(deftest test-grep-file-no-match
  (testing "should return 0 when no matches found"
    (with-test-file (test-file)
      (ok (= 0 (clgrep:grep-file "NOMATCH" (namestring test-file)))))))

(deftest test-grep-file-nonexistent-file
  (testing "should raise file-error for nonexistent file"
    (ok (signals (clgrep:grep-file "pattern" "/nonexistent/file.txt")
                 'file-error))))

(deftest test-extract-toplevel-form-basic
  (testing "should extract simple top-level forms with location info"
    (let ((content "(defun foo ()
  \"A simple function\"
  (+ 1 2))

(defun bar (x)
  ;; This is a comment
  (* x 2))"))
      (let ((result1 (clgrep:extract-toplevel-form content 2))
            (result2 (clgrep:extract-toplevel-form content 6)))
        ;; Check text content
        (ok (string= "(defun foo ()
  \"A simple function\"
  (+ 1 2))"
                     (cdr (assoc :text result1))))
        (ok (string= "(defun bar (x)
  ;; This is a comment
  (* x 2))"
                     (cdr (assoc :text result2))))
        ;; Check location info for first form
        (ok (= 1 (cdr (assoc :start-line result1))))
        (ok (= 3 (cdr (assoc :end-line result1))))
        (ok (= 0 (cdr (assoc :start-byte result1))))
        ;; Check location info for second form
        (ok (= 5 (cdr (assoc :start-line result2))))
        (ok (= 7 (cdr (assoc :end-line result2))))))))

(deftest test-extract-toplevel-form-with-strings
  (testing "should handle strings with escaped quotes"
    (let ((content "(defvar *config*
  '(:key \"value with \\\" quote\"
    :another-key 123))"))
      (let* ((result (clgrep:extract-toplevel-form content 2))
             (text (cdr (assoc :text result))))
        (ok (search "value with \\\" quote" text))
        (ok (search "*config*" text))
        ;; Check location info
        (ok (= 1 (cdr (assoc :start-line result))))
        (ok (= 3 (cdr (assoc :end-line result))))))))

(deftest test-extract-toplevel-form-truncation
  (testing "should truncate long forms and keep context around target line"
    (let* ((long-lines (loop for i from 1 to 100
                            collect (format nil "  (line-~D \"data-~D\")" i i)))
           (long-form (format nil "(defun huge-function ()~%~{~A~%~})" long-lines))
           (result (clgrep:extract-toplevel-form long-form 50))
           (text (cdr (assoc :text result))))
      ;; Should be truncated
      (ok (< (length text) (length long-form)))
      ;; Should contain the target line area
      (ok (search "line-50" text))
      ;; Should contain ellipsis markers
      (ok (search "..." text))
      ;; Should have location info
      (ok (= 1 (cdr (assoc :start-line result))))
      (ok (= 102 (cdr (assoc :end-line result)))))))

(deftest test-extract-toplevel-form-not-found
  (testing "should return NIL when target line is outside any form"
    (let ((content "(defun foo ()
  (+ 1 2))

(defun bar ()
  (* 3 4))"))
      ;; Line 3 is the empty line between the two forms
      (ok (null (clgrep:extract-toplevel-form content 3))))))

(deftest test-glob-to-regex
  (testing "should convert glob patterns to regex"
    (ok (string= "[^/]*\\.o" (clgrep::glob-to-regex "*.o")))
    (ok (string= "(?:.*/|)[^/]*\\.log" (clgrep::glob-to-regex "**/*.log")))
    (ok (string= "\\.git/.*" (clgrep::glob-to-regex ".git/")))
    (ok (string= "^root-only\\.txt" (clgrep::glob-to-regex "/root-only.txt")))))

(deftest test-collect-target-files
  (testing "should collect .lisp, .asd, and .ros files from project"
    (let ((files (clgrep:collect-target-files
                  (asdf:system-source-directory :clgrep))))
      ;; Should find at least the main source files
      (ok (>= (length files) 2))
      ;; All files should have target extensions
      (ok (every (lambda (f)
                   (member (pathname-type f) '("lisp" "asd" "ros")
                           :test #'string-equal))
                 files))
      ;; Should not include .git directory files
      (ok (notany (lambda (f)
                    (search ".git" (namestring f)))
                  files)))))

(deftest test-target-file-p
  (testing "should identify target file extensions"
    (ok (clgrep::target-file-p #P"/path/to/file.lisp"))
    (ok (clgrep::target-file-p #P"/path/to/system.asd"))
    (ok (clgrep::target-file-p #P"/path/to/script.ros"))
    (ok (not (clgrep::target-file-p #P"/path/to/file.txt")))
    (ok (not (clgrep::target-file-p #P"/path/to/file.fasl")))
    (ok (not (clgrep::target-file-p #P"/path/to/file")))))

(deftest test-path-ignored-p
  (testing "should match paths against ignore patterns"
    (let ((patterns (list "[^/]*\\.o"           ; *.o
                         "\\.git/.*"            ; .git/
                         "(?:.*/|)[^/]*\\.log"))) ; **/*.log
      (ok (clgrep::path-ignored-p #P"/project/test.o" #P"/project/" patterns))
      (ok (clgrep::path-ignored-p #P"/project/.git/config" #P"/project/" patterns))
      (ok (clgrep::path-ignored-p #P"/project/logs/app.log" #P"/project/" patterns))
      (ok (not (clgrep::path-ignored-p #P"/project/src/main.lisp" #P"/project/" patterns))))))

(deftest test-extract-package-for-line
  (testing "should extract package name from in-package forms"
    (let ((content "(uiop:define-package test
  (:use #:cl))
(in-package #:test)

(defun foo ()
  (+ 1 2))

(in-package :another-package)

(defun bar ()
  (* 3 4))"))
      (ok (string= "TEST" (clgrep::extract-package-for-line content 5)))
      (ok (string= "ANOTHER-PACKAGE" (clgrep::extract-package-for-line content 10)))
      ;; Before any in-package declaration
      (ok (null (clgrep::extract-package-for-line content 1))))))

(deftest test-semantic-grep
  (testing "should search across project files and return structured results"
    (let ((results (clgrep:semantic-grep
                    (asdf:system-source-directory :clgrep)
                    "defun grep-file")))
      ;; Should find at least the grep-file function
      (ok (>= (length results) 1))
      ;; Each result should have the required keys
      (let ((result (first results)))
        (ok (assoc :file result))
        (ok (assoc :line result))
        (ok (assoc :match result))
        (ok (assoc :package result))
        (ok (assoc :form result))
        ;; New location fields
        (ok (assoc :form-start-line result))
        (ok (assoc :form-end-line result))
        (ok (assoc :form-start-byte result))
        (ok (assoc :form-end-byte result))
        ;; Package should be CLGREP
        (ok (string= "CLGREP" (cdr (assoc :package result))))
        ;; Form should contain the function definition
        (ok (search "defun grep-file" (cdr (assoc :form result))))
        ;; Location sanity checks
        (ok (<= (cdr (assoc :form-start-line result))
                (cdr (assoc :line result))))
        (ok (>= (cdr (assoc :form-end-line result))
                (cdr (assoc :line result))))
        (ok (< (cdr (assoc :form-start-byte result))
               (cdr (assoc :form-end-byte result))))
        ;; Form type and name fields
        (ok (assoc :form-type result))
        (ok (assoc :form-name result))
        (ok (string= "defun" (cdr (assoc :form-type result))))
        (ok (string= "grep-file" (cdr (assoc :form-name result))))))))

(deftest test-extract-form-type-and-name
  (testing "should extract form type and name from various form types"
    ;; defun
    (let ((result (clgrep::extract-form-type-and-name "(defun foo (x) (+ x 1))")))
      (ok (string= "defun" (cdr (assoc :type result))))
      (ok (string= "foo" (cdr (assoc :name result)))))
    ;; defmethod with specializers
    (let ((result (clgrep::extract-form-type-and-name "(defmethod bar ((x string)) x)")))
      (ok (string= "defmethod" (cdr (assoc :type result))))
      (ok (string= "bar" (cdr (assoc :name result)))))
    ;; defvar
    (let ((result (clgrep::extract-form-type-and-name "(defvar *my-var* 42)")))
      (ok (string= "defvar" (cdr (assoc :type result))))
      (ok (string= "*my-var*" (cdr (assoc :name result)))))
    ;; defclass
    (let ((result (clgrep::extract-form-type-and-name "(defclass my-class () ())")))
      (ok (string= "defclass" (cdr (assoc :type result))))
      (ok (string= "my-class" (cdr (assoc :name result)))))
    ;; Non-recognized form
    (ok (null (clgrep::extract-form-type-and-name "(my-custom-form foo)")))
    ;; Non-form string
    (ok (null (clgrep::extract-form-type-and-name "just some text")))))

(deftest test-semantic-grep-form-type-filter
  (testing "should filter results by form type"
    ;; Search for all forms containing "defun" - should find defun forms
    (let ((results (clgrep:semantic-grep
                    (asdf:system-source-directory :clgrep)
                    "grep-file"
                    :form-types '("defun"))))
      (ok (>= (length results) 1))
      ;; All results should be defun forms
      (ok (every (lambda (r) (string= "defun" (cdr (assoc :form-type r))))
                 results)))
    ;; Search with non-matching form type should return empty
    (let ((results (clgrep:semantic-grep
                    (asdf:system-source-directory :clgrep)
                    "grep-file"
                    :form-types '("defclass"))))
      (ok (= 0 (length results))))))

(deftest test-extract-form-signature
  (testing "should extract signatures from various form types"
    ;; defun with parameters
    (ok (string= "(foo x y &key z)"
                 (clgrep:extract-form-signature "(defun foo (x y &key z) (+ x y z))")))
    ;; defun with no parameters
    (ok (string= "(bar)"
                 (clgrep:extract-form-signature "(defun bar () nil)")))
    ;; defmethod with specializers
    (ok (string= "(emit (logger json-logger) event &key stream)"
                 (clgrep:extract-form-signature "(defmethod emit ((logger json-logger) event &key stream) body)")))
    ;; defvar
    (ok (string= "*my-var*"
                 (clgrep:extract-form-signature "(defvar *my-var* 42)")))
    ;; defclass with superclass
    (ok (string= "(my-class parent-class)"
                 (clgrep:extract-form-signature "(defclass my-class (parent-class) ())")))
    ;; defclass without superclass
    (ok (string= "empty-class"
                 (clgrep:extract-form-signature "(defclass empty-class () ())")))
    ;; defstruct
    (ok (string= "my-struct"
                 (clgrep:extract-form-signature "(defstruct my-struct field1 field2)")))
    ;; Non-recognized form
    (ok (null (clgrep:extract-form-signature "(my-custom-form foo)")))))

(deftest test-semantic-grep-signature-field
  (testing "should include signature in results"
    (let ((results (clgrep:semantic-grep
                    (asdf:system-source-directory :clgrep)
                    "defun grep-file"
                    :form-types '("defun"))))
      (ok (>= (length results) 1))
      (let ((result (first results)))
        ;; Should have signature field
        (ok (assoc :signature result))
        ;; Signature should contain function name and parameters
        (ok (search "grep-file" (cdr (assoc :signature result))))))))

(deftest test-semantic-grep-include-form
  (testing "should omit form when :include-form is nil"
    (let ((results (clgrep:semantic-grep
                    (asdf:system-source-directory :clgrep)
                    "defun grep-file"
                    :form-types '("defun")
                    :include-form nil)))
      (ok (>= (length results) 1))
      (let ((result (first results)))
        ;; Should NOT have :form field
        (ok (null (assoc :form result)))
        ;; Should still have signature
        (ok (assoc :signature result)))))
  (testing "should include form when :include-form is t (default)"
    (let ((results (clgrep:semantic-grep
                    (asdf:system-source-directory :clgrep)
                    "defun grep-file"
                    :form-types '("defun")
                    :include-form t)))
      (ok (>= (length results) 1))
      (let ((result (first results)))
        ;; Should have :form field
        (ok (assoc :form result))
        ;; Should also have signature
        (ok (assoc :signature result))))))
