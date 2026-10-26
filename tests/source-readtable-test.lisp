(defpackage #:cl-mcp/tests/source-readtable-test
  (:use #:cl #:rove)
  (:import-from #:cl-ppcre)
  (:import-from #:cl-mcp/src/cst)
  (:import-from #:cl-mcp/src/project-root)
  (:import-from #:cl-mcp/src/lisp-edit-form)
  (:import-from #:cl-mcp/src/lisp-patch-form))

(in-package #:cl-mcp/tests/source-readtable-test)

(defparameter *fixture-path*
  (asdf:system-relative-pathname "cl-mcp" "tests/fixtures/source-readtable.lisp"))

(defvar *source-reader-flag* nil)

(defstruct source-reader-probe value)

(deftest source-reader-does-not-call-structure-constructors
  (let ((original (symbol-function 'make-source-reader-probe))
        (*source-reader-flag* nil)
        (literal "#S(cl-mcp/tests/source-readtable-test::source-reader-probe :value 1)"))
    (unwind-protect
         (progn
           (setf (symbol-function 'make-source-reader-probe)
                 (lambda (&rest arguments)
                   (setf *source-reader-flag* t)
                   (apply original arguments)))
           ;; Positive control: ordinary CL:READ calls the constructor.
           (let ((*readtable* (copy-readtable nil))
                 (*read-eval* nil))
             (read-from-string literal))
           (ok *source-reader-flag*)
           (setf *source-reader-flag* nil)
           (dolist (readtable '(nil :standard))
             (dolist (source (list literal (concatenate 'string "#.(list " literal ")")))
               (multiple-value-bind (nodes reader-error)
                   (cl-mcp/src/cst:parse-top-level-forms source :readtable readtable)
                 (ok (null reader-error))
                 (ok (eq :expr (cl-mcp/src/cst:cst-node-kind (first nodes))))))
             (ok (null *source-reader-flag*))))
      (setf (symbol-function 'make-source-reader-probe) original))))

(deftest source-reader-rejects-malformed-dispatch-syntax
  (dolist (readtable '(nil :standard))
    (dolist (text '("#2.(values 1)" "#2S(foo)"
                   "#S42" "#S()" "#S(42 :slot 1)" "#S(foo :slot)"
                   "#S(foo 42 1)" "#S(foo . bar)" "#S#1=(foo . #1#)"))
      (ok (handler-case
              (nth-value 1 (cl-mcp/src/cst:parse-top-level-forms
                            text :readtable readtable))
            (error () t))
          (format nil "~S rejects ~A" readtable text)))))

(deftest source-reader-accepts-string-designators-and-suppressed-dispatch
  (let ((text "(list #S(probe :slot 1 \"OTHER\" 2 #\\X 3)
                      #+:cl-mcp-test-feature-that-does-not-exist #2.(values 4)
                      #+:cl-mcp-test-feature-that-does-not-exist #2S(probe)
                      :kept)"))
    (dolist (readtable '(nil :standard))
      (multiple-value-bind (nodes reader-error)
          (cl-mcp/src/cst:parse-top-level-forms text :readtable readtable)
        (ok (null reader-error))
        (let ((form (cl-mcp/src/cst:cst-node-value (first nodes))))
          (ok (= 3 (length form)))
          (ok (eq :kept (third form))))))))
(deftest source-reader-edits-preserve-read-time-source
  (let* ((cl-mcp/src/project-root:*project-root*
           (uiop:pathname-directory-pathname *fixture-path*))
         (before (uiop:read-file-string *fixture-path*))
         (after (cl-ppcre:regex-replace ":before" before ":after")))
    (uiop:with-temporary-file (:pathname temporary-path :stream stream
                               :directory cl-mcp/src/project-root:*project-root*
                               :type "lisp")
      (write-string before stream)
      (finish-output stream)
      (let ((path (namestring temporary-path)))
        (flet ((patch (dry-run)
                 (cl-mcp/src/lisp-patch-form:lisp-patch-form
                  :file-path path :form-type "defun" :form-name "source-reader-fixture"
                  :old-text ":before" :new-text ":after" :dry-run dry-run))
               (restore-form (dry-run)
                 (let* ((nodes (cl-mcp/src/cst:parse-top-level-forms before))
                        (node (find 'defun nodes
                                    :key (lambda (candidate)
                                           (let ((value (cl-mcp/src/cst:cst-node-value candidate)))
                                             (when (consp value) (first value)))))))
                   (cl-mcp/src/lisp-edit-form:lisp-edit-form
                    :file-path path :form-type "defun" :form-name "source-reader-fixture"
                    :operation "replace" :dry-run dry-run
                    :content (subseq before (cl-mcp/src/cst:cst-node-start node)
                                     (cl-mcp/src/cst:cst-node-end node))))))
          (patch t)
          (ok (string= before (uiop:read-file-string path)))
          (patch nil)
          (ok (string= after (uiop:read-file-string path)))
          (restore-form t)
          (ok (string= after (uiop:read-file-string path)))
          (restore-form nil)
          (ok (string= before (uiop:read-file-string path))))))))
