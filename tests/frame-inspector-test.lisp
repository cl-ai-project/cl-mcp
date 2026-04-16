;;;; tests/frame-inspector-test.lisp

(defpackage #:cl-mcp/tests/frame-inspector-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/frame-inspector
                #:capture-error-context))

(in-package #:cl-mcp/tests/frame-inspector-test)

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

(deftest capture-error-context-type-error
  (testing "captures type error details"
    (let ((ctx (handler-case
                   (+ "not a number" 1)
                 (error (e)
                   (capture-error-context e)))))
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

(deftest frame-source-location-returns-real-line-number
  #+sbcl
  (testing
   "frame :source-line is a real line number, not a small TLF-offset integer"
   ;; Regression guard: the previous %frame-source-location fell back to
   ;; sb-di:code-location-toplevel-form-offset (a TLF index like 0 or 1)
   ;; and presented it as a line number.  The current implementation
   ;; resolves TLF offset to a real character position via
   ;; debug-source-start-positions and converts to an actual line.
   ;; Pad the file so the defun starts beyond small TLF-offset integers.
   (let* ((path (format nil "/tmp/cl-mcp-frame-demo-~A.lisp"
                        (random 1000000)))
          (sym-name "CL-MCP-FRAME-DEMO-FN-XYZQ")
          captured)
     (unwind-protect
          (progn
            (with-open-file (s path :direction :output :if-exists :supersede)
              (format s "(in-package :cl-user)~%")
              (dotimes (i 8) (format s ";; padding line ~A~%" i))
              ;; Defun starts at line 10 (8 padding lines + line 1).
              (format s "(defun ~A ()~%" sym-name)
              (format s "  (declare (optimize (debug 3)))~%")
              (format s "  (error \"boom\"))~%"))
            (load path)
            (let ((fn (find-symbol sym-name :cl-user)))
              ;; handler-bind (not handler-case) so the stack is not
              ;; unwound before the capture runs.
              (block caught
                (handler-bind
                    ((error
                      (lambda (e)
                        (setf captured
                              (cl-mcp/src/frame-inspector:capture-error-context
                               e :max-frames 100))
                        (return-from caught))))
                  (funcall fn))))
            (let ((demo-frame
                   (find-if (lambda (f)
                              (search sym-name
                                      (or (getf f :function) "")))
                            (getf captured :frames))))
              (ok demo-frame "demo function frame was captured")
              (when demo-frame
                (let ((line (getf demo-frame :source-line))
                      (file (getf demo-frame :source-file)))
                  (ok (integerp line) "source-line is an integer")
                  (when (and (stringp file) (search "cl-mcp-frame-demo" file))
                    ;; Old code returned a TLF offset (0 or 1) here.
                    ;; New code returns the defun line (>= 10).
                    (ok (and line (>= line 10))
                        "source-line is the defun line, not a TLF offset"))))))
       (ignore-errors (delete-file path))
       (let ((sym (find-symbol sym-name :cl-user)))
         (when sym (unintern sym :cl-user)))))))
