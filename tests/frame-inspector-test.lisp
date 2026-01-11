;;;; tests/frame-inspector-test.lisp

(defpackage #:cl-mcp/tests/frame-inspector-test
  (:use #:cl #:rove)
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
