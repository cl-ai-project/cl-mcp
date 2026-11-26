;;;; Loaded into a private registry by the around integration test.
(in-package #:cl-mcp/tests/spec-around-test)

(defmacro define-around-contract (name &key wrapper-error cleanup-error)
  `(progn
     (defun ,name (value) value)
     (cl-spec:defspec-function ,name
       (:args (value integer))
       (:fixture
         (:isolation :around)
         (:version 1)
         (:recipe (recipe (range integer 1 4)))
         (:setup (context) (list recipe))
         (:cleanup (context) ,(when cleanup-error '(error "Cleanup failed.")))
         (:around (trial) (funcall trial) ,(when wrapper-error '(error "Wrapper failed."))))
       (:cases (:positive (:when (plusp value)) (:returns integer))))))

(define-around-contract around-identity)
(define-around-contract around-wrapper-error :wrapper-error t)
(define-around-contract around-cleanup-error :cleanup-error t)
(define-around-contract around-programmatic)

(let ((fixture (make-instance 'cl-spec/src/fixture:trial-fixture
                              :recipe-name 'recipe
                              :recipe-spec (cl-spec:normalize-spec-form '(range integer 1 4))
                              :version 1 :isolation :around
                              :setup-function (lambda (recipe context)
                                                (declare (ignore context)) (list recipe))
                              :cleanup-function (lambda (recipe context)
                                                  (declare (ignore recipe context)))
                              :around-function #'funcall)))
  (reinitialize-instance (cl-spec:find-function-spec 'around-programmatic) :fixture fixture))
