(defpackage #:cl-mcp/tests/utils-system-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/utils/system
                #:fd-count))

(in-package #:cl-mcp/tests/utils-system-test)

(deftest fd-count-returns-nil-or-fixnum
  (testing "fd-count returns NIL or a non-negative integer"
    (let ((result (fd-count)))
      (ok (or (null result)
              (and (integerp result)
                   (>= result 0)))))))

(deftest fd-count-is-consistent
  (testing "fd-count returns consistent results on consecutive calls"
    (let ((first (fd-count))
          (second (fd-count)))
      ;; Both should have the same type (nil or integer)
      (ok (eql (null first) (null second)))
      ;; If both are integers, they should be close in value
      (when (and first second)
        (ok (<= (abs (- first second)) 5))))))

(deftest fd-count-does-not-signal
  (testing "fd-count never signals an error"
    (ok (progn
          (fd-count)
          t))))
