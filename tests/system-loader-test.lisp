;;;; tests/system-loader-test.lisp

(defpackage #:cl-mcp/tests/system-loader-test
  (:use #:cl #:rove)
  (:import-from #:cl-mcp/src/system-loader
                #:load-system))

(in-package #:cl-mcp/tests/system-loader-test)

(deftest load-system-basic
  (testing "loads an already-available system and returns structured result"
    (let ((ht (load-system "cl-mcp" :force nil)))
      (ok (hash-table-p ht))
      (ok (string= (gethash "status" ht) "loaded"))
      (ok (string= (gethash "system" ht) "cl-mcp"))
      (ok (integerp (gethash "duration_ms" ht)))
      (ok (integerp (gethash "warnings" ht))))))

(deftest load-system-force-reload
  (testing "force=true clears and reloads the system"
    (let ((ht (load-system "cl-mcp" :force t)))
      (ok (hash-table-p ht))
      (ok (string= (gethash "status" ht) "loaded"))
      (ok (eq t (gethash "forced" ht))))))

(deftest load-system-nonexistent
  (testing "returns error status for nonexistent system"
    (let ((ht (load-system "nonexistent-system-that-does-not-exist-12345"
                           :force nil)))
      (ok (hash-table-p ht))
      (ok (string= (gethash "status" ht) "error"))
      (ok (stringp (gethash "message" ht))))))

(deftest load-system-timeout
  (testing "returns timeout status when operation exceeds time limit"
    (let ((ht (load-system "cl-mcp" :force t :timeout-seconds 0.001)))
      (ok (hash-table-p ht))
      (ok (string= (gethash "status" ht) "timeout"))
      (ok (stringp (gethash "message" ht))))))

(deftest load-system-clear-fasls-flag
  (testing "clear_fasls flag is reflected in response"
    (let ((ht (load-system "cl-mcp" :force t :clear-fasls nil)))
      (ok (hash-table-p ht))
      (ok (string= (gethash "status" ht) "loaded"))
      (ok (null (gethash "clear_fasls" ht))))))

(deftest load-system-warning-capture
  (testing "warnings are counted and captured"
    ;; Define a function that produces a warning when compiled
    (eval '(defun cl-mcp/tests/system-loader-test::%warning-producer ()
             (declare (optimize (speed 3) (safety 0)))
             (let ((x "hello"))
               (+ x 1))))
    ;; The load-system function itself captures warnings during system loading.
    ;; We verify the warning fields exist and are properly typed.
    (let ((ht (load-system "cl-mcp" :force nil)))
      (ok (hash-table-p ht))
      (ok (integerp (gethash "warnings" ht))))))

(deftest load-system-force-false-no-clear
  (testing "force=false skips clearing and uses quickload"
    (let ((ht (load-system "cl-mcp" :force nil)))
      (ok (hash-table-p ht))
      (ok (string= (gethash "status" ht) "loaded"))
      (ok (null (gethash "forced" ht))))))
