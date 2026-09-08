;;;; tests/utils-bounded-stream-test.lisp
;;;;
;;;; The capture stream test output goes through.  Its job is to bound what a
;;;; run *holds*, not only what it reports: truncating a STRING-OUTPUT-STREAM
;;;; afterwards leaves the heap paying for everything the suite produced.

(defpackage #:cl-mcp/tests/utils-bounded-stream-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/utils/bounded-stream
                #:make-bounded-output-stream
                #:bounded-output-string
                #:bounded-output-dropped))

(in-package #:cl-mcp/tests/utils-bounded-stream-test)

(deftest bounded-stream-keeps-only-up-to-its-limit
  (testing "output under the limit is returned unchanged and unannotated"
    (let ((s (make-bounded-output-stream 100)))
      (write-string "hello" s)
      (let ((text (bounded-output-string s)))
        (ok (equal "hello" text))
        (ok (null (search "truncated" text))))))
  (testing "output past the limit is kept up to it and counted after it"
    (let ((s (make-bounded-output-stream 10)))
      (write-string (make-string 1000 :initial-element #\x) s)
      (ok (= 990 (bounded-output-dropped s)))
      (let ((text (bounded-output-string s)))
        (ok (equal (make-string 10 :initial-element #\x)
                   (subseq text 0 10))
            "the retained prefix is the first LIMIT characters")
        (ok (search "1000 total chars" text)
            "and the note reports the true total, not the retained length"))))
  (testing "a zero limit retains nothing but still accepts writes"
    (let ((s (make-bounded-output-stream 0)))
      (write-string "dropped entirely" s)
      (ok (= 16 (bounded-output-dropped s)))
      (ok (search "16 total chars" (bounded-output-string s))))))

(deftest bounded-stream-bounds-every-write-path
  ;; WRITE-CHAR and WRITE-STRING reach different generic functions, and a
  ;; suite's output arrives through both -- FORMAT picks per directive.
  (testing "character-at-a-time writing is bounded"
    (let ((s (make-bounded-output-stream 5)))
      (dotimes (i 100) (write-char #\a s))
      (ok (= 95 (bounded-output-dropped s)))
      (ok (equal "aaaaa" (subseq (bounded-output-string s) 0 5)))))
  (testing "a bounded substring write counts only the substring"
    (let ((s (make-bounded-output-stream 3)))
      (write-string "0123456789" s :start 2 :end 6)
      (ok (= 1 (bounded-output-dropped s)) "four written, three kept")
      (ok (equal "234" (subseq (bounded-output-string s) 0 3)))))
  (testing "FORMAT output is bounded too"
    (let ((s (make-bounded-output-stream 20)))
      (dotimes (i 100) (format s "line ~D~%" i))
      (ok (plusp (bounded-output-dropped s)))
      (ok (search "total chars" (bounded-output-string s)))))
  (testing "the pretty printer can write to it"
    ;; STREAM-LINE-COLUMN returns NIL, which the printer must tolerate.
    (let ((s (make-bounded-output-stream 1000)))
      (let ((*print-pretty* t))
        (prin1 '(a (b (c (d (e (f (g (h (i (j)))))))))) s))
      (ok (plusp (length (bounded-output-string s)))))))

(deftest bounded-stream-drains-like-a-string-output-stream
  (testing "reading it twice yields the retained text once"
    (let ((s (make-bounded-output-stream 100)))
      (write-string "first" s)
      (ok (equal "first" (bounded-output-string s)))
      (ok (equal "" (bounded-output-string s)))))
  (testing "the drop count resets with it, so a later note is not inflated"
    (let ((s (make-bounded-output-stream 2)))
      (write-string "aaaaaa" s)
      (bounded-output-string s)
      (write-string "bb" s)
      (let ((text (bounded-output-string s)))
        (ok (equal "bb" text))
        (ok (null (search "truncated" text)))))))
