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
  (testing "column-sensitive output matches a string-output-stream exactly"
    ;; Tolerating the stream is not enough -- it has to be faithful.
    ;; FRESH-LINE, ~T and the pretty printer all consult STREAM-LINE-COLUMN,
    ;; so a stream answering NIL silently rewrites what it captures: ~& emits
    ;; a newline even at the start of a line, ~T mis-tabulates, and lines wrap
    ;; as though every write began at column zero.  Comparing against the
    ;; stream this replaced is the only assertion that catches that.
    (flet ((render (make-stream)
             (let ((s (funcall make-stream)))
               (format s "first line~%")
               (format s "~&already at column zero~%")
               (write-string "mid-line" s)
               (format s "~&after a partial line~%")
               (format s "col:~10Tx~%")
               (write-string "PREFIX: " s)
               (let ((*print-pretty* t) (*print-right-margin* 20))
                 (prin1 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) s))
               s)))
      (let ((bounded (bounded-output-string
                      (render (lambda () (make-bounded-output-stream 10000)))))
            (reference (get-output-stream-string
                        (render #'make-string-output-stream))))
        (ok (equal reference bounded)
            (format nil "bounded=~S reference=~S" bounded reference)))))
  (testing "the column keeps tracking past the limit"
    ;; Dropped writes still move the cursor the writer sees, so ~& has to make
    ;; the same decision after the limit as before it.
    (let ((s (make-bounded-output-stream 5)))
      (write-string "0123456789" s)
      (ok (= 10 (sb-gray:stream-line-column s)))
      (write-char #\Newline s)
      (ok (= 0 (sb-gray:stream-line-column s))))))

(deftest bounded-stream-does-not-retain-what-it-drops
  (testing "writing far past the limit costs a fraction of what keeping it would"
    ;; The point of the stream, and the one property none of the assertions
    ;; above would notice: truncating a STRING-OUTPUT-STREAM afterwards gives
    ;; exactly the same reported text while the heap pays for everything the
    ;; writer produced.  Calibrated against that stream rather than a fixed
    ;; byte count, so the test says "bounded" rather than encoding one SBCL's
    ;; allocation behaviour.
    (let ((chunk (make-string 10000 :initial-element #\x))
          (reps 200))
      (flet ((consed (thunk)
               (sb-ext:gc :full t)
               (let ((before (sb-ext:get-bytes-consed)))
                 (funcall thunk)
                 (- (sb-ext:get-bytes-consed) before))))
        (let ((retaining (consed (lambda ()
                                   (let ((s (make-string-output-stream)))
                                     (dotimes (i reps) (write-string chunk s))
                                     (get-output-stream-string s)))))
              (bounding (consed (lambda ()
                                  (let ((s (make-bounded-output-stream 1000)))
                                    (dotimes (i reps) (write-string chunk s))
                                    (bounded-output-string s))))))
          (ok (< bounding (floor retaining 10))
              (format nil "~D bytes to bound ~D characters, against ~D to retain them"
                      bounding (* reps (length chunk)) retaining)))))))

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
