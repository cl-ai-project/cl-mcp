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
      (let ((text (bounded-output-string s)))
        (ok (search "16 total chars" text))
        ;; The note separates itself from retained text with a newline; with
        ;; nothing retained there is nothing to separate from, and a leading
        ;; blank line would read as output that was kept.
        (ok (not (eql #\Newline (char text 0)))
            "no leading blank line when nothing was kept")))))

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
  (testing "the column keeps tracking once nothing more is being kept"
    ;; Dropped writes still move the cursor the writer sees, so ~& has to make
    ;; the same decision after the limit as before it.  A write that straddles
    ;; the limit does not test this -- it still takes the keeping path -- so
    ;; the writes below go on until nothing at all is being kept.
    (let ((s (make-bounded-output-stream 5)))
      (write-string "0123456789" s)
      (ok (= 10 (sb-gray:stream-line-column s)) "a straddling write")
      (write-string "abcde" s)
      (ok (= 15 (sb-gray:stream-line-column s))
          "a write with nothing left to keep still advances the column")
      (write-char #\x s)
      (ok (= 16 (sb-gray:stream-line-column s))
          "and so does a character with nothing left to keep")
      (write-char #\Newline s)
      (ok (= 0 (sb-gray:stream-line-column s)))))
  (testing "a single write carrying several newlines lands on the last one"
    ;; Rove prints several lines per WRITE-STRING, so the column after such a
    ;; write is measured from the final newline, not the first.
    (let ((s (make-bounded-output-stream 1000)))
      (write-string "one
two
three!" s)
      (ok (= 6 (sb-gray:stream-line-column s)))))
  (testing "consecutive partial writes accumulate rather than replace"
    (let ((s (make-bounded-output-stream 1000)))
      (write-string "abc" s)
      (write-string "de" s)
      (ok (= 5 (sb-gray:stream-line-column s))))))

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

(deftest bounded-stream-transform-cannot-consume-its-own-note
  ;; TRANSFORM exists so a caller that rewrites the captured text cannot have
  ;; the rewriting swallow the note that says output went missing.  repl-eval
  ;; passes SANITIZE-FOR-JSON, and capture cut mid-escape-sequence leaves an
  ;; introducer whose terminator was dropped -- a sanitizer applied to the
  ;; composed string then eats everything after it, note included.
  (testing "the note is composed after the transform, not passed through it"
    (let ((s (make-bounded-output-stream 3)))
      (write-string "abcdefghij" s)
      ;; Stands in for a sanitizer that consumes a trailing introducer and
      ;; whatever follows it.
      (let ((text (bounded-output-string
                   s :transform (lambda (raw) (subseq raw 0 2)))))
        (ok (search "truncated" text)
            (format nil "the note survived the transform: ~S" text))
        (ok (string= "ab" (subseq text 0 2))
            "and the transform still applied to the retained text"))))
  (testing "the total counts what was captured, not what the transform left"
    ;; A transform that shortens the text must not shrink the reported total:
    ;; that number is the caller's only measure of how much was lost.
    (let ((s (make-bounded-output-stream 5)))
      (write-string (make-string 100 :initial-element #\x) s)
      (let* ((text (bounded-output-string
                    s :transform (lambda (raw) (declare (ignore raw)) "Z")))
             (marker (search "(truncated, " text))
             (total (and marker (parse-integer text :start (+ marker 12)
                                                    :junk-allowed t))))
        (ok (eql 100 total)
            (format nil "reported ~A, captured 100" total)))))
  (testing "and still counts it when the transform empties the text entirely"
    ;; Reachable from repl-eval: everything retained was an escape sequence,
    ;; so sanitizing leaves nothing.  Counting only the dropped characters
    ;; here understates the total by exactly what was retained.
    (let ((s (make-bounded-output-stream 5)))
      (write-string (make-string 100 :initial-element #\x) s)
      (let* ((text (bounded-output-string
                    s :transform (lambda (raw) (declare (ignore raw)) "")))
             (marker (search "(truncated, " text))
             (total (and marker (parse-integer text :start (+ marker 12)
                                                    :junk-allowed t))))
        (ok (eql 100 total)
            (format nil "reported ~A, captured 100" total))
        (ok (not (eql #\Newline (char text 0)))
            "with no leading blank line where the text would have been"))))
  (testing "the counters are reset before the transform runs"
    ;; TRANSFORM is arbitrary caller code.  One that signals must not leave the
    ;; stream drained but still counting its old total against the limit.
    (let ((s (make-bounded-output-stream 5)))
      (write-string "abcdefghij" s)
      (ignore-errors
       (bounded-output-string s :transform (lambda (raw)
                                             (declare (ignore raw))
                                             (error "transform failed"))))
      (write-string "xy" s)
      (ok (equal "xy" (bounded-output-string s))
          "the stream still accepts and retains after a failed transform"))))
