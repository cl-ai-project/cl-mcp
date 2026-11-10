;;;; src/utils/sanitize.lisp

(defpackage #:cl-mcp/src/utils/sanitize
  (:use #:cl)
  (:import-from #:cl-ppcre
                #:regex-replace-all)
  (:import-from #:yason)
  (:import-from #:cl-mcp/src/utils/printing
                #:bounded-princ-to-string)
  (:export #:sanitize-for-json
           #:sanitize-error-message
           #:sanitize-condition-text
           #:transport-safe-json
           #:encode-json-line))

(in-package #:cl-mcp/src/utils/sanitize)

(defun %string-needs-sanitizing-p (string)
  "Return T if STRING contains any characters that sanitize-for-json would strip.
Fast pre-scan: iterates once looking for ESC, control chars, DEL, or
supplemental plane characters."
  (declare (type string string) (optimize (speed 3) (safety 1)))
  (loop for i fixnum from 0 below (length string)
        for code fixnum = (char-code (char string i))
        thereis (or (= code 27)        ; ESC
                    (and (< code 32)    ; Control chars
                         (not (= code 9))   ; except tab
                         (not (= code 10))  ; newline
                         (not (= code 13))) ; carriage return
                    (= code 127)       ; DEL
                    (> code #xFFFF)))) ; Supplemental plane

(defun sanitize-for-json (string)
  "Remove characters that are invalid in JSON strings.
Strips ANSI/ECMA-48 escape sequences (CSI, OSC, DCS, SOS, PM, APC, and
2-byte sequences like SS2/SS3), control characters (codes 0-31 except tab,
newline, carriage return), and the DEL character (code 127).
Returns NIL when given NIL. Non-string inputs are converted via
princ-to-string then sanitized."
  (when (null string) (return-from sanitize-for-json nil))
  (unless (stringp string)
    (return-from sanitize-for-json
      (sanitize-for-json (princ-to-string string))))
  ;; Fast path: if string has no problematic characters, return as-is
  (unless (%string-needs-sanitizing-p string)
    (return-from sanitize-for-json string))
  (let ((result
         (make-array (length string) :element-type 'character :fill-pointer 0
                     :adjustable t))
        (i 0)
        (len (length string)))
    (loop while (< i len)
          do (let* ((char (char string i))
                    (code (char-code char)))
               (cond
                ;; ESC (0x1B) — dispatch on the byte following ESC per ECMA-48:
                ;;   ESC [         CSI   — params + final byte (0x40-0x7E)
                ;;   ESC ]         OSC   — until BEL or ST (ESC \)
                ;;   ESC P/X/^/_   DCS/SOS/PM/APC — until ST (ESC \)
                ;;   ESC 0x40-0x5F other 2-byte (SS2, SS3, etc.)
                ;;   ESC + non-ASCII      — preserve char, strip ESC
                ;;   ESC + other ASCII    — strip ESC only
                ((= code 27)
                 (let ((next-i (1+ i)))
                   (if (>= next-i len)
                       ;; Bare ESC at end of string — strip it
                       (incf i)
                       (let* ((next-char (char string next-i))
                              (next-code (char-code next-char)))
                         (cond
                          ;; CSI: ESC [ — consume params + final byte (0x40-0x7E)
                          ;; Non-ASCII bytes (>= 0x80) are NOT CSI parameters per ECMA-48
                          ((char= next-char #\[)
                           (setf i (+ next-i 1))
                           (loop while (and (< i len)
                                            (let ((c (char-code (char string i))))
                                              (and (< c #x80)
                                                   (not (<= #x40 c #x7e)))))
                                 do (incf i))
                           ;; Skip the final byte only if it is actually a CSI final byte
                           (when (and (< i len)
                                      (let ((c (char-code (char string i))))
                                        (<= #x40 c #x7e)))
                             (incf i)))
                          ;; OSC: ESC ] — consume until BEL (0x07) or ST (ESC \)
                          ((char= next-char #\])
                           (setf i (+ next-i 1))
                           (loop while (< i len)
                                 do (cond
                                     ((= (char-code (char string i)) 7)
                                      (incf i) (return))
                                     ((and (= (char-code (char string i)) 27)
                                           (< (1+ i) len)
                                           (char= (char string (1+ i)) #\\))
                                      (incf i 2) (return))
                                     (t (incf i)))))
                          ;; DCS/SOS/PM/APC: ESC P, ESC X, ESC ^, ESC _
                          ;; Consume until ST (ESC \)
                          ((member next-char '(#\P #\X #\^ #\_))
                           (setf i (+ next-i 1))
                           (loop while (< i len)
                                 do (cond
                                     ((and (= (char-code (char string i)) 27)
                                           (< (1+ i) len)
                                           (char= (char string (1+ i)) #\\))
                                      (incf i 2) (return))
                                     (t (incf i)))))
                          ;; Other 2-byte sequences: ESC + byte in 0x40-0x5F
                          ;; Includes SS2 (ESC N), SS3 (ESC O), etc.
                          ((<= #x40 next-code #x5f)
                           (setf i (+ next-i 1)))
                          ;; Non-ASCII after ESC — preserve the non-ASCII char,
                          ;; strip only the ESC
                          ((>= next-code #x80)
                           (incf i))
                          ;; Unknown ASCII after ESC — strip ESC only
                          (t (incf i)))))))
                ;; Preserve allowed whitespace
                ((member char '(#\Tab #\Newline #\Return))
                 (vector-push-extend char result) (incf i))
                ;; Strip control characters (0-31)
                ((< code 32) (incf i))
                ;; Strip DEL (127)
                ((= code 127) (incf i))
                ;; Supplemental plane chars (above U+FFFF) crash yason:encode
                ;; on some streams — replace with U+FFFD REPLACEMENT CHARACTER
                ((> code #xFFFF)
                 (vector-push-extend (code-char #xFFFD) result) (incf i))
                ;; Pass through everything else
                (t (vector-push-extend char result) (incf i)))))
    (coerce result 'string)))

(defun sanitize-error-message (msg)
  "Sanitize an error message for external consumption.
Strips SBCL internal object representations (#<...>), multi-line
Stream: sections, and truncates to a reasonable length.  Returns a
clean string suitable for JSON-RPC error responses."
  (when (null msg) (return-from sanitize-error-message ""))
  (unless (stringp msg)
    (setf msg (princ-to-string msg)))
  ;; Remove #<...> object representations (SBCL stream objects, etc.)
  (setf msg (cl-ppcre:regex-replace-all "#<[^>]*>" msg ""))
  ;; Unwrap #P"..." pathname literals (SBCL file error messages) to show the actual path
  (setf msg (cl-ppcre:regex-replace-all "#P\"([^\"]*)\"" msg "\\1"))
  ;; Remove multi-line "Stream:" sections that SBCL appends
  (setf msg (cl-ppcre:regex-replace-all "(?s)\\s*Stream:.*" msg ""))
  ;; Collapse multiple spaces into one
  (setf msg (cl-ppcre:regex-replace-all "\\s+" msg " "))
  ;; Trim whitespace
  (setf msg (string-trim '(#\Space #\Tab #\Newline #\Return) msg))
  ;; Truncate to reasonable length
  (when (> (length msg) 500)
    (setf msg (concatenate 'string (subseq msg 0 497) "...")))
  msg)

(defun sanitize-condition-text (condition)
  "Return CONDITION's report text fit for a client. The implementation's own
END-OF-FILE (its exact type, not a subtype) is described as \"unexpected end
of input\": its report names the stream object, which SANITIZE-ERROR-MESSAGE
would strip down to a dangling \"end of file on\". A subtype with a report
of its own (cst's UNTERMINATED-SOURCE, which says what is still open) keeps
that text, and anything else goes through SANITIZE-ERROR-MESSAGE.
The report is printed by BOUNDED-PRINC-TO-STRING: a report can print an object
it was given, and one that is circular -- the reader's fixup node in the report
of a #S it could not build, say -- would otherwise print forever."
  (if (eq (type-of condition) 'end-of-file)
      "unexpected end of input"
      (sanitize-error-message (bounded-princ-to-string condition))))

(declaim (inline %unsendable-json-char-p))
(defun %unsendable-json-char-p (char)
  "True for a character that must not reach the transport as itself: a UTF-16
surrogate (U+D800-U+DFFF), or a control character below U+0020 other than tab,
newline and carriage return."
  (let ((code (char-code char)))
    (or (<= #xD800 code #xDFFF)
        (and (< code #x20) (/= code 9) (/= code 10) (/= code 13)))))

(defun transport-safe-json (json)
  "Return JSON, an encoded JSON text, with every character that would break
the transport rewritten; JSON itself when it holds none.

- A lone UTF-16 surrogate (U+D800-U+DFFF) becomes U+FFFD.  Yason escapes a
  character above U+FFFF as a \\uXXXX surrogate pair, which is plain ASCII, but
  writes a lone surrogate as the character itself -- and a UTF-8 stream cannot
  encode one.  The write then signals: a worker writing to the parent died, and
  the parent writing to a client broke the HTTP response, dropped the TCP
  connection, or ended the stdio loop.  A client's JSON \"\\uDFFF\" decodes to
  one, and so does (code-char #xDFFF) in evaluated code.
- A control character below U+0020 becomes its \\u00XX escape.  Yason 0.7.x
  escapes only \\b \\f \\n \\r \\t and writes the others raw, which no JSON
  parser accepts (RFC 8259 section 7): one NUL in a repl-eval result_preview made
  the client drop the connection (#342).  Yason 0.8 escapes them itself, so on
  it this finds nothing.  Tab, newline and carriage return are left alone: every
  yason escapes them inside a string, so raw they are whitespace between tokens.

Applied to the encoded text rather than by changing yason's escaping, so it
holds for every yason version and every field whatever produced it, and leaves
the yason that a user's own code runs in the worker as it was.  It is safe
because yason writes no other character below U+0020 and no surrogate outside a
JSON string, where an escape means the same character.

One scan when there is nothing to rewrite; otherwise the text before the first
such character is copied whole and only the rest is gone through."
  (let ((first (position-if #'%unsendable-json-char-p json)))
    (if (null first)
        json
        (with-output-to-string (out)
          (write-string json out :end first)
          (loop for index from first below (length json)
                for char = (char json index)
                for code = (char-code char)
                do (cond ((<= #xD800 code #xDFFF)
                          (write-char (code-char #xFFFD) out))
                         ((and (< code #x20) (/= code 9) (/= code 10) (/= code 13))
                          (format out "\\u~4,'0X" code))
                         (t (write-char char out))))))))

(defun encode-json-line (object)
  "Return OBJECT encoded by yason as one line of JSON that every transport can
carry: the encoded text through TRANSPORT-SAFE-JSON.  Every line cl-mcp sends --
to a client, between parent and worker, or to the log -- is made here (#344), so
no send site can leave the safe path out; the parent's answers to a client go
through %ENCODE-JSON in src/protocol.lisp, which adds a fallback for an object
yason cannot encode and ends in TRANSPORT-SAFE-JSON as well."
  (transport-safe-json (with-output-to-string (stream) (yason:encode object stream))))
