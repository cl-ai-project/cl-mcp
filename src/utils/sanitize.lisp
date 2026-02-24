;;;; src/utils/sanitize.lisp

(defpackage #:cl-mcp/src/utils/sanitize
  (:use #:cl)
  (:import-from #:cl-ppcre
                #:regex-replace-all)
  (:export #:sanitize-for-json
           #:sanitize-error-message))

(in-package #:cl-mcp/src/utils/sanitize)

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
