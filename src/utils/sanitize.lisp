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
           #:encode-json-text
           #:encode-json-line
           #:json-encoding-error
           #:json-number-p))

(in-package #:cl-mcp/src/utils/sanitize)

(defun %string-needs-sanitizing-p (string)
  "Return true if STRING contains a C0 control to filter or DEL."
  (declare (type string string) (optimize (speed 3) (safety 1)))
  (loop for i fixnum from 0 below (length string)
        for code fixnum = (char-code (char string i))
        thereis (or (and (< code 32)
                         (not (= code 9))   ; tab
                         (not (= code 10))  ; newline
                         (not (= code 13))) ; carriage return
                    (= code 127))))

(defun sanitize-for-json (string)
  "Filter terminal controls from STRING for display.
Strips ANSI/ECMA-48 escape sequences (CSI, OSC, DCS, SOS, PM, APC, and
2-byte sequences like SS2/SS3), C0 controls except tab, newline and carriage
return, and DEL. Preserves supplementary Unicode characters in display text.
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
                ;; Pass through everything else
                (t (vector-push-extend char result) (incf i)))))
    (coerce result 'string)))

(defun %after-printed-prefix (prefix text start)
  "Return the first index after PREFIX when it starts at START in TEXT."
  (let ((end (+ start (length prefix))))
    (and (<= end (length text)) (string-equal prefix text :start2 start :end2 end) end)))

(defun %printed-field-separator-p (ch)
  "True when CH separates values or introduces a quoted field in printed Lisp text."
  (or (find ch '(#\Space #\Tab #\Newline #\Return #\Page))
      (find ch "([{'`,:=<>#)")))

(defstruct (%printed-frame (:constructor %make-printed-frame (start finished-thread-p)))
  "A pending object and its position in the retained message."
  start
  finished-thread-p
  (list-depth 0)
  (tentatively-closed-p nil))

(setf (documentation '%printed-frame-start 'function)
      "Index of this object's opening # in the retained output."
      (documentation '%printed-frame-finished-thread-p 'function)
      "True when SBCL's finished-thread header identifies the enclosing value list."
      (documentation '%printed-frame-list-depth 'function)
      "Number of open Lisp list parentheses inside this printed object."
      (documentation '%printed-frame-tentatively-closed-p 'function)
      "True after the first possible short close has been applied to the output.")

(defun %printed-name-character-p (ch)
  (or (alphanumericp ch) (find ch "-+:<>=*/_&!%?$.")))

(defun %printed-character-end (message start)
  "Return the exclusive end of the character or character name at START."
  (if (alphanumericp (char message start))
      (or (position-if-not (lambda (ch) (or (alphanumericp ch) (find ch "-_+")))
                           message :start (1+ start))
          (length message))
      (1+ start)))

(defun %printed-function-end (message start address thread-p single-parent-p)
  "Return the end of a named function when its name or context identifies it.
Only look inside a bare name. Reader punctuation and a new #< start other values."
  (let* ((symbol-start (or (%after-printed-prefix "#:" message start) start))
         (end (or (position-if-not #'%printed-name-character-p message :start symbol-start)
                  (length message)))
         (angle (position #\> message :start symbol-start :end end)))
    (when angle
      (cond
        ;; A named function with an identity, e.g. #:NUMBER->TEXT {123}>.
        ((and (eql end (first address)) (char/= (char message (1- end)) #\>))
         (second address))
        ;; An initial > names the function, as in #<FUNCTION >> or #<FUNCTION >=>.
        ((or (= angle symbol-start) (char= (char message (1- angle)) #\:))
         (let ((close (position #\> message :start (1+ angle) :end end)))
           (when close (1+ close))))
        ;; A thread prints its return values before its own identity. A
        ;; bare function name can end at a comma, list delimiter or space.
        ;; Standalone, preserve ambiguous trailing diagnostic punctuation.
        ((and (char= (char message (1- end)) #\>)
              (or thread-p (and single-parent-p (eql end (first address)))))
         end)))))

(defun %opaque-printed-object-end (message start addresses parents)
  "Return the end when a printed name and its identity identify an entire object."
  (let ((identity (first addresses))
        (name (%after-printed-prefix "#<FUNCTION " message start)))
    (cond
      ((and identity (or (%after-printed-prefix "#<WAITQUEUE " message start)
                         (%after-printed-prefix "#<SB-THREAD:WAITQUEUE " message start)))
       ;; Waitqueue names are printed verbatim. A short close before a separate
       ;; printed object must not borrow that object's identity.
       (let ((close (position #\> message :start start :end identity)))
         (if (and close (%printed-field-separator-p (char message (1+ close)))
                  (search "#<" message :start2 (1+ close) :end2 identity))
             (1+ close)
             (second addresses))))
      (name
       (%printed-function-end message name addresses
                              (and parents (%printed-frame-finished-thread-p (first parents)))
                              (and parents (null (cdr parents)))))
      ;; The default CLOS printer writes a single type name before its identity.
      ;; A delimiter within that name, as in ARG->RESULT, belongs to the type.
      ((and identity (< (+ start 2) identity)
            (or (null parents) (char/= (char message (1- identity)) #\>))
            (loop for i from (+ start 2) below identity
                  for ch = (char message i)
                  always (or (%printed-name-character-p ch) (find ch "\"|\\"))))
       (second addresses)))))

(defun %finished-thread-p (message start)
  "Match SBCL's result header at START, examining only the type and optional name."
  (let ((name (or (%after-printed-prefix "#<THREAD " message start)
                  (%after-printed-prefix "#<SB-THREAD:THREAD " message start))))
    (when name
      (if (and (< name (length message)) (char= (char message name) #\"))
          (loop for i from (1+ name) below (length message)
                for ch = (char message i)
                do (case ch
                     (#\\ (incf i))
                     (#\" (return (%after-printed-prefix " FINISHED values:" message (1+ i))))))
          (%after-printed-prefix "FINISHED values:" message name)))))

(defun %strip-printed-objects (message)
  "Remove complete #<...> representations, preserving the surrounding diagnostic."
  (let ((first (search "#<" message)))
    (if (null first)
        message
        (let* ((end (length message))
               (out (make-array end :element-type 'character :fill-pointer first))
               (addresses (cl-ppcre:all-matches " \\{[0-9A-Fa-f]+\\}>" message :start first))
               (openings nil)
               (quote-delimiter nil)
               (field-end nil))
          (replace out message :end2 first)
          ;; Keep unfinished objects in OUT. Closing a frame drops only its own text.
          (loop for i from first below end
                for ch = (char message i)
                for frame = (first openings)
                for field-start-p = (and frame (or (eql i field-end)
                                                   (%printed-field-separator-p
                                                    (char message (1- i)))))
                do (vector-push ch out)
                   (loop while (and addresses (<= (second addresses) i))
                         do (setf addresses (cddr addresses)))
                   (cond
                     (quote-delimiter
                      (cond ((and (char= ch #\\) (< (1+ i) end))
                             (vector-push (char message (incf i)) out))
                            ((char= ch quote-delimiter)
                             (setf quote-delimiter nil field-end (1+ i)))))
                     ((and frame (char= ch #\\) (char= (char message (1- i)) #\#)
                           (< (1+ i) end))
                      (setf field-end (%printed-character-end message (1+ i)))
                      (loop while (< (1+ i) field-end)
                            do (vector-push (char message (incf i)) out)))
                     ((and frame (find ch "\"|")
                           (or field-start-p
                               ;; A printed pathname introduces its string with #P.
                               (and (char= ch #\") (>= i 2)
                                    (char-equal (char message (1- i)) #\P)
                                    (char= (char message (- i 2)) #\#))))
                      (setf quote-delimiter ch))
                     ((and (char= ch #\#) (< (1+ i) end) (char= (char message (1+ i)) #\<)
                           (or (null frame) field-start-p))
                      (let ((opaque-end (%opaque-printed-object-end message i addresses openings)))
                        (if opaque-end
                            (progn (decf (fill-pointer out)) (setf i (1- opaque-end)))
                            (push (%make-printed-frame (1- (fill-pointer out))
                                                       (%finished-thread-p message i))
                                  openings))))
                     ((and frame (char= ch #\() (or field-start-p
                                                    (plusp (%printed-frame-list-depth frame))))
                      (incf (%printed-frame-list-depth frame)))
                     ((and frame (char= ch #\)) (plusp (%printed-frame-list-depth frame)))
                      (decf (%printed-frame-list-depth frame))
                      (setf field-end (1+ i)))
                     ((and frame (char= ch #\>))
                      (cond ((or (eql (1+ i) (second addresses))
                                 (and (zerop (%printed-frame-list-depth frame))
                                      (not (%printed-frame-finished-thread-p frame))))
                             (pop openings)
                             (setf (fill-pointer out) (%printed-frame-start frame)))
                            ;; Keep text after the first possible short close. If a matching
                            ;; enclosing close arrives later, the same frame removes it.
                            ((not (%printed-frame-tentatively-closed-p frame))
                             (setf (fill-pointer out) (%printed-frame-start frame)
                                   (%printed-frame-tentatively-closed-p frame) t))))))
          out))))

(defun sanitize-error-message (msg)
  "Sanitize an error message for external consumption.
Strips SBCL internal object representations (#<...>), multi-line
Stream: sections, and truncates to a reasonable length.  Returns a
clean string suitable for JSON-RPC error responses."
  (when (null msg) (return-from sanitize-error-message ""))
  (unless (stringp msg)
    (setf msg (princ-to-string msg)))
  (setf msg (%strip-printed-objects msg))
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

One scan when there is nothing to rewrite, with the predicate open-coded (a
POSITION-IF would call it for every character); otherwise the text before the
first such character is copied whole and only the rest is gone through."
  (let ((first (loop for index from 0 below (length json)
                     when (%unsendable-json-char-p (char json index))
                       return index)))
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

(defun json-number-p (object)
  "True when OBJECT is a number yason writes as a JSON number: an integer, a
finite float, or a ratio whose double-float (what yason writes for it) is
finite.  JSON has no complex, infinity or NaN -- yason has no method for a
complex and writes the other two in Lisp syntax, which no parser reads -- and a
ratio past double range signals while it is written, so a preview gives each of
those as its printed text instead (#417)."
  (flet ((finite-p (float)
           (not (or (sb-ext:float-infinity-p float) (sb-ext:float-nan-p float)))))
    (typecase object
      (integer t)
      (float (finite-p object))
      (ratio (let ((double (ignore-errors (coerce object 'double-float))))
               (and double (finite-p double))))
      (t nil))))

(define-condition json-encoding-error (error)
  ((message :initarg :message :reader json-encoding-error-message))
  (:report (lambda (condition stream)
             (format stream "JSON encoding failed: ~A"
                     (json-encoding-error-message condition))))
  (:documentation "Signalled by ENCODE-JSON-TEXT when yason cannot encode a value.
Its message is text made while the failure was still in progress, so the
condition holds nothing yason was writing to."))

(defun encode-json-text (object)
  "Return OBJECT encoded by yason as JSON text, whatever printer settings the
calling thread meets, and signal JSON-ENCODING-ERROR when it cannot be encoded.

yason writes an integer with PRINC, so a *PRINT-BASE* or *PRINT-RADIX* that code
of the session set globally -- a test's (setf *print-base* 16) -- put 2A or #x2A
on the wire, and the parent killed the worker for it (#416).

yason's own error holds the string stream it was writing to, which
WITH-OUTPUT-TO-STRING allocates on the stack: printing that error once the
stream is gone, as every handler of it did, was a memory fault, and the worker
ended instead of answering (#417).  The error is turned into one holding only
its text while the stream still exists."
  (handler-bind ((error (lambda (condition)
                          (unless (typep condition 'json-encoding-error)
                            (error 'json-encoding-error
                                   :message (or (ignore-errors
                                                 (sanitize-condition-text condition))
                                                (string (type-of condition))))))))
    (let ((*print-base* 10)
          (*print-radix* nil))
      (with-output-to-string (stream) (yason:encode object stream)))))

(defun encode-json-line (object)
  "Return OBJECT encoded by yason as one line of JSON that every transport can
carry: ENCODE-JSON-TEXT's text through TRANSPORT-SAFE-JSON.  Every line cl-mcp
sends -- to a client, between parent and worker, or to the log -- is made here
(#344), so no send site can leave the safe path out; the parent's answers to a
client go through %ENCODE-JSON in src/protocol.lisp, which adds a fallback for
an object yason cannot encode and ends in TRANSPORT-SAFE-JSON as well."
  (transport-safe-json (encode-json-text object)))
