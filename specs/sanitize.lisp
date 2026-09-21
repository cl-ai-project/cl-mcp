;;;; specs/sanitize.lisp
;;;;
;;;; Contracts and properties for CL-MCP/SRC/UTILS/SANITIZE:SANITIZE-FOR-JSON and
;;;; CL-MCP/SRC/UTILS/SANITIZE:SANITIZE-ERROR-MESSAGE.
;;;;
;;;; These check the project's sanitization policy as the docstrings and the
;;;; implementation's own comments state it, not JSON or Unicode rules at large.
;;;;
;;;; Inputs are generated as data -- segment, escape-sequence and message
;;;; descriptors -- and rendered to a string inside the property, so a
;;;; counterexample prints as readable data rather than as a string of raw
;;;; control characters, which a client strips before showing it.
;;;;
;;;; SANITIZE-FOR-JSON, verified domain: NIL; integers; strings of up to eight
;;;; segments, each a run of allowed text (printable ASCII, a few non-ASCII BMP
;;;; characters, tab, newline, return), a forbidden C0 control or DEL, a
;;;; character above U+FFFF, a complete ECMA-48 sequence (CSI, OSC ended by BEL
;;;; or ST, DCS/SOS/PM/APC ended by ST, or a two-character ESC Fe), or a proper
;;;; prefix of one.  Not covered: other non-string objects, C1 controls,
;;;; surrogate code points, other Unicode ranges, and sequences whose bytes
;;;; stray outside ECMA-48's parameter, intermediate and final ranges.
;;;;
;;;; SANITIZE-ERROR-MESSAGE, verified domain: NIL; integers; messages of up to
;;;; ten words, #P"..." pathnames and #<...> object representations separated
;;;; by runs of space, tab, newline, return or page, optionally followed by a
;;;; "Stream:" section; single-spaced text of 0-40, 480-500 and 501-1500
;;;; characters; and strings of fragments that put those markers out of place.
;;;; Not covered: nested #<...> or ones containing ">", messages whose words
;;;; contain "Stream:" or "#", and whitespace outside cl-ppcre's \s class.

(defpackage #:cl-mcp/specs/sanitize
  (:use #:cl)
  (:import-from #:cl-spec/main
                #:defspec
                #:defspec-function
                #:defproperty
                #:defgenerator)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-for-json
                #:sanitize-error-message)
  (:import-from #:cl-mcp/specs/fixtures
                #:code-character
                #:*printable-ascii*
                #:*non-ascii-text*
                #:pick
                #:chance
                #:draw-string
                #:draw-allowed-text
                #:shrink-text-candidates)
  (:export #:register-specifications
           #:contract-names
           #:property-names
           #:spec-names
           #:generator-names
           #:call-examples
           #:json-policy-clean-p
           #:single-line-normalized-p
           #:render-escape-sequence
           #:render-segments
           #:render-message
           #:visible-text))

(in-package #:cl-mcp/specs/sanitize)

;;; ------------------------------------------------------------------------
;;; Policy predicates

(defun json-policy-clean-p (string)
  "True when STRING holds none of the characters SANITIZE-FOR-JSON's docstring
says it strips or replaces: C0 controls other than tab, newline and carriage
return (ESC among them), DEL, and characters above U+FFFF."
  (notany (lambda (character)
            (let ((code (char-code character)))
              (or (and (< code 32) (not (member code '(9 10 13))))
                  (= code 127)
                  (> code #xFFFF))))
          string))

(defun single-line-normalized-p (string)
  "True when STRING has no tab, newline, return or page character, no two
spaces in a row, and no space at either end."
  (let ((length (length string)))
    (and (notany (lambda (character)
                   (member character '(#\Tab #\Newline #\Return #\Page)))
                 string)
         (not (search "  " string))
         (or (zerop length)
             (and (char/= #\Space (char string 0))
                  (char/= #\Space (char string (1- length))))))))

;;; ------------------------------------------------------------------------
;;; ECMA-48 escape sequences, as data

(defparameter *escape* (code-character 27))

(defparameter *bell* (code-character 7))

(defun %code-range-string (from to &optional (excluded ""))
  "Return the characters with codes FROM through TO, minus those in EXCLUDED."
  (coerce (loop for code from from to to
                for character = (code-character code)
                unless (find character excluded)
                  collect character)
          'string))

(defparameter *csi-parameter-bytes* (%code-range-string #x30 #x3F)
  "ECMA-48 parameter bytes of a control sequence.")

(defparameter *csi-intermediate-bytes* (%code-range-string #x20 #x2F)
  "ECMA-48 intermediate bytes of a control sequence.")

(defparameter *csi-final-bytes* (%code-range-string #x40 #x7E)
  "ECMA-48 final bytes of a control sequence.")

(defparameter *fe-final-bytes* (%code-range-string #x40 #x5F "[]PX^_")
  "Second characters of a two-character ESC Fe sequence, minus the six that
open a control sequence or a control string instead.")

(defparameter *control-string-payload*
  (concatenate 'string *printable-ascii* *non-ascii-text*)
  "Characters a drawn OSC or DCS/SOS/PM/APC payload is made of.")

(defun draw-escape-sequence ()
  "Return a descriptor of a complete ECMA-48 sequence:
  (:CSI PARAMETERS INTERMEDIATES FINAL)  ESC [ ... final byte
  (:OSC PAYLOAD :BEL|:ST)                ESC ] ... ended by BEL or ESC \\
  (:CONTROL-STRING INTRODUCER PAYLOAD)   ESC P|X|^|_ ... ended by ESC \\
  (:FE FINAL)                            ESC and one character"
  (ecase (random 5)
    (0 (list :csi
             (draw-string *csi-parameter-bytes* (random 5))
             (draw-string *csi-intermediate-bytes* (random 2))
             (pick *csi-final-bytes*)))
    (1 (list :osc (draw-string *control-string-payload* (random 13)) :bel))
    (2 (list :osc (draw-string *control-string-payload* (random 13)) :st))
    (3 (list :control-string (pick "PX^_")
             (draw-string *control-string-payload* (random 13))))
    (4 (list :fe (pick *fe-final-bytes*)))))

(defun %string-terminator (terminator)
  "Return the characters that end a control string: BEL or ST (ESC \\)."
  (ecase terminator
    (:bel (string *bell*))
    (:st (coerce (list *escape* #\\) 'string))))

(defun render-escape-sequence (sequence)
  "Return the characters of the escape-sequence descriptor SEQUENCE."
  (destructuring-bind (kind &rest parts) sequence
    (ecase kind
      (:csi (destructuring-bind (parameters intermediates final) parts
              (format nil "~C[~A~A~C" *escape* parameters intermediates final)))
      (:osc (destructuring-bind (payload terminator) parts
              (format nil "~C]~A~A" *escape* payload (%string-terminator terminator))))
      (:control-string (destructuring-bind (introducer payload) parts
                         (format nil "~C~C~A~A" *escape* introducer payload
                                 (%string-terminator :st))))
      (:fe (format nil "~C~C" *escape* (first parts))))))

;;; ------------------------------------------------------------------------
;;; Mixed text for SANITIZE-FOR-JSON, as a list of segments

(defparameter *forbidden-control-codes*
  (append (loop for code from 0 below 32
                unless (member code '(9 10 13 27))
                  collect code)
          (list 127))
  "C0 controls other than tab, newline, return and ESC, and DEL.")

(defparameter *supplementary-codes* (list #x10000 #x1F600 #x10FFFF)
  "Code points above U+FFFF, the first, a common emoji and the last.")

(defun draw-segment ()
  "Return one segment descriptor:
  (:TEXT STRING) (:CONTROL CODE) (:SUPPLEMENTARY CODE)
  (:SEQUENCE ESCAPE-SEQUENCE) (:TRUNCATED ESCAPE-SEQUENCE LENGTH)
where LENGTH keeps a proper, non-empty prefix of the sequence."
  (let ((roll (random 100)))
    (cond ((< roll 40) (list :text (draw-allowed-text :max-length 8)))
          ((< roll 55) (list :control (pick *forbidden-control-codes*)))
          ((< roll 65) (list :supplementary (pick *supplementary-codes*)))
          ((< roll 90) (list :sequence (draw-escape-sequence)))
          (t (let* ((sequence (draw-escape-sequence))
                    (length (length (render-escape-sequence sequence))))
               (list :truncated sequence (1+ (random (1- length)))))))))

(defun render-segments (segments)
  "Return a fresh string made of the segment descriptors SEGMENTS."
  (with-output-to-string (out)
    (dolist (segment segments)
      (destructuring-bind (kind &rest parts) segment
        (ecase kind
          (:text (write-string (first parts) out))
          ((:control :supplementary) (write-char (code-character (first parts)) out))
          (:sequence (write-string (render-escape-sequence (first parts)) out))
          (:truncated (write-string (render-escape-sequence (first parts)) out
                                    :end (second parts))))))))

(defun %shrink-argument-list (arguments)
  "Return smaller one-element argument lists to try in place of ARGUMENTS when
shrinking a contract failure: substrings of a string, zero for another
integer, nothing for NIL.  The check-it backend calls a :SHRINK clause only on
a whole argument generator, which is why the contracts' generators carry one
and the properties' do not."
  (let ((value (first arguments)))
    (mapcar #'list
            (typecase value
              (string (shrink-text-candidates value))
              (integer (unless (zerop value) (list 0)))
              (t '())))))

;;; ------------------------------------------------------------------------
;;; Messages for SANITIZE-ERROR-MESSAGE

(defparameter *message-word-characters* "abcdefghijklmnopqrstuvwxyz0123456789.,;:()'/-_"
  "Characters of a drawn word.  No upper case, so no word can contain
\"Stream:\", and no #, so none can start a #<...> or #P\"...\".")

(defparameter *pathname-characters* "abcdefghijklmnopqrstuvwxyz0123456789/._-"
  "Characters of the path inside a drawn #P\"...\".")

(defparameter *message-whitespace*
  (coerce (list #\Space #\Tab #\Newline #\Return #\Page) 'string)
  "The whitespace cl-ppcre's \\s matches, which SANITIZE-ERROR-MESSAGE collapses.")

(defparameter *object-types*
  (list "SB-SYS:FD-STREAM" "HASH-TABLE" "STANDARD-OBJECT"
        "SB-IMPL::STRING-OUTPUT-STREAM" "FUNCTION FOO")
  "Type names a drawn #<...> representation shows.")

(defparameter *object-details*
  (list nil ":TEST EQUAL :COUNT 0" "for \"file /tmp/x.lisp\"" "RUNNING")
  "Text a drawn #<...> representation shows after its type, or none.")

(defun draw-normalized-text (length)
  "Return a fresh single-spaced string of exactly LENGTH word characters and
spaces, with no space at either end and no two in a row."
  (let ((string (make-string length)))
    (dotimes (index length string)
      (setf (char string index)
            (if (and (< 0 index (1- length))
                     (char/= #\Space (char string (1- index)))
                     (chance 15))
                #\Space
                (pick *message-word-characters*))))))

(defun draw-message ()
  "Return a message descriptor
  (:TOKENS TOKENS :SEPARATORS SEPARATORS :STREAM-SECTION STRING-OR-NIL)
where each token is (:WORD STRING), (:PATHNAME PATH) or
\(:OBJECT TYPE DETAIL ADDRESS), and SEPARATORS holds one more whitespace run
than there are tokens: a leading run, one non-empty run after each token but
the last, and a trailing run."
  (let* ((count (random 11))
         (tokens (loop repeat count
                       collect (let ((roll (random 100)))
                                 (cond ((< roll 60)
                                        (list :word (draw-string *message-word-characters*
                                                                 (1+ (random 12)))))
                                       ((< roll 80)
                                        (list :pathname
                                              (draw-string *pathname-characters*
                                                           (1+ (random 16)))))
                                       (t (list :object (pick *object-types*)
                                                (pick *object-details*)
                                                (random #x100000)))))))
         (separators (loop for index from 0 to count
                           collect (draw-string *message-whitespace*
                                                (if (< 0 index count)
                                                    (1+ (random 3))
                                                    (random 4))))))
    (list :tokens tokens
          :separators separators
          :stream-section (when (chance 30)
                            (format nil "Stream: #<SB-SYS:FD-STREAM for \"file /tmp/x\" ~
                                         {10017E0103}>~%  ~A"
                                    (draw-string *message-word-characters* 10))))))

(defun %render-token (token)
  "Return the characters of a message TOKEN."
  (destructuring-bind (kind &rest parts) token
    (ecase kind
      (:word (first parts))
      (:pathname (format nil "#P~S" (first parts)))
      (:object (destructuring-bind (type detail address) parts
                 (format nil "#<~A~@[ ~A~] {~X}>" type detail address))))))

(defun render-message (message)
  "Return a fresh string made of the message descriptor MESSAGE."
  (destructuring-bind (&key tokens separators stream-section) message
    (with-output-to-string (out)
      (write-string (first separators) out)
      (loop for token in tokens
            for separator in (rest separators)
            do (write-string (%render-token token) out)
               (write-string separator out))
      (when stream-section
        (write-string stream-section out)))))

(defun visible-text (message)
  "Return what a reader should see of MESSAGE: its words and the paths of its
pathnames, in order, one space apart, with object representations and the
Stream: section gone.  An independent statement of the policy, built from the
descriptor rather than from the rendered string."
  (format nil "~{~A~^ ~}"
          (loop for (kind value) in (getf message :tokens)
                unless (eq kind :object)
                  collect value)))

(defparameter *message-fragments*
  (list " " "  " (string #\Tab) (string #\Newline) (string #\Return) (string #\Page)
        "#<" ">" "#P\"" "\"" "Stream:" "word" "日本語" (string (code-character 7))
        (string (code-character 11)))
  "Pieces of error text that put SANITIZE-ERROR-MESSAGE's markers out of place:
unclosed #<, stray >, lone quotes, a Stream: anywhere, and controls it keeps.")

(defun draw-error-message-argument ()
  "Return one argument for SANITIZE-ERROR-MESSAGE: NIL, an integer, a rendered
message, long single-spaced text, or a string of *MESSAGE-FRAGMENTS*."
  (let ((roll (random 100)))
    (cond ((< roll 10) nil)
          ((< roll 20) (- (random 2000000) 1000000))
          ((< roll 55) (render-message (draw-message)))
          ((< roll 75) (draw-normalized-text (+ 480 (random 300))))
          (t (format nil "~{~A~}" (loop repeat (random 40)
                                        collect (pick *message-fragments*)))))))

;;; ------------------------------------------------------------------------
;;; Bundle interface

(defun contract-names ()
  "Return the functions this file puts a Function Spec on."
  '(sanitize-for-json sanitize-error-message))

(defun property-names ()
  "Return the properties this file defines."
  '(sanitize-for-json-keeps-allowed-text
    sanitize-for-json-is-idempotent
    sanitize-for-json-removes-complete-escape-sequences
    sanitize-for-json-removes-truncated-escape-sequence
    sanitize-for-json-leaves-its-argument-unmodified
    sanitize-error-message-keeps-normalized-text
    sanitize-error-message-truncates-long-text
    sanitize-error-message-keeps-only-visible-words))

(defun spec-names ()
  "Return the named data specs this file defines."
  '(allowed-text mixed-segments wrapped-sequence cut-sequence
    short-normalized-text long-normalized-text error-message))

(defun generator-names ()
  "Return the custom generators this file defines."
  '(allowed-text-generator mixed-segments-generator wrapped-sequence-generator
    cut-sequence-generator sanitize-for-json-arguments
    short-normalized-text-generator long-normalized-text-generator
    error-message-generator sanitize-error-message-arguments))

(defun call-examples ()
  "Return concrete calls, as (FUNCTION ARGUMENTS [CASE]), that CHECK-CALL runs
against the contracts apart from generated trials: NIL, the empty string and
an integer; a lone ESC, a lone BEL and a colored word; text above U+FFFF; and
single-spaced messages one below, at and one above the 500-character limit."
  (flet ((text (&rest codes-or-strings)
           (format nil "~{~A~}"
                   (mapcar (lambda (part)
                             (if (integerp part) (string (code-character part)) part))
                           codes-or-strings))))
    (list (list 'sanitize-for-json (list nil) :absent)
          (list 'sanitize-for-json (list "") :text)
          (list 'sanitize-for-json (list 42) :printed)
          (list 'sanitize-for-json (list (text 27)) :text)
          (list 'sanitize-for-json (list (text 7)) :text)
          (list 'sanitize-for-json (list (text 27 "[31mred" 27 "[0m")) :text)
          (list 'sanitize-for-json (list (text "日本語" #x1F600)) :text)
          (list 'sanitize-error-message (list nil))
          (list 'sanitize-error-message (list ""))
          (list 'sanitize-error-message (list 42))
          (list 'sanitize-error-message (list (make-string 499 :initial-element #\a)))
          (list 'sanitize-error-message (list (make-string 500 :initial-element #\a)))
          (list 'sanitize-error-message (list (make-string 501 :initial-element #\a)))
          (list 'sanitize-error-message (list (text " a " 9 " b " 10))))))

(defun register-specifications ()
  "Install this file's generators, specs, Function Specs and properties in
CL-SPEC:*REGISTRY*.  Registering again replaces each definition by name."
  ;; SANITIZE-FOR-JSON
  (defgenerator allowed-text-generator ()
    "Draw text SANITIZE-FOR-JSON must leave alone (see DRAW-ALLOWED-TEXT)."
    (draw-allowed-text :max-length 24))
  (defspec allowed-text string
    (:generator allowed-text-generator))
  (defgenerator mixed-segments-generator ()
    "Draw zero to eight segments (see DRAW-SEGMENT)."
    (loop repeat (random 9) collect (draw-segment)))
  (defspec mixed-segments list
    (:generator mixed-segments-generator))
  (defgenerator wrapped-sequence-generator ()
    "Draw (BEFORE SEQUENCE AFTER): a complete escape sequence between two runs
of allowed text."
    (list (draw-allowed-text :max-length 8)
          (draw-escape-sequence)
          (draw-allowed-text :max-length 8)))
  (defspec wrapped-sequence list
    (:generator wrapped-sequence-generator))
  (defgenerator cut-sequence-generator ()
    "Draw (BEFORE SEQUENCE LENGTH): allowed text and the number of characters
of a complete escape sequence that follow it, at least one and fewer than all."
    (let* ((sequence (draw-escape-sequence))
           (length (length (render-escape-sequence sequence))))
      (list (draw-allowed-text :max-length 8) sequence (1+ (random (1- length))))))
  (defspec cut-sequence list
    (:generator cut-sequence-generator))
  (defgenerator sanitize-for-json-arguments ()
    "Draw a one-element argument list: NIL, an integer, or rendered segments."
    (:shrink (arguments) (%shrink-argument-list arguments))
    (list (let ((roll (random 100)))
            (cond ((< roll 10) nil)
                  ((< roll 25) (- (random (expt 10 20)) (floor (expt 10 20) 2)))
                  (t (render-segments (loop repeat (random 9) collect (draw-segment))))))))
  (defspec-function sanitize-for-json
    "NIL gives NIL.  A string gives a string with none of the characters the
docstring says are stripped or replaced, and no longer than the argument.  An
integer gives its printed form, which never needs sanitizing.  Checked over
SANITIZE-FOR-JSON-ARGUMENTS; other non-string objects are outside this
contract."
    (:args (value (or null string integer)))
    (:args-generator sanitize-for-json-arguments)
    (:cases
     (:absent
      (:when (null value))
      (:returns null))
     (:text
      (:when (stringp value))
      (:returns string)
      (:post (and (json-policy-clean-p result)
                  (<= (length result) (length value)))))
     (:printed
      (:when (integerp value))
      (:returns string)
      (:post (string= result (princ-to-string value))))))
  (defproperty sanitize-for-json-keeps-allowed-text
      ((text allowed-text))
    "Text made only of allowed characters comes back unchanged.  Paired with the
contract's removal clause, this is what rules out an implementation that
returns the empty string for everything."
    (:about sanitize-for-json)
    (:kind :preservation)
    (:trials (:smoke 25 :normal 200))
    (string= text (sanitize-for-json text)))
  (defproperty sanitize-for-json-is-idempotent
      ((segments mixed-segments))
    "Sanitizing what SANITIZE-FOR-JSON returned changes nothing, over text
mixing allowed runs, forbidden controls, characters above U+FFFF, complete
escape sequences and cut-off ones."
    (:about sanitize-for-json)
    (:kind :idempotence)
    (:trials (:smoke 25 :normal 200))
    (let ((once (sanitize-for-json (render-segments segments))))
      (string= once (sanitize-for-json once))))
  (defproperty sanitize-for-json-removes-complete-escape-sequences
      ((parts wrapped-sequence))
    "A complete ECMA-48 sequence -- CSI, OSC ended by BEL or ST, DCS, SOS, PM or
APC ended by ST, or a two-character ESC Fe -- placed between two runs of
allowed text disappears, and both runs survive untouched."
    (:about sanitize-for-json)
    (:kind :composition)
    (:trials (:smoke 25 :normal 200))
    (destructuring-bind (before sequence after) parts
      (string= (concatenate 'string before after)
               (sanitize-for-json (concatenate 'string before
                                               (render-escape-sequence sequence)
                                               after)))))
  (defproperty sanitize-for-json-removes-truncated-escape-sequence
      ((parts cut-sequence))
    "An escape sequence cut off by the end of the input -- as captured output
cut at a length limit ends -- leaves no fragment behind, and the allowed text
before it survives untouched."
    (:about sanitize-for-json)
    (:kind :boundary)
    (:trials (:smoke 25 :normal 200))
    (destructuring-bind (before sequence length) parts
      (string= before
               (sanitize-for-json
                (concatenate 'string before
                             (subseq (render-escape-sequence sequence) 0 length))))))
  (defproperty sanitize-for-json-leaves-its-argument-unmodified
      ((segments mixed-segments))
    "The argument string holds the same characters after the call as before.
Whether the result is the argument itself or a copy is not specified."
    (:about sanitize-for-json)
    (:kind :non-destructive)
    (:trials (:smoke 25 :normal 200))
    (let* ((text (render-segments segments))
           (before (copy-seq text)))
      (sanitize-for-json text)
      (string= before text)))
  ;; SANITIZE-ERROR-MESSAGE
  (defgenerator short-normalized-text-generator ()
    "Draw single-spaced text of 0-40 or 480-500 characters, 499 and 500 often."
    (draw-normalized-text (let ((roll (random 100)))
                            (cond ((< roll 40) (random 41))
                                  ((< roll 70) (pick '(499 500)))
                                  (t (+ 480 (random 21)))))))
  (defspec short-normalized-text string
    (:generator short-normalized-text-generator))
  (defgenerator long-normalized-text-generator ()
    "Draw single-spaced text of 501-1500 characters, 501 often."
    (draw-normalized-text (let ((roll (random 100)))
                            (cond ((< roll 40) 501)
                                  ((< roll 90) (+ 502 (random 40)))
                                  (t (+ 1000 (random 501)))))))
  (defspec long-normalized-text string
    (:generator long-normalized-text-generator))
  (defgenerator error-message-generator ()
    "Draw a message descriptor (see DRAW-MESSAGE)."
    (draw-message))
  (defspec error-message list
    (:generator error-message-generator))
  (defgenerator sanitize-error-message-arguments ()
    "Draw a one-element argument list (see DRAW-ERROR-MESSAGE-ARGUMENT)."
    (:shrink (arguments) (%shrink-argument-list arguments))
    (list (draw-error-message-argument)))
  (defspec-function sanitize-error-message
    "Whatever it is given, the result is a string of at most 500 characters on
one line, with no run of whitespace and none at either end.  Checked over
SANITIZE-ERROR-MESSAGE-ARGUMENTS; other non-string objects are outside this
contract."
    (:args (message (or null string integer)))
    (:args-generator sanitize-error-message-arguments)
    (:returns string)
    (:post (and (<= (length result) 500)
                (single-line-normalized-p result))))
  (defproperty sanitize-error-message-keeps-normalized-text
      ((text short-normalized-text))
    "Single-spaced text of at most 500 characters, with no #<, #P\" or Stream:
in it, comes back unchanged -- including at exactly 499 and 500 characters."
    (:about sanitize-error-message)
    (:kind :preservation)
    (:trials (:smoke 25 :normal 200))
    (string= text (sanitize-error-message text)))
  (defproperty sanitize-error-message-truncates-long-text
      ((text long-normalized-text))
    "Single-spaced text longer than 500 characters comes back as exactly 500:
its first 497 characters, then \"...\"."
    (:about sanitize-error-message)
    (:kind :boundary)
    (:trials (:smoke 25 :normal 200))
    (let ((result (sanitize-error-message text)))
      (and (= 500 (length result))
           (string= text result :end1 497 :end2 497)
           (string= "..." result :start2 497))))
  (defproperty sanitize-error-message-keeps-only-visible-words
      ((message error-message))
    "A message of words, #P\"...\" pathnames and #<...> representations, joined
by any whitespace and perhaps followed by a Stream: section, comes back as its
words and paths one space apart: representations and the Stream: section go,
pathnames lose their #P\"\" wrapper, and whitespace collapses."
    (:about sanitize-error-message)
    (:kind :composition)
    (:trials (:smoke 25 :normal 200))
    (string= (visible-text message)
             (sanitize-error-message (render-message message))))
  (values))
