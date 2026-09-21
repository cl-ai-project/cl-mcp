;;;; specs/strings.lisp
;;;;
;;;; Contract and property for CL-MCP/SRC/UTILS/STRINGS:ENSURE-TRAILING-NEWLINE.
;;;;
;;;; Verified domain: strings of 0 to about 22 characters drawn from printable
;;;; ASCII, a few non-ASCII BMP characters, tab and carriage return, with
;;;; newlines inside and one of five endings (none, LF, CRLF, LF LF, CR).
;;;; Each is a simple string, a string with a fill pointer, or a base string
;;;; when every character allows it.  Not covered: strings of other element
;;;; types, very long strings, and anything that is not a string, which the
;;;; function's FTYPE declaration excludes.

(defpackage #:cl-mcp/specs/strings
  (:use #:cl)
  (:import-from #:cl-spec/main
                #:defspec
                #:defspec-function
                #:defproperty
                #:defgenerator)
  (:import-from #:cl-mcp/src/utils/strings
                #:ensure-trailing-newline)
  (:import-from #:cl-mcp/specs/fixtures
                #:pick
                #:chance
                #:draw-allowed-text)
  (:export #:register-specifications
           #:contract-names
           #:property-names
           #:spec-names
           #:generator-names
           #:call-examples
           #:ends-with-newline-p))

(in-package #:cl-mcp/specs/strings)

(defparameter *line-endings*
  (list ""
        (string #\Newline)
        (coerce (list #\Return #\Newline) 'string)
        (coerce (list #\Newline #\Newline) 'string)
        (string #\Return))
  "Endings a drawn text can have.  Only the ones ending in LF are terminated.")

(defun %respell (text)
  "Return a fresh string with TEXT's characters, as a simple string, a string
with a fill pointer (whose storage holds one extra, hidden character), or a
base string when every character is a base character."
  (let ((roll (random 100)))
    (cond ((and (< roll 15) (every (lambda (c) (typep c 'base-char)) text))
           (coerce text 'simple-base-string))
          ((< roll 30)
           (let ((string (make-array (1+ (length text))
                                     :element-type 'character
                                     :initial-element #\Newline
                                     :fill-pointer (length text))))
             (replace string text)))
          (t (copy-seq text)))))

(defun draw-line-text ()
  "Return a fresh string for ENSURE-TRAILING-NEWLINE: up to three lines of
allowed text, one of *LINE-ENDINGS*, and one of three string representations.
The hidden character past a fill pointer is a newline, so an implementation
that looked past the fill pointer would see a terminated string."
  (let ((lines (loop repeat (random 4)
                     collect (remove #\Newline (draw-allowed-text :max-length 6)))))
    (%respell (concatenate 'string
                           (format nil "~{~A~^~%~}" lines)
                           (if (chance 20) "" (pick *line-endings*))))))

(defun ends-with-newline-p (string)
  "True when the last character of STRING is a newline."
  (let ((length (length string)))
    (and (plusp length)
         (char= #\Newline (char string (1- length))))))

(defun contract-names ()
  "Return the functions this file puts a Function Spec on."
  '(ensure-trailing-newline))

(defun property-names ()
  "Return the properties this file defines."
  '(ensure-trailing-newline-keeps-terminated-text))

(defun spec-names ()
  "Return the named data specs this file defines."
  '(line-text))

(defun generator-names ()
  "Return the custom generators this file defines."
  '(line-text-generator ensure-trailing-newline-arguments))

(defun call-examples ()
  "Return concrete calls, as (FUNCTION ARGUMENTS), that CHECK-CALL runs against
the contract apart from generated trials: the empty string, a lone newline, a
lone carriage return, one character, a doubled newline, and a string whose
fill pointer hides a trailing newline."
  (list (list 'ensure-trailing-newline (list ""))
        (list 'ensure-trailing-newline (list (string #\Newline)))
        (list 'ensure-trailing-newline (list (string #\Return)))
        (list 'ensure-trailing-newline (list "a"))
        (list 'ensure-trailing-newline (list (format nil "a~%~%")))
        (list 'ensure-trailing-newline
              (list (make-array 3 :element-type 'character
                                  :initial-contents (format nil "ab~%")
                                  :fill-pointer 2)))))

(defun register-specifications ()
  "Install this file's generators, spec, Function Spec and property in
CL-SPEC:*REGISTRY*.  Registering again replaces each definition by name."
  (defgenerator line-text-generator ()
    "Draw text for ENSURE-TRAILING-NEWLINE (see DRAW-LINE-TEXT)."
    (draw-line-text))
  (defspec line-text string
    (:generator line-text-generator))
  (defgenerator ensure-trailing-newline-arguments ()
    "Draw a one-element argument list of LINE-TEXT."
    (list (draw-line-text)))
  ;; Every comparison with the argument uses BEFORE, a copy taken ahead of the
  ;; call: the argument object itself may have been overwritten by then, and an
  ;; implementation that fills its argument with newlines and returns it would
  ;; otherwise pass every clause.  :CAPTURE makes this a state-observing
  ;; contract, which cl-spec does not shrink.
  (defspec-function ensure-trailing-newline
    "The result ends in a newline, starts with the whole argument as it was
before the call, and is at most one character longer; the argument itself is
left as it was.  Together these rule out returning a bare newline, dropping or
overwriting text, and adding more than one character.  Checked over
ENSURE-TRAILING-NEWLINE-ARGUMENTS (see DRAW-LINE-TEXT)."
    (:args (text string))
    (:args-generator ensure-trailing-newline-arguments)
    (:capture (before (copy-seq text)))
    (:returns string)
    (:post (and (ends-with-newline-p result)
                (<= (length before) (length result) (1+ (length before)))
                (string= before result :end2 (length before))))
    (:state-post (string= before text)))
  (defproperty ensure-trailing-newline-keeps-terminated-text
      ((body line-text))
    "Text that already ends in a newline comes back with the same characters it
had before the call: the function adds a newline only when one is missing.
The argument is built by appending a newline to any LINE-TEXT, so every trial
is terminated, and the expected text is copied before the call."
    (:about ensure-trailing-newline)
    (:kind :preservation)
    (:trials (:smoke 25 :normal 200))
    (let* ((terminated (concatenate 'string body (string #\Newline)))
           (before (copy-seq terminated)))
      (string= before (ensure-trailing-newline terminated))))
  (values))
