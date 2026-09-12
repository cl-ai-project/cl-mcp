;;;; src/spec-adapter-core.lisp
;;;;
;;;; Late-bound access to cl-spec, and the pieces that turn its Lisp values
;;;; into something an MCP client can read.
;;;;
;;;; cl-mcp does not depend on cl-spec.  A project that has never heard of
;;;; cl-spec must keep working, so the API is resolved through FIND-PACKAGE
;;;; and FIND-SYMBOL at call time, the way SRC/PROXY.LISP reaches the pool and
;;;; SRC/CODE-CORE.LISP reaches SB-INTROSPECT.  Resolving fixed literal names
;;;; is not the same thing as interning a name that arrived from outside: this
;;;; file never interns anything.

(defpackage #:cl-mcp/src/spec-adapter-core
  (:use #:cl)
  (:import-from #:cl-mcp/src/object-registry
                #:inspectable-p
                #:register-object)
  (:import-from #:cl-mcp/src/utils/bounded-stream
                #:make-bounded-output-stream
                #:bounded-output-string
                #:bounded-output-dropped)
  (:export #:cl-spec-api
           #:cl-spec-api-p
           #:make-cl-spec-api
           #:cl-spec-api-functions
           #:cl-spec-api-classes
           #:cl-spec-api-specials
           #:api-special
           #:cl-spec-api-version
           #:cl-spec-api-system-directory
           #:cl-spec-api-missing
           #:api-fn
           #:api-has-p
           #:api-class
           #:api-backend-available-p
           #:resolve-cl-spec-api
           #:resolve-symbol-designator
           #:find-package-named
           #:find-keyword
           #:symbol-data
           #:externalize-value
           #:digest-string
           #:printed-for-digest
           #:printed-for-display
           #:print-form-bounded
           #:core-schema-data #:definition-digest))

(in-package #:cl-mcp/src/spec-adapter-core)

;;; ---------------------------------------------------------------------------
;;; cl-spec API handles
;;; ---------------------------------------------------------------------------

(defstruct cl-spec-api
  "Handles on the cl-spec public API, resolved once per call.

FUNCTIONS and CLASSES are plists keyed by the adapter's own keywords rather
than by cl-spec's symbols, so a test can build one out of lambdas and exercise
every branch of the report layer in an image where cl-spec was never loaded.

SPECIALS holds the special variable SYMBOLS themselves, which FUNCTIONS cannot
substitute for.  A run happens on a deadline thread, and a new thread does not
inherit dynamic bindings: reading a special through a closure gives the
caller's value, but making the run SEE that value takes PROGV, and PROGV needs
the symbol.  Empty on a stub API, where PROGV then binds nothing."
  (functions nil :type list)
  (classes nil :type list)
  (specials nil :type list)
  (version nil)
  (system-directory nil)
  (missing nil :type list))

(defparameter +required-functions+
  '((:semantic-data . "SEMANTIC-DATA")
    (:spec-data . "SPEC-DATA")
    (:property-data . "PROPERTY-DATA")
    (:properties-for . "PROPERTIES-FOR")
    (:run-property . "RUN-PROPERTY")
    (:backend-default-trials . "BACKEND-DEFAULT-TRIALS")
    (:result-status . "PROPERTY-RESULT-STATUS")
    (:result-trials . "PROPERTY-RESULT-TRIALS")
    (:result-seed . "PROPERTY-RESULT-SEED")
    (:result-profile . "PROPERTY-RESULT-PROFILE")
    (:result-counterexample . "PROPERTY-RESULT-COUNTEREXAMPLE")
    (:result-shrunk-counterexample . "PROPERTY-RESULT-SHRUNK-COUNTEREXAMPLE")
    (:result-condition . "PROPERTY-RESULT-CONDITION")
    (:result-elapsed . "PROPERTY-RESULT-ELAPSED"))
  "Adapter key to cl-spec function name.  Every one of these must be present
and fbound for the adapter to report itself usable.")

(defparameter +optional-functions+
  '((:result-data . "RESULT-DATA")
     (:list-specs . "LIST-SPECS")
    (:list-properties . "LIST-PROPERTIES")
    (:list-function-specs . "LIST-FUNCTION-SPECS")
    (:properties-with-tag . "PROPERTIES-WITH-TAG")
    (:function-spec-data . "FUNCTION-SPEC-DATA")
    (:check-function . "CHECK-FUNCTION")
    (:check-rejected . "FUNCTION-CHECK-RESULT-REJECTED")
    (:check-failure-reason . "FUNCTION-CHECK-RESULT-FAILURE-REASON")
    (:check-explanation . "FUNCTION-CHECK-RESULT-EXPLANATION")
    (:check-budget . "FUNCTION-CHECK-RESULT-BUDGET"))
  "Adapter key to cl-spec function name, resolved when present.

Absence costs one operation rather than the whole adapter: a cl-spec without
these still answers every question about a symbol the caller already knows,
so putting them in +REQUIRED-FUNCTIONS+ would take the whole integration down
to lose a listing.  API-HAS-P is how a caller asks.")

(defparameter +required-specials+
  '((:registry . "*REGISTRY*")
    (:generator-backend . "*GENERATOR-BACKEND*"))
  "Adapter key to cl-spec special name.  Each becomes a zero-argument reader
in the FUNCTIONS plist, so the report layer reads a special the same way it
calls a function and a stub can supply either.")

(defparameter +condition-classes+
  '((:cl-spec-error . "CL-SPEC-ERROR")
    (:no-generator-backend . "NO-GENERATOR-BACKEND")
    (:generator-unavailable . "GENERATOR-UNAVAILABLE")
    (:unknown-spec . "UNKNOWN-SPEC")
    (:unknown-property . "UNKNOWN-PROPERTY")
    (:unknown-function-spec . "UNKNOWN-FUNCTION-SPEC")
    (:not-implemented . "NOT-IMPLEMENTED"))
  "Adapter key to cl-spec condition name.  Absence is tolerated: a missing
condition class only costs a coarser classification, never an error.")

(defun %system-version-and-directory ()
  "Return (values VERSION DIRECTORY) for the loaded cl-spec ASDF system.

Both are NIL when ASDF does not know the system.  Neither is guessed: a
version reported here has to have come from the system definition."
  (handler-case
      (let ((system (asdf:find-system "cl-spec" nil)))
        (if system
            (values (asdf:component-version system)
                    (ignore-errors
                     (namestring (asdf:system-source-directory system))))
            (values nil nil)))
    (error () (values nil nil))))

(defun resolve-cl-spec-api ()
  "Return (values API STATUS) for the cl-spec loaded in this image.

STATUS is :NOT-LOADED when the CL-SPEC package is absent, :INCOMPLETE when it
is present but some name this adapter needs is missing or unbound, and :OK
otherwise.  The three are kept apart because they call for different advice:
load the system, report a version mismatch, or proceed."
  (let ((package (find-package "CL-SPEC")))
    (unless package
      (return-from resolve-cl-spec-api (values nil :not-loaded)))
    (let ((functions '())
          (classes '())
          (specials '())
          (missing '()))
      (loop for (key . name) in +required-functions+
            for symbol = (find-symbol name package)
            do (if (and symbol (fboundp symbol))
                   (setf functions (list* key (fdefinition symbol) functions))
                   (push name missing)))
      (loop for (key . name) in +optional-functions+
            for symbol = (find-symbol name package)
            do (when (and symbol (fboundp symbol))
                 (setf functions (list* key (fdefinition symbol) functions))))
      (loop for (key . name) in +required-specials+
            for symbol = (find-symbol name package)
            do (if (and symbol (boundp symbol))
                   (setf functions
                         (list* key
                                (let ((special symbol))
                                  (lambda () (symbol-value special)))
                                functions)
                         specials (list* key symbol specials))
                   (push name missing)))
      (loop for (key . name) in +condition-classes+
            for symbol = (find-symbol name package)
            do (when (and symbol (find-class symbol nil))
                 (setf classes (list* key symbol classes))))
      (multiple-value-bind (version directory) (%system-version-and-directory)
        (let ((api (make-cl-spec-api :functions functions
                                     :classes classes
                                     :specials specials
                                     :version version
                                     :system-directory directory
                                     :missing (sort missing #'string<))))
          (values api (if missing :incomplete :ok)))))))

(defun api-has-p (api key)
  "Return true when API carries a handle for KEY."
  (and api (getf (cl-spec-api-functions api) key) t))

(defun api-fn (api key)
  "Return API's handle for KEY, signalling when it is absent.

Absence is a programming error in this adapter rather than a user-facing
condition: the report layer checks the API's status before it calls anything."
  (or (and api (getf (cl-spec-api-functions api) key))
      (error "cl-spec API handle ~S is not available." key)))

(defun api-class (api key)
  "Return the condition class symbol API carries for KEY, or NIL."
  (and api (getf (cl-spec-api-classes api) key)))

(defun api-special (api key)
  "Return the special variable symbol API carries for KEY, or NIL.

Used to PROGV-bind cl-spec's specials inside a run thread.  A stub API carries
none, and a caller binding an empty list of symbols simply binds nothing."
  (and api (getf (cl-spec-api-specials api) key)))

(defun api-backend-available-p (api)
  "Return true when a cl-spec generator backend is installed.

An absent backend is not an error: introspection works without one and only
execution needs it, so the two are reported separately."
  (and (api-has-p api :generator-backend)
       (handler-case (and (funcall (api-fn api :generator-backend)) t)
         (error () nil))))

;;; ---------------------------------------------------------------------------
;;; Symbol resolution
;;; ---------------------------------------------------------------------------
;;;
;;; The reader is not used here, and neither is INTERN.  A tool argument is
;;; text from outside the image: reading it would run reader macros, and
;;; interning it would let a caller grow the image by asking about names that
;;; do not exist.  FIND-PACKAGE and FIND-SYMBOL answer the only question that
;;; matters -- does this name already denote something -- and answer it
;;; without side effects.

(defun find-package-named (name)
  "Return the package NAME denotes, trying NAME before its upcased form.

Exact first, because a package genuinely created with a lower-case name must
not be shadowed by an upper-case one that happens to exist."
  (or (find-package name)
      (find-package (string-upcase name))))

(defun %find-symbol-named (name package)
  "Return (values SYMBOL STATUS) for NAME in PACKAGE, exact form first."
  (multiple-value-bind (symbol status) (find-symbol name package)
    (if status
        (values symbol status)
        (find-symbol (string-upcase name) package))))

(defun %split-designator (designator)
  "Split DESIGNATOR into (values PACKAGE-PART NAME-PART DOUBLE-COLON-P).

PACKAGE-PART is NIL when DESIGNATOR carries no package marker.  A leading
colon names a keyword, which is why that case reports a double colon: a
keyword has no external/internal distinction to enforce."
  (let ((colon (position #\: designator)))
    (cond
      ((null colon) (values nil designator nil))
      ((zerop colon)
       (let ((start (if (and (> (length designator) 1)
                             (char= #\: (char designator 1)))
                        2
                        1)))
         (values "KEYWORD" (subseq designator start) t)))
      (t
       (let* ((double (and (< (1+ colon) (length designator))
                           (char= #\: (char designator (1+ colon)))))
              (start (+ colon (if double 2 1))))
         (values (subseq designator 0 colon)
                 (subseq designator start)
                 double))))))

(defun resolve-symbol-designator (designator &key package)
  "Return (values SYMBOL NIL) for DESIGNATOR, or (values NIL REASON).

DESIGNATOR is \"SYM\", \"PKG:SYM\" or \"PKG::SYM\".  PACKAGE is consulted only
for the unqualified form and defaults to COMMON-LISP-USER.  A single colon
accepts only an external symbol, as the reader would; a double colon accepts
an internal one.

REASON is a plist headed by :REASON, one of :MALFORMED, :PACKAGE-NOT-FOUND,
:SYMBOL-NOT-FOUND or :NOT-EXTERNAL, carrying the names involved so the caller
can say which package and which name it looked in."
  (unless (and (stringp designator) (plusp (length designator)))
    (return-from resolve-symbol-designator
      (values nil (list :reason :malformed :input designator
                        :detail "symbol must be a non-empty string"))))
  (when (or (find #\| designator) (find #\\ designator))
    (return-from resolve-symbol-designator
      (values nil (list :reason :malformed :input designator
                        :detail "escaped symbol names are not supported"))))
  (multiple-value-bind (package-part name-part double-colon)
      (%split-designator designator)
    (when (or (zerop (length name-part)) (find #\: name-part))
      (return-from resolve-symbol-designator
        (values nil (list :reason :malformed :input designator
                          :detail "expected SYM, PKG:SYM or PKG::SYM"))))
    (let* ((package-name (or package-part
                             (and (stringp package) (plusp (length package))
                                  package)
                             "COMMON-LISP-USER"))
           (found-package (find-package-named package-name)))
      (unless found-package
        (return-from resolve-symbol-designator
          (values nil (list :reason :package-not-found
                            :package package-name :input designator))))
      (multiple-value-bind (symbol status)
          (%find-symbol-named name-part found-package)
        (cond
          ((null status)
           (values nil (list :reason :symbol-not-found
                             :package (package-name found-package)
                             :name (string-upcase name-part)
                             :input designator)))
          ;; A qualified name written with one colon must name an external
          ;; symbol, exactly as the reader requires.  Accepting an internal
          ;; one here would make PKG:SYM and PKG::SYM interchangeable, and
          ;; the distinction is the only thing telling a caller that it is
          ;; reaching past a package's own boundary.
          ((and package-part (not double-colon) (not (eq status :external)))
           (values nil (list :reason :not-external
                             :package (package-name found-package)
                             :name (symbol-name symbol)
                             :input designator)))
          (t (values symbol nil)))))))

(defun find-keyword (name)
  "Return the keyword named NAME if it already exists, else NIL.

Never interns.  A keyword no loaded code mentions cannot be carried by any
registered definition either, so the absence is an answer rather than a reason
to create one -- the same rule tool arguments follow everywhere here."
  (when (and (stringp name) (plusp (length name)))
    (find-symbol (string-upcase name) "KEYWORD")))

(defun symbol-data (symbol)
  "Return SYMBOL as the plist every response uses for a symbol.

  (:package <string-or-nil> :name <string> :qualified <string>)

Package and name are carried separately because two symbols with the same name
in different packages are different contracts, and a single printed string
would let a consumer conflate them (cl-spec specification 72.6)."
  (let ((package (symbol-package symbol)))
    (list :package (when package (package-name package))
          :name (symbol-name symbol)
          :qualified (if package
                         (format nil "~A::~A"
                                 (package-name package) (symbol-name symbol))
                         (format nil "#:~A" (symbol-name symbol))))))

;;; ---------------------------------------------------------------------------
;;; Value externalization
;;; ---------------------------------------------------------------------------

(defun %value-type-name (value)
  "Return a short, stable type name for VALUE.

TYPE-OF is not used directly because on SBCL it answers a fixnum with its
whole range -- (INTEGER 0 4611686018427387903) -- which tells a reader
nothing it wanted to know.  The common shapes are named outright and the
rest fall back to the head of TYPE-OF, which is the class name for a CLOS
instance or a structure."
  (typecase value
    (null "null")
    (integer "integer")
    (ratio "ratio")
    (float "float")
    (complex "complex")
    (string "string")
    (character "character")
    (symbol "symbol")
    (cons "cons")
    (hash-table "hash-table")
    (function "function")
    (package "package")
    (pathname "pathname")
    (vector "vector")
    (array "array")
    (t (let ((name (type-of value)))
         (string-downcase (princ-to-string (if (consp name) (first name) name)))))))

(defparameter *value-print-level* 12
  "Depth beyond which a generated value is printed as # rather than walked.

Finite, deliberately.  Bounding only the characters that are RETAINED bounds
the memory a rendering costs but not the work it does: PRIN1 still walks the
whole structure, and a generated value can be as deep as its generator was
asked to make it.  A value cut here is a preview by construction, which is
what EXTERNALIZE-VALUE's :RESTORABLE reports.")

(defparameter *value-print-length* 200
  "Elements per level beyond which a generated value is printed as ... .

Same reasoning as *VALUE-PRINT-LEVEL*: a list of a million elements costs a
million steps to render even when only the first two thousand characters are
kept.")

(defun %drain-bounded (stream limit)
  "Return (values RETAINED DROPPED) for a bounded STREAM written up to LIMIT.

BOUNDED-OUTPUT-STRING appends a note of its own -- \"... (truncated, N total
chars)\" -- when anything was dropped.  That note is right for a captured
transcript and wrong for a value: it pushes the text past the caller's
max_chars, restates the count the caller already gets as :OMITTED-CHARS, and
puts a newline inside what is documented as a one-line rendering.  The
retained text is exactly LIMIT characters whenever anything was dropped, since
the sink stops accepting at LIMIT, so cutting there removes the note and
nothing else."
  ;; Nested rather than parallel: BOUNDED-OUTPUT-STRING drains the sink and
  ;; resets the counters, so the dropped count has to be read first.
  (let ((dropped (bounded-output-dropped stream)))
    (let ((text (bounded-output-string stream)))
      (values (if (plusp dropped)
                  (subseq text 0 (min (length text) limit))
                  text)
              dropped))))

(defun %print-bounded (value max-chars)
  "Return (values TEXT DROPPED) for VALUE, retaining at most MAX-CHARS.

Three bounds, not one.  MAX-CHARS bounds what is retained, so the memory a
rendering costs does not follow the value's size.  *VALUE-PRINT-LEVEL* and
*VALUE-PRINT-LENGTH* bound what is WALKED, which MAX-CHARS alone does not:
PRIN1 traverses the whole structure whatever the sink does with the
characters.  *PRINT-CIRCLE* is on so a shared or circular value prints as #n=
notation instead of running forever."
  (handler-case
      (let ((stream (make-bounded-output-stream (max 1 max-chars))))
        (let ((*print-circle* t)
              (*print-readably* nil)
              (*print-pretty* nil)
              (*print-level* *value-print-level*)
              (*print-length* *value-print-length*))
          (prin1 value stream))
        (%drain-bounded stream (max 1 max-chars)))
    (serious-condition (condition)
      (values (format nil "#<error printing a ~A: ~A>"
                      (%value-type-name value) (type-of condition))
              0))))

(defun externalize-value (value &key (max-chars 2000))
  "Return VALUE as the plist every response uses for a generated value.

  (:printed <string> :printed-complete <boolean> :omitted-chars <integer>
   :restorable <boolean> :print-level <integer> :print-length <integer>
   :type <string> :object-id <integer-or-nil>)

No Lisp value is emitted as a JSON number: an integer seed or a rational can
exceed what a JSON consumer holds exactly, and a rounded number that looks
like a value is worse than text that admits to being text.

:PRINTED-COMPLETE and :RESTORABLE are separate answers to separate questions.
The first says no characters were dropped at MAX-CHARS.  The second says the
text can be read back as this value, and it is FALSE for most values whose
text is complete: printing runs with *PRINT-READABLY* NIL and bounded depth,
so a CLOS instance renders as #<FOO {1004}> -- complete, and not the object.
A symbol is excluded too, because whether its text denotes it depends on the
reading package.  What survives is numbers, characters, strings, keywords and
NIL/T.  Everything else offers :OBJECT-ID instead, which the existing
inspect-object tool drills into."
  (multiple-value-bind (printed dropped) (%print-bounded value max-chars)
    (list :printed printed
          :printed-complete (zerop dropped)
          :omitted-chars dropped
          :restorable (and (zerop dropped)
                           (or (numberp value)
                               (characterp value)
                               (stringp value)
                               (keywordp value)
                               (member value '(nil t)))
                           t)
          :print-level *value-print-level*
          :print-length *value-print-length*
          :type (%value-type-name value)
          :object-id (when (inspectable-p value)
                       (ignore-errors (register-object value))))))

;;; ---------------------------------------------------------------------------
;;; Definition digest
;;; ---------------------------------------------------------------------------

(defconstant +fnv-offset-basis+ 14695981039346656037
  "FNV-1a 64-bit offset basis.")

(defconstant +fnv-prime+ 1099511628211
  "FNV-1a 64-bit prime.")

(defconstant +fnv-mask+ #xFFFFFFFFFFFFFFFF
  "Mask keeping the FNV-1a accumulator at 64 bits.")

(defun digest-string (string)
  "Return the FNV-1a 64-bit digest of STRING as 16 lower-case hex digits.

FNV-1a rather than a real hash because this identifies a definition for a
human and an agent to compare, not for anything to trust: a 64-bit
non-cryptographic digest is enough to notice that a property changed, and
adding a crypto dependency to cl-mcp for it would not be."
  (let ((hash +fnv-offset-basis+))
    (loop for byte across (sb-ext:string-to-octets string :external-format :utf-8)
          do (setf hash (logand (* (logxor hash byte) +fnv-prime+) +fnv-mask+)))
    (format nil "~(~16,'0x~)" hash)))

(defparameter *digest-print-limit* 1000000
  "Characters of printed definition a digest is computed over.

Generous rather than tight: a digest that silently ignored part of a
definition would report a match between two definitions that differ only past
the cut, which is the one thing a digest exists to prevent.  PRINTED-FOR-DIGEST
reports when the limit was reached so the caller is told rather than misled.")

(defun printed-for-digest (form)
  "Return (values TEXT TRUNCATED-P) for FORM, printed the same way everywhere.

*PACKAGE* is bound to KEYWORD so every symbol prints with its home package:
the same form read in two packages must not digest differently, and a symbol
printed without its package would let two same-named symbols collide.

Bounded by *DIGEST-PRINT-LIMIT* characters rather than by depth or length.  A
source form comes from a file and its size is bounded by that file, so the
limit is a guard against a pathological literal rather than the usual case --
and unlike a depth cut, a character cut is detectable, which is what lets a
digest computed from truncated input say so instead of quietly colliding with
every other definition that shares its first megabyte."
  (handler-case
      (let ((stream (make-bounded-output-stream *digest-print-limit*)))
        (let ((*package* (find-package "KEYWORD"))
              (*print-circle* t)
              (*print-pretty* nil)
              (*print-readably* nil)
              (*print-level* nil)
              (*print-length* nil)
              (*print-base* 10)
              (*print-radix* nil)
              (*print-case* :upcase)
              (*read-default-float-format* 'double-float))
          (prin1 form stream))
        (multiple-value-bind (text dropped)
            (%drain-bounded stream *digest-print-limit*)
          (values text (plusp dropped))))
    (serious-condition (condition)
      (values (format nil "#<unprintable: ~A>" (type-of condition)) t))))

(defparameter *display-print-level* 50
  "Depth guard for the display printer.

Display prints with *PRINT-CIRCLE* NIL, which cannot terminate on a circular
form by itself.  A source form comes from the reader and is a tree, so this
never fires in practice; it is the guard that makes \"never in practice\" safe
to rely on.")

(defparameter *display-print-length* 10000
  "Length guard for the display printer.  See *DISPLAY-PRINT-LEVEL*.")

(defmacro with-display-printing (&body body)
  "Run BODY with the printer bound for reading by a person or a model.

*PRINT-CIRCLE* is NIL here and T in WITH-DIGEST-PRINTING, and that difference
is the whole point.  A form loaded from a compiled file has its tails
coalesced by the file compiler -- ordinary structure sharing, not circularity
-- and *PRINT-CIRCLE* T renders that as #1=(LOW . #2=(HIGH)), which reads as a
dotted improper list to anyone skimming it.  The same body loaded from source,
or defined at a REPL, prints cleanly, so the broken rendering appears exactly
when the definition came from the file it is supposed to document.

The depth and length guards replace the termination that *PRINT-CIRCLE* was
providing: a circular form stops at them instead of running forever.

Not pretty-printed: BODY and SOURCE_FORM travel as JSON fields a client may
compare across calls, and pretty printing makes their line breaks depend on
*PRINT-RIGHT-MARGIN*."
  `(let ((*package* (find-package "KEYWORD"))
         (*print-circle* nil)
         (*print-pretty* nil)
         (*print-readably* nil)
         (*print-level* *display-print-level*)
         (*print-length* *display-print-length*)
         (*print-base* 10)
         (*print-radix* nil)
         (*print-case* :upcase)
         (*read-default-float-format* 'double-float))
     ,@body))

(defun printed-for-display (form)
  "Return FORM printed for a reader, with no structure-sharing labels."
  (handler-case (with-display-printing (prin1-to-string form))
    (serious-condition (condition)
      (format nil "#<unprintable: ~A>" (type-of condition)))))

(defun print-form-bounded (form max-chars)
  "Return (values TEXT COMPLETE-P OMITTED-CHARS) for FORM at MAX-CHARS.

Bounded on the way out rather than printed in full and cut afterwards.  The
caller of a spec-describe asks for 8000 characters by default; rendering a
megabyte to hand back eight kilobytes costs the megabyte, three times per
property.  The sink counts what it discards, so the remainder reported here is
the true one rather than the difference between two limits.

Prints for display, not for the digest: see WITH-DISPLAY-PRINTING."
  (let ((limit (max 1 max-chars)))
    (handler-case
        (let ((stream (make-bounded-output-stream limit)))
          (with-display-printing (prin1 form stream))
          (multiple-value-bind (text dropped) (%drain-bounded stream limit)
            (values text (zerop dropped) dropped)))
      (serious-condition (condition)
        (values (format nil "#<unprintable: ~A>" (type-of condition)) nil 0)))))

(defun %collect-spec-references (spec-plist accumulator)
  "Push every :REFERENCE target reachable from SPEC-PLIST onto ACCUMULATOR.

Returns the accumulator.  Walks :CHILDREN, which is how SPEC-DATA nests an
AND or a LIST-OF node."
  (when (listp spec-plist)
    (when (eq :reference (getf spec-plist :kind))
      (let ((target (getf spec-plist :target)))
        (when target (pushnew target accumulator))))
    (dolist (child (getf spec-plist :children))
      (setf accumulator (%collect-spec-references child accumulator))))
  accumulator)

(defun %spec-sort-key (entry)
  "Return the ordering key for one (NAME . DATA) entry of a digest input.

Package and name are kept apart, and read off the symbol rather than printed.
PRINC-TO-STRING renders A::ACCOUNT and B::ACCOUNT both as \"ACCOUNT\", so two
same-named specs from different packages tied and their order fell out of
traversal instead of the sort -- in the one case the sort exists for.  It also
followed *PRINT-CASE*, which a digest must not."
  (let* ((symbol (car entry))
         (package (symbol-package symbol)))
    (concatenate 'string
                 (if package (package-name package) "#:")
                 "|"
                 (symbol-name symbol))))

(defun core-schema-data (data)
  "Return recognized core metadata, independent of the adapter's JSON schema.
Absent or unsupported core versions return NIL; they are never inferred."
  (when (eql 1 (getf data :schema-version))
    (loop for key in '(:schema-version :record-kind :entity-kind :definition-digest
                      :definition-digest-complete :definition-digest-covers :capabilities)
          append (list key (getf data key)))))

(defun definition-digest (api property-name registry
                          &key (property nil property-p)
                               (data-key :property-data))
  "Return (values DIGEST COMPLETE-P) for PROPERTY-NAME's definition.

DATA-KEY names the reader to fall back on when PROPERTY is not supplied.  It
matters when a name carries both a property and a contract: digesting the
property and stamping the result on the contract's run would report the
definitions unchanged on the strength of a definition that was not run.

PROPERTY, when supplied, is DATA-KEY's plist for the same name, already
fetched by the caller.  Passing it is what keeps a listing of N properties to
N calls rather than 2N: the digest needs exactly the data the summary beside
it already read.

DIGEST is NIL when the definition cannot be read at all.  COMPLETE-P is false
when the printed input hit *DIGEST-PRINT-LIMIT*, in which case two definitions
differing only past the cut would digest the same -- so a caller comparing
digests has to be told.

The digest covers the property's own data and the data of every named spec
reachable from its arguments -- and, for a contract, its return spec --
transitively.  Covering only the property would
miss the case that matters most in practice: the property text is untouched
but the spec it generates from was widened, so the same seed now explores a
different input domain and the run is not a reproduction of the earlier one.

A reference to a spec that is not registered is recorded as unresolved rather
than skipped, so the digest still changes if it is defined later."
  (handler-case
      (let ((property (or (if property-p
                              property
                              (funcall (api-fn api data-key)
                                       property-name :registry registry))
                          ;; A reader that answered NIL is not an empty
                          ;; definition.  Digested as one it gave the same
                          ;; stable hex for every unreadable name, published
                          ;; with COMPLETE true -- so a replay against a
                          ;; definition nothing had read came back "match",
                          ;; in the field whose whole job is to say the
                          ;; definition did not move.
                          (return-from definition-digest (values nil nil))))
            (pending '())
            (seen (make-hash-table :test #'eq))
            (specs '()))
        ;; A versioned core record owns its digest, including incompleteness.
        ;; Never turn an unknown schema or missing dependency into a legacy match.
        (when (get-properties property '(:schema-version))
          (return-from definition-digest
            (if (and (eql 1 (getf property :schema-version))
                     (getf property :definition-digest-complete)
                     (stringp (getf property :definition-digest)))
                (values (getf property :definition-digest) t)
                (values nil nil))))
        (dolist (argument (getf property :arguments))
          (setf pending (%collect-spec-references (getf argument :spec) pending)))
        ;; :RETURNS as well, for a function spec.  The contract's own data
        ;; holds only a reference node naming the spec, so widening that spec
        ;; leaves every byte of the contract identical -- and the run whose
        ;; output domain just moved would come back "faithful".  Absent from a
        ;; property's data, where this is a no-op.
        (setf pending (%collect-spec-references (getf property :returns) pending))
        (loop while pending
              for name = (pop pending)
              unless (gethash name seen)
                do (setf (gethash name seen) t)
                   (let ((data (handler-case
                                   (funcall (api-fn api :spec-data)
                                            name :registry registry)
                                 (error () (list :unresolved-reference name)))))
                     (push (cons name data) specs)
                     (setf pending (%collect-spec-references data pending))))
        (multiple-value-bind (text truncated)
            (printed-for-digest
             (list :property property
                   :specs (sort specs #'string< :key #'%spec-sort-key)))
          (values (digest-string text) (not truncated))))
    (error () (values nil nil))))
