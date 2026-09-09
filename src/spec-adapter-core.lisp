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
           #:cl-spec-api-version
           #:cl-spec-api-system-directory
           #:cl-spec-api-missing
           #:api-fn
           #:api-has-p
           #:api-class
           #:api-backend-available-p
           #:resolve-cl-spec-api
           #:resolve-symbol-designator
           #:symbol-data
           #:externalize-value
           #:digest-string
           #:printed-for-digest
           #:definition-digest))

(in-package #:cl-mcp/src/spec-adapter-core)

;;; ---------------------------------------------------------------------------
;;; cl-spec API handles
;;; ---------------------------------------------------------------------------

(defstruct cl-spec-api
  "Handles on the cl-spec public API, resolved once per call.

FUNCTIONS and CLASSES are plists keyed by the adapter's own keywords rather
than by cl-spec's symbols, so a test can build one out of lambdas and exercise
every branch of the report layer in an image where cl-spec was never loaded."
  (functions nil :type list)
  (classes nil :type list)
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
          (missing '()))
      (loop for (key . name) in +required-functions+
            for symbol = (find-symbol name package)
            do (if (and symbol (fboundp symbol))
                   (setf functions (list* key (fdefinition symbol) functions))
                   (push name missing)))
      (loop for (key . name) in +required-specials+
            for symbol = (find-symbol name package)
            do (if (and symbol (boundp symbol))
                   (setf functions
                         (list* key
                                (let ((special symbol))
                                  (lambda () (symbol-value special)))
                                functions))
                   (push name missing)))
      (loop for (key . name) in +condition-classes+
            for symbol = (find-symbol name package)
            do (when (and symbol (find-class symbol nil))
                 (setf classes (list* key symbol classes))))
      (multiple-value-bind (version directory) (%system-version-and-directory)
        (let ((api (make-cl-spec-api :functions functions
                                     :classes classes
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

(defun %find-package-named (name)
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
           (found-package (%find-package-named package-name)))
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

(defun %print-bounded (value max-chars)
  "Return (values TEXT DROPPED) for VALUE, retaining at most MAX-CHARS.

The bound is on what is *retained*, not on what is produced: printing an
unbounded structure into a string and cutting it afterwards costs the whole
structure in heap first, and a generated counterexample can be arbitrarily
large.  *PRINT-CIRCLE* is on so a shared or circular value prints as #n=
notation instead of running forever."
  (handler-case
      (let ((stream (make-bounded-output-stream (max 1 max-chars))))
        (let ((*print-circle* t)
              (*print-readably* nil)
              (*print-pretty* nil)
              (*print-level* nil)
              (*print-length* nil))
          (prin1 value stream))
        (let ((dropped (bounded-output-dropped stream)))
          (values (bounded-output-string stream) dropped)))
    (serious-condition (condition)
      (values (format nil "#<error printing a ~A: ~A>"
                      (%value-type-name value) (type-of condition))
              0))))

(defun externalize-value (value &key (max-chars 2000))
  "Return VALUE as the plist every response uses for a generated value.

  (:printed <string> :printed-complete <boolean> :omitted-chars <integer>
   :type <string> :object-id <integer-or-nil>)

No Lisp value is emitted as a JSON number: an integer seed or a rational can
exceed what a JSON consumer holds exactly, and a rounded number that looks
like a value is worse than text that admits to being text.

:PRINTED-COMPLETE NIL means the text was cut at MAX-CHARS.  Such text is a
display preview and NOT a value that can be read back -- the distinction
cl-spec specification 72.6 asks for.  :OBJECT-ID, when non-NIL, is the
object-registry id the existing inspect-object tool drills into."
  (multiple-value-bind (printed dropped) (%print-bounded value max-chars)
    (list :printed printed
          :printed-complete (zerop dropped)
          :omitted-chars dropped
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

(defun printed-for-digest (form)
  "Return FORM printed the same way regardless of the caller's environment.

*PACKAGE* is bound to KEYWORD so every symbol prints with its home package:
the same form read in two packages must not digest differently, and a symbol
printed without its package would let two same-named symbols collide."
  (handler-case
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
        (prin1-to-string form))
    (serious-condition (condition)
      (format nil "#<unprintable: ~A>" (type-of condition)))))

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

(defun definition-digest (api property-name registry)
  "Return a digest of PROPERTY-NAME's definition, or NIL when it cannot be read.

The digest covers the property's own data and the data of every named spec
reachable from its arguments, transitively.  Covering only the property would
miss the case that matters most in practice: the property text is untouched
but the spec it generates from was widened, so the same seed now explores a
different input domain and the run is not a reproduction of the earlier one.

A reference to a spec that is not registered is recorded as unresolved rather
than skipped, so the digest still changes if it is defined later."
  (handler-case
      (let ((property (funcall (api-fn api :property-data)
                               property-name :registry registry))
            (pending '())
            (seen (make-hash-table :test #'eq))
            (specs '()))
        (dolist (argument (getf property :arguments))
          (setf pending (%collect-spec-references (getf argument :spec) pending)))
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
        (digest-string
         (printed-for-digest
          (list :property property
                :specs (sort specs #'string<
                             :key (lambda (entry)
                                    (princ-to-string (car entry))))))))
    (error () nil)))
