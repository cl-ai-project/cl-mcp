;;;; src/spec-core-record.lisp
;;;;
;;;; One cl-spec versioned record, turned into data an MCP client can read.
;;;;
;;;; Nothing here calls cl-spec.  The input is a plist cl-spec already
;;;; produced -- RESULT-DATA or FUNCTION-SPEC-DATA -- so every rule in this
;;;; file can be exercised against the revisions this project cannot install,
;;;; including ones that do not exist yet.
;;;;
;;;; Three rules shape it.  A record is validated before it is read, because a
;;;; versioned API that answers something malformed is a fault to report
;;;; rather than a reason to fall back to an older reader.  Availability is
;;;; decided with GET-PROPERTIES and never with GETF, because a key that is
;;;; absent and a key whose value is NIL are different answers and GETF gives
;;;; the same one for both.  And projection is driven by a schema descriptor
;;;; rather than by the shape of the value, because the shape does not say
;;;; what the value means: (:AT-LEAST :AT-MOST) is two case names and
;;;; (:KIND :RANGE) is a plist, and nothing in the conses tells them apart.

(defpackage #:cl-mcp/src/spec-core-record
  (:use #:cl)
  (:import-from #:cl-mcp/src/spec-adapter-core
                #:symbol-data
                #:externalize-value)
  (:export #:safe-json-integer-p
           #:project-value))

(in-package #:cl-mcp/src/spec-core-record)

(defconstant +max-safe-json-integer+ (1- (expt 2 53))
  "The largest integer a JSON consumer holds exactly.

A double's mantissa is 53 bits, so a consumer reading JSON into doubles --
which is every JavaScript one -- rounds anything wider.  An SBCL fixnum reaches
2^62, and a cl-spec seed is a fixnum, which is why this adapter has always sent
seeds as text.  The rule is stated once here rather than at each site that
publishes a number.")

(defun safe-json-integer-p (value)
  "Return true when VALUE is an integer every JSON consumer holds exactly."
  (and (integerp value)
       (<= (- +max-safe-json-integer+) value +max-safe-json-integer+)))

(defun project-value (value &key (max-chars 2000))
  "Return VALUE as a tagged projection node, decided by its type.

The node is (:SCALAR x), (:SYMBOL plist) or (:VALUE plist).  The tag is for the
renderer: a SYMBOL-DATA plist and an EXTERNALIZE-VALUE plist are both plists,
and a renderer that had to tell them apart by looking for :QUALIFIED or
:PRINTED would break the first time either grew a key.

A keyword becomes its lower-case name, because cl-spec uses keywords as the
vocabulary of its statuses and kinds and a client renders them as words.  NIL
is a scalar null rather than the symbol COMMON-LISP::NIL, or every absent value
in every record would arrive as a symbol reference.  Any other symbol keeps its
package: two same-named symbols from different packages are different
definitions.

An integer inside the JSON-safe range stays a number; a wider one becomes a
decimal string rather than a number a consumer would round.  Everything else --
a CLOS instance, a structure, a hash table, a function, a value from the code
under test -- goes through EXTERNALIZE-VALUE, which prints it bounded and
offers an object id instead of pretending the text is the object."
  (cond ((keywordp value) (list :scalar (string-downcase (symbol-name value))))
        ((null value) (list :scalar nil))
        ((symbolp value) (list :symbol (symbol-data value)))
        ((stringp value) (list :scalar value))
        ((safe-json-integer-p value) (list :scalar value))
        ((integerp value) (list :scalar (format nil "~D" value)))
        (t (list :value (externalize-value value :max-chars max-chars)))))
