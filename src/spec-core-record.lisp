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
           #:project-value
           #:project-record
           #:*projection-max-depth*
           #:*projection-max-length*
           #:*record-shapes*))

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

(defparameter *projection-max-depth* 12
  "How deep a record projection descends before it externalizes the rest.

Matched to *VALUE-PRINT-LEVEL*, which bounds the printer this module hands its
leaves to: a record whose depth ran past what the printer would show anyway
costs traversal for output nobody sees.")

(defparameter *projection-max-length* 200
  "How many entries of one list or plist a record projection keeps.")

(defvar *record-shapes* '()
  "Plist of NAME to descriptor, for the descriptors that refer to themselves.

An EXPLAIN-DATA error datum holds :ERRORS, a list of error datums, so its
descriptor cannot be written as a literal without (:REF :ERROR-DATUM).")

(defun %json-key (keyword)
  "Return KEYWORD as the snake_case JSON key this project publishes under.

Keys are snake_case and values keep their hyphens -- \"budget_source\" holding
\"not-collected\" -- which is the convention every existing response follows."
  (substitute #\_ #\- (string-downcase (symbol-name keyword))))

(defun %dotted-path (path)
  "Return PATH, a list of key names and indices, as one dotted string."
  (format nil "~{~A~^.~}" path))

(defun %resolve-descriptor (descriptor)
  "Return DESCRIPTOR with a (:REF NAME) indirection followed."
  (if (and (consp descriptor) (eq :ref (first descriptor)))
      (getf *record-shapes* (second descriptor))
      descriptor))

(defun project-record (value descriptor &optional path)
  "Project VALUE under DESCRIPTOR and return (values NODE ISSUES UNKNOWN-KEYS).

DESCRIPTOR says what VALUE means; VALUE's own shape never decides.  That is the
point: (:AT-LEAST :AT-MOST) is two case names and (:KIND :RANGE) is a plist,
and no test on the conses tells them apart, so a projector that guessed would
publish a key/value relation cl-spec never declared.

NODE is one of (:SCALAR x), (:SYMBOL plist), (:VALUE plist),
(:OBJECT ((key . NODE) ...)) or (:ARRAY (NODE ...)).  ISSUES records every
place the projection was cut, as (:PATH path :REASON reason [:OMITTED-ITEMS n]),
so a consumer can tell a record with two errors from a record with ten that was
cut at two.  UNKNOWN-KEYS names the keys no descriptor covers: their existence
is reported and their meaning is deliberately not guessed.

PATH is the position reached so far, for the entries of ISSUES and
UNKNOWN-KEYS."
  (let ((issues '())
        (unknown '()))
    (labels
        ((walk (value descriptor path depth)
           (let ((descriptor (%resolve-descriptor descriptor)))
             (cond
               ((eq :leaf descriptor) (project-value value))
               ((eq :opaque descriptor) (list :value (externalize-value value)))
               ;; >= rather than >: DEPTH counts containers already opened on
               ;; the way here, so the container that would be the (n+1)th is
               ;; the one cut, not one further past it.
               ((>= depth *projection-max-depth*)
                (push (list :path (reverse path) :reason :depth-limit) issues)
                (list :value (externalize-value value)))
               ((eq :word-list descriptor)
                (list :array (walk-list value :leaf path depth)))
               ((not (consp descriptor)) (project-value value))
               ((eq :array (first descriptor))
                (list :array (walk-list value (second descriptor) path depth)))
               ((eq :alist (first descriptor))
                (list :array (walk-alist value (second descriptor) path depth)))
               ((eq :object (first descriptor))
                (walk-object value (rest descriptor) path depth))
               (t (project-value value)))))
         (bounded (items path)
           ;; Cut here rather than in each caller, so the entry that records
           ;; the cut cannot be forgotten in one of them.
           (if (<= (length items) *projection-max-length*)
               items
               (progn
                 (push (list :path (reverse path) :reason :length-limit
                             :omitted-items (- (length items)
                                               *projection-max-length*))
                       issues)
                 (subseq items 0 *projection-max-length*))))
         (walk-list (items descriptor path depth)
           (loop for item in (bounded items path)
                 for index from 0
                 collect (walk item descriptor (cons index path) (1+ depth))))
         (walk-alist (entries descriptor path depth)
           (loop for entry in (bounded entries path)
                 for index from 0
                 collect
                 (list :object
                       (list (cons "name" (project-value (car entry)))
                             (cons "value"
                                   (walk (cdr entry) descriptor
                                         (cons index path) (1+ depth)))))))
         (walk-object (plist fields path depth)
           (let ((entries '()))
             (loop for (key raw) on (bounded plist path) by #'cddr
                   for field = (assoc key fields)
                   do (if field
                          (push (cons (%json-key key)
                                      (walk raw (cdr field)
                                            (cons (%json-key key) path)
                                            (1+ depth)))
                                entries)
                          ;; Named, not interpreted.  A future key's meaning is
                          ;; cl-spec's to define, and publishing a guess at it
                          ;; is the one thing this module must not do.
                          (push (%dotted-path
                                 (reverse (cons (%json-key key) path)))
                                unknown)))
             (list :object (nreverse entries)))))
      (let ((node (walk value descriptor (reverse path) 0)))
        (values node (nreverse issues) (nreverse unknown))))))
