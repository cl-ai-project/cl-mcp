;;;; src/tools/clos-response-builders.lisp
;;;;
;;;; Parent-side half of clos-describe: annotate the worker's report with the
;;;; form_type and form_name of each definition, read from its source file, and
;;;; render the content text.  Loads eclector (through code-refs-scan), so the
;;;; worker must not import this file.

(defpackage #:cl-mcp/src/tools/clos-response-builders
  (:use #:cl)
  (:import-from #:cl-mcp/src/code-refs-scan
                #:top-level-forms-at)
  (:import-from #:cl-mcp/src/code-refs-core
                #:sequence->list
                #:*note-stale*)
  (:import-from #:cl-mcp/src/lisp-edit-form-core
                #:locate-form-in-nodes)
  (:import-from #:cl-mcp/src/cst
                #:parse-top-level-forms
                #:cst-node-start
                #:cst-node-end)
  (:import-from #:cl-mcp/src/fs
                #:fs-read-source-text)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht
                #:text-content)
  (:import-from #:cl-ppcre
                #:regex-replace-all
                #:quote-meta-chars)
  (:export #:clos-report-p
           #:annotate-report-forms
           #:build-clos-describe-response
           #:*note-no-form-at-line*
           #:*note-unparseable*
           #:*reason-verification-unavailable*
           #:*reason-not-locatable*
           #:*reason-not-readable*
           #:*reason-no-source-line*
           #:*reason-source-not-on-disk*
           #:*reason-no-source-recorded*))

(in-package #:cl-mcp/src/tools/clos-response-builders)

(defparameter *note-no-form-at-line* "no top-level form starts at this line"
  "Note on a definition whose recorded line starts no top-level form in its file.")

(defparameter *note-unparseable* "file could not be parsed"
  "Note on a definition whose source file does not parse.")

(defun %true-p (value)
  "True when VALUE, a JSON boolean, is true.  False arrives as YASON:FALSE
in-process and as NIL after the worker's JSON round trip."
  (and value (not (eq value yason:false))))

(defun clos-report-p (object)
  "True when OBJECT is a clos-describe report rather than an error result.
PROXY-TO-WORKER returns a crash notice or a worker error as a hash-table with
isError and content, which must reach the caller untouched."
  (and (hash-table-p object)
       (nth-value 1 (gethash "symbol_status" object))
       (not (%true-p (gethash "isError" object)))))

(defun %located-entries (report)
  "Return every object in REPORT that carries a source location: each generic
function and its methods, the class and its methods."
  (let ((entries '()))
    (dolist (gf (sequence->list (gethash "generic_functions" report)))
      (push gf entries)
      (dolist (method (sequence->list (gethash "methods" gf)))
        (push method entries)))
    (let ((class (gethash "class" report)))
      (when (hash-table-p class)
        (push class entries)
        (dolist (method (sequence->list (gethash "methods" class)))
          (push method entries))))
    (nreverse entries)))

(defun %json-token (plist)
  "Convert PLIST, a source token (:TOKEN .. :IN-PACKAGE ..) from
CODE-REFS-SCAN's %DEFINITION-SOURCE-SIGNATURE (spec 3.2), to the JSON token
CL-MCP/SRC/CLOS-VERIFY-CORE:VERIFY-ENTRIES documents, or NIL."
  (and plist
       (make-ht "token" (getf plist :token) "in_package" (getf plist :in-package))))

(defun %json-tokens (plists)
  "Convert PLISTS, a list of source tokens, to a JSON array."
  (map 'vector #'%json-token (sequence->list plists)))

(defun %json-name (plist)
  "Convert PLIST, a source name (:TOKEN .. :SETF .. :IN-PACKAGE ..) from
%DEFINITION-SOURCE-SIGNATURE, to JSON, or NIL."
  (and plist
       (make-ht "token" (getf plist :token)
                "setf" (and (getf plist :setf) t)
                "in_package" (getf plist :in-package))))

(defun %json-eql-datum (plist)
  "Convert PLIST, a tagged EQL datum (spec 3.3), to JSON."
  (ecase (getf plist :kind)
    (:keyword (make-ht "kind" "keyword" "name" (getf plist :name)))
    (:integer (make-ht "kind" "integer" "value" (getf plist :value)))
    (:ratio (make-ht "kind" "ratio"
                     "numerator" (getf plist :numerator)
                     "denominator" (getf plist :denominator)))
    (:character (make-ht "kind" "character" "value" (getf plist :value)))
    (:boolean (make-ht "kind" "boolean" "value" (getf plist :value)))
    (:symbol
     (let ((ht (make-ht "kind" "symbol"
                        "token" (getf plist :token)
                        "in_package" (getf plist :in-package)
                        "quoted" (string-downcase (symbol-name (getf plist :quoted))))))
       (when (getf plist :quote-token)
         (setf (gethash "quote_token" ht) (%json-token (getf plist :quote-token))))
       ht))
    (:unverifiable (make-ht "kind" "unverifiable" "reason" (getf plist :reason)))))

(defun %json-specializer (plist)
  "Convert PLIST, a specializer (spec 3.2/3.3: kind class, eql or
unverifiable), to JSON."
  (ecase (getf plist :kind)
    (:class (make-ht "kind" "class"
                     "token" (getf plist :token)
                     "in_package" (getf plist :in-package)))
    (:eql (make-ht "kind" "eql" "datum" (%json-eql-datum (getf plist :datum))))
    (:unverifiable (make-ht "kind" "unverifiable" "reason" (getf plist :reason)))))

(defun %json-specializers (plists)
  "Convert PLISTS, a list of specializers, to a JSON array, or NIL."
  (and plists (map 'vector #'%json-specializer (sequence->list plists))))

(defun %json-slot (plist)
  "Convert PLIST, a slot (:NAME :READERS :WRITERS) from
%DEFINITION-SOURCE-SIGNATURE, to JSON."
  (make-ht "name" (%json-token (getf plist :name))
           "readers" (%json-tokens (getf plist :readers))
           "writers" (%json-tokens (getf plist :writers))))

(defun %json-method-option (plist)
  "Convert PLIST, a DEFGENERIC (:method ...) option, to JSON."
  (make-ht "qualifiers" (%json-tokens (getf plist :qualifiers))
           "specializers" (%json-specializers (getf plist :specializers))))

(defun %json-signature (plist)
  "Convert PLIST, TOP-LEVEL-FORMS-AT's :SIGNATURE (CODE-REFS-SCAN's
%DEFINITION-SOURCE-SIGNATURE, spec 3.2), to the candidate JSON
CL-MCP/SRC/CLOS-VERIFY-CORE:VERIFY-ENTRIES documents."
  (let* ((kind (getf plist :kind))
         (ht (make-ht "kind" (string-downcase (symbol-name kind)))))
    (when (getf plist :head)
      (setf (gethash "head" ht) (%json-token (getf plist :head))))
    (case kind
      (:defmethod
        (setf (gethash "name" ht) (%json-name (getf plist :name))
              (gethash "qualifiers" ht) (%json-tokens (getf plist :qualifiers))
              (gethash "specializers" ht) (%json-specializers (getf plist :specializers))))
      (:defgeneric
        (setf (gethash "name" ht) (%json-name (getf plist :name))
              (gethash "methods" ht)
              (map 'vector #'%json-method-option (sequence->list (getf plist :methods)))))
      ((:defclass :define-condition)
        (setf (gethash "name" ht) (%json-name (getf plist :name))
              (gethash "slots" ht)
              (and (getf plist :slots)
                   (map 'vector #'%json-slot (sequence->list (getf plist :slots))))))
      (:defstruct
        (setf (gethash "name" ht) (%json-name (getf plist :name)))))
    ht))

(defparameter *reason-verification-unavailable* "verification unavailable"
  "SOURCE_MATCH_REASON when the clos-verify-source step could not be
completed: a worker error, a crash notice, or any signalled condition
(spec 3.6).  Every located entry falls back to UNVERIFIED and the report
is still returned; the tool call itself never fails.")

(defparameter *reason-not-locatable* "not uniquely locatable for editing"
  "SOURCE_MATCH_REASON for a MATCHED verdict LOCATE-FORM-IN-NODES cannot
confirm with a unique, same-span round trip (spec 3.5): lisp-edit-form's
own matching would not resolve FORM_TYPE/FORM_NAME back to this exact
form, so returning them would invite an edit the JSON does not support.")

(defparameter *reason-not-readable* "file is outside the readable paths"
  "SOURCE_MATCH_REASON when TOP-LEVEL-FORMS-AT's read policy refuses the
file: every located entry still gets a SOURCE_MATCH (spec 3.1), even one
whose file cannot be opened at all.")

(defparameter *reason-no-source-line* "no source line recorded"
  "SOURCE_MATCH_REASON when ENTRY has a source file (PATH/ABS_PATH) but no
LINE: DEFINITION-SOURCE-LOCATION computes them independently and either may
be NIL (src/code-core.lisp).  There is no line to scan TOP-LEVEL-FORMS-AT
for, so this is decided locally, without a VERIFY-FN round trip.")

(defparameter *reason-source-not-on-disk* "source not on disk"
  "SOURCE_MATCH_REASON when ENTRY has a display PATH -- DEFINITION-SOURCE-
LOCATION still names one, such as \"repl-eval\" for a definition made
through repl-eval -- but no usable ABS_PATH: there is no file to read a
candidate from at all, so this is decided by ANNOTATE-REPORT-FORMS's
closing sweep, without ever reaching TOP-LEVEL-FORMS-AT.")

(defparameter *reason-no-source-recorded* "no source recorded"
  "SOURCE_MATCH_REASON when ENTRY has neither PATH nor ABS_PATH: the image
recorded no source location for it at all.  Set by ANNOTATE-REPORT-FORMS's
closing sweep so every located entry ends with a SOURCE_MATCH (spec 3.1),
even one %LOCATION-TEXT renders as \"(no source)\".")

(defun %verification-results (raw)
  "Return RAW's \"results\" array as a list, or NIL when RAW is not a valid
{\"results\": [...]} object -- a worker error or crash notice
(PROXY-TO-WORKER's shape for either), which carries \"content\"/\"isError\"
and no \"results\" key."
  (and (hash-table-p raw) (nth-value 1 (gethash "results" raw))
       (sequence->list (gethash "results" raw))))

(defun %call-verify-fn (verify-fn entries)
  "Call VERIFY-FN with ENTRIES (spec 3.6's batch) and return its results
list, or NIL when verification is unavailable: VERIFY-FN signalled a
condition, or its return value is not a valid results object."
  (handler-case (%verification-results (funcall verify-fn entries))
    (error () nil)))

(defun %edit-unit (identity signature-kind)
  "Return the container form_type SIGNATURE-KIND (a %DEFINITION-SOURCE-SIGNATURE
:KIND, spec 3.2) edits on IDENTITY's behalf, or NIL when IDENTITY's own
form is its edit unit (spec 3.4).  Only a method IDENTITY matched through a
DEFGENERIC's inline (:method ...) option or a DEFCLASS/DEFINE-CONDITION's
slot options has a container: editing FORM_TYPE/FORM_NAME there changes
more than this one IDENTITY."
  (when (equal (gethash "kind" identity) "method")
    (case signature-kind
      (:defgeneric "defgeneric")
      (:defclass "defclass")
      (:define-condition "define-condition")
      (t nil))))

(defun %file-nodes (abs-path cache)
  "Return ABS-PATH's parsed top-level CST nodes, memoized in CACHE (an
EQUAL hash table), or NIL when the file cannot be read or parsed -- a
MATCHED verdict there then cannot be round-trip-confirmed (spec 3.5) and
falls back to UNVERIFIED."
  (multiple-value-bind (value found) (gethash abs-path cache)
    (if found
        value
        (setf (gethash abs-path cache)
              (ignore-errors
                (parse-top-level-forms (fs-read-source-text abs-path)
                                       :source-path (pathname abs-path)))))))

(defun %round-trip-ok-p (nodes form-type form-name start end)
  "True when NODES resolve FORM-TYPE and FORM-NAME (LOCATE-FORM-IN-NODES,
the same matching lisp-edit-form uses) to the single CST node spanning
exactly START/END (spec 3.5) -- comparing spans, not line numbers, and
never accepting an ambiguous or absent match."
  (and nodes
       (multiple-value-bind (node reason) (locate-form-in-nodes nodes form-type form-name)
         (and node (null reason)
              (= (cst-node-start node) start) (= (cst-node-end node) end)))))

(defun %set-source-match (entry status reason)
  "Set ENTRY's SOURCE_MATCH and SOURCE_MATCH_REASON (spec 3.1); REASON is
NIL exactly when STATUS is \"matched\"."
  (setf (gethash "source_match" entry) status
        (gethash "source_match_reason" entry) reason))

(defun %set-matched-form (entry candidate identity)
  "Set ENTRY's FORM_TYPE and FORM_NAME from CANDIDATE, a TOP-LEVEL-FORMS-AT
plist, and its EDIT_UNIT (spec 3.4) when IDENTITY's own form is not
CANDIDATE's -- after a MATCHED verdict's round trip is confirmed."
  (setf (gethash "form_type" entry) (getf candidate :form-type)
        (gethash "form_name" entry) (getf candidate :form-name))
  (let ((edit-unit (%edit-unit identity (getf (getf candidate :signature) :kind))))
    (when edit-unit
      (setf (gethash "edit_unit" entry) edit-unit))))

(defun %apply-verification (entry forms result node-cache)
  "Set ENTRY's SOURCE_MATCH (and, once confirmed, FORM_TYPE/FORM_NAME/
EDIT_UNIT) from RESULT, worker/clos-verify-source's verdict for ENTRY's
candidates FORMS (spec 3.1, 3.5), or *REASON-VERIFICATION-UNAVAILABLE* when
RESULT is NIL.  A STATUS other than \"matched\"/\"mismatched\"/\"unverified\"
-- a future verifier version skew -- is clamped to \"unverified\" naming the
unexpected value, never passed through as-is: the three-word contract holds
regardless of what the worker sends.  A stale ENTRY (spec 3.1) never keeps a
MATCHED verdict."
  (if (null result)
      (%set-source-match entry "unverified" *reason-verification-unavailable*)
      (let ((status (gethash "status" result))
            (reason (gethash "reason" result)))
        (cond
          ((equal status "matched")
           (let* ((index (gethash "candidate_index" result))
                  (candidate (and (integerp index) (nth index forms)))
                  (nodes (and candidate
                              (%file-nodes (gethash "abs_path" entry) node-cache))))
             (if (and candidate
                      (%round-trip-ok-p nodes (getf candidate :form-type)
                                        (getf candidate :form-name)
                                        (getf candidate :start) (getf candidate :end)))
                 (progn
                   (%set-source-match entry "matched" nil)
                   (%set-matched-form entry candidate (gethash "identity" entry)))
                 (%set-source-match entry "unverified" *reason-not-locatable*))))
          ((member status '("mismatched" "unverified") :test #'equal)
           (%set-source-match entry status reason))
          (t
           (%set-source-match entry "unverified"
                               (format nil "unexpected verifier status ~S" status))))))
  (when (and (%true-p (gethash "stale" entry))
             (equal (gethash "source_match" entry) "matched"))
    (setf (gethash "form_type" entry) nil (gethash "form_name" entry) nil)
    (remhash "edit_unit" entry)
    (%set-source-match entry "unverified" *note-stale*)))

(defun annotate-report-forms (report verify-fn)
  "Fill in the form_type, form_name, source_match, source_match_reason and
(spec 3.4) edit_unit of every located object in REPORT, then remove
abs_path from each; return REPORT.

Each file's top-level forms starting on a located line are scanned once
(TOP-LEVEL-FORMS-AT) and converted to the candidate JSON spec 3.2 defines
(%JSON-SIGNATURE); every located entry, across every file, is sent to
VERIFY-FN in one batch (spec 3.6) -- a callback CLOS.LISP supplies, calling
worker/clos-verify-source over the pool or CLOS-VERIFY-CORE:VERIFY-ENTRIES
in-process.  A MATCHED verdict only reaches FORM_TYPE/FORM_NAME once
LOCATE-FORM-IN-NODES confirms it resolves back to that very CST span (spec
3.5); anything else -- a real mismatch, an unresolvable identity, a
signalled condition, or a non-conforming VERIFY-FN result -- is MISMATCHED
or UNVERIFIED, never a silent fallback to MATCHED.

A line with no candidates at all -- the file could not be read
(*REASON-NOT-READABLE*) or parsed (*NOTE-UNPARSEABLE*), or simply starts no
top-level form (*NOTE-NO-FORM-AT-LINE*) -- is decided locally, without a
VERIFY-FN round trip: there is nothing to send.  An entry with a source file
but no LINE at all (*REASON-NO-SOURCE-LINE*: DEFINITION-SOURCE-LOCATION
computes them independently, spec src/code-core.lisp) is decided the same
way, since there is no line to scan for.

Every located entry ends this function with a SOURCE_MATCH: this is an
invariant, not a case analysis left to each caller.  A closing sweep gives
any entry the scan above never touched -- one with a display PATH but no
usable ABS_PATH (a REPL-defined method's PATH is the literal string
\"repl-eval\", never a file; *REASON-SOURCE-NOT-ON-DISK*), or one with
neither at all (*REASON-NO-SOURCE-RECORDED*) -- UNVERIFIED with the reason
that fits.  %LOCATION-TEXT relies on this: it renders EVERY entry's
SOURCE_MATCH, including the \"(no source)\" case, so the text can never
assert a state (or its absence) the JSON disagrees with."
  (let ((by-file (make-hash-table :test #'equal))
        (contexts (make-hash-table :test #'equal))
        (node-cache (make-hash-table :test #'equal))
        (entries-json '())
        (counter 0))
    (dolist (entry (%located-entries report))
      (let ((abs-path (gethash "abs_path" entry)))
        (cond
          ((not (stringp abs-path)))
          ((integerp (gethash "line" entry)) (push entry (gethash abs-path by-file)))
          (t (%set-source-match entry "unverified" *reason-no-source-line*)))))
    (maphash
     (lambda (abs-path file-entries)
       (multiple-value-bind (table failure)
           (top-level-forms-at abs-path
                               (mapcar (lambda (e) (gethash "line" e)) file-entries))
         (dolist (entry file-entries)
           (let ((forms (gethash (gethash "line" entry) table)))
             (cond
               (forms
                (let ((id (format nil "~D" (incf counter))))
                  (setf (gethash id contexts) (list entry forms))
                  (push (make-ht "id" id "identity" (gethash "identity" entry)
                                 "candidates"
                                 (map 'vector
                                      (lambda (f) (%json-signature (getf f :signature)))
                                      forms))
                        entries-json)))
               ((eq failure :denied)
                (%set-source-match entry "unverified" *reason-not-readable*))
               (failure
                (%set-source-match entry "unverified"
                                   (format nil "~A: ~A" *note-unparseable* failure)))
               (t (%set-source-match entry "unverified" *note-no-form-at-line*)))))))
     by-file)
    (let* ((batch (coerce (nreverse entries-json) 'vector))
           (results (and (plusp (length batch)) (%call-verify-fn verify-fn batch)))
           (results-by-id (make-hash-table :test #'equal)))
      (dolist (result results)
        (setf (gethash (gethash "id" result) results-by-id) result))
      (maphash (lambda (id context)
                 (destructuring-bind (entry forms) context
                   (%apply-verification entry forms (gethash id results-by-id) node-cache)))
               contexts))
    (dolist (entry (%located-entries report))
      (when (null (gethash "source_match" entry))
        (%set-source-match entry "unverified"
                            (if (gethash "path" entry)
                                *reason-source-not-on-disk*
                                *reason-no-source-recorded*))))
    (dolist (entry (%located-entries report))
      (remhash "abs_path" entry))
    report))

(defun %home-package-name (report)
  "Return the package name of REPORT's resolved symbol, or NIL."
  (let* ((resolved (gethash "resolved_symbol" report))
         (colon (and (stringp resolved) (position #\: resolved))))
    (and colon (plusp colon) (subseq resolved 0 colon))))

(defun %short (text home)
  "Return TEXT, names printed fully qualified, without the prefixes of HOME and
COMMON-LISP, as a reader in HOME would write them."
  (let ((result (or text "")))
    (dolist (package (remove nil (list home "COMMON-LISP")) result)
      (setf result (regex-replace-all
                    (format nil "(?<![^\\s(])~A::?" (quote-meta-chars package))
                    result "")))))

(defun %location-text (entry)
  "Return where ENTRY is defined: PATH:LINE, or (no source) when ENTRY has no
PATH at all, followed by (FORM_TYPE FORM_NAME) -- with an extra
[edit_unit: X] when editing FORM_TYPE/FORM_NAME edits a container around
ENTRY, not ENTRY's own form (spec 3.4) -- when SOURCE_MATCH is \"matched\",
or [STATE: REASON] otherwise (spec 3.1), so the text never invites an edit
the JSON does not support, and never shows a bracketed state (or its
absence) the JSON disagrees with: ANNOTATE-REPORT-FORMS guarantees every
located entry -- (no source) ones included -- ends with a SOURCE_MATCH.
ENTRY's NOTE, when present -- a live-object read failure unrelated to
source matching -- is always appended in its own bracket."
  (let* ((path (gethash "path" entry))
         (line (gethash "line" entry))
         (match (gethash "source_match" entry))
         (note (gethash "note" entry))
         (location (if path (format nil "~A~@[:~D~]" path line) "(no source)"))
         (body
           (if (equal match "matched")
               (let* ((form-type (gethash "form_type" entry))
                      (form-name (gethash "form_name" entry))
                      (edit-unit (gethash "edit_unit" entry))
                      (form-text (and form-type (format nil "~A~@[ ~A~]" form-type form-name))))
                 (format nil "~A~@[ (~A)~]~@[  [edit_unit: ~A]~]" location form-text edit-unit))
               (format nil "~A [~A~@[: ~A~]]" location (or match "unverified")
                       (gethash "source_match_reason" entry)))))
    (concatenate 'string body (if note (format nil "  [~A]" note) ""))))

(defun %method-signature (method home &key with-name class-name)
  "Return METHOD's signature: [NAME] QUALIFIERS (SPECIALIZERS) [kind], plus
'via CLASS' when it was found through a superclass of CLASS-NAME."
  (let ((qualifiers (sequence->list (gethash "qualifiers" method)))
        (specializers (sequence->list (gethash "specializers" method)))
        (kind (gethash "kind" method))
        (via (gethash "via" method)))
    (%short (format nil "~@[~A ~]~{~A ~}(~{~A~^ ~})~:[ [~A]~;~*~]~@[ via ~A~]"
                    (and with-name (gethash "generic_function" method))
                    qualifiers specializers
                    (equal kind "method") kind
                    (and via (not (equal via class-name)) via))
            home)))

(defun %write-methods (stream methods home &key with-name class-name)
  "Write one aligned line per method in METHODS to STREAM."
  (let* ((signatures (mapcar (lambda (method)
                               (%method-signature method home
                                                  :with-name with-name
                                                  :class-name class-name))
                             methods))
         (width (reduce #'max signatures :key #'length :initial-value 0)))
    (loop for method in methods
          for signature in signatures
          do (format stream "  ~vA  ~A~%" width signature (%location-text method)))))

(defun %write-more (stream entry)
  "Write how many of ENTRY's methods were left out, when any were."
  (let ((count (or (gethash "method_count" entry) 0))
        (shown (length (sequence->list (gethash "methods" entry)))))
    (when (> count shown)
      (format stream "  … and ~D more (raise limit to see them)~%" (- count shown)))))

(defun %write-generic-function (stream gf home)
  "Write the text for GF, one generic_functions entry, to STREAM."
  (let ((methods (sequence->list (gethash "methods" gf))))
    (format stream "Generic function ~A ~A — ~@[~(~A~) combination, ~]~D method~:P~%"
            (gethash "name" gf)
            (%short (gethash "lambda_list" gf) home)
            (gethash "method_combination" gf)
            (or (gethash "method_count" gf) 0))
    (when (gethash "documentation" gf)
      (format stream "~A~%" (gethash "documentation" gf)))
    (if (gethash "path" gf)
        (format stream "Defined at ~A~%" (%location-text gf))
        (format stream "No defgeneric: created implicitly by a defmethod or a slot accessor.~%"))
    (%write-methods stream methods home)
    (%write-more stream gf)))

(defun %accessor-words (slot home)
  "Return the reader, writer and accessor words for SLOT: accessor X when X
reads it and (SETF X) writes it."
  (let ((readers (mapcar (lambda (name) (%short name home))
                         (sequence->list (gethash "readers" slot))))
        (writers (mapcar (lambda (name) (%short name home))
                         (sequence->list (gethash "writers" slot))))
        (words '()))
    (dolist (reader readers)
      (let ((writer (format nil "(SETF ~A)" reader)))
        (if (member writer writers :test #'string=)
            (progn (push (format nil "accessor ~A" reader) words)
                   (setf writers (remove writer writers :test #'string=)))
            (push (format nil "reader ~A" reader) words))))
    (dolist (writer writers)
      (push (format nil "writer ~A" writer) words))
    (nreverse words)))

(defun %slot-parts (slot class-name home)
  "Return (NAME ORIGIN ATTRIBUTES), the three columns of SLOT's text line:
its name, 'direct' or 'from CLASS' when a superclass of CLASS-NAME defines it,
and its initargs, initform, type, class allocation and accessors."
  (let ((from (gethash "from" slot))
        (type (gethash "type" slot)))
    (list (%short (gethash "name" slot) home)
          (if (and from (not (equal from class-name)))
              (format nil "from ~A" (%short from home))
              "direct")
          (format nil "~{ :initarg ~A~}~@[ :initform ~A~]~@[ :type ~A~]~
~:[~; :allocation :class~]~{  ~A~}"
                  (sequence->list (gethash "initargs" slot))
                  (gethash "initform" slot)
                  (and type (not (equal type "T")) type)
                  (equal (gethash "allocation" slot) "class")
                  (%accessor-words slot home)))))

(defun %write-class (stream class home)
  "Write the text for CLASS, the report's class entry, to STREAM."
  (let* ((name (gethash "name" class))
         (cpl (gethash "precedence_list" class))
         ;; Effective slots exist exactly when the precedence list does.  Test
         ;; that, not the slots: after the worker's JSON round trip an empty
         ;; array and null are both NIL.
         (slots (sequence->list (gethash (if cpl "effective_slots" "direct_slots") class)))
         (initargs (sequence->list (gethash "default_initargs" class)))
         (omitted (sequence->list (gethash "omitted_classes" class))))
    (format stream "Class ~A (~(~A~)~:[, not finalized~;~]) — ~A~%"
            name
            (%short (gethash "metaclass" class) home)
            (%true-p (gethash "finalized" class))
            (%location-text class))
    (when (gethash "documentation" class)
      (format stream "~A~%" (gethash "documentation" class)))
    (format stream "Superclasses: ~:[(none)~;~:*~{~A~^ ~}~]~%"
            (mapcar (lambda (c) (%short c home))
                    (sequence->list (gethash "direct_superclasses" class))))
    (format stream "Subclasses: ~:[(none)~;~:*~{~A~^ ~}~]~%"
            (mapcar (lambda (c) (%short c home))
                    (sequence->list (gethash "direct_subclasses" class))))
    (if cpl
        (format stream "Precedence: ~{~A~^ ~}~%"
                (mapcar (lambda (c) (%short c home)) (sequence->list cpl)))
        (format stream "Precedence: unavailable (undefined superclass ~{~A~^, ~})~%"
                (mapcar (lambda (c) (%short c home))
                        (sequence->list (gethash "undefined_superclasses" class)))))
    (format stream "~:[Direct slots~;Slots~] (~D):~%" cpl (length slots))
    (let* ((parts (mapcar (lambda (slot) (%slot-parts slot name home)) slots))
           (name-width (reduce #'max parts :key (lambda (p) (length (first p)))
                                           :initial-value 0))
           (origin-width (reduce #'max parts :key (lambda (p) (length (second p)))
                                             :initial-value 0)))
      (dolist (part parts)
        (format stream "~A~%"
                (string-right-trim " " (format nil "  ~vA  ~vA~A"
                                               name-width (first part)
                                               origin-width (second part)
                                               (third part))))))
    (format stream "Default initargs:~:[ (none)~;~]~%" initargs)
    (dolist (initarg initargs)
      (format stream "  ~A ~A~:[~; from ~A~]~%"
              (gethash "initarg" initarg)
              (%short (gethash "form" initarg) home)
              (not (equal (gethash "from" initarg) name))
              (%short (gethash "from" initarg) home)))
    (format stream "Methods (~D~@[; standard protocol on ~{~A~^, ~} omitted~]):~%"
            (or (gethash "method_count" class) 0)
            (and omitted (mapcar (lambda (c) (%short c home)) omitted)))
    (%write-methods stream (sequence->list (gethash "methods" class)) home
                    :with-name t :class-name name)
    (%write-more stream class)))

(defun %format-clos-report (report)
  "Return the content text for REPORT, an annotated clos-describe payload."
  (let ((status (gethash "symbol_status" report))
        (home (%home-package-name report))
        (gfs (sequence->list (gethash "generic_functions" report)))
        (class (gethash "class" report)))
    (with-output-to-string (s)
      (cond
        ((equal status "package_not_found")
         (format s "Package ~S not found (nothing was interned). ~
                    Load the system that defines it with load-system.~%"
                 (gethash "lookup_package" report)))
        ((equal status "not_found")
         (format s "Symbol ~S not found in ~A (nothing was interned). ~
                    Is the system loaded? Run load-system first.~%"
                 (gethash "lookup_name" report) (gethash "lookup_package" report)))
        ((and (null gfs) (not (hash-table-p class)))
         (if (equal (gethash "symbol_kind" report) "unbound")
             (format s "~A names nothing in this image. Is the system loaded?~%"
                     (gethash "resolved_symbol" report))
             (format s "~A names a ~A, not a generic function or class; ~
                        code-describe describes it.~%"
                     (gethash "resolved_symbol" report)
                     (gethash "symbol_kind" report))))
        (t
         (loop for (gf . more) on gfs
               do (%write-generic-function s gf home)
                  (when (or more (hash-table-p class))
                    (terpri s)))
         (when (hash-table-p class)
           (%write-class s class home))))
      (dolist (note (sequence->list (gethash "notes" report)))
        (format s "Note: ~A~%" note)))))

(defun build-clos-describe-response (report verify-fn)
  "Return REPORT, a clos-describe payload, annotated (VERIFY-FN, spec 3.6)
and with its content text.

A result that is not a report (CLOS-REPORT-P), such as the error
PROXY-TO-WORKER returns when the worker crashed, is returned unchanged;
VERIFY-FN is never called for it."
  (if (clos-report-p report)
      (progn
        (annotate-report-forms report verify-fn)
        (setf (gethash "content" report)
              (text-content (%format-clos-report report)))
        report)
      report))
