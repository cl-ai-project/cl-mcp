;;;; src/project-scaffold-core.lisp
;;;;
;;;; Pure helpers for project-scaffold: input validation, template rendering,
;;;; path math, and file manifest construction. No I/O. No worker interaction.
;;;; This module exists so that the effectful layer in project-scaffold.lisp
;;;; stays thin and the bulk of the logic is trivially unit-testable.

(defpackage #:cl-mcp/src/project-scaffold-core
  (:use #:cl)
  (:import-from #:cl-ppcre
                #:scan
                #:regex-replace-all)
  (:import-from #:cl-mcp/src/project-scaffold-templates
                #:*asd-template-rove*
                #:*asd-template-fiveam*
                #:*claude-md-template*
                #:*agents-md-template*
                #:*readme-template*
                #:*gitignore-template*
                #:*main-lisp-template*
                #:*main-test-template-rove*
                #:*main-test-template-fiveam*)
  (:export #:validate-project-name
           #:validate-destination
           #:validate-text-field
           #:validate-framework
           #:*supported-frameworks*
           #:*default-framework*
           #:escape-lisp-string
           #:render-template
           #:compute-parent-prompts-path
           #:plan-scaffold
           #:*scaffold-marker-file*
           #:scaffold-marker-content
           #:invalid-argument-error
           #:invalid-argument-field
           #:invalid-argument-value
           #:invalid-argument-reason))

(in-package #:cl-mcp/src/project-scaffold-core)

(define-condition invalid-argument-error (error)
  ((field :initarg :field :reader invalid-argument-field)
   (value :initarg :value :reader invalid-argument-value)
   (reason :initarg :reason :reader invalid-argument-reason))
  (:documentation "Signaled when a project-scaffold input argument is invalid.")
  (:report
   (lambda (condition stream)
     (format stream "Invalid argument ~A = ~S: ~A"
             (invalid-argument-field condition)
             (invalid-argument-value condition)
             (invalid-argument-reason condition)))))

(setf (documentation 'invalid-argument-field 'function)
      "Return the name of the offending field from an INVALID-ARGUMENT-ERROR.")

(setf (documentation 'invalid-argument-value 'function)
      "Return the rejected value from an INVALID-ARGUMENT-ERROR.")

(setf (documentation 'invalid-argument-reason 'function)
      "Return the human-readable reason string from an INVALID-ARGUMENT-ERROR.")

(defparameter *project-name-regex* "^[a-z][a-z0-9-]*$"
  "Regular expression that valid project names must fully match.")

(defparameter *project-name-max-length* 64
  "Maximum allowed length for a project name.")

(defun validate-project-name (name)
  "Return NAME unchanged when valid, else signal INVALID-ARGUMENT-ERROR.
A valid name is a non-empty string of length at most *PROJECT-NAME-MAX-LENGTH*
that fully matches *PROJECT-NAME-REGEX*: a lower-case letter followed by
lower-case letters, digits, or hyphens."
  (unless (stringp name)
    (error 'invalid-argument-error
           :field "name" :value name
           :reason "must be a string"))
  (when (zerop (length name))
    (error 'invalid-argument-error
           :field "name" :value name
           :reason "must not be empty"))
  (when (> (length name) *project-name-max-length*)
    (error 'invalid-argument-error
           :field "name" :value name
           :reason (format nil "must be at most ~D characters"
                           *project-name-max-length*)))
  (unless (cl-ppcre:scan *project-name-regex* name)
    (error 'invalid-argument-error
           :field "name" :value name
           :reason (format nil "must match ~A" *project-name-regex*)))
  name)

(defun validate-destination (destination)
  "Return DESTINATION when it is a safe relative path, else signal error.
A valid destination is a non-empty relative path with no absolute segment
and no '..' component. Empty strings and NIL are rejected."
  (unless (and (stringp destination) (plusp (length destination)))
    (error 'invalid-argument-error
           :field "destination" :value destination
           :reason "must be a non-empty string"))
  (when (char= (char destination 0) #\/)
    (error 'invalid-argument-error
           :field "destination" :value destination
           :reason "must be a relative path (no leading /)"))
  (dolist (segment (uiop:split-string destination :separator "/"))
    (when (string= segment "..")
      (error 'invalid-argument-error
             :field "destination" :value destination
             :reason "must not contain '..' path segments")))
  destination)

(defun validate-text-field (field-name value)
  "Return VALUE when it is an acceptable free-text field, else signal.
FIELD-NAME is included in the error for caller-side diagnostics. A valid
value is a string containing none of: newline (#\\Newline), carriage
return (#\\Return), double quote, or backslash. The last two are rejected
because these fields are interpolated into Lisp string literals in the
generated .asd, where an unescaped quote would close the literal and turn
the remainder of the value into evaluated code. Empty strings are
allowed."
  (unless (stringp value)
    (error 'invalid-argument-error
           :field field-name :value value
           :reason "must be a string"))
  (when (or (find #\Newline value) (find #\Return value))
    (error 'invalid-argument-error
           :field field-name :value value
           :reason "must not contain newline characters"))
  (when (or (find #\" value) (find #\\ value))
    (error 'invalid-argument-error
           :field field-name :value value
           :reason "must not contain double quote or backslash characters"))
  value)

(defparameter *supported-frameworks* '(:rove :fiveam)
  "Test frameworks PROJECT-SCAFFOLD can generate a project for.
Deliberately narrower than what run-tests can execute: run-tests also
drives Prove and an ASDF fallback, but only frameworks with a complete
.asd plus test-file template pair belong here.  Accepting a name without
templates would generate a Rove project and label it as something else,
which is worse than refusing the name.")

(defparameter *default-framework* :rove
  "Framework used when the caller names none.
Rove, because that is what every scaffold generated before the framework
argument existed shipped with, and what the prompts @-included by the
generated CLAUDE.md describe.")

(defun validate-framework (framework)
  "Return FRAMEWORK as one of *SUPPORTED-FRAMEWORKS*, else signal.
FRAMEWORK is a string or symbol designator compared case-insensitively,
so both \"fiveam\" and FiveAM's own \"FiveAM\" spelling resolve to
:FIVEAM. NIL selects *DEFAULT-FRAMEWORK*, which is what keeps the
argument optional for callers written before it existed. Any other value
signals INVALID-ARGUMENT-ERROR."
  (cond
    ((null framework) *default-framework*)
    ((or (stringp framework) (symbolp framework))
     (let ((name (string framework)))
       (or (find-if (lambda (supported)
                      (string-equal name (symbol-name supported)))
                    *supported-frameworks*)
           (error 'invalid-argument-error
                  :field "framework" :value framework
                  :reason (format nil "must be one of ~{~(~A~)~^, ~}"
                                  *supported-frameworks*)))))
    (t
     (error 'invalid-argument-error
            :field "framework" :value framework
            :reason "must be a string or symbol"))))

(defun escape-lisp-string (value)
  "Return VALUE escaped for embedding inside a Lisp string literal.
Every backslash and every double quote in VALUE is prefixed with a
backslash, so the result can be interpolated between two double quotes
without terminating the literal. This is the substitution-boundary half
of the defence against .asd template injection; VALIDATE-TEXT-FIELD
rejects the same characters up front."
  (with-output-to-string (out)
    (loop for ch across value
          do (when (or (char= ch #\\) (char= ch #\"))
               (write-char #\\ out))
             (write-char ch out))))

(defun render-template (template bindings)
  "Return TEMPLATE with each '{{key}}' substituted using BINDINGS.
BINDINGS is an alist of (KEY-STRING . VALUE-STRING). Unknown placeholders
are left intact. Values are substituted literally; regex metacharacters
in values (including backslash and dollar sign) are handled safely via
cl-ppcre's :simple-calls replacement callback."
  (cl-ppcre:regex-replace-all
   "\\{\\{([A-Za-z_-][A-Za-z0-9_-]*)\\}\\}"
   template
   (lambda (match &rest registers)
     (declare (ignore match))
     (let* ((key (first registers))
            (entry (assoc key bindings :test #'string=)))
       (if entry
           (cdr entry)
           (format nil "{{~A}}" key))))
   :simple-calls t))

(defun compute-parent-prompts-path (destination)
  "Return the relative path from DESTINATION/<name> back to <root>/prompts.
DESTINATION is a slash-separated relative directory (e.g. \"scaffolds\",
\"work/samples\"). The returned path goes up by one '..' for every real
path segment in DESTINATION plus one for the project directory itself,
then descends into 'prompts'.

Empty segments (from consecutive slashes) and '.' segments are skipped
when counting depth, because they do not contribute a directory level
on disk. '..' segments are rejected upstream by VALIDATE-DESTINATION,
so they never reach this function."
  (let* ((raw-segments (uiop:split-string destination :separator "/"))
         (segments (remove-if (lambda (s)
                                (or (zerop (length s))
                                    (string= s ".")))
                              raw-segments))
         (depth (1+ (length segments)))
         (ups (with-output-to-string (s)
                (dotimes (i depth)
                  (declare (ignore i))
                  (write-string "../" s)))))
    (concatenate 'string ups "prompts")))

(defparameter *scaffold-marker-file* ".cl-mcp-scaffold"
  "Name of the ownership marker file written into every generated scaffold.
Its presence is the only proof project-scaffold accepts that a directory
was generated by this tool, and therefore the only thing that authorizes
replacing that directory when OVERWRITE is true. Directories without the
marker are never deleted.")

(defun scaffold-marker-content (name files)
  "Return the text of the ownership marker for scaffold NAME.
FILES is the list of generated relative path strings. The marker is a
comment header followed by a single readable plist holding the manifest,
so tooling can recover what was generated. NAME has already passed
VALIDATE-PROJECT-NAME and FILES comes from the fixed template manifest,
so neither can smuggle extra forms into the marker."
  (let ((*print-length* nil)
        (*print-level* nil)
        (*print-pretty* nil)
        (*print-readably* nil))
    (with-output-to-string (out)
      (write-line ";;;; cl-mcp project-scaffold ownership marker." out)
      (write-line ";;;; Its presence authorizes the project-scaffold tool to replace" out)
      (write-line ";;;; this directory when overwrite is true. Delete it to opt out." out)
      (format out "(:generator \"cl-mcp/project-scaffold\" :version 1~% :name ~S~% :files ~S)~%"
              name files))))

(defun %framework-templates (framework)
  "Return (values ASD-TEMPLATE MAIN-TEST-TEMPLATE LABEL) for FRAMEWORK.
FRAMEWORK is a keyword already normalized by VALIDATE-FRAMEWORK, so the
ECASE can only be reached with a supported one. LABEL is the framework's
own spelling of its name, substituted into the generated Markdown as
{{test-framework}}."
  (ecase framework
    (:rove (values *asd-template-rove* *main-test-template-rove* "Rove"))
    (:fiveam (values *asd-template-fiveam* *main-test-template-fiveam*
                     "FiveAM"))))

(defun plan-scaffold (&key name description author license destination framework)
  "Return an alist of (RELATIVE-PATH . CONTENT) for the scaffold manifest.
Applies all template substitutions but performs no I/O. Two binding sets
are used: the .asd template drops its values inside Lisp string literals,
so those are escaped with ESCAPE-LISP-STRING, while the Markdown and
source templates receive the raw values.

FRAMEWORK selects which .asd and tests/main-test.lisp templates are used
and which name the generated Markdown gives the test framework; it is
normalized through VALIDATE-FRAMEWORK, so NIL means *DEFAULT-FRAMEWORK*
and an unsupported name signals rather than silently generating the
default. The other arguments are assumed to be already validated,
normalized strings."
  (let ((framework (validate-framework framework))
        (parent-prompts (compute-parent-prompts-path destination)))
    (multiple-value-bind (asd-template main-test-template framework-label)
        (%framework-templates framework)
      (let* ((raw-bindings `(("name" . ,name)
                             ("description" . ,description)
                             ("author" . ,author)
                             ("license" . ,license)
                             ("parent-prompts" . ,parent-prompts)
                             ("test-framework" . ,framework-label)))
             (asd-bindings `(("name" . ,name)
                             ("description" . ,(escape-lisp-string description))
                             ("author" . ,(escape-lisp-string author))
                             ("license" . ,(escape-lisp-string license))
                             ("parent-prompts" . ,parent-prompts)
                             ("test-framework" . ,framework-label)))
             (render (lambda (tpl) (render-template tpl raw-bindings))))
        (list
         (cons (format nil "~A.asd" name) (render-template asd-template
                                                           asd-bindings))
         (cons "CLAUDE.md"            (funcall render *claude-md-template*))
         (cons "AGENTS.md"            (funcall render *agents-md-template*))
         (cons "README.md"            (funcall render *readme-template*))
         (cons ".gitignore"           (funcall render *gitignore-template*))
         (cons "src/main.lisp"        (funcall render *main-lisp-template*))
         (cons "tests/main-test.lisp" (funcall render main-test-template)))))))
