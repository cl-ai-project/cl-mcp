;;;; src/clgrep.lisp
;;;; MCP tools for semantic grep using clgrep library

(defpackage #:cl-mcp/src/clgrep
  (:use #:cl)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:cl-mcp/src/fs #:*project-root*)
  (:import-from #:cl-mcp/src/utils/clgrep
                #:semantic-grep)
  (:export
   #:clgrep-search
   #:clgrep-signatures))
(in-package #:cl-mcp/src/clgrep)

(defun %ensure-project-root ()
  "Ensure *project-root* is set. Signal an error with instructions if not."
  (unless *project-root*
    (error "Project root is not set. Call fs-set-project-root first.")))

(defun %resolve-search-path (path)
  "Resolve PATH to an absolute pathname within project root.
If PATH is NIL or empty, returns *project-root*.
Signals an error if PATH is outside project root."
  (%ensure-project-root)
  (let* ((base *project-root*)
         (target (if (or (null path) (string= path ""))
                     base
                     (let ((pn (uiop:ensure-pathname path
                                                     :want-pathname t
                                                     :defaults base)))
                       (if (uiop:absolute-pathname-p pn)
                           pn
                           (uiop:merge-pathnames* pn base))))))
    ;; Security: ensure target is under project root
    (let ((canonical (ignore-errors (truename target))))
      (unless canonical
        (error "Path does not exist: ~A" target))
      (unless (uiop:subpathp canonical base)
        (error "Path ~A is outside project root ~A" target base))
      canonical)))

(defun %normalize-result (result base-path)
  "Normalize a single search result, making file paths relative to BASE-PATH."
  (let ((file (cdr (assoc :file result))))
    (when file
      (let ((relative (enough-namestring (pathname file)
                                         (uiop:ensure-directory-pathname base-path))))
        (setf (cdr (assoc :file result)) relative))))
  result)

(defun %parse-form-types (form-types)
  "Parse FORM-TYPES argument into a list of strings.
Accepts a list of strings, a single string, or NIL."
  (cond
    ((null form-types) nil)
    ((listp form-types) form-types)
    ((stringp form-types) (list form-types))
    (t nil)))

(defun clgrep-search (pattern &key path recursive case-insensitive form-types limit)
  "Perform semantic grep search for PATTERN in Lisp files.

Arguments:
  PATTERN          - cl-ppcre regular expression pattern (required)
  PATH             - Search root directory, relative to project root (optional)
  RECURSIVE        - Search subdirectories recursively (default: T)
  CASE-INSENSITIVE - Case-insensitive matching (default: NIL)
  FORM-TYPES       - Filter by form types, e.g., '(\"defun\" \"defmethod\") (optional)
  LIMIT            - Maximum number of results to return (optional)

Returns a list of alists, each containing:
  :file            - File path (relative to project root)
  :line            - Line number of the match
  :match           - The matching line text
  :package         - Package name active at that line
  :form-type       - Type of the form (defun, defmethod, etc.) or NIL
  :form-name       - Name of the form or NIL
  :signature       - Signature of the form
  :form-start-line - Start line of the containing form
  :form-end-line   - End line of the containing form
  :form            - The full top-level form text"
  (let* ((search-path (%resolve-search-path path))
         (recursive-p (if (null recursive) t recursive))
         (types (%parse-form-types form-types))
         (results (semantic-grep search-path pattern
                                 :recursive recursive-p
                                 :case-insensitive case-insensitive
                                 :form-types types
                                 :include-form t
                                 :limit limit)))
    (log-event :info "clgrep.search"
               "pattern" pattern
               "path" (namestring search-path)
               "limit" limit
               "matches" (length results))
    (mapcar (lambda (r) (%normalize-result r search-path)) results)))


(defun clgrep-signatures (pattern &key path recursive case-insensitive form-types limit)
  "Perform semantic grep search for PATTERN, returning signatures only.
This is a token-efficient version that omits full form text.

Arguments are the same as clgrep-search.

Returns a list of alists, each containing:
  :file            - File path (relative to project root)
  :line            - Line number of the match
  :match           - The matching line text
  :package         - Package name active at that line
  :form-type       - Type of the form (defun, defmethod, etc.) or NIL
  :form-name       - Name of the form or NIL
  :signature       - Signature of the form
  :form-start-line - Start line of the containing form
  :form-end-line   - End line of the containing form"
  (let* ((search-path (%resolve-search-path path))
         (recursive-p (if (null recursive) t recursive))
         (types (%parse-form-types form-types))
         (results (semantic-grep search-path pattern
                                 :recursive recursive-p
                                 :case-insensitive case-insensitive
                                 :form-types types
                                 :include-form nil
                                 :limit limit)))
    (log-event :info "clgrep.signatures"
               "pattern" pattern
               "path" (namestring search-path)
               "limit" limit
               "matches" (length results))
    (mapcar (lambda (r) (%normalize-result r search-path)) results)))

