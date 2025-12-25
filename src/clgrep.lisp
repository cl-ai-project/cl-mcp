;;;; src/clgrep.lisp
;;;; MCP tools for semantic grep using clgrep library

(defpackage #:cl-mcp/src/clgrep
  (:use #:cl)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:cl-mcp/src/project-root #:*project-root*)
  (:import-from #:cl-mcp/src/utils/paths
                #:resolve-path-in-project)
  (:import-from #:cl-mcp/src/utils/clgrep
                #:semantic-grep)
  (:export
   #:clgrep-search))
(in-package #:cl-mcp/src/clgrep)

;; Path resolution is now handled by cl-mcp/src/utils/paths:resolve-path-in-project

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
Accepts a list of strings, a vector of strings, a single string, or NIL."
  (cond
    ((null form-types) nil)
    ((listp form-types) form-types)
    ((vectorp form-types) (coerce form-types 'list))
    ((stringp form-types) (list form-types))
    (t nil)))

(defun clgrep-search (pattern &key path recursive case-insensitive form-types limit include-form)
  "Perform semantic grep search for PATTERN in Lisp files.

Arguments:
  PATTERN          - cl-ppcre regular expression pattern (required)
  PATH             - Search root directory, relative to project root (optional)
  RECURSIVE        - Search subdirectories recursively (default: T)
  CASE-INSENSITIVE - Case-insensitive matching (default: NIL)
  FORM-TYPES       - Filter by form types, e.g., '(\"defun\" \"defmethod\") (optional)
  LIMIT            - Maximum number of results to return (optional)
  INCLUDE-FORM     - If true, include full form text in results (default: NIL)

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
  :form            - The full top-level form text (only if INCLUDE-FORM is true)"
  (let* ((search-path (resolve-path-in-project path :must-exist t))
         (recursive-p (if (null recursive) t recursive))
         (types (%parse-form-types form-types))
         (results (semantic-grep search-path pattern
                                 :recursive recursive-p
                                 :case-insensitive case-insensitive
                                 :form-types types
                                 :include-form include-form
                                 :limit limit)))
    (log-event :info "clgrep.search"
               "pattern" pattern
               "path" (namestring search-path)
               "limit" limit
               "include-form" include-form
               "matches" (length results))
    (mapcar (lambda (r) (%normalize-result r search-path)) results)))

