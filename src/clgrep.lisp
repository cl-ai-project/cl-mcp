;;;; src/clgrep.lisp
;;;; MCP tools for semantic grep using clgrep library

(defpackage #:cl-mcp/src/clgrep
  (:use #:cl)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:cl-mcp/src/utils/paths
                #:resolve-path-in-project)
  (:import-from #:cl-mcp/src/utils/clgrep
                #:semantic-grep)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:rpc-error #:text-content)
  (:import-from #:cl-mcp/src/tools/registry
                #:register-tool)
  (:import-from #:yason
                #:encode)
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

(defun clgrep-search-descriptor ()
  "Return the MCP tool descriptor for clgrep-search."
  (make-ht
   "name" "clgrep-search"
   "description"
   "Perform semantic grep search for a pattern in Lisp files.
Unlike regular grep, this tool understands Lisp structure and returns
the top-level form signature containing each match.

KEY ADVANTAGE: Works WITHOUT loading systems - faster and no side effects.
Use this as the FIRST choice for code exploration before code-find/code-describe.

Default: Returns signatures only (token-efficient, ~70% reduction vs full forms).
Use 'includeForm: true' to get complete form text when needed.

Recommended workflow:
1. clgrep-search to locate functions/usages across the project
2. lisp-read-file with name_pattern to read specific definitions in detail"
   "inputSchema"
   (let ((p (make-hash-table :test #'equal)))
     (setf (gethash "pattern" p)
           (make-ht "type" "string"
                    "description"
                    "cl-ppcre regular expression pattern to search for"))
     (setf (gethash "path" p)
           (make-ht "type" "string"
                    "description"
                    "Search root directory, relative to project root (optional, defaults to project root)"))
     (setf (gethash "recursive" p)
           (make-ht "type" "boolean"
                    "description"
                    "Search subdirectories recursively (default: true)"))
     (setf (gethash "caseInsensitive" p)
           (make-ht "type" "boolean"
                    "description"
                    "Case-insensitive matching (default: false)"))
     (setf (gethash "formTypes" p)
           (make-ht "type" "array"
                    "items" (make-ht "type" "string")
                    "description"
                    "Filter by form types, e.g., [\"defun\", \"defmethod\"] (optional)"))
     (setf (gethash "limit" p)
           (make-ht "type" "integer"
                    "description"
                    "Maximum number of results to return (optional, defaults to unlimited)"))
     (setf (gethash "includeForm" p)
           (make-ht "type" "boolean"
                    "description"
                    "Include full form text in results (default: false, returns signatures only)"))
     (make-ht "type" "object"
              "properties" p
              "required" (vector "pattern")))))

(defun %alist-to-hash-table (alist)
  "Convert an alist to a hash table for JSON encoding."
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (pair alist ht)
      (setf (gethash (string-downcase (symbol-name (car pair))) ht)
            (cdr pair)))))

(defun %format-clgrep-results (results)
  "Convert clgrep results (list of alists) to a vector of hash tables."
  (map 'vector #'%alist-to-hash-table results))

(defun clgrep-search-handler (state id args)
  "Handle the clgrep-search MCP tool call."
  (declare (ignore state))
  (handler-case
      (let ((pattern (and args (gethash "pattern" args)))
            (path (and args (gethash "path" args)))
            (recursive (multiple-value-bind (val presentp)
                          (and args (gethash "recursive" args))
                        (if presentp val t)))
            (case-insensitive (and args (gethash "caseInsensitive" args)))
            (form-types (and args (gethash "formTypes" args)))
            (limit (and args (gethash "limit" args)))
            (include-form (and args (gethash "includeForm" args))))
        (unless (stringp pattern)
          (return-from clgrep-search-handler
            (rpc-error id -32602 "pattern must be a string")))
        (let* ((results (clgrep-search pattern
                                       :path path
                                       :recursive recursive
                                       :case-insensitive case-insensitive
                                       :form-types form-types
                                       :limit limit
                                       :include-form include-form))
               (formatted (%format-clgrep-results results)))
          (result id
                  (make-ht "content" (text-content
                                      (with-output-to-string (s)
                                        (encode formatted s)))
                           "matches" formatted
                           "count" (length results)
                           "limited" (and limit (<= limit (length results)))))))
    (error (e)
      (rpc-error id -32603
                 (format nil "Internal error during clgrep-search: ~A" e)))))

;;; Tool Registration

(register-tool "clgrep-search"
               (clgrep-search-descriptor)
               #'clgrep-search-handler)
