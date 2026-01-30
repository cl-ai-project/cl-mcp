;;;; src/clgrep.lisp
;;;; MCP tools for semantic grep using clgrep library

(defpackage #:cl-mcp/src/clgrep
  (:use #:cl)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:cl-mcp/src/utils/paths
                #:resolve-path-in-project)
  (:import-from #:cl-mcp/src/utils/hash
                #:alist-to-hash-table)
  (:import-from #:cl-mcp/src/utils/clgrep
                #:semantic-grep)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:text-content)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
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
         (types (%parse-form-types form-types))
         (results (semantic-grep search-path pattern
                                 :recursive recursive
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

(defun %format-clgrep-results (results)
  "Convert clgrep results (list of alists) to a vector of hash tables."
  (map 'vector #'alist-to-hash-table results))


(define-tool "clgrep-search"
  :description "Perform semantic grep search for a pattern in Lisp files.
Unlike regular grep, this tool understands Lisp structure and returns
the top-level form signature containing each match.

KEY ADVANTAGE: Works WITHOUT loading systems - faster and no side effects.
Use this as the FIRST choice for code exploration before code-find/code-describe.

Default: Returns signatures only (token-efficient, ~70% reduction vs full forms).
Use 'include_form: true' to get complete form text when needed.

Recommended workflow:
1. clgrep-search to locate functions/usages across the project
2. lisp-read-file with name_pattern to read specific definitions in detail"
  :args ((pattern :type :string :required t
                  :description "cl-ppcre regular expression pattern to search for")
         (path :type :string
               :description "Search root directory, relative to project root (optional, defaults to project root)")
         (recursive :type :boolean :default t
                    :description "Search subdirectories recursively (default: true)")
         (case-insensitive :type :boolean :json-name "case_insensitive"
                           :description "Case-insensitive matching (default: false)")
         (form-types :type :array :json-name "form_types"
                     :description "Filter by form types, e.g., [\"defun\", \"defmethod\"] (optional)")
         (limit :type :integer
                :description "Maximum number of results to return (optional, defaults to unlimited)")
         (include-form :type :boolean :json-name "include_form"
                       :description "Include full form text in results (default: false, returns signatures only)"))
  :body
  (let* ((results
          (clgrep-search pattern
                         :path path
                         :recursive recursive
                         :case-insensitive case-insensitive
                         :form-types form-types
                         :limit limit
                         :include-form include-form))
         (formatted (%format-clgrep-results results)))
    (result id
            (make-ht "content" (text-content (with-output-to-string (s)
                                               (encode formatted s)))
                     "matches" formatted
                     "count" (length results)
                     "limited" (and limit (<= limit (length results)))))))
