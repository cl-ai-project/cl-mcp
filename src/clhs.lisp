;;;; src/clhs.lisp --- Common Lisp HyperSpec lookup tool

(defpackage #:cl-mcp/src/clhs
  (:use #:cl)
  (:import-from #:cl-mcp/src/log
                #:log-event)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:text-content)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:export #:clhs-lookup))

(in-package #:cl-mcp/src/clhs)

;;; ---------------------------------------------------------------------------
;;; Symbol Map Cache
;;; ---------------------------------------------------------------------------

(defvar *clhs-symbol-map* nil
  "Hash table mapping uppercase symbol names to relative HyperSpec paths.")

(defvar *clhs-root* nil
  "Cached HyperSpec root directory pathname.")

(defun %ensure-clhs-loaded ()
  "Ensure the :clhs library is loaded. Signals an error on failure."
  (unless (find-package :clhs)
    (handler-case
        (progn
          (log-event :info "clhs" "action" "loading :clhs library via Quicklisp")
          (funcall (find-symbol "QUICKLOAD" :ql) :clhs :silent t))
      (error (c)
        (error "Failed to load :clhs library via Quicklisp. ~
                Ensure Quicklisp is installed and configured. Error: ~A" c)))))

(defun %load-symbol-map ()
  "Load the symbol map from HyperSpec Data/Map_Sym.txt.
If the HyperSpec is not installed locally, attempts to install it first."
  (%ensure-clhs-loaded)
  (let* ((root (funcall (find-symbol "HYPERSPEC-ROOT" :clhs)))
         (map-file (merge-pathnames "Data/Map_Sym.txt" root))
         (table (make-hash-table :test 'equalp)))
    ;; If HyperSpec not installed locally, try to install it
    (unless (probe-file map-file)
      (log-event :info "clhs" "action" "HyperSpec not found locally, attempting installation")
      (handler-case
          (let ((install-fn (find-symbol "INSTALL-CLHS-USE-LOCAL" :clhs)))
            (when (and install-fn (fboundp install-fn))
              (funcall install-fn)
              ;; Update root after installation
              (setf root (funcall (find-symbol "HYPERSPEC-ROOT" :clhs))
                    map-file (merge-pathnames "Data/Map_Sym.txt" root))))
        (error (c)
          (log-event :warn "clhs" "action" "auto-install failed" "error" (princ-to-string c)))))
    ;; Final check - error if still not available
    (unless (probe-file map-file)
      (error "HyperSpec symbol map not found at ~A. ~
              Auto-installation failed. Please run (clhs:install-clhs-use-local) ~
              manually in your REPL to install the HyperSpec locally."
             map-file))
    (with-open-file (s map-file)
      (loop for symbol = (read-line s nil)
            for path = (read-line s nil)
            while (and symbol path)
            do (setf (gethash symbol table) path)))
    (setf *clhs-root* root
          *clhs-symbol-map* table)
    (log-event :info "clhs" "action" "loaded symbol map" "count" (hash-table-count table))
    table))

(defun %get-symbol-map ()
  "Return the symbol map, loading it if necessary."
  (or *clhs-symbol-map* (%load-symbol-map)))

(defun %get-clhs-root ()
  "Return the HyperSpec root, ensuring it is loaded."
  (%get-symbol-map)
  *clhs-root*)

(defun %section-to-filename (section-string)
  "Convert a section number like '22.3.1' to a filename like '22_ca.htm'.
Each subsection letter corresponds to a=1, b=2, c=3, etc."
  (let* ((parts (cl-ppcre:split "\\." section-string))
         (chapter (first parts))
         (subsections (rest parts)))
    (if (null subsections)
        (format nil "~A_.htm" chapter)
        (format nil "~A_~{~A~}.htm"
                chapter
                (mapcar (lambda (n)
                          (code-char (+ (char-code #\a) (1- (parse-integer n)))))
                        subsections)))))

(defun %section-local-path (section-string)
  "Return the absolute local file path for a section number, or NIL if not found."
  (let* ((filename (%section-to-filename section-string))
         (path (merge-pathnames (format nil "Body/~A" filename) (%get-clhs-root))))
    (when (probe-file path)
      (namestring path))))

(defun %section-number-p (string)
  "Return T if STRING looks like a section number (e.g., '22.3', '3.1.2')."
  (and (> (length string) 0)
       (every (lambda (c) (or (digit-char-p c) (char= c #\.))) string)
       (digit-char-p (char string 0))))

(defun clhs-lookup-section (section-string &key (include-content t))
  "Look up a section number (e.g., '22.3') in the Common Lisp HyperSpec.
Returns a hash table with:
  - section: The section number
  - url: The HyperSpec URL (file:// or http://)
  - source: 'local' or 'remote'
  - content: Extracted text content (when INCLUDE-CONTENT is true and local)"
  (let* ((local-path (%section-local-path section-string))
         (is-local (and local-path (probe-file local-path)))
         (filename (%section-to-filename section-string))
         (url (if is-local
                  (format nil "file://~A" local-path)
                  (format nil "http://www.lispworks.com/documentation/HyperSpec/Body/~A"
                          filename))))
    (unless is-local
      (log-event :warn "clhs" "action" "section not found locally" "section" section-string))
    (let ((result (make-ht "section" section-string
                           "url" url
                           "source" (if is-local "local" "remote"))))
      (when (and include-content is-local)
        (let ((content (%extract-text-from-html local-path)))
          (when content
            (setf (gethash "content" result)
                  (text-content content)))))
      result)))
;;; ---------------------------------------------------------------------------
;;; URL and Path Resolution
;;; ---------------------------------------------------------------------------

(defun %symbol-relative-path (symbol-name)
  "Return the relative path (e.g., 'Body/m_loop.htm') for SYMBOL-NAME, or NIL."
  (let* ((table (%get-symbol-map))
         (key (string-upcase (string-trim " " symbol-name)))
         (raw-path (gethash key table)))
    (when raw-path
      ;; Map_Sym.txt contains paths like "../Body/m_loop.htm"
      ;; We need to strip the "../" prefix
      (if (and (>= (length raw-path) 3)
               (string= "../" raw-path :end2 3))
          (subseq raw-path 3)
          raw-path))))

(defun %symbol-local-path (symbol-name)
  "Return the absolute local file path for SYMBOL-NAME, or NIL."
  (let ((relative (%symbol-relative-path symbol-name)))
    (when relative
      (namestring (merge-pathnames relative (%get-clhs-root))))))

(defun %symbol-url (symbol-name)
  "Return the HyperSpec URL for SYMBOL-NAME.
Returns a file:// URL for local installations, or http:// URL otherwise."
  (let ((local-path (%symbol-local-path symbol-name)))
    (cond
      ((and local-path (probe-file local-path))
       (format nil "file://~A" local-path))
      (t
       ;; Fallback to LispWorks remote URL
       (let ((relative (%symbol-relative-path symbol-name)))
         (when relative
           (format nil "http://www.lispworks.com/documentation/HyperSpec/~A"
                   relative)))))))

;;; ---------------------------------------------------------------------------
;;; HTML Text Extraction
;;; ---------------------------------------------------------------------------

(defun %extract-text-from-html (path &key (max-chars 8000))
  "Extract plain text from HTML file at PATH.
Returns at most MAX-CHARS characters of content."
  (unless (probe-file path)
    (return-from %extract-text-from-html nil))
  (with-open-file (s path :external-format :latin-1)
    (let ((result (make-array 0 :element-type 'character
                                :adjustable t :fill-pointer 0))
          (in-script nil)
          (in-style nil))
      (flet ((append-text (text)
               (when (and text (< (length result) max-chars))
                 (loop for char across text
                       while (< (length result) max-chars)
                       do (vector-push-extend char result)))))
        (loop for line = (read-line s nil)
              while (and line (< (length result) max-chars))
              do (let ((text line))
                   ;; Track script/style blocks to skip
                   (when (search "<script" text :test #'char-equal)
                     (setf in-script t))
                   (when (search "</script>" text :test #'char-equal)
                     (setf in-script nil)
                     (setf text ""))
                   (when (search "<style" text :test #'char-equal)
                     (setf in-style t))
                   (when (search "</style>" text :test #'char-equal)
                     (setf in-style nil)
                     (setf text ""))
                   (unless (or in-script in-style)
                     ;; Remove HTML tags
                     (setf text (cl-ppcre:regex-replace-all "<[^>]+>" text ""))
                     ;; Decode common HTML entities
                     (setf text (cl-ppcre:regex-replace-all "&lt;" text "<"))
                     (setf text (cl-ppcre:regex-replace-all "&gt;" text ">"))
                     (setf text (cl-ppcre:regex-replace-all "&amp;" text "&"))
                     (setf text (cl-ppcre:regex-replace-all "&quot;" text "\""))
                     (setf text (cl-ppcre:regex-replace-all "&nbsp;" text " "))
                     (setf text (cl-ppcre:regex-replace-all "&#\\d+;" text ""))
                     ;; Collapse multiple spaces
                     (setf text (cl-ppcre:regex-replace-all "  +" text " "))
                     (let ((trimmed (string-trim '(#\Space #\Tab) text)))
                       (when (plusp (length trimmed))
                         (append-text trimmed)
                         (append-text (string #\Newline))))))))
      (coerce result 'string))))

;;; ---------------------------------------------------------------------------
;;; Main Lookup Function
;;; ---------------------------------------------------------------------------

(defun clhs-lookup (query &key (include-content t))
  "Look up QUERY in the Common Lisp HyperSpec.
QUERY can be either:
  - A symbol name (e.g., 'loop', 'format', 'handler-case')
  - A section number (e.g., '22.3', '3.1.2')

Returns a hash table with:
  - symbol or section: The query identifier
  - url: The HyperSpec URL (file:// or http://)
  - source: 'local' or 'remote'
  - content: Extracted text content (when INCLUDE-CONTENT is true and local)"
  (if (%section-number-p query)
      (clhs-lookup-section query :include-content include-content)
      ;; Symbol lookup
      (let* ((normalized (string-upcase (string-trim " " query)))
             (url (%symbol-url query))
             (local-path (%symbol-local-path query))
             (is-local (and local-path (probe-file local-path))))
        (unless url
          (error "No HyperSpec entry found for '~A'. ~
                  Verify it is a standard Common Lisp symbol or valid section number."
                 query))
        (let ((result (make-ht "symbol" (string-downcase normalized)
                               "url" url
                               "source" (if is-local "local" "remote"))))
          (when (and include-content is-local)
            (let ((content (%extract-text-from-html local-path)))
              (when content
                (setf (gethash "content" result)
                      (text-content content)))))
          result))))

;;; ---------------------------------------------------------------------------
;;; Tool Definition
;;; ---------------------------------------------------------------------------

(define-tool "clhs-lookup"
  :description "Look up a symbol or section in the Common Lisp HyperSpec (ANSI standard).

Accepts either:
- Symbol names: 'loop', 'format', 'handler-case', 'defmethod'
- Section numbers: '22.3', '3.1.2', '7.1' (auto-detected by digit.digit pattern)

Use this tool when you need:
- Exact syntax for complex macros (LOOP, FORMAT, SETF)
- Standard behavior guarantees and edge cases
- Following cross-references like 'See Section 22.3'

Examples:
  query='loop' - LOOP macro syntax and clauses
  query='format' - FORMAT function (references Section 22.3)
  query='22.3' - Formatted Output section (FORMAT directives)
  query='22.3.1' - Basic Output subsection (~C, ~%, etc.)"
  :args ((query :type :string :required t
                :description "Symbol name or section number to look up")
         (include_content :type :boolean :required nil
                          :description "Include extracted text content (default: true)"))
  :body
  (let ((include-content (if (boundp 'include_content) include_content t)))
    (result id (clhs-lookup query :include-content include-content))))
