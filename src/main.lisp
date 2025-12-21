(uiop:define-package clgrep
  (:use #:cl)
  (:import-from #:cl-ppcre
                #:scan)
  (:export #:main
           #:grep-file
           #:extract-toplevel-form
           #:extract-form-signature
           #:collect-target-files
           #:semantic-grep))
(in-package #:clgrep)

(defun grep-file (pattern filepath)
  "Search for PATTERN in FILEPATH and print matching lines with line numbers.
   Returns the number of matches found."
  (let ((match-count 0))
    (with-open-file (stream filepath :direction :input)
      (loop for line = (read-line stream nil nil)
            for line-number from 1
            while line
            when (cl-ppcre:scan pattern line)
            do (format t "~A:~A: ~A~%" filepath line-number line)
               (incf match-count)))
    match-count))

(defun main ()
  "Entry point for the clgrep command-line tool.
   Usage: clgrep PATTERN FILE"
  (let ((args (uiop:command-line-arguments)))
    (cond
      ((< (length args) 2)
       (format *error-output* "Usage: clgrep PATTERN FILE~%")
       (uiop:quit 1))
      (t
       (let ((pattern (first args))
             (filepath (second args)))
         (handler-case
             (let ((matches (grep-file pattern filepath)))
               (when (zerop matches)
                 (uiop:quit 1)))
           (file-error (condition)
             (format *error-output* "Error: ~A~%" condition)
             (uiop:quit 2))
           (cl-ppcre:ppcre-syntax-error (condition)
             (format *error-output* "Invalid regex pattern: ~A~%" condition)
             (uiop:quit 3))))))))

(defstruct toplevel-form
  "Represents a top-level form with its position and line range."
  start-pos
  end-pos
  start-line
  end-line)

(defun scan-toplevel-forms (content)
  "Scan CONTENT and return a list of TOPLEVEL-FORM structs.
   Handles strings, comments, character literals, and escape sequences correctly."
  (let ((forms nil)
        (state :normal)
        (paren-depth 0)
        (current-line 1)
        (form-start-pos nil)
        (form-start-line nil)
        (pos 0)
        (len (length content)))

    (loop while (< pos len)
          for char = (char content pos)
          do (case state
               (:normal
                (cond
                  ;; Character literal #\x - skip the next character
                  ((and (char= char #\#)
                        (< (1+ pos) len)
                        (char= (char content (1+ pos)) #\\))
                   ;; Skip #\ and the following character
                   (incf pos 2)
                   (when (< pos len)
                     (when (char= (char content pos) #\Newline)
                       (incf current-line))
                     (incf pos))
                   ;; Continue to next iteration without incrementing pos again
                   (decf pos))

                  ;; String literal starts
                  ((char= char #\")
                   (setf state :string))

                  ;; Line comment starts
                  ((char= char #\;)
                   (setf state :comment))

                  ;; Block comment starts #|
                  ((and (char= char #\#)
                        (< (1+ pos) len)
                        (char= (char content (1+ pos)) #\|))
                   (setf state :block-comment)
                   (incf pos))

                  ;; Opening paren
                  ((char= char #\()
                   (when (zerop paren-depth)
                     (setf form-start-pos pos
                           form-start-line current-line))
                   (incf paren-depth))

                  ;; Closing paren
                  ((char= char #\))
                   (when (plusp paren-depth)
                     (decf paren-depth)
                     (when (zerop paren-depth)
                       (push (make-toplevel-form
                              :start-pos form-start-pos
                              :end-pos (1+ pos)
                              :start-line form-start-line
                              :end-line current-line)
                             forms)
                       (setf form-start-pos nil
                             form-start-line nil))))))

               (:string
                (cond
                  ;; Escape sequence in string
                  ((char= char #\\)
                   (setf state :escape))

                  ;; String literal ends
                  ((char= char #\")
                   (setf state :normal))))

               (:escape
                ;; After escape, return to string
                (setf state :string))

               (:comment
                ;; Comment ends at newline
                (when (char= char #\Newline)
                  (setf state :normal)))

               (:block-comment
                ;; Block comment ends with |#
                (when (and (char= char #\|)
                           (< (1+ pos) len)
                           (char= (char content (1+ pos)) #\#))
                  (setf state :normal)
                  (incf pos))))

             ;; Track line numbers
             (when (char= char #\Newline)
               (incf current-line))

             (incf pos))

    (nreverse forms)))

(defun split-lines (text)
  "Split TEXT into a list of lines."
  (let ((lines nil)
        (start 0))
    (loop for pos from 0 below (length text)
          when (char= (char text pos) #\Newline)
          do (push (subseq text start pos) lines)
             (setf start (1+ pos))
          finally (when (< start (length text))
                    (push (subseq text start) lines)))
    (nreverse lines)))
(defun truncate-form (form-text target-line form-start-line context-lines)
  "Truncate FORM-TEXT if longer than 2000 chars, keeping CONTEXT-LINES around TARGET-LINE."
  (if (<= (length form-text) 2000)
      form-text
      (let* ((lines (split-lines form-text))
             (target-relative-line (- target-line form-start-line))
             (start-context (max 0 (- target-relative-line context-lines)))
             (end-context (min (1- (length lines)) (+ target-relative-line context-lines)))
             (before-lines (when (> start-context 0)
                            (subseq lines 0 (min 2 start-context))))
             (context-lines-list (subseq lines start-context (1+ end-context)))
             (after-lines (when (< end-context (1- (length lines)))
                           (subseq lines (max (- (length lines) 2) (1+ end-context))))))
        (with-output-to-string (out)
          (when before-lines
            (dolist (line before-lines)
              (format out "~A~%" line))
            (format out "...~%~%"))

          (dolist (line context-lines-list)
            (format out "~A~%" line))

          (when after-lines
            (format out "~%...~%")
            (dolist (line after-lines)
              (format out "~A~%" line)))))))

(defun extract-toplevel-form (content target-line-number &optional (context-lines 5))
  "Extract the top-level form containing TARGET-LINE-NUMBER from CONTENT.
   If the form exceeds 2000 characters, truncate it keeping CONTEXT-LINES
   around the target line.
   Returns an alist with :text, :start-line, :end-line, :start-byte, :end-byte
   or NIL if no form is found at that line."
  (let ((forms (scan-toplevel-forms content)))
    (dolist (form forms)
      (when (and (>= target-line-number (toplevel-form-start-line form))
                 (<= target-line-number (toplevel-form-end-line form)))
        (let ((form-text (subseq content
                                 (toplevel-form-start-pos form)
                                 (toplevel-form-end-pos form))))
          (return-from extract-toplevel-form
            (list (cons :text (truncate-form form-text
                                             target-line-number
                                             (toplevel-form-start-line form)
                                             context-lines))
                  (cons :start-line (toplevel-form-start-line form))
                  (cons :end-line (toplevel-form-end-line form))
                  (cons :start-byte (toplevel-form-start-pos form))
                  (cons :end-byte (toplevel-form-end-pos form)))))))
    nil))

(defun glob-to-regex (pattern)
  "Convert a gitignore glob PATTERN to a regular expression string.
   Handles *, ?, **, and basic glob syntax."
  (let ((result (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
        (i 0)
        (len (length pattern)))

    ;; Anchor to start if pattern starts with /
    (when (and (> len 0) (char= (char pattern 0) #\/))
      (vector-push-extend #\^ result)
      (incf i))

    (loop while (< i len)
          do (let ((char (char pattern i)))
               (cond
                 ;; Handle **/ for matching any directory depth
                 ((and (< (+ i 2) len)
                       (char= char #\*)
                       (char= (char pattern (+ i 1)) #\*)
                       (char= (char pattern (+ i 2)) #\/))
                  (loop for c across "(?:.*/|)" do (vector-push-extend c result))
                  (incf i 3))

                 ;; Handle /** at end for matching directory and contents
                 ((and (< (+ i 1) len)
                       (char= char #\*)
                       (char= (char pattern (+ i 1)) #\*)
                       (= (+ i 2) len))
                  (loop for c across ".*" do (vector-push-extend c result))
                  (incf i 2))

                 ;; Handle single * (match anything except /)
                 ((char= char #\*)
                  (loop for c across "[^/]*" do (vector-push-extend c result))
                  (incf i))

                 ;; Handle ? (match single char except /)
                 ((char= char #\?)
                  (loop for c across "[^/]" do (vector-push-extend c result))
                  (incf i))

                 ;; Escape special regex characters
                 ((find char ".+^$(){}[]|\\")
                  (vector-push-extend #\\ result)
                  (vector-push-extend char result)
                  (incf i))

                 ;; Regular characters
                 (t
                  (vector-push-extend char result)
                  (incf i)))))

    ;; If pattern ends with /, match directory and its contents
    (when (and (> len 0) (char= (char pattern (1- len)) #\/))
      (loop for c across ".*" do (vector-push-extend c result)))

    result))

(defun parse-gitignore (gitignore-path)
  "Parse a .gitignore file and return a list of regex pattern strings.
   Returns NIL if the file doesn't exist."
  (when (probe-file gitignore-path)
    (with-open-file (stream gitignore-path :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            for trimmed = (string-trim '(#\Space #\Tab #\Return) line)
            unless (or (zerop (length trimmed))           ; Empty line
                       (char= (char trimmed 0) #\#))      ; Comment
            collect (glob-to-regex trimmed)))))

(defun path-ignored-p (path root-dir ignore-patterns)
  "Check if PATH matches any of the IGNORE-PATTERNS.
   PATH is made relative to ROOT-DIR for matching."
  (let* ((relative-path (enough-namestring path root-dir))
         ;; Convert backslashes to forward slashes for consistency
         (normalized-path (substitute #\/ #\\ relative-path)))
    (dolist (pattern ignore-patterns)
      (when (cl-ppcre:scan pattern normalized-path)
        (return-from path-ignored-p t)))
    nil))

(defun target-file-p (pathname)
  "Check if PATHNAME has a target extension (.lisp, .asd, or .ros)."
  (let ((type (pathname-type pathname)))
    (and type
         (member type '("lisp" "asd" "ros") :test #'string-equal))))

(defun collect-target-files (root-directory &key (recursive t))
  "Collect .lisp, .asd, and .ros files from ROOT-DIRECTORY.
   If RECURSIVE is true (default), search subdirectories recursively.
   Respects .gitignore patterns and excludes .git directories."
  (let* ((root-dir (truename root-directory))
         (gitignore-path (merge-pathnames ".gitignore" root-dir))
         (ignore-patterns (parse-gitignore gitignore-path))
         (git-pattern (glob-to-regex ".git/"))
         (all-patterns (cons git-pattern ignore-patterns))
         (result nil))

    (if recursive
        (labels ((collect-from-dir (dir)
                   "Recursively collect files from DIR."
                   (dolist (entry (uiop:directory* (merge-pathnames uiop:*wild-file* dir)))
                     (unless (path-ignored-p entry root-dir all-patterns)
                       (when (target-file-p entry)
                         (push entry result))))

                   ;; Recursively process subdirectories
                   (dolist (subdir (uiop:subdirectories dir))
                     (unless (path-ignored-p subdir root-dir all-patterns)
                       (collect-from-dir subdir)))))
          (collect-from-dir root-dir))

        ;; Non-recursive: only files directly in root-dir
        (dolist (entry (uiop:directory* (merge-pathnames uiop:*wild-file* root-dir)))
          (unless (path-ignored-p entry root-dir all-patterns)
            (when (target-file-p entry)
              (push entry result)))))

    (nreverse result)))

(defparameter *known-form-types*
  '("defun" "defmethod" "defgeneric" "defmacro" "define-compiler-macro"
    "defvar" "defparameter" "defconstant"
    "defclass" "defstruct" "deftype" "define-condition"
    "defpackage" "in-package"
    "defsystem" "deftest")
  "List of known form type keywords to recognize.")

(defun extract-form-type-and-name (form-text)
  "Extract the form type and name from FORM-TEXT.
   Returns an alist with :type and :name, or NIL if not a recognized form.

   Examples:
     (defun foo ...) => ((:type . \"defun\") (:name . \"foo\"))
     (defmethod bar ...) => ((:type . \"defmethod\") (:name . \"bar\"))
     (defvar *x* ...) => ((:type . \"defvar\") (:name . \"*x*\"))"
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) form-text)))
    (when (and (> (length trimmed) 0)
               (char= (char trimmed 0) #\())
      ;; Extract the first token (form type) and second token (name)
      (multiple-value-bind (match groups)
          (cl-ppcre:scan-to-strings
           "^\\(\\s*([a-zA-Z][a-zA-Z0-9*_+-]*)\\s+([^\\s()]+|\\([^)]+\\))"
           trimmed)
        (when match
          (let ((form-type (string-downcase (aref groups 0)))
                (form-name (aref groups 1)))
            (when (member form-type *known-form-types* :test #'string=)
              (list (cons :type form-type)
                    (cons :name form-name)))))))))

(defun extract-balanced-sexp (text start)
  "Extract a balanced S-expression starting at position START in TEXT.
   Returns (values sexp-string end-position) or (values nil nil) if not found."
  (when (and (< start (length text))
             (char= (char text start) #\())
    (let ((depth 1)
          (pos (1+ start))
          (len (length text))
          (state :normal))
      (loop while (and (< pos len) (plusp depth))
            for char = (char text pos)
            do (case state
                 (:normal
                  (cond
                    ((char= char #\() (incf depth))
                    ((char= char #\)) (decf depth))
                    ((char= char #\") (setf state :string))
                    ((char= char #\;) (setf state :comment))
                    ((char= char #\\) (incf pos))))
                 (:string
                  (cond
                    ((char= char #\\) (incf pos))
                    ((char= char #\") (setf state :normal))))
                 (:comment
                  (when (char= char #\Newline)
                    (setf state :normal))))
               (incf pos))
      (when (zerop depth)
        (values (subseq text start pos) pos)))))

(defun extract-form-signature (form-text)
  "Extract the signature from FORM-TEXT.
   Returns a string representing the signature, or NIL if not a recognized form.

   Examples:
     (defun foo (x y &key z) ...) => \"(foo x y &key z)\"
     (defmethod emit ((logger json-logger) event) ...) => \"(emit (logger json-logger) event)\"
     (defvar *x* 42) => \"*x*\"
     (defclass my-class (parent) ...) => \"(my-class parent)\""
  (let* ((type-info (extract-form-type-and-name form-text))
         (form-type (cdr (assoc :type type-info)))
         (form-name (cdr (assoc :name type-info))))
    (when (and form-type form-name)
      (cond
        ;; Forms with lambda-list: defun, defmacro, defgeneric, defmethod
        ((member form-type '("defun" "defmacro" "defgeneric" "defmethod"
                             "define-compiler-macro" "deftest")
                 :test #'string=)
         (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline) form-text))
                (name-pattern (format nil "^\\(\\s*~A\\s+~A\\s*"
                                      (cl-ppcre:quote-meta-chars form-type)
                                      (cl-ppcre:quote-meta-chars form-name)))
                (name-end (nth-value 1 (cl-ppcre:scan name-pattern trimmed))))
           (when name-end
             (multiple-value-bind (lambda-list end-pos)
                 (extract-balanced-sexp trimmed name-end)
               (declare (ignore end-pos))
               (if lambda-list
                   (let ((params (string-trim '(#\Space #\Tab #\Newline)
                                              (subseq lambda-list 1 (1- (length lambda-list))))))
                     (if (zerop (length params))
                         (format nil "(~A)" form-name)
                         (format nil "(~A ~A)" form-name params)))
                   form-name)))))
        ;; Variable definitions: just the name
        ((member form-type '("defvar" "defparameter" "defconstant") :test #'string=)
         form-name)
        ;; Class definitions: name + superclasses
        ((string= form-type "defclass")
         (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline) form-text))
                (name-pattern (format nil "^\\(\\s*defclass\\s+~A\\s*"
                                      (cl-ppcre:quote-meta-chars form-name)))
                (name-end (nth-value 1 (cl-ppcre:scan name-pattern trimmed))))
           (when name-end
             (multiple-value-bind (superclasses end-pos)
                 (extract-balanced-sexp trimmed name-end)
               (declare (ignore end-pos))
               (if superclasses
                   (let ((supers (string-trim '(#\Space #\Tab #\Newline)
                                              (subseq superclasses 1 (1- (length superclasses))))))
                     (if (zerop (length supers))
                         form-name
                         (format nil "(~A ~A)" form-name supers)))
                   form-name)))))
        ;; Struct definitions: just the name
        ((string= form-type "defstruct")
         form-name)
        ;; Other definitions: just the name
        (t form-name)))))
(defun extract-package-for-line (content line-number)
  "Extract the package name that is active at LINE-NUMBER in CONTENT.
   Returns the package name as a string, or NIL if not found."
  (let ((current-package nil)
        (current-line 1))
    (with-input-from-string (stream content)
      (loop for line = (read-line stream nil nil)
            while (and line (<= current-line line-number))
            do (let ((trimmed (string-trim '(#\Space #\Tab) line)))
                 ;; Match (in-package :xxx) or (in-package #:xxx) or (in-package "xxx")
                 (multiple-value-bind (match groups)
                     (cl-ppcre:scan-to-strings
                      "\\(in-package\\s+[:#]?['\"]?([^)\\s'\"]+)"
                      trimmed)
                   (when match
                     ;; Remove leading : or #: from package name
                     (let ((pkg-name (aref groups 0)))
                       (setf current-package
                             (string-upcase
                              (string-left-trim ":#" pkg-name)))))))
               (incf current-line)))
    current-package))

(defun search-in-file (filepath pattern &key case-insensitive form-types (include-form t))
  "Search for PATTERN in FILEPATH and return a list of match results.
   If CASE-INSENSITIVE is true, perform case-insensitive matching.
   If FORM-TYPES is a list of strings (e.g., '(\"defun\" \"defmethod\")),
   only include results where the form type matches.
   If INCLUDE-FORM is NIL, omit the :form field from results (saves tokens).
   Each result is an alist with file, line, match, package, signature, and optionally form."
  (let ((results nil))
    (handler-case
        (let* ((content (uiop:read-file-string filepath))
               (scanner (if case-insensitive
                            (cl-ppcre:create-scanner pattern :case-insensitive-mode t)
                            pattern)))
          (with-input-from-string (stream content)
            (loop for line = (read-line stream nil nil)
                  for line-number from 1
                  while line
                  when (cl-ppcre:scan scanner line)
                  do (let ((package (extract-package-for-line content line-number))
                           (form-info (extract-toplevel-form content line-number)))
                       (when form-info
                         (let* ((full-form-text (subseq content
                                                        (cdr (assoc :start-byte form-info))
                                                        (cdr (assoc :end-byte form-info))))
                                (type-info (extract-form-type-and-name full-form-text))
                                (form-type (cdr (assoc :type type-info)))
                                (form-name (cdr (assoc :name type-info)))
                                (signature (extract-form-signature full-form-text)))
                           ;; Apply form-type filter if specified
                           (when (or (null form-types)
                                     (member form-type form-types :test #'string-equal))
                             (let ((result (list (cons :file (namestring filepath))
                                                 (cons :line line-number)
                                                 (cons :match line)
                                                 (cons :package (or package "UNKNOWN"))
                                                 (cons :form-type form-type)
                                                 (cons :form-name form-name)
                                                 (cons :signature signature)
                                                 (cons :form-start-line (cdr (assoc :start-line form-info)))
                                                 (cons :form-end-line (cdr (assoc :end-line form-info)))
                                                 (cons :form-start-byte (cdr (assoc :start-byte form-info)))
                                                 (cons :form-end-byte (cdr (assoc :end-byte form-info))))))
                               ;; Conditionally add :form
                               (when include-form
                                 (setf result (append result
                                                      (list (cons :form (cdr (assoc :text form-info)))))))
                               (push result results)))))))))
      (error (condition)
        (format *error-output* "Warning: Could not read ~A: ~A~%"
                filepath condition)))
    (nreverse results)))

(defun semantic-grep (root-directory pattern &key (recursive t) case-insensitive form-types (include-form t))
  "Search for PATTERN across all Lisp files in ROOT-DIRECTORY.

   Keyword arguments:
     :recursive        - If true (default), search subdirectories recursively
     :case-insensitive - If true, perform case-insensitive matching
     :form-types       - List of form types to include (e.g., '(\"defun\" \"defmethod\"))
                         If NIL, all forms are included.
     :include-form     - If true (default), include full form text in results.
                         Set to NIL to omit :form field (saves tokens).

   Returns a list of alists, each containing:
     :file            - File path
     :line            - Line number of the match
     :match           - The matching line text
     :package         - Package name active at that line
     :form-type       - Type of the form (defun, defmethod, etc.) or NIL
     :form-name       - Name of the form or NIL
     :signature       - Signature of the form (e.g., \"(foo x y &key z)\")
     :form-start-line - Start line of the containing form
     :form-end-line   - End line of the containing form
     :form-start-byte - Start byte offset of the containing form
     :form-end-byte   - End byte offset of the containing form
     :form            - The top-level form (only if :include-form is true)

   Example usage:
     (semantic-grep \"/path/to/project\" \"defun.*foo\")
     (semantic-grep \"/path/to/project\" \"error\" :case-insensitive t)
     (semantic-grep \"/path/to/project\" \"timeout\" :form-types '(\"defun\" \"defmethod\"))
     (semantic-grep \"/path/to/project\" \"emit\" :include-form nil)  ; signature only"
  (let ((files (collect-target-files root-directory :recursive recursive))
        (all-results nil))
    (dolist (file files)
      (let ((file-results (search-in-file file pattern
                                          :case-insensitive case-insensitive
                                          :form-types form-types
                                          :include-form include-form)))
        (setf all-results (append all-results file-results))))
    all-results))
