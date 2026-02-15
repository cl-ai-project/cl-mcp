;;;; src/prompts.lisp

(defpackage #:cl-mcp/src/prompts
  (:use #:cl)
  (:import-from #:cl-mcp/src/log
                #:log-event)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht)
  (:export #:prompts-directory
           #:read-prompt-file
           #:discover-prompts
           #:find-prompt-by-name))

(in-package #:cl-mcp/src/prompts)

(defun prompts-directory ()
  "Return prompts directory pathname from the cl-mcp system source tree.
Prompts are bundled with the server implementation and do not depend on
the caller's project root."
  (let* ((system-root (handler-case
                          (asdf:system-source-directory "cl-mcp")
                        (error () nil)))
         (system-dir (and system-root
                          (merge-pathnames "prompts/" system-root))))
    (when (and system-dir (uiop/filesystem:directory-exists-p system-dir))
      system-dir)))

(defun read-prompt-file (file)
  "Read bundled prompt FILE as a UTF-8 text string."
  (uiop:read-file-string file :external-format :utf-8))

(defun %markdown-heading-text (line)
  "Return heading text when LINE is a Markdown heading, else NIL."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Return) line))
         (len (length trimmed)))
    (when (and (> len 1)
               (char= (char trimmed 0) #\#))
      (let ((idx (loop for i from 0 below len
                       while (char= (char trimmed i) #\#)
                       finally (return i))))
        (when (and (< idx len)
                   (char= (char trimmed idx) #\Space))
          (string-trim '(#\Space #\Tab)
                       (subseq trimmed (1+ idx))))))))

(defun %extract-prompt-title (content default-title)
  "Extract the first Markdown heading from CONTENT, else DEFAULT-TITLE."
  (with-input-from-string (in content)
    (loop for raw = (read-line in nil nil)
          while raw
          for heading = (%markdown-heading-text raw)
          when (and heading (plusp (length heading)))
            do (return heading)
          finally (return default-title))))

(defun %extract-prompt-description (content)
  "Extract the first non-empty non-heading line from CONTENT."
  (with-input-from-string (in content)
    (loop for raw = (read-line in nil nil)
          while raw
          for line = (string-trim '(#\Space #\Tab #\Return) raw)
          unless (string= line "")
            do (unless (%markdown-heading-text line)
                 (return line))
          finally (return ""))))

(defun discover-prompts ()
  "Return prompt metadata list discovered from prompts/*.md.
Skips files that cannot be read and logs a warning for each."
  (let ((dir (prompts-directory)))
    (if (null dir)
        'nil
        (let ((files
                (sort (directory (merge-pathnames "*.md" dir))
                      #'string<
                      :key #'namestring)))
          (loop for file in files
                for name = (string-downcase (or (pathname-name file) ""))
                for content = (handler-case (read-prompt-file file)
                                (error (e)
                                  (log-event :warn
                                             "prompts.read-fail"
                                             "file" (namestring file)
                                             "error" (princ-to-string e))
                                  nil))
                when content
                  collect (make-ht "name" name
                                   "title" (%extract-prompt-title content name)
                                   "description" (%extract-prompt-description content)
                                   "file_path" (namestring file)))))))

(defun find-prompt-by-name (name)
  "Return prompt metadata hash table for NAME, or NIL when not found."
  (find-if (lambda (prompt)
             (string= (gethash "name" prompt) name))
           (discover-prompts)))
