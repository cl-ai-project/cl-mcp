;;;; tests/server-instructions-test.lisp

(defpackage #:cl-mcp/tests/server-instructions-test
  (:use #:cl)
  (:import-from #:rove
                #:deftest #:testing #:ok)
  (:import-from #:cl-mcp/src/tools/registry
                #:*enabled-tool-groups*
                #:register-tool-group-instructions
                #:enabled-tool-group-instructions
                #:get-all-tool-descriptors)
  (:import-from #:cl-mcp/src/server-instructions
                #:+instructions-budget+
                #:+base-instructions+
                #:server-instructions)
  ;; Loads every tool module, so the registry below is the one the server has.
  (:import-from #:cl-mcp/src/protocol
                #:process-json-line
                #:+supported-protocol-versions+)
  (:import-from #:cl-mcp/src/proxy
                #:*use-worker-pool*)
  (:import-from #:yason
                #:parse)
  ;; Bare import-from: only the ASDF dependency is needed, not the symbols.
  (:import-from #:cl-ppcre))

(in-package #:cl-mcp/tests/server-instructions-test)

(deftest group-instructions-follow-the-enabled-groups
  (let ((cl-mcp/src/tools/registry::*tool-group-instructions* '()))
    (register-tool-group-instructions :test-instructions-group "Test group text.")
    (testing "a group that is off contributes nothing"
      (let ((*enabled-tool-groups* '()))
        (ok (not (member "Test group text." (enabled-tool-group-instructions)
                         :test #'string=)))))
    (testing "a group that is on contributes its text"
      (let ((*enabled-tool-groups* (list "TEST-INSTRUCTIONS-GROUP")))
        (ok (member "Test group text." (enabled-tool-group-instructions)
                    :test #'string=))))
    (testing "re-registering replaces the text instead of adding a second one"
      (register-tool-group-instructions "test-instructions-group" "Replaced.")
      (let ((*enabled-tool-groups* (list "TEST-INSTRUCTIONS-GROUP")))
        (ok (equal '("Replaced.") (enabled-tool-group-instructions)))))))

(deftest group-instructions-follow-registration-order
  ;; F1: multiple groups compose in registration order, not enablement order,
  ;; and re-registering a group replaces its text where it already stands.
  (let ((cl-mcp/src/tools/registry::*tool-group-instructions* '()))
    (register-tool-group-instructions :group-a "A1")
    (register-tool-group-instructions :group-b "B")
    (register-tool-group-instructions :group-a "A2")
    (let ((*enabled-tool-groups* (list "GROUP-B" "GROUP-A")))
      (ok (equal (format nil "~A~%~%~A~%~%~A" +base-instructions+ "A2" "B")
                 (server-instructions))))))

(deftest register-tool-group-instructions-refuses-empty-names
  ;; F2: NIL and "" both normalize to "no group", which TOOL-GROUP-ENABLED-P
  ;; always treats as enabled, so their text would ship unconditionally.
  (dolist (group (list nil ""))
    (ok (handler-case (progn (register-tool-group-instructions group "text") nil)
          (error () t))
        (format nil "~S is refused" group))))

(defparameter *group-settings* (list '() (list "CL-SPEC"))
  "Every combination of the groups cl-mcp ships, as *ENABLED-TOOL-GROUPS* values.")

(defun %mentions-tool-p (text name)
  "True when TEXT names tool NAME as a word of its own, not inside a longer name."
  (cl-ppcre:scan (format nil "(?<![a-z0-9-])~A(?![a-z0-9-])"
                         (cl-ppcre:quote-meta-chars name))
                 text))

(defun %listed-tool-names ()
  (map 'list (lambda (d) (gethash "name" d)) (get-all-tool-descriptors)))

(deftest base-instructions-only-without-groups
  (let ((*enabled-tool-groups* '()))
    (ok (string= +base-instructions+ (server-instructions)))
    (ok (not (search "spec-check" (server-instructions))))))

(deftest cl-spec-instructions-follow-the-group
  (let ((*enabled-tool-groups* (list "CL-SPEC")))
    (let ((text (server-instructions)))
      (ok (eql 0 (search +base-instructions+ text)) "the base text comes first")
      (ok (search "function=" text))
      (ok (search "Replay:" text)))))

(deftest instructions-keep-the-prompts-workflow
  ;; What the summaries must not lose from prompts/: the Lisp tools, not a
  ;; shell ros or sbcl, run the code; a new behaviour gets its contract before
  ;; its code; a change is checked by both spec-check and run-tests; and any
  ;; contract change the request did not ask for needs the user.
  (ok (search "not ros or sbcl from a shell" +base-instructions+))
  ;; Naming grep/cat/sed left python and heredocs open; name every other way.
  (ok (search "no shell command, script or built-in Read/Edit"
              (substitute #\Space #\Newline +base-instructions+)))
  (let ((*enabled-tool-groups* (list "CL-SPEC")))
    (let ((text (format nil "~{~A~}" (enabled-tool-group-instructions))))
      (ok (search "before the code" text))
      (ok (search "re-run spec-check and run-tests" text))
      (ok (search "only when the request asks" text)))))

(deftest instructions-allow-the-recovery-lisp-edit-form-gives
  ;; Rule 2 keeps Lisp source to cl-mcp tools, yet a new file and a file that
  ;; no longer parses are written with fs-write-file, and lisp-edit-form's own
  ;; error sends the agent there.  The rule has to allow that path, and the
  ;; path may name only tools the client can see.
  (ok (search "fs-write-file only for" +base-instructions+))
  (ok (search "a new or unparseable file" +base-instructions+))
  (let* ((root (asdf:system-source-directory :cl-mcp))
         (cl-mcp/src/project-root:*project-root* root)
         (relative "tests/tmp/instructions-unparseable.lisp")
         (path (merge-pathnames relative root)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out "(defun a ()~%  (list 1 2)~%~%(defun b () 2)~%"))
    (unwind-protect
         (let ((message (handler-case
                            (progn (cl-mcp/src/lisp-edit-form:lisp-edit-form
                                    :file-path relative :form-type "defun" :form-name "b"
                                    :operation "insert_after" :content "(defun c () 3)")
                                   nil)
                          (error (e) (princ-to-string e))))
               (listed (%listed-tool-names)))
           (ok message "lisp-edit-form refuses a file that does not parse")
           (ok (and message (%mentions-tool-p message "fs-write-file"))
               "the recovery writes the file back with fs-write-file")
           (loop for name being the hash-keys of cl-mcp/src/tools/registry::*tool-registry*
                 when (and message (%mentions-tool-p message name))
                   do (ok (member name listed :test #'string=)
                          (format nil "the recovery names ~A, which tools/list shows" name))))
      (ignore-errors (delete-file path)))))

(deftest instructions-fit-the-budget
  (dolist (groups *group-settings*)
    (let ((*enabled-tool-groups* groups))
      (ok (<= (length (server-instructions)) +instructions-budget+)
          (format nil "~S: ~D characters"
                  groups (length (server-instructions)))))))

(deftest core-rules-lead-the-base-text
  ;; ChatGPT asks for the essentials in the first 512 characters; Claude Code
  ;; cuts from the end.  Either way the opening has to stand alone.
  (dolist (tool '("fs-set-project-root" "lisp-edit-form" "load-system"))
    (let ((position (search tool +base-instructions+)))
      (ok (and position (< position 512))
          (format nil "~A must appear within the first 512 characters" tool)))))

(deftest instructions-name-only-listed-tools
  ;; The point of deriving both from *ENABLED-TOOL-GROUPS*: guidance never
  ;; recommends a tool the client cannot see.
  (let ((registered (loop for name being the hash-keys
                            of cl-mcp/src/tools/registry::*tool-registry*
                          collect name)))
    (dolist (groups *group-settings*)
      (let* ((*enabled-tool-groups* groups) ; mallet:suppress needless-let*
             (text (server-instructions))
             (listed (%listed-tool-names)))
        (dolist (name registered)
          (when (%mentions-tool-p text name)
            (ok (member name listed :test #'string=)
                (format nil "~S names ~A, which tools/list hides" groups name))))))))

(deftest instructions-name-real-tools
  ;; Guards the text against a misspelt or renamed tool.
  (let ((*enabled-tool-groups* (list "CL-SPEC")))
    (let ((listed (%listed-tool-names)))
      (dolist (name '("fs-set-project-root" "clgrep-search" "lisp-read-file"
                      "lisp-edit-form" "lisp-patch-form" "load-system" "repl-eval"
                      "run-tests" "clos-describe" "lisp-macroexpand"
                      "inspect-object" "lisp-check-parens" "spec-list"
                      "spec-symbol" "spec-describe" "spec-check"))
        (ok (member name listed :test #'string=) name)
        (ok (%mentions-tool-p (server-instructions) name)
            (format nil "the instructions name ~A" name))))))

(defun %initialize-result (version)
  (let ((*use-worker-pool* nil))
    (gethash "result"
             (parse (process-json-line
                     (format nil "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",~
                                  \"params\":{\"protocolVersion\":\"~A\"}}"
                             version))))))

(deftest initialize-returns-the-instructions
  (dolist (groups *group-settings*)
    (let ((*enabled-tool-groups* groups))
      (dolist (version +supported-protocol-versions+)
        (let ((result (%initialize-result version)))
          (ok (equal (server-instructions) (gethash "instructions" result))
              (format nil "~A with groups ~S" version groups)))))))
