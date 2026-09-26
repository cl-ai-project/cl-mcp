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
      (ok (equal '("Replaced.") (enabled-tool-group-instructions))))))

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
