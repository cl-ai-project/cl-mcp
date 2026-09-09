;;;; src/tools/registry.lisp
;;;;
;;;; Tool registry for MCP server.
;;;; Provides centralized registration and lookup of tool descriptors and handlers.
;;;;
;;;; A tool may belong to an optional GROUP.  Tools in a group are registered
;;;; like any other but stay hidden until the group is switched on, because a
;;;; tool that only makes sense alongside another system should not cost every
;;;; other user a line in tools/list and a description in the model's context.

(defpackage #:cl-mcp/src/tools/registry
  (:use #:cl)
  (:import-from #:uiop
                #:getenv)
  (:export #:register-tool
           #:get-tool-handler
           #:get-all-tool-descriptors
           #:clear-tool-registry
           #:*enabled-tool-groups*
           #:normalize-tool-group
           #:parse-tool-groups
           #:tool-group-enabled-p
           #:disabled-tool-group))

(in-package #:cl-mcp/src/tools/registry)

(defstruct tool-entry
  "Entry for a registered tool."
  (name "" :type string)
  (descriptor nil :type (or null hash-table))
  (handler nil :type (or null function))
  (group nil))

(defparameter *tool-registry* (make-hash-table :test #'equal)
  "Registry mapping tool names to tool-entry structs.")

(defun normalize-tool-group (group)
  "Return GROUP as the upper-case string the registry compares by.

Groups arrive as keywords from Lisp callers and as text from the environment,
and the two have to meet somewhere.  They meet as strings rather than as
symbols so that naming a group nothing has registered -- a typo, or a group
from a newer build -- costs a name that matches nothing instead of a symbol
interned into the image on a config value's say-so."
  (etypecase group
    (null nil)
    (string (string-upcase group))
    (symbol (string-upcase (symbol-name group)))))

(defvar *enabled-tool-groups* '()
  "Optional tool groups switched on for this process, as upper-case strings.

Empty by default: a group is opt-in.  Set from MCP_ENABLE_TOOL_GROUPS at load
time, and overridden by CL-MCP:RUN's :TOOL-GROUPS argument, which is the same
arrangement *USE-WORKER-POOL* has with MCP_NO_WORKER_POOL.")

(defun parse-tool-groups (text)
  "Return the group names in TEXT, a comma or space separated list.

  \"cl-spec\"            => (\"CL-SPEC\")
  \"cl-spec, other\"     => (\"CL-SPEC\" \"OTHER\")

Returns NIL for NIL or for text with no names in it."
  (when (stringp text)
    (let ((names '())
          (start 0))
      (flet ((emit (end)
               (let ((piece (string-trim '(#\Space #\Tab) (subseq text start end))))
                 (when (plusp (length piece))
                   (push (string-upcase piece) names)))))
        (loop for index from 0 below (length text)
              when (member (char text index) '(#\, #\Space #\Tab))
                do (emit index) (setf start (1+ index))
              finally (emit (length text))))
      (nreverse names))))

(let ((from-environment (parse-tool-groups (getenv "MCP_ENABLE_TOOL_GROUPS"))))
  (when from-environment
    (setf *enabled-tool-groups* from-environment)))

(defun tool-group-enabled-p (group)
  "Return true when GROUP is switched on, or when there is no group.

A tool with no group is always available; that is the ordinary case and it
must not depend on configuration."
  (let ((name (normalize-tool-group group)))
    (or (null name)
        (and (member name *enabled-tool-groups* :test #'string=) t))))

(defun register-tool (name descriptor handler &key group)
  "Register a tool with its NAME, DESCRIPTOR (hash-table), and HANDLER (function).
NAME should be a string like \"repl-eval\" or \"fs-read-file\".
DESCRIPTOR should be a hash-table with keys like \"name\", \"description\", \"inputSchema\".
HANDLER should be a function of (state id args) that returns a JSON-RPC result.

GROUP, when given, names an optional group: the tool is registered either way,
but stays out of tools/list and refuses calls until the group is switched on.
Registration is unconditional so that enabling a group at run time -- after
every module has loaded -- is enough to reach its tools."
  (setf (gethash name *tool-registry*)
        (make-tool-entry :name name
                         :descriptor descriptor
                         :handler handler
                         :group group))
  name)

(defun get-tool-handler (name)
  "Return the handler function for tool NAME, or NIL if not found or disabled."
  (let ((entry (gethash name *tool-registry*)))
    (when (and entry (tool-group-enabled-p (tool-entry-group entry)))
      (tool-entry-handler entry))))

(defun disabled-tool-group (name)
  "Return the group keeping tool NAME hidden, or NIL.

Lets a caller tell \"there is no such tool\" from \"that tool is real and is
switched off\", which are different answers to the same call: the first is a
mistake to correct, the second a setting to change."
  (let ((entry (gethash name *tool-registry*)))
    (when (and entry (not (tool-group-enabled-p (tool-entry-group entry))))
      (tool-entry-group entry))))

(defun get-all-tool-descriptors ()
  "Return a vector of the descriptors of every enabled tool."
  (let ((descriptors '()))
    (maphash (lambda (name entry)
               (declare (ignore name))
               (when (tool-group-enabled-p (tool-entry-group entry))
                 (push (tool-entry-descriptor entry) descriptors)))
             *tool-registry*)
    (coerce (nreverse descriptors) 'vector)))

(defun clear-tool-registry ()
  "Clear all registered tools. Mainly for testing."
  (clrhash *tool-registry*))
