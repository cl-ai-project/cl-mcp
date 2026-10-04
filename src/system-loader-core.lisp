;;;; src/system-loader-core.lisp
;;;;
;;;; Core system loading logic, shared between parent and worker processes.
;;;; Uses only ASDF (no Quicklisp dependency).

(defpackage #:cl-mcp/src/system-loader-core
  (:use #:cl)
  (:import-from #:cl-mcp/src/utils/deadline
                #:call-with-deadline-thread)
  (:import-from #:cl-mcp/src/utils/request-debugger-boundary
                #:request-debugger-escape-error-p
                #:request-debugger-escape-error-display-text)
  (:import-from #:cl-mcp/src/log
                #:log-event)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht
                #:transient-error)
  (:import-from #:cl-mcp/src/utils/sanitize
                #:sanitize-for-json)
  (:import-from #:cl-mcp/src/utils/paths
                #:discover-asd-in-project)
  (:import-from #:cl-mcp/src/utils/fasls
                #:fasl-source-directory)
  (:export #:load-system
           #:*system-load-lock-wrapper*
           #:*last-compiler-stderr*))

(in-package #:cl-mcp/src/system-loader-core)

(declaim (ftype (function (function (or number null)) (values t &rest t))
                %load-with-timeout))

(defvar *system-load-lock-wrapper* ()
  "Optional function of one argument (a thunk) wrapping the ASDF work.

Bound by the worker handler to WITH-ASDF-LOAD-LOCK.  It is read on the
caller's thread but applied INSIDE the deadline thread, which is the whole
point: the thread that performs the ASDF work is then the thread that owns the
lock.  A load that outlives its deadline keeps the lock it is still using, so
the next load gets that lock's own timeout error instead of quietly starting a
second ASDF operation alongside the first.  Wrapping outside the deadline
thread would release the lock the moment the caller was answered, while ASDF
was still running.

It also puts the time spent waiting for the lock inside the caller's deadline,
rather than ahead of it.")

(defun %load-with-timeout (thunk timeout-seconds)
  "Execute THUNK under a TIMEOUT-SECONDS deadline on its own thread.
Returns (values result-list timed-out-p errored-p leaked-p).
RESULT-LIST is a list of the thunk's multiple return values on success,
or a single-element list containing the error condition on failure.
TIMED-OUT-P is T if the load was still running when the deadline passed.
ERRORED-P is T if the thunk signaled.
LEAKED-P is T when the load thread outlived both the cooperative unwind and
DESTROY-THREAD.  It matters to the caller: that thread is still inside ASDF,
so the load this call gave up on keeps mutating the image's ASDF, package and
compiler state while later requests run against it.

*SYSTEM-LOAD-LOCK-WRAPPER*, when installed, is applied around THUNK on the
deadline thread rather than around this call, so the lock and the work it
protects share a thread.  See its docstring.

If the load completes while the deadline is being enforced, the result is
returned as a success -- completed work is never discarded as a timeout.
See CALL-WITH-DEADLINE-THREAD for how the deadline is enforced."
  (let* ((wrapper *system-load-lock-wrapper*)
         ;; Read here, on the caller's thread, and applied there, on the
         ;; deadline thread: a dynamic binding made by the caller is not
         ;; visible inside a thread it spawns.
         (wrapped (if wrapper
                      (lambda () (funcall wrapper thunk))
                      thunk)))
    (handler-case
        (multiple-value-bind (result status leaked)
            (call-with-deadline-thread wrapped timeout-seconds
                                       :name "mcp-load-system")
          (when leaked
            (ignore-errors
             (log-event :warn "load.timeout.thread-leaked"
                        "name" "mcp-load-system"
                        "timeout" timeout-seconds)))
          (ecase status
            (:ok (values result nil nil nil))
            (:timeout (values nil t nil leaked))
            (:error (values (list result) nil t nil))))
      ;; Only reachable on the inline path (no deadline), where
      ;; CALL-WITH-DEADLINE-THREAD lets conditions propagate.
      (error (c)
        (values (list c) nil t nil)))))

(defvar *last-compiler-stderr* nil
  "Captured compiler stderr from the most recent %call-with-suppressed-output call.
Always set via unwind-protect so it survives error unwinds.  When the call
completes normally this is set to NIL; on error it holds the stderr string
accumulated up to the point of failure.")

(defvar *auto-discovered-asd* nil
  "When non-NIL, holds the namestring of the .asd file that was auto-discovered
and registered during the most recent load-system call.  Bound dynamically so
that response builders can include an informational hint.")

(defun %redefinition-warning-p (warning)
  "Return T when WARNING is an SBCL \"redefining X in DEFUN/DEFMACRO/...\"
notification that is pure noise under force=true reloads. Uses the
condition class where available and falls back to a textual prefix match
on other implementations so the filter still works in portable images."
  (or #+sbcl
      (let ((cls (find-class 'sb-kernel:redefinition-warning nil)))
        (and cls (typep warning cls)))
      (let ((text (ignore-errors (princ-to-string warning))))
        (and (stringp text)
             (uiop:string-prefix-p "redefining " text)
             ;; Require " in " so unrelated warnings happening to start
             ;; with "redefining " (e.g. method-combination chatter)
             ;; are not mistakenly muffled.
             (search " in " text)))))

(defun %definition-source-name (object)
  "Return the source namestring SBCL recorded for OBJECT -- a function, a macro
function, a generic function or a method -- or NIL when it cannot be told.  The
name is as recorded, not yet known to be a file (%SOURCE-FILE)."
  #+sbcl
  (or (ignore-errors
       (and (functionp object)
            (not (typep object 'generic-function))
            (sb-c::debug-source-namestring
             (sb-c::debug-info-source
              (sb-kernel:%code-debug-info
               (sb-kernel:fun-code-header (sb-kernel:%fun-fun object)))))))
      (ignore-errors
       (sb-c:definition-source-location-namestring
        (sb-pcl::definition-source object))))
  #-sbcl
  (progn object nil))

(defun %new-definition-source-name (warning)
  "Return the source namestring of the definition WARNING, an SBCL redefinition
warning, announces -- recorded the same way as %DEFINITION-SOURCE-NAME's -- or NIL.
A DEFGENERIC's or DEFMETHOD's warning carries it as its new location; a DEFUN's or
DEFMACRO's loaded from a fasl carries no location, only the new function."
  #+sbcl
  (flet ((slot (name)
           (and (slot-exists-p warning name)
                (slot-boundp warning name)
                (slot-value warning name))))
    (ignore-errors
     (let ((location (slot 'sb-kernel::new-location))
           (function (slot 'sb-kernel::new-function)))
       (or (and location (sb-c:definition-source-location-namestring location))
           (and function (%definition-source-name function))))))
  #-sbcl
  (progn warning nil))

(defun %source-file (name)
  "Return the file NAME, a recorded source namestring, names, or NIL when it names
none: a logical pathname as is (SBCL's own sources, SYS:SRC; and SYS:CONTRIB;),
and an absolute path to an existing file as its truename, so a file reached
through a symbolic link is one file.  SBCL records a NAMESTRING, which escapes
[, * and ? with a backslash, so NAME is read as one first and as a native name
after.  Code compiled inside another compilation unit can record that unit's
name instead -- repl-eval gives every file compiled in it \"repl-eval\" -- and
that names no file."
  (and (stringp name)
       (plusp (length name))
       (if (char= (char name 0) #\/)
           (or (ignore-errors (probe-file (parse-namestring name)))
               (ignore-errors (probe-file (uiop:parse-native-namestring name))))
           (ignore-errors (logical-pathname name)))))

(defun %cached-source-file (name cache)
  "%SOURCE-FILE of NAME, remembered in CACHE, an EQUAL hash table: a load
redefines many definitions of one file."
  (multiple-value-bind (file present-p) (gethash name cache)
    (if present-p
        file
        (setf (gethash name cache) (%source-file name)))))

(defun %redefined-definition (warning)
  "Return the definition WARNING, an SBCL redefinition warning, is about to
replace: the old method, or the old macro function or function of its name,
whichever it was -- a DEFUN can replace a macro and a DEFMACRO a function, and
FDEFINITION of a macro is SBCL's own guard, defined in no file of the project --
or NIL."
  #+sbcl
  (ignore-errors
   (if (typep warning 'sb-kernel:redefinition-with-defmethod)
       (slot-value warning 'sb-kernel::old-method)
       (let ((name (slot-value warning 'sb-kernel::name)))
         (or (and (symbolp name) (macro-function name))
             (and (fboundp name) (fdefinition name))))))
  #-sbcl
  (progn warning nil))

(defun %name-prefix-p (prefix string)
  "True when STRING starts with PREFIX, ignoring case."
  (and (<= (length prefix) (length string))
       (string-equal prefix string :end2 (length prefix))))

(defun %copy-of-project-file-p (old new root project-name)
  "True when OLD, a file outside ROOT, is another copy of NEW, a file under ROOT,
one of the project's directories: OLD's path ends with NEW's path relative to
ROOT, and the directory that path hangs from is named after the project --
PROJECT-NAME, its primary system's name, or ROOT's own name -- as another
checkout or a Quicklisp dist's release directory (alexandria-20241012-git) is.
A project the worker had already loaded from elsewhere -- one of cl-mcp's own
dependencies, being developed -- replaces every definition of its old copy, and
none of that is a conflict.  A library file that only shares the path, such as
a lists.lisp of its own, is not a copy."
  (and (not (typep old 'logical-pathname))
       (not (uiop:subpathp old root))
       (let ((old-name (uiop:native-namestring old))
             (suffix (concatenate 'string "/"
                                  (uiop:native-namestring
                                   (uiop:enough-pathname new root)))))
         (and (uiop:string-suffix-p old-name suffix)
              (let* ((parent (subseq old-name 0 (- (length old-name) (length suffix))))
                     (directory (subseq parent
                                        (1+ (or (position #\/ parent :from-end t) -1)))))
                (some (lambda (name)
                        (and (stringp name)
                             (plusp (length name))
                             (%name-prefix-p name directory)))
                      (list project-name (car (last (pathname-directory root))))))))))

(defun %redefinition-conflict (warning project-directories project-name cache)
  "When WARNING, an SBCL redefinition warning, is a conflict the project can act
on, return the old definition's file and the new one's; otherwise NIL.

A conflict is a file under one of PROJECT-DIRECTORIES, truenames, replacing what
another file defined: two of its files defining one name, or one of them
clobbering a library's or SBCL's definition (a DEFUN on a symbol inherited by
:USE).  Not one: a file redefining its own definitions -- a reload, which
load-system exists to do -- anything a dependency redefines, a copy of the same
file from elsewhere (%COPY-OF-PROJECT-FILE-P, with PROJECT-NAME), and anything
whose old or new source names no file.  A definition moved from one file of the
project to another is reported once, by the load that moves it, since the image
still holds the old file's.  A name defined twice in one file is not told here:
SBCL's own DUPLICATE-DEFINITION warning reports a DEFUN or DEFMACRO, and nothing
reports a DEFMETHOD or DEFGENERIC.  CACHE is %CACHED-SOURCE-FILE's."
  (let* ((new (and project-directories
                   (%cached-source-file (%new-definition-source-name warning) cache)))
         (root (and new
                    (not (typep new 'logical-pathname))
                    (find-if (lambda (directory) (uiop:subpathp new directory))
                             project-directories))))
    (when root
      (let ((old (%cached-source-file
                  (%definition-source-name (%redefined-definition warning))
                  cache)))
        (when (and old
                   (not (equal old new))
                   (not (%copy-of-project-file-p old new root project-name)))
          (values old new))))))

(defun %display-file (file)
  "FILE, a pathname from %SOURCE-FILE, as text for a warning's details."
  (if (typep file 'logical-pathname)
      (namestring file)
      (uiop:native-namestring file)))

(defun %decide-suppress-redefinition (flag)
  "Resolve the `suppress-redefinition-warnings` flag to a mode of
%CALL-WITH-SUPPRESSED-OUTPUT.

  :auto  - :CONFLICTS: drop every redefinition but a conflict the project can
           act on (%REDEFINITION-CONFLICT).  A reload redefines what it
           reloads, and a dependency the worker had already loaded is read
           again; neither tells anyone anything.
  T      - always suppress.
  NIL    - never suppress."
  (if (eq flag :auto) :conflicts flag))

(defun %project-directories (system-name)
  "Return the truenames of the directories of SYSTEM-NAME's primary system -- the
project a load-system of it is about, whose files' conflicts are worth a word:
the directory of its .asd first, then that of its components when :PATHNAME
puts them elsewhere (an .asd in systems/ with :pathname \"../src/\") -- or NIL
when ASDF cannot find it."
  (ignore-errors
   (let ((system (asdf:find-system (asdf:primary-system-name system-name) nil)))
     (and system
          (remove-duplicates
           (loop for directory in (list (asdf:system-source-directory system)
                                        (asdf:component-pathname system))
                 for truename = (and directory (ignore-errors (truename directory)))
                 when truename collect truename)
           :test #'equal :from-end t)))))

(defun %call-with-suppressed-output (thunk &key suppress-redefinition project-directories
                                                project-name)
  "Call THUNK with compilation and load output suppressed.
Returns (values thunk-result warning-count warning-details compiler-stderr).
The stderr string is also saved to *last-compiler-stderr* via unwind-protect
so it survives error unwinds and can be retrieved by callers that catch the error.

When SUPPRESS-REDEFINITION is non-nil, warnings identified by
%REDEFINITION-WARNING-P are silently muffled and do not increment the
returned count.  When it is :CONFLICTS, a conflict the project in
PROJECT-DIRECTORIES, truenames, can act on is kept instead
(%REDEFINITION-CONFLICT, with PROJECT-NAME, its primary system's name), and
its details name both files; without PROJECT-DIRECTORIES nothing is one.  Useful under force=true reloads where
'redefining X in DEFUN' lines are noise that drown real warnings."
  (let ((warning-count 0)
        (warning-details (make-string-output-stream))
        (stderr (make-string-output-stream))
        (source-files (make-hash-table :test #'equal)))
    ;; Reset before each call so stale data from a previous run is not
    ;; mistakenly attributed to this invocation.
    (setf *last-compiler-stderr* nil)
    (flet ((handle-warning (w)
             (let ((redefinition-p (and suppress-redefinition
                                        (%redefinition-warning-p w))))
               (multiple-value-bind (old new)
                   (and redefinition-p
                        (eq suppress-redefinition :conflicts)
                        (%redefinition-conflict w project-directories project-name
                                                source-files))
                 (cond
                   ((and redefinition-p (not old))
                    (when (find-restart 'muffle-warning)
                      (invoke-restart 'muffle-warning)))
                   (t
                    (incf warning-count)
                    (if old
                        (format warning-details "~A (defined in ~A, redefined in ~A)~%"
                                w (%display-file old) (%display-file new))
                        (format warning-details "~A~%" w))
                    (when (find-restart 'muffle-warning)
                      (invoke-restart 'muffle-warning))))))))
      #+sbcl
      (let ((err-sym (find-symbol "*COMPILER-ERROR-OUTPUT*" "SB-C"))
            (note-sym (find-symbol "*COMPILER-NOTE-STREAM*" "SB-C"))
            (trace-sym (find-symbol "*COMPILER-TRACE-OUTPUT*" "SB-C"))
            (syms nil)
            (vals nil))
        (when err-sym (push err-sym syms) (push stderr vals))
        (when note-sym (push note-sym syms) (push stderr vals))
        (when trace-sym (push trace-sym syms) (push nil vals))
        (let ((result nil)
              (completed-p nil))
          (unwind-protect
              (progn
                (setf result
                      (handler-bind ((warning #'handle-warning))
                        (let* ((interactive
                                 (make-two-way-stream
                                  (make-concatenated-stream) stderr))
                               (*compile-verbose* nil)
                               (*compile-print* nil)
                               (*load-verbose* nil)
                               (*load-print* nil)
                               ;; Discarded rather than collected: these two
                               ;; are bound purely to suppress, and nothing
                               ;; ever reads them back.  A string stream would
                               ;; hold every character a noisy compile emits
                               ;; for output no one will ever see.
                               (*standard-output* (make-broadcast-stream))
                               (*trace-output* (make-broadcast-stream))
                               (*error-output* stderr)
                               ;; log4cl's console appender writes to a synonym
                               ;; stream for *DEBUG-IO*, which in a worker
                               ;; resolves to the original stdout fd whose read
                               ;; end the parent closed after the handshake --
                               ;; any write there raises BROKEN-PIPE and aborts
                               ;; the load.  Rebinding these interactive streams
                               ;; keeps that output captured instead of hitting
                               ;; the dead pipe.  A two-way stream, because ANSI
                               ;; requires these three to be bidirectional: a
                               ;; system whose load-time code asks Y-OR-N-P
                               ;; should read EOF, not fail on "not an input
                               ;; stream".
                               (*debug-io* interactive)
                               (*terminal-io* interactive)
                               (*query-io* interactive))
                          (if syms
                              (progv (nreverse syms) (nreverse vals)
                                (with-compilation-unit (:override t)
                                  (funcall thunk)))
                              (with-compilation-unit (:override t)
                                (funcall thunk))))))
                (setf completed-p t)
                (values result warning-count
                        (get-output-stream-string warning-details)
                        (get-output-stream-string stderr)))
            ;; Always capture stderr so it survives error unwind.
            (unless completed-p
              (setf *last-compiler-stderr*
                    (ignore-errors (get-output-stream-string stderr)))))))
      #-sbcl
      (let ((result nil)
            (completed-p nil))
        (unwind-protect
            (progn
              (setf result
                    (handler-bind ((warning #'handle-warning))
                      (let* ((interactive
                               (make-two-way-stream
                                (make-concatenated-stream) stderr))
                             (*compile-verbose* nil)
                             (*compile-print* nil)
                             (*load-verbose* nil)
                             (*load-print* nil)
                             (*standard-output* (make-broadcast-stream))
                             (*trace-output* (make-broadcast-stream))
                             (*error-output* stderr)
                             (*debug-io* interactive)
                             (*terminal-io* interactive)
                             (*query-io* interactive))
                        (funcall thunk))))
              (setf completed-p t)
              (values result warning-count
                      (get-output-stream-string warning-details)
                      (get-output-stream-string stderr)))
          ;; Always capture stderr so it survives error unwind.
          (unless completed-p
            (setf *last-compiler-stderr*
                  (ignore-errors (get-output-stream-string stderr)))))))))

(defun %delete-system-fasls (system-name)
  "Delete the cached fasls under SYSTEM-NAME's output-translation
directory (FASL-SOURCE-DIRECTORY).  ASDF's :FORCE T only forces the named
system, not its dependencies — for package-inferred systems the actual code
lives in dependency subsystems, so forcing the top system alone recompiles
nothing, and a source edit landing in the same second as the previous
compile is masked by second-granularity FILE-WRITE-DATE.  Deleting the
fasls makes recompilation unconditional.

Returns two values: the number of files deleted (0 when no system or cache
directory is found), and the name of the system whose directory was
cleared, or NIL."
  (multiple-value-bind (source-dir cleared) (fasl-source-directory system-name)
    (if (null source-dir)
        (values 0 nil)
        (let ((deleted 0))
          (dolist (fasl (directory
                         (merge-pathnames
                          "**/*.fasl"
                          (asdf:apply-output-translations source-dir))))
            (when (ignore-errors (delete-file fasl) t)
              (incf deleted)))
          (values deleted cleared)))))

(declaim (ftype (function (string &key (:force boolean)
                                       (:clear-fasls boolean)
                                       (:timeout-seconds (or null (real (0))))
                                       (:suppress-redefinition-warnings t))
                          (values hash-table &rest t))
                load-system))

(defun load-system
       (system-name &key (force t) (clear-fasls nil) (timeout-seconds 120)
                         (suppress-redefinition-warnings :auto))
  "Load ASDF system SYSTEM-NAME with structured result.

When FORCE is true (default), clears loaded state before loading so
changed files are picked up. When CLEAR-FASLS is true, deletes the
system's cached fasls (its output-translation directory) before
loading, guaranteeing recompilation from source — including
package-inferred dependency subsystems that :FORCE T alone would not
rebuild. TIMEOUT-SECONDS must be a positive number
or NIL (no timeout). Default is 120 seconds.

SUPPRESS-REDEFINITION-WARNINGS controls whether SBCL
'redefining X in DEFUN' style notifications are dropped from the
captured warning stream.  Values:
  :auto  - suppress all but a conflict the project can act on: a file
           under SYSTEM-NAME's primary system's directories replacing
           what another file defined (%REDEFINITION-CONFLICT).  A reload
           redefines what it reloads, and a dependency the worker had
           already loaded is read again; both are dropped.
  T      - always suppress.
  NIL    - never suppress (preserve pre-change behavior).

If ASDF signals MISSING-COMPONENT for the requested system, searches
*project-root* for a matching .asd file and retries once after
registering it."
  (check-type system-name string)
  (check-type timeout-seconds (or null (real (0))))
  (let ((system-name (string-downcase system-name))
        (start-time (get-internal-real-time))
        ;; Set by the load thread; read after it has been joined.
        (fasls-deleted nil)
        (fasls-cleared-from nil))
    (setf *auto-discovered-asd* nil)
    (log-event :info "load-system" "system" system-name "force" force
               "clear_fasls" clear-fasls "timeout" timeout-seconds)
    (multiple-value-bind (result-list timed-out-p errored-p leaked-p)
        (%load-with-timeout
         (lambda ()
           (flet ((%do-load ()
                    (when clear-fasls
                      (multiple-value-bind (count from)
                          (%delete-system-fasls system-name)
                        ;; Summed: a retry after auto-discovery clears again.
                        (setf fasls-deleted (+ (or fasls-deleted 0) count)
                              fasls-cleared-from (or from fasls-cleared-from))))
                    (when (and force
                               (member system-name
                                       (asdf:already-loaded-systems)
                                       :test #'string-equal))
                      (let ((asd-src
                              (ignore-errors
                               (asdf:system-source-file
                                (asdf:find-system system-name nil)))))
                        (asdf:clear-system system-name)
                        (when asd-src
                          (ignore-errors
                           (asdf:load-asd asd-src)))))
                    (%call-with-suppressed-output
                     (lambda ()
                       (asdf:load-system system-name :force clear-fasls))
                     :suppress-redefinition
                     (%decide-suppress-redefinition suppress-redefinition-warnings)
                     :project-directories (%project-directories system-name)
                     :project-name (asdf:primary-system-name system-name))))
             (handler-case (%do-load)
               (asdf/find-component:missing-component (c)
                 (let* ((missing (princ-to-string
                                  (asdf/find-component:missing-requires c)))
                        (root-name (subseq system-name
                                           0 (or (position #\/ system-name)
                                                 (length system-name))))
                        (asd-path
                          (when (or (string-equal system-name missing)
                                    (string-equal root-name missing))
                            (discover-asd-in-project system-name))))
                   (unless asd-path
                     (error c))
                   (log-event :info "load-system-auto-discover"
                              "system" system-name
                              "asd_path" (namestring asd-path))
                   (asdf/find-system:load-asd asd-path)
                   (setf *auto-discovered-asd* (namestring asd-path))
                   (%do-load))))))
         timeout-seconds)
      (let ((elapsed-ms
              (round
               (* 1000
                  (/ (- (get-internal-real-time) start-time)
                     internal-time-units-per-second))))
            (ht (make-ht "system" system-name)))
        (cond
          (timed-out-p (setf (gethash "status" ht) "timeout")
           (setf (gethash "duration_ms" ht) elapsed-ms)
           (setf (gethash "message" ht)
                 (if leaked-p
                     (format nil "Load timed out after ~,2F seconds and could ~
                                  not be stopped: it is still running in this ~
                                  worker and may hold the ASDF load lock. Use ~
                                  pool-kill-worker to get a fresh worker."
                             timeout-seconds)
                     (format nil "Load timed out after ~,2F seconds"
                             timeout-seconds)))
           (log-event :warn "load-system-timeout" "system" system-name
                      "timeout" timeout-seconds
                      "thread_leaked" (if leaked-p "true" "false")))
          (errored-p
           (let* ((err (first result-list))
                  (saved-text
                    (when (request-debugger-escape-error-p err)
                      (request-debugger-escape-error-display-text err)))
                  (compiler-stderr *last-compiler-stderr*))
             (setf (gethash "status" ht) "error")
             (setf (gethash "duration_ms" ht) elapsed-ms)
             ;; Carried so the response builder can withhold its standing
             ;; advice to replace the worker.  A transient error means this
             ;; load never started -- another load still holds the lock --
             ;; so it changed nothing, and that advice would destroy work
             ;; about to finish.  That is all it says: not that the image is
             ;; sound, which nothing here has checked.  The builder consumes
             ;; the key; it is not part of the response.
             (when (typep err 'transient-error)
               (setf (gethash "load_not_started" ht) t))
             (setf (gethash "message" ht)
                   (sanitize-for-json
                    (or saved-text
                        (ignore-errors (princ-to-string err))
                        (format nil "~A" (type-of err)))))
             (when (and (stringp compiler-stderr)
                        (plusp (length compiler-stderr)))
               (setf (gethash "compiler_output" ht)
                     (sanitize-for-json compiler-stderr)))
             (log-event :error "load-system-error" "system" system-name
                        "error" (or saved-text
                                    (ignore-errors (princ-to-string err))
                                    "unprintable error"))))
          (t
           (destructuring-bind
               (load-result warning-count warning-details
                &optional compiler-stderr)
               result-list
             (declare (ignore load-result compiler-stderr))
             (setf (gethash "status" ht) "loaded")
             (setf (gethash "duration_ms" ht) elapsed-ms)
             (setf (gethash "forced" ht) force)
             (setf (gethash "clear_fasls" ht) clear-fasls)
             (setf (gethash "warnings" ht) warning-count)
             (when (plusp warning-count)
               (setf (gethash "warning_details" ht)
                     (sanitize-for-json warning-details)))
             (log-event :info "load-system-complete" "system" system-name
                        "duration_ms" elapsed-ms "warnings" warning-count))))
        (when *auto-discovered-asd*
          (setf (gethash "auto_discovered_asd" ht) *auto-discovered-asd*))
        ;; What clear_fasls did, not only that it was asked: a request that
        ;; deleted nothing forced no recompilation, and the caller must be
        ;; able to see that.
        (when (and clear-fasls fasls-deleted)
          (setf (gethash "fasls_deleted" ht) fasls-deleted)
          (when fasls-cleared-from
            (setf (gethash "fasls_cleared_from" ht) fasls-cleared-from)))
        ht))))
