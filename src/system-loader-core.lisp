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
                #:discover-asd-in-project
                #:normalize-path-for-display)
  (:import-from #:cl-mcp/src/code-core
                #:%offset->line)
  (:import-from #:cl-mcp/src/utils/fasls
                #:fasl-source-directory
                #:delete-same-second-fasls)
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
notification, which LOAD-SYSTEM drops on a first load and a reload alike. Uses the
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

(defvar *last-warning-records* nil
  "The warning records of the most recent %CALL-WITH-SUPPRESSED-OUTPUT call that
ended by unwinding: what had been recorded when the error struck, in the form
the call returns them.  NIL after a call that completed.  Kept as
*LAST-COMPILER-STDERR* is, so the caller that catches the error can still say
which warnings came before it.")

(defun %warning-severity (warning)
  "Return \"style-warning\" for a STYLE-WARNING and \"warning\" for any other
warning.  It is the distinction a build turns on: only the second makes
COMPILE-FILE report a failure."
  (if (typep warning 'style-warning) "style-warning" "warning"))

(defun %warning-class-name (warning)
  "Return the name of WARNING's class with its package, as SB-INT:TYPE-WARNING
is written."
  (let ((*package* (find-package :keyword))
        (*print-case* :upcase))
    (prin1-to-string (type-of warning))))

(defun %warning-kind-key (warning)
  "Return what tells one kind of warning from another: WARNING's class and, for a
SIMPLE-CONDITION, its format control too.  SBCL signals \"defined but never
used\", \"undefined function\" and a good many more as one class, so the
control is all that separates them.  The key is compared, never shown."
  (cons (type-of warning)
        (when (typep warning 'simple-condition)
          (let ((control (simple-condition-format-control warning)))
            (if (stringp control)
                control
                ;; SBCL hands over a tokenized control; its string is what two
                ;; warnings of one kind share.
                (let ((reader (and (find-package "SB-FORMAT")
                                   (find-symbol "FMT-CONTROL-STRING" "SB-FORMAT"))))
                  (or (and reader
                           (fboundp reader)
                           (ignore-errors (funcall reader control)))
                      control)))))))

(defun %compiler-place ()
  "Return three values saying where the compiler is: the file it is reading, the
octet position at which it began to read the top-level form it is compiling,
and the definitions that enclose the code -- what SBCL prints after \"in:\".
No values when no compilation is under way.

Read from the compiler's own error context, as SLIME reads it.  Each accessor
is looked up by name, so an SBCL that lacks one yields no place rather than an
error."
  (let ((package (find-package "SB-C")))
    (flet ((call (name &rest arguments)
             (let ((function (and package (find-symbol name package))))
               (when (and function (fboundp function))
                 (ignore-errors (apply function arguments))))))
      (let ((context (call "FIND-ERROR-CONTEXT" nil)))
        (if context
            (values (call "COMPILER-ERROR-CONTEXT-FILE-NAME" context)
                    (call "COMPILER-ERROR-CONTEXT-FILE-POSITION" context)
                    (call "COMPILER-ERROR-CONTEXT-CONTEXT" context))
            (values))))))

(defun %enclosing-form-label (context)
  "Return the top-level definition CONTEXT names as source text, such as
\"(defun wrong-arity)\", or NIL.  CONTEXT is the list SBCL prints after \"in:\";
its first element is the definition lisp-edit-form would address.  Printed in
the package of the name it defines, so the name stands bare."
  (let ((form (first context)))
    (when (consp form)
      (let* ((name (second form))
             (symbol (cond ((symbolp name) name)
                           ((and (consp name) (symbolp (second name)))
                            (second name))))
             (*package* (or (and symbol (symbol-package symbol)) *package*))
             (*print-case* :downcase)
             (*print-pretty* nil))
        (ignore-errors (prin1-to-string form))))))

(defun %make-warning-record (warning)
  "Return WARNING as a property list: :SEVERITY, :CLASS, :MESSAGE, the :KIND-KEY
that groups it and, when it was signalled during a compilation, the :FILE, the
octet :POSITION and the enclosing :FORM.  Runs in the handler, inside whatever
signalled, so it must not signal itself: a part that cannot be had is left out."
  (let ((class (or (ignore-errors (%warning-class-name warning)) "WARNING")))
    (multiple-value-bind (file position context)
        (ignore-errors (%compiler-place))
      (list :severity (%warning-severity warning)
            :class class
            :message (or (ignore-errors (princ-to-string warning)) class)
            :kind-key (ignore-errors (%warning-kind-key warning))
            :file (and (typep file '(or pathname string)) file)
            :position (and (integerp position) position)
            :form (ignore-errors (%enclosing-form-label context))))))

(defun %repeated-at-load-p (record records)
  "True when RECORD only says again what one of RECORDS says.  SBCL signals a
duplicate definition, a package at variance and some others twice: compiling
the file, where the compiler knows the place, and loading what it compiled,
where nothing does.  The second, placeless one is the repeat."
  (and (null (getf record :file))
       (find-if (lambda (earlier)
                  (and (getf earlier :file)
                       (equal (getf earlier :class) (getf record :class))
                       (equal (getf earlier :message) (getf record :message))))
                records)
       t))

(defun %warning-record-tables (records)
  "Return RECORDS, property lists in the order signalled, as the JSON objects
load-system reports: \"severity\", \"class\", \"message\", \"kind\" -- a number
the records of one kind share, counted from 1 in order of first appearance --
and, for a warning from a compilation, \"file\", \"line\" and \"form\".  The line
is that of the top-level form; SBCL keeps no finer place."
  (let ((kinds '()))
    (mapcar
     (lambda (record)
       (let* ((key (getf record :kind-key))
              (kind (or (position key kinds :test #'equal)
                        (progn (setf kinds (append kinds (list key)))
                               (1- (length kinds)))))
              (file (getf record :file))
              (table (make-ht "severity" (getf record :severity)
                              "class" (sanitize-for-json (getf record :class))
                              "message" (sanitize-for-json (getf record :message))
                              "kind" (1+ kind))))
         (when file
           (let ((shown (ignore-errors (normalize-path-for-display file)))
                 (line (ignore-errors (%offset->line file (getf record :position))))
                 (form (getf record :form)))
             (when shown
               (setf (gethash "file" table) (sanitize-for-json shown)))
             (when line
               (setf (gethash "line" table) line))
             (when form
               (setf (gethash "form" table) (sanitize-for-json form)))))
         table))
     records)))

(defun %warning-details (tables)
  "Return the messages of TABLES one per line: the text load-system reported
before it reported records, kept for the clients that read it."
  (format nil "~{~A~%~}"
          (mapcar (lambda (table) (gethash "message" table)) tables)))

(defun %call-with-suppressed-output (thunk)
  "Call THUNK with compilation and load output suppressed.
Returns (values thunk-result warning-count warning-details compiler-stderr
warning-records).  When THUNK unwinds, the stderr string and the records are
saved to *LAST-COMPILER-STDERR* and *LAST-WARNING-RECORDS* instead, so a caller
that catches the error can still report them.

Every warning is recorded (%MAKE-WARNING-RECORD) but two: a redefinition notice
(%REDEFINITION-WARNING-P), dropped on a first load and a reload alike because
redefining is ordinary Common Lisp development and a reload exists to do it,
and a warning signalled a second time when the compiled file loads
(%REPEATED-AT-LOAD-P).

What is muffled is another matter, settled by what muffling does to the build.
A STYLE-WARNING cannot fail a compile, so it is muffled once it is recorded:
left alone it would be printed, and ASDF would add a warning of its own about
the file.  A full WARNING is never muffled.  COMPILE-FILE offers a warning to
outer handlers before it counts it, and one muffled there is not counted: the
file compiles \"cleanly\", ASDF loads it, and the FASL left behind makes every
later load agree -- where ASDF by itself, and run-tests, refuse the file."
  (let ((records '())
        (stderr (make-string-output-stream)))
    ;; Reset before each call so stale data from a previous run is not
    ;; mistakenly attributed to this invocation.
    (setf *last-compiler-stderr* nil
          *last-warning-records* nil)
    (flet ((handle-warning (w)
             (let ((redefinition-p (%redefinition-warning-p w)))
               (unless redefinition-p
                 (let ((record (%make-warning-record w)))
                   (unless (%repeated-at-load-p record records)
                     (push record records))))
               (when (and (or redefinition-p (typep w 'style-warning))
                          (find-restart 'muffle-warning))
                 (invoke-restart 'muffle-warning))))
           (finished-records ()
             (%warning-record-tables (reverse records))))
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
                (let ((tables (finished-records)))
                  (values result
                          (length tables)
                          (%warning-details tables)
                          (get-output-stream-string stderr)
                          tables)))
            ;; Always capture stderr and the records so they survive an error
            ;; unwind.
            (unless completed-p
              (setf *last-compiler-stderr*
                    (ignore-errors (get-output-stream-string stderr))
                    *last-warning-records*
                    (ignore-errors (finished-records)))))))
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
              (let ((tables (finished-records)))
                (values result
                        (length tables)
                        (%warning-details tables)
                        (get-output-stream-string stderr)
                        tables)))
          ;; Always capture stderr and the records so they survive an error
          ;; unwind.
          (unless completed-p
            (setf *last-compiler-stderr*
                  (ignore-errors (get-output-stream-string stderr))
                  *last-warning-records*
                  (ignore-errors (finished-records)))))))))

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
                                       (:timeout-seconds (or null (real (0)))))
                          (values hash-table &rest t))
                load-system))

(defun load-system
       (system-name &key (force t) (clear-fasls nil) (timeout-seconds 120))
  "Load ASDF system SYSTEM-NAME with structured result.

When FORCE is true (default), clears loaded state before loading so
changed files are picked up -- a file written in the same second as its fasl
too, whose fasl is deleted first (DELETE-SAME-SECOND-FASLS), since ASDF's
one-second timestamps would keep it. When CLEAR-FASLS is true, deletes the
system's cached fasls (its output-translation directory) before
loading, guaranteeing recompilation from source — including
package-inferred dependency subsystems that :FORCE T alone would not
rebuild. TIMEOUT-SECONDS must be a positive number
or NIL (no timeout). Default is 120 seconds.

Warnings are recorded with their class, severity and place and reported as
\"warning_records\" (%CALL-WITH-SUPPRESSED-OUTPUT).  SBCL's 'redefining X in
DEFUN' notifications are dropped, on a first load and a reload alike, whatever
FORCE is and wherever the old definition came from.  A full WARNING signalled
while a file compiles fails the load, as it does under ASDF itself: the result
is an \"error\" that carries the records made up to then.

If ASDF signals MISSING-COMPONENT for the requested system, searches
*project-root* for a matching .asd file and retries once after
registering it."
  (check-type system-name string)
  (check-type timeout-seconds (or null (real (0))))
  (let ((system-name (string-downcase system-name))
        (start-time (get-internal-real-time))
        ;; Set by the load thread; read after it has been joined.
        (fasls-deleted nil)
        (fasls-cleared-from nil)
        (same-second-deleted nil))
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
                    ;; A file edited in the second its fasl was written looks
                    ;; current to ASDF, so a reload would run the code from
                    ;; before the edit; that fasl goes.  Before the clearing
                    ;; below, which unregisters what this reads, as run-tests
                    ;; does.  clear_fasls has removed them all already.
                    (when (and force (not clear-fasls))
                      (let ((stale (ignore-errors
                                    (delete-same-second-fasls system-name))))
                        (when (and stale (plusp stale))
                          (setf same-second-deleted
                                (+ (or same-second-deleted 0) stale))
                          (log-event :info "load-system-same-second-fasls"
                                     "system" system-name "deleted" stale))))
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
                       (asdf:load-system system-name :force clear-fasls)))))
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
             ;; For the builder as well, and consumed by it: the compiler's
             ;; verdict is what stopped the load.  With a full warning among
             ;; the records, that warning is the thing to fix.
             (when (typep err 'uiop:compile-file-error)
               (setf (gethash "compile_failed" ht) t))
             ;; What had been recorded when the error struck: for a compile
             ;; that failed on a warning, the cause itself.
             (let ((records *last-warning-records*))
               (when records
                 (setf (gethash "warnings" ht) (length records)
                       (gethash "warning_details" ht)
                       (sanitize-for-json (%warning-details records))
                       (gethash "warning_records" ht) (coerce records 'vector))))
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
                &optional compiler-stderr warning-records)
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
             (when warning-records
               (setf (gethash "warning_records" ht)
                     (coerce warning-records 'vector)))
             (log-event :info "load-system-complete" "system" system-name
                        "duration_ms" elapsed-ms "warnings" warning-count))))
        (when *auto-discovered-asd*
          (setf (gethash "auto_discovered_asd" ht) *auto-discovered-asd*))
        ;; What clear_fasls did, not only that it was asked: a request that
        ;; deleted nothing forced no recompilation, and the caller must be
        ;; able to see that.
        ;; Fasls a same-second edit made look current, deleted: the files this
        ;; load reached among them were compiled from source.
        (when same-second-deleted
          (setf (gethash "same_second_fasls_deleted" ht) same-second-deleted))
        (when (and clear-fasls fasls-deleted)
          (setf (gethash "fasls_deleted" ht) fasls-deleted)
          (when fasls-cleared-from
            (setf (gethash "fasls_cleared_from" ht) fasls-cleared-from)))
        ht))))
