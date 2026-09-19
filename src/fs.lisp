;;;; src/fs.lisp

(defpackage #:cl-mcp/src/fs
  (:use #:cl)
  (:import-from #:cl-mcp/src/log #:log-event)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*
                #:*project-root-lock*)
  (:import-from #:bordeaux-threads
                #:with-lock-held
                #:make-lock
                #:make-recursive-lock
                #:with-recursive-lock-held)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result #:text-content #:rpc-error)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/utils/paths
                #:ensure-project-root
                #:allowed-read-path
                #:canonical-path
                #:ensure-write-path
                #:native-path-namestring
                #:broad-root-p)
  (:import-from #:cl-mcp/src/utils/system
                #:fd-count)
  (:import-from #:uiop
                #:ensure-directory-pathname
                #:getenv
                #:getcwd
                #:chdir
                #:subpathp
                #:merge-pathnames*
                #:directory
                #:directory-exists-p
                #:absolute-pathname-p)
  (:import-from #:cl-mcp/src/proxy
                #:*use-worker-pool*
                #:*current-session-id*)
  (:import-from #:cl-mcp/src/pool
                #:pool-worker-info
                #:send-root-to-session-worker)
  (:import-from #:uiop/utility #:string-prefix-p)
  (:import-from #:uiop/filesystem #:ensure-directories-exist)
  ;; No cycle: paren-diagnostics depends on parinfer and uiop only.
  (:import-from #:cl-mcp/src/paren-diagnostics
                #:diagnose-delimiters
                #:format-delimiter-diagnosis)
  (:export #:*lisp-file-unparseable-hook*
           #:*fs-read-max-bytes*
           #:with-file-lock
           #:fs-resolve-read-path
           #:fs-read-file
           #:fs-read-source-text
           #:fs-read-source-octets
           #:fs-window-start
           #:fs-write-file
           #:fs-list-directory
           #:fs-get-project-info
           #:fs-set-project-root))

(in-package #:cl-mcp/src/fs)

;; *project-root* is imported from cl-mcp/src/project-root and re-exported

(defparameter *hidden-prefixes* '("." ".git" ".hg" ".svn" ".cache" ".fasl"))
(defparameter *skip-extensions* '("fasl" "ufasl" "x86f" "cfasl"))
(defparameter *fs-read-max-bytes* 1048576
  "Maximum number of characters allowed for fs-read-file when LIMIT is provided.")

(defun %read-file-string (pn offset limit)
  "Read file PN honoring OFFSET and LIMIT (both may be NIL).
Returns (VALUES content-string truncated-p file-length).
TRUNCATED-P is T when the file was larger than the effective read cap.
FILE-LENGTH is the total size of the file (NIL if unknown)."
  (when (and offset (< offset 0))
    (error "offset must be non-negative"))
  (when (and limit (< limit 0))
    (error "limit must be non-negative"))
  (when (and limit (> limit *fs-read-max-bytes*))
    (error "limit ~D exceeds maximum ~D" limit *fs-read-max-bytes*))
  (with-open-file (in pn :direction :input :element-type 'character)
    (when offset (file-position in offset))
    (let* ((raw-len (ignore-errors (file-length in)))
           (available-octets (and raw-len (max 0 (- raw-len (or offset 0)))))
           (effective (or limit available-octets *fs-read-max-bytes*))
           (capped (min effective *fs-read-max-bytes*))
           (buf (make-string capped))
           (count (read-sequence buf in :end capped))
           (text (subseq buf 0 count))
           ;; FILE-LENGTH counts octets even on a character stream, so a
           ;; multibyte file can look bigger than the cap and still fit in
           ;; it: report truncation only when the buffer filled and input
           ;; really remains.
           ;; Whether input remains past what was read: the peek decodes one
           ;; character past the buffer; if that byte is not decodable the
           ;; file simply continues. Reported separately from TRUNCATED so a
           ;; caller with a LIMIT can tell a window from a whole file without
           ;; comparing characters to octets.
           (remaining (and (= count capped)
                           (handler-case
                               (not (eq (peek-char nil in nil :eof) :eof))
                             (error () t))))
           ;; Without a known file length (FILE-LENGTH failed) an uncapped
           ;; read that filled the buffer with input remaining is still a
           ;; truncated read.
           (truncated (if raw-len
                          (and (> effective capped) remaining)
                          (and (null limit) remaining))))
      (values text truncated raw-len remaining))))

(defun %read-file-octets (pn)
  "Read the whole file PN as a fresh vector of (UNSIGNED-BYTE 8).

The stream is opened with an octet element type, so the result is the file's
exact bytes: nothing is decoded and no read cap applies.  FS-READ-SOURCE-OCTETS
is the caller-facing entry; this helper does no policy check of its own."
  (with-open-file (in pn :direction :input :element-type '(unsigned-byte 8))
    (let* ((size (or (file-length in) 0))
           (buffer (make-array size :element-type '(unsigned-byte 8)))
           (count (read-sequence buffer in)))
      (if (= count size) buffer (subseq buffer 0 count)))))

(defun fs-resolve-read-path (path)
  "Return a canonical pathname for PATH when it is readable per policy.
Signals an error when PATH is outside the allow-list."
  (let ((pn (allowed-read-path path)))
    (unless pn
      (error "Read not permitted for path ~A" path))
    pn))

(defun fs-read-file (path &key offset limit)
  "Read text file PATH with optional OFFSET and LIMIT.
Returns (VALUES content-string truncated-p file-length remaining-p):
TRUNCATED-P is T when the read was cut at the read cap, FILE-LENGTH is the
file's size in octets, and REMAINING-P is T when input remains past what was
read (so a LIMIT read can be told apart from a whole file)."
  (when (and offset (not (integerp offset)))
    (error "offset must be an integer"))
  (when (and limit (not (integerp limit)))
    (error "limit must be an integer"))
  (let ((pn (allowed-read-path path)))
    (unless pn
      (error "Read not permitted for path ~A" path))
    (log-event :debug "fs.read.open"
               "path" (namestring pn)
               "offset" offset
               "limit" limit
               "fd" (fd-count))
    (multiple-value-bind (text truncated file-length remaining)
        (%read-file-string pn offset limit)
      (log-event :debug "fs.read.close"
                 "path" (namestring pn)
                 "fd" (fd-count))
      (values text truncated file-length remaining))))

(defun fs-read-source-text (path)
  "Return the whole text of the file PATH, decoded as UTF-8 with every invalid
byte replaced by #\\?.

Use this instead of FS-READ-FILE for source files that tooling scans whole,
such as code-find-references' source scan: FS-READ-FILE stops at
*FS-READ-MAX-BYTES*, silently cutting a large file short, and decodes without
replacement, so one stray invalid byte (in a comment, say) would make the
whole file unreadable.  The read policy is FS-READ-FILE's: an error is
signalled when ALLOWED-READ-PATH does not permit PATH.  A file that cannot be
opened or read signals as well."
  (let ((pn (allowed-read-path path)))
    (unless pn
      (error "Read not permitted for path ~A" path))
    (log-event :debug "fs.read-source.open"
               "path" (namestring pn)
               "fd" (fd-count))
    (unwind-protect
         (uiop:read-file-string pn :external-format '(:utf-8 :replacement #\?))
      (log-event :debug "fs.read-source.close"
                 "path" (namestring pn)
                 "fd" (fd-count)))))

(defun fs-read-source-octets (path)
  "Return the whole file at PATH as a fresh vector of (UNSIGNED-BYTE 8).

The octet counterpart of FS-READ-SOURCE-TEXT, for a caller that needs the
file's exact bytes rather than decoded text: CL-MCP/SRC/SOURCE-SNAPSHOT digests
them for an edit guard, and a digest taken over re-encoded text would not
describe the bytes on disk.  Like FS-READ-SOURCE-TEXT the whole file is read
(*FS-READ-MAX-BYTES* caps FS-READ-FILE only) under FS-READ-FILE's read policy:
an error is signalled when ALLOWED-READ-PATH does not permit PATH, and the file
is then never opened.  A file that cannot be opened or read signals as well."
  (let ((pn (allowed-read-path path)))
    (unless pn
      (error "Read not permitted for path ~A" path))
    (log-event :debug "fs.read-source-octets.open"
               "path" (namestring pn)
               "fd" (fd-count))
    (unwind-protect
         (%read-file-octets pn)
      (log-event :debug "fs.read-source-octets.close"
                 "path" (namestring pn)
                 "fd" (fd-count)))))

(defun fs-window-start (path offset)
  "Return two values for the window of PATH that FS-READ-FILE opens at OFFSET:
the number of newlines before the window and the number of characters between
the last of those newlines (or the start of the file) and the window. A
failure reported at window line L, column C is therefore at file line
L + newlines and, on the first window line only, column C + that count.
The prefix is read one character at a time up to the same FILE-POSITION
%READ-FILE-STRING seeks to, so the count stops exactly where the window starts
even in a multibyte file, and no buffer is built, so *FS-READ-MAX-BYTES* does
not apply. PATH is checked against the read policy like FS-READ-FILE.
Returns (VALUES 0 0) for a NIL or zero OFFSET."
  (when (and offset (not (integerp offset)))
    (error "offset must be an integer"))
  (when (and offset (< offset 0))
    (error "offset must be non-negative"))
  (if (or (null offset) (zerop offset))
      (values 0 0)
      (let ((pn (allowed-read-path path)))
        (unless pn
          (error "Read not permitted for path ~A" path))
        (with-open-file (in pn :direction :input :element-type 'character)
          (let ((lines 0)
                (col 0))
            (loop for ch = (and (< (file-position in) offset)
                                (read-char in nil nil))
                  while ch
                  do (if (char= ch #\Newline)
                         (setf lines (1+ lines)
                               col 0)
                         (incf col)))
            (values lines col))))))

(defvar *file-lock-table* (make-hash-table :test #'equal)
  "Maps a file's lock key (FILE-LOCK-KEY) to the recursive lock that serialises
cl-mcp's own writes to that file. Read and written only under
*FILE-LOCK-TABLE-LOCK*.

Entries are never removed. An entry is one small lock object, and a key is one
distinct path this process has been asked to write: the three callers
(FS-WRITE-FILE, LISP-EDIT-FORM, LISP-PATCH-FORM) each resolve their argument to
a path under the project root before taking the lock. The bound is therefore
the number of distinct paths written over this image's lifetime, which is not
the same as the number of files the project has: PROJECT-SCAFFOLD's
%WRITE-FILES-TO-TEMP writes every generated file through FS-WRITE-FILE into a
fresh .tmp-project-scaffold-<random>/ directory, so each scaffold call leaves
one key per file behind permanently, keyed on a path renamed away moments
later. Reclaiming an entry would also have to prove that no thread is about to
take the lock being dropped, and getting that wrong hands two threads two
different locks for one file, which is exactly the bug the table exists to
prevent.")

(defvar *file-lock-table-lock* (make-lock "cl-mcp-file-lock-table")
  "Guards *FILE-LOCK-TABLE*. Held only around the table lookup and insert in
FILE-LOCK, never while a file is read or written.")

(defun file-lock-key (path)
  "Return the string that identifies PATH in *FILE-LOCK-TABLE*.

PATH is made absolute against *PROJECT-ROOT* with CANONICAL-PATH and then
resolved with TRUENAME -- the same two steps ALLOWED-READ-PATH and
ENSURE-WRITE-PATH already take, and therefore the same resolution
%NORMALIZE-PATHS gets for an edit. Two spellings of the same EXISTING file
(relative and absolute, through a symlink, or with a .. component) collapse to
one key and so take one lock.

A file that does not exist yet has no TRUENAME and keys on its unresolved
absolute namestring instead, so two acquisitions for one path can key
differently: once the file exists every caller resolves it the same way, but
a caller that took the unresolved key before it existed holds a different lock
from one arriving after. That is what WITH-FILE-LOCK's nesting note means by
the outer and inner keys not always agreeing; it costs mutual exclusion over
that one span and cannot deadlock.

Signals when *PROJECT-ROOT* is unset, as every write path already does."
  (let* ((abs (canonical-path path))
         (resolved (or (handler-case (truename abs) (file-error () nil)) abs)))
    (namestring resolved)))

(defun file-lock (path)
  "Return the recursive lock that serialises cl-mcp's writes to PATH, creating
it on first use. The lock is per file, keyed by FILE-LOCK-KEY.

Internal: the symbol is not exported, and mallet forbids the :: that would let
production code in another package name it, so WITH-FILE-LOCK -- which expands
into a call to this -- is the entry point everywhere outside this file. Only
CL-MCP/TESTS/FS-TEST reaches it directly, through an :IMPORT-FROM that needs no
export, to check the keying."
  (let ((key (file-lock-key path)))
    (with-lock-held (*file-lock-table-lock*)
      (or (gethash key *file-lock-table*)
          (setf (gethash key *file-lock-table*)
                (make-recursive-lock key))))))

(defmacro with-file-lock ((path) &body body)
  "Evaluate BODY holding the per-file lock for PATH, so that cl-mcp's own
read-verify-write sequences on one file cannot interleave and silently lose
each other's changes. PATH is evaluated once; every spelling of the same
existing file takes the same lock (FILE-LOCK-KEY).

The lock is recursive, so an outer holder nests with an inner one that keys
the same way: LISP-EDIT-FORM and LISP-PATCH-FORM hold it from before they read
the file until after they write it, and FS-WRITE-FILE takes it again
underneath. The two keys agree for every file that already exists. For a file
that does not (FILE-LOCK-KEY keys it on its unresolved absolute path), they
CAN differ -- if something outside these three tools creates the file between
the outer and the inner acquisition, TRUENAME then resolves and the inner one
takes a different lock, leaving the inner span outside the outer one's mutual
exclusion. That loses serialisation for that span, not safety: these locks are
only ever taken outer then inner, so no opposing order exists and nesting
cannot deadlock on it.

DEADLOCK DISCIPLINE -- while this lock is held, take no other cl-mcp lock
except CL-MCP/SRC/LOG's *LOG-LOCK*, and *FILE-LOCK-TABLE-LOCK* itself through
a nested WITH-FILE-LOCK's own call to FILE-LOCK (as FS-WRITE-FILE's does
underneath LISP-EDIT-FORM's, above): FILE-LOCK holds *FILE-LOCK-TABLE-LOCK*
only for one gethash/setf and always releases it before
WITH-RECURSIVE-LOCK-HELD can block, so it is never the far side of a wait on
a per-file lock and nesting cannot deadlock on it. Also read *PROJECT-ROOT*
rather than setting it, and never make a worker RPC
(CL-MCP/SRC/PROXY:PROXY-TO-WORKER) or any other call that blocks on another
process or on a reply. The three tools that hold it today run inline in the
parent and call no worker.

What it does NOT provide: the lock lives in this image and only the three
tools above take it, so those three writers are all it orders. A write made
from the worker process -- evaluation under REPL-EVAL, and whatever RUN-TESTS
and LOAD-SYSTEM write -- takes no lock at all and is not in this image anyway,
and a second cl-mcp server over the same checkout is coordinated no more than
an external editor is: its writes take their own, unrelated lock table. Within
this image, PROJECT-SCAFFOLD renames a whole prepared subtree into place
without taking these locks, so it can move a directory out from under a holder.
Nor is the lock a transaction or a crash-safety mechanism."
  (let ((lock (gensym "FILE-LOCK")))
    `(let ((,lock (file-lock ,path)))
       (with-recursive-lock-held (,lock)
         ,@body))))

(defvar *temp-name-serial* 0
  "Counter behind %NEXT-TEMP-SERIAL. Only that function reads or writes it,
and only under *TEMP-NAME-LOCK*.")

(defvar *temp-name-lock* (make-lock "cl-mcp-temp-name")
  "Guards *TEMP-NAME-SERIAL*. Independent of *FILE-LOCK-TABLE-LOCK* and of any
per-file lock, and held for one INCF only.")

(defun %next-temp-serial ()
  "Return a fresh integer, distinct for every call in this process.
Used to make a temp file name unique per write."
  (with-lock-held (*temp-name-lock*)
    (incf *temp-name-serial*)))

(defun %temp-pathname-for (pn)
  "Return the pathname %WRITE-STRING-TO-FILE writes before renaming it onto PN.

The name is \".<name>.<type>.<pid>.<serial>\" with the type \"tmp\", so it ends
in .tmp rather than in PN's own extension: a leftover temp beside a .lisp file
is not itself a .lisp file, and the tools that scan Lisp sources by extension
(clgrep-search, code-find-references' source scan) skip it. The leading dot
hides it from directory listings as the old fixed name did, and it stays in
PN's own directory so the rename remains a same-filesystem rename.

The process id and the per-process serial make the name unique to one call, so
two writers -- two threads, or two cl-mcp processes over the same checkout --
can never share one temp file, interleave their content into it, or delete one
out from under the other's RENAME-FILE.

What a crash leaves behind: only the call that created a temp ever deletes it,
so a process killed between the open and the rename leaves that one file on
disk, and nothing later cleans it up. It is inert -- a hidden .tmp file that no
cl-mcp tool reads -- but it does accumulate one file per hard crash, and each
has a different name, so they are removed by hand (or by the build's own
cleanup), not overwritten by the next write."
  (let ((name (pathname-name pn))
        (type (pathname-type pn)))
    (make-pathname :name (format nil ".~A~@[.~A~].~D.~D"
                                 (if (stringp name) name "file")
                                 (and (stringp type) type)
                                 (sb-posix:getpid)
                                 (%next-temp-serial))
                   :type "tmp"
                   :defaults pn)))

(defun %write-string-to-file (pn content)
  "Write CONTENT to PN atomically via write-to-temp-then-rename.
On failure the original file is preserved.

The temp file is unique to this call (%TEMP-PATHNAME-FOR) and the cleanup
deletes only that file, so concurrent writers in the same directory cannot
corrupt or delete each other's temp. The rename makes each write all-or-
nothing on its own; it does not order two writes. A caller that must not lose
another writer's change takes CL-MCP/SRC/FS:WITH-FILE-LOCK around its whole
read-modify-write, as FS-WRITE-FILE does."
  (ensure-directories-exist pn)
  (let ((tmp (%temp-pathname-for pn)))
    (unwind-protect
         (progn
           (with-open-file (out tmp
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :element-type 'character)
             (write-string content out)
             (finish-output out))
           (rename-file tmp pn)
           t)
      ;; Clean up this call's temp file on failure
      (when (probe-file tmp)
        (handler-case (delete-file tmp) (file-error () nil))))))

(defun fs-write-file (path content)
  "Write CONTENT to PATH relative to project root.
Returns T on success.

The write is serialised against cl-mcp's other writes to the same file by
WITH-FILE-LOCK. A caller that already holds that lock over a wider span --
LISP-EDIT-FORM and LISP-PATCH-FORM hold it from before they read the file --
nests here, since the lock is recursive. Only this process's writes are
ordered: a second cl-mcp server over the same checkout, or an external editor,
is not coordinated by it."
  (let ((pn (ensure-write-path path)))
    (with-file-lock (pn)
      (log-event :debug "fs.write.open"
                 "path" (namestring pn)
                 "bytes" (length content)
                 "fd" (fd-count))
      (unwind-protect
           (%write-string-to-file pn content)
        (log-event :debug "fs.write.close"
                   "path" (namestring pn)
                   "fd" (fd-count))))))

(defun %lisp-source-pathname-p (pn)
  "Return T when PN has a Common Lisp source extension."
  (let ((type (pathname-type pn)))
    (and type
         (member (string-downcase type)
                 '("lisp" "asd")
                 :test #'string=))))

(defvar *lisp-file-unparseable-hook* nil
  "Predicate of two arguments (an absolute pathname and the file's text, which
%LISP-FILE-UNPARSEABLE-P has read) that
returns T when the structural editing tools cannot parse that Lisp file in a
way no readtable can fix. The fs-write-file overwrite guard consults it so
that overwriting is permitted exactly when lisp-edit-form and
lisp-patch-form cannot locate any form in the file.
cl-mcp/src/lisp-edit-form-core installs a predicate built on its own parser
(which understands named-readtable declarations); this indirection exists
because fs cannot import that parser without a dependency cycle. When NIL
the guard always holds: there is no weaker fallback definition.")

(defun %lisp-file-unparseable-p (pn)
  "Return T when the structural editing tools cannot parse the Lisp source at
PN in a way no readtable can fix, so that overwriting it is the only repair
path. The verdict comes from *LISP-FILE-UNPARSEABLE-HOOK*, installed by
cl-mcp/src/lisp-edit-form-core at load time: the edit tools' own parser,
which handles named-readtable declarations and classifies failures by
condition type. Without a hook (a partial image that loaded fs alone) the
answer is NIL, i.e. the overwrite guard always holds -- there is no
second, weaker definition of \"unparseable\" to drift from the tools'.
A read truncated at *FS-READ-MAX-BYTES* is reported as parseable, since a
cut-off prefix proves nothing, and so is a file that cannot be decoded at
all (invalid UTF-8, say): failing closed keeps the guard in place instead of
turning the write into an internal error."
  (multiple-value-bind (text truncated)
      (handler-case (%read-file-string pn nil nil)
        (error () (values "" t)))
    (and (not truncated)
         *lisp-file-unparseable-hook*
         (funcall *lisp-file-unparseable-hook* pn text)
         t)))

(defun %existing-lisp-overwrite-error (id path allow-unparseable)
  "Return a structured RPC error for a forbidden Lisp overwrite, or NIL.
New Lisp source file creation is always allowed. An existing .lisp/.asd file
may be overwritten only when the caller passed ALLOW-UNPARSEABLE, i.e.
explicitly judged that the breakage is real rather than custom reader syntax
the structural tools could handle with a readtable, AND the file does not
parse (a missing or stray parenthesis, per %LISP-FILE-UNPARSEABLE-P). The
parse is attempted only when the caller opted in: without the flag the
answer is a refusal either way, so the common case pays nothing.
No heuristic can tell real breakage from custom syntax without knowing the
readtable, so the decision is the caller's; the guard only makes sure a
file that does parse is never rewritten wholesale."
  (let ((pn (ensure-write-path path)))
    (when (and (probe-file pn)
               (%lisp-source-pathname-p pn))
      (let ((unparseable (and allow-unparseable (%lisp-file-unparseable-p pn))))
        (unless unparseable
          (if allow-unparseable
              (rpc-error id -32602
                         (format nil "Cannot overwrite existing .lisp/.asd with fs-write-file: ~
the file does not fail on a missing or stray parenthesis (it parses, or fails for ~
a reader-level reason such as an unknown reader macro), so allow_unparseable_overwrite ~
does not apply; use lisp-edit-form (with the readtable parameter if the file uses ~
custom reader syntax -- that is refused only while the readtable the file needs can ~
still be resolved in this process; a file whose own (in-readtable ...) names one this ~
process does not have is overwritable here instead, since nothing could parse it).")
                         (make-ht "code" "existing_lisp_overwrite_forbidden"
                                  "path" path
                                  "next_tool" "lisp-edit-form"
                                  "required_args"
                                  (vector "file_path" "form_type" "form_name"
                                          "operation" "content")
                                  "new_file_creation_allowed" t))
              ;; The plain refusal keeps its historical wording; the opt-in
              ;; is advertised through the data field.
              (rpc-error id -32602
                         (format nil "Cannot overwrite existing .lisp/.asd with ~
fs-write-file; use lisp-edit-form.")
                         (make-ht "code" "existing_lisp_overwrite_forbidden"
                                  "path" path
                                  "next_tool" "lisp-edit-form"
                                  "required_args"
                                  (vector "file_path" "form_type" "form_name"
                                          "operation" "content")
                                  "new_file_creation_allowed" t
                                  "allow_unparseable_overwrite_available" t))))))))

(defun %post-write-parse-warning (pn path content)
  "Return a warning for the caller of fs-write-file when CONTENT, just written
to the Lisp source file PN (PATH is its project-relative name), does not
parse; NIL otherwise, and NIL for non-Lisp files. The verdict is the one
*LISP-FILE-UNPARSEABLE-HOOK* gives, i.e. exactly the condition under which the
overwrite guard would let this file be rewritten: a delimiter failure no
readtable can fix. A reader-level failure (an unknown reader macro) gets no
warning, since the hook cannot tell it from custom syntax. Without a hook (a
partial image that loaded fs alone) there is no verdict and no warning. The
text carries the shared delimiter diagnosis -- or, should the reader fail
where the scan sees balance, a plain sentence -- and says that the next write
needs allow_unparseable_overwrite=true, because the file now exists and does
not parse, so the guard would otherwise refuse the very fix it asks for.

CONTENT longer than *FS-READ-MAX-BYTES* is the one case where that promise
would be false: the guard re-reads the file from disk on the next write and
treats a read cut at the cap as parseable, so it would refuse the repair. For
such content the text says to split the file or fix it outside cl-mcp instead,
as %LOCATE-TARGET-FORM does for files it cannot read whole.

This runs after the file is already on disk, so nothing here may turn a
successful write into an error: an error from the hook counts as no verdict
(no warning, as with no hook at all), and an error while diagnosing falls
back to the plain sentence."
  (when (and *lisp-file-unparseable-hook*
             (%lisp-source-pathname-p pn)
             (ignore-errors (funcall *lisp-file-unparseable-hook* pn content)))
    (format nil "WARNING: the file was written but does not parse.~%~A~%~A"
            (or (handler-case
                    (format-delimiter-diagnosis (diagnose-delimiters content)
                                                :target path)
                  (error () nil))
                (concatenate 'string
                             "The editing tools' reader cannot parse the file as "
                             "written; run lisp-check-parens for the position."))
            (if (> (length content) *fs-read-max-bytes*)
                (format nil "The file is also larger than the fs read cap (~D characters), ~
                             so neither lisp-edit-form nor fs-write-file's overwrite path ~
                             (allow_unparseable_overwrite) can repair it: split the file ~
                             or fix it outside cl-mcp."
                        *fs-read-max-bytes*)
                (format nil "Fix it and write it again with fs-write-file (path=~S, ~
                             allow_unparseable_overwrite=true; the file now exists and ~
                             does not parse, so the overwrite guard requires the flag)."
                        path)))))

(defun %entry-name (path)
  "Return display name for PATH, trimming trailing slash on directories."
  (let* ((namestr (file-namestring path))
         (trimmed (and namestr (string-right-trim "/" namestr))))
    (if (and trimmed (plusp (length trimmed)))
        trimmed
        (let* ((dir (pathname-directory path))
               (leaf (car (last dir))))
          (and leaf (string leaf))))))

(defun %should-skip-entry-p (path &key show-hidden)
  "Return T when PATH should be omitted from a directory listing.
Build artifacts (fasl and related extensions) are always filtered.
Dotfiles and other entries matching *HIDDEN-PREFIXES* are filtered
unless SHOW-HIDDEN is non-nil."
  (let ((name (%entry-name path)) (type (pathname-type path)))
    (or (null name)
        (and (not show-hidden)
             (some (lambda (pref) (string-prefix-p pref name))
                   *hidden-prefixes*))
        (and type
             (member (string-downcase type) *skip-extensions* :test
                     #'string=)))))

(defun fs-list-directory (path &key show-hidden)
  "List directory entries at PATH respecting read allow-list.
Returns a vector of hash-tables with keys \"name\" and \"type\" (file|directory).
When SHOW-HIDDEN is nil (default), dotfiles and entries matching
*HIDDEN-PREFIXES* are omitted. When SHOW-HIDDEN is non-nil, those are
included, but build artifacts (fasl family) remain filtered so that
listings stay useful."
  (let ((pn (allowed-read-path path)))
    (unless pn (error "Read not permitted for path ~A" path))
    (unless (directory-exists-p pn)
      (error "Directory ~A (resolved to ~A) does not exist or is not readable"
             path (native-path-namestring pn)))
    (let* ((patterns (list #P"*" #P"*.*"))
           (entries
            (loop for pat in patterns
                  append (directory (merge-pathnames* pat pn))))
           (seen (make-hash-table :test #'equal))
           (results 'nil))
      (dolist (p entries)
        (unless (%should-skip-entry-p p :show-hidden show-hidden)
          (let ((key (namestring p)))
            (unless (gethash key seen)
              (setf (gethash key seen) t)
              (let ((h (make-hash-table :test #'equal)) (name (%entry-name p)))
                (setf (gethash "name" h) name
                      (gethash "type" h)
                        (if (uiop/pathname:directory-pathname-p p)
                            "directory"
                            "file"))
                (push h results))))))
      (coerce (nreverse results) 'vector))))

(defun fs-get-project-info ()
  "Return project root and working directory information.
Returns a hash-table with keys:
  - project_root: absolute path to project root
  - cwd: current working directory
  - project_root_source: how project root was determined (env|cwd|asdf)
  - relative_cwd: cwd relative to project_root (when inside project)"
  (ensure-project-root)
  (let ((cwd (ignore-errors (uiop:getcwd)))
        (env-root (uiop:getenv "MCP_PROJECT_ROOT"))
        (h (make-hash-table :test #'equal)))
    (let ((root-source (if env-root "env" "explicit")))
      (setf (gethash "project_root" h) (native-path-namestring *project-root*)
            (gethash "cwd" h) (native-path-namestring cwd)
            (gethash "project_root_source" h) root-source)
      (let ((root (uiop:ensure-directory-pathname *project-root*)))
        (when (and cwd (uiop:subpathp cwd root))
          (setf (gethash "relative_cwd" h)
                (uiop:native-namestring (uiop:enough-pathname cwd root)))))
      (when *use-worker-pool*
        (setf (gethash "workers" h) (pool-worker-info)))
      h)))

(defun fs-set-project-root (path)
  "Set the project root to PATH and change the current working directory.
Returns a hash-table with updated path information:
  - project_root: the new absolute project root path
  - cwd: the new current working directory
  - previous_root: the previous project root path (or (not set) if was nil)
  - status: confirmation message"
  (unless (stringp path) (error "path must be a string"))
  (when (string= (string-trim '(#\Space #\Tab) path) "")
    (error "path must not be empty"))
  (let* ((prev-root *project-root*)
         ;; PARSE-UNIX-NAMESTRING, not ENSURE-DIRECTORY-PATHNAME: the latter
         ;; hands a string to the CL pathname reader, which reads [ and ] as
         ;; wildcard syntax, so the very path this tool returns for a root
         ;; named project[old]/ came back as a wild pathname it then refused.
         ;; The wire protocol carries POSIX paths; parse them as such.
         (requested (uiop:parse-unix-namestring path :ensure-directory t))
         (base (ignore-errors (uiop/os:getcwd)))
         (temp-root
          (if (uiop/pathname:absolute-pathname-p requested)
              requested
              (uiop/pathname:merge-pathnames* requested base))))
    (unless (uiop/filesystem:directory-exists-p temp-root)
      (error "Directory ~A does not exist" path))
    ;; C2: Reject overly broad roots that would disable the security sandbox.
    (when (broad-root-p temp-root)
      (error "Refusing to set project root to ~A — too broad"
             (native-path-namestring temp-root)))
    (let ((new-root (truename temp-root)))
      ;; C3: Atomic multi-step mutation under lock
      (bt:with-lock-held (*project-root-lock*)
        (setf *project-root* new-root)
        (uiop/os:chdir new-root)
        (setf *default-pathname-defaults*
                (uiop/pathname:ensure-directory-pathname new-root)))
      (log-event :info "fs.set-project-root" "previous"
       (if prev-root
           (native-path-namestring prev-root)
           "(not set)")
       "new" (native-path-namestring new-root))
      (when *use-worker-pool*
        (ignore-errors
         (send-root-to-session-worker *current-session-id* new-root)))
      (let ((h (make-hash-table :test #'equal)))
        (setf (gethash "project_root" h) (native-path-namestring new-root)
              (gethash "cwd" h) (native-path-namestring (uiop/os:getcwd))
              (gethash "previous_root" h)
                (if prev-root
                    (native-path-namestring prev-root)
                    "(not set)")
              (gethash "status" h)
                (format nil "Project root set to ~A"
                        (native-path-namestring new-root)))
        h))))

(define-tool "fs-read-file"
  :description "Read a text file with optional offset and limit.
Prefer absolute paths inside the project; offset/limit are character counts
to avoid loading whole files.
It can only open files in the project or in loaded dependent libraries.

For .lisp and .asd files, prefer 'lisp-read-file' instead - it provides
collapsed signatures view that saves ~70% of context window tokens."
  :args ((path :type :string :required t
               :description "Absolute path inside the project or a registered ASDF system")
         (offset :type :integer
                 :description "0-based character offset to start reading")
         (limit :type :integer
                :description "Maximum characters to return; omit to read to end"))
  :body
  (multiple-value-bind (content-string truncated file-length)
      (fs-read-file path :offset offset :limit limit)
    (let ((ht (make-ht "content" (text-content
                                  (if truncated
                                      (let ((next-offset (+ (or offset 0) (length content-string))))
                                        (format nil "~A~%~%[TRUNCATED: file is ~:D chars, showing ~:D from offset ~:D. Use offset=~D to read more.]"
                                                content-string file-length (length content-string) (or offset 0) next-offset))
                                      content-string))
                       "text" content-string
                       "path" path
                       "offset" offset
                       "limit" limit)))
      (when truncated
        (setf (gethash "truncated" ht) t
              (gethash "file_length" ht) file-length
              (gethash "read_length" ht) (length content-string)))
      (result id ht))))

(define-tool "fs-write-file"
  :description "Write text content to a file relative to project root.
Parent directories are automatically created if they do not exist.
Use this for creating NEW files or editing non-Lisp files (e.g., markdown, config files).
For editing EXISTING Lisp source code, you MUST use 'lisp-edit-form' instead
to preserve structure and comments. The one exception: when an existing .lisp
file no longer parses (a missing or stray parenthesis), lisp-edit-form cannot
locate any form in it, so overwriting it here is the repair path -- but only
with allow_unparseable_overwrite=true, because a file that only looks broken
to the default reader may be valid under a custom readtable.
After writing a .lisp/.asd file its content is checked with the parser the
overwrite guard uses: the write still succeeds, but if the file does not parse
the response says so, shows the diagnosis, and reminds you that the next write
to it needs allow_unparseable_overwrite=true."
  :args ((path :type :string :required t
               :description "Relative path under the project root; absolute paths are rejected")
         (content :type :string :required t
                  :description "Text content to write")
         (allow-unparseable-overwrite
          :type :boolean :default nil
          :description "Permit overwriting an existing .lisp/.asd file that does not parse
(missing or stray parenthesis). Pass true only when you know the file uses no custom
reader syntax; otherwise use lisp-edit-form with the readtable parameter. Never
overrides the guard for a file that parses."))
  :body
  ;; The overwrite decision reads and parses the file, and the post-write check
  ;; parses it again, so all three steps run under one WITH-FILE-LOCK: without
  ;; it a concurrent lisp-edit-form could land between the decision and the
  ;; write, making the verdict (the allow_unparseable_overwrite judgement
  ;; included) describe bytes this write then destroys. The key is the one
  ;; FS-WRITE-FILE itself computes, so its own acquisition nests here.
  (with-file-lock ((ensure-write-path path))
    (or (%existing-lisp-overwrite-error id path allow-unparseable-overwrite)
        (progn
          (fs-write-file path content)
          (let* ((warning (%post-write-parse-warning (ensure-write-path path) path content))
                 (payload (make-ht "success" t
                                   "content" (text-content
                                              (format nil "Wrote ~A (~D chars)~@[~%~A~]"
                                                      path (length content) warning))
                                   "path" path
                                   "bytes" (length content))))
            (when warning
              (setf (gethash "unparseable" payload) t))
            (result id payload))))))

(define-tool "fs-list-directory"
  :description "List entries in a directory, filtering hidden and build artifacts.
Use absolute paths inside the project or an ASDF system.

Dotfiles (names starting with '.' such as .gitignore) are omitted by
default. Pass show_hidden=true to include them. Build artifacts (fasl
family) are always filtered so listings stay useful."
  :args ((path :type :string :required t
               :description "Absolute directory path under the project root or a registered
ASDF system")
         (show-hidden :type :boolean :default nil
                      :description "Include dotfiles and entries that would normally be hidden."))
  :body
  (let ((entries (fs-list-directory path :show-hidden show-hidden)))
    (result id
            (make-ht "content" (text-content
                                (with-output-to-string (s)
                                  (format s "~D entries in ~A~%" (length entries) path)
                                  (loop for e across entries
                                      do (if (hash-table-p e)
                                             (format s "~A ~A~%"
                                                     (if (equal (gethash "type" e) "directory")
                                                         "[dir] " "[file]")
                                                     (gethash "name" e))
                                             (format s "~A~%" e)))))
                     "entries" entries
                     "path" path
                     "show_hidden" show-hidden))))

(define-tool "fs-get-project-info"
  :description "Get project root and current working directory information for
path resolution context."
  :args ()
  :body
  (let* ((info (fs-get-project-info))
         (workers (gethash "workers" info))
         (summary (format nil "Project root: ~A~%CWD: ~A~%Source: ~A~@[~%Workers: ~A active~]"
                          (gethash "project_root" info)
                          (or (gethash "cwd" info) "(none)")
                          (gethash "project_root_source" info)
                          (when (and workers (arrayp workers) (plusp (length workers)))
                            (length workers)))))
    (result id
            (make-ht "content" (text-content summary)
                     "project_root" (gethash "project_root" info)
                     "cwd" (gethash "cwd" info)
                     "project_root_source" (gethash "project_root_source" info)
                     "relative_cwd" (gethash "relative_cwd" info)
                     "workers" (gethash "workers" info)))))

(define-tool "fs-set-project-root"
  :description "Set the server's project root directory to the specified path.
Use this to synchronize the server's working directory with the client's
project location. The server will change its current working directory
to the specified path.
RESTRICTION: You MUST only provide your current working directory (e.g., obtained via pwd).
Do not use arbitrary paths."
  :args ((path :type :string :required t
               :description "Absolute path to the project root directory"))
  :body
  (let ((info (fs-set-project-root path)))
    (result id
            (make-ht "content" (text-content (gethash "status" info))
                     "info" info))))
