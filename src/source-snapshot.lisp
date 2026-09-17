;;;; src/source-snapshot.lisp
;;;;
;;;; Read a source file exactly once and digest it, so an edit guard's file
;;;; and form digests come from the same bytes a CST parse saw (design doc
;;;; 2026-09-16-clos-describe-fail-closed-design.md, section 4.1).

(defpackage #:cl-mcp/src/source-snapshot
  (:use #:cl)
  (:import-from #:cl-mcp/src/project-root
                #:*project-root*)
  (:import-from #:cl-mcp/src/utils/paths
                #:allowed-read-path)
  ;; No cycle: fs depends on paths, proxy, pool and paren-diagnostics, none of
  ;; which reach this file.  The parent-only consumers (lisp-edit-form-core and
  ;; the clos-describe response builders) already load fs.
  (:import-from #:cl-mcp/src/fs
                #:fs-read-source-octets)
  (:export #:read-source-snapshot
           #:snapshot-decode-lossy-p
           #:snapshot-range-digest
           #:digest-string-octets))

(in-package #:cl-mcp/src/source-snapshot)

(defun %ensure-sb-md5 ()
  "Load and return the SB-MD5 package when available, else NIL.

SB-MD5 ships with SBCL as a contrib and is loaded lazily here, the way
CL-MCP/SRC/CODE-CORE's %ENSURE-SB-INTROSPECT loads SB-INTROSPECT. A build
without it must not error: every digest in this file degrades to NIL so a
caller can treat the source as UNVERIFIED instead of crashing."
  (or (find-package :sb-md5)
      (ignore-errors
       (require :sb-md5)
       (find-package :sb-md5))))

(defun %hex-string (octets)
  "Return OCTETS, a vector of (UNSIGNED-BYTE 8), as a lower-case hex string."
  (with-output-to-string (out)
    (loop for byte across octets do (format out "~(~2,'0X~)" byte))))

(defun %md5-digest-of-octets (octets)
  "Return \"md5:<hex>\" for OCTETS, a vector of (UNSIGNED-BYTE 8), or NIL
when SB-MD5 cannot be loaded in this image."
  (let ((package (%ensure-sb-md5)))
    (when package
      (let ((md5sum-sequence (find-symbol "MD5SUM-SEQUENCE" package)))
        (when md5sum-sequence
          (concatenate 'string "md5:" (%hex-string (funcall md5sum-sequence octets))))))))

(defun digest-string-octets (string)
  "Return \"md5:<hex>\" for STRING encoded as UTF-8 octets, or NIL when
SB-MD5 cannot be loaded in this image.

This is what SNAPSHOT-RANGE-DIGEST calls on a substring of a snapshot's
already-decoded :TEXT. Re-encoding a substring to UTF-8 does not reproduce
the exact original file bytes when that substring contains a character that
replaced an invalid byte on decode (see READ-SOURCE-SNAPSHOT) -- an accepted
approximation, since an edit guard only needs to notice that a range's
content changed between two reads, not to reconstruct the disk bytes."
  (%md5-digest-of-octets (sb-ext:string-to-octets string :external-format :utf-8)))

(defun %readable-path (path)
  "Return PATH's resolved pathname when the read policy allows reading it,
else NIL.

The policy is FS-READ-FILE's: CL-MCP/SRC/UTILS/PATHS:ALLOWED-READ-PATH, the
same predicate FS-READ-SOURCE-TEXT and CODE-REFS-SCAN's %READABLE-PATH use.
NIL also when *PROJECT-ROOT* is unset or the check itself signals, so a path
the policy cannot vouch for is never opened.

FS-READ-SOURCE-OCTETS re-applies the same check when it opens the file; this
one runs first so READ-SOURCE-SNAPSHOT can report a refusal as :DENIED
instead of as an unreadable file."
  (and *project-root*
       (handler-case (allowed-read-path path)
         (error () nil))))

(defun %decode-utf-8-replacing (octets)
  "Decode OCTETS as UTF-8 text, replacing every invalid byte with #\\?, the
same decoding FS-READ-SOURCE-TEXT applies via UIOP:READ-FILE-STRING."
  (sb-ext:octets-to-string octets :external-format '(:utf-8 :replacement #\?)))

(defun %first-line (text)
  "Return TEXT's first line, for a one-line failure summary."
  (subseq text 0 (or (position #\Newline text) (length text))))

(defun read-source-snapshot (abs-path)
  "Read the file at ABS-PATH exactly once and return (VALUES SNAPSHOT
FAILURE).

SNAPSHOT is a plist (:ABS-PATH namestring :TEXT text :OCTET-COUNT n :DIGEST
digest) built from that single read: the raw octets are both hashed for
:DIGEST and decoded as UTF-8 (invalid bytes replaced by #\\?, matching
FS-READ-SOURCE-TEXT) for :TEXT, so :TEXT and :DIGEST always describe the
same bytes -- never mix a :TEXT from one read with a :DIGEST from another.
:DIGEST is NIL when SB-MD5 cannot be loaded in this image; treat that as
UNVERIFIED, never as a fabricated match.

The file is opened by CL-MCP/SRC/FS:FS-READ-SOURCE-OCTETS, so the read goes
through the fs layer and its policy (CL-MCP/SRC/UTILS/PATHS:ALLOWED-READ-PATH,
the same predicate behind FS-READ-FILE and CODE-REFS-SCAN's %READABLE-PATH);
%READABLE-PATH applies that policy here first so a refusal is reported as
:DENIED rather than as an unreadable file. FAILURE is NIL on
success, :DENIED when the policy refuses ABS-PATH -- the file is then never
opened -- and a one-line string when the file cannot be read for any other
reason (missing, permission, I/O error). SNAPSHOT is NIL whenever FAILURE is
non-NIL: a refused or unreadable file never yields a partial snapshot."
  (let ((readable (%readable-path abs-path)))
    (if (null readable)
        (values nil :denied)
        (handler-case
            (let* ((octets (fs-read-source-octets readable))
                   (text (%decode-utf-8-replacing octets)))
              (values (list :abs-path (namestring readable)
                            :text text
                            :octet-count (length octets)
                            :digest (%md5-digest-of-octets octets))
                      nil))
          (error (e) (values nil (%first-line (princ-to-string e))))))))

(defun snapshot-decode-lossy-p (snapshot)
  "True when SNAPSHOT's :TEXT is not a faithful decode of the bytes
READ-SOURCE-SNAPSHOT read: the file is not valid UTF-8, so %DECODE-UTF-8-
REPLACING turned at least one byte into #\\? and writing :TEXT back to disk
would destroy that byte.  A caller that edits through :TEXT -- lisp-edit-form's
guarded path -- must refuse such a file rather than rewrite it.

Decided from what the snapshot already carries, by re-encoding :TEXT as UTF-8:
the octet count first, then the MD5 digest of the original bytes, which is what
catches a single invalid byte (#xE9 becomes #\\?, one octet either way).  With
SB-MD5 unavailable :DIGEST is NIL and only the count can be compared; a
same-length replacement then goes unnoticed here, but a guard cannot be
verified without a digest anyway (CHECK-EDIT-GUARD's check 3 fails first)."
  (let* ((text (getf snapshot :text))
         (octets (sb-ext:string-to-octets text :external-format :utf-8))
         (digest (getf snapshot :digest)))
    (or (not (eql (length octets) (getf snapshot :octet-count)))
        (and digest (not (equal digest (%md5-digest-of-octets octets)))))))

(defun snapshot-range-digest (snapshot start end)
  "Return \"md5:<hex>\" for the UTF-8 octets of (SUBSEQ text START END),
where text is SNAPSHOT's :TEXT (a plist from READ-SOURCE-SNAPSHOT), or NIL
when SB-MD5 is unavailable.

START and END are character offsets into text, 0-based, END exclusive -- the
same convention CST-NODE-START/CST-NODE-END use. SUBSEQ signals when START
or END falls outside text or END is less than START, so a caller with a bad
range finds out immediately rather than silently hashing the wrong slice."
  (digest-string-octets (subseq (getf snapshot :text) start end)))
