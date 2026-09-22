;;;; specs/write-paths.lisp
;;;;
;;;; Properties of the write boundary: CL-MCP/SRC/UTILS/PATHS:ENSURE-WRITE-PATH,
;;;; which decides where a write may land, and CL-MCP/SRC/FS:FS-WRITE-FILE,
;;;; which writes there.
;;;;
;;;; The policy they are checked against, stated on descriptors in
;;;; specs/write-fixtures.lisp (EXPECTED-WRITE-DECISION) and never computed with
;;;; the functions under test or their helpers:
;;;;
;;;;   an absolute argument                                   -> refused
;;;;   a target whose real location is outside the project    -> refused,
;;;;                                                             registered
;;;;                                                             dependency or not
;;;;   a .. right after a link or a name not created yet      -> refused
;;;;   anything else, existing or not yet                     -> allowed, as the
;;;;                                                             real path
;;;;
;;;; A check lists the whole scratch tree, without following links, just before
;;;; and just after its one call, and judges from the two listings while the
;;;; tree still exists: the validator must change nothing, a refused write must
;;;; change nothing, and an allowed write must add exactly the expected new
;;;; directories and file, or change exactly the existing file, and nothing
;;;; else.  Only then does the fixture remove the tree, adopting whatever was
;;;; written into it.
;;;;
;;;; Verified domain: a scratch tree under the temporary directory; targets in
;;;; project/, the dependency (with its fixture ASDF system registered or not),
;;;; outside/ and project-other/; zero to two existing directories, then zero to
;;;; two new ones, then a new or existing file, named with plain, spaced,
;;;; Japanese, dotted and bracketed parts, with or without a type; reached
;;;; directly or through one symlink at the root of project/ or outside/, to the
;;;; target's directory or to an existing target file; spelled relative, as a
;;;; relative pathname, with a ./ and D/../D detour, or absolute; with the
;;;; project root given directly or as a symlink alias.
;;;;
;;;; WRITE-PRESERVES-SAFE-SPELLINGS is a relation on top of that oracle: for one
;;;; allowed case it calls ENSURE-WRITE-PATH with the base relative string, the
;;;; same string parsed into a relative pathname, ./ in front of it, and one
;;;; separator doubled, and demands the same real path from all four.  The base
;;;; is first checked against EXPECTED-WRITE-NATIVE, so refusing everything, or
;;;; sending every spelling to one wrong file, does not pass as agreement.  It
;;;; adds no rule about what is allowed.
;;;;
;;;; Fixed cases only (tests/write-path-specs-test.lisp): dangling links, a file
;;;; as an ancestor, no file name, an unset or unresolvable root.  Not covered:
;;;; races with the filesystem (TOCTOU), permissions, ACLs, hard links, mount
;;;; namespaces, link chains and loops, Windows paths, and what the MCP tools do
;;;; around FS-WRITE-FILE.

(defpackage #:cl-mcp/specs/write-paths
  (:use #:cl)
  (:import-from #:cl-spec/main
                #:defspec
                #:defproperty
                #:defgenerator)
  (:import-from #:cl-mcp/src/utils/paths
                #:ensure-write-path)
  (:import-from #:cl-mcp/src/fs
                #:fs-write-file)
  (:import-from #:cl-mcp/specs/path-fixtures
                #:register-dependency
                #:unregister-dependency)
  (:import-from #:cl-mcp/specs/write-fixtures
                #:draw-project-write-case
                #:draw-refused-write-case
                #:draw-link-write-case
                #:draw-write-case
                #:write-case
                #:with-write-fixture
                #:expected-write-decision
                #:validator-agrees-p
                #:writer-agrees-p
                #:expected-write-native
                #:no-changes-p
                #:draw-safe-spelling-case
                #:safe-spelling-variants
                #:observe-spellings
                #:safe-spelling-coverage-p)
  (:export #:register-specifications
           #:contract-names
           #:property-names
           #:spec-names
           #:generator-names
           #:call-examples))

(in-package #:cl-mcp/specs/write-paths)

(defun contract-names ()
  "Return the functions this file puts a Function Spec on: none."
  '())

(defun property-names ()
  "Return the properties this file defines."
  '(write-resolves-project-targets-without-creating
    write-refuses-outside-and-absolute
    write-follows-existing-links
    writer-changes-only-the-expected-entries
    write-preserves-safe-spellings))

(defun spec-names ()
  "Return the named data specs this file defines."
  '(project-write-case refused-write-case link-write-case any-write-case
    safe-spelling-write-case))

(defun generator-names ()
  "Return the custom generators this file defines."
  '(project-write-case-generator refused-write-case-generator
    link-write-case-generator any-write-case-generator
    safe-spelling-write-case-generator))

(defun call-examples ()
  "Return the concrete CHECK-CALL examples of this file: none, as it has no
contract.  Its fixed cases are Rove tests (tests/write-path-specs-test.lisp)."
  '())

(defun register-specifications ()
  "Install this file's generators, specs and properties in CL-SPEC:*REGISTRY*.
Registering again replaces each definition by name.  Registering creates no
file and touches no ASDF registration; each trial does that for itself."
  (defgenerator project-write-case-generator ()
    "Draw a write inside the project, reached directly (DRAW-PROJECT-WRITE-CASE)."
    (draw-project-write-case))
  (defspec project-write-case list
    (:generator project-write-case-generator))
  (defgenerator refused-write-case-generator ()
    "Draw a write outside the project with a project control (DRAW-REFUSED-WRITE-CASE)."
    (draw-refused-write-case))
  (defspec refused-write-case list
    (:generator refused-write-case-generator))
  (defgenerator link-write-case-generator ()
    "Draw a write through one symlink (DRAW-LINK-WRITE-CASE)."
    (draw-link-write-case))
  (defspec link-write-case list
    (:generator link-write-case-generator))
  (defgenerator any-write-case-generator ()
    "Draw a write of any of the three shapes above (DRAW-WRITE-CASE)."
    (draw-write-case))
  (defspec any-write-case list
    (:generator any-write-case-generator))
  (defproperty write-resolves-project-targets-without-creating
      ((case project-write-case))
    "A target inside the project -- an existing file, a new file, or a new file
below one or two new directories -- is allowed, and ENSURE-WRITE-PATH returns
an absolute pathname naming its real path, spelled relative, as a relative
pathname or with a ./ and D/../D detour through an existing directory, with
the project root given directly or as a symlink alias.  An absolute spelling
is refused, and so is a detour whose .. follows a directory not created yet.
Either way the call creates and changes nothing anywhere in the tree."
    (:about ensure-write-path)
    (:kind :preservation)
    (:trials (:smoke 3 :normal 12))
    (with-write-fixture (fixture case)
      (validator-agrees-p fixture case)))
  (defproperty write-refuses-outside-and-absolute
      ((case refused-write-case))
    "A target in the dependency directory, outside/ or project-other/ is
refused by ENSURE-WRITE-PATH before the dependency's ASDF system is
registered, while it is and after it is removed -- reading follows the
registry, writing never does -- and FS-WRITE-FILE, asked to write there while
the system is registered, refuses and changes nothing.  In every phase a
project control is allowed as its real path when spelled relative and refused
when spelled absolute.  Only WRITE-PATH-REFUSED counts as a refusal."
    (:about ensure-write-path fs-write-file)
    (:kind :boundary)
    (:trials (:smoke 3 :normal 12))
    (let* ((control (getf case :project-control))
           (relative-control (write-case control))
           (absolute-control (write-case control :spelling :absolute)))
      (with-write-fixture (fixture case :extra-targets (list control))
        (flet ((phase-as-expected-p ()
                 (and (validator-agrees-p fixture case)
                      (validator-agrees-p fixture relative-control)
                      (validator-agrees-p fixture absolute-control))))
          (and (eq :refused (expected-write-decision case))
               (eq :allowed (expected-write-decision relative-control))
               (phase-as-expected-p)
               (progn (register-dependency fixture)
                      (and (phase-as-expected-p)
                           (writer-agrees-p fixture case)))
               (progn (unregister-dependency fixture)
                      (phase-as-expected-p)))))))
  (defproperty write-follows-existing-links
      ((case link-write-case))
    "A write through a symlink is decided by where the link really leads, not
by where it sits or how the path is spelled: through a directory link to the
project, a new or existing file, below new directories or not, is allowed as
its real path; through a link to the dependency, outside/ or project-other/ it
is refused, even for a file that does not exist yet.  A file link is judged by
its existing target the same way.  A link may sit in outside/ and be reached
through ../outside/, and a .. right after a link is refused.  Nothing is
created."
    (:about ensure-write-path)
    (:kind :resolution)
    (:trials (:smoke 3 :normal 12))
    (with-write-fixture (fixture case)
      (validator-agrees-p fixture case)))
  (defproperty writer-changes-only-the-expected-entries
      ((case any-write-case))
    "FS-WRITE-FILE, on any of the cases above, either writes exactly what the
policy allows -- it adds the expected new directories and file with exactly the
bytes written, or replaces the bytes of the existing file, through links
included, and adds, removes or changes nothing else anywhere in the tree, no
temporary file included -- or signals WRITE-PATH-REFUSED and changes nothing."
    (:about fs-write-file ensure-write-path)
    (:kind :state-transition)
    (:trials (:smoke 3 :normal 12))
    (with-write-fixture (fixture case)
      (writer-agrees-p fixture case)))
  (defgenerator safe-spelling-write-case-generator ()
    "Draw an allowed project write and the separator to double (DRAW-SAFE-SPELLING-CASE)."
    (draw-safe-spelling-case))
  (defspec safe-spelling-write-case list
    (:generator safe-spelling-write-case-generator))
  (defproperty write-preserves-safe-spellings
      ((case safe-spelling-write-case))
    "An allowed target in the project comes back as one and the same real path
however it is safely spelled.  Domain: an existing or not yet created file below
existing and new directories, reached directly or through one directory link
inside the project, under the project root or its alias.  The base spelling, a
relative native string with no .., no . and no trailing slash, must return
EXPECTED-WRITE-NATIVE.  Then that string parsed natively into a relative
pathname, ./ in front of it, and one of its separators doubled (.// in front
when it has none) must each be allowed and return exactly the base's path.  No
call may change the tree.  Every trial makes all four calls, one each.  A
refusal in any spelling fails the trial, since two refusals are not agreement."
    (:about ensure-write-path)
    (:kind :equivalence)
    (:trials (:smoke 3 :normal 12))
    (with-write-fixture (fixture case)
      ;; The expected path is fixed before the first call to the target.
      (let* ((expected (expected-write-native fixture case))
             (records (observe-spellings fixture (safe-spelling-variants case)))
             (base (first records)))
        (and
         ;; Every spelling was called once, a pathname and two strings among them.
         (safe-spelling-coverage-p records)
         ;; The base lands where the constructive oracle says, and creates nothing.
         (getf base :absolute)
         (equal expected (getf base :returned))
         (no-changes-p (getf base :changes))
         ;; Every other spelling is allowed, lands on the base's path, and
         ;; creates nothing.
         (every (lambda (record)
                  (and (getf record :absolute)
                       (equal (getf base :returned) (getf record :returned))
                       (no-changes-p (getf record :changes))))
                (rest records))))))
  (values))
