;;;; specs/paths.lisp
;;;;
;;;; Properties of the read-access decision:
;;;; CL-MCP/SRC/UTILS/PATHS:ALLOWED-READ-PATH and
;;;; CL-MCP/SRC/UTILS/PATHS:RESOLVE-READABLE-PATH.
;;;;
;;;; The policy they are checked against, stated on descriptors in
;;;; specs/path-fixtures.lisp (EXPECTED-READ-DECISION) and never computed with
;;;; the functions under test or their helpers:
;;;;
;;;;   the resolved target is inside the project root              -> allowed
;;;;   it is inside a registered ASDF system's source directory    -> allowed
;;;;   it is inside neither                                        -> denied
;;;;
;;;; Roots are directories: sharing a string prefix (project/ and
;;;; project-other/) is not containment.  An allowed read must come back as an
;;;; absolute pathname naming the very object the case reaches -- checked
;;;; against the truename of what the fixture created -- and a denied one as
;;;; NIL from ALLOWED-READ-PATH and as RESOLVE-READABLE-PATH's refusal.  Each
;;;; function is compared with that expectation on its own.
;;;;
;;;; These are properties, not Function Specs: the answer depends on the
;;;; project root, the ASDF registry and the filesystem, so each trial builds its
;;;; own scratch tree from a descriptor inside the property and removes it
;;;; again (specs/path-fixtures.lisp).
;;;;
;;;; Verified domain: existing regular files and directories in a scratch tree
;;;; under the temporary directory, named with plain, spaced, Japanese, dotted
;;;; and bracketed parts, up to two directories deep; reached by relative,
;;;; absolute, pathname and ./-and-d/../d spellings, directly or through one
;;;; acyclic symlink to a file or to its directory, with the project root given
;;;; directly or as a symlink alias; with one fixture ASDF system registered or
;;;; not.  RESOLVE-READABLE-PATH runs with :MUST-EXIST T and NIL.
;;;;
;;;; Not covered: writes, paths that do not exist, dangling links and loops,
;;;; .. after a symlink (the OS and a lexical resolver disagree there), races
;;;; with the filesystem (TOCTOU), permissions, ACLs, hard links, mount
;;;; namespaces, Windows paths, and whether any MCP endpoint uses these
;;;; functions the way it should.

(defpackage #:cl-mcp/specs/paths
  (:use #:cl)
  (:import-from #:cl-spec/main
                #:defspec
                #:defproperty
                #:defgenerator)
  (:import-from #:cl-mcp/src/utils/paths
                #:allowed-read-path
                #:resolve-readable-path)
  (:import-from #:cl-mcp/specs/path-fixtures
                #:draw-project-read-case
                #:draw-dependency-read-case
                #:draw-denied-read-case
                #:draw-link-read-case
                #:with-read-fixture
                #:register-dependency
                #:unregister-dependency
                #:read-fixture-registered
                #:expected-read-decision
                #:both-allow-as-p
                #:both-deny-p)
  (:export #:register-specifications
           #:contract-names
           #:property-names
           #:spec-names
           #:generator-names
           #:call-examples))

(in-package #:cl-mcp/specs/paths)

(defun contract-names ()
  "Return the functions this file puts a Function Spec on: none."
  '())

(defun property-names ()
  "Return the properties this file defines."
  '(read-allows-project-files-as-themselves
    read-follows-dependency-registration
    read-denies-unlisted-regions
    read-judges-symlinks-by-their-target))

(defun spec-names ()
  "Return the named data specs this file defines."
  '(project-read-case dependency-read-case denied-read-case link-read-case))

(defun generator-names ()
  "Return the custom generators this file defines."
  '(project-read-case-generator dependency-read-case-generator
    denied-read-case-generator link-read-case-generator))

(defun call-examples ()
  "Return the concrete CHECK-CALL examples of this file: none, as it has no
contract.  Its fixed cases are Rove tests (tests/path-specs-test.lisp)."
  '())

(defun register-specifications ()
  "Install this file's generators, specs and properties in CL-SPEC:*REGISTRY*.
Registering again replaces each definition by name.  Registering creates no
file and touches no ASDF registration; each trial does that for itself."
  (defgenerator project-read-case-generator ()
    "Draw a read of a project file or directory (DRAW-PROJECT-READ-CASE)."
    (draw-project-read-case))
  (defspec project-read-case list
    (:generator project-read-case-generator))
  (defgenerator dependency-read-case-generator ()
    "Draw a read of a dependency place with two controls (DRAW-DEPENDENCY-READ-CASE)."
    (draw-dependency-read-case))
  (defspec dependency-read-case list
    (:generator dependency-read-case-generator))
  (defgenerator denied-read-case-generator ()
    "Draw a read of an unlisted place (DRAW-DENIED-READ-CASE)."
    (draw-denied-read-case))
  (defspec denied-read-case list
    (:generator denied-read-case-generator))
  (defgenerator link-read-case-generator ()
    "Draw a read through one symlink (DRAW-LINK-READ-CASE)."
    (draw-link-read-case))
  (defspec link-read-case list
    (:generator link-read-case-generator))
  (defproperty read-allows-project-files-as-themselves
      ((case project-read-case))
    "A file or directory inside the project is readable, and both functions
return an absolute pathname naming that very object, not merely some allowed
one: spelled relative, absolute, as a pathname or with a ./ and d/../d detour;
with the project root given directly or as a symlink alias; whether or not a
dependency is registered.  Names include spaces, Japanese and brackets."
    (:about allowed-read-path resolve-readable-path)
    (:kind :preservation)
    (:trials (:smoke 3 :normal 12))
    (destructuring-bind (&key target spelling root-alias dependency-registered must-exist)
        case
      (with-read-fixture (fixture :places (list target) :root-alias root-alias
                                  :register-dependency dependency-registered)
        (and (eq :allowed (expected-read-decision target dependency-registered))
             (both-allow-as-p fixture target spelling :must-exist must-exist)))))
  (defproperty read-follows-dependency-registration
      ((case dependency-read-case))
    "A place in a dependency directory outside the project is denied until its
ASDF system is registered, allowed as itself while it is, and denied again once
it is removed; meanwhile a project file stays allowed and a file in outside/ or
project-other/ stays denied.  The system is the fixture's own, with its .asd in
the dependency directory itself; no other registration is touched."
    (:about allowed-read-path resolve-readable-path)
    (:kind :state-transition)
    (:trials (:smoke 3 :normal 12))
    (destructuring-bind (&key target spelling project-control outside-control must-exist)
        case
      (with-read-fixture (fixture :places (list target project-control outside-control))
        ;; Each phase judges all three places by the policy under the
        ;; registration state the fixture is in at that moment.
        (flet ((all-as-expected-p ()
                 (let ((registered (read-fixture-registered fixture)))
                   (every (lambda (place)
                            (ecase (expected-read-decision place registered)
                              (:allowed (both-allow-as-p fixture place spelling
                                                         :must-exist must-exist))
                              (:denied (both-deny-p fixture place spelling
                                                    :must-exist must-exist))))
                          (list target project-control outside-control)))))
          (and (not (read-fixture-registered fixture))
               (all-as-expected-p)
               (progn (register-dependency fixture)
                      (all-as-expected-p))
               (progn (unregister-dependency fixture)
                      (all-as-expected-p)))))))
  (defproperty read-denies-unlisted-regions
      ((case denied-read-case))
    "A file or directory in outside/, or in project-other/ -- whose name only
shares the project's as a string prefix -- is denied by both functions:
ALLOWED-READ-PATH returns NIL and RESOLVE-READABLE-PATH signals its refusal.
Relative spellings go through ../, and a registered dependency elsewhere makes
no difference.  Only the refusal itself counts as a denial; any other
condition fails the trial."
    (:about allowed-read-path resolve-readable-path)
    (:kind :boundary)
    (:trials (:smoke 3 :normal 12))
    (destructuring-bind (&key target spelling root-alias dependency-registered must-exist)
        case
      (with-read-fixture (fixture :places (list target) :root-alias root-alias
                                  :register-dependency dependency-registered)
        (and (eq :denied (expected-read-decision target dependency-registered))
             (both-deny-p fixture target spelling :must-exist must-exist)))))
  (defproperty read-judges-symlinks-by-their-target
      ((case link-read-case))
    "A read through a symlink in the project or the dependency is decided by
where the link leads, not where it sits: to the project or a registered
dependency, it is allowed and returns the target itself; to outside/,
project-other/ or an unregistered dependency, it is denied.  Links go to the
file or to its directory, never to one of their own ancestors."
    (:about allowed-read-path resolve-readable-path)
    (:kind :resolution)
    (:trials (:smoke 3 :normal 12))
    (destructuring-bind (&key link spelling root-alias dependency-registered must-exist) case
      (with-read-fixture (fixture :places (list (getf link :target)) :links (list link)
                                  :root-alias root-alias
                                  :register-dependency dependency-registered)
        (ecase (expected-read-decision link dependency-registered)
          (:allowed (both-allow-as-p fixture link spelling :must-exist must-exist))
          (:denied (both-deny-p fixture link spelling :must-exist must-exist))))))
  (values))
