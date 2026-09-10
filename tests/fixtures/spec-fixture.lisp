;;;; tests/fixtures/spec-fixture.lisp
;;;;
;;;; Fixture definitions for tests/spec-integration-test.lisp.
;;;;
;;;; Deliberately NOT listed in tests.lisp or cl-mcp.asd: nothing depends on
;;;; this file, so ASDF never loads it and a normal image never sees it.  The
;;;; integration test LOADs it into a registry of its own and restores the
;;;; previous one afterwards, because one of the properties below is written
;;;; to fail and a failing property must not be left lying in a working image.
;;;;
;;;; CL-SPEC symbols are written package-qualified so this file needs no
;;;; :IMPORT-FROM on a system that may not be loaded.

(defpackage #:cl-mcp/tests/fixtures/spec-fixture
  (:use #:cl)
  (:export #:clamp
           #:function-specs-supported-p
           #:magnitude
           #:widen
           #:never-callable
           #:small-int
           #:clamp-is-within-bounds
           #:clamp-is-idempotent
           #:clamp-is-wrong-on-purpose
           #:register-corrected-property))

(in-package #:cl-mcp/tests/fixtures/spec-fixture)

(defun clamp (value low high)
  "Return VALUE confined to the closed interval [LOW, HIGH].

No I/O and no shared state: the smallest thing worth checking a property
against, which is what a first demonstration calls for."
  (cond ((< value low) low)
        ((> value high) high)
        (t value)))

(cl-spec:defspec small-int (and integer (range 0 100)))

(cl-spec:defproperty clamp-is-within-bounds
    ((value small-int) (low small-int))
  "CLAMP never returns a value below LOW."
  (:about clamp)
  (:kind :invariant)
  (:tags :bounds)
  (:trials (:normal 100))
  (>= (clamp value low 100) low))

(cl-spec:defproperty clamp-is-idempotent
    ((value small-int))
  "Clamping twice is the same as clamping once."
  (:about clamp)
  (:kind :idempotence)
  (= (clamp (clamp value 10 90) 10 90)
     (clamp value 10 90)))

(cl-spec:defproperty clamp-is-wrong-on-purpose
    ((value small-int))
  "A property that is false, so a test can see a real counterexample.

Not a claim about CLAMP: it asserts that clamping to [10, 90] leaves every
value unchanged, which is false for anything outside that interval."
  (:about clamp)
  (:kind :invariant)
  (= (clamp value 10 90) value))

(defun register-corrected-property ()
  "Re-register CLAMP-IS-WRONG-ON-PURPOSE as a statement that holds.

Stands in for an edit followed by a reload: DEFPROPERTY expands into a
REGISTER-PROPERTY call, so evaluating this replaces the registered definition
with one carrying a fresh source form, metadata and compiled function --
exactly what loading an edited file produces."
  (cl-spec:defproperty clamp-is-wrong-on-purpose
      ((value small-int))
    "Corrected: clamping to [10, 90] is idempotent."
    (:about clamp)
    (:kind :invariant)
    (= (clamp (clamp value 10 90) 10 90)
       (clamp value 10 90))))


(defun widen (value)
  "Return VALUE moved one step away from zero.

Written to break its own contract at the top of SMALL-INT's range, so a test
can see a contract failure that is not a property failure."
  (1+ value))


(defun never-callable (value)
  "Return VALUE. Its contract's :PRE admits nothing, so nothing ever calls it."
  value)


(defun magnitude (value)
  "Return the absolute value of VALUE."
  (abs value))

(defun function-specs-supported-p ()
  "Return true when the loaded cl-spec implements function specs.

FUNCTION-SPEC-DATA is the discriminator, not CHECK-FUNCTION: a cl-spec that
predates function specs still has CHECK-FUNCTION fbound, as a stub that
signals NOT-IMPLEMENTED, so FBOUNDP alone answers the wrong question."
  (let ((data (find-symbol "FUNCTION-SPEC-DATA" "CL-SPEC")))
    (and data (fboundp data) t)))

;; Guarded because this file is LOADed, not compiled: against a cl-spec whose
;; DEFSPEC-FUNCTION is still a stub, one signalling form aborts the load and
;; every test in the file fails -- including the three that have nothing to do
;; with contracts.  cl-mcp does not depend on cl-spec and must stay green
;; against whichever revision happens to be installed.
(when (function-specs-supported-p)
  (cl-spec:defspec-function clamp
    "CLAMP returns a value inside the interval it was given."
    (:args (value small-int) (low small-int) (high small-int))
    (:pre (<= low high))
    (:returns small-int)
    (:post (and (<= low result) (<= result high))))

  (cl-spec:defspec-function widen
    "WIDEN stays inside SMALL-INT, which it does not."
    (:args (value small-int))
    (:returns small-int)
    (:post (> result value)))

  (cl-spec:defspec-function never-callable
    "A contract whose precondition no generated value can satisfy."
    (:args (value small-int))
    (:pre (> value 1000))
    (:returns small-int))

  (cl-spec:defspec-function magnitude
    "The magnitude is never negative, and has no upper bound worth naming."
    (:args (value small-int))
    (:returns (range integer 0 *))))
