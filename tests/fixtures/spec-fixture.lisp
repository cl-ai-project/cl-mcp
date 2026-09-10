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
           #:register-corrected-property
           #:function-specs-supported-p
           #:contracts-registered-p))

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
  "Return VALUE moved halfway up SMALL-INT's range, leaving it at the top.

Written to break its :RETURNS over most of that range rather than at one end
of it.  (1+ value) broke it only at 100, one of the 101 values check-it draws
uniformly, so the test that reads the failure passed 300 trials and still came
up empty about once in twenty runs -- measured at 4 misses in 60.  A fixture
whose failure is rare makes the test that reads it a coin toss."
  (+ value 50))

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

(defvar *contracts-registered* nil
  "True once the contract half of this fixture loaded without signalling.")

(defun contracts-registered-p ()
  "Return true when this fixture's function specs are in the registry.

What a test should ask before running contract coverage.  FUNCTION-SPECS-
SUPPORTED-P says the loaded cl-spec has the API; this says the definitions
actually made it in, which is not the same answer when DEFSPEC-FUNCTION
signals at run time on a revision that exports it."
  (and *contracts-registered* t))

;; Loaded rather than guarded in place.  This file is LOADed, not compiled,
;; and LOAD reads each top-level form before evaluating it -- so a
;; CL-SPEC:DEFSPEC-FUNCTION form written here is resolved by the reader
;; whatever a guard around it says, and against a revision that does not
;; export the symbol the reader error takes the whole fixture down, CLAMP and
;; the properties with it.  Only a separate file is left unread.
;;
;; HANDLER-CASE for the other half of the same promise: a revision that reads
;; the file and then signals while registering -- a normalization change, a
;; duplicate registration -- would otherwise take the same three unrelated
;; tests down that the split was written to protect.  Reported rather than
;; swallowed, and CONTRACTS-REGISTERED-P tells a test which happened.
(when (function-specs-supported-p)
  (handler-case
      (progn
        (load (merge-pathnames "tests/fixtures/spec-fixture-contracts.lisp"
                               (asdf:system-source-directory "cl-mcp")))
        (setf *contracts-registered* t))
    (error (condition)
      (format *error-output*
              "~&;; spec-fixture: contracts NOT registered: ~A~%" condition))))
