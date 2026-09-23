;;;; tests/fixtures/spec-wire-fixture.lisp
;;;;
;;;; Declarations for tests/spec-wire-test.lisp, which loads this file through
;;;; the public load-system tool -- into a worker, or into the server's own
;;;; image when the pool is off -- as an inferred subsystem of cl-mcp.
;;;;
;;;; Nothing depends on it, so a normal load of cl-mcp never reads it, and its
;;;; :import-from clauses pull in cl-spec and the check-it backend only when it
;;;; is asked for by name.  One property is written to fail and one fails with
;;;; a compound counterexample, so it belongs in an image that is thrown away.

(defpackage #:cl-mcp/tests/fixtures/spec-wire-fixture
  (:use #:cl)
  (:import-from #:cl-spec/main
                #:defspec #:defspec-function #:defproperty)
  ;; A bare :import-from declares the check-it backend as a dependency
  ;; without importing any symbol from it.
  (:import-from #:cl-spec/src/backends/check-it)
  (:export #:wire-clamp
           #:wire-int
           #:wire-clamp-stays-inside
           #:wire-clamp-is-wrong-on-purpose
           #:wire-lists-are-empty))

(in-package #:cl-mcp/tests/fixtures/spec-wire-fixture)

(defun wire-clamp (value low high)
  "Return VALUE limited to the interval [LOW, HIGH]."
  (min high (max low value)))

(defspec wire-int (range integer 0 100))

(defspec-function wire-clamp
  "WIRE-CLAMP returns a value inside the interval it was given."
  (:args (value wire-int) (low wire-int) (high wire-int))
  (:pre (<= low high))
  (:returns wire-int)
  (:post (and (<= low result) (<= result high))))

(defproperty wire-clamp-stays-inside
    ((value wire-int))
  "Clamping to [10, 90] lands inside [10, 90]."
  (:about wire-clamp)
  (:kind :invariant)
  (<= 10 (wire-clamp value 10 90) 90))

(defproperty wire-clamp-is-wrong-on-purpose
    ((value wire-int))
  "False on purpose: clamping to [10, 90] changes anything outside it."
  (:about wire-clamp)
  (:kind :invariant)
  (= (wire-clamp value 10 90) value))

(defproperty wire-lists-are-empty
    ((items (list-of wire-int)))
  "False on purpose, over a list, so the counterexample is a compound value
that earns an object id."
  (:kind :invariant)
  (null items))
