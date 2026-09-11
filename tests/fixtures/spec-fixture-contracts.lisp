;;;; tests/fixtures/spec-fixture-contracts.lisp
;;;;
;;;; The function-spec half of tests/fixtures/spec-fixture.lisp, in a file of
;;;; its own so that a cl-spec without DEFSPEC-FUNCTION costs these four
;;;; definitions and nothing else.
;;;;
;;;; A (WHEN (function-specs-supported-p) ...) guard in the main file could not
;;;; do that.  LOAD reads a whole top-level form before evaluating it, so
;;;; CL-SPEC:DEFSPEC-FUNCTION is resolved by the reader -- against a revision
;;;; that does not export the symbol, the guard has not run yet and the reader
;;;; error takes the entire fixture down, CLAMP and the properties with it.
;;;; A second file is only read when it is loaded.
;;;;
;;;; Loaded by the main fixture, never by ASDF: nothing depends on it.

(in-package #:cl-mcp/tests/fixtures/spec-fixture)

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
  (:returns (range integer 0 *)))
