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

(cl-spec:defgenerator scripted-arguments ()
  "Return the next scripted argument list, so a test controls its inputs."
  (pop *scripted-arguments*))

(cl-spec:defspec-function remaining-balance
  "Require the remainder when the balance suffices, and the named error when
it does not."
  (:args (balance (range integer 0 1000)) (amount (range integer 1 1000)))
  (:args-generator scripted-arguments)
  (:cases
    (:sufficient-funds
      "The amount fits: return the remaining balance."
      (:when (<= amount balance))
      (:returns (range integer 0 *))
      (:post (= result (- balance amount))))
    (:insufficient-funds
      "The amount does not fit: signal the named error."
      (:when (> amount balance))
      (:signals (type insufficient-funds)))))

(cl-spec:defspec-function overlapping-balance
  "Two guards that both hold when the amounts are equal."
  (:args (balance (range integer 0 1000)) (amount (range integer 1 1000)))
  (:args-generator scripted-arguments)
  (:cases
    (:at-least (:when (>= balance amount)) (:returns (range integer 0 *)))
    (:at-most (:when (<= balance amount)) (:returns (range integer 0 *)))))

(cl-spec:defspec-function withdraw-without-recording!
  "A successful call must reduce the stored balance, which this target does not."
  (:args (purse (satisfies purse-p)) (amount (range integer 1 100)))
  (:args-generator scripted-arguments)
  (:capture
    (balance-before (purse-balance purse))
    (id-before (purse-id purse)))
  (:cases
    (:sufficient-funds
      (:when (<= amount balance-before))
      (:returns (type integer))
      (:state-post (= (purse-balance purse) (- balance-before amount))
                   (eql (purse-id purse) id-before)))))

(cl-spec:defspec impossible-int (and (range integer 0 100)
                                     (satisfies never-satisfied-p)))

(cl-spec:defspec-function magnitude-of-impossible
  "A contract whose argument spec no generated candidate satisfies."
  (:args (value impossible-int))
  (:returns (range integer 0 *)))
