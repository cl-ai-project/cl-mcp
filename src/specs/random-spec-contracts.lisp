;;;; src/specs/random-spec-contracts.lisp
;;;;
;;;; The function-spec half of src/specs/random-spec.lisp, in a file of its
;;;; own so that a cl-spec without DEFSPEC-FUNCTION costs this one definition
;;;; and nothing else.
;;;;
;;;; A (WHEN (FUNCTION-SPECS-SUPPORTED-P) ...) guard in the main file could not
;;;; do that.  LOAD reads a whole top-level form before evaluating it, so
;;;; CL-SPEC:DEFSPEC-FUNCTION is resolved by the reader -- against a revision
;;;; that does not export the symbol, the guard has not run yet and the reader
;;;; error takes the entire file down, the spec and the property with it.
;;;;
;;;; Loaded by src/specs/random-spec.lisp, never by ASDF: nothing depends on it.

(in-package #:cl-mcp/src/specs/random-spec)

(cl-spec:defspec-function generate-random-hex-string
  "N-BYTES of random data, rendered as a lowercase hex string.

The length half of the contract, which the property in the main file does not
state.  An :about selection does not reach a contract -- properties and
function specs are selected separately -- so this one is run with
spec-check function=GENERATE-RANDOM-HEX-STRING."
  (:args (n-bytes hex-byte-count))
  (:returns (type string))
  (:post (= (length result) (* 2 n-bytes))))
