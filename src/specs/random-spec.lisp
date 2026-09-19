;;;; src/specs/random-spec.lisp
;;;;
;;;; cl-spec definitions for CL-MCP/SRC/UTILS/RANDOM.
;;;;
;;;; Opt-in, and never loaded by cl-mcp itself: nothing in main.lisp,
;;;; tests.lisp or cl-mcp.asd refers to this file, so the server keeps its
;;;; promise of not depending on cl-spec.  Load cl-spec first, then this
;;;; system:
;;;;
;;;;   (asdf:load-system "cl-spec/check-it")
;;;;   (asdf:load-system "cl-mcp/src/specs/random-spec")
;;;;
;;;; then, from an MCP client:
;;;;
;;;;   spec-list
;;;;   spec-check property=CL-MCP/SRC/SPECS/RANDOM-SPEC::HEX-STRING-IS-LOWERCASE-HEX
;;;;   spec-check function=CL-MCP/SRC/UTILS/RANDOM:GENERATE-RANDOM-HEX-STRING
;;;;
;;;; Both are needed.  SPEC-CHECK SYMBOL=... is not the two in one: an :about
;;;; selection covers properties only, and answers CONTRACT-NOT-RUN for the
;;;; function spec it did not run.
;;;;
;;;; CL-SPEC symbols are written package-qualified, so no :IMPORT-FROM on
;;;; cl-spec is needed -- which also keeps ASDF from inferring a cl-spec
;;;; dependency for this system.  The load order above is therefore explicit
;;;; rather than declared: this file cannot be read without cl-spec present.
;;;;
;;;; The function-spec half lives in src/specs/random-spec-contracts.lisp, for
;;;; the reason tests/fixtures/spec-fixture-contracts.lisp documents at length:
;;;; LOAD (and COMPILE-FILE) read a whole top-level form before evaluating it,
;;;; so a CL-SPEC:DEFSPEC-FUNCTION form is resolved by the reader whatever
;;;; guard surrounds it.  Against a cl-spec without function specs, guarding in
;;;; place would take the spec and the property below down with it; a separate
;;;; file is simply never read.

(defpackage #:cl-mcp/src/specs/random-spec
  (:use #:cl)
  (:import-from #:cl-mcp/src/utils/random
                #:generate-random-hex-string)
  (:export #:hex-byte-count
           #:hex-string-is-lowercase-hex
           #:function-specs-supported-p))

(in-package #:cl-mcp/src/specs/random-spec)

(defun function-specs-supported-p ()
  "Return true when the loaded cl-spec implements function specs.

FUNCTION-SPEC-DATA is the discriminator, not CHECK-FUNCTION: a cl-spec that
predates function specs still has CHECK-FUNCTION fbound, as a stub that signals
NOT-IMPLEMENTED, so FBOUNDP alone answers the wrong question."
  (let ((data (find-symbol "FUNCTION-SPEC-DATA" "CL-SPEC")))
    (and data (fboundp data) t)))

;; The argument is a byte count, so the domain is an integer range and
;; cl-spec's own generator covers it completely.  That is the whole reason this
;; function is the first target: (TYPE STRING) would be generated as short
;; alphanumeric strings, and a property about stripping control characters
;; would pass without ever producing one.
(cl-spec:defspec hex-byte-count
  (and integer (range 0 32)))

(cl-spec:defproperty hex-string-is-lowercase-hex
    ((n-bytes hex-byte-count))
  "The result is entirely lowercase hex digits, and two characters per byte."
  (:about generate-random-hex-string)
  (:kind :invariant)
  (:tags :encoding :security)
  (:trials (:smoke 20 :normal 200))
  (every (lambda (character) (find character "0123456789abcdef"))
         (generate-random-hex-string n-bytes)))

;; Loaded rather than guarded in place -- see the file comment above.
;;
;; ASDF:SYSTEM-RELATIVE-PATHNAME and not *LOAD-TRUENAME*: when ASDF compiles
;; this file, the form below runs from the FASL, whose directory is the output
;; translation, not src/specs/.
;;
;; HANDLER-CASE for the other half of the same promise: a cl-spec that reads
;; the file and then signals while registering -- a normalization change, a
;; duplicate registration -- would otherwise take the spec and the property
;; above down with the same failure.  Reported rather than swallowed, and
;; FUNCTION-SPECS-SUPPORTED-P tells a reader which happened.
(when (function-specs-supported-p)
  (handler-case
      (load (asdf:system-relative-pathname
             "cl-mcp" "src/specs/random-spec-contracts.lisp"))
    (error (condition)
      (format *error-output*
              "~&;; random-spec: contracts NOT registered: ~A~%" condition))))
