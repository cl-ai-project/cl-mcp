;;;; src/tools/spec-tools.lisp
;;;;
;;;; MCP tools over cl-spec: find the contracts registered about a symbol,
;;;; read one, and run it.
;;;;
;;;; All three run in the worker.  The cl-spec registry is populated by
;;;; loading the system that defines the properties, and that load happens in
;;;; the session's worker image -- so discovery, detail and execution have to
;;;; happen there too, or a tool would answer from a registry nobody wrote to.
;;;; It is also what makes "edit, load-system, re-check" work: the definition
;;;; that runs is the one the last load put in this worker.

(defpackage #:cl-mcp/src/tools/spec-tools
  (:use #:cl)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result)
  (:import-from #:cl-mcp/src/tools/spec-entry
                #:spec-symbol-response
                #:spec-describe-response
                #:spec-check-response)
  (:import-from #:cl-mcp/src/proxy
                #:with-proxy-dispatch)
  (:export #:spec-symbol
           #:spec-describe
           #:spec-check))

(in-package #:cl-mcp/src/tools/spec-tools)

(define-tool "spec-symbol"
  :description
  "Find the cl-spec contracts registered about a symbol, joined with this
image's own knowledge of it (signature, docstring, source location).

Use this FIRST when you are about to change a function: it tells you which
properties claim something about it, without running anything.

PREREQUISITE: cl-spec must be loaded in this session's worker, and so must the
system that defines the specs and properties.  Load them with 'load-system'
(system 'cl-spec/check-it' for execution as well as introspection).  A status
of cl-spec-not-loaded is NOT evidence that the symbol has no contract.

Property bodies are NOT included here: each entry reports how many forms the
body has and points at 'spec-describe' for the text.  This keeps one response's
size from depending on how much the property's author wrote.

properties_about lists ONLY properties registered with (:about <symbol>).
Callers, generic-function methods, macro users and shared state are not
analysed; this is not a change-impact analysis.

Examples:
  symbol='my-app::transfer'
  symbol='transfer', package='my-app'"
  :args
  ((symbol :type :string :required t
    :description
    "Symbol to look up: 'SYM', 'PKG:SYM' or 'PKG::SYM'. Resolved with find-symbol; never interned.")
   (package :type :string
    :description "Package for an unqualified symbol (default: COMMON-LISP-USER)")
   (include-runtime :type :boolean :json-name "include_runtime" :default t
    :description
    "Join this image's signature, docstring and source location (default: true)"))
  :body
  (with-proxy-dispatch (id "worker/spec-symbol"
                           (make-ht "symbol" symbol
                                    "package" package
                                    "include_runtime" include-runtime))
    (result id (spec-symbol-response
                (make-ht "symbol" symbol
                         "package" package
                         "include_runtime" include-runtime)))))

(define-tool "spec-describe"
  :description
  "Read one registered cl-spec definition in full: a property's body and
source form, or a spec's normalized tree.

Use this after 'spec-symbol' has told you which names exist.  A property body
is a specification you can read: it says what relation must hold, over which
generated inputs.  It does not say the relation holds for every input -- only
that this relation is checked over that domain.

kind='property'      the property's arguments, body, source form and digest
kind='spec'          the spec's normalized IR tree
kind='function-spec' NOT SUPPORTED by this cl-spec revision; the tool says so
                     rather than inventing a projection

Long bodies are cut at max_chars and the cut is reported. Truncated text is a
preview for reading, NOT a form that can be read back.

Examples:
  kind='property', name='my-app::transfer-preserves-total'
  kind='spec', name='my-app::account'"
  :args
  ((kind :type :string :required t
    :enum ("property" "spec" "function-spec")
    :description "What to describe")
   (name :type :string :required t
    :description "Registered name: 'SYM', 'PKG:SYM' or 'PKG::SYM'")
   (package :type :string
    :description "Package for an unqualified name (default: COMMON-LISP-USER)")
   (max-chars :type :integer :json-name "max_chars"
    :description "Maximum characters of body and source form (default: 8000)"))
  :body
  (with-proxy-dispatch (id "worker/spec-describe"
                           (make-ht "kind" kind
                                    "name" name
                                    "package" package
                                    "max_chars" max-chars))
    (result id (spec-describe-response
                (make-ht "kind" kind
                         "name" name
                         "package" package
                         "max_chars" max-chars)))))

(define-tool "spec-check"
  :description
  "Run cl-spec properties and return structured results and counterexamples.

Give EITHER property (one named property) OR symbol (every property registered
with (:about <symbol>)).  Not both.

WHAT A RESULT MEANS
  passed          not falsified over the trials that were generated. This is
                  evidence about the inputs that were tried, not a proof.
  failed          a counterexample was found; both the original and the shrunk
                  arguments are reported.
  error           the property body signalled.
  timeout         the deadline expired. NOTHING was proved or disproved.
  generator-error no value could be generated. Nothing was checked.
  not-run         the whole-call budget was spent before this property started.
  no-properties   ZERO properties were selected. This is NOT a successful
                  verification: nothing ran.

verified is true only when at least one property was selected and every one of
them passed.

REPRODUCING A RUN
Every result carries seed (decimal TEXT, because a cl-spec seed can exceed
what JSON holds exactly as a number), profile, and definition_digest.  Re-run
with the same property, seed and profile to regenerate the same trial
sequence.  Pass expect_definition_digest to be told when the definitions moved
underneath you: definition_match then reports match or mismatch.

This regenerates the trial sequence.  It does NOT reproduce the code revision,
external I/O, the clock or shared mutable state, and it is NOT replay of a
saved counterexample against a fixed implementation.

TIMEOUT
timeout_seconds is the budget for the WHOLE call, spent across the selection
in order, enforced by cl-mcp rather than by cl-spec. A property that exhausts
it is reported as timeout and the rest as not-run.  If a run thread cannot be
stopped the response says so; use 'pool-kill-worker' before trusting later
results in that session.

PREREQUISITE: load 'cl-spec/check-it' and the system defining the properties
with 'load-system' first.  After editing a definition, load-system again: this
tool runs what is in the worker image, and it is the same worker your
load-system call wrote to.

Examples:
  symbol='my-app::transfer'
  property='my-app::transfer-preserves-total'
  property='my-app::transfer-preserves-total', seed='3963993791726803706',
    profile='normal', expect_definition_digest='a41f9c2b7d0e5518'"
  :args
  ((property :type :string
    :description "One registered property to run. Exclusive with symbol.")
   (symbol :type :string
    :description
    "Run every property registered (:about <symbol>). Exclusive with property.")
   (package :type :string
    :description "Package for an unqualified name (default: COMMON-LISP-USER)")
   (profile :type :string
    :description "Trial-count profile, e.g. 'normal' or 'smoke' (default: normal)")
   (seed :type :string
    :description
    "Decimal digits AS A STRING. A JSON number would already have lost digits.")
   (expect-definition-digest :type :string
    :json-name "expect_definition_digest"
    :description
    "Digest from an earlier run; a mismatch is reported rather than ignored")
   (timeout-seconds :type :number :json-name "timeout_seconds"
    :description "Budget for the whole call in seconds (default: 60)")
   (max-value-chars :type :integer :json-name "max_value_chars"
    :description
    "Maximum printed characters per counterexample value (default: 2000)"))
  :body
  (let ((params (make-ht "property" property
                         "symbol" symbol
                         "package" package
                         "profile" profile
                         "seed" seed
                         "expect_definition_digest" expect-definition-digest
                         "timeout_seconds" timeout-seconds
                         "max_value_chars" max-value-chars)))
    (with-proxy-dispatch (id "worker/spec-check" params)
      (result id (spec-check-response params)))))
