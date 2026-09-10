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
;;;;
;;;; All three belong to the optional :CL-SPEC tool group and are hidden until
;;;; it is switched on.  cl-mcp does not depend on cl-spec and most of its
;;;; users have never heard of it; showing them three tools they cannot use --
;;;; and three long descriptions in every model context -- is a cost with no
;;;; return.  Enable with MCP_ENABLE_TOOL_GROUPS=cl-spec or
;;;; (cl-mcp:run :tool-groups (list :cl-spec)).

(defpackage #:cl-mcp/src/tools/spec-tools
  (:use #:cl)
  (:import-from #:cl-mcp/src/tools/define-tool
                #:define-tool)
  (:import-from #:cl-mcp/src/tools/helpers
                #:make-ht #:result)
  (:import-from #:cl-mcp/src/tools/spec-entry
                #:spec-list-response
                #:spec-symbol-response
                #:spec-describe-response
                #:spec-check-response)
  (:import-from #:cl-mcp/src/proxy
                #:with-proxy-dispatch)
  (:export #:spec-list
           #:spec-symbol
           #:spec-describe
           #:spec-check))

(in-package #:cl-mcp/src/tools/spec-tools)

(define-tool "spec-list"
  :group :cl-spec
  :description
  "List the cl-spec specs, function specs and properties registered in this
session's worker.

Use this when you do not yet know what is here. The other spec tools all take
a name you already have; this is the one that answers \"what contracts does
this project define?\".

Returns names; for each property its kind, tags, the symbols it is
(:about ...), and its docstring; for each function spec its parameter names and
whether it carries a :returns. NOT bodies and NOT specs -- read one with
'spec-describe', run one with 'spec-check'.

A function spec is a contract on one function: which inputs it accepts and
which output it must return. A property is a relation someone asserted about
one or more functions. A project can have either without the other, so a
listing with no function specs is not a listing with no contracts.

An empty listing is NOT evidence that a project has no contracts: it shows
what is registered in THIS worker, so a definition whose system has not been
loaded is not here. Load it with 'load-system' first.

tag names a keyword. A tag no loaded code mentions is reported as
no-such-keyword rather than as an empty result -- 'nothing carries this tag'
and 'this tag does not exist here' are different answers.

Examples:
  (no arguments) -- everything registered
  kind='properties', package='my-app'
  kind='function-specs'
  tag='critical'"
  :args
  ((kind :type :string
    :enum ("specs" "properties" "function-specs" "both")
    :description "What to list (default: both, which is all three)")
   (package :type :string
    :description "Only names whose home package is this one")
   (tag :type :string
    :description "Only properties carrying this tag. Applies to properties only.")
   (limit :type :integer
    :description "Maximum names of each kind to return (default: 200)")
   (timeout-seconds :type :number :json-name "timeout_seconds"
    :description "Deadline for reading the registry in seconds (default: 30)"))
  :body
  (let ((params (make-ht "kind" kind
                         "package" package
                         "tag" tag
                         "limit" limit
                         "timeout_seconds" timeout-seconds)))
    (with-proxy-dispatch (id "worker/spec-list" params)
      (result id (spec-list-response params)))))

(define-tool "spec-symbol"
  :group :cl-spec
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
    "Join this image's signature, docstring and source location (default: true)")
   (timeout-seconds :type :number :json-name "timeout_seconds"
    :description
    "Deadline for reading the registry in seconds (default: 30). Digesting every property about a symbol is bounded work, but not free."))
  :body
  ;; One binding, used by both branches of WITH-PROXY-DISPATCH.  Marshalling
  ;; the arguments twice means keeping two copies in step by hand, and a
  ;; divergence would make the pooled and inline paths answer differently for
  ;; the same call with no test able to see it.
  (let ((params (make-ht "symbol" symbol
                         "package" package
                         "include_runtime" include-runtime
                         "timeout_seconds" timeout-seconds)))
    (with-proxy-dispatch (id "worker/spec-symbol" params)
      (result id (spec-symbol-response params)))))

(define-tool "spec-describe"
  :group :cl-spec
  :description
  "Read one registered cl-spec definition in full: a property's body and
source form, or a spec's normalized tree.

Use this after 'spec-symbol' has told you which names exist.  A property body
is a specification you can read: it says what relation must hold, over which
generated inputs.  It does not say the relation holds for every input -- only
that this relation is checked over that domain.

kind='property'      the property's arguments, body, source form and digest
kind='spec'          the spec's normalized IR tree
kind='function-spec' the contract: the spec of each argument, the spec of the
                     return value, and the :pre and :post forms. This is what
                     answers \"which inputs does this accept and which output
                     must it return\". A cl-spec revision that cannot project
                     one says so rather than having a projection invented for
                     it.

Long bodies are cut at max_chars and the cut is reported. Truncated text is a
preview for reading, NOT a form that can be read back.

Examples:
  kind='property', name='my-app::transfer-preserves-total'
  kind='spec', name='my-app::account'
  kind='function-spec', name='my-app::transfer'"
  :args
  ((kind :type :string :required t
    :enum ("property" "spec" "function-spec")
    :description "What to describe")
   (name :type :string :required t
    :description "Registered name: 'SYM', 'PKG:SYM' or 'PKG::SYM'")
   (package :type :string
    :description "Package for an unqualified name (default: COMMON-LISP-USER)")
   (max-chars :type :integer :json-name "max_chars"
    :description "Maximum characters of body and source form (default: 8000)")
   (timeout-seconds :type :number :json-name "timeout_seconds"
    :description "Deadline for reading the registry in seconds (default: 30)"))
  :body
  (let ((params (make-ht "kind" kind
                         "name" name
                         "package" package
                         "max_chars" max-chars
                         "timeout_seconds" timeout-seconds)))
    (with-proxy-dispatch (id "worker/spec-describe" params)
      (result id (spec-describe-response params)))))

(define-tool "spec-check"
  :group :cl-spec
  :description
  "Run cl-spec properties and return structured results and counterexamples.

Give EXACTLY ONE of property (one named property), symbol (every property
registered with (:about <symbol>)) or function (one registered function spec,
run with check-function).

A function spec is the contract: which inputs the function accepts (:args,
:pre) and which output it must return (:returns, :post).  It is NOT selected
by symbol -- an :about selection covers properties only -- and the response
says so when one exists.  Read it first with 'spec-describe'
kind='function-spec'.

WHAT A RESULT MEANS

Per property, results[].status:
  passed          not falsified over the trials that were generated. This is
                  evidence about the inputs that were tried, not a proof.
  failed          a counterexample was found; both the original and the shrunk
                  arguments are reported.
  error           the property body signalled.
  skipped         defined by cl-spec; the current backend does not produce it.
  pending         same. Reported as-is if it ever appears.
  timeout         the deadline expired. NOTHING was proved or disproved.
  not-run         the whole-call budget was spent before this property started.
  generator-error no value could be generated. Nothing was checked.
  backend-error   cl-spec signalled something else. Nothing was checked.
  not-registered  the name resolved but nothing is registered under it.
  undefined-function
                  a function the run needed is not defined in this image --
                  the one a contract names, or one its property body calls.
                  Write it, or load its system. NOT an adapter fault.
  unsupported     the loaded cl-spec cannot run this. Nothing was checked.
  internal-error  this adapter failed. NOT a statement about the property.

For the whole call, status:
  no-properties   ZERO properties were selected. This is NOT a successful
                  verification: nothing ran.
  unsupported     the loaded cl-spec cannot run a contract. Nothing ran.
  completed       every selected property reached a verdict.
  incomplete      at least one timeout, not-run, or *-error.
  cl-spec-not-loaded / cl-spec-incomplete / backend-not-loaded /
  unresolved-symbol / not-registered / invalid-arguments / internal-error
                  the call did not get as far as running anything. None of
                  these is evidence about the symbol or the property.

counts has a field for the five common statuses plus other, and by_status,
which covers every status that occurred; selected always equals their sum. A
status without a field of its own is in by_status, never dropped.

verified is true ONLY when at least one property was selected, every one of
them passed, AND every one evaluated at least one trial. A property whose
profile resolves to a budget of zero passes without running anything, and that
is not a verification. verification_gaps names what the run could not
establish, and always includes input-domain coverage, which nothing measures.

CONTRACT RUNS (function=...)
A contract check generates arguments from :args, drops the ones :pre refuses,
calls the function, then checks :returns and :post. results[].contract carries:
  rejected         generated argument lists :pre refused
  effective_trials trials minus rejected -- what the function was ACTUALLY
                   called with. A passing run whose effective_trials is 0
                   checked nothing; cl-spec reports that as skipped, not
                   passed. NULL means the figure could not be derived, NOT
                   zero calls: read rejected_measured and rejected_overcounted
                   to see which.
  rejected_measured
                   false when this cl-spec exports no reader for the refused
                   count. rejected is then null and the trial count above is
                   an upper bound on what was checked.
  rejected_overcounted
                   true when cl-spec reported more refusals than trials, which
                   its counter can do when the function signals. The
                   difference is withheld rather than published as a negative
                   or clamped to a zero that would read as never called.
  has_precondition false when the contract has no :pre at all, so nothing
                   could be refused and rejected 0 is not a shortfall. Null
                   when the definition could not be read.
  failure_reason   which half broke: return-spec, postcondition, precondition,
                   condition. Absent has two meanings, told apart by
                   failure_reason_readable: true means cl-spec could not
                   reproduce the counterexample, so the function is not
                   deterministic; false means this cl-spec exports no reader
                   for it, which says nothing about the function.
  explanation      cl-spec's structured account of a return value that missed
                   its :returns spec.
Because rejections are counted here, verification_gaps does not claim they are
unmeasured for a contract run.

A contract has no :trials table for profile to select from, so its budget is
the backend default unless you pass trials. Raise it when effective_trials
comes back small: a :pre that refuses most of what is generated leaves the
interesting inputs unreached, and cl-spec does not yet reflect a precondition
into the generator (its specification 19). Raise timeout_seconds alongside it:
that budget covers the WHOLE call and defaults to 60 seconds, so a run sized
past it comes back as a timeout with worker_reuse unknown -- not as a result,
and not as a smaller run.

REPRODUCING A RUN
Every result carries seed (decimal TEXT, because a cl-spec seed can exceed
what JSON holds exactly as a number), profile, and definition_digest.  Re-run
with the same property, seed and profile to regenerate the same trial
sequence.  Pass expect_definition_digest to be told when the definitions moved
underneath you.

definition_match is four-valued: match, mismatch, unknown (the digest could
not be computed, or its input hit the print limit -- this is NOT a
disagreement) and not-checked (no expect_definition_digest was given).
reproduction_faithful says the same four things about the call as a whole.
A matching digest means the property and the specs it references are
unchanged; it says nothing about the implementation, the backend, or the
environment.

This regenerates the trial sequence.  It does NOT reproduce the code revision,
external I/O, the clock or shared mutable state, and it is NOT replay of a
saved counterexample against a fixed implementation.

TIMEOUT
timeout_seconds is the budget for the WHOLE call, spent across the selection
in order, enforced by cl-mcp rather than by cl-spec. A property that exhausts
it is reported as timeout and the rest as not-run.  If a run thread cannot be
stopped the response says so.

After ANY timeout, worker_reuse is unknown: a thread that stopped is not
evidence that what it was doing was undone, and cl-spec runs no cleanup this
adapter can observe. With a worker pool, use 'pool-kill-worker' before
trusting a later result in that session; running inline (MCP_NO_WORKER_POOL),
restart the process.

PREREQUISITE: load 'cl-spec/check-it' and the system defining the properties
with 'load-system' first.  After editing a definition, load-system again: this
tool runs what is in the worker image, and it is the same worker your
load-system call wrote to.

Examples:
  symbol='my-app::transfer'
  function='my-app::transfer'
  property='my-app::transfer-preserves-total'
  property='my-app::transfer-preserves-total', seed='3963993791726803706',
    profile='normal', expect_definition_digest='a41f9c2b7d0e5518'"
  :args
  ((property :type :string
    :description
    "One registered property to run. Exclusive with symbol and function.")
   (symbol :type :string
    :description
    "Run every property registered (:about <symbol>). Does NOT include the
symbol's function spec; the response names it when one exists.")
   (function :type :string
    :description
    "One registered function spec to run against its function. Exclusive with
property and symbol.")
   (trials :type :integer
    :description
    "Trial count for a contract run, a POSITIVE integer. Contract runs only: a
property takes its count from its own :trials table, selected by profile.
Given with property= or symbol= it is refused, not ignored.")
   (package :type :string
    :description "Package for an unqualified name (default: COMMON-LISP-USER)")
   (profile :type :string
    :description
    "Trial-count profile, e.g. 'normal' or 'smoke' (default: normal). Property
runs only -- a contract has no :trials table to select from and cl-spec's
check-function takes no profile, so it is refused with function=, the mirror
of how trials is refused with property=. Size a contract run with trials=.")
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
                         "function" function
                         "trials" trials
                         "package" package
                         "profile" profile
                         "seed" seed
                         "expect_definition_digest" expect-definition-digest
                         "timeout_seconds" timeout-seconds
                         "max_value_chars" max-value-chars)))
    (with-proxy-dispatch (id "worker/spec-check" params)
      (result id (spec-check-response params)))))
