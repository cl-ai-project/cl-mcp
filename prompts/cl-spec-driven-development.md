# Common Lisp Development with cl-spec

## Scope

These are additional instructions for an agent that uses cl-mcp together with cl-spec to add features, fix bugs and refactor Common Lisp code.
They supplement the quality rules in `common-lisp-expert.md` and the operating rules in `repl-driven-development.md`. Follow those for file operations, structural editing, reading source and running ordinary tests.
Where they overlap, this document's reload and verification steps take precedence for anything a cl-spec check depends on: before an acceptance check, for example, reload with `load-system` (§6) rather than re-evaluating a form with `repl-eval`.

The goal is to **read or write the requirement as executable contracts and Properties, change the implementation on that basis, and keep what was confirmed apart from what was not**.
A cl-spec check is neither a replacement for ordinary tests nor a proof that the whole program is correct.

This is a day-to-day development procedure, not the dogfooding evaluation procedure. It does not require fault injection, limits on when you may read the implementation, or a ledger of every REPL call.

## Quick Reference

```text
load-system    cl-spec/check-it, then the application and its contract system
spec-symbol    symbol=<fn>                        what is registered about it
spec-describe  kind=function-spec name=<fn>       read the contract before editing
spec-check     function=<fn> trials=N             baseline: the Function Spec
spec-check     symbol=<fn>                        baseline: the Properties about it
run-tests      system=<test system>               baseline: ordinary tests
lisp-edit-form / lisp-patch-form                  change and save
load-system    <primary> clear_fasls=true, then the contract system
spec-check     <copied from the Replay: line>     same seed, same digest
spec-check     without seed; run-tests            widen
```

The rules that matter most:

1. Never weaken a contract to make a check pass. Changing an existing contract needs the user's confirmation (§1).
2. `symbol=` runs only the Properties about a symbol and `function=` only its Function Spec. Run both.
3. A seed is a decimal string. Replay by copying the `Replay:` line.
4. An edit reaches the worker only through `load-system`.
5. `verified` covers only what that call ran. Report `verification_gaps` as given.

## The Loop

```text
Confirm the requirement and the scope of the change
  → Find the contracts and related Properties / design the contracts you need
  → Check and record the state before the change
  → Change the implementation and save it to source
  → Reload the implementation and the contracts
  → Re-run the failing check under the same conditions
  → Widen verification to related Properties, Function Specs and ordinary tests
  → Report the change, the evidence and the remaining limits
```

When something fails, decide whether the cause lies in the implementation, the contract, the generator or the environment, and return to that step.

## 1. Handling Contracts

**In bug fixes and refactorings, keep the existing contracts.**
Do not make a check pass by narrowing the input domain, strengthening `:pre`, dropping a guarantee, narrowing a generator or removing a failing Property.

**Do not change an existing contract on your own authority.**
If a contract seems to contradict the requirement, stop before editing it and bring the conflict to the user: the requirement, the contract clause, and the counterexample or reasoning that shows they disagree.
Change the contract only after the user confirms. Until then the failure stays open; do not report the work as done.

The exception is a change the user's request itself asks for: a new feature, or an intentional change to the behaviour a contract states.
Then writing the acceptance criteria into contracts, Properties and ordinary tests is part of the task, and comes before the implementation. List every declaration you changed in the report.
Where the requirement leaves something undecided, state your assumption and ask; do not settle it silently in a contract.

Put declarations where they matter for this change: public boundaries, important business rules, state updates, properties that broke before. Covering every function is not the goal.
If the existing contracts are sufficient, do not add duplicate Properties just to tick a checklist.

## 2. Preparing the Session

Confirm the project root and read the schemas of the tools currently exposed.
The cl-spec tools are opt-in, and two different things can be missing:

- **The `spec-*` tools are absent from your tool list, or a call returns a JSON-RPC error naming the `cl-spec` group:** the server was started without the group (`MCP_ENABLE_TOOL_GROUPS=cl-spec` or the equivalent `tool-groups` setting). Ask the user to enable it and restart the server; nothing inside the session can fix this.
- **A call returns `cl-spec-not-loaded`:** the group is on, but cl-spec is not in the worker. Load `cl-spec/check-it`.

Load the following into the same session's worker with `load-system`:

```text
cl-spec/check-it       cl-spec including the execution backend
<application-system>   the implementation you are changing
<specification-system> the contract and Property definitions, if they are a separate system
```

Loading `cl-spec` alone is for introspection only; it does not prepare generated checks.
Find where the contracts are defined from the `.asd` and the package dependencies, and load that system. Do not guess system names such as `<application>/specs`.

An empty `spec-list`, `cl-spec-not-loaded` or `unsupported` does not by itself mean "this project has no contracts". Before concluding that, load `cl-spec/check-it` and the contract system, then check the `*_listable` flags, the filters and `limit`.

### Setting Up Contracts in a New Project

Keep the contracts in a file of their own that the application never depends on, so the production system does not pull in cl-spec and there is no cycle between implementation and contracts.
In a package-inferred project:

```lisp
;;;; src/contracts.lisp
(defpackage #:my-app/src/contracts
  (:use #:cl)
  (:import-from #:cl-spec/main
                #:defspec #:defspec-function #:defproperty #:defgenerator)
  ;; A bare :import-from declares the check-it backend as an ASDF
  ;; dependency without importing any symbol from it.
  (:import-from #:cl-spec/src/backends/check-it)
  (:import-from #:my-app/src/account
                #:account-p #:account-balance #:make-account
                #:insufficient-funds #:withdraw! #:transfer!))
(in-package #:my-app/src/contracts)
```

Load it by name, `load-system {"system":"my-app/src/contracts"}`; ASDF loads the implementation and the backend through the `:import-from` clauses, and no `.asd` edit is needed.
If `run-tests` should see the same definitions, add `"my-app/src/contracts"` to the test system's `:depends-on`, not the application's.
In a project that is not package-inferred, define a secondary system in the same `.asd` instead: `(defsystem "my-app/contracts" :depends-on ("my-app" "cl-spec/check-it") :components ((:file "contracts")))`.

## 3. Finding Contracts and Checking Them Against the Requirement

If you know the target symbol, start with `spec-symbol`. If not, look for candidates with `spec-list`.
Then read only the definitions you need with `spec-describe`.

| What you want to read | Tool and arguments |
|---|---|
| What is registered | `spec-list` |
| The target function's contract and related Properties | `spec-symbol symbol=...` |
| A Function Spec | `spec-describe kind="function-spec" name=...` |
| A Property's inputs, body and declaration | `spec-describe kind="property" name=...` |
| A named data spec | `spec-describe kind="spec" name=...` |

A Function Spec is named by its target function's symbol, and `spec-list`'s `package` filter matches the home package of each name. To find a Function Spec, filter by the target's package or leave `package` out; the package of the file that declares it does not match.
In `spec-symbol`, read both the definitions named by the symbol itself and the related Properties under `properties_about`.

In a Function Spec, read not only the types and shapes of the inputs but also `:pre`, `:returns` (including multiple values), `:post` or `:post-values`, `:signals`, `:cases`, `:capture`, `:state-post` and the argument generator.
In a Property, confirm which input domain it makes a claim about, and what relation it claims.
An omitted or truncated body is not the complete contract: raise `max_chars`, or read the declaration in its source file with `lisp-read-file`. Edit from the source file, never from a fragment of a tool's display.

`(:about ...)` is an explicit link, not a full impact analysis.
Use `code-find-references`, `clos-describe`, `clgrep-search` and reading the source to find callers, shared state, methods, macro uses and related tests.

### When Contracts Are Missing

Add the contracts this requirement calls for. Express the inputs, result and state transition of a single call as a Function Spec, a relation across several operations or functions as a Property, and concrete examples and integration paths as ordinary tests.

For a money transfer, for example, derive from the requirement the balance change on success, conservation of the total, and unchanged state on refusal, rather than just "returns a number".
Derive expected values from the requirement. An expected value computed by copying the current implementation, or a condition that is always true, is not evidence.

`:pre` marks inputs the function is not meant to be called with. An error path the specification requires you to check belongs in a `:signals` case, not behind `:pre`.
When the result differs by branch, consider `:cases`; for the relation between state before and after, consider `:capture` and `:state-post`.

Create mutable objects fresh for every trial and never carry state over from another trial.
Where there is external state, provide test setup, teardown and isolation. Worker isolation alone does not isolate side effects on an external database, files or the network.

### Writing Declarations

```lisp
;; A named data spec.
(defspec money (range integer 1 1000))

;; A mutable argument cannot come out of :args alone, and a trial must not
;; start from the object the previous trial mutated: build a fresh one per
;; trial.  cl-spec binds *random-state* from the run's seed, so cl:random
;; here still replays.
(defgenerator withdraw-arguments ()
  (list (make-account (random 1000)) (1+ (random 1000))))

;; A Function Spec is named by its target.  With :cases each branch declares
;; its own outcome; :capture records state before the call and :state-post
;; relates it to the state after.
(defspec-function withdraw!
  "Withdraw exactly AMOUNT when the balance covers it; otherwise refuse and
leave the balance unchanged."
  (:args (account (satisfies account-p)) (amount money))
  (:args-generator withdraw-arguments)
  (:capture (balance-before (account-balance account)))
  (:cases
    (:sufficient-funds
      (:when (<= amount balance-before))
      (:returns (type integer))
      (:state-post (= (account-balance account) (- balance-before amount))))
    (:insufficient-funds
      (:when (> amount balance-before))
      (:signals (type insufficient-funds))
      (:state-post (= (account-balance account) balance-before)))))

;; A Property states a relation; NIL or a signalled condition fails it.
;; :about links it to a symbol for spec-symbol and spec-check symbol=.
(defproperty transfer-preserves-total
    ((from-balance (range integer 0 1000))
     (to-balance (range integer 0 1000))
     (amount money))
  "A transfer moves money between accounts without creating or losing any."
  (:about transfer!)
  (:kind :invariant)
  (:trials (:smoke 20 :normal 200))
  (let ((from (make-account from-balance))
        (to (make-account to-balance)))
    (handler-case (transfer! from to amount)
      (insufficient-funds () nil))
    (= (+ (account-balance from) (account-balance to))
       (+ from-balance to-balance))))
```

Four rules, each of which otherwise costs a compile-error round trip:

1. `:cases` cannot be combined with a top-level `:returns`, `:signals`, `:post`, `:post-values` or `:state-post`. Each case has exactly one `(:when ...)` and exactly one of `:returns` or `:signals`; `:args`, `:args-generator`, `:pre` and `:capture` stay at the top level.
2. In a spec position, a bare symbol names a registered spec or a standard type such as `integer` or `string`. A user class or condition, or a compound type such as `(eql :done)`, needs a head: `(type ...)` or `(instance-of ...)`. Other heads include `satisfies`, `range`, `member`, `and` and `or`.
3. Case guards must be exclusive: once `:pre` admits an input, exactly one `:when` must hold. None or several is a `case-selection-error`, a defect in the contract that reproduces against correct code too. `(:when t)` is not an else branch.
4. `:signals` cannot coexist with `:returns` or `:post` at the same level, and `:post` and `:post-values` exclude each other.

When `:pre` refuses most generated inputs, write an `:args-generator` that produces admissible ones instead of raising `trials`.
For the full clause grammar, run `code-describe` on the macro once cl-spec is loaded (`cl-spec:defspec-function`, `cl-spec:defproperty`, `cl-spec:defspec`, `cl-spec:defgenerator`): each docstring lists its clauses. Existing declarations in the project are the next best reference. cl-spec's own README and examples are usually outside the project root, where the file tools refuse to read.

Edit declarations with `lisp-edit-form` or `lisp-patch-form` like any other form. `form_type` is the macro name without a package prefix (`defspec-function`, `defproperty`, `defspec`, `defgenerator`), even when the source writes `cl-spec:defspec-function`, and `form_name` is the defined name; for `defspec-function` that is the target function's name.
In an `.asd`, `form_type` is `"defsystem"`, never `"asdf:defsystem"`.

## 4. Recording the Baseline

Before changing the implementation, run the target's Function Spec, its related Properties and its related ordinary tests. You do not need to invent a kind of definition that does not exist, but make clear what exists and what you ran.

Choose the `spec-check` selection as follows.

| Argument | Runs | Budget |
|---|---|---|
| `function` | one Function Spec | `trials` |
| `property` | one Property | `profile` |
| `symbol` | the Properties registered `(:about <symbol>)` | `profile` |

**`symbol=` does not run the Function Spec. `function=` does not run the related Properties.**
When both exist, run them separately. Give exactly one of `property`, `symbol` and `function` per call.
`trials` with `property=` or `symbol=`, and `profile` with `function=`, are refused as `INVALID-ARGUMENTS`, not ignored.

The following show the shape of the arguments; adapt the names and budgets to the project.

```text
spec-check
  {"function":"my-app::transfer!","trials":100,"timeout_seconds":60}

spec-check
  {"symbol":"my-app::transfer!","profile":"normal","timeout_seconds":60}
```

`timeout_seconds` is the budget for the whole call, not for one trial or one Property. When you raise the trial count or widen the selection, revisit the time budget too.

Take the `symbol=` baseline without a `seed`. When the selection holds more than one Property, a seed is refused (`seed reproduces a single property run; the selection holds more than one property. Name one with property= instead.`).
After a failure, narrow down with `property=` using the individual seed each result carries.

Keep the following for the final report; there is no need to write them to a file.
The target name; the selection and budget actually used; each result's seed and definition digest; the outcome and counterexample; `verification_gaps`; and for a Function Spec, `effective_trials` and how often each case ran.
Also note the implementation's commit and uncommitted diff and the systems you loaded, so each result can be tied to the code it ran against.

For a bug fix, check whether the existing checks detect the reported problem. If needed, add a regression test grounded in the requirement to reproduce it.
If you cannot reproduce it, keep that fact. For a refactoring a passing baseline is enough; there is no need to manufacture a failure.
For a new function, register its contract before writing it. The baseline then reports `undefined-function` for that function: that is the expected red, not a fault to work around.

## 5. Locating the Failure

Read the individual results and the structured data available, not just the headline.

- **Implementation or Property failure:** read the counterexample, the expected and actual results and the failed condition, and decide whether the requirement was violated. A status of `error` means something signalled; read the condition and the failure phase before deciding whether it is a fault in the implementation.
- **Contract or generation problem:** tell ambiguous cases, failures while evaluating the contract, `skipped`, `generator-error` and the like apart from counterexamples in the implementation. Rejecting every input or failing to generate is not a pass.
- **Environment or execution problem:** handle not-loaded, undefined functions, unsupported schemas, timeouts and worker crashes each by its own cause. Do not resolve them by weakening the contract.

For a function that changes state, the printed arguments may show the state after the call. Read the values before the call from the collected capture evidence (`captured:`).

A Function Spec run stops at its first failing trial, so right after a failure the later cases show `NEVER CALLED`. That is expected; judge case coverage on the run after the fix (§8).

What each status, each `verification_gaps` value and each `core_result` field means, including fields such as `outcome` that read differently for a Function Spec and a Property, is defined in `spec-check`'s own tool description.
Read it there. This document does not repeat those lists, because they change.

If a shrunk counterexample exists, confirm that it is the same failure. A shrink that could not run, or ran out of budget, does not invalidate the counterexample already found.
If shrinking was refused because state cannot be restored, use the original counterexample. A shrunk counterexample is smaller, not necessarily minimal; do not call it minimal.

## 6. Changing, Saving and Reloading the Implementation

Check the source and the target definition, then make the change with `lisp-edit-form` or `lisp-patch-form`.
You may experiment in the REPL, but the final implementation, contracts and tests must always be saved to files.
Preserve not only the case of this bug but every other input, state and branch the contracts you read require.

**Editing a file does not update the definitions in the worker.**
After saving, reload the systems that define the implementation and the contracts with `load-system`.

Plain `force=true` recompiles only files whose source is newer than their FASL, so an edit made within the same second as the last compile can be missed.
For the acceptance check after a fix, use `clear_fasls=true`, which deletes every FASL under the source directory of the system you name.
Name the **primary** system, the one the `.asd` defines. A package-inferred subsystem such as `my-app/src/contracts` has no source directory of its own, so `clear_fasls=true` on it deletes nothing, while the response still reports that it cleared.
Then load the contract system, which the application does not depend on, to recompile and re-register the contracts.
If the contracts or another changed dependency live in a different project, clear that project's primary system too.

```text
load-system
  {"system":"my-app","clear_fasls":true}

load-system
  {"system":"my-app/src/contracts"}
```

Check the load result, the warnings and the registered contracts.
If you changed a declaration, confirm its new content and digest with `spec-describe`. If you changed only the implementation, an unchanged declaration digest is normal, and it is not proof that the reload succeeded.

When you delete or rename a definition or replace the registry, also check that no stale registration remains.
A reload replaces what it loads and leaves everything else in place, including old registrations and global state. When state must be rebuilt reliably, confirm what is saved and what you own, then recreate your own worker.

## 7. Re-verifying Under the Same Conditions

First re-run the individual failing Property or Function Spec with the recorded seed and the same budget.
The `Replay:` line at the end of a `spec-check` response gives the selection, seed, profile or trials, and digest for the first result that did not pass. Copy it. For any other result, build the call from that result's own seed and digest.
Pass the original result's digest as `expect_definition_digest`.

```text
spec-check
  {"property":"my-app/src/contracts::transfer-preserves-total",
   "profile":"normal",
   "seed":"42",
   "expect_definition_digest":"<the Property digest from the baseline>"}

spec-check
  {"function":"my-app::transfer!",
   "trials":100,
   "seed":"42",
   "expect_definition_digest":"<the Function Spec digest from the baseline>"}
```

The seeds above are examples. In practice use each result's value exactly as given, and pass it as a **decimal string**, not a JSON number.
Use each result's own seed and digest; they do not carry over to another result.

Read `definition_match` and `reproduction_faithful`.
Only `mismatch` says the declarations moved; a pass after a mismatch is a pass against a different contract, not evidence that the fix kept the original one.
`unknown` and `not-checked` are neither a match nor a mismatch: report such a replay as unconfirmed, not as either.

**A matching digest covers the declarations and their registered dependencies, nothing more.**
It does not cover the target function or helper implementations, external state or backend settings.
Re-running with the same seed regenerates the inputs; it does not restore the past environment or state, and it is not a rerun of a saved concrete counterexample.

After fixing the implementation, report it as "a regression check under the same contract and seed".
When a contract or generator declaration changed, whether with the user's confirmation (§1) or as part of a requested behaviour change, record why and the digest change, and take a new baseline.

## 8. Widening Verification

Once the individual re-check is done, widen to the set of related Properties, the Function Spec and the ordinary tests affected.
Passing with the same seed is only the first step: run again without a seed, which draws a new one, and with an appropriate profile or trial budget.
Confirm the profiles to use from the actual Property declarations.

For a Function Spec, check the effective trial count and the rejection count, not only the `trials` you asked for.
If there are `:cases`, check that each case was called. Even when the result is `passed`, do not claim a pass for a branch whose case is `NEVER CALLED`.

If almost every input is rejected, revisit how the generator matches the input domain before simply raising the budget.
When you improve the distribution, keep every region the contract must guarantee. If you change a generator declaration, record that change as a change to the reproduction conditions.

Run the relevant unit and integration tests with `run-tests`, and widen to the whole suite as needed.
`run-tests` runs the test framework's tests; a cl-spec definition is checked there only if a test calls it, so run `spec-check` as well. Conversely, a passing `spec-check` is not evidence that the ordinary tests were run.
`NO TESTS RAN` or an empty selection is not a pass: check the system name and the selection.

### Reading a Verdict

- Judge each result, not the call. A call that completed can still contain failures, and `verified` covers only what that call selected and ran, not the program.
- When `verified` is false, find the reason in `verification_gaps` and the per-result statuses instead of guessing.
- Do not make an empty `verification_gaps` your finish line. Some gaps are present on every run by design (see `spec-check`'s description); they name what was not measured, and running more will not remove them.
- Keep each call's gaps as that call reported them. When one call covers what another left out, such as a `function=` run for a `symbol=` run's `contract-not-run`, say so in the report. Do not delete the original gap, or describe one result as having checked both.

## 9. Auxiliary Operations Through the Public API, and Recovery

The cl-spec tools are exactly `spec-list`, `spec-symbol`, `spec-describe` and `spec-check`; use them and the ordinary cl-mcp tools first.
For an operation none of them provides, check the public API of the cl-spec in use and call it through `repl-eval`.

To check one arbitrary concrete argument list, consider `cl-spec:check-call` and `cl-spec:call-check-data`.
This is a separate check from re-running a generated test; do not merge its result into, or overwrite, the original `spec-check` tally or `verified`.
Keep any important regression example you find in source, as an ordinary test or a Property.

To use a saved counterexample artifact, check the inputs and constraints of that public API.
`make-counterexample-artifact` takes a live result object, not `spec-check`'s JSON counterexample.
`recheck-counterexample` executes only with `:state-policy :stateless`, which asserts that external state needs no restoration; it restores nothing, and a counterexample whose target mutates its inputs is unsupported. For a stateful contract, replay with `spec-check` and the seed instead.

After a `spec-check` timeout, `worker_reuse` is unknown; stop and recover before any further check in that session.
With a worker pool, confirm that everything that needs saving is saved, then replace your own worker with `pool-kill-worker`.

Whenever the worker is replaced, by `pool-kill-worker`, a crash or recovery after a timeout, the cl-spec registry is gone with it: it lives in the worker.
Load `cl-spec/check-it` and the application and contract systems again, rebuild any fixtures, and take the baseline again. A result from before the replacement is not comparable with one after it, and old object IDs do not exist in the new worker.

A live worker, an error returned as structured data, or a thread that was stopped does not mean side effects were rolled back.
Recover external state separately. If you cannot recover and re-run safely, report that scope as unverified.

Runtime instrumentation is not a requirement of this procedure.
If you adopt it, confirm its supported scope and its install/uninstall lifecycle separately; loading contracts does not by itself enable instrumentation or checking.

## 10. Completion Report

Report the changes and the evidence together, and list only checks that actually ran as having run.

Use the full form below when you changed a contract, fixed a failing check, or the user asked for it.
When every check passed both before and after the change, a short report is enough: what changed, each check that ran (selection, seed, outcome) and its `verification_gaps` as given.

```text
Changes:
  What changed in the implementation, contracts and tests, and why

Contracts:
  Guarantees kept
  Any declaration changed: the requirement behind it, the user's confirmation
  or the request that asked for it, and the old/new digest

Loading:
  Revision and uncommitted diff of the implementation verified
  Systems loaded, and confirmation of recompilation from source

Results:
  Individual Properties: name, profile, seed, digest comparison, outcome
  Function Specs: name, seed, effective trials, per-case execution, outcome
  Ordinary tests: what ran, how many, failures and pending
  Additional checks with other seeds and on affected code

Unverified and limits:
  Each call's verification_gaps, unaltered
  What another run covered, and which result it corresponds to
  Unreached cases, unmeasured input domains, external state, tests not run
```

Instead of "proved correct", state under which implementation, contracts, inputs and run conditions you confirmed what, and what remains.

## References

Tool schemas and the meaning of every result field are in the tools' own descriptions, which always match the running server.
This document is not a copy of them; it sets the order in which to choose operations and the criteria for judging results.

<!--
Maintenance notes, not instructions for the agent.

Checked against cl-mcp main c8343549b02afcb130d70acd529fa9be85e2fe79 (2026-09-21):
prompts/common-lisp-expert.md, prompts/repl-driven-development.md,
docs/tools.md (optional tool groups, spec-*, load-system, run-tests),
src/tools/spec-tools.lisp, src/tools/spec-response-builders.lisp (Replay: line),
src/system-loader-core.lisp (force / clear_fasls),
.claude/skills/dogfooding-cl-spec/SKILL.md (verification loop, observed pitfalls).

Checked against cl-spec 08d3adaf912b614537ca32cf5a9451fe0b807337 (2026-09-22):
README.md (blob 6068cfe00a7ee0deb704ef645d487cdc1de1e4eb) for systems, check-call,
counterexample artifacts and instrumentation; src/dsl.lisp docstrings for §3.

Run in a worker on 2026-09-22: the §3 declarations (pass, and fail on a planted
bug), the §2 package-inferred layout, the §4 refusals, undefined-function on a
contract registered before its function, and §6 clear_fasls on the primary
system (recompiles) against a package-inferred subsystem (deletes nothing).
-->
