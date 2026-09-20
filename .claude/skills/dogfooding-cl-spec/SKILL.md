---
name: dogfooding-cl-spec
description: Use when you want feedback on cl-mcp's cl-spec tool group (spec-list, spec-symbol, spec-describe, spec-check), when deciding whether a proposed spec-* tool is worth building, or when measuring whether an agent can repair code without breaking a declared contract.
---

# Dogfooding cl-spec

## Overview

Repair a bug in a throwaway project that carries cl-spec contracts, using only
the four `spec-*` tools plus the ordinary cl-mcp tools. Record every place the
first-class tools were not enough and every response field that was misread.

**Core principle:** the question is not "which tools are missing" — it is
**can an agent read a contract, change the code, and tell verified from
unverified**. A missing operation is a measurement (how often, how painful),
not a conclusion. Three to five recorded cycles decide what gets built; one
frustrating moment does not.

This differs from `dogfooding-cl-mcp`, which measures tool-surface breadth.
Here the project is small and the *epistemics* are the subject: what the run
proved, what it did not, and whether the agent claimed more than it measured.

## When to Use

- User asks for cl-spec dogfooding, spec-driven development feedback, or
  "try the spec tools on a real task"
- A new `spec-*` tool (`spec-check-call`, `spec-impact`, `spec-coverage`, …) is
  being considered and nobody has measured whether the `repl-eval` fallback is
  actually painful
- You changed the adapter (`src/spec-adapter-*.lisp`, `src/tools/spec-*.lisp`)
  and want evidence beyond unit tests

**Do NOT use** for: exercising the whole cl-mcp tool surface (use
`dogfooding-cl-mcp`), or for a project the user intends to keep.

## The Iron Rule of this skill

**Do not implement a new tool during a dogfooding cycle.** Not `spec-check-call`,
not `spec-coverage`, not a convenience wrapper. The cycle's output is a ledger,
and a cycle that "fixed" its own friction measured nothing. Adding tools is a
separate PR, after three cycles, and only for friction classed C or D
(see Fallback ledger).

`repl-eval` is an allowed escape hatch. **Using it without recording it is the
one way to fail a cycle outright.**

## Setup

Scaffolds live under `experiments/` inside the cl-mcp checkout (gitignored, so
generated files never reach `git status`). The feedback file is
`claudedocs/dogfooding-cl-spec-feedback.md` (also gitignored) unless the user
named another — **append, never overwrite**.

```
fs-set-project-root path=<cl-mcp checkout>
ToolSearch select:mcp__cl-mcp__spec-list,mcp__cl-mcp__spec-symbol,mcp__cl-mcp__spec-describe,mcp__cl-mcp__spec-check,mcp__cl-mcp__load-system,mcp__cl-mcp__repl-eval,mcp__cl-mcp__lisp-edit-form,mcp__cl-mcp__lisp-patch-form,mcp__cl-mcp__lisp-read-file,mcp__cl-mcp__fs-write-file,mcp__cl-mcp__run-tests,mcp__cl-mcp__project-scaffold,mcp__cl-mcp__inspect-object,mcp__cl-mcp__code-describe,mcp__cl-mcp__code-find-references,mcp__cl-mcp__pool-kill-worker
```

Hydrate the schemas first: `spec-check`'s `trials` is an integer and
`seed` is a **string**, and an unhydrated deferred tool rejects both with a
misleading type error (harness-side, not cl-mcp).

The tool group must be enabled in the **server's** environment
(`MCP_ENABLE_TOOL_GROUPS=cl-spec`). If `spec-list` comes back as a JSON-RPC
error naming the group, the running server was started without it — that is a
restart, not a bug to record.

Then, in the session's worker:

```
load-system system=cl-spec/check-it     # execution backend; plain cl-spec is introspection only
load-system system=<project>            # registers the project's own contracts
```

## Build the subject

Scaffold with `project-scaffold destination=experiments`, then write **one
contracts file** that the implementation does not depend on, and add it to the
`.asd` `:depends-on`. Verified wiring for a package-inferred project:

```lisp
(defpackage #:my-app/src/contracts
  (:use #:cl)
  (:import-from #:cl-spec/main
                #:defspec #:defproperty #:defspec-function #:defgenerator)
  ;; Bare :import-from — declares the check-it backend as an ASDF dependency
  ;; without importing a symbol from it.
  (:import-from #:cl-spec/src/backends/check-it)
  (:import-from #:my-app/src/account
                #:account-balance #:account-p #:make-account
                #:insufficient-funds #:withdraw!))
(in-package #:my-app/src/contracts)

(defspec small-amount (and integer (range 0 1000)))

;; A struct cannot come out of :args alone, and a trial must not start from the
;; object the previous trial mutated: build a fresh one per trial.  cl-spec
;; binds *random-state* from the run's seed, so cl:random here still replays.
(defgenerator withdrawal-arguments ()
  (list (make-account (random 1000) 7) (1+ (random 1000))))

(defspec-function withdraw!
  "A successful withdrawal reduces the balance by exactly the amount."
  (:args (account (satisfies account-p)) (amount (range integer 1 1000)))
  (:args-generator withdrawal-arguments)
  (:capture (balance-before (account-balance account))
            (id-before (account-id account)))
  (:cases
    (:sufficient-funds
      (:when (<= amount balance-before))
      (:returns (satisfies listp))
      (:state-post (= (account-balance account) (- balance-before amount))
                   (eql (account-id account) id-before)))
    (:insufficient-funds
      (:when (> amount balance-before))
      (:signals (type insufficient-funds))
      (:state-post (= (account-balance account) balance-before)))))

(defproperty total-money-preserved
    ((from-balance small-amount) (to-balance small-amount) (amount small-amount))
  "A transfer moves money without creating or losing any."
  (:about transfer!) (:kind :invariant) (:tags :money) (:trials (:normal 200))
  ...)
```

Richer patterns: cl-mcp's own `tests/fixtures/spec-fixture-contracts.lisp` —
inside the project root, so `lisp-read-file` reads it. cl-spec's own
`examples/*.lisp` are richer still but live **outside** the root, and the file
tools answer `Read not permitted` there. That boundary is cl-mcp's, and this
skill is measuring cl-mcp: **do not route around it with `cat`.** Use the
in-root fixture, and if an external reference turns out to be genuinely
necessary, that need is itself a feedback item — record it and say what the
in-root material failed to show.

**Four DSL rules, each of which otherwise costs a compile-error round trip:**

1. `:cases` **cannot** be combined with a top-level `:returns`, `:signals`,
   `:post`, `:post-values` or `:state-post`. With cases, every outcome clause
   goes **inside** a case. `:args`, `:pre` and `:capture` stay at the top.
2. An outcome spec needs a spec head. `(:returns (eql :touching))` is refused —
   write `(:returns (type (eql :touching)))`. The heads are `type`,
   `instance-of`, `satisfies`, `range`, `member`, `and` / `or`.
3. **Overlapping guards are a contract defect, not a fixture.** When two
   `:when` guards both hold for one trial, cl-spec signals a
   `case-selection-error` of kind `:ambiguous-case`; that fails the trial, and
   a contract run stops at its first failure — so every case after the
   ambiguity stays `NEVER CALLED` and the run says nothing about them. It
   reproduces identically on the same seed whether or not the implementation is
   correct. If you plant one deliberately, expect the run to end there, and
   treat disambiguating the guards as an edit to the **contract** (the digest
   moves — say so in the report), not as turning a red run green.
4. Adding the contracts file to the `.asd`: `lisp-patch-form` /
   `lisp-edit-form` match `form_type: "defsystem"`, never `"asdf:defsystem"`.

Authoring these declarations is the most expensive phase of a first cycle
(measured: ~20 of ~60 tool calls). When `load-system` reports a compile failure
whose condition is cut off (`[N more characters truncated]`), the actionable
sentence is usually in the tail. Reproduce the **same** load path rather than a
different one:

```
repl-eval max_output_length=20000
  code=(handler-case (asdf:load-system "my-app" :force t)
         (error (c) (princ-to-string c)))
```

`compile-file` on the single file is the second resort — it skips the
package-inferred dependency load, so it can fail differently or not at all.
Either way, record it as a ledger entry: it is friction.

### The fault, and who is allowed to know it

A cycle measures one of two different things, and mixing them invalidates the
stronger claim.

```text
Fault source:
  workflow cycle:
    the same agent MAY plant the fault.  Tool friction, response legibility and
    the ledger are scored.  "The bug was located from the evidence" is NOT
    scored -- the agent already knows the answer.

  repair-quality cycle:
    the fault MUST be prepared outside the repairing agent's context, by one of
      - a pre-authored faulty fixture under experiments/, written in an earlier
        session and left there,
      - a deterministic fault-injection script that applies one patch from a
        set the repair agent never reads, or
      - a separate setup session (or subagent) that plants it and reports only
        "planted".
    Only here may diagnosis sufficiency be scored, and the repair agent must
    not be told which function, file or clause was touched.
```

Either way the fault must be one the contract can see but a hand-written unit
test plausibly misses: a state mutation of the wrong size
(`(decf balance (* amount 2))`), a guard that admits one value too many, an
error signalled after a partial update. Say in the report which cycle type was
run — a workflow cycle that claims a diagnosis result is the one way to produce
a confident wrong conclusion about cl-spec.

**Vary the task between cycles.** Repeating bank-account measures the skill, not
the tools. Pick a different shape each time: `:pre`-heavy pure function ·
disjoint `:cases` over a boundary · `:capture` + `:state-post` on a mutable
object · a property over two functions that must agree · a `:pre` that refuses
every generated input (`skipped` — **not** passed) · a generator that cannot
produce a value (`generator-error`, with `generation-incomplete` in the gaps —
not a verdict at all) · overlapping `:cases` guards — an advanced shape that
measures the `:ambiguous-case` path rather than the implementation, so pair it
with a property (see DSL rule 3).

Those last two are different outcomes and the difference is worth one cycle:
`skipped` means nothing was admitted, `generator-error` means nothing was
drawn.

## The forced loop

Run the phases in order. Skipping one is a failed cycle, not a shortcut.

| Phase | Tools | The one thing to record |
|---|---|---|
| 0 Declare | `fs-write-file`, `load-system` | How many round trips the contract took to compile, and which rule you had to rediscover |
| 1 Discover | `spec-list`, `spec-symbol` | Could you pick the right property from the listing alone, or did you have to read bodies? |
| 2 Understand | `spec-describe`, `code-describe`, `code-find-references`, `lisp-read-file` — **contracts, signatures and callers only; not the target's body** | Was the response too long / too short to decide what the contract permits? |
| 3 Baseline | `spec-check` (property **and** contract), `run-tests` | The seed, the digest and the exact `verification_gaps` — before any edit |
| 4 Diagnose + repair | **now** read the target body: `lisp-read-file`, then `lisp-patch-form` / `lisp-edit-form` | Did the failure evidence tell you where to look, or did you have to scan unrelated code? |
| 5 Reload | `load-system` | How did you confirm the contract was re-registered, not just the code reloaded? |
| 6 Focused recheck | `spec-check` with the **same seed and `expect_definition_digest`** | `reproduction:` and `(match)` / `(mismatch)` — verbatim |
| 7 Broad verification | `spec-check symbol=` and `function=`, `run-tests` | For a `:cases` contract: the per-case called / passed / never-called counts before and after. Otherwise: `effective_trials`, the `rejected` count and `verification_gaps` |
| 8 Report | — | The cycle type, the ledger, and what you did **not** verify |

Two orderings matter, for the same reason:

- **Never repair before phase 3.** A fix applied before the baseline destroys
  the only evidence that the contract could see the fault at all.
- **Never read the target's body before phase 4.** The contract states what the
  function must do; the evidence says where it did not. Reading the
  implementation first replaces both with ordinary code review, and the cycle
  then measures nothing that cl-spec is for. The rest of the file — other
  functions, callers, the tests — is fair game in phase 2.

## Fallback ledger

Every operation that `repl-eval` had to do gets one entry. The class is the
whole point — it is what decides whether a tool gets built.

```text
Operation:     Check ONE argument list I chose myself, not one a seed generates
Desired path:  first-class cl-spec MCP tool
Actual path:   repl-eval -> cl-spec:check-call -> cl-spec:call-check-data
Outcome:       succeeded (1 call, first try)
Friction:      manual package qualification; result is a Lisp plist printed with
               #1= sharing labels; the :arguments entry shows the struct AFTER
               mutation; one object-id for the whole plist, none per value
Class:         B
Severity:      P3
```

Name the operation precisely, because two different things read alike.
Re-running a **generated** run is `spec-check` with `seed=` and
`expect_definition_digest=` — first-class, no fallback, no ledger entry.
Checking an argument list **you** chose has no tool, and that is the entry
above. Filing the first as a fallback inflates the case for a tool nobody needs.

| Class | Meaning | Consequence |
|---|---|---|
| A | `spec-check` alone was enough; the fallback was not needed | No tool. Remove it from the wish list. |
| B | `repl-eval` worked, first or second try | No tool yet. Record and move on. |
| C | Possible via `repl-eval` but repeatedly painful — wrong argument shape, several retries, hard-to-read result | **Candidate.** Needs 3 cycles of Class C before a PR. |
| D | Not realistically possible via `repl-eval` | Candidate now; say what made it impossible. |

Known Lisp-level fallbacks (all exported, all callable from `repl-eval` in the
same worker):

- One concrete invocation: `cl-spec:check-call` + `cl-spec:call-check-data`.
- A **saved** counterexample: `cl-spec:make-counterexample-artifact` +
  `cl-spec:recheck-counterexample` (execution requires
  `:state-policy :stateless`).

The second one has a constraint worth measuring rather than working around.
`make-counterexample-artifact` takes a **live `property-result` object**
(`check-function`'s `function-check-result` is a subclass of it, so both
runners qualify) — **`spec-check`'s JSON counterexample cannot be handed to
it.** So the whole artifact path has to start and end in `repl-eval`:

```text
repl-eval: run-property / check-function -> live result
           -> make-counterexample-artifact -> serialize-counterexample-artifact
keep the serialized string outside the image
   ... repair, load-system ...
repl-eval: deserialize-counterexample-artifact
           -> recheck-counterexample :state-policy :stateless
```

Which means a cycle that wants a saved counterexample cannot use `spec-check`
for the run at all. If that recurs across cycles, **that** is the argument for
`spec-recheck-counterexample` — record it each time with the number of
`repl-eval` calls it cost.

A ledger entry is not the same as a feedback item. Also record, as P1/P2/P3
under a dated heading (`## Session YYYY-MM-DD — <project>`):

- a response field you **could not interpret** (name the field)
- a decision where you **needed information no tool gave** (name the decision)
- output that was **too long to use** or **too short to act on**
- a moment you were **about to claim "verified"** and a gap stopped you —
  or one where nothing stopped you and you had to catch it yourself

## Success criteria

One cycle is complete when all of these are true:

- [ ] At least one Function Spec was read with `spec-describe kind=function-spec`
- [ ] At least one property related to the changed symbol was found before editing
- [ ] `spec-check` **detected the fault** — with a counterexample, not a
      crash. Either selection counts; if the contract could not reach it (an
      ambiguous case set, a generator that drew nothing), say which one did and why
- [ ] The repair went through `lisp-edit-form` / `lisp-patch-form`
- [ ] The target's body was not read before phase 4
- [ ] The focused recheck used the **same seed**, and `reproduction:` was read
- [ ] Both the property selection and the contract were run, plus `run-tests`
- [ ] `verification_gaps` was quoted in the report, and no claim exceeded it
- [ ] At least one of `:state-post`, `:cases` or `:pre` was exercised
- [ ] Every `repl-eval` fallback has a ledger entry with a class
- [ ] A dated feedback section was **actually appended to disk** — even if it
      says "no new friction" — and the append was verified by reading the tail
      back. "I'll record it at the end" is how the last skill lost its findings

Scenario-dependent, to be claimed only when the cycle was built for it:

- [ ] **Repair-quality cycle only:** the fault came from outside this agent's
      context, and the bug was located from the counterexample and failure
      evidence rather than from a scan of the implementation
- [ ] **Multi-property cycle only:** two or more properties about one symbol,
      so the `symbol=` baseline and the per-property seeded replay both got
      exercised

Do not manufacture a second property just to tick a box — a fixture shaped to
the checklist measures the checklist. A cycle that found no friction is a
reportable result: say so, and say which operations went through first-class
tools only.

## Optional: the two A/B cycles

Two different comparisons, often confused. Each needs the same fault, prepared
outside both agents' contexts (see "The fault, and who is allowed to know it"),
and two fresh sessions.

**Adapter A/B — is first-class MCP exposure worth it?**

```text
A: the contracts are present in the project, spec-* forbidden,
   ordinary cl-mcp only (the agent may still read contracts.lisp and call
   cl-spec through repl-eval)
B: the same project, the same contracts, spec-* enabled
```

This is the cheaper of the two and the one that speaks to **cl-mcp's** tool
group — not to cl-spec. Do not report it as evidence about cl-spec.

**Framework A/B — does cl-spec reduce wrong repairs?**

```text
A: the repair agent gets ordinary tests only; the contracts are not in the
   project and not available to it
B: the same implementation fault, with contracts + spec-* available
```

This is the one that addresses the original hypothesis. It is also the more
expensive to set up honestly, because A's project must not contain the
declarations at all — not merely be forbidden from reading them.

Metrics for either: tool calls, tokens, retries, wrong edits, test runs before
success, regressions introduced, completion. Run one of these every few cycles
rather than never, and name which of the two you ran.

## Known pitfalls

Measured, not guessed. Cite these instead of filing duplicates.

| Symptom | Cause | What to do |
|---|---|---|
| `spec-list package=<contracts package>` reports `0 function specs` although one exists | A Function Spec is named by its **target** symbol, so `package=` filters on the target's home package, not the file that declares it | `spec-list kind=function-specs` with no `package`, or filter by the target's package |
| `spec-symbol` on the function you are about to change says `function spec: none` / `property: none` | Those lines cover registrations **named by** that symbol; properties about it are the separate `Properties about this symbol (N)` block | Read both blocks. `properties_about` is `(:about ...)` only — not change-impact analysis |
| `spec-check symbol=<fn>` is green and the contract never ran | `:about` selects properties only; a Function Spec runs only under `function=` | Run both. The headline, `selection.contract_not_run` and `verification_gaps` each say so |
| Contract run reports `trials: 1 executed of 100 budget` and `1 declared case never reached` | A contract run stops at the first failure, so later cases are unreached — `cases-never-called` is the gap | Not a pass for those cases. After the fix, check the per-case counts (`sufficient-funds 47 called \| insufficient-funds 53 called`) |
| The counterexample's argument shows a state the input never had (`ACCOUNT = #S(... :BALANCE -118 ...)`) | Arguments are printed after the call, so a mutated struct shows its **post-call** state | The pre-call values are under `captured:`. Read those, never the printed argument |
| `shrinking: state-restoration-unavailable` on a stateful contract | Nothing restores observed state, so no candidate may call the target again | Expected. The unshrunk counterexample is the evidence |
| `✓ VERIFIED (the definitions moved since the digest given -- this did NOT reproduce that run)`, `reproduction: unfaithful` | You edited the property or contract, not only the implementation, so the digest moved | A green headline here is **not** a reproduction. Re-baseline with the new digest, and say in the report that the contract changed |
| `verification gaps: rejection-counts-unmeasured, input-coverage-unmeasured` on a passing run | Ordinary for a property run: they name what was not measured, not something that failed | Quote them. `verified: true` is scoped to the trials that ran and to the inputs the generator drew |
| You are about to compose a replay call by hand | Every `spec-check` response ends with a `Replay:` line carrying the seed, profile and digest | Copy it verbatim. The seed is a decimal **string** in JSON; a number is refused |
| `spec-check symbol=<fn> seed=<n>` is refused: `seed reproduces a single property run; the selection holds more than one property. Name one with property= instead.` | A seed identifies one run, and `:about` selected several | Baseline with `symbol=` and no seed to see which property fails, then replay that one with `property=`. Two properties about one symbol is the normal case, so expect this on the first baseline |
| Contract run ends at a `case-selection-error` of kind `:ambiguous-case` | Two `:when` guards held for the same trial. Not an implementation fault — it reproduces on the same seed against correct code too | See DSL rule 3. Disambiguating is a contract edit (the digest moves); a property with no case mechanism is the way to still measure the implementation |
| `lisp-read-file` / `fs-read-file` answer `Read not permitted` for cl-spec's own `examples/*.lisp` | Those files are outside the project root, which the file tools guard on purpose | Read the in-repo `tests/fixtures/spec-fixture-contracts.lisp` instead. Do not reach for `cat`: routing around the boundary you are dogfooding turns a measurement into a workaround. If the in-root material really is not enough, record that as feedback |
| `load-system`'s compile error ends in `[N more characters truncated]` | The response caps the condition's length, and the actionable sentence is often in the tail | Re-run the same load path: `repl-eval max_output_length=20000 (handler-case (asdf:load-system "<system>" :force t) (error (c) (princ-to-string c)))`. `compile-file` on the one file is the second resort — it skips the package-inferred dependency load and may fail differently. Record it: it is a ledger entry, not a detour |
| Every registration is gone and `spec-list` is empty after a worker reset | `pool-kill-worker`, a crash or a `spec-check` timeout replaces the worker, and the registry lives in it | `load-system cl-spec/check-it` then the project, and re-run the baseline. A run whose worker changed mid-cycle is not comparable evidence |
| `spec-*` returns `cl-spec-not-loaded` | The tools resolve cl-spec at call time in the session's worker | `load-system cl-spec/check-it` first — and again after a worker crash. Not evidence that a symbol has no contract |
| `skipped` in a per-property result | A `:pre` refused every generated input, or the trial budget resolved to zero. **Not** "the generator ran out" — that is `generator-error`, with `generation-incomplete` in the gaps | **Not a pass.** Raise `trials` if admission is merely rare; otherwise align the generator with the precondition, preferably with `:args-generator`, instead of buying more trials that will also be refused |

## Anti-patterns

- **Building the tool you wished for mid-cycle.** See the Iron Rule.
- **Using `repl-eval` silently.** An unrecorded fallback is the one failure mode
  that makes the whole cycle worthless.
- **Reading the implementation before the counterexample.** The measurement is
  whether the evidence was sufficient; reading the code first erases it.
- **Reporting `✓ VERIFIED` as "the code is correct".** It means: not falsified,
  over the inputs drawn, under the definitions as they stand now.
- **Same project every cycle.** Vary the contract shape, or you measure habit.
- **Fixing the property to make the run green.** That changes the claim, not the
  code. A declaration that is genuinely wrong — an ambiguous case set, a
  generator that admits nothing — is its own finding: fix it as a contract edit,
  report the digest move, and keep the implementation's verdict separate.

## Output when a cycle completes

1. **Cycle type and fault source:** workflow or repair-quality, and where the
   fault came from. State it first — it is what the rest may be read as
   evidence for
2. **Project:** name + path, and the fault in one line
3. **Contracts:** which Spec / Function Spec / Properties, which clauses
   (`:pre`, `:cases`, `:capture`, `:state-post`)
4. **Detection:** which selection caught it, the counterexample, the seed
5. **Repair and recheck:** the digest before and after, `reproduction:`, and
   the per-case counts (or `effective_trials` and `rejected`, for a contract
   without cases)
6. **What was NOT verified:** `verification_gaps` verbatim, plus anything the
   selection left unrun
7. **Ledger:** each fallback with its class; the A/B/C/D tally
8. **Feedback:** count and P1/P2/P3 breakdown, appended and verified on disk
