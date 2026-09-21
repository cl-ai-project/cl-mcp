# cl-mcp/specs: executable contracts for cl-mcp's own code

`cl-mcp/specs` is an opt-in bundle of [cl-spec](https://github.com/masatoi/cl-spec)
Function Specs and Properties about a few of cl-mcp's own functions, a direct
runner that checks them through cl-spec's Lisp API, and a CI job that runs it.
It exists so that an agent changing one of these functions can read the
contract before editing, keep it while editing, and say afterwards exactly what
was checked and what was not.

A passing check is evidence about the inputs that were generated, under the
seeds and budgets that ran. It is not a proof, and a cl-spec type in `:args` or
`:returns` is a runtime check on those inputs, not a static type.

## What is covered

| Function | Function Spec (one per function) | Properties |
|---|---|---|
| `cl-mcp/src/utils/strings:ensure-trailing-newline` | ends in a newline, starts with the whole argument, at most one character longer | `ensure-trailing-newline-keeps-terminated-text` |
| `cl-mcp/src/utils/sanitize:sanitize-for-json` | three cases: `NIL` gives `NIL`; a string gives a string free of what the docstring says is stripped (C0 controls but tab/LF/CR, DEL, anything above U+FFFF) and no longer than the argument; an integer gives its printed form | `…-keeps-allowed-text`, `…-is-idempotent`, `…-removes-complete-escape-sequences`, `…-removes-truncated-escape-sequence`, `…-leaves-its-argument-unmodified` |
| `cl-mcp/src/utils/sanitize:sanitize-error-message` | a string of at most 500 characters, on one line, with no whitespace run and none at either end | `…-keeps-normalized-text`, `…-truncates-long-text`, `…-keeps-only-visible-words` |

Property names are in `cl-mcp/specs/strings` and `cl-mcp/specs/sanitize`. Each
Function Spec is registered on the production symbol itself.

The properties are chosen so that one check covers what another cannot. For
example, the removal clause in `sanitize-for-json`'s contract passes an
implementation that always returns `""`, and `…-keeps-allowed-text` does not.
Likewise, `ensure-trailing-newline-keeps-terminated-text` passes an
implementation that never adds a newline, and the Function Spec does not.
The negative control below runs both of those wrong implementations.

### Input domains

The generators build inputs as data (segment, escape-sequence and message
descriptors) and render them inside the property. A counterexample therefore
prints as readable data, not as raw control characters. Every draw uses
`cl:random`, which cl-spec binds from the run's seed, and conses fresh strings.
No generator reads the clock, files or a random state of its own, or calls the
function under test.

| Function | Generated | Deliberately not covered |
|---|---|---|
| `ensure-trailing-newline` | up to ~22 characters (printable ASCII, a few non-ASCII BMP characters, tab, CR) with newlines inside and one of five endings (none, LF, CRLF, LF LF, CR); simple strings, strings with a fill pointer hiding a newline, base strings | other element types, long strings, non-strings (excluded by the function's `ftype`) |
| `sanitize-for-json` | `NIL`; integers up to ±5·10^19; up to 8 segments of allowed text, forbidden C0 controls, DEL, U+10000/U+1F600/U+10FFFF, complete ECMA-48 sequences (CSI, OSC ended by BEL or ST, DCS/SOS/PM/APC ended by ST, ESC Fe), or a proper prefix of one | other non-string objects, C1 controls, **surrogates** (see *Known issues*), other Unicode ranges, sequences whose bytes stray outside ECMA-48's parameter/intermediate/final ranges |
| `sanitize-error-message` | `NIL`; integers; up to 10 words, `#P"..."` paths and `#<...>` representations separated by runs of space/tab/LF/CR/page, optionally followed by a `Stream:` section; single-spaced text of 0–40, 480–500 (499 and 500 often) and 501–1500 (501 often) characters; strings of fragments that misplace `#<`, `>`, `#P"`, quotes and `Stream:` | nested `#<...>` or ones containing `>`, words containing `Stream:` or `#`, whitespace outside cl-ppcre's `\s` |

Boundaries are also covered by fixed inputs, apart from generated trials:
`call-examples` in each spec file lists concrete calls that the runner checks
with `cl-spec:check-call`, and the default Rove suite has exact-value tests for
the 499/500/501-character limit, whitespace collapse, `Stream:` removal and a
CSI sequence cut off at the end of the input
(`tests/utils-sanitize-test.lisp`, `tests/utils-strings-test.lisp`).

## Dependencies

```
cl-mcp/specs ──> cl-mcp/src/utils/{strings,sanitize}
             ──> cl-spec/main, cl-spec/src/backends/check-it

cl-mcp (load, run, tests.lisp) ──X──> cl-mcp/specs, cl-spec
```

Nothing in `cl-mcp.asd`, `main.lisp` or `tests.lisp` refers to the bundle.
`cl-mcp` is a package-inferred system, so `cl-mcp/specs` (`specs.lisp`) and its
subsystems (`specs/*.lisp`) exist without any `.asd` entry. The runner's own
tests, `cl-mcp/tests/specs-runner-test`, are likewise left out of `tests.lisp`.

Loading `cl-mcp/specs` registers the bundle in `cl-spec:*registry*` and does
nothing else: no check runs, nothing is instrumented, and no server or worker
starts. The registration stays in that registry, which is where `spec-list` and
`spec-symbol` look.

## From an MCP client

The server needs the `cl-spec` tool group (`MCP_ENABLE_TOOL_GROUPS=cl-spec`).
If the `spec-*` tools are missing, check how the client discovered the tools and
whether it is connected before concluding the group is off. Load the systems
into your session's worker:

```text
load-system {"system": "cl-spec/check-it"}
load-system {"system": "cl-mcp/specs"}
```

Find, read and run:

```text
spec-list    {"kind": "function-specs"}
spec-symbol  {"symbol": "cl-mcp/src/utils/sanitize:sanitize-for-json"}
spec-describe {"kind": "function-spec", "name": "cl-mcp/src/utils/sanitize:sanitize-for-json"}
spec-describe {"kind": "property",
               "name": "cl-mcp/specs/sanitize::sanitize-for-json-removes-complete-escape-sequences"}
spec-check   {"function": "cl-mcp/src/utils/sanitize:sanitize-for-json", "trials": 200}
spec-check   {"symbol": "cl-mcp/src/utils/sanitize:sanitize-for-json", "profile": "normal"}
```

Things that are easy to get wrong here:

- A Function Spec is listed under the home package of the function it
  describes. `spec-list` with `package: "CL-MCP/SPECS/SANITIZE"` shows the
  properties and **no** Function Spec; filter by `CL-MCP/SRC/UTILS/SANITIZE`, or
  not at all.
- `function=` runs only the Function Spec and `symbol=` runs only the
  properties `(:about ...)` that symbol. Run both.
- `symbol=` selects several properties, so take that baseline without a `seed`.
  To re-check one, use `property=` with that result's own seed (a decimal
  string) and `expect_definition_digest`, copied from its `Replay:` line.
- `definition_digest` covers the declarations and the specs and generators they
  refer to. It does not cover the function body or the helper functions a
  predicate calls (`json-policy-clean-p`, `render-message`, ...). A matching
  digest does not show that your edit was loaded.

After editing a function or a spec file, reload before checking again. The
loader does not clear a package-inferred subsystem's FASLs (issue #167), so
clear the primary system and then load the bundle:

```text
load-system {"system": "cl-mcp", "clear_fasls": true}
load-system {"system": "cl-mcp/specs"}
```

Then re-check under the same seed and budget, and separately without a seed.
Only the worker is reloaded: `lisp-edit-form` and the other parent-side tools
keep running the parent server's image until the server restarts.

### Changing a function the bundle covers

1. Read the contract and the properties about the function
   (`spec-symbol`, then `spec-describe`).
2. Take a baseline: `spec-check function=` and `spec-check symbol=`, and the
   function's Rove tests with `run-tests`.
3. Edit, reload as above, and re-check the same selections: once with the
   baseline's seeds and digests, and once without a seed.
4. Report each call's `verification_gaps` as it gave them.

Do not weaken a contract to make a check pass. If a contract seems wrong,
stop and raise it (see `prompts/cl-spec-driven-development.md`).

### Rebuilding the registrations

`(cl-mcp/specs:register-specifications)` re-registers the whole bundle in
`cl-spec:*registry*`. `(cl-mcp/specs:register-specifications registry)` does the
same for a registry you pass in, such as `(cl-spec:make-hash-table-registry)`.
Registering again replaces each definition by name. It adds no duplicate names
or `:about` links and leaves other registrations alone, and
`tests/specs-runner-test.lisp` checks all three. A definition that you renamed or
deleted stays registered under its old name until the worker is replaced
(`pool-kill-worker`), after which you load both systems again.

## The direct runner

`cl-mcp/specs/runner` checks the bundle through `cl-spec:run-property`,
`cl-spec:check-function`, `cl-spec:check-call` and `cl-spec:result-data`. It
never goes through the MCP adapter. This is what CI judges by. Adapter-side
gaps such as `input-coverage-unmeasured` are always present in MCP output and
play no part here.

```lisp
(asdf:load-system "cl-mcp/specs/runner")
(cl-mcp/specs/runner:print-report
 (cl-mcp/specs/runner:run-bundle
  :expected-root (asdf:system-source-directory "cl-mcp")))
```

`run-bundle` registers the bundle into a fresh registry and first checks that
registry against the bundle's own listing (`contract-names`, `property-names`,
`spec-names`, `generator-names`). It then runs every target under the fixed
seeds `20260922`, `1` and `7777777`. These are Lisp integers; the same seed in
`spec-check` is the string `"20260922"`. Properties run at profile `:normal`,
from each property's `:trials` table (200 each). Function Specs run with 200
trials. Each target has a 120-second deadline per seed. A local run takes a few
seconds.

A run passes only when all of the following hold:

- at least one target was selected, and every target is registered;
- every run answered `:passed`;
- every run executed at least one trial, and not every trial was rejected;
- every declared `:cases` branch was called, with no case-selection or capture
  error;
- every `check-call` example passed and selected the case it names;
- cl-mcp and every contracted function were loaded from the expected checkout.

Everything else fails the run. That includes `:failed`, `:error`, `:skipped`, a
signalled condition such as a generator error, a timeout, a profile the
property does not declare, and a result schema the runner does not know.
(cl-spec itself would run an undeclared profile at its default budget and
report it under the requested name.)

The report records, per run: target, seed, profile or trials, status, trials,
budget, rejections, shrinking capability, digest and whether it is complete,
the case counts, and for a failure the counterexample, the shrunk
counterexample and a replay line for Lisp and for MCP. It also records the Lisp,
ASDF and backend, the version, directory, git revision and local changes of
cl-mcp, cl-spec and check-it, and the file each contracted function was loaded
from.

### From the command line

```sh
ros run --load scripts/check-specs.lisp                                   # check
CL_MCP_SPECS_MODE=self-test        ros run --load scripts/check-specs.lisp
CL_MCP_SPECS_MODE=negative-control ros run --load scripts/check-specs.lisp
CL_MCP_SPECS_REPORT=specs-check.sexp ros run --load scripts/check-specs.lisp
```

`sbcl --non-interactive --load scripts/check-specs.lisp` also works when
Quicklisp and cl-spec can be found without Roswell. The script puts its own
checkout first in ASDF's search. Exit status: `0` passed, `1` the checks ran and
something failed, `2` the script could not run them.

- `self-test` runs `cl-mcp/tests/specs-runner-test`. These tests use small
  fixtures, each registered in a registry made for that test. They check that
  the runner refuses a failing property, an empty selection, an unregistered
  name, zero trials, an undeclared profile, a contract that rejected every
  input, an unreached case, a generator error and a timeout. They also check
  registration, re-registration and a full bundle run. You can run the same
  tests with `run-tests system=cl-mcp/tests/specs-runner-test`.
- `negative-control` replaces `ensure-trailing-newline` with `identity` and
  `sanitize-for-json` with a function that returns `""`. It requires that the
  bundle's own Function Spec and properties answer `:failed` with a
  counterexample against these wrong implementations, pass again once the real
  functions are back, and use the same definition digests both times. It
  replaces global function definitions while it runs, so **run it only in a
  process of its own**, never in an MCP worker you are using. Catching a
  planted fault shows that these checks can fail. It does not measure an
  agent's diagnosis.

### Shrinking

With the check-it backend, a custom generator's `:shrink` clause is used only
when that generator produces a whole argument list. That is what the Function
Specs' `:args-generator`s do, so a contract failure shrinks: against the
negative control, `ensure-trailing-newline` shrinks to `""`. A property's
arguments are specs whose generators are nested, so property counterexamples
are reported unshrunk. `spec-describe` still says `shrinking: enabled`, because
that is the option the declaration carries. The run itself reports
`shrinking: :none`. An unshrunk counterexample is still a valid one.

## CI

The `specs` job in `.github/workflows/ci.yml` does the following:

1. Installs Roswell with `sbcl-bin/2.5.0`.
2. Fetches cl-spec at the commit pinned in `CL_SPEC_REVISION`. cl-spec is not
   in Quicklisp; bump the pin deliberately, in its own commit.
3. Checks that no other copy of cl-mcp is installed. The job does not run
   `ros install cl-ai-project/cl-mcp`, and the runner also fails when cl-mcp
   comes from anywhere but the checkout.
4. Runs `self-test`, `check` and `negative-control` as separate processes, each
   under `timeout 900` inside a 30-minute job, and uploads the report files.

check-it and cl-mcp's other dependencies come from the current Quicklisp dist;
the report records which one. The job uses no cache. The default `test` job is
unchanged and still needs no cl-spec. The Lint job covers `specs.lisp`,
`specs/*.lisp` and `scripts/*.lisp` as well as `src/` and `tests/`.

To reproduce the job locally, clone cl-spec at the pinned commit where
Roswell's local-projects can see it, then run the three commands above from
the checkout.

## Adding to the bundle

1. Pick a function whose behaviour can be stated independently of its code,
   with an input domain you can generate. A long branch that you would have to
   copy to compute the expected value is not a good first candidate.
2. Write the definitions inside a `register-specifications` function in
   `specs/<module>.lisp`, so that re-registration and fresh registries work. Add
   every name to that file's `contract-names`, `property-names`, `spec-names`
   and `generator-names`, and add the module to `specs.lisp`. The consistency
   check fails the run for any definition that is not listed.
3. Derive expected values from the requirement: preservation, idempotence,
   composition, boundaries, or a small independent rule. Never compute them
   with the function under test or its helpers.
4. Build inputs from structure rather than filtering them with `:pre`. Bound
   the sizes, and list what is covered and what is not in the file header.
5. Add boundary inputs to `call-examples` and, for exact values, to the Rove
   tests.
6. Run `self-test` and `check`, and make sure a wrong implementation of the new
   function fails a check. The negative control shows how.

Not every function needs a contract, and not every change needs the full
bundle run. Use it when a change touches a function the bundle covers.

## Known issues found while writing this

These are recorded here, not fixed in this change:

- `sanitize-error-message` does not fully remove a nested `#<...>`, or one with
  `>` inside a quoted name, although its docstring says it strips `#<...>`.
  `"x #<A #<B {1}> {2}> y"` gives `"x {2}> y"`, and
  `"x #<THREAD \"a>b\" {1}> y"` gives `"x b\" {1}> y"`. The generators leave
  both shapes out, and this file says so.
- `sanitize-error-message` removes `Stream:` wherever it appears, so
  `"UpStream: closed"` gives `"Up"`.
- `sanitize-for-json` passes lone surrogates (U+D800–U+DFFF) through. SBCL
  cannot encode them as UTF-8, and returning one from `repl-eval`, as in
  `(string (code-char #xD800))`, takes the MCP worker down. The docstring does
  not claim to handle them, so this bundle neither generates nor claims
  anything about them. Never put a surrogate in a generator: a counterexample
  carrying one crashes the worker that prints it.
