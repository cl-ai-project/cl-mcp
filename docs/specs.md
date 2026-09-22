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
| `cl-mcp/src/utils/strings:ensure-trailing-newline` | ends in a newline, starts with the whole argument as it was before the call, at most one character longer, and leaves the argument as it was | `ensure-trailing-newline-keeps-terminated-text` |
| `cl-mcp/src/utils/sanitize:sanitize-for-json` | three cases: `NIL` gives `NIL`; a string gives a string free of what the docstring says is stripped (C0 controls but tab/LF/CR, DEL, anything above U+FFFF) and no longer than the argument; an integer gives its printed form | `…-keeps-allowed-text`, `…-is-idempotent`, `…-removes-complete-escape-sequences`, `…-removes-truncated-escape-sequence`, `…-leaves-its-argument-unmodified` |
| `cl-mcp/src/utils/sanitize:sanitize-error-message` | a string of at most 500 characters, on one line, with no whitespace run and none at either end | `…-keeps-normalized-text`, `…-truncates-long-text`, `…-keeps-only-visible-words` |
| `cl-mcp/src/utils/paths:allowed-read-path` | none (see *Read access*) | `read-allows-project-files-as-themselves`, `read-follows-dependency-registration`, `read-denies-unlisted-regions`, `read-judges-symlinks-by-their-target` |
| `cl-mcp/src/utils/paths:resolve-readable-path` | none | the same four |

Property names are in `cl-mcp/specs/strings`, `cl-mcp/specs/sanitize` and
`cl-mcp/specs/paths`. Each Function Spec is registered on the production symbol
itself. Each read-access property is `(:about ...)` both path functions.

The properties are chosen so that one check covers what another cannot. For
example, the removal clause in `sanitize-for-json`'s contract passes an
implementation that always returns `""`, and `…-keeps-allowed-text` does not.
Likewise, `ensure-trailing-newline-keeps-terminated-text` passes an
implementation that never adds a newline, and the Function Spec does not.
The negative control below runs both of those wrong implementations.

Every check that says the argument's text is kept compares against a copy
taken **before** the call. That copy is `:capture (before (copy-seq text))` in
a Function Spec, and a `copy-seq` ahead of the call in a Property. The argument
object itself may have been overwritten by the time the check runs. An
implementation that fills its argument with newlines and returns it passed
every clause that read the argument after the call. It also passed
`…-keeps-terminated-text`, because expected and actual were then one object.
The negative control now includes that implementation, and a `sanitize-for-json`
that overwrites its argument.

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

## Read access

`allowed-read-path` and `resolve-readable-path` decide whether cl-mcp may read an
existing file or directory. The properties in `specs/paths.lisp` check that
decision against this policy:

```
resolved target inside the project root                     -> allowed
resolved target inside a registered ASDF source directory   -> allowed
inside neither                                              -> denied
```

Roots are directories. Sharing a string prefix (`project/` and
`project-other/`) is not containment. Where a symlink sits does not matter;
where it leads does. So a link in the project to a registered dependency is
allowed, and a link in the project to an unlisted file is denied.

An allowed read must come back as an absolute pathname naming the very object
the case reaches. That is checked against the truename of what the fixture
created, so returning some other allowed file fails. A denied read must come
back as `NIL` from `allowed-read-path`, and as `resolve-readable-path`'s refusal:
a `simple-error` whose format control says "outside project root". Only that
refusal counts. Any other condition, from the call or from the fixture, fails
the trial. Each function is compared with the expectation separately.

The expectation comes from the case's descriptor alone:
`expected-read-decision` in `specs/path-fixtures.lisp` reads the region the
target is in and whether the fixture's system is registered. It never calls
the functions under test, `path-inside-p`, or ASDF's source-directory lookup.

These are properties, not Function Specs. The answer depends on the project
root, the ASDF registry and the filesystem as well as the argument. A contract
would need its generator to create files that outlive the call, or a `:post`
that calls the target again.

| Property | Checks |
|---|---|
| `read-allows-project-files-as-themselves` | project files and directories are allowed as themselves, by every spelling, with or without a root alias |
| `read-follows-dependency-registration` | a dependency place is denied, then allowed while its system is registered, then denied again; a project file stays allowed and an unlisted file denied throughout |
| `read-denies-unlisted-regions` | `outside/` and `project-other/` are denied, relative (`../`) and absolute, whether or not a dependency is registered |
| `read-judges-symlinks-by-their-target` | one symlink, to a file or to its directory, in the project or the dependency, leading to any region: allowed and returned as the target, or denied, by where it leads |

### Fixtures

Generators return printable descriptors only: regions, name parts, spellings
and link topologies. They create nothing. Each trial builds its own tree from
the descriptor inside the property, and removes it before the trial ends:

```
<tmp>/cl-mcp-read-spec-<pid>-<serial>-<time>/
  project/         the project root
  project-alias    a symlink to project/, when the case uses one as the root
  dependency/      its own .asd; registered as an ASDF system only when asked
  outside/         outside the project, never registered
  project-other/   shares project/'s name as a string prefix only
```

- `*project-root*` is bound for the body only. The parent server's root, the
  worker's global root and the current directory are not touched.
- The ASDF system is the fixture's own. It has a fresh name, and its `.asd` is
  in `dependency/` itself, so its source directory is `dependency/` and never
  the scratch root. Registration uses `asdf:load-asd`, removal
  `asdf:clear-system` of that name. No other system is removed or changed, and
  the tests check that every other registered system is still there as the
  same object. The fixture takes charge of removing its system before
  `load-asd` runs, not after it returns. ASDF registers the system as soon as
  its `defsystem` is evaluated, so a load that fails later, or is cut short by
  a deadline, is still undone; cleanup asks the registry itself whether the
  system is there. The cl-spec registry and the ASDF registry are separate: a
  fresh cl-spec registry isolates neither ASDF nor the filesystem.
- Before building anything, the fixture refuses to run if a registered
  system's source directory contains the temporary directory. Such a system
  would make `outside/` readable for a reason the case does not model. The
  refusal is an error, so the trial fails; it is not counted as a pass.
- Cleanup removes exactly what was created, newest first: it unlinks files and
  links and removes directories one at a time. It never recurses and never
  follows a link. A failure to remove something signals
  `read-fixture-cleanup-error` after a normal exit, which fails the trial.
  When the body is already unwinding from its own condition, the failure is
  signalled as a `read-fixture-cleanup-warning` instead, so the body's
  condition goes on. The runner keeps that warning, with the paths it could
  not remove, in the report.
  A process killed outright (SIGKILL, CI timeout) runs no Lisp cleanup, so its
  scratch directory stays under the temporary directory, recognisable by the
  `cl-mcp-read-spec-` prefix. CI throws the whole runner away afterwards.
- Temporary names come from the process id, a counter and the clock, never
  from `cl:random`. They do not disturb the seed's random stream. Re-running a
  seed rebuilds the same descriptors and topology under a different scratch
  path; it does not restore inodes or absolute paths.

### Domain

- Existing regular files and directories, up to two directories deep, named
  with plain, spaced, Japanese, dotted and bracketed parts (`x[1]`,
  `v[old] 2`). Arguments are built natively, never through the pathname
  reader, which would read brackets as wild.
- Spellings: relative to the project root (`../` for the other regions),
  absolute strings, natively parsed pathnames, and a `./` plus `d/../d` detour
  through a real directory. Directories are spelled without a trailing slash.
- At most one symlink per case: to a file, or to its directory, never to one
  of its own ancestors. The project root is given directly or as a symlink
  alias. `resolve-readable-path` runs with `:must-exist t` and `nil`.
- Not covered: writes, paths that do not exist, dangling links, loops, `..`
  after a symlink (see *Known issues*), races with the filesystem (TOCTOU),
  permissions and ACLs, hard links, mount namespaces, and Windows paths. These
  properties do not show that any MCP endpoint, such as `fs-read-file`, calls
  these functions the way it should.

The key cases also run as fixed Rove tests in the default suite
(`tests/path-specs-test.lisp`), so they do not depend on what a seed happens to
draw:

- every spelling, including names with spaces, Japanese and brackets;
- the region roots, and the project root given as an alias;
- a dependency before registration, while registered and after removal;
- `outside/` and `project-other/`, including `../project-other/p.lisp`;
- seven link topologies, each with and without a root alias.

The same file tests the fixtures themselves:
- the tree and registration are gone after a normal exit, after an error, and
  after a registration that failed inside the `.asd` once `defsystem` had run;
- cleanup does not follow a link out of the scratch tree;
- a failed cleanup fails the run, or arrives as a warning while an error
  unwinds;
- the environment check refuses a covering source directory.

Each property runs 12 trials at `:normal` and 3 at `:smoke`, far fewer than
the string properties, because every trial touches the disk and ASDF.

| Property | Per seed at `:normal`, native runner (fresh process) | Per seed, MCP worker (~200 systems registered) |
|---|---|---|
| project | 0.01 s | 0.02 s |
| dependency | 3.1–3.3 s | 6.6 s |
| unlisted regions | 0.5 s | 1.1 s |
| symlinks | 0.2–0.5 s | 1.1 s |

Nearly all of that time is the functions under test consulting ASDF for every
denied path (see *Known issues*).

## Dependencies

```
cl-mcp/specs ──> cl-mcp/src/utils/{strings,sanitize,paths}
             ──> cl-spec/main, cl-spec/src/backends/check-it

cl-mcp (load, run) ──X──> cl-mcp/specs, cl-spec
tests.lisp ──> cl-mcp/tests/path-specs-test ──> cl-mcp/specs/path-fixtures
               (no cl-spec; not the bundle)
```

Nothing in `cl-mcp.asd` or `main.lisp` refers to the bundle. `cl-mcp` is a
package-inferred system, so `cl-mcp/specs` (`specs.lisp`) and its subsystems
(`specs/*.lisp`) exist without any `.asd` entry. The runner's own tests,
`cl-mcp/tests/specs-runner-test`, are left out of `tests.lisp`. The default
suite does load `tests/path-specs-test.lisp` and the fixture library it uses,
`specs/path-fixtures.lisp`. Neither needs cl-spec or loads the bundle.

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

The read-access functions have properties and no Function Spec. `spec-symbol`
shows "function spec: none" for them, so there is nothing to run with
`function=`:

```text
spec-symbol  {"symbol": "cl-mcp/src/utils/paths:allowed-read-path"}
spec-describe {"kind": "property",
               "name": "cl-mcp/specs/paths::read-judges-symlinks-by-their-target"}
spec-check   {"symbol": "cl-mcp/src/utils/paths:allowed-read-path", "profile": "normal",
              "timeout_seconds": 300}
spec-check   {"property": "cl-mcp/specs/paths::read-denies-unlisted-regions",
              "profile": "normal", "seed": "<from its Replay: line>",
              "expect_definition_digest": "<likewise>"}
```

These run inside your worker. Each trial creates its scratch tree under the
worker's temporary directory, and registers its ASDF system in the worker's
image, which is the image that runs the functions under test. A pass there
checks those functions in that worker. It is not an end-to-end check of the
parent server's file tools.

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
2. Take a baseline: `spec-check function=` when the function has a Function
   Spec, `spec-check symbol=`, and the function's Rove tests with `run-tests`
   (for the path functions: `utils-paths-test` and `path-specs-test`).
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
from each property's `:trials` table: 200 for the string properties and 12 for
the read-access ones. Function Specs run with 200 trials. Each target has a
120-second deadline per seed. A local `check` takes about 20 s, most of it the
dependency-registration property.

A run passes only when all of the following hold:

- at least one target was selected, and every target is registered;
- every run answered `:passed`;
- every run executed at least one trial, with a rejection count from 0 to
  fewer than the trials;
- every declared `:cases` branch was called, with no case-selection or capture
  error;
- every `check-call` example passed and selected the case it names;
- cl-mcp and every covered function were loaded from the expected checkout.
  The covered functions are those with a Function Spec plus every `(:about ...)`
  target of the bundle's properties (`covered-functions`), so the path
  functions are checked although they have no contract.

Everything else fails the run. That includes `:failed`, `:error`, `:skipped`, a
signalled condition such as a generator error, a timeout, a profile the
property does not declare, and a result schema the runner does not know.
(cl-spec itself would run an undeclared profile at its default budget and
report it under the requested name.)

The report records, per run: target, seed, profile or trials, status, trials,
budget, rejections, shrinking capability, digest and whether it is complete,
the case counts, and for a failure the counterexample, the shrunk
counterexample and a replay line for Lisp and for MCP. It also records the Lisp,
ASDF and backend, and the file each contracted function was loaded from. For
cl-mcp, cl-spec and check-it it records the version, directory, git revision
and `git status` lines.

A file list alone cannot tell two edits of one file apart, so each run also
records, per `.lisp` or `.asd` file that differs from HEAD (untracked ones
included), git's hash of its current contents. A fingerprint over those hashes
is printed on the summary line. Two runs at the same HEAD with the same file
list but different uncommitted code get different fingerprints
(`cl-mcp/specs/runner:git-state`).

Anything a run writes to `*error-output*` is captured. The report shows how
many lines were written and the first one; the rest of the text is not kept.
The read-access runs make ASDF reload `.asd` files and warn hundreds of times,
and that would otherwise bury the report.

Warnings are recorded as data instead, without muffling them. The report has
one row per condition type, with a count and up to three distinct reports of
up to 2000 characters each. The reports are printed whole when the run failed
and cut to a line when it passed. This is how a read fixture's cleanup
failure, signalled while a property's own condition unwinds, survives next to
the ASDF noise. Neither the output nor the warnings play any part in a
verdict.

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

- `self-test` runs `cl-mcp/tests/specs-runner-test` and
  `cl-mcp/tests/path-specs-test`, and fails if either loads no test. The runner
  tests use small fixtures, each registered in a registry made for that test. They check that
  the runner refuses a failing property, an empty selection, an unregistered
  name, zero trials, an undeclared profile, a contract that rejected every
  input, a rejection count below zero or above the trials, an unreached case,
  a generator error and a timeout. They also check registration,
  re-registration, the printed and written reports, the worktree fingerprint
  (on a scratch git repository) and a full bundle run. You can run the same
  tests with `run-tests system=cl-mcp/tests/specs-runner-test`.
- `negative-control` swaps in eight wrong implementations, one at a time:
  - an `ensure-trailing-newline` that returns its argument unchanged;
  - one that overwrites its argument with newlines and returns it;
  - a `sanitize-for-json` that returns `""`;
  - one that overwrites its argument with `a`s and returns it;
  - an `allowed-read-path` that returns `NIL` for everything. The project and
    dependency properties must fail.
  - one that allows the project only, ignoring ASDF. The dependency property
    must fail.
  - one that allows any path written under the project root, wherever it
    leads, and otherwise defers to the real function. Only a denial can differ
    from the real function, and the symlink property must fail.
  - one that treats a string prefix of the project root as containment. Only
    the `project-other/` denial can catch it, in the unlisted-regions property.

  `resolve-readable-path` is not replaced; it calls `allowed-read-path` for
  its decision. Nothing is written to a path that should be denied.

  For each, it requires that the targets named for it answer `:failed` with a
  counterexample, that every target passes again once the real function is
  back, and that both runs use the same definition digests. It
  replaces global function definitions while it runs, so **run it only in a
  process of its own**, never in an MCP worker you are using. Catching a
  planted fault shows that these checks can fail. It does not measure an
  agent's diagnosis.

### Shrinking

With the check-it backend, a custom generator's `:shrink` clause is used only
when that generator produces a whole argument list. That is what the
`:args-generator`s of `sanitize-for-json` and `sanitize-error-message` do, so
their contract failures shrink. `ensure-trailing-newline`'s contract uses
`:capture`, which makes it state-observing, and cl-spec does not shrink a
state-observing contract. Its counterexamples are reported as found. That is
the price of comparing against a copy taken before the call. A property's
arguments are specs whose generators are nested, so property counterexamples
are reported unshrunk as well. `spec-describe` still says `shrinking: enabled`, because
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
   with the function under test or its helpers. When a check compares with the
   argument, compare with a copy taken before the call (`:capture` in a
   contract, `copy-seq` first in a property), never with the argument object
   after it.
4. Build inputs from structure rather than filtering them with `:pre`. Bound
   the sizes, and list what is covered and what is not in the file header.
5. Add boundary inputs to `call-examples` and, for exact values, to the Rove
   tests.
6. Run `self-test` and `check`, and make sure a wrong implementation of the new
   function fails a check, including one that overwrites its argument. The
   negative control shows how.

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
- `allowed-read-path` resolves `..` lexically before the filesystem sees the
  path, so `..` after a symlink cancels the link's name instead of leaving the
  link's target. Take `dlnk` in the project, a link to `outside/`: the OS
  resolves `dlnk/../project/x` to the project's `x`, but `allowed-read-path`
  returns `project/project/x`, a path that does not exist, and allows it
  because it lies under the project. The decision is made on the lexically
  resolved path. When that path exists, it is resolved and judged like any
  other. When it does not, as here, it comes back unresolved and reading it
  fails. The read-access generators never put `..` after a link, and the file
  header says so. This is recorded, not tested: whether `..` should follow the
  OS here is a policy question.
- Every denied path costs `allowed-read-path` a lookup of every registered ASDF
  system by name. That runs `find-system`, which reloads `.asd` files whose
  systems do not match their file names: check-it's, once cl-spec is loaded.
  In a worker with about 200 systems this took about 46 ms per denied path,
  with two ASDF warnings each time.
