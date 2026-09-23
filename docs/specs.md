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
| `cl-mcp/src/utils/paths:ensure-write-path` | none (see *Write access*) | `write-resolves-project-targets-without-creating`, `write-refuses-outside-and-absolute`, `write-follows-existing-links`, `writer-changes-only-the-expected-entries`, `write-preserves-safe-spellings` |
| `cl-mcp/src/fs:fs-write-file` | none | `write-refuses-outside-and-absolute`, `writer-changes-only-the-expected-entries` |
| `cl-mcp/src/spec-core-record:field-availability` | none (see *Record fidelity*) | `core-record-availability-separates-absence-from-nil`, `core-record-ignores-order-duplicates-and-unknown-keys` |
| `cl-mcp/src/spec-core-record:validate-versioned-record` | none | `core-record-validation-separates-ok-unsupported-malformed` |
| `cl-mcp/src/spec-core-record:project-record` | none | `core-record-projects-each-field-by-its-role`, `core-record-seeds-stay-decimal-text`, `core-record-ignores-order-duplicates-and-unknown-keys`, `core-record-reports-every-cut` |
| `cl-mcp/src/spec-core-record:project-core-record` | none | all five of those, and `core-record-validation-separates-ok-unsupported-malformed` |
| `cl-mcp/src/spec-adapter-report::%counts` (internal) | none (see *Verdicts*) | `check-verdict-counts-keep-every-status` |
| `cl-mcp/src/spec-adapter-report::%contract-plist` (internal) | none | `check-verdict-effective-trials-only-from-a-usable-count` |
| `cl-mcp/src/spec-adapter-report::%verified-p` (internal) | none | `check-verdict-verified-needs-evidence-from-every-result` |
| `cl-mcp/src/spec-adapter-report::%verification-gaps` (internal) | none | `check-verdict-gaps-name-each-shortfall-and-nothing-else`, `check-verdict-verified-needs-evidence-from-every-result` |
| `cl-mcp/src/spec-adapter-report::%target-argument-error` (internal) | none (see *Routing*) | `check-routing-target-arguments-are-exclusive` |
| `cl-mcp/src/spec-adapter-report::%resolve-profile` (internal) | none | `check-routing-target-arguments-are-exclusive` |
| `cl-mcp/src/tools/spec-entry:parse-seed-string` | none | `check-routing-seed-text-keeps-every-digit` |
| `cl-mcp/src/spec-adapter-report::%select-properties` (internal) | none | `check-routing-selection-names-only-what-was-asked` |
| `cl-mcp/src/spec-adapter-report::%trials-budget` (internal) | none | `check-routing-budget-comes-from-its-stated-source` |
| `cl-mcp/src/spec-adapter-report::%definition-match` (internal) | none | `check-routing-digest-comparison-has-four-answers` |
| `cl-mcp/src/spec-adapter-core:api-backend-available-p` | none (see *Inspection*) | `spec-inspection-operations-need-their-own-handles` |
| `cl-mcp/src/spec-adapter-report::contract-operation-missing` (internal) | none | `spec-inspection-operations-need-their-own-handles` |
| `cl-mcp/src/spec-adapter-report:list-report` | none | `spec-inspection-listing-separates-capability-from-count` |
| `cl-mcp/src/spec-adapter-report:symbol-report` | none | `spec-inspection-registration-is-not-read-failure` |
| `cl-mcp/src/spec-adapter-report:describe-report` | none | `spec-inspection-registration-is-not-read-failure` |
| `cl-mcp/src/spec-adapter-report::%describe-function-spec` (internal) | none | `spec-inspection-contract-declaration-survives-describe` |
| `cl-mcp/src/spec-adapter-core:definition-digest` | none | `spec-inspection-digest-comes-from-the-record-or-the-readers` |
| `cl-mcp/src/tools/spec-response-builders:build-spec-list-response` | none (see *Responses*) | `spec-list-response-says-why-a-kind-has-no-names`, `spec-response-json-keeps-false-null-and-absent-apart` |
| `cl-mcp/src/tools/spec-response-builders:build-spec-symbol-response` | none | `spec-symbol-response-separates-registration-from-failure`, `spec-response-json-…` |
| `cl-mcp/src/tools/spec-response-builders:build-spec-describe-response` | none | `spec-describe-response-carries-the-declaration-it-was-given`, `spec-response-json-…` |
| `cl-mcp/src/tools/spec-response-builders:build-spec-check-response` | none | `spec-check-response-carries-the-verdict-and-its-reservations`, `spec-check-replay-line-asks-for-the-run-it-reports`, `spec-response-json-…` |
| `cl-mcp/src/pool:get-or-assign-worker` | none (see *Pool ownership*) | `pool-ownership-holds-over-operation-sequences`, `pool-ownership-holds-when-the-pool-is-full` |
| `cl-mcp/src/pool:release-session` | none | the same two |
| `cl-mcp/src/pool:kill-session-worker` | none | the same two |
| `cl-mcp/src/pool:shutdown-pool` | none | the same two |
| `cl-mcp/src/proxy:proxy-to-worker` | none (see *Request lifecycle*, *Reset events*) | `request-lifecycle-keeps-its-promises`, `resets-are-told-exactly-once`, `resets-are-told-exactly-once-when-the-pool-is-full` |
| `cl-mcp/src/proxy:cancel-request` | none | `request-lifecycle-keeps-its-promises` |
| `cl-mcp/src/pool:kill-session-worker`, `release-session`, `shutdown-pool` (resets) | none (see *Reset events*) | `resets-are-told-exactly-once`, `…-when-the-pool-is-full` |
| `cl-mcp/src/tools/pool-kill-worker::pool-kill-worker-handler` (internal; the tool) | none | the same two |
| `cl-mcp/src/tools/pool-kill-worker::%with-resets` (internal) | none | the same two |
| `cl-mcp/src/proxy:reset-notice` | none | the same two |
| `cl-mcp/src/reset-events:record-termination`, `claim-session-resets`, `discard-session-resets`, `discard-all-resets` | none | the same two |
| `cl-mcp/src/object-registry:register-object`, `lookup-object`, `clear-registry` | none (see *Reset events*) | `object-ids-never-outlive-their-image` |
| `cl-mcp/src/pool:shutdown-pool`, `get-or-assign-worker`, `release-session` (concurrency) | none (see *Concurrency and shutdown*) | `pool-shutdown-leaves-nothing-behind`, `pool-holds-while-operations-overlap` |
| `cl-mcp/src/pool::%spawn-and-bind`, `%replenish-standbys`, `%handle-worker-crash`, `%signal-worker`, `%wait-for-work-in-flight`, `%begin-ending`, `%end-worker` (internal) | none | `pool-shutdown-leaves-nothing-behind` |
| `cl-mcp/src/pool:kill-session-worker`, `cl-mcp/src/pool::%check-worker-health`, `%handle-worker-crash`, `%effective-pool-size` (internal) | none | `pool-holds-while-operations-overlap` |

Property names are in `cl-mcp/specs/strings`, `cl-mcp/specs/sanitize`,
`cl-mcp/specs/paths`, `cl-mcp/specs/write-paths`, `cl-mcp/specs/core-records`,
`cl-mcp/specs/check-verdicts`, `cl-mcp/specs/check-routing`, `cl-mcp/specs/spec-inspection`,
`cl-mcp/specs/spec-responses`, `cl-mcp/specs/pool-ownership`,
`cl-mcp/specs/request-lifecycle`, `cl-mcp/specs/reset-events` and
`cl-mcp/specs/concurrency`. Each Function Spec is
registered on the production symbol itself. Each read-access property is
`(:about ...)` both read functions; each write-access property names the
function or functions it calls.

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

## Write access

`ensure-write-path` decides where a write may land, and `fs-write-file` writes
there. The properties in `specs/write-paths.lisp` check both against this
policy:

```
an absolute argument                                    -> refused
a target whose real location is outside the project     -> refused
a .. right after a symlink, or after a name that does   -> refused
  not exist yet
anything else, whether the target exists or not yet     -> allowed, as its real path
```

The real location is where the OS will put the file. Every existing directory
and symlink on the way is followed, and only the names that do not exist yet
are taken as written. A registered ASDF system's directory outside the project
is readable but never writable: before, during and after its registration.
After a symlink, `..` means the parent of the link's target to the OS, but a
lexical reading takes it as cancelling the link's name. After a name that does
not exist yet, the OS cannot resolve `..` at all. Both are refused as
uncheckable rather than guessed.

The expectation comes from the case's descriptor alone:
`expected-write-decision` in `specs/write-fixtures.lisp` reads the region the
target is in, the spelling, and which segment a detour would go back over. It
never calls the functions under test, `allowed-read-path`, `path-inside-p` or
any helper they use. An allowed call must return the truename of the target's
existing directory followed by its new names, compared as native strings.

### What was found, and what changed

The following were reproduced in scratch trees, each in a Lisp process of its
own, before anything was fixed. Fixed tests pin each one (`e7b410a`,
`2051f8e`).

- `project/escape` was a link to `../outside/`. `ensure-write-path` allowed
  `escape/new.txt` and `escape/new-dir/new.txt` and returned them under
  `project/`, so `fs-write-file` created `outside/new.txt` and
  `outside/new-dir/new.txt`. `escape/sentinel.txt`, which existed, was
  refused. The old code merged the argument onto the root lexically and called
  `truename` only when the whole path existed.
- The same gap let a new file through a project link to a registered
  dependency's directory.
- A relative pathname, `#P"../outside/n1/new.txt"`, was allowed and written to
  `outside/n1/`. A natively parsed pathname keeps `..` as `:up`,
  `canonical-path` does not collapse it, and `uiop:subpathp` compares directory
  lists lexically. The same path as a string was refused.
- When the project root was a symlink alias, a new target was refused while an
  existing one was allowed.
- A project root that did not exist was taken as its own spelling, so a write
  created it.
- `plain.txt/x.txt` was allowed, and the write then failed inside
  `ensure-directories-exist`. A dangling link as the target was allowed and
  then replaced by a regular file.
- Separately, `fs-write-file` "overwrote" an existing file without a type
  (`Makefile`, `LICENSE`, `.hidden`) by writing `NAME.tmp` beside it and
  reporting success. `rename-file` merges its new name with the temporary
  file's pathname, which supplied the type `tmp`. `3284844` renames onto a
  type of `:unspecific`, which merging leaves alone.
- An existing directory named as the file to write (`src`, or a link to it)
  came back as the directory itself. On main, `fs-write-file` then renamed its
  temporary file onto itself, deleted it, and returned `T`, having written
  nothing. After `3284844` the name was merged in instead, and a file named
  `src/.file.<pid>.<serial>` appeared. Found in review of #171, and now
  refused as `:directory-target`.

`44ca65f` replaces the resolver. From the project root's truename, it takes the
argument one name at a time, split natively (so a bracket or an asterisk is
part of a name), and looks at each with `lstat`:

- It enters a real directory.
- It replaces a symlink with the real directory the link leads to. The link is
  checked with `stat` first, because SBCL's `truename` returns a dangling link
  as itself.
- `..` leaves the real directory reached so far.
- The first name that does not exist, and every name after it, is new.

The write is allowed when the resulting real path is inside the root's
truename, and that path is returned. Every refusal is a `write-path-refused`,
a `simple-error`, whose `reason` says why.

What changes for callers:

- An allowed path comes back as its real path. `link-a/new.txt`, with `link-a`
  a link to `src/`, returns `project/src/new.txt`.
- These are refused now, each with its own reason. Before, they were allowed,
  and a write then failed, replaced a dangling link, or (for a directory)
  returned `T` without writing the file asked for:
  - no file name: `""`, `dir/`, `.`, `dir/..` (`:no-file-name`);
  - a project root that does not resolve (`:unresolvable-root`);
  - an ancestor that is a file, or a link to one (`:non-directory-ancestor`);
  - an ancestor that is a dangling link (`:unresolvable-ancestor`);
  - `..` right after a link (`:parent-after-link`);
  - `..` after a name that does not exist yet (`:parent-after-missing`);
  - a final link that leads nowhere (`:unresolvable-target`);
  - an existing directory, or a link to one, as the file to write
    (`:directory-target`).
- `:absolute` and `:outside-project` keep their old messages.
- The read side is unchanged, and still resolves `..` lexically (see *Known
  issues*).
- The check and the write are not atomic. A directory swapped for a symlink
  between them is not detected.

| Property | About | Checks |
|---|---|---|
| `write-resolves-project-targets-without-creating` | `ensure-write-path` | project targets (existing, new, below one or two new directories) are allowed as their real path, in relative, pathname and detour spellings, with or without a root alias; absolute spellings and a detour back over a new directory are refused; nothing is created |
| `write-refuses-outside-and-absolute` | both | a target in the dependency, `outside/` or `project-other/` is refused before, during and after the dependency's registration, and `fs-write-file` refuses it while registered; a project control is allowed relative and refused absolute in every phase |
| `write-follows-existing-links` | `ensure-write-path` | one link in `project/` or `outside/`, to the target's directory or to an existing target file, decides by where it leads, for new and existing targets alike; `..` right after a link is refused |
| `writer-changes-only-the-expected-entries` | both | on any of the above, `fs-write-file` adds exactly the expected directories and file with exactly the bytes written, or replaces the existing file's bytes, and nothing else in the tree changes, temporary files included; or it refuses and nothing changes |

### Observing writes

Write cases use the read fixture (see *Fixtures* above) with three additions:

- `scratch-snapshot` lists the whole scratch tree, without following any link:
  each entry's path below the scratch root, its kind, and a link's target or a
  file's bytes. It records no times, so taking a snapshot changes nothing a
  later one sees.
- `snapshot-changes` compares two snapshots: added, removed and changed
  entries.
- `:adopt-new-entries` makes cleanup also remove, deepest first, whatever below
  the scratch root the fixture did not create. It removes one entry at a time,
  never follows a link, and lists each entry in `read-fixture-adopted`.
  Adoption runs only when the fixture's own mkdir of the scratch directory
  succeeded, and the path is still a directory. When the path was already
  taken, by a directory or a symlink, mkdir fails, and cleanup lists, removes
  and reports nothing below it. Before this check, a taken path had all its
  entries adopted and deleted (found in review of #171). A test aims a fixture
  at a taken path through `*scratch-name-function*`.

Each check makes exactly one call, snapshots the tree just before and just
after, and judges from those two snapshots while the tree still exists. Only
then does cleanup adopt what was written. Only `write-path-refused` from that
one call counts as a refusal; any other condition fails the trial. Every
argument lies in the check's own scratch tree, so even a wrong implementation
writes nowhere else. The negative control relies on that.

### Domain

- Targets in `project/`, the dependency (registered or not), `outside/` and
  `project-other/`: zero to two existing directories, then zero to two new
  ones, then a new or existing file. The names are plain, spaced, Japanese,
  dotted and bracketed, with or without a type.
- The target is reached directly, or through one link at the root of
  `project/` or `outside/`. The link goes to the target's directory or to an
  existing target file, and `outside/` links are reached through
  `../outside/`.
- Spellings: relative strings, relative pathnames, `./` plus a `D/../D` detour,
  and absolute strings. The project root is given directly or as an alias.
- Measured over 2000 draws per generator:
  - project cases: 71 % allowed;
  - link cases: 16 % allowed, and 35 % have the shape of the original escape
    (a directory link out of the project, a new target, a relative spelling);
  - writer cases: 27 % allowed, 12 % of the escape shape.
  So one seed of 12 trials misses the escape shape with probability 0.6 % in
  the link property and 21 % in the writer property. The fixed tests do not
  depend on that.
- Fixed tests only: dangling links, a file as an ancestor, no file name, an
  existing directory or a link to one as the target, an unset or unresolvable
  root, `..` in a relative pathname.
- Not covered: races with the filesystem (TOCTOU), permissions and ACLs, hard
  links, link chains and loops, mount namespaces, Windows paths, and what the
  MCP tools do around `fs-write-file`.

The fixed tests are in `tests/write-path-specs-test.lisp`, in the default
suite:

- the original escape, for the validator and the writer, with the sentinel
  file left untouched;
- the table of topologies behind every reason above;
- the root alias, and the dependency before, during and after registration;
- the writer's exact effects: new directories and file, an in-place update,
  names without a type, and links;
- the fixtures themselves: no new directory is created before the call, what
  a write created is seen before it is adopted, adoption unlinks a link
  without following it and also runs when the body signals, and a scratch
  path that already exists, as a directory or a link, is neither adopted nor
  reported, and every file in it keeps its bytes;
- a fixed sample of the generators.

Each write property runs 12 trials at `:normal` and 3 at `:smoke`. Each takes
0.01–0.06 s per seed, in the native runner and in an MCP worker alike.

### Safe spellings

`write-preserves-safe-spellings` (`:about ensure-write-path`, `:kind
:equivalence`) is a relation, not another permission rule. For one allowed case
it calls `ensure-write-path` with four spellings of the same file, one call
each, in every trial:

| Spelling | Built from the base `s` by | `src/new.txt` | `Makefile` |
|---|---|---|---|
| `:relative-string` | nothing: `s` itself | `src/new.txt` | `Makefile` |
| `:relative-pathname` | a native parse of `s` | `#P"src/new.txt"` | `#P"Makefile"` |
| `:leading-dot` | `./` in front | `./src/new.txt` | `./Makefile` |
| `:repeated-separator` | one `/` doubled, or `.//` in front when `s` has none | `src//new.txt` | `.//Makefile` |

POSIX pathname resolution ignores a `.` segment, and a repeated separator
anywhere but at the very start, so all four name one file. A leading `/` or
`//`, a trailing `/`, `D/../D`, and `..` after a link do not follow that rule,
and none of them is generated. The variants are made from `s` by string
operations and a native parse only, never by resolving it, so the differences
are still there when the function is called. Each argument is a fresh object.
The pathname is parsed from `s` alone, so it is physical and never wild, with
brackets taken literally. The three strings go to the function as they are.

The property demands, in this order, each as its own condition in the body:

1. All four spellings were called once, a pathname and at least two strings
   among them (`safe-spelling-coverage-p`). An empty list, or the base alone,
   fails.
2. The base returned an absolute path equal to `expected-write-native`, fixed
   before the first call. This is the constructive oracle again, so refusing
   everything, or sending every spelling to one wrong file, cannot pass by
   agreeing with itself.
3. Every other spelling returned an absolute path equal to the base's.
4. No call changed the tree.

A refusal in any spelling fails the trial; two refusals are not agreement.
Each result is kept as a native string the moment it is observed. If a call
changes the tree, observation stops there, and the trial fails.

The domain is `draw-safe-spelling-case`. The target is a project file, existing
or new, below zero to two existing directories and zero to two new ones. It is
reached directly, or through one directory link at the root of `project/` that
leads to the target's existing directory. The project root is given directly or
as an alias. Names are the fixture's plain, spaced, Japanese, dotted and
bracketed parts, with or without a type. Over 2000 draws: 39 % go through a
link, 32 % use the root alias, 18 % target an existing file, 40 % have new
directories, 33 % have no type, and 12 % have no separator, so they use
`.//`. The existing generators and their digests are unchanged.

A counterexample reports the case descriptor only.
`(cl-mcp/specs/write-fixtures:explain-safe-spellings case)` rebuilds that case in
a fresh fixture and returns, per spelling, the input's type and text, the
returned path or the refusal's reason, and the tree's changes. It makes the
calls again; it does not replay the failed run.

Cost: 12 trials make 48 calls on 12 descriptors (four spellings each, not 48
independent cases), in about 0.02 s per seed. The fixed tests cover specific
cases:

- a file without a type at the root, new and existing;
- an existing file;
- two new directories;
- a link with spaces, Japanese and brackets;
- the root alias;
- a link to the root under the alias.

They also check the spellings themselves: relative, the same file name, no
`..`, no trailing slash, a physical pathname, no shared strings. Finally, they
check that coverage refuses an empty list, the base alone, and a missing
pathname.

Not covered here: the writer, a link compared with the direct path it leads
to, and anything outside the project. Refusals are the other properties' job.
Those are the next unit of work.

## Record fidelity

`cl-mcp/src/spec-core-record` carries one cl-spec versioned record to an MCP
client. `field-availability` says whether a field is there, and
`validate-versioned-record` says whether the record can be read at all.
`project-record` projects each field by what it means, and
`project-core-record` puts that projection beside what cl-mcp knows about
carrying it. The properties in `specs/core-records.lisp` check that the carrying
changes nothing the record says. They run against these four public functions
directly, never through `spec-check` or the bundle.

What a record means is restated in `specs/core-record-fixtures.lisp` from
cl-spec's public contract: the version-1 required metadata of its
`SCHEMA-INFO`, and the fields `RESULT-DATA` documents as holding the
`:not-collected` sentinel. Nothing there is read from the module under test.
None of `*record-shapes*`, `+sentinel-fields+` or `+v1-required-metadata+` is
used. The opt-in tests check the restated required metadata against the real
`SCHEMA-INFO`.

| Property | Checks, every trial |
|---|---|
| `core-record-availability-separates-absence-from-nil` | a missing key is `:absent`; a present NIL is `:collected`; `:not-collected` is `:not-collected` in a sentinel field and `:collected` in an ordinary one; at the front or the back of a record |
| `core-record-projects-each-field-by-its-role` | a boolean NIL is `(:bool nil)` (false); an absent observation is null; an empty collection is `[]`; a missing key is missing from `:data` while a present NIL phase is null; a collected capture value, even one shaped like cl-spec's unavailable marker, is an externalized value, with no invented `reason` or `type`. The value itself survives: its printed text is the standard printer's, and a list's object id names the record's own list in the same registry, while an atom has none |
| `core-record-seeds-stay-decimal-text` | five seeds: small, around 2^53, just under cl-spec's 2^62 draw bound, anywhere below it, and past it. Each reaches `:data` as its decimal text, computed by integer division, while a small trial count stays a number |
| `core-record-ignores-order-duplicates-and-unknown-keys` | reordered pairs, a later duplicate with another value (the first wins), and unknown keys leave every known field's projection and availability as they were; unknown keys are named, not guessed into `:data`, and change neither `complete` nor `schema_supported` |
| `core-record-reports-every-cut` | all three limits, list length, characters and depth, each on its own record, pushed just under, to, just past and far past its bound. A list or string is whole up to and including its bound and cut past it. A depth is whole only below its bound: the container that reaches it is the one cut, externalized. Whole means no issue and `complete` true. Cut means exactly one issue, at the field's path with the limit's reason, and `complete` false. The kept part is the head, compared item by item and character by character. Every item and character differs from its neighbours, so a projector that kept the tail or reordered would show. An omitted count is the true excess when it says it is exact, and less when it says it is not. No issue appears inside `:data` |
| `core-record-validation-separates-ok-unsupported-malformed` | three records, each one cause from a valid one: `:ok` and projected; `:malformed` (a dropped required key, NIL, an improper, odd-length or string-keyed plist, a wrong record or entity kind, no version) with no report; `:unsupported-schema` for another integer version, reported collected with `schema_supported` false and no `:data` |

JSON's grammar allows any integer as a number. A seed stays text because a
consumer reading JSON into binary64 doubles rounds anything past 2^53, and a
rounded seed reproduces a different run. cl-spec draws seeds below 2^62 and
accepts any non-negative integer, so both sides of both bounds are generated.

Every projection runs inside `with-isolated-object-registry`. That binds
`*object-registry*` to a registry of the check's own, so opaque values never
evict, or show, a user's `inspect-object` ids. It is a dynamic binding, so it
covers the calling thread, which is where each check makes its calls. The
cl-spec registry is a different thing, and the properties do not touch it.

Each property runs 25 trials at `:normal` and 5 at `:smoke`, and makes a fixed
number of calls per trial:
- availability: 7 `field-availability` calls;
- roles: 1 `project-core-record` call;
- seeds: 5;
- key relations: 4;
- cuts: 12 on a passing trial, four sizes for each of the three limits (a failing
  trial may stop sooner);
- validation: 3 `validate-versioned-record` and 3 `project-core-record` calls.

One seed of all six takes a few milliseconds.

The fixed cases are in the default suite (`tests/spec-core-record-test.lisp`):
- the JSON decoder setting itself;
- a record taken to JSON with false, null, `[]`, a missing key and a present
  null apart, and no `source`;
- an array check that asks for a vector that is not a string before it looks
  at the length. `(equalp #() "")` is true, so neither `""` nor `"lost"` may
  pass for `[]` or for a non-empty array;
- nine seeds from 0 to 2^64+1, before and after JSON;
- every sentinel and value field in every state;
- one reordered, duplicated and extended record;
- each limit at 39/40/41 items, 29/30/31 characters, and depth 3/4/5, with
  the kept items and characters compared, not only counted;
- every single-cause malformation and four unsupported versions;
- an opaque value's id resolving in the bound registry while the global
  registry is unchanged.

Records from a real cl-spec are the opt-in `tests/core-record-specs-test.lisp`,
which `self-test` runs and CI requires:
- Four declarations of the test's own sit in a cl-spec registry made for them:
  a property that holds, one that fails, and the same pair of Function Specs.
- Each runs once with seed 2^53+1. Its `RESULT-DATA` record goes through
  `project-core-record`, the adapter's own core-record renderer, `yason`
  encoding, and a decoder set to keep false, null, `[]` and a missing key apart.
- Selected fields are compared with what the record itself says: record and
  entity kind, status, the seed's decimal text, digest completeness as a JSON
  boolean, each sentinel's availability, and null against an object for the
  failure. Also checked: `projection.complete`, no unknown keys, and no
  `source` key.
- `elapsed` and the digest's value are not compared.
- A missing cl-spec is a load failure there, not a skip.

This is record-to-JSON, not an end-to-end check of JSON-RPC.

Not covered: selection, the `verified` tally and `verification_gaps` (see
*Verdicts* below), the legacy fallback, other cl-spec versions, Function Spec
definition records, circular metadata (the existing termination tests keep
that), and whether any tool passes these records on correctly.

## Verdicts

Once a selection has run, `check-report` turns its results into four answers:
`counts`, each contract's `effective_trials`, `verified` and
`verification_gaps`. The properties in `specs/check-verdicts.lisp` check the
four internal functions that compute them: `%counts`, `%contract-plist`,
`%verified-p` and `%verification-gaps` in `cl-mcp/src/spec-adapter-report`.
Each property calls its function directly, on its own thread. None goes through
`check-report`, `spec-check` or the bundle. No public function or wrapper was
added to reach them. Internal symbols are fine for `:about`, and `spec-symbol`
takes them package-qualified (`cl-mcp/src/spec-adapter-report::%verified-p`).

Two layers are kept apart, because the functions sit on either side of one
normalization:
- `%contract-plist` reads cl-spec's own answers. That is a v1 result record, or
  the individual readers of an older cl-spec, reached through a `make-cl-spec-api`
  stub.
- The other three read what the adapter made of those answers: the result plists
  `%result-plist` and `%run-one` return, and the selection plist the selection
  step returns.

`specs/check-verdict-fixtures.lisp` builds each shape the function really
receives, and needs no cl-spec.

**Where expectations come from.** Three tables in the fixture file restate what
spec-check's description promises a client:
- `+result-kinds+`: 36 kinds of result (every status, property and contract
  runs, every refusal-count state, case report and schema state). Each row
  gives three answers:
  - can the result support `verified`;
  - which gaps it justifies on its own;
  - does it carry a refusal count a response may subtract with.
- `+selection-kinds+`: the gaps each kind of selection justifies.
- `+rejection-rows+`: one row per condition on a refusal count.

Expected gaps are the rows' own gaps put together, plus two call-level rules:
- `rejection-counts-unmeasured` unless every result is a contract run with a
  usable count;
- `input-coverage-unmeasured` always.

Counts are counted from the drawn descriptors, and effective trials are the
drawn `E - R`. Nothing is computed by the functions under test, by their
helpers (`%evaluated-p` and the rest), by the renderer, or by `verified` itself.

| Property | Checks, every trial |
|---|---|
| `check-verdict-counts-keep-every-status` | the empty list and two drawn lists of results. One holds each of the 13 statuses zero to three times; the other holds every status at least once. For each, every named field and `other` equal the number of results with those statuses. `by_status` has one positive entry per status that occurred and no other entry. `selected` equals the length, the sum of the named fields and `other`, and the sum of `by_status`. A reordered list tallies the same, and two lists joined tally the sum of theirs |
| `check-verdict-effective-trials-only-from-a-usable-count` | every row, read from a v1 record and through the legacy readers, plus the unreadable row once for each of seven ways a count can be missing. Usable: a known `:pre` with 0 ≤ R ≤ E, or no `:pre` with R = 0, and E − R effective trials (0 when every input was refused). Unusable, with no effective trials (not 0, not the raw count): an unreadable count or trial count, R < 0, R > E, refusals without a `:pre`, an unknown `:pre`. Also checked: `rejected_readable` (the record declares the key, or the reader answered), `rejected_measured`, the overcounted and contradicted flags, and the failure reason with its readable flag. On the record path, the legacy readers answer differently and are never called |
| `check-verdict-verified-needs-evidence-from-every-result` | one to four good results verify, in any order and under any names, seeds and timings. They still carry gaps. The empty list does not verify. One result of each of the 30 other kinds, alone or inserted among the good ones, refuses the verdict: every status but passed, zero or unknown trials, zero or unknown effective trials however many raw trials ran, a declared case never reached, a case report missing where cases are declared, cases whose existence is unknown, and a record whose schema is unsupported. A cut projection is not one of them |
| `check-verdict-gaps-name-each-shortfall-and-nothing-else` | three lists each trial: a `property=` or `symbol=` run, a `function=` run, and a mixed list that always pairs a usable count with a result without one. The gaps must equal the expected set, with no duplicates. Then each result is taken away in turn, and so is the selection's shortfall. The gaps that result grounded must go, and the others must stay. Failed and error add no gap. A shrink that ran out is not `generation-incomplete`. Cases whose existence is unknown are not reported as uncovered |

Two kinds of result never reach these functions from a real run. They are
kept for the verdict property only, to show that each condition refuses on its
own:
- a passing property with no trial count. cl-spec refuses to build a result
  whose `:trials` is not a non-negative integer.
- an unreadable declaration beside a usable count. When the declaration cannot
  be read, the adapter also takes its `:pre` as unknown, so the count is
  unusable.

The first reports `zero-trials`, the same gap as a budget of zero. Nothing
in spec-check's description names a gap for it, and the gap property does not
take it as a requirement.

`no-properties-selected` comes from `check-report`'s empty-selection answer,
not from `%verification-gaps`. The gap property lets an empty list carry it or
not, and the fixed `check-report` test requires it exactly once. Every fixed
`check-report` test requires each gap to be listed once. A set comparison alone
would miss a gap added by both the helper and `check-report`.

Each property runs 25 trials at `:normal` and 5 at `:smoke`. Calls per trial:
- counts: 5 `%counts` calls;
- effective trials: 25 `%contract-plist` calls;
- verified: 64 `%verified-p` calls and 1 `%verification-gaps` call;
- gaps: 8 to 15 `%verification-gaps` calls.

The fixed tests below cover every row of the three tables. The properties do
not. Each trial runs a fixed set of states and draws the numbers, combinations
and order within them.

| Property | Run in every trial | Drawn |
|---|---|---|
| counts | all 13 statuses, in the second list | the counts, the order, and which kind of result carries each status |
| effective trials | every refusal-count row on both paths, and all seven ways a count can be missing | the numbers, and the free `:pre` and failure-reason choices |
| verified | the empty list, and each of the 30 kinds that are not evidence, alone and among good results | the one to four good results, and the selection |
| gaps | a property or symbol run, a contract run, and a mixed list that pairs a usable count with a result without one | each list's result kinds and its selection |

Each of the five planted faults in the negative control lies in a state that
every trial runs. Catching it therefore needs no lucky draw.

The fixed cases are in the default suite (`tests/check-verdict-test.lisp`).
They need no cl-spec:
- every row of the three tables once, with concrete expected values;
- the joint between the two layers: the real `%contract-plist` output goes into
  a result, and `%verified-p` and `%verification-gaps` read it. A hundred raw
  trials beside an unreadable count is `effective-trials-unknown`, not
  verified and not `zero-trials`;
- `check-report` over a stub API that answers from v1 records and records every
  run it is asked for, in six cases:
  - a passing and a failing property;
  - a passing property, verified with its two gaps;
  - a contract verified from 5 trials minus 2 refusals, with `properties-not-run`;
  - a contract that refused every input, answered `skipped`;
  - a contract with a case never reached;
  - an empty selection.

  Each checks the recorded runs, `status`, the results, `counts`, `verified` and
  the gaps, and that the object registry holds as many objects as before. The
  stub keeps its record of calls in a closure. `check-report` runs each property
  in a thread of its own, which does not see the caller's dynamic bindings.

No real-cl-spec contrast was added. `tests/spec-integration-test.lisp` already
reads a real contract through `spec-check` in each of these cases:
- verified, with its refusals counted and no refusal gap;
- skipped when every input is refused;
- a case never reached;
- generation exhaustion.

That suite skips each test when the image has no cl-spec recent enough, and
in CI it does. The default `test` job has none, and the `specs` job, which has
the pinned cl-spec, does not run the suite. Until a job runs it with a skip
counted as a failure, those real results are checked locally only.

Not covered: selecting from a real registry, profile/trials/seed/digest routing
(see *Routing* below), the deadline and real timeouts, rendering and JSON-RPC,
the legacy fallback beyond the two refusal readers, and a record availability
of `unavailable`.

## Routing

Between a request and a runner, spec-check decides five things:
- whether the target arguments are valid;
- what the seed text means;
- which definitions the request names;
- which trial budget each run gets;
- how a definition digest compares with the one the caller expected.

`specs/check-routing.lisp` checks the functions that decide them, each called
directly:

| Function | Decides |
|---|---|
| `%target-argument-error`, `%resolve-profile` | exactly one of property, symbol and function; trials only with function; profile only with property or symbol; the profile keyword, never interned |
| `parse-seed-string` | a seed's decimal text as an integer, or a refusal |
| `%select-properties` | the names a selection runs, and what it reports as not run |
| `%trials-budget` | the budget and where it came from |
| `%definition-match` | not-checked, unknown, true or false |

Three kinds of evidence are kept apart:
1. **Generated properties** over descriptors and small stub APIs. They call no
   runner, start no thread and use no real cl-spec.
2. **Fixed tests with a recording spy**, in the default suite
   (`tests/check-routing-test.lisp`). These show whether a decision reaches
   the runner. `SPY-API` stands in for cl-spec:
   - its runners take only the keywords the real ones take, and signal on any
     other;
   - they record their raw argument lists, and the registry and backend they
     see on the thread `check-report` runs them on;
   - the record lives in a closure, because that thread does not see the
     caller's dynamic bindings.
3. **A real cl-spec**, opt-in (`tests/check-routing-specs-test.lisp`). This
   checks the stub's registration model against cl-spec itself. It wraps
   cl-spec's own two runners in a copy of the API to record them, and
   swaps nothing global.

**Where expectations come from.** `specs/check-routing-fixtures.lisp` (no
cl-spec) states each rule as data:
- the target rule, as spec-check's description gives it;
- a registry descriptor's relations: a Function Spec for `routing-f`, a
  property of the same name, properties P1 to P3 `(:about routing-f)` in any
  registration order, an unrelated U, and `routing-f` in another package;
- distinct value ranges for a profile entry (0 to 40), the backend default
  (100 to 199) and explicit trials (1000 to 1999);
- decimal text written by integer division (`decimal-string`);
- the digest table.

Nothing is computed by the functions under test. A name meant to be unknown
is only ever a string, and the property checks that trying it interns nothing.

| Property | Run in every trial | Drawn | Calls per trial |
|---|---|---|---|
| `check-routing-target-arguments-are-exclusive` | all 8 presences of the three targets, each with and without trials and profile; no profile; three known profile spellings; three unknown names | the names, the trial count, the profile and its spelling | 39 |
| `check-routing-seed-text-keeps-every-digit` | no seed (not seed 0); 0, 1, 9, 10, both sides of 2^53, 2^62 and 2^64, 10^40+7; eleven refused spellings of one seed (sign, whitespace, exponent, fraction, radix, `#.`, separator); the empty string and four non-strings | four seeds below 2^128, each with 0 to 3 leading zeros | 34 |
| `check-routing-selection-names-only-what-was-asked` | sixteen requests (property= each target, symbol= and function= the subject, the other package and two unresolvable names) against a drawn registry and against it with a contract, a same-named property and a readable index; then adding U, reversing the order, removing one related property | the registry (contract, same-named property, which related ones and in which order, U, whether the index can be read) and each request's qualified or unqualified form | 38 to 40 |
| `check-routing-budget-comes-from-its-stated-source` | a property with an entry for the profile, an entry of 0, an entry only for another profile, and no entry while the backend is absent, the reader signals, answers NIL or is missing; a contract with explicit trials, without, and without a backend | the entry, the default and the explicit trials | 11 |
| `check-routing-digest-comparison-has-four-answers` | nine rows: three with nothing expected; nothing to compare; an incomplete digest, equal and not; two complete digests, equal and not | the digest, and which hex digit the other one changes | 9 |

Each selection check also asserts two things:
- no runner is called;
- every reader is handed the registry it was given.

Symbol resolution is exercised only for these names. The resolver itself is
not re-specified.

**Fixed cases, default suite** (`tests/check-routing-test.lisp`, 18 tests):
- **Target arguments:** the eight target presences and the trials and profile
  rules. Through `check-report`, no runner is called for any refused
  combination, even with cl-spec not loaded.
- **Profile:** none given is `:normal`, and `smoke` is `:smoke`. An unknown
  name is refused, runs nothing and stays uninterned.
- **Seed text:** twelve spellings from 0 to 10^40+7, leading zeros included,
  and eighteen refused inputs.
- **The entry refuses early:** `spec-check-response` refuses a bad seed or
  trials (0, −1, 1,000,001, 1.5, `"3"`) before cl-spec is consulted. It
  accepts 1 and 1,000,000 as far as the report. The target there resolves to
  nothing, so nothing runs.
- **Seed delivery:**
  - The runner receives 0, 2^62+1 and 10^40+7 exactly, and the report shows
    each as its text.
  - With no seed, the runner receives `:seed NIL` and the report shows the
    seed the result recorded.
- **Fan-out:**
  - nothing related runs nothing;
  - one related property runs with the seed;
  - two with a seed, 0 included, are refused before either runs;
  - two with only an expected digest both run.
- **Raw arguments:** a property's runner gets `(:profile :seed :registry)`.
  A contract's gets `(:seed :registry :trials)`, with the backend default
  when no trials are given. When the default cannot be read, the `:trials`
  keyword is absent, not given as NIL.
- **Budget records:** asked for 7, a result recording a budget of 5 and 2
  trials is reported as budget 5 from `cl-spec result`, with 2 executed.
- **Identity:** the registry the runner is given, and the registry and backend
  its thread sees, are the objects the call captured. The budget came from
  that same backend.
- **Digests:**
  - Per result: true, false, unknown (incomplete, equal or not) and
    not-checked.
  - A mismatch leaves `passed` and `verified` as they are, and a failed run
    can still match.
  - Overall, a false and an unknown make the replay unknown. An empty
    selection is not checked.
  - A run not started for lack of budget, or one that signals, is
    not-checked.
  - A property and a contract of one name keep their own digests.
  - The digest the result recorded wins over the one read before the run.

**Real cl-spec, opt-in** (`tests/check-routing-specs-test.lisp`, 5 tests). Two
registries of the test's own hold the same declarations except for one
related property. Each check binds `cl-spec:*registry*` around the call.
- **Selection:**
  - `symbol=` runs only P1 and P2, and names the contract and the same-named
    property as not run;
  - `property=` of the shared name runs that property;
  - `function=` calls only `check-function`.
- **Registries:** A runs P1 and P2 where B runs P1 alone. P2 is
  not-registered in B, and nothing runs.
- **Delivery:**
  - A contract with trials 3 and seed 0: cl-spec's record says seed 0 and
    budget 3.
  - A property with the smoke profile and a seed past 2^62: cl-spec's record
    says that seed and 2 trials.
- **Replay:** the same declaration and seed replay as `true`/`true`.
  Declaring P1 again with another docstring gives `false`, and the run still
  passes.
- **The entry:** `spec-check-response` reads `"00042"` as seed 42 and reports
  `"42"`. With no seed, it reports the seed cl-spec drew.

**In CI, a skip is not a pass.** `specs/suite-judge.lisp` reads a Rove run's
per-test results. Every required test must have run, failed nothing, skipped
nothing and asserted something. `ROVE:RUN`'s own answer counts a skipped test
as passed: a run in which every test of `spec-integration-test` skipped came
back `T`, and the judge fails it. `scripts/check-specs.lisp`'s `integration`
mode runs one suite and judges it. It keeps the list of tests each suite must
run apart from the suite, so deleting or renaming one fails the step.
The `specs` job runs two steps, each in its own process:
- `spec-integration-test` (14 tests);
- `check-routing-specs-test` (5 tests).

The default `test` job still lets `spec-integration-test` skip when cl-spec
cannot be found. `tests/suite-judge-test.lisp`, in the default suite, checks
the judge on results built from Rove's own classes:
- a skip, including one nested in `testing`, fails the suite;
- so do a failure, a missing required test, an empty run, a test that
  asserted nothing, and a skip outside any test;
- the system → suite → test nesting is read correctly.

Found while writing this, and left as they are:
- `parse-seed-string` accepts decimal digits of other scripts (`"１２"` reads
  as 12): `digit-char-p` answers for any Unicode digit. The value is kept
  exactly, and the report then shows `"12"`.
- `%definition-match` compares digests without regard to case. The fixtures
  never rely on that either way.

Not covered: symbol resolution beyond these names; the renderer; JSON-RPC end
to end; real timeouts; and anything after the runner is called, which
*Verdicts* covers. The listing and describe paths are *Inspection* below.

## Inspection

Before anything runs, an agent reads: what this cl-spec can serve, what is
registered, what a declaration says, and which digest stands for it. The
danger here is the false negative — an empty answer given for something that
could not be read — so every check keeps three answers apart: present, absent,
and unreadable.

`specs/spec-inspection.lisp` checks the functions that decide these:

| Function | Decides |
|---|---|
| `api-backend-available-p`, `contract-operation-missing` | which operations this cl-spec can serve |
| `list-report` | what is registered, and what could not be enumerated |
| `symbol-report`, `describe-report` | registration against read failure |
| `%describe-function-spec` | what a contract declares |
| `definition-digest` | which digest stands for a declaration |

Expectations come from `specs/spec-inspection-fixtures.lisp`, stated once:
`+operation-handles+` (which handles each operation needs),
`+required-handles+`, a registry descriptor's definitions with their packages,
tags and `:about` relation, and a contract descriptor's own arguments, clauses
and cases. None of it is read from cl-mcp's `+listing-kinds+`,
`+contract-operations+`, `+required-functions+` or record shapes.

The stub records every reader call — its key, the name asked for and the
registry it was handed — so a check can say what was read as well as what came
back. Its runners signal if a read ever reaches them.

The recorded registry is checked by identity, against the object the stub's own
`registry` handle answered. It has to be: a reader called without a registry
still answers, out of whatever registry the image holds, and the stub answers
from the descriptor it closes over either way. Accepting a call that carries
`NIL` would accept exactly that. `+registry-taking-handles+` names the readers
this applies to; a handle that takes no registry is not asked to carry one.

| Property | Run in every trial | Drawn |
|---|---|---|
| `spec-inspection-operations-need-their-own-handles` | every subset of the handles each of the two contract operations needs — enumerated, not drawn — with unrelated handles beside them; the four backend states | the unrelated handles, and the backend state |
| `spec-inspection-listing-separates-capability-from-count` | the drawn request, then all four kinds under the full handle set; the three tag states | the registry, handles, kind, package, tag and limit |
| `spec-inspection-registration-is-not-read-failure` | the drawn subject under all four reader behaviours | the registry, the subject, the kind asked for |
| `spec-inspection-contract-declaration-survives-describe` | the drawn declaration, and both sides of one character budget | the arguments, clauses, cases, generator, schema and budget |
| `spec-inspection-digest-comes-from-the-record-or-the-readers` | five rows: a complete v1 digest, a refused one, an unknown version, the old shape, and an explicitly unread definition | the metadata state, the version, and the spec the old shape references |

What the properties hold to:
- **An operation needs its own handles.** A cl-spec that cannot run a contract
  can still describe one; one that cannot project a contract can do neither.
  A backend is available only as an object — a special bound to `NIL`, a
  reader that signals and a missing reader are all unavailable, and none of
  them is an error. Reading a registry needs no backend at all.
- **A listing keeps capability, scope and count apart.** A kind not asked for,
  or one that cannot be listed, has no count — never 0, which would say the
  registry holds none. The `*_listable` flags describe the revision, not the
  request. `tag_filterable` is the narrower statement its name reads as —
  whether this request's tag could be applied — so it is true when no tag was
  asked for, there being nothing to filter with. A package narrows by the home
  package of the registered name. A tag narrows properties only, and its three
  states (not requested, known, no such keyword here) stay apart without
  interning the unknown one. A limit cuts the lists and sets `truncated`, and
  changes no count. The names come back with the counts, and are the
  definitions the filters leave — a listing that returns the right number of
  the wrong names is not right. Only the kinds asked for are enumerated, and
  every reader is handed the registry the listing is about.
- **Registration is not read failure.** cl-spec saying "unknown name" gives
  not-registered; a reader that breaks gives an internal error; and
  `CL:UNDEFINED-FUNCTION` means the target is undefined only for a contract —
  out of a property or spec reader it is an adapter fault.
  `nothing_registered` is said only about a lookup that worked, and
  `include_runtime` false reads no runtime and says why.
- **A declaration arrives as written.** Arguments in order with their kind
  (`:required` where version 1 omits the key), supplied-p and keyword; range
  ends as `*` and `"0"`; clauses as forms that can be pasted back, one as
  itself and several joined by `AND`; a clause that is not there reported as
  not applicable, and one that is there and `NIL` reported as a clause; cases
  in declared order with their guards, outcomes and clauses. The argument
  generator, the whole-argument schema and the `:returns` and `:signals` nodes
  are absent or are the ones the record declares, by name and by type — not
  merely something non-`NIL`. A clause of a stated length is whole at that
  budget; one character under it, what comes back is that clause's own
  beginning and the remainder is counted exactly. A
  record of a version this cl-mcp does not know, one missing required
  metadata, one of the wrong kind, and a projection that came back `NIL` are
  all refused rather than read as an empty contract.
- **A digest comes from the record when the record has one.** A complete
  version 1 digest is used as it stands, and no dependency reader is called.
  A digest that is missing, incomplete or of an unknown version gives no
  digest at all — never one computed from the readers instead. The old shape,
  with no version key, digests from the readers, which it does read, and its
  digest follows the spec it references. An explicitly unread definition is
  not fetched again.

Clause text is compared by reading it back with `*read-eval*` off, not by
matching characters: the projector prints package-qualified, and every form
compared this way was written in the fixtures. Where a printed length matters,
the form is a string, whose length does not depend on the printer's package.
A read that fails is not a clause that means `NIL`, and a form with anything
after it is the text of something longer than the clause; both are told apart
from the clause `(NIL)` rather than counted as it.

**Fixed cases, default suite** (`tests/spec-inspection-test.lisp`, 16 tests):
each row above with concrete values, including the four backend states, a
count of none against not looking, the three tag states, a package filter, a
limit, registry delivery (and a recorded call that carries none), the four
reader behaviours, the declaration fields, a clause that could not be read
against one that means `NIL`, a cut clause at its own length and one character
under, the four schema refusals, and the five digest rows.

**Real cl-spec, opt-in** (`tests/spec-inspection-specs-test.lisp`, 6 tests).
Declarations of its own in registries of its own:
- a spec, a property of the subject's name, a property `(:about ...)` it, a
  plain contract and one with named cases, a capture and a state
  postcondition;
- introspection with no backend installed, which still lists and describes;
- cl-spec's own projection: the argument's kind, the open range end, the
  clauses, the case order, the capture and the argument generator;
- **a read runs nothing**: the target, the generator, `:pre`, the capture, a
  case guard, `:post` and `:state-post` each increment a counter, and a
  snapshot taken after registration is unchanged after six reads and an entry
  call. Each of those reads is first checked to have found the declaration it
  asked for — otherwise every counter would be unchanged for the wrong reason,
  which is what an entry call reading some other registry would look like.
  That is a statement about the forms a contract holds, not about everything
  an implementation may do while printing;
- declaring a contract again changes what describe says, and leaves another
  registry alone;
- the three entries keep the kind, name, limit and character budget they were
  given.

That suite installs its registry as `cl-spec:*registry*`'s global value and
puts the previous one back: the entry functions read the registry on a
deadline thread of their own, which does not see a dynamic binding. That is
why it runs in a process of its own, not in an MCP worker in use.

**Resolving the API** (`tests/spec-api-resolution-test.lisp`, 9 tests).
`resolve-cl-spec-api` reads the real `CL-SPEC` package, so this suite builds
one of its own — and refuses to run when a `CL-SPEC` package already exists,
rather than renaming or deleting someone else's. It needs no cl-spec at all,
and the `specs` job runs it as its own step:
- no package is `:not-loaded`, with no API;
- a missing required function, one that is not fbound, and an unbound required
  special are each `:incomplete`, naming only what is missing;
- a special bound to `NIL` is bound: the adapter is `:ok`, and the backend is
  reported absent, which is a different fact;
- optional functions and condition classes cost their own operation only;
- a complete cl-spec resolves to that package's own definitions, with the
  special named and read through;
- resolving calls none of them and interns nothing it did not find;
- the package is gone again afterwards.

Not covered: cl-spec revisions other than the one pinned; the listing of
specs and properties beyond these fixtures; and instrumentation. The renderer
is *Responses* below; JSON-RPC and the transports are not covered anywhere yet.

## Responses

The last step before a caller. `specs/spec-responses.lisp` checks the four
builders of `src/tools/spec-response-builders.lisp` —
`build-spec-list-response`, `build-spec-symbol-response`,
`build-spec-describe-response` and `build-spec-check-response` — through the
two things that actually arrive: the JSON document, and the text an MCP client
renders.

Three kinds of correctness, kept apart:

1. the structured fields keep the meaning of the report they were given;
2. the text makes no claim about that report that is not so;
3. both survive being encoded and read back.

The first two are checked against the scenario descriptors of
`specs/spec-response-fixtures.lisp`, never against each other. A field and a
sentence rendered from the same mistake agree perfectly.

**Everything goes through JSON.** A builder's hash-table cannot answer whether
a field is false or absent: `yason:false` and `NIL` are both objects in Lisp,
and only the document tells them apart. The fixtures encode with the call the
server makes and parse with `:json-booleans-as-symbols t`,
`:json-nulls-as-keyword t`, `:json-arrays-as-vectors t` and
`:object-as :hash-table`, which keeps five answers apart:

| In the document | Read back as | Means |
|---|---|---|
| `false` | `yason:false` | measured, and it is not so |
| `true` | `yason:true` | measured, and it is so |
| `null` | `:null` | there is no such value — nobody looked, or the definition has no such part |
| `[]` | an empty vector | looked, and there are none |
| key absent | `(values nil nil)` | this answer has no such field at all |

Note that the server's own reader is not this one: `yason:parse` with no
arguments, where `false` and `null` are both `NIL`. That is right for reading
a request and useless for asking what a response said, which is why the
fixtures carry a decoder of their own — and why the fixed cases guard it with
a document of known shape before anything else runs.

Two Lisp traps the helpers exist for, each with a fixed case of its own:
`yason:false` is a symbol, so `(when value ...)` reads a JSON false as true;
and a string is a vector, so `vectorp` and `equalp` cannot tell `""` from `[]`.

| Property | Function(s) | Run in every trial | Drawn |
|---|---|---|---|
| `spec-response-json-keeps-false-null-and-absent-apart` | all four builders | every listing answer, every spec-symbol answer, every kind of declaration, and the drawn spec-check answer | the spec-check answer |
| `spec-list-response-says-why-a-kind-has-no-names` | `build-spec-list-response` | all five listing answers under the drawn limit | the answer, the limit |
| `spec-symbol-response-separates-registration-from-failure` | `build-spec-symbol-response` | all six spec-symbol answers | the answer |
| `spec-describe-response-carries-the-declaration-it-was-given` | `build-spec-describe-response` | the drawn declaration in all four clause states | the declaration, the clause state, the documentation |
| `spec-check-response-carries-the-verdict-and-its-reservations` | `build-spec-check-response` | all sixteen spec-check answers, and the one robustness case beside them | the answer |
| `spec-check-replay-line-asks-for-the-run-it-reports` | `build-spec-check-response` | all sixteen, through the printed line | the answer |

What they hold to:
- **A count is a number only where someone looked.** A kind that was not
  requested, one this cl-spec cannot enumerate and a tag it cannot filter by
  each give `null` and a text that prints no zero for them; a kind that was
  asked for and holds none gives `0` and an empty array. The capability flags
  stay booleans whatever the request was. A limit cuts the list and sets
  `truncated`; it never touches the count.
- **Registration is not read failure.** A lookup that worked and found nothing
  says `nothing_registered`; an image without cl-spec and a reader that broke
  carry a status and no such claim at all — not even the key. A registration
  that is absent is `null`, not an empty object. A runtime nobody read says
  why.
- **A declaration arrives as written.** Kind and name, arguments in order with
  their own kinds, cases in order, and `returns` or `signals` — never both.
  The documentation crosses as itself through JSON escaping, for text with
  quotes, a backslash, a newline and characters outside ASCII. A clause that
  is whole, one that was cut and a definition with no such clause are `true`,
  `false` and `null`, with the count of what was dropped beside the second and
  a truncation notice in the text. A key that is not part of that kind of
  definition is `null`, never a `false` that says the definition turned it off.
- **A verdict carries what it does not cover.** The three verdicts are three
  words, and the word a reader stops at names the coverage it stands on: a
  declared case nobody reached, a contract that was not run, the properties
  that were not. A verdict is about the results alone — a digest that
  disagrees with the caller, or a contract nobody ran, leaves it standing and
  is reported beside it — and a status with no run claims no verdict at all.
  The gaps reach the text as well as the payload, and the line that lists them
  lists all of them. A counterexample that is empty, none, unavailable or
  never generated stays four answers; a captured `NIL` is application data
  where an unavailable capture is not a value, in the payload as well as in
  the text. Every seed stays a decimal string where there was one and null
  where the run never started, every symbol stays a package and a name, and
  cl-spec's raw record stays behind the projection — behind it, not withheld:
  what the text says about a capture or a case is in `core_result.data` too.
- **The replay line asks for the run it reports.** It is read out of the text
  the response produced — by the grammar the line is written in, never by
  evaluating it — and checked as the request it asks for: the first result
  that did not pass, with that result's own seed and digest. A property is
  asked for by `property=` and a profile; a contract by `function=` and a
  trial budget, and never by `property=` or a profile it did not use. A run
  with no seed prints no line at all, rather than one whose arguments are
  `NIL`.

**The scenarios are states the report layer builds.** Each report is written
in that layer's own vocabulary: the statuses of `+result-statuses+` and
`+call-statuses+`, the gaps of `+verification-gap-values+` in the order
`%verification-gaps` appends them, and the counterexample and shrink answers a
run that reached a verdict gives. A combination it does not build is not a
positive example of anything, however plausible it reads, so a fixed case
checks every value a descriptor uses against those documented sets. Two
consequences are worth naming, because a hand-written report drifts from them
first: every run carries at least `input-coverage-unmeasured`, and a property
run carries `rejection-counts-unmeasured` beside it — there is no check with
no gaps; and `verified` is decided by the results alone.

One case is kept apart, in `+check-robustness-cases+`: a whole-call timeout,
which `build-spec-check-response` answers and no spec-check call produces
(`%within-deadline` wraps the other three tools, and `+call-statuses+` does
not name it). It is checked as robustness, not as a state a run reports.

**Fixed cases, default suite** (`tests/spec-responses-test.lisp`, 25 tests):
the decoder's own guard, the two Lisp traps, the vocabulary check above, each
listing answer, each spec-symbol answer, the clause states, the documentation
strings, the verdict words, the four counterexample answers, a captured `NIL`
against an unavailable capture in the text *and* in the payload, the seed, and
the replay line for a property, for a contract and for a run with nothing to
replay.

**Real cl-spec, opt-in** (`tests/spec-responses-specs-test.lisp`, 2 tests): a
descriptor cannot say whether the printed line, handed back to the tool it
names, runs the same thing again. These two do that. A failing property and a
failing contract are run through `spec-check-response`; the line is read back
by the same grammar an agent would read it by, its arguments are passed to
`spec-check-response`, and the second run names the same target, carries the
same seed, compares `match` against the digest the line carried, and produces
the same counterexample — not merely another failure.

**Where the fields actually are.** A tool result here is
`{"content": [{"type":"text","text":…}], …}` with every structured field a
sibling of `content`, inside the JSON-RPC `result`. cl-mcp emits no
`structuredContent` and declares no `outputSchema` — grep for either in `src/`
and there is nothing — so an MCP client that renders only `content[].text`,
which is all the protocol requires of it, sees the text and nothing else.
That is why the properties here check the text for the reservations and not
only the payload. Moving to standard structured output is a separate
proposal, not part of this.

Not covered here: the tool entry points beyond that one round trip, the
worker's own JSON round trip, JSON-RPC and the transports. Those are 3E-2 (see
*The wire*).

### A false becomes a null on the way through a worker (fixed in 3E-2)

Measured while writing this and fixed in 3E-2. With the worker pool enabled --
the default -- a response crosses JSON twice: the worker encodes it, the parent
parses it, and the parent encodes the result again. The middle parse used
`yason:parse` with no arguments, which reads both `false` and `null` as `NIL`,
and the second encode wrote `NIL` as `null`:

```text
builder   {"verified":false,"gaps":[],"name":"","count":0,"absent":null}
client    {"verified":null, "gaps":[],"name":"","count":0,"absent":null}
```

`worker-rpc` and `proxy-to-worker` now take `:preserve-json-types`, which
parses with booleans as `yason:true`/`yason:false`, null as `:null` and arrays
as vectors, so the result encodes back to the JSON the worker wrote.
`with-proxy-dispatch` -- every tool whose worker result goes to the client
unread -- passes it. The pool's own RPCs (`worker/init-status`,
`worker/set-project-root`) and `clos-describe`, which read the result with Lisp
truth tests, do not: a `yason:false` is a true Lisp value. The encoder's retry
(`%sanitize-for-encoding`) keeps the three literals as literals instead of
printing them as `"FALSE"`.

## The wire (3E-2)

**Real cl-spec, a real server and real workers, opt-in**
(`tests/spec-wire-test.lisp`, 6 tests). Everything above checks a builder's
output or the adapter in the calling image. These tests are a client: a TCP
server started with the `cl-spec` group, one TCP connection per session, the
MCP handshake, and only the public tools. Every answer is parsed from the bytes
on the socket with `false`, `true`, `null`, `[]` and a missing key kept apart.
The declarations come from `tests/fixtures/spec-wire-fixture.lisp`, an inferred
subsystem that nothing depends on. Its `:import-from` clauses pull in cl-spec
and the check-it backend, so `load-system` loads it by name.

- **One session end to end:** `load-system` (cl-spec, then the fixture), then
  `spec-list`, `spec-symbol`, `spec-describe` and `spec-check` in the same
  session. A failing check arrives with `verified` and `thread_leaked` as JSON
  `false`, a decimal-string seed, a digest and its gaps. The Replay line it
  printed is read back by its own grammar and run again: `faithful`, `match`,
  the same seed, the same digest and the same counterexample. A passing
  contract arrives as `true`.
- **Evidence belongs to its session:** a compound counterexample's object id
  inspects in the session that ran the check. Another session on the same
  server gets "not found", because the object lives in the other worker.
- **Sessions and where things run:** the declarations one session loaded are
  not in another session's registry. `pool-status` shows two bound workers
  with distinct pids, neither of them the server. The server's own image
  never loaded the fixture, so no success here was the parent's.
- **Clean-up:** after the server stops, every worker it spawned is gone
  (reaped, not a zombie) and nothing listens on its port. The server macro
  stops the server, its pool and its listener however the body exits.
- **Inline against pool:** the same seven calls -- a failing property, a
  passing one, a contract, a compound counterexample, `spec-symbol`,
  `spec-describe` and `spec-list` -- go through a pooled server and then an
  inline one. The two answers are compared node by node: an object's keys as
  a set (a key holding `{}` is not a missing key), an array's length before its
  elements, and every leaf by value (`true` is not `false`, `false` is not
  `null`). Only three kinds of field are compared by JSON kind alone: elapsed
  times, object ids and the registry's printed identity. They belong to one run
  in one image, and a `null` where a number was is still a difference. With the
  parse fix taken out, this test reports `verified`, `thread_leaked` and
  `results[0].thread_leaked` as `null`.
- **The comparison itself:** fifteen fixed pairs, no server. They cover `true`
  against `false`, `false` against `null`, a key holding `{}` against no key,
  `[{}]` against `[{},{}]`, and an elapsed time that differs only in value
  (the same) against one that is `null` (a difference). The inline-against-pool
  test is only as strong as this function.

It spawns processes, so it is not in `tests.lisp`: in the default suite it
could only skip, and a suite that skips is a suite nobody ran. The `specs` job
runs it as its own step, where a skip fails. The unit halves -- the parse
option and the encoder's retry -- are in `tests/protocol-test.lisp`, in the
default suite.

Not covered: the other transports (stdio and HTTP share `process-json-line`
with TCP but not its framing), `structuredContent`, and the pool's lifecycle
under failure. Timeouts, crashes, cancellation and contention are Phase 4.

## Pool ownership (4A)

Phase 4 moves from the cl-spec tools to what runs them: the worker pool. 4A
is ownership. Which worker does the pool hold, which does it lend to which
session, which does it take back, and does it end every one it lets go of?
Deadlines and cancellation (4B), abnormal exits and worker replacement (4C)
and concurrency (4D) come after it.

**How the real pool is run.** `src/pool.lisp` reaches a worker's process only
through four specials: `*spawn-worker-function*`,
`*kill-worker-function*`, `*worker-alive-function*` and
`*start-pool-thread-function*`. Each defaults to the function production
uses, and nothing in the pool rebinds them. The health monitor's iteration is
`%check-worker-health`, which the monitor thread calls.
`specs/pool-fixtures.lisp` sets the specials for one run and puts them back:
- workers have no process;
- background work (standby replenishment, crash recovery) is queued and runs
  only when an operation asks for it;
- the health check runs as an operation.

The fakes and the declarations live under `specs/`. Nothing in `src/` refers
to them.

**Three records, kept apart.**
- The **ledger** is written by the fake lifecycle alone. It records every
  worker the pool was handed, every worker the pool ended, and every fake
  process that died.
- The **pool's own lists** (`*all-workers*`, `*standby-workers*`,
  `*affinity-map*`, read under its lock) are what the invariants are about.
- The **model** records what the operations promise a caller: which session
  was lent which worker, and which workers are known unusable. It says what
  must not happen, not how the pool should keep its lists, so it is not a
  second pool.

**Invariants.** Some hold between any two operations; others hold only at
rest, when nothing is queued. With work queued the pool is legitimately
between states.
- *Always:*
  - no worker is held twice;
  - every held worker is tracked, once;
  - no ended worker is held or tracked;
  - every worker the ledger says the pool was handed and did not end is
    tracked (a worker in no list and never ended is an orphan);
  - a mapped worker names its own session;
  - the pool answers for no more workers than its cap, counted from the
    ledger rather than by the pool's own arithmetic: every worker it was
    handed and has not ended, plus every spawn in flight. The pool's own
    count (`%effective-pool-size`) must agree with that; the pool decides
    every spawn by it, so an undercount spawns past the cap while agreeing
    with itself.
- *At rest:* no placeholder is left in the map; the tracked list is exactly
  the mapped workers and the standbys; and, in a run where no injected spawn
  failure was spent, the pool has its warmup of standbys as far as the cap
  leaves room (replenishment stops at a failed spawn and waits for the next
  event, so a spent failure excuses a short pool).
- *After a shutdown:* the pool holds nothing, and every worker it was handed
  has been ended.
- *Model:*
  - a newly lent worker is `:bound` to its session and not known unusable;
  - a worker is lent to one session only;
  - a session gets its worker back while that worker is usable, judged
    **before** the acquire: the worker bound to it then, `:bound`, not ended
    and not known unusable, is the one it must get. Judged after, an acquire
    that ended that worker itself would excuse its own replacement;
  - a binding the pool made on its own (crash recovery binding a
    replacement) is a lending too, checked the same way when it appears;
  - the worker a session held **before** a release or kill must be ended
    afterwards, whatever became of it in the lists: a release that kept it
    as a standby leaves every list consistent;
  - an acquire may be refused only for a reason judged from outside the pool:
    it was stopped; or it had nothing it could hand over -- no worker to give
    back to the session and no usable standby, both decided **before** the
    acquire -- and then either the spawn it had to make failed on a spawn
    failure injected for this acquire, with room by the ledger's count, or
    it had no room by that count. Anything else is an unexpected refusal: a
    spent spawn failure does not excuse an acquire that threw away a worker
    or a standby it could have handed over;
  - a refused acquire leaves nothing mapped for the session.

**The fixture's own cleanup.** After a run the fixture shuts the pool down and
drains the queue. Only then does it end, itself, any worker the pool left live.
Each such worker is reported as `:reaped-by-fixture`, so the cleanup cannot
make a leak disappear. `a-leak-is-reported-and-not-hidden-by-the-fixture`
plants one (`(:stray-spawn)`) and checks that it is reported as an orphan
while the pool runs, as live after the shutdown, and as reaped by the
fixture.

**Generated** (`specs/pool-ownership.lisp`, 2 properties):
`pool-ownership-holds-over-operation-sequences` (one standby, room for four)
and `pool-ownership-holds-when-the-pool-is-full` (two standbys, room for two,
so the standbys fill the pool). Each draws 5 to 30 operations over four
sessions:
- acquire, release and kill a session's worker;
- a worker's process dies;
- an RPC times out and marks a worker crashed;
- a health check;
- running the queued work;
- a spawn failure;
- shutdown and restart.

The generator does not shrink. A counterexample is the whole sequence, and
each violation names the operation index it followed.

The negative control swaps in seven wrong pools, and each must fail:
- a `release-session` that ends nothing;
- a `shutdown-pool` that ends nothing;
- a `get-or-assign-worker` that refuses every session;
- a `get-or-assign-worker` that ends the session's healthy worker and binds
  another on every call;
- a `get-or-assign-worker` that ends a usable standby before acquiring, so a
  failed spawn refuses a pool that had a worker to give;
- a `release-session` that keeps the worker as a standby;
- a `%effective-pool-size` that counts nothing.

**Fixed** (`tests/pool-ownership-test.lisp`, in the default suite): the checks
catch a leak, and tell at-rest from in-between. Regression cases pin the
three faults below. With real processes, a released session's process, a
crashed standby's, a dead standby's and every process left at a shutdown are
all gone afterwards (reaped, not zombies).

**What was found, and what changed.** Mapping the pool turned up three ways
it lost track of a worker it owned, and the generated sequences a fourth. The
fixed case for each fails against the old code. The generated sequences catch
the first two (18 of 200 random sequences failed against the old code) and
found the fourth; the third needs an ordering no sequential run produces.
- **A standby an RPC marked crashed was lent.** A timed-out RPC (the
  project-root broadcast reaches every standby) marks the standby `:crashed`
  and closes its connection, while the process lives on until the reaper gets
  to it. The standby loop checked only that the process was alive, so it
  bound the crashed worker to a session. It now also requires `:standby`.
- **A dead standby was dropped, not ended.** It left the lists, and its
  connection and stderr thread were left to nobody. It is now ended outside
  the lock. The capacity refusal, which used to be signalled inside the lock
  before anything could end it, now comes after.
- **A spawn that finished after a shutdown was registered.** `shutdown-pool`
  does not wait for on-demand spawns. A spawn completing afterwards was put
  into the emptied map, and the next `initialize-pool` forgot it. The process
  was never killed. `%spawn-and-bind` now registers only while the pool runs
  and the map still holds its own placeholder. Otherwise it ends the worker
  and signals `pool-shutting-down`. This one needs two threads in a fixed
  order (a spawn that has started, a shutdown, then the spawn finishing),
  pinned with semaphores. It is the one ordering test in 4A.
- **Recovery left a crashed standby on the standby list.** A standby whose
  process died and which an RPC then marked `:crashed` reaches recovery's
  `:crashed` arm. That arm removed it from `*all-workers*` and ended it, but
  left it on `*standby-workers*`: an ended worker, still offered as a
  standby and still counted when replenishing. It now leaves both lists, and
  the arm schedules replenishment for it as the `:standby` arm does -- it
  used to return having replenished only for a bound worker, leaving the pool
  a standby short.

The review of the first version of these checks found three ways they could
pass a wrong pool, and each is now a negative control as well as a fixed
case:
- an acquire that refuses everything, which keeps the lists empty and
  consistent (every refusal now needs a reason);
- a release that puts the worker back as a standby, unended (the worker a
  release must end is named before it runs);
- a pool size that counts nothing, so replenishment spawns past the cap
  (the cap is checked against the ledger's count).

A second review found a fourth: affinity was judged after the acquire, so an
acquire that ended the session's healthy worker and bound another excused
itself. The worker to keep is now decided before the acquire runs, and that
wrong acquire is a negative control and a fixed case too. A third found the
same flaw for standbys: whether one was usable was read after the acquire, so
an acquire that ended a usable standby and then failed to spawn excused its
refusal. Everything an acquire is judged against is now decided before it
runs (`%acquire-pre-state`), and that acquire is a seventh negative control.

Making the model check recovery's own bindings removed one false positive:
a replacement that died after recovery bound it was read as an unusable
worker newly lent.

Found and left for later phases (see *Known issues*): one crash counted twice
by the circuit breaker, and the pool briefly over its cap during recovery
(4D); `:crashed` overwriting `:released`, a cancelled spawn reporting the
wrong message, and a runtime owner left pointing at a dead worker (4C).

## Request lifecycle (4B)

4A made each worker's ownership first-class. 4B does the same for each
request: which request a cancellation acts on, what a request is reported to
have done, and that nothing runs twice.

**The record** (`src/request-lifecycle.lisp`). A proxied request is registered
under its session and its JSON-RPC id -- the session is part of the identity,
and the id is printed readably, so `1` and `"1"` stay two requests. It is
registered first of all, before a worker is found for it, and moves through
`:registered`, `:acquiring`, `:waiting-to-send`, `:executing` and
`:responded`. `WORKER-RPC` calls two hooks while it holds the worker's
stream: `before-send`, just before the bytes go out, and `after-receive`,
once the answer has been read. So `:executing` means "this worker is running
this request and nothing else".

**Cancellation** acts by phase, under the registry lock:
- before `:executing` it only marks the request, which stops at its next
  boundary -- after the worker is found, or in `before-send` -- without
  running, and its worker is left alone;
- at `:executing` the worker is signalled while the registry still holds the
  request there, and is remembered as stopped for a cancellation, so a
  request queued behind it on that worker is refused in `before-send` instead
  of being sent into a dying process;
- at `:responded`, or once the request is gone, nothing is done.

**Answer against cancellation.** An answer read from the worker is delivered
only through `note-response`, which orders it against a cancellation under
the same lock: if the cancellation got there first, it has already signalled
the worker, so the answer is withheld (`worker-rpc` signals
`rpc-answer-withdrawn`, reported as cancelled while running,
`execution-unknown`); otherwise the request becomes `:responded` and a later
cancellation is too late. Never both -- a success delivered from a worker a
cancellation stopped would describe a session whose state was just lost.

**Outcomes.** The phase a request reached decides what an error result the
proxy builds may say (`execution_status`): `not-executed` before
`:executing`, `execution-unknown` at it -- sent, no answer -- and
`completed` once the worker answered, an error answer included. A request
found a dead worker when its turn came is `not-executed`, not "timed out"
or "crashed" itself. `repl-eval`'s own timeout is an error with
`execution-unknown`, told by a sixth value rather than the raw value
`:timeout`, which an expression may return.

**Checking** (`specs/request-fixtures.lisp`). The real `proxy-to-worker`,
`worker-rpc` and `cancel-request` run over a real socket to a fake worker, a
TCP server in the image that keeps its own ledger of the requests it
received and answers, errors, drops the connection or holds a request as
scripted. Only the pool is stood in for. The ledger is the independent
account: a request the fake worker never received did not run. A scenario
fixes the worker's behavior for the request under test, where its
cancellation arrives (never, while its worker is found, while it waits
behind another request, while it runs, after its answer was read and before
it was delivered, after its answer, or from another session) and whether a
second request waits behind it -- 56 combinations, in fixed orderings, never
raced. The read-but-not-delivered point pauses `note-response` on a
semaphore, and the cancellation runs on a thread of its own, since ending
the worker waits for the stream the paused request holds.

The checks, against the ledger: one result per request, nothing sent twice;
an unsent request reports `not-executed` and a sent one never does; an
answered one gets its answer, a worker error `completed`, a dropped one
`execution-unknown`; a cancellation before the send withdraws the request
and keeps the worker, one during it stops the worker and the request behind
is told it did not run, and one after the answer or from another session
changes nothing; nothing is left registered.

- Generated: `request-lifecycle-keeps-its-promises` draws the scenarios.
- Fixed (`tests/request-lifecycle-test.lisp`, default suite): the registry,
  cancellation by phase, one case per fault below, the checks catching three
  wrong lifecycles, and every combination of the real one.
- Real worker (`tests/cancel-test.lisp`): a running `(sleep 30)` is
  cancelled, its worker stopped, the result `execution-unknown`, and the
  session goes on with a fresh worker.
- Negative control: a cancellation that stops the worker whatever the
  request's phase, an account that calls every request completed, an answer
  published although a cancellation got there first, and a lookup that
  ignores the session.

**What was found, and what changed.**
- **a. A cancellation while the worker was being found was lost**, and the
  request then ran: the request was registered, but a cancellation found no
  worker to stop, removed the entry and returned. Now it marks the request,
  which stops before it is sent.
- **b. A cancellation stopped the session's worker, not the request**: a
  request waiting behind another had the other one killed, and a cancellation
  just after an answer could kill the session's next request. Now the worker
  is stopped only while it runs the request named.
- **c. Requests were keyed by id alone**, and every session numbers its
  requests from the same small integers: two sessions' requests with the same
  id overwrote each other, and the first to finish removed the other's entry.
- **e. A request that never ran was reported as timing out or crashing
  itself** -- the request queued behind one that timed out was told "Worker
  RPC timed out ... took too long". Now it is told it was not run.
- **f. A `repl-eval` timeout was a successful result.** Now it is an error
  whose outcome is unknown.
- **Found in review: an answer and a cancellation could both be delivered.**
  The answer was read, a cancellation then found the request still
  `:executing` and stopped the worker, and the answer was published anyway.
  They are now ordered at one point, as above.

Left for later: the reset notice after a stopped worker (fixed in 4C, see
*Reset events*), no deadline on the stream lock and the write to the worker
(4D), and the worker's idle read timing out in the middle of a line (the
transport/deadline phase) -- see *Known issues*.

## Reset events (4C)

When a worker ends, the session it was bound to loses its Lisp state. 4C
makes each such loss an event, told to that session exactly once, and says
truthfully why it happened.

**The ledger** (`src/reset-events.lisp`). One worker's end is one event:
which worker, which session's state it held, why it ended, how its process
exited, and whether the session has been told. Causes are `:crashed`,
`:timeout` (the proxy's deadline abandoned it), `:retired` (it exited rather
than serve a request while carrying a leaked thread), `:cancelled`, `:killed`
(pool-kill-worker), `:released`, `:shutdown` and `:stopped` (the pool ended
it for any other reason -- a fallback nobody should need).

- **The first cause recorded is kept.** A decision is recorded before the
  signal that carries it out: `cancel-request` records `:cancelled` before
  SIGTERM, `kill-session-worker` `:killed`, `release-session` `:released`,
  `shutdown-pool` `:shutdown`. The EOF, stream error or dead process that
  follows adds only exit details (`amend-termination-exit`), never a new
  cause. A worker the pool already let go stays `:released` or `:dead`;
  `%mark-worker-crashed` no longer turns it `:crashed`.
- **Owed only by a bound worker.** An event is owed to the session the worker
  was bound to when it ended. A standby's end, a released session's worker
  and a shutdown owe nobody. A release discards what its session was owed, so
  a session reusing the id is not told someone else's loss; a shutdown
  discards everything.
- **Told once, by whoever claims it.** `claim-session-resets` hands a
  session's pending events to one response and marks them delivered:
  the failed request that met the death (in band), the session's next
  request in its place (out of band, `not-executed`), a pool error, or the
  pool-kill-worker response, which receives them as `kill-session-worker`'s
  second value and names each one, its own kill included. Several
  pending events are told together, oldest first, in one response. Nothing is
  copied to a replacement worker any more: the per-worker
  `needs-reset-notification` flag, the owed-reset table and the hand-off
  code are gone.

Each told worker is one sentence, `Worker <id> <why>`, followed once by what
it cost: `This session's Lisp state (loaded systems, defined functions,
package state) was lost with it.` Only what the response knows is said:
`The session is now using another worker.` is added only where this response
acquired one (the out-of-band notice, a cancellation after a successful
acquire, `pool-kill-worker` with `reset`), and nothing is ever promised about
a worker still to come. The pool-kill-worker response renders its own kill
the same way, through the same `reset-notice`.

**Object ids** (`src/object-registry.lisp`) are opaque strings,
`o-<generation>-<n>`, the generation 128 random bits. A registry draws its
generation when it first hands out an id, and a new one whenever it is cleared. A worker that is replaced
numbers its objects from 1 again, so an integer id from the old image used to
name whatever the new one registered first; now `inspect-object` refuses it
as `OBJECT_STALE`. An id of this generation whose object was evicted is
`OBJECT_NOT_FOUND`, and a string that is not an id `INVALID_OBJECT_ID`; an
integer from an older client is refused by the argument check before any
lookup (`id must be a string`).

**Checking** (`specs/reset-fixtures.lisp`). The pool runs with fake workers
as in 4A, and a request is the real `proxy-to-worker` with only the socket
replaced: the fake RPC answers, or meets the worker's death as `worker-rpc`
does. The harness keeps its own account -- which worker each session was
lent, which ended while bound and how, which losses were excused by a
release or shutdown -- and reads what was told from the responses' text
(`Worker <id> <phrase>`), a kill's included: `pool-kill-worker` is called as
the real tool, never read from the ledger or from what the pool hands the
tool. A notice that says the session is using another worker must match a
worker the pool holds bound for it. After every operation, and after a drain in which every
session asks until a request reaches its worker: each loss told at most once,
to its own session, only after it happened and never once excused, with a
cause the harness applied; at the end every loss told, the ledger empty, and
after the shutdown nothing left owed. 4A's ownership invariants are checked
in the same runs.

- Generated: `resets-are-told-exactly-once` (one standby, room for four) and
  `…-when-the-pool-is-full` (two and two) draw pool operations, requests,
  requests whose worker dies under them, and RPC timeouts on the worker a
  session holds. `object-ids-never-outlive-their-image` draws registrations,
  lookups, clears and replaced images over two registries: an id is found
  only where it was issued and until a clear, refused as stale anywhere else,
  never answered with another object.
- The request-lifecycle scenarios (4B) also count the notices: a worker
  stopped or dropped is named exactly once across the scenario's responses,
  none otherwise, and nothing is left owed.
- Fixed (`tests/reset-events-test.lisp`, default suite): the ledger's rules,
  one sequence per defect below, three orderings fixed on a held fake worker
  (a cause found first survives the EOF; a kill during a request is told as
  a kill, by the kill; a cancellation is told once, as one), a pending reset
  crossed with a cancellation during a successful acquire (told once; the
  acquired worker kept and said to be in place; no future worker promised),
  a pool error telling a reset, and the checks catching seven wrong
  implementations. Real processes: `pool-test` (a
  replaced worker's object id is stale), `worker-leaked-thread-test` (a
  retirement is owed, told and discarded as above).
- Negative control: a claim that leaves the events pending, a claim that
  takes nothing, a later record replacing the first cause, a pool-kill-worker
  response that drops the resets it claimed, and a lookup by number alone.
- `:about`: each reset property names the surfaces it drives -- the proxy and
  the pool-kill-worker tool's handler -- and every function a negative
  control breaks for it, plus the release and shutdown paths whose discard
  it claims. `specs-runner-test` checks that every function a reset negative
  control replaces is an `:about` target of the property that must catch it.

**What was found, and what changed.**
- **A1. A cancellation's reset was told twice** -- the proxy cleared the
  worker's flag, and `kill-worker` set it again -- and the second notice said
  "crashed and was restarted".
- **A2. pool-kill-worker left an older untold reset** for the next request.
  Its response now tells it.
- **A3. A death met by a request and found by the health monitor** was told
  by both, with different reasons.
- **A4. Of two deaths before either was told, only the first was kept.**
- **B5. A request running when its worker was killed or released** was told
  the worker crashed and was restarted: the EOF overwrote `:released` with
  `:crashed`.
- **B6. Messages said "was restarted", "was terminated" and "has been
  reset"** before any of it had happened. They now say what is known: the
  state was lost, and -- only where this response acquired one -- that the
  session is using another worker. (Found in review: the first version still
  promised "the session's next request starts a new worker", which nothing
  knew; and a cancellation after a successful acquire said so while the
  session kept the worker it had just acquired.)
- **B7. A request a worker retired on was "may have run"**; the worker
  retires on receiving a request, before running it, so it is `not-executed`.
- **B8. `exit_status=running`** was shown for a process that had not exited;
  only an exit code or signal is shown now.
- **B9. A cancelled spawn's message** was overwritten by "failed to start".
- **C10. `*runtime-owner*` kept naming a dead worker**; when its worker
  crashes it now forgets the worker but keeps the session, so another live
  session cannot take the runtime over. It is cleared on release, shutdown
  and pool-kill-worker (the explicit re-arm, as before).
- **C11. Object ids were per-image integers**, so a stale id resolved to
  another object.
- **Unsupported wording.** `load-system`'s response carried `worker_healthy`,
  an undocumented field that said only that the load never started; it is
  now an internal key the builder removes. `make-timeout-result` no longer
  calls a worker healthy after a timeout.

The external `worker_reuse` field for every tool is a separate phase.

## Concurrency and shutdown (4D)

4A checked the pool's ownership one operation at a time. 4D checks it when
operations overlap -- clients acting at once, background work running on its
own threads, a shutdown arriving in the middle -- and what waiting for a
worker costs a request.

**Work in flight outside the lock.** Spawning and ending a worker take
seconds, so the pool does both outside `*pool-lock*`, and in between the
worker is in none of its lists. Each is now accounted for from the moment it
is decided, in the critical section that decides it: a spawn is counted
(`%begin-spawn`) until its worker is registered -- in the same critical
section, so it is never counted twice -- or ended; a worker taken out of the
lists is listed as ending (`%begin-ending`) until it is ended (`%end-worker`).
The pool's size, which the cap limits, is the workers it tracks plus the
spawns in flight: a replenishment's spawn, which has no placeholder, counts
from the start, and a worker being ended does not.

**Shutdown.** `shutdown-pool` stops the pool under its lock, joins the
health monitor, waits for every spawn and ending in flight
(`%wait-for-work-in-flight`, bounded by the worker startup timeout plus 15
s), waits for the replenishment and recovery threads, then records every
worker it still holds as ended by the shutdown, signals each -- so an RPC
blocked on one lets go of its stream, as `release-session` already did --
and ends them. When it returns, the pool owes nothing. Background work notes
the pool generation it was started for (`*pool-generation*`), and a
replenishment still spawning when its pool stopped gives its worker to no
later pool.

**Crash recovery and the breaker.** `%handle-worker-crash` now decides
everything about a death in one critical section: the worker is published
`:crashed`, its death counted against the breaker
(`%count-crash-against-breaker`) -- an acquire meeting it cannot count it
again -- taken out of every list, and, for a bound one, the session's entry
replaced by a placeholder whose spawn is counted. The replacement is spawned
by `%spawn-and-bind`, like an acquire's; a request for the session waits on
the placeholder instead of spawning a second one.

**Waiting for a worker.** A request waits for its session's worker while
another request of the session runs on it. `worker-rpc` now takes
`:lock-timeout` and `:while-waiting` (`%call-with-stream-held`), and the
proxy passes both: the wait is bounded by the request's own RPC budget, and
ends within a tenth of a second of the request being cancelled. Either way
the request was not sent: `not-executed`. The deadline on the exchange now
covers writing the request as well as reading the answer. A full pool
refuses at once (`pool-capacity-exceeded`, `not-executed`); there is no
queue, and 4D keeps it that way.

**Checking** (`specs/concurrency-fixtures.lisp`). The real pool with fake
workers, whose background work runs on real threads, and a fake lifecycle
with a ledger of its own: every spawn and ending and when it completed,
which processes died, every thread the pool started.

- *Shutdown scenarios* fix with semaphores what is in flight when
  `shutdown-pool` is called -- an acquire's spawn, a replenishment's, a
  recovery's, a worker being ended, an RPC holding a worker's stream -- and
  open the gates only once the shutdown has begun, in a drawn order, with or
  without an acquire arriving after. Deterministic. The checks: the shutdown
  returns, within a bound and without anything but its own signal letting a
  held RPC go; afterwards every worker is ended, no spawn or ending completes
  later, the lists and counts are empty, and no pool thread runs; and no
  acquire it overtook, or made after it began, was lent a worker.
- *Concurrent runs* let two to four clients apply drawn operations at once,
  while an observer checks, under the pool's lock and then the ledger's,
  what must hold at every moment: nothing held or tracked twice, nothing
  ended held, the size within the cap, and every live worker tracked, being
  spawned or being ended. A shutdown made while they run ends each run,
  judged as above, and no worker is lent to two sessions or after the
  shutdown. The interleaving is the scheduler's: a seed replays the
  operations, not the run.

- Generated: `pool-shutdown-leaves-nothing-behind` draws scenarios;
  `pool-holds-while-operations-overlap` draws plans.
- Fixed (`tests/concurrency-test.lisp`, default suite): each in-flight kind
  alone and all together; the checks catching four wrong implementations;
  one ordering per defect below; the waits; the full pool. Real process
  (`pool-test`): a shutdown behind a running `(sleep 60)` returns at once
  and the worker's process is gone.
- Negative control: a shutdown that does not signal its workers, one that
  does not wait for work in flight, and a pool that takes a worker out to
  end it without accounting for it.

**What was found, and what changed.**
- **Shutdown waited behind a running request**: it ended workers without
  signalling them, and ending one waits for its stream.
- **Shutdown returned while it still owed work**: it waited about 5 s for
  replenishment and not at all for an acquire's spawn or a worker being
  ended elsewhere.
- **A replenishment could feed the next pool** after its own was shut down.
- **One crash could count twice** against the breaker.
- **Recovery could take the pool over its cap**, and an acquire for the same
  session spawned a second replacement beside it.
- **A surplus standby was ended under `*pool-lock*`**, stopping every session
  for up to two seconds.
- **A queued request waited without a deadline**, and a cancelled one went on
  waiting until the request ahead of it finished.

Left: the pool's own RPCs wait for a worker's stream without a deadline, and
the worker's idle read timing out mid-line is for the transport/deadline
phase.

## Dependencies

```
cl-mcp/specs ──> cl-mcp/src/utils/{strings,sanitize,paths}, cl-mcp/src/fs,
                 cl-mcp/src/spec-core-record, cl-mcp/src/spec-adapter-{core,report},
                 cl-mcp/src/tools/spec-entry, cl-mcp/src/pool, cl-mcp/src/proxy
             ──> cl-spec/main, cl-spec/src/backends/check-it

cl-mcp (load, run) ──X──> cl-mcp/specs, cl-spec
tests.lisp ──> cl-mcp/tests/path-specs-test ──> cl-mcp/specs/path-fixtures
           ──> cl-mcp/tests/write-path-specs-test ──> cl-mcp/specs/write-fixtures
                                                       ──> cl-mcp/specs/path-fixtures
           ──> cl-mcp/tests/spec-core-record-test ──> cl-mcp/specs/core-record-fixtures
           ──> cl-mcp/tests/check-verdict-test ──> cl-mcp/specs/check-verdict-fixtures
                                                ──> cl-mcp/specs/core-record-fixtures
           ──> cl-mcp/tests/check-routing-test ──> cl-mcp/specs/check-routing-fixtures
                                                ──> cl-mcp/specs/core-record-fixtures
           ──> cl-mcp/tests/suite-judge-test ──> cl-mcp/specs/suite-judge ──> rove
           ──> cl-mcp/tests/spec-inspection-test ──> cl-mcp/specs/spec-inspection-fixtures
           ──> cl-mcp/tests/pool-ownership-test ──> cl-mcp/specs/pool-fixtures
                                                 ──> cl-mcp/src/pool
           ──> cl-mcp/tests/request-lifecycle-test ──> cl-mcp/specs/request-fixtures
                                                    ──> cl-mcp/src/proxy
               (no cl-spec; not the bundle)
self-test   ──> cl-mcp/tests/core-record-specs-test ──> cl-spec (opt-in)
integration ──> cl-mcp/tests/spec-integration-test, cl-mcp/tests/check-routing-specs-test,
                cl-mcp/tests/spec-inspection-specs-test, cl-mcp/tests/spec-responses-specs-test,
                cl-mcp/tests/spec-wire-test ──> cl-spec (opt-in)
            ──> cl-mcp/tests/spec-api-resolution-test ──> NO cl-spec, by design
                all judged by cl-mcp/specs/suite-judge
```

Nothing in `cl-mcp.asd` or `main.lisp` refers to the bundle. `cl-mcp` is a
package-inferred system, so `cl-mcp/specs` (`specs.lisp`) and its subsystems
(`specs/*.lisp`) exist without any `.asd` entry. The runner's own tests,
`cl-mcp/tests/specs-runner-test`, the real-record tests,
`cl-mcp/tests/core-record-specs-test`, the real routing tests,
`cl-mcp/tests/check-routing-specs-test`, the real inspection tests,
`cl-mcp/tests/spec-inspection-specs-test`, the wire tests,
`cl-mcp/tests/spec-wire-test`, and the API resolution tests,
`cl-mcp/tests/spec-api-resolution-test`, are left out of `tests.lisp`.
(`cl-mcp/tests/spec-integration-test` is in it, and skips there when cl-spec
cannot be found.)
The default suite does load these tests and the fixture libraries they use:
- `tests/path-specs-test.lisp`, with `specs/path-fixtures.lisp`;
- `tests/write-path-specs-test.lisp`, with `specs/write-fixtures.lisp`;
- `tests/spec-core-record-test.lisp`, with `specs/core-record-fixtures.lisp`;
- `tests/check-verdict-test.lisp`, with `specs/check-verdict-fixtures.lisp`;
- `tests/check-routing-test.lisp`, with `specs/check-routing-fixtures.lisp`;
- `tests/suite-judge-test.lisp`, with `specs/suite-judge.lisp`;
- `tests/spec-inspection-test.lisp`, with `specs/spec-inspection-fixtures.lisp`;
- `tests/pool-ownership-test.lisp`, with `specs/pool-fixtures.lisp`;
- `tests/request-lifecycle-test.lisp`, with `specs/request-fixtures.lisp`.

None of them needs cl-spec or loads the bundle.

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

The write boundary works the same way; run `symbol=` on each function, since
`fs-write-file` has two of the four properties:

```text
spec-symbol  {"symbol": "cl-mcp/src/utils/paths:ensure-write-path"}
spec-check   {"symbol": "cl-mcp/src/utils/paths:ensure-write-path", "profile": "normal",
              "timeout_seconds": 300}
spec-check   {"symbol": "cl-mcp/src/fs:fs-write-file", "profile": "normal",
              "timeout_seconds": 300}
```

These run inside your worker. Each trial creates its scratch tree under the
worker's temporary directory, and registers its ASDF system in the worker's
image, which is the image that runs the functions under test. A pass there
checks those functions in that worker. It is not an end-to-end check of the
parent server's file tools. The parent keeps running the code it started with
until the server restarts, so a fix to `ensure-write-path` does not reach the
`fs-write-file` tool, or `lisp-edit-form`, before then. Do not try a write-path
change by pointing the MCP write tools at the working tree.

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
   (for the read functions: `utils-paths-test` and `path-specs-test`; for the
   write functions: `utils-paths-test`, `write-path-specs-test` and `fs-test`).
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
from each property's `:trials` table: 200 for the string properties, 12 for the
read- and write-access ones, and 25 for the record, verdict, routing and inspection ones. Function Specs run with 200 trials. Each target has a
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
CL_MCP_SPECS_MODE=integration CL_MCP_SPECS_SUITE=cl-mcp/tests/spec-integration-test \
  ros run --load scripts/check-specs.lisp
CL_MCP_SPECS_MODE=integration CL_MCP_SPECS_SUITE=cl-mcp/tests/check-routing-specs-test \
  ros run --load scripts/check-specs.lisp
```

`sbcl --non-interactive --load scripts/check-specs.lisp` also works when
Quicklisp and cl-spec can be found without Roswell. The script puts its own
checkout first in ASDF's search. Exit status: `0` passed, `1` the checks ran and
something failed, `2` the script could not run them.

- `self-test` runs `cl-mcp/tests/specs-runner-test`,
  `cl-mcp/tests/path-specs-test`, `cl-mcp/tests/write-path-specs-test` and
  `cl-mcp/tests/core-record-specs-test`, and fails if any of them loads no
  test. The last one needs cl-spec, so a missing cl-spec fails it rather than
  skipping it. The runner
  tests use small fixtures, each registered in a registry made for that test. They check that
  the runner refuses a failing property, an empty selection, an unregistered
  name, zero trials, an undeclared profile, a contract that rejected every
  input, a rejection count below zero or above the trials, an unreached case,
  a generator error and a timeout. They also check registration,
  re-registration, the printed and written reports, the worktree fingerprint
  (on a scratch git repository) and a full bundle run. You can run the same
  tests with `run-tests system=cl-mcp/tests/specs-runner-test`.
- `integration` runs the one real-cl-spec suite `CL_MCP_SPECS_SUITE` names,
  `cl-mcp/tests/spec-integration-test` or
  `cl-mcp/tests/check-routing-specs-test`, `cl-mcp/tests/spec-inspection-specs-test`,
  `cl-mcp/tests/spec-responses-specs-test`, `cl-mcp/tests/spec-wire-test`
  or `cl-mcp/tests/spec-api-resolution-test`, and judges it from Rove's
  per-test results (see *Routing*).  The last one is the odd case: it needs a
  process with no cl-spec, and refuses to run when one is there. A test that is missing, failed, skipped or asserted
  nothing fails the run with status `1`. A suite it does not know, or one that
  does not load, exits `2`. Both suites swap the cl-spec registry, or bind
  one, while they run: use a process of their own, not the MCP worker you
  are working in.
- `negative-control` swaps in thirty-six wrong implementations, one at a time:
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

  - an `ensure-write-path` that refuses every path. The project, writer and
    safe-spelling properties must fail.
  - the resolver from before the fix, which trusts new names below a link. The
    link and writer properties must fail.
  - one that allows whatever the read policy allows, so an absolute project
    path and a registered dependency become writable. The refusal property
    must fail.
  - one that sends a string with a doubled separator to the file beside the
    right one, inside the project, and defers to the real function otherwise.
    The safe-spelling property must fail. The project property runs against it
    too, and passes: no existing generator spells `//`. Among the fixed Rove
    tests, `write-allows-project-targets-as-their-real-path` (its `src//./new.txt`
    case) catches it as well.

  - a `field-availability` that reads a key present with NIL as absent. The
    availability property must fail.
  - a `project-record` that turns a record's seed back into a JSON number. The
    seed property must fail.
  - a `project-core-record` that claims a complete projection whatever its
    issues say. The cut property must fail.
  - a `project-record` that keeps the tail of an over-long list. It still cuts
    the list and reports the cut correctly, so only the kept items are wrong.
    The cut property must fail. Of the default suite's record tests,
    only the new limit test catches this one. The first two are also caught by
    older fixed tests (`present-nil-is-not-absent` and
    `a-seed-is-decimal-text-even-inside-the-safe-range` among them).

  - a `%counts` that drops every status without a field of its own from
    `other` and `by_status`, while `selected` still counts it. The counts
    property must fail.
  - a `%contract-plist` that publishes the raw trial count as effective trials
    whenever the refusal count cannot be subtracted with. The effective-trials
    property must fail.
  - a `%verified-p` that verifies an empty list of results. The verified
    property must fail.
  - a `%verified-p` that judges results as if every declared case had been
    reached. The verified property must fail.
  - a `%verification-gaps` that drops `rejection-counts-unmeasured` when any
    one result, rather than every one, has a usable count. The gap property
    must fail: every trial's mixed list pairs a usable count with a result
    without one.

  - a `%select-properties` that also runs a symbol's own contract or same-named
    property on `symbol=`. The selection property must fail: every trial
    selects against a registry holding both.
  - a `%trials-budget` that ignores the profile entry and explicit trials for
    the backend default. The budget property must fail.
  - a `parse-seed-string` that takes the seed through a double. The seed
    property must fail on 2^53+1, which every trial reads.
  - a `%definition-match` that counts an incomplete digest equal to the
    expected one as a match. The digest property must fail.

  - a `list-report` that reports a count it never looked for as 0. The
    listing property must fail.
  - a `%property-listing` that reads a row without the registry it was given,
    so the reader answers out of whatever registry the image holds. The rows
    still come back right — the stub answers from its own descriptor either
    way — so only the recorded call shows it, and the listing property must
    fail.
  - a `%describe-function-spec` that reports every argument as required, and
    one that reports a cut precondition as complete. The declaration property
    must fail for each.
  - a `definition-digest` that digests from the readers when the record's own
    digest was refused. The digest-source property must fail.

  - a `build-spec-list-response` that publishes a count nobody took as 0, and
    five `build-spec-check-response`s: one that publishes a verified of false
    as null (the fault the worker boundary has today, planted at the builder),
    one that opens every headline with VERIFIED while every field beside it
    stays right, one that publishes the evidence to the text and not to the
    payload, one that lists one gap fewer in the text than in the payload, and
    one that replays a contract as `property=`. The response properties must
    fail for each.

  Each wrong record, verdict, routing and inspection function calls the real
  one and bends one rule of its answer. The verdict properties' own outcome is read from cl-spec's
  result, as for every control; `verified` and the MCP rendering play no part
  in judging a control.

  `resolve-readable-path` is not replaced; it calls `allowed-read-path` for
  its decision. Nothing is written to a path that should be denied.
  `fs-write-file` is not replaced either; it calls `ensure-write-path`. A wrong
  `ensure-write-path` does make `fs-write-file` write where it should not, but
  only into the check's own scratch tree, where the write is observed and then
  adopted by cleanup.

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
4. Runs `self-test`, `check`, `negative-control` and `integration` for each of
   the five real-cl-spec suites (`spec-integration-test`,
   `check-routing-specs-test`, `spec-inspection-specs-test`,
   `spec-responses-specs-test`, `spec-wire-test`) and for the API resolution suite, as separate
   processes, each under `timeout 900` inside a 30-minute job, and uploads the
   report files.

check-it and cl-mcp's other dependencies come from the current Quicklisp dist;
the report records which one. The job uses no cache. The default `test` job is
unchanged and still needs no cl-spec. There, `spec-integration-test` skips
when cl-spec cannot be found, as before; the `specs` job is where a skip fails.
The Lint job covers `specs.lisp`,
`specs/*.lisp` and `scripts/*.lisp` as well as `src/` and `tests/`.

To reproduce the job locally, clone cl-spec at the pinned commit where
Roswell's local-projects can see it, then run the commands above from the
checkout.

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

- **Requests, for the transport/deadline phase.** The worker's idle read
  (`*worker-read-timeout*`) is a `with-timeout` around `read-line`; firing
  mid-line, it drops the partial line and parses the rest as a request, whose
  `id nil` reply the parent reads as a protocol error.
- **Requests and pool, fixed in 4D.** A queued request's wait for the
  worker's stream now has a deadline and ends when it is cancelled; the
  breaker counts a crash once; recovery counts against the cap from the start;
  a surplus standby is ended outside `*pool-lock*` (see *Concurrency and
  shutdown*). Waiting for the stream in the pool's own RPCs (the root sync,
  the init monitor's polling) is still unbounded.
- **Pool, fixed in 4C.** `release-session`'s in-flight RPC no longer marks
  the released worker `:crashed`; a cancelled spawn keeps its own message;
  `*runtime-owner*` forgets a dead worker and is cleared on release and
  shutdown (see *Reset events*).

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
  OS here is a policy question. `ensure-write-path` no longer has the problem:
  it refuses `..` right after a link (see *Write access*).
- Every denied path costs `allowed-read-path` a lookup of every registered ASDF
  system by name. That runs `find-system`, which reloads `.asd` files whose
  systems do not match their file names: check-it's, once cl-spec is loaded.
  In a worker with about 200 systems this took about 46 ms per denied path,
  with two ASDF warnings each time.
