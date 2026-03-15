# Parent-Side Package Context Synthesis for `lisp-edit-form`

**Date**: 2026-03-15
**Status**: Proposed
**Branch**: main

## Problem

`lisp-edit-form` and `lisp-read-file` run in the parent process. The parent does
not normally load user systems, so package objects that exist in a worker may be
absent in the parent.

The existing lenient reader fixes one class of failure:

- unknown package-qualified symbols like `fake-pkg:foo`

It does **not** fully solve package-local nickname resolution:

- `ad:foo` should resolve via the current package's local nickname table
- that table exists only if the parent process has the package object with the
  right local nickname definitions

Today, `parse-top-level-forms` updates `*package*` on `in-package`, but only if
`find-package` succeeds in the parent. If the package does not exist there, the
reader falls back to treating `ad` as an ordinary package designator, which is
not semantically equivalent to a local nickname.

## Goals

1. Allow `lisp-edit-form` to parse files and replacement content that rely on
   package-local nicknames without requiring full user-system loading in the
   parent process.
2. Preserve the current parent/worker isolation model.
3. Keep temporary package objects isolated and cleaned up after the tool call.
4. Reuse the same mechanism for `lisp-read-file` collapsed mode.
5. Continue to support the current lenient handling of unknown package prefixes.

## Non-Goals

- Fully reproducing all package semantics of a loaded user system.
- Executing user `defpackage` helper code or arbitrary top-level forms.
- Replacing the worker with the parent as the authoritative runtime image.
- Solving unrelated reader macro issues beyond the existing `readtable` support.

## Current Behavior

### What works now

- Unknown package prefixes are handled by ephemeral stub packages created during
  read via `call-with-lenient-packages`.
- If the parent already has the package named by `in-package`, local nicknames
  on that package work because `*package*` is updated during parsing.

### What still fails

- If the file's package does not exist in the parent, `in-package` cannot
  switch `*package*` to a package with the right local nickname table.
- Replacement content for `lisp-edit-form` is parsed separately from the file
  body, so it needs the same package context even when the content snippet does
  not itself contain `in-package`.

## Proposed Approach

Add a parent-side **package context synthesis** layer that reconstructs just
enough package metadata to let the reader resolve package-local nicknames.

The core idea is:

1. Discover the target package for the file.
2. Find a corresponding `defpackage` or `uiop:define-package` form in project
   source when the package is not already present in the parent.
3. Materialize a temporary package object in the parent with:
   - canonical package name
   - nicknames
   - `:use` list when safe
   - local nickname mappings
4. Run file parsing and replacement-content validation inside that temporary
   package context.
5. Delete any temporary packages and nickname target stubs after the operation.

This preserves the current design:

- the parent still does structural file parsing/editing
- the worker still owns runtime state and evaluation

## Why This Approach

- It fixes the actual gap: missing package metadata in the parent.
- It avoids loading arbitrary user systems into the parent process.
- It composes naturally with the existing lenient package handling.
- It keeps file editing in one process, avoiding new cross-process write paths.

## Alternatives Considered

### 1. Load user systems in the parent

Rejected. This breaks the isolation introduced by the worker pool and recreates
the original state-sharing/staleness problems.

### 2. Proxy `lisp-edit-form` to the worker

Rejected for this design. It would require moving filesystem write authority and
structure-preserving edit logic into the worker or adding a new round-trip for
every edit. That is a larger architectural change than needed here.

### 3. Keep relying on stub packages only

Rejected. Stub packages solve unknown package names, but they do not recreate
the local nickname table of the current package.

## Design

### New Module: `src/package-context.lisp`

Add a new helper module responsible for:

- discovering package definitions
- extracting a minimal package spec
- creating temporary parent-side package objects
- cleaning them up after use

Suggested public API:

```lisp
(call-with-file-package-context file-path thunk)
(call-with-package-context package-name thunk)
(discover-package-spec package-name &key project-root)
```

Suggested internal structures:

```lisp
(defstruct package-spec
  name
  nicknames
  use
  local-nicknames
  source-path
  source-form)

(defstruct synthesized-package-context
  root-package-name
  created-packages
  reused-packages)
```

`created-packages` is needed for cleanup and for integration with lenient symbol
resolution.

### Package Discovery

When a file is parsed for editing:

1. Read enough of the file to find its first `in-package` form.
2. If that package already exists in the parent, reuse it and skip synthesis.
3. Otherwise, locate a source form that defines the package.

Discovery order:

1. `defpackage` / `uiop:define-package` forms in the same file before the first
   non-package top-level form.
2. Project-wide search for top-level package definitions matching:
   - canonical package name
   - declared nicknames
3. Optional later optimization: cache by package name and file mtime.

Project-wide search can start with a text filter (`rg`) and then parse only the
candidate forms structurally. The implementation does not need to parse the
whole project eagerly.

### Supported Package Forms

Initial support should cover:

- `defpackage`
- `uiop:define-package`

The extracted subset should be intentionally narrow:

- package name
- `:nicknames`
- `:use`
- `:local-nicknames`

Everything else is ignored for now.

This is enough for reader package resolution. Import/export directives are not
needed to model local nickname activation. Unknown symbols referenced through
temporary target packages can still be handled by the existing lenient reader.

### Materialization Rules

#### Root package

If the target package does not already exist:

- create a temporary package with the real package name
- attach declared nicknames when there is no conflict
- add safe `:use` packages that already exist

If a `:use` package does not exist in the parent:

- do not auto-load it
- optionally create a stub only if required by a later read failure

#### Local nickname targets

For each `(:local-nicknames (...))` entry:

- if the target package exists, use it
- otherwise create a temporary stub package for the target package name
- register the local nickname on the synthesized root package

The target package does not need a full API surface. When the reader later sees
`ad:foo`, the existing lenient machinery can still intern/export symbols into a
temporary target package if that package is marked as managed by the current
context.

### Integration with Lenient Reader

The current `call-with-lenient-packages` only tracks packages it created itself.
That is not sufficient once package-context synthesis pre-creates:

- the root package
- local nickname target stubs

We need a unified wrapper, for example:

```lisp
(call-with-managed-package-context context thunk)
```

Responsibilities:

- establish temporary packages before read
- expose the set of managed packages to the lenient reader handlers
- allow symbol-missing recovery for synthesized stub packages
- delete all created packages on unwind

This can be implemented either by:

1. extending `call-with-lenient-packages` to accept an initial package set, or
2. introducing a new dynamic variable consumed by the existing handlers

The second option is less disruptive and preserves the current API for callers
that do not need package-context synthesis.

### Changes to `src/cst.lisp`

Extend `parse-top-level-forms` to accept source context:

```lisp
(parse-top-level-forms text &key readtable source-path initial-package)
```

Behavior:

- if `source-path` is provided, call `call-with-file-package-context`
- if `initial-package` is provided, bind `*package*` before parsing
- continue updating `*package*` when `in-package` forms are encountered

This preserves the existing behavior for already-loaded packages while enabling
synthetic package activation when the parent lacks them.

### Changes to `src/lisp-edit-form-core.lisp`

`%locate-target-form` should pass the file path into `parse-top-level-forms` so
file parsing runs under synthesized package context.

It should also return the resolved root package name or context handle so that
replacement-content validation can reuse the same package context.

### Changes to `src/lisp-edit-form.lisp`

`%validate-and-repair-content` needs a new optional argument describing the
package context to use when reading replacement content.

Suggested shape:

```lisp
(%validate-and-repair-content content readtable-designator package-context)
```

`lisp-edit-form` flow becomes:

1. locate target form under file package context
2. validate replacement content under the same package context
3. apply structural edit

This is required because replacement content typically omits `in-package`, yet
still uses the file's local nicknames.

### Changes to `src/lisp-read-file.lisp`

Pass `source-path` to `parse-top-level-forms` so collapsed read mode benefits
from the same package-context synthesis without separate logic.

## Cleanup and Safety

Temporary objects created by package-context synthesis must be deleted after the
read/edit operation.

Rules:

- only delete packages created by the synthesis layer during the current call
- never delete or mutate a pre-existing real package
- if nickname attachment would mutate a pre-existing package, reuse the package
  as-is and do not patch it
- if a required nickname cannot be represented without mutating a real package,
  signal a clear error

This keeps the parent process side-effect free across tool calls.

## Failure Modes

### Package definition not found

Return a targeted error:

> Could not reconstruct package context for PACKAGE. Load the package in the
> parent process or provide a package definition file under project root.

### Conflicting nickname

If a synthesized package nickname conflicts with an existing package:

- prefer safety over cleverness
- signal an explicit conflict error
- do not silently bind the nickname to a different package

### Unsupported `uiop:define-package` features

If the package definition uses advanced options not covered by the minimal
extractor:

- ignore options that do not affect reader resolution
- signal a clear error only when an omitted option prevents correct package or
  local nickname reconstruction

## Test Plan

### Unit tests

Add tests for package-spec extraction:

1. `defpackage` with `:nicknames`
2. `defpackage` with `:local-nicknames`
3. `uiop:define-package` with `:local-nicknames`
4. package discovery by canonical name
5. package discovery by nickname

### Integration tests

Add or extend tests so they do **not** pre-create the package in the parent:

1. `parse-top-level-forms` succeeds when the file's package is defined in a
   separate package file and only local nicknames make `ad:foo` valid
2. `lisp-edit-form` succeeds on replacement content using local nicknames when
   the parent has not loaded the package beforehand
3. `lisp-read-file` collapsed mode succeeds on the same file
4. synthesized packages are deleted after the call
5. local nickname target package may be a stub and still allows repeated
   single-colon symbol access

### Regression tests

1. existing lenient unknown-package tests still pass
2. already-loaded real packages are reused, not deleted
3. package nickname conflict produces explicit error

## Rollout Plan

1. Introduce package-spec extraction and package discovery.
2. Add temporary package synthesis and cleanup.
3. Integrate synthesized package sets with lenient reader handling.
4. Wire `source-path` through `parse-top-level-forms`, `lisp-edit-form`, and
   `lisp-read-file`.
5. Add tests for same-file and separate-file package definitions.

## Open Questions

1. How much of `uiop:define-package` should be supported in v1 beyond
   `:nicknames`, `:use`, and `:local-nicknames`?
2. Should package discovery search only under `*project-root*`, or also loaded
   dependent systems visible to ASDF?
3. Do we want a small cache keyed by package name + source file timestamp, or
   is per-call discovery fast enough initially?

## Summary

The missing feature is not "preload stub packages in the parent". The missing
feature is a parent-side mechanism to reconstruct the package metadata that
activates local nicknames. This design adds a temporary package-context layer
that keeps the current worker isolation intact while making `lisp-edit-form`
and `lisp-read-file` robust for files that rely on package-local nicknames.
