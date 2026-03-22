---
description: Test a Common Lisp product from multiple perspectives using parallel agents with cl-mcp tools. Each agent adopts a different role (user, security, maintainability, etc.) to find issues that single-perspective testing misses. Use for pre-release validation or quality audits.
allowed-tools: Task, Read, Write, Edit, Glob, Grep, Bash, AskUserQuestion, mcp__cl-mcp__repl-eval, mcp__cl-mcp__load-system, mcp__cl-mcp__run-tests, mcp__cl-mcp__clgrep-search, mcp__cl-mcp__lisp-read-file, mcp__cl-mcp__code-find, mcp__cl-mcp__code-describe, mcp__cl-mcp__code-find-references, mcp__cl-mcp__clhs-lookup, mcp__cl-mcp__lisp-check-parens, mcp__cl-mcp__inspect-object, mcp__cl-mcp__fs-list-directory
---

# Comprehensive Test (Common Lisp / cl-mcp)

Test a Common Lisp product from multiple perspectives using parallel agents, then aggregate and evaluate findings.

## Arguments

`$ARGUMENTS` should contain:
- What to test (a project, feature, module, or API)
- Optionally, specific areas of concern

Examples:
- `/comprehensive-test evaluator`
- `/comprehensive-test sandbox safety`
- `/comprehensive-test full`

## Severity Criteria

All testers and the coordinator MUST use these definitions consistently:

| Severity | Definition | Examples |
|----------|------------|----------|
| **Critical** | Crashes, security vulnerabilities, data loss, spec violations that break core guarantees | Sandbox escape, infinite loop without halting, wrong result from arithmetic |
| **Major** | Incorrect behavior, missing validation, unhelpful error messages, undocumented deviation from spec | Type error not caught, error message missing context, edge case returning wrong value |
| **Minor** | Style issues, naming inconsistency, documentation gaps, non-idiomatic patterns | Inconsistent predicate naming, missing docstring, redundant code |

## Instructions

### 1. Analyze the Product

Before spawning testers, build a **context brief** to share with all testers:

1. **Set project root**: Use `fs-set-project-root` with the working directory to ensure cl-mcp file tools resolve paths correctly
2. **Discover the system**: Read the `.asd` file to find the main system name and test system definitions (look for `defsystem "*/tests"` or similar patterns)
3. **Load the system**: Use `load-system` to load the target system
4. **Read project structure**: Use `lisp-read-file` (collapsed mode) on key source files to get an overview of exported symbols and modules
5. **Run existing tests**: Use `run-tests` on each discovered test system to establish the baseline
6. **Identify key areas**: Use `clgrep-search` to find the main code areas (builtins, evaluator, reader, API, etc.)

**Full suite shortcut** (Bash fallback for clean process):
```bash
rove {system-name}.asd
```

**Compile the context brief** (pass this to every tester):
```
System name: {discovered system name}
Test baseline: {N passed, N failed, N pending per test system}
Source modules: {list of src/ files with brief purpose}
Public API: {key exported symbols}
Known gaps: {any failing tests or untested areas spotted}
```

### 2. Determine Perspectives

Based on the product type, select 3-5 testing perspectives. Not all perspectives apply to every product.

**Perspective pool:**

| Perspective | When to use | Focus area | Example probe |
|-------------|-------------|------------|---------------|
| **End User** | Always | Public API: does it work as documented? Are error messages helpful? | Call key public functions with typical inputs — does the happy path return expected values? |
| **Edge Case Explorer** | Always | Boundary values per function/form: empty inputs, nil, huge inputs, nested structures, type mismatches | Call each public function with nil, empty list, wrong type, zero, negative, deeply nested — one by one |
| **Security** | Network, user input, sandboxing | Injection, escape attempts, resource exhaustion, unsafe access | Try to reach host operations or bypass restrictions through the public API |
| **Maintainability** | Libraries, long-lived projects | API consistency, naming, modularity, condition hierarchy, separation of concerns | Are error types in a clear hierarchy? Are naming conventions consistent across the API? |
| **Performance** | Data processing, recursion heavy | Bottlenecks, unnecessary allocation, O(n^2) patterns, optimization effectiveness | Stress-test with large inputs — does performance scale as expected? |
| **Spec Compliance** | When spec/docs exist | Implementation vs specification gaps, missing features, undocumented behavior | Read spec/docs, compare each documented feature against actual behavior via `repl-eval` |
| **Error Handling** | All | What happens when things go wrong? Condition types, messages, resource cleanup on error | Pass invalid inputs to each function — are errors caught with clear types and messages? |

**Note on example probes:** The examples above are generic patterns. Design your actual probes using the public API and internal functions discovered in the context brief. Adapt to the project's domain — not every project is an evaluator or interpreter.

Select perspectives based on what matters most for this product. Briefly explain to the user which perspectives were chosen and why, then proceed.

### 3. Spawn Testers

For each selected perspective, spawn a tester agent in parallel:

```
subagent_type: "general-purpose"
```

**Tester prompt template:**

> You are a tester reviewing a Common Lisp product from the **{Perspective}** perspective.
>
> ## Context (from coordinator)
>
> - Product: {what's being tested}
> - Working directory: {project root}
> - System name: {from context brief}
> - Test baseline: {from context brief}
> - Source modules: {from context brief}
> - Public API: {from context brief}
>
> ## Your role
>
> {Role description from the perspective table above}
>
> **Focus area**: {focus area from table}
>
> ## How to work
>
> Use cl-mcp tools (NOT shell commands like grep/cat/sed). Key tools:
> - `fs-set-project-root` (call FIRST with working directory), `load-system`, `repl-eval`, `run-tests` for runtime testing
> - `clgrep-search`, `lisp-read-file` for code exploration
> - `code-find`, `code-describe`, `code-find-references` for symbol analysis (after load)
> - Shell only for: `rove` (test fallback), `mallet` (linting), `git`
>
> **Workflow**: fs-set-project-root → load-system → read relevant code → repl-eval to probe → report findings
>
> Design your probes using the Public API and internal functions from the context brief above. Don't assume any specific function names — explore what this project actually provides.
>
> ## Severity criteria
>
> - **Critical**: Crashes, security holes, data loss, spec violations breaking core guarantees
> - **Major**: Incorrect behavior, missing validation, unhelpful errors, undocumented spec deviation
> - **Minor**: Style, naming, docs gaps, non-idiomatic patterns
>
> ## Output format
>
> Report your **top 5 most important findings** (fewer is fine if thorough). Quality over quantity.
>
> ### {Perspective} Review
>
> #### Issues Found
>
> ##### 1. [Issue title]
> - **Severity**: Critical|Major|Minor
> - **Location**: `path/to/file.lisp:line`
> - **Description**: ...
> - **Reproduction**: `(repl-eval expression that demonstrates the issue)`
> - **Suggestion**: ...
>
> (repeat for each issue)
>
> #### Positive Observations
> - [Things that are well done from this perspective]
>
> #### Summary
> - Issues: {N Critical, N Major, N Minor}
> - Overall assessment from this perspective (1-2 sentences)
>
> #### Coverage Statement
> If you found zero issues, document: what you tested, how many expressions you evaluated, and why you're confident the code is sound from your perspective.
>
> #### cl-mcp Tool Feedback
>
> Report observations about the cl-mcp tools you used. Classify each as:
> - **Bug**: Wrong result, crash, unexpected behavior
> - **Friction**: Worked but awkward, required workarounds, confusing parameters
> - **Missing**: Needed a capability that no tool provided
> - **Praise**: Worked especially well, saved significant effort
>
> | # | Tool | Category | Observation |
> |---|------|----------|-------------|
> | 1 | tool-name | Category | What happened |
>
> Be honest — include the exact tool name, what you tried, and what happened.
>
> IMPORTANT:
> - Always reproduce issues with `repl-eval` before reporting them.
> - Distinguish between actual bugs and style preferences.
> - Focus on YOUR perspective. Don't try to cover everything.

Launch all tester agents in parallel (multiple Task tool calls in one message).

### 4. Aggregate Results

After all testers return, compile the results:

**Product issues:**

1. **Verify**: For any Critical/Major issues, reproduce them yourself using `repl-eval` to confirm they're real
2. **Deduplicate**: If multiple perspectives found the same issue, merge them and note which perspectives flagged it (issues caught by multiple perspectives are likely more important)
3. **Prioritize**: Sort by severity (Critical > Major > Minor), then by how many perspectives flagged it
4. **Categorize**: Group by area (e.g., "Reader", "Evaluator", "Builtins", "Safety", "API")

**cl-mcp feedback:**

5. **Collect**: Gather all `cl-mcp Feedback` sections from every tester
6. **Deduplicate**: Merge identical observations, noting how many testers reported the same thing
7. **Accumulate**: Append new feedback to the persistent feedback log (see Step 6)

### 5. Report

Present a consolidated report:

```
## Comprehensive Test Report: {Product}

### Test Configuration
- Perspectives used: {list}
- Date: {today}
- Test baseline: {pass/fail summary from run-tests}

### Summary
- Critical: {N}
- Major: {N}
- Minor: {N}
- Total issues: {N}

### Critical Issues
(deduplicated, with reproduction `repl-eval` expressions, all flagging perspectives noted)

### Major Issues
(deduplicated)

### Minor Issues
(deduplicated)

### Positive Findings
(consolidated from all perspectives)

### Recommended Actions
1. [Prioritized action item]
2. ...
```

### 6. Accumulate cl-mcp Feedback

After compiling the product report, update the persistent cl-mcp feedback log.

**Feedback file**: `~/.claude/memory/cl-mcp-feedback.md` (global, shared across all projects)

**If the file already exists**, read it first and append only NEW observations (don't duplicate existing entries). Use the `Edit` tool to append.

**If the file does not exist**, create it with the `Write` tool using this template:

```markdown
# cl-mcp Tool Feedback Log

Accumulated feedback from comprehensive-test runs. Each entry records
friction, bugs, missing capabilities, and praise observed while using
cl-mcp tools for real testing tasks.

---

## Run: {date} — {what was tested}

| # | Tool | Category | Testers | Observation |
|---|------|----------|---------|-------------|
| 1 | {tool} | Bug/Friction/Missing/Praise | {which perspectives reported it} | {description} |
```

**Rules:**
- Each run gets a dated section header
- The `Testers` column lists which perspectives reported the same observation
- Deduplicate within a run, but keep entries from different runs even if similar (frequency matters)
- If an old entry has been resolved (tool was updated), note `[RESOLVED {date}]` next to it

This log serves as input for cl-mcp development priorities. Patterns that appear across multiple runs are high-value improvement targets.

### 7. Next Steps

Ask the user:
- Fix critical issues now?
- Write new test cases for the gaps found?
- Save the report to `docs/`?
- Review cl-mcp feedback and file issues upstream?
