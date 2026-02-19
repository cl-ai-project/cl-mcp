---
description: Test a product from multiple perspectives using parallel agents. Each agent adopts a different role (user, security, maintainability, etc.) to find issues that single-perspective testing misses. Use for pre-release validation or quality audits.
allowed-tools: Task, Read, Glob, Grep, Bash, AskUserQuestion
---

# Comprehensive Test

Test a product from multiple perspectives using parallel agents, then aggregate and evaluate findings.

## Arguments

`$ARGUMENTS` should contain:
- What to test (a project, feature, module, or API)
- Optionally, specific areas of concern

Examples:
- `/comprehensive-test Mallet linter`
- `/comprehensive-test Ignis MCP server connection handling`
- `/comprehensive-test Qlot install command`

## Instructions

### 1. Analyze the Product

Before spawning testers, understand what's being tested:

- Read the project README, main source files, and test directory
- Identify the product type (CLI tool, library, API, web app, etc.)
- Identify the target users
- Note existing test coverage (what's already tested, what's not)

### 2. Determine Perspectives

Based on the product type, select 3-5 testing perspectives. Not all perspectives apply to every product.

**Perspective pool:**

| Perspective | When to use | Focus |
|-------------|-------------|-------|
| **End User** | Always | Does it work as documented? Are error messages helpful? Is the happy path smooth? |
| **Edge Case Explorer** | Always | Boundary values, empty inputs, huge inputs, Unicode, nil/null, concurrent access |
| **Security** | Network, file I/O, user input | Injection, path traversal, unsafe deserialization, information leakage |
| **Maintainability** | Libraries, long-lived projects | API consistency, naming, modularity, breaking change risk |
| **Performance** | Data processing, I/O heavy | Bottlenecks, unnecessary allocations, O(n^2) patterns, resource leaks |
| **Integration** | Multi-component systems | Interface contracts, version compatibility, error propagation across boundaries |
| **Error Handling** | All | What happens when things go wrong? Recovery, cleanup, partial failure states |
| **Documentation** | Libraries, public APIs | Do docs match behavior? Are examples runnable? Are edge cases documented? |

Select perspectives based on what matters most for this product. Briefly explain to the user which perspectives were chosen and why, then proceed.

### 3. Spawn Testers

For each selected perspective, spawn a tester agent in parallel:

```
subagent_type: "general-purpose"
name: "tester-{perspective}"
```

**Tester prompt template:**

> You are a tester reviewing a product from the **{Perspective}** perspective.
>
> Product: {what's being tested}
> Working directory: {project root}
>
> Your role: {role description from the table above}
>
> Instructions:
> 1. Read the relevant source code, tests, and documentation
> 2. Run existing tests if available to understand current state
> 3. Actively try to find problems from your perspective
> 4. For each issue found, provide:
>    - **Description**: What's the problem
>    - **Location**: File and line number (or "design-level" if architectural)
>    - **Reproduction**: How to trigger it (if applicable)
>    - **Severity**: Critical / Major / Minor
>    - **Suggestion**: How to fix it
>
> 5. Output format:
>
> ## {Perspective} Review
>
> ### Issues Found
>
> #### 1. [Issue title]
> - **Severity**: Critical|Major|Minor
> - **Location**: `path/to/file.ext:line`
> - **Description**: ...
> - **Reproduction**: ...
> - **Suggestion**: ...
>
> (repeat for each issue)
>
> ### Positive Observations
> - [Things that are well done from this perspective]
>
> ### Summary
> - Issues: {N Critical, N Major, N Minor}
> - Overall assessment from this perspective (1-2 sentences)
>
> IMPORTANT:
> - Be thorough. Read actual code, don't guess.
> - Finding zero issues means you haven't looked hard enough.
> - Distinguish between actual bugs and style preferences.
> - Focus on YOUR perspective. Don't try to cover everything.

Launch all tester agents in parallel (multiple Task tool calls in one message).

### 4. Aggregate Results

After all testers return, compile the results:

1. **Deduplicate**: If multiple perspectives found the same issue, merge them and note which perspectives flagged it (issues caught by multiple perspectives are likely more important)
2. **Prioritize**: Sort by severity (Critical > Major > Minor), then by how many perspectives flagged it
3. **Categorize**: Group by area (e.g., "Input Handling", "Error Recovery", "API Design")

### 5. Report

Present a consolidated report:

```
## Comprehensive Test Report: {Product}

### Test Configuration
- Perspectives used: {list}
- Date: {today}

### Summary
- Critical: {N}
- Major: {N}
- Minor: {N}
- Total issues: {N}

### Critical Issues
(deduplicated, with all flagging perspectives noted)

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

### 6. Next Steps

Ask the user:
- Create Todoist tasks for the issues?
- Fix critical issues now?
- Save the report to a file?
