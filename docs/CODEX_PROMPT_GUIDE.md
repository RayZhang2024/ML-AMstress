# Codex Prompt Guide

Core principle: **Issue = contract; Codex prompt = execution wrapper.**

Before drafting a Codex implementation prompt, GPT should read the live GitHub
issue and this guide from current trusted `main`. The live issue is the
authoritative source for scope, acceptance criteria, validation, risk,
dependencies, and stop conditions. The prompt should point Codex to that
contract and add only the execution context needed to complete the work safely.

## Length Defaults

Use these as practical defaults, not hard token limits:

- Ordinary GREEN work: approximately 300-600 words.
- Protected/YELLOW work: approximately 500-900 words.
- More than roughly 1,000 words should be exceptional and justified by a real
  cross-component safety boundary.

Prefer references such as "complete the full live Issue #N validation matrix"
instead of reproducing that matrix. Duplicate only a specific known-risk item
when it needs emphasis beyond the issue text.

## Preferred Structure

A Codex implementation prompt should usually cover:

1. Issue, repository, authorized branch, and trusted base SHA.
2. Authorization and dependency state, including relevant authorization comment
   IDs when applicable.
3. Instruction to read the live issue and this guide from current trusted
   `main`.
4. Core objective in one short paragraph.
5. Important architecture seam or ownership boundary.
6. Critical safety boundaries.
7. Allowed scope, prohibited scope, and mandatory stop conditions.
8. Validation expectations.
9. PR requirements, including open/unmerged review state.
10. Required completion report.

## Include When Applicable

Always include applicable facts that affect safe execution:

- Exact issue number.
- Exact authorized branch.
- Exact trusted base SHA.
- Relevant authorization comment IDs.
- Dependency state.
- Unusual architecture or security boundaries.
- Mandatory stop conditions.
- Controlled-runtime authorization state.
- Merge prohibition.

Protected/YELLOW prompts must clearly distinguish implementation authority,
standing merge authority, controlled A5/A6/A7/Abaqus/scientific execution
authority, and RED authorization. Authorization to implement does not imply
authorization to merge, run controlled scientific validation, or proceed into
RED scope.

## Omit By Default

Normally omit:

- Repetition of the full eight-section issue contract.
- Long historical narrative.
- Acceptance or test matrices already clear in the issue.
- Repeated negative instructions.
- Unnecessary intermediate progress-report requests.

Keep the prompt concise and practical. Use examples or templates only when they
materially clarify the standard.

## Execution Standard

Codex should continue to genuinely review-ready completion unless it encounters
a real blocker. If a blocker appears, Codex must stop and report it rather than
broadening scope, weakening safeguards, inventing authorization, or changing
unrelated files.

An inspection or progress checkpoint without a real blocker should continue on
the same authorized issue and branch. It should not create a new issue, branch,
claim, or authorization.

## Completion Report

The expected completion report should include:

- Exact final head SHA.
- Exact changed files.
- Important architecture, API, or schema changes.
- Focused and full tests run.
- Exact-head hosted CI state.
- Confirmation that prohibited scope was preserved.
- Controlled-runtime state.
- Confirmation that the PR remains open and unmerged.
