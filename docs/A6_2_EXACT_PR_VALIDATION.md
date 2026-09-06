# A6.2 bounded exact-PR Abaqus validation

A6.2 is a protected, manual-only controller for one explicitly selected,
same-repository PR head. It is not a PR workflow and does not execute target
workflow YAML, setup hooks, profile configuration, or production AM code.

## Dispatch and metadata gate

The trusted-main workflow accepts only four bounded values: PR number, issue
number, exact 40-character head SHA, and the fixed profile identifier
`inert-cae-runtime-probe`. It runs only for `RayZhang2024/ML-AMstress` on
`refs/heads/main`, with read-only `contents`, `pull-requests`, and `issues`
permissions and `persist-credentials: false`.

Before any self-hosted runner is scheduled, a GitHub-hosted trusted-main
metadata-gate job resolves and validates the complete live PR/issue/file set.
It emits only a bounded authorization record. The dependent self-hosted job
then re-resolves the same metadata immediately before it creates any target
workspace. Both gates require an open same-repository PR targeting `main`, the supplied exact head,
an open linked issue with exactly one valid status/risk label, `status:review`,
and non-RED risk. The deterministic linkage is exactly one standalone PR body
line: `Refs #<target issue number>`. Any fork, stale head, label ambiguity,
closed resource, linkage mismatch, or protected path fails closed.

The GitHub PR-file API is paginated at 100 entries. The controller fetches
every page up to a fixed 1,000-file bound and requires the unique fetched set
to equal GitHub's authoritative `changed_files` count; incomplete, duplicate,
or over-bound enumeration fails closed. Target changes cannot include `.github/**`, `scripts/a6_*`, or existing A4/A5
protected control-plane paths. This reuses the A4/A5 protection contract; both
ordinary GREEN implementation and A5 repair already reject the `scripts/a6_`
prefix.

## Workspace and profile boundary

The controller checkout is trusted `main`. A separate fresh temporary target
workspace fetches only the expected immutable SHA with Git credentials and
credential helpers disabled, verifies detached `HEAD`, and is removed after
the run. The target workspace is never used for workflow execution or setup.

The sole enabled profile, `inert-cae-runtime-probe`, calls controller-owned
A6.1 runtime probing and does not import or execute target code. Future
profiles marked as target-code-capable fail closed until a dedicated
Abaqus-validation Windows execution identity/runner with no Codex/OpenAI,
GitHub-write, personal SSH, or unrelated-user credential access is separately
established and validated.

Child environments remove token/key/secret/auth credential variables in the
GitHub, GH, Actions, OpenAI, Codex, automation, repository, and API families,
plus `SSH_AUTH_SOCK`, while preserving required Abaqus runtime/license context.
The profile is bounded by its controller-defined timeout. No profile command,
arguments, paths, executable content, or evidence configuration comes from a
PR, issue, label, artifact, commit, or workflow input.

## Evidence

Each execution emits exactly one bounded JSON line prefixed
`A6_PR_VALIDATION_EVIDENCE=`. Its schema includes controller/run identity,
target PR/issue/SHA, risk, profile, runner role/labels, approved non-secret
Abaqus command, release, outcome (`passed`, `failed`, or `unavailable`), and a
fixed failure category. It never includes raw Abaqus output, environment,
paths, usernames, license-server data, credentials, or target content.

A6.2 does not change model, solver, meshing, material, thermal, GUI, ML, A4,
A5, or merge behavior. Controlled live acceptance on a fresh harmless fixture
PR remains a separate post-review action; A7 is not enabled by this work.
