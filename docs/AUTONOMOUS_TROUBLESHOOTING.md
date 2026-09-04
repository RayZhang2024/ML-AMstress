# GREEN Codex worker troubleshooting

Preserve the failed state before recovery. The trusted worker owns control-plane recovery; sandboxed Codex does not perform GitHub eligibility, claims, status, push, PR, or merge actions. Never paste secrets, login material, or unsanitized output into a ticket or log. See [AUTONOMOUS_WORKER_RUNBOOK.md](AUTONOMOUS_WORKER_RUNBOOK.md) for the baseline.

## Workspace is dirty or has widespread line-ending changes

**Symptom:** Preflight rejects a dirty checkout, often with many unrelated files changed.

**Root cause:** A reused workspace or Git conversion does not match required LF state.

**Diagnosis/evidence:** Use the bounded isolated workspace-status diagnostic and inspect local Git EOL settings.

**Correct fix:** Recover/re-check out through the approved worker process with `core.autocrlf=false` and `core.eol=lf`, then verify clean status.

**Do not fix by:** Committing, discarding, or bulk-normalizing unrelated files.

**Prevention/regression protection:** Workflow preflight and repository-local Git settings; configuration can still drift.

**Evidence references:** Issue #30 integration; worker workspace diagnostic.

## Codex is not found on Windows

**Symptom:** Preflight says the configured Codex executable is unavailable on `PATH`.

**Root cause:** The runner account cannot resolve the CLI or the configured command is wrong.

**Diagnosis/evidence:** Run non-secret preflight as the actual runner user and check resolution/version.

**Correct fix:** Install or expose approved pinned Codex on that account's `PATH`.

**Do not fix by:** Switching user, embedding a developer-specific path, or installing an unpinned CLI in the workflow.

**Prevention/regression protection:** Worker preflight plus documented setup; no unit test alone guarantees PATH state.

**Evidence references:** `resolve_codex_executable`; Issue #30 setup.

## Codex rejects legacy `--full-auto`

**Symptom:** Codex exits with an unsupported-option error.

**Root cause:** Pinned CLI behavior no longer supports the old `--full-auto` invocation.

**Diagnosis/evidence:** Compare the worker command with the installed CLI's supported invocation.

**Correct fix:** Use established `codex exec` with `--sandbox workspace-write` and `approval_policy="never"`.

**Do not fix by:** Opportunistic downgrades or loosening sandbox/approval settings.

**Prevention/regression protection:** Documented baseline and worker command coverage; an intentional pin change can alter compatibility.

**Evidence references:** Issue #30 controlled integration.

## Codex succeeds but makes no changes

**Symptom:** Codex returns success but no paths changed and initial diagnostics are insufficient.

**Root cause:** The task may be satisfied/unclear, or Codex declined without a useful final response.

**Diagnosis/evidence:** Use the bounded redacted no-op diagnostic; do not equate zero exit status with implementation.

**Correct fix:** Review the Necessity Gate and safe diagnostic, then clarify or close an already-satisfied task through authorized process.

**Do not fix by:** Fabricating a change to satisfy the worker.

**Prevention/regression protection:** No-op detection and tests for bounded redacted diagnostics.

**Evidence references:** Issue #30 follow-up diagnostic work.

## No-op diagnostic misses the useful message

**Symptom:** A no-op lacks explanation although Codex emitted useful stream output.

**Root cause:** Useful data can be stderr while final response is normally stdout.

**Diagnosis/evidence:** Inspect captured streams only via worker redaction/selection logic.

**Correct fix:** Prefer usable redacted stdout final response; otherwise use redacted stderr fallback.

**Do not fix by:** Printing raw streams or the full issue contract.

**Prevention/regression protection:** Unit-tested stdout-first/stderr-fallback selection and redaction.

**Evidence references:** `format_codex_noop_diagnostic` tests.

## Sandboxed Codex tries to own GitHub eligibility checks

**Symptom:** Codex asks for credentials or attempts labels, PRs, dependencies, claims, or race checks.

**Root cause:** Control-plane duties were confused with local implementation.

**Diagnosis/evidence:** The immutable prompt and autonomy policy assign those actions to the trusted worker.

**Correct fix:** Keep Codex to local Necessity Gate/scoped edits and let the trusted worker handle control plane.

**Do not fix by:** Providing `GITHUB_TOKEN`, `GH_TOKEN`, or credentials to Codex.

**Prevention/regression protection:** Prompt contract, credential stripping, Git isolation, and governance docs.

**Evidence references:** `AUTONOMOUS_DEVELOPMENT.md` trusted-worker boundary.

## Development dry-run guidance is stale or circular

**Symptom:** A maintainer expects a separate dry run or treats Issue #30 as both prerequisite and proof.

**Root cause:** Earlier `docs/DEVELOPMENT.md` language mixed preflight and controlled integration.

**Diagnosis/evidence:** Separate independent runner preflight from the normal eligible-issue lifecycle in the runbook.

**Correct fix:** Verify preflight independently, then use a real eligible GREEN issue for the controlled path.

**Do not fix by:** Replaying Issue #30, bypassing eligibility, or redefining failure as dry run after the fact.

**Prevention/regression protection:** Documentation links/runbook only; this is not an automated guarantee.

**Evidence references:** Issue #30; `docs/DEVELOPMENT.md` history.

## Codex cannot receive the complete issue contract

**Symptom:** Codex acts on a truncated/malformed Windows-launched contract.

**Root cause:** Long Windows `.CMD` argv prompt transport was unreliable.

**Diagnosis/evidence:** The working command sends the prompt on stdin using final `-` input argument.

**Correct fix:** Preserve stdin prompt delivery in trusted worker.

**Do not fix by:** Reintroducing long `.CMD` quoting, lossy fragments, or logs.

**Prevention/regression protection:** Worker implementation/tests and documented invocation.

**Evidence references:** PR #62.

## Git push reports invalid credentials

**Symptom:** Trusted Git-over-HTTPS push fails despite a valid Actions token.

**Root cause:** Bearer authentication is unsuitable for this Git transport.

**Diagnosis/evidence:** Trusted push uses origin-scoped `http.https://github.com/.extraheader` Basic auth.

**Correct fix:** Base64-encode `x-access-token:<AUTOMATION_APP_TOKEN>` for Basic auth and inject it only for the trusted event-generating `git push origin <branch>`. The short-lived repository App token is minted by the workflow; the built-in `GITHUB_TOKEN` remains for labels, status, and audit state.

**Do not fix by:** Using Bearer, persisting checkout credentials, or exposing token to Codex.

**Prevention/regression protection:** Worker push implementation/tests, `persist-credentials: false`, and documentation.

**Evidence references:** Issue #30; `push_branch`.

## PR creation returns `POST /pulls: HTTP 403`

**Symptom:** Push succeeds but the worker cannot create its review PR.

**Root cause:** Repository Actions setting does not allow Actions to create pull requests.

**Diagnosis/evidence:** API returns 403 after successful trusted push; workflow permissions do not override repository setting.

**Correct fix:** Authorized maintainer enables repository setting allowing Actions to create PRs.

**Do not fix by:** Broadening tokens, manually hiding a workaround PR, or changing worker permissions without a separate issue.

**Prevention/regression protection:** Repository configuration plus rebuild checklist; code tests cannot guarantee setting remains enabled.

**Evidence references:** Issue #30 integration.

## Bot-created PR CI is `action_required`

**Symptom:** PR exists, but hosted CI has not run and GitHub shows `action_required`.

**Root cause:** The PR was created or updated with a credential that does not generate the required normal pull-request event under the repository's policy.

**Diagnosis/evidence:** Check workflow state; this is distinct from a test failure.

**Correct fix:** Verify the trusted workflow minted the repository-scoped App token and used it for the worker branch push and PR create/update. The App-authenticated event should start normal PR CI without a manual approval gate.

**Do not fix by:** Retrying worker with broader credentials, exposing token material, merging before CI, or changing permissions to force execution.

**Prevention/regression protection:** Repository App configuration, workflow token-scoping tests, and runbook checklist.

**Evidence references:** Issue #30 hosted PR CI.

## Retry is blocked by deterministic claim branch

**Symptom:** Fresh trigger cannot claim `codex/issue-<number>-<slug>` after earlier failure.

**Root cause:** Preserved deterministic branch is the claim lock.

**Diagnosis/evidence:** Worker reports existing claim/claim creation failure; recovery is through authorized control plane.

**Correct fix:** After preserving evidence and confirming prior attempt inactive, deliberately delete or reset failed claim, then issue fresh authorized trigger.

**Do not fix by:** Force-pushing claim, random replacement branch, or concurrent worker.

**Prevention/regression protection:** Deterministic-claim logic/tests and recovery documentation; manual recovery remains necessary.

**Evidence references:** Worker claim semantics; Issue #30 recovery lessons.
