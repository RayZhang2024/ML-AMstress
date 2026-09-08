# Terra Medium Canary
TERRA_MEDIUM_CANARY=passed

## A5.3 accepted blocker repair evidence

AC-1 worker run evidence for `d570290a99d31fab6a015d64a8e7ea35806a4413`:

- Immutable workflow-run payload from the local A5 review-loop `workflow_run`
  event records run `34244331051` with `status: completed`,
  `conclusion: success`, `run_attempt: 1`, and `previous_attempt_url: null`.
- The same payload records `head_sha:
  d570290a99d31fab6a015d64a8e7ea35806a4413`, `head_branch:
  codex/issue-171-terra-medium-live-canary-verify-post-migration-cod`, PR base
  `main` at `9e56f0b022ebb576fd26c97184e4ac7096563e4f`, PR head
  `d570290a99d31fab6a015d64a8e7ea35806a4413`, and display title
  `Issue #171: Terra medium live canary: verify post-migration Codex worker`.
- Local immutable Git evidence confirms
  `d570290a99d31fab6a015d64a8e7ea35806a4413^ =
  9e56f0b022ebb576fd26c97184e4ac7096563e4f`, so the successful first-attempt
  run used one deterministic GREEN branch from trusted `main`.

AC-4 worker execution profile evidence for run `34244331051`:

- Trusted-main worker source at
  `9e56f0b022ebb576fd26c97184e4ac7096563e4f:scripts/codex_issue_worker.py`
  defines `CODEX_WORKER_MODEL = "gpt-5.6-terra"` and
  `CODEX_WORKER_REASONING_EFFORT = "medium"`.
- The same worker constructs the implementation command as:
  `codex exec --model gpt-5.6-terra --sandbox workspace-write -c
  model_reasoning_effort="medium" -c approval_policy="never"`.
- The same trusted-main worker source rejects any non-first attempt before
  execution (`run_attempt != 1`) and hard-codes the model and reasoning effort
  instead of reading `CODEX_WORKER_MODEL`, `CODEX_WORKER_REASONING_EFFORT`, or
  `CODEX_MODEL` from the environment; local regression coverage asserts those
  environment values are ignored and that `gpt-5.5` and
  `model_reasoning_effort="high"` do not appear in the implementation command.
  This is the no-fallback evidence for the implementation profile used by the
  successful first-attempt run above.
