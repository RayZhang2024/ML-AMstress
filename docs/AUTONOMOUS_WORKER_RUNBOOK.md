# GREEN/YELLOW Codex worker runbook

This is the authoritative operational guide for the current review-first
trusted router. The GREEN lane was proven in the controlled Issue #30
integration. The automated-YELLOW lane is separately pre-authorized and does
not itself authorize controlled runtime, repair, or merge.

Use it with [AUTONOMOUS_DEVELOPMENT.md](AUTONOMOUS_DEVELOPMENT.md) and [AUTONOMOUS_ORCHESTRATION.md](AUTONOMOUS_ORCHESTRATION.md). For recovery, see [AUTONOMOUS_TROUBLESHOOTING.md](AUTONOMOUS_TROUBLESHOOTING.md).
GitHub-native terminal observations are documented separately in
[A4_18_COMPLETION_OBSERVER.md](A4_18_COMPLETION_OBSERVER.md); they add an audit
record only and do not change the worker's review-first lifecycle.

## Known-good baseline

| Item | Proven baseline |
| --- | --- |
| Runner | Windows x64 self-hosted runner named `ml-amstress-runner`, with `ml-amstress-codex` (and standard self-hosted Windows/x64) labels |
| Runner identity | Interactive Windows user `zhang` |
| Codex | Maintainer-approved pinned `codex-cli 0.149.1`, resolved as `codex` on the runner `PATH` |
| Authentication | Non-interactive ChatGPT-authenticated Codex; `OPENAI_API_KEY` is explicitly absent |
| Trusted-worker Python | Python 3.11 or newer on `PATH` |
| Codex invocation | Prompt through stdin: `codex exec --sandbox workspace-write -c approval_policy="never" -` |
| Checkout and EOL | `actions/checkout` has `persist-credentials: false`; checkout/local Git use `core.autocrlf=false` and `core.eol=lf` |
| Trusted event writes | A repository-scoped, short-lived App token (`AUTOMATION_APP_TOKEN`) is minted from `AUTOMATION_APP_CLIENT_ID` and `AUTOMATION_APP_PRIVATE_KEY`; only trusted branch claim/push and PR create/update use it |
| Trusted state writes | The built-in `GITHUB_TOKEN` performs issue labels, status, and bounded audit state only |
| Repository setting | GitHub Actions is allowed to create pull requests |
| PR CI | App-authenticated branch pushes and PR writes emit normal PR events, so no manual Actions approval gate is required for worker PR CI |
| Merge policy | Merge and auto-merge remain disabled for A4; human review and human merge are required |

Never place runner registration tokens, GitHub tokens, ChatGPT session material, OAuth data, cookies, or raw authentication output in documentation, issues, PRs, or logs.

## Responsibilities and lifecycle

The trusted worker owns all GitHub/control-plane actions: eligibility and dependency checks, deterministic branch claim, labels/status, authoritative normal-Python validation, commit, trusted push, PR creation, and merge/no-merge policy. Sandboxed Codex treats its immutable claimed snapshot as authoritative and owns only the local Necessity Gate, scoped implementation, optional focused checks, and truthful reporting. It does not query GitHub or receive GitHub/API credentials.

```text
completed pre-start assessment + status:ready + fresh agent:codex label
  + risk:green, or risk:yellow plus canonical trusted #147 evidence
  -> trusted preflight, eligibility, deterministic claim -> status:in-progress
  -> sandboxed Codex implementation / optional focused checks
  -> trusted normal-Python validation, commit, push, PR -> status:review
  -> hosted PR CI on the App-authenticated PR event
  -> human review and human merge
```

Failure preserves the deterministic claim branch and blocks the work; do not launch a concurrent worker. Authorized recovery must deliberately delete or reset a failed claim before a new trigger can claim it.

The `agent:codex` event enters one trusted router. Before adding it,
the orchestration owner must assess expected paths, affected behavior/control-
plane surface, generated artifacts, and required evidence. Protected paths,
YELLOW behavior require canonical pre-start authorization; RED, scientific/
runtime behavior, and uncertain scope fail closed rather than being inferred
GREEN or YELLOW. The YELLOW lane uses `codex-yellow/issue-<N>-<slug>`, emits
canonical claim/A5 identity evidence, enforces the authorized changed-path
subset repeatedly, and cannot invoke automatic repair.
An existing label is not a retry mechanism. `status:review` records a
reviewable implementation, not issue completion: controlled runtime/scientific
acceptance and human merge remain separately authorized, and merge is bound to
the reviewed current head where the platform supports it.

The router accepts only a first-attempt (`GITHUB_RUN_ATTEMPT=1`) label event
whose repository and positive issue identity are exact. A workflow rerun is
not a fresh label event. Invalid event identity stops before issue mutation;
after a valid event resolves its exact issue, any routing/pre-start rejection
sets exactly one `status:blocked` label. Lane-specific failures remain owned by
the existing GREEN or YELLOW worker after delegation.

The automated-YELLOW pre-start transport is one exact maintainer-authored
`<!-- yellow-implementation-prestart:{canonical-#147-json} -->` issue comment.
The trusted worker binds it to the repository, issue, current base SHA, YELLOW
risk, scientific/runtime prohibition, and exact authorized paths before claim;
then it writes canonical `a5.yellow-prestart`, `a5.yellow-claim`, and run-bound
completion-observer identity markers. Those records authorize neither repair,
controlled runtime, nor merge.

## Rebuild and preflight checklist

- Register a dedicated Windows x64 self-hosted runner as `ml-amstress-runner` with `ml-amstress-codex`, interactively under `zhang` rather than an unrelated service identity.
- Install Git, Python 3.11+, and approved `codex-cli 0.149.1` so `git`, `python`, and `codex` resolve for that account.
- Complete the maintained ChatGPT login and verify it non-interactively without displaying credentials. Confirm no `OPENAI_API_KEY` is configured.
- Set only expected non-secret repository variables for runner name, Windows user, and Codex version. Keep tokens and registration material in approved secret/configuration locations.
- Verify a clean checkout with `core.autocrlf=false` and `core.eol=lf`; never commit, discard, or bulk-normalize unrelated files just to make it clean.
- Verify the existing workflow's `persist-credentials: false`, stdin prompt transport, `workspace-write`, and `approval_policy="never"`; these are verification points, not authorization to alter the workflow.
- Configure the repository variable `AUTOMATION_APP_CLIENT_ID` and secret `AUTOMATION_APP_PRIVATE_KEY` for the installed repository App; never print or retrieve their values. Verify that the App is scoped to this repository and can write contents and pull requests.
- Verify Actions can create PRs and that App-authenticated worker PR events start normal CI without manual approval.
- Verify the trusted worker owns deterministic claims and that merge/auto-merge remain disabled.

Issue #30 is the controlled integration evidence. PRs #62 and #63 are useful forensic references for stdin transport and the integration-era result, but do not replace this preflight.
