# Development workflow

This line was added by the controlled GREEN worker integration exercise for Issue #30.

## Environments

The GUI requires a normal Python 3 environment with the packages in `requirements.txt` (PyQt5, pandas, NumPy, matplotlib, openpyxl, scikit-learn, Optuna, and joblib). Install them with `python -m pip install -r requirements.txt` and launch with `python AM_gui_v7.py`.

Abaqus automation is a separate runtime. Use the configured Abaqus/CAE 2021 command (for example `abq2021.bat`) for `cae noGUI` scripts and solver jobs. Do not install GUI dependencies into Abaqus's Python environment or assume that normal Python can import Abaqus modules.

## Issue, branch, and PR workflow

Start from an issue-specific branch, normally `codex/issue-<n>-<slug>`, and keep one coherent issue per PR. Inspect the current implementation before changing it. Run focused checks, review the full diff, and report behavior, files, risks, and limitations. Do not work directly on `main` or auto-merge.

## Checks and validation

Run normal Python syntax/import checks and any practical automated tests for GUI-side code. Static checks do not prove Abaqus compatibility. Report GUI smoke tests separately from Abaqus/CAE model-generation validation and from Abaqus solver validation. Claim the latter only when Abaqus was actually run with a usable license; otherwise state that validation remains manual.

### Pull-request CI

Pull requests targeting `main` run `.github/workflows/python-ci.yml`. The job
uses hosted Ubuntu with Python 3.11, sets `QT_QPA_PLATFORM=offscreen` and
`MPLBACKEND=Agg` for headless GUI-side imports/tests, and installs only the
current normal-Python test dependencies: PyQt5, NumPy, matplotlib, and joblib.
PyVista, PyVistaQt, VTK, and Abaqus are not installed or invoked by this CI.

The deterministic commands run from the repository root are:

```text
python -m py_compile AM_gui_v7.py data_extract.py
python -m unittest discover -s tests -p "test_*.py"
```

The test discovery includes the existing helper-path, helper-root, CPU/GPU,
heat-treatment, and governance regression modules. The compile check covers
the Python 3 GUI and dual-runtime data-extraction entry point; it is not an
Abaqus/CAE compatibility or solver check.

## GREEN Codex worker (Issue #28)

The first worker is intentionally narrow. It runs only from an `issues:
labeled` event whose label is `agent:codex`; it then re-fetches the issue and
fails closed unless the issue is open, has exactly `status:ready` and
`risk:green`, has a valid autonomous contract, has satisfied dependencies, and
has no competing branch or open PR. A deterministic `codex/issue-<number>-<slug>`
ref is created as the claim lock before the issue is marked
`status:in-progress`. The worker runs Codex on that branch, rejects any diff
outside GREEN paths (`docs/`, `scripts/`, `tests/`, and the small
governance-file allowlist). Control-plane paths are explicitly protected:
`.github/**`, `scripts/codex_issue_worker.py`, `AGENTS.md`,
`docs/AUTONOMOUS_DEVELOPMENT.md`, and `docs/AUTONOMOUS_ORCHESTRATION.md` are
always rejected, rather than being part of the autonomous edit surface. It runs the normal-Python checks, pushes once, opens
one PR against `main`, and changes the issue to `status:review` only after PR
creation. Failures preserve the branch and report `status:blocked`; no merge or
auto-merge operation is available.

The worker uses `GITHUB_TOKEN` only in the trusted orchestration step. The
workflow requests `contents: write` to create/push the single claim branch,
`issues: write` to record labels/comments, and `pull-requests: write` to open
the review PR. `actions/checkout` uses `persist-credentials: false`; the worker
removes this token (and `GH_TOKEN`) before starting Codex and injects a
one-command git extra-header only for the final push. No narrower GitHub
permission can perform those three API operations; the worker code has no merge
endpoint. The Codex subprocess uses Git's supported `GIT_CONFIG_GLOBAL` null
override together with `GIT_CONFIG_NOSYSTEM=1`, so it cannot read the runner
user's global credential helpers; trusted post-Codex push authentication is
unchanged. The worker does not use `OPENAI_API_KEY` or any other API-key billing
path.

## Self-hosted GREEN worker (Issue #31)

The GREEN worker runs on a dedicated repository self-hosted Windows runner
labeled `self-hosted`, `windows`, `x64`, and `ml-amstress-codex`. Hosted
normal-Python PR CI is unchanged. The worker uses a locally installed,
maintainer-approved pinned Codex CLI authenticated with the maintainer's
ChatGPT account; it does not use `OPENAI_API_KEY`, and there is no API-key or
API-billing fallback.

### Maintainer setup

1. In repository settings, add a repository-level Windows x64 self-hosted
   runner with the labels above. Keep it interactive under the same Windows
   account that will run Codex; do not initially run it as `LocalSystem` or
   another service identity.
2. Install Git and the approved pinned Codex CLI in that account's `PATH`.
   For an npm-managed installation, use the maintainer-approved version, for
   example `npm install --global @openai/codex@<approved-version>`; do not put
   this installation in the workflow. Complete `codex login` with the
   maintainer's ChatGPT account, then verify `codex login status` reports
   `Logged in using ChatGPT`. The status command must succeed without a prompt
   or credential output.
3. Configure repository variables (identifiers, not secrets):
   `CODEX_EXPECTED_VERSION` (the exact `codex --version` text or stable version
   token), `CODEX_EXPECTED_RUNNER_NAME` (the registered runner name), and
   `CODEX_EXPECTED_WINDOWS_USER` (the value returned by
   `python -c "import getpass; print(getpass.getuser())"` under the runner
   account). Do not configure an `OPENAI_API_KEY` secret for this worker.
4. Ensure the runner workspace has network access for GitHub API operations, a
   usable `git` executable, and permission for Actions checkout. The worker
   fails closed if the checkout is not clean or required policy files are
   missing.
5. Install a maintainer-approved Python 3.11 or newer on that same account's
   `PATH`. This Python is required for trusted-worker preflight and its
   authoritative final normal-Python validation. The dedicated runner's
   currently validated local interpreter is Python 3.13.14. The self-hosted
   workflow verifies the local executable and version before dependency
   installation; it does not use `actions/setup-python` or download, install,
   repair, or modify Python/Windows registry state.

The workflow performs preflight before any issue claim. It verifies Windows and
x64 runner identity, the expected runner/user context, Codex executable and
version, ChatGPT-only non-interactive authentication, Git availability, and a
clean Git workspace. Probe output is captured and never logged, so credentials
cannot be exposed. If ChatGPT authentication is unavailable to the runner user,
the run stops and reports the failure; it never silently switches to API-key
billing. Existing `GITHUB_TOKEN` isolation, checkout credential disabling,
trusted push/API operations, and protected control-plane paths remain intact.

### Preflight and controlled integration sequence

After the maintainer has configured the designated runner, pinned
ChatGPT-authenticated Codex CLI, expected runner/user/version variables, Git,
and trusted-worker Python, verify the basic runner preflight and login under
the runner account without exposing credentials. This confirms the runner
identity, Codex version and non-interactive ChatGPT login, Git availability,
and trusted-worker Python availability. It is a prerequisite check, not a
separate end-to-end worker dry run.

Once those prerequisites are established, Issue #30 is the controlled live
end-to-end GREEN worker integration test. It is intended to exercise the
harmless scoped implementation path, trusted validation, push, PR creation,
transition to `status:review`, and stop-without-merge behavior. Do not assume
that this full integration has succeeded until Issue #30 actually completes
those steps; prior preflight or launch observations establish only their
respective prerequisites.

Sandboxed Codex may run optional local checks when available, but its inability
to invoke Python or other optional tooling is not by itself a reason to decline
an otherwise clear GREEN edit. It must report that limitation truthfully. The
trusted worker, not sandboxed Codex, owns the required Python 3.11+ preflight
and authoritative final normal-Python validation.

## Paths and reproducibility

Do not add developer-specific absolute paths to source or default configuration. Use user-selected paths, repository-relative paths, or documented placeholders. The GUI may generate temporary absolute paths at runtime; those are artifacts, not defaults to commit.

## Scope discipline

Use the Necessity Gate before production changes: identify evidence, the required files, and the issue scope. Preserve the Python 3/Abaqus runtime boundary and the model contract documented in `ABAQUS_MODEL_CONTRACT.md`.
