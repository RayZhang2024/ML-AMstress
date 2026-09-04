# Development workflow

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

The workflow requires these names only (never their values):

- `OPENAI_API_KEY` — GitHub Actions secret used by the Codex CLI. It is scoped
  only to the final worker step (dependency setup/install steps do not receive
  it), and is never printed or placed in a prompt/comment/PR.
- `GITHUB_TOKEN` — GitHub's built-in token, supplied only to the trusted worker
  step. The workflow requests `contents: write` to create/push the single claim
  branch, `issues: write` to record labels/comments, and `pull-requests: write`
  to open the review PR. `actions/checkout` uses `persist-credentials: false`;
  the worker removes this token (and `GH_TOKEN`) before starting Codex and
  injects a one-command git extra-header only for the final push. No narrower
  GitHub permission can perform those three API operations; the worker code has
  no merge endpoint.
- `CODEX_CLI_PACKAGE` — repository variable naming a maintainer-approved,
  pinned `@openai/codex` package version for `npm install`; it is configuration,
  not a secret. The workflow fails before the worker if it is absent.

Before enabling the label trigger, a maintainer must set the pinned package
variable and `OPENAI_API_KEY`, verify organization secret policies, and run one
controlled test issue that changes only documentation/tests. Do not use a
personal broad-scope token or print credentials in Actions logs. The worker's
automated tests use fakes and do not invoke Codex; this controlled integration
test remains manual.

## Paths and reproducibility

Do not add developer-specific absolute paths to source or default configuration. Use user-selected paths, repository-relative paths, or documented placeholders. The GUI may generate temporary absolute paths at runtime; those are artifacts, not defaults to commit.

## Scope discipline

Use the Necessity Gate before production changes: identify evidence, the required files, and the issue scope. Preserve the Python 3/Abaqus runtime boundary and the model contract documented in `ABAQUS_MODEL_CONTRACT.md`.
