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

## Paths and reproducibility

Do not add developer-specific absolute paths to source or default configuration. Use user-selected paths, repository-relative paths, or documented placeholders. The GUI may generate temporary absolute paths at runtime; those are artifacts, not defaults to commit.

## Scope discipline

Use the Necessity Gate before production changes: identify evidence, the required files, and the issue scope. Preserve the Python 3/Abaqus runtime boundary and the model contract documented in `ABAQUS_MODEL_CONTRACT.md`.
