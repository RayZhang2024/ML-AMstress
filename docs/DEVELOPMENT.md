# Development workflow

## Environments

The GUI requires a normal Python 3 environment with the packages in `requirements.txt` (PyQt5, pandas, NumPy, matplotlib, openpyxl, scikit-learn, Optuna, and joblib). Install them with `python -m pip install -r requirements.txt` and launch with `python AM_gui_v7.py`.

Abaqus automation is a separate runtime. Use the configured Abaqus/CAE 2021 command (for example `abq2021.bat`) for `cae noGUI` scripts and solver jobs. Do not install GUI dependencies into Abaqus's Python environment or assume that normal Python can import Abaqus modules.

## Issue, branch, and PR workflow

Start from an issue-specific branch, normally `codex/issue-<n>-<slug>`, and keep one coherent issue per PR. Inspect the current implementation before changing it. Run focused checks, review the full diff, and report behavior, files, risks, and limitations. Do not work directly on `main` or auto-merge.

## Checks and validation

Run normal Python syntax/import checks and any practical automated tests for GUI-side code. Static checks do not prove Abaqus compatibility. Report GUI smoke tests separately from Abaqus/CAE model-generation validation and from Abaqus solver validation. Claim the latter only when Abaqus was actually run with a usable license; otherwise state that validation remains manual.

## Paths and reproducibility

Do not add developer-specific absolute paths to source or default configuration. Use user-selected paths, repository-relative paths, or documented placeholders. The GUI may generate temporary absolute paths at runtime; those are artifacts, not defaults to commit.

## Scope discipline

Use the Necessity Gate before production changes: identify evidence, the required files, and the issue scope. Preserve the Python 3/Abaqus runtime boundary and the model contract documented in `ABAQUS_MODEL_CONTRACT.md`.
