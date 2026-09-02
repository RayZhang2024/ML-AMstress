# AGENTS.md

## Project purpose

ML-AMstress is a Python GUI for machine-learning-assisted residual-stress simulation of additive manufacturing. It orchestrates CAD import and partitioning, material assignment, meshing, boundary conditions, thermal/UTEMP generation, input/job generation, ODB extraction, and ML calibration/prediction.

## Runtime boundary

There are two runtimes. The GUI (`AM_gui_v7.py`) is Python 3 and uses PyQt5, pandas, NumPy, matplotlib, scikit-learn, Optuna, and joblib. Abaqus-side scripts (`build_cae.py`, `import_and_partition.py`, `apply_materials.py`, `apply_meshing.py`, `apply_boundary.py`, `create_input.py`, and the Abaqus branch of `data_extract.py`) are executed by Abaqus/CAE; the supported baseline is Abaqus/CAE 2021 and its Python runtime. Do not introduce Python-3-only syntax into Abaqus-side scripts without explicitly changing and validating the supported Abaqus runtime.

## Engineering priorities

1. Simulation correctness
2. Prevention of invalid Abaqus models
3. Reproducibility
4. Backward compatibility
5. Clear diagnostics
6. Maintainability
7. Performance
8. UI convenience

## Development workflow

For a non-trivial change: inspect the implementation; identify the evidence and root cause; state the intended behavioral change; keep the work to one issue; implement the smallest coherent solution; add or update tests where practical; run relevant checks; review the complete diff; and report changed files, behavior, checks, limitations, and any Abaqus validation that still requires manual execution.

### Necessity Gate

Before modifying production code, identify the concrete problem or evidence, explain why the proposed files must change, and confirm that the change is within the issue scope. Documentation-only work should not become a pretext for runtime changes.

## Git rules

- Do not work directly on `main`.
- Use one branch per issue, such as `codex/issue-<n>-<slug>`.
- Prefer small, focused pull requests.
- Do not mix unrelated refactors with a feature or fix.
- Do not auto-merge unless explicitly instructed.

## Abaqus model safety rules

- `set-0` is the base; `set-1` through `set-N` are build layers; `set-(N+1)` is the whole build region.
- Layer sets must be contiguous. Never silently skip a missing required layer.
- Before input generation, every required layer set must exist and contain mesh elements, not only geometry cells.
- Do not create `ModelChange` interactions that reference an undefined or empty required element set.
- The active imported-CAD mesher uses `FREE + TET + C3D10` by default (C3D4 only when explicitly requested). Earlier C3D8R/mixed strategies in `apply_meshing.py` are commented-out legacy implementations, not active behavior. Do not silently reintroduce mixed C3D8R/C3D10 meshes.
- Critical model-generation failures must not be swallowed with `except: pass`.

## Architecture and configuration direction

New work should move toward separation between GUI, workflow orchestration, configuration, Abaqus execution, validation, visualization, and ML functionality. Avoid making `AM_gui_v7.py` substantially larger without clear justification.

The current GUI patches Abaqus scripts with regular expressions. Do not add more regex-based configuration mechanisms unless compatibility requires it. The target direction is an explicit structured configuration interface (for example JSON), preserving existing behavior until migration is tested.

The planned viewer should be independent of direct `.cae` reading where practical, using a GUI-side PyVista/PyVistaQt/VTK/meshio direction and an Abaqus-side export layer when necessary. Future views include geometry, mesh, element types, layer sets, materials, boundary conditions, and mesh-quality diagnostics.

## Validation language

Never claim Abaqus compatibility was verified unless Abaqus was actually run. Distinguish automated Python tests, static/code checks, Abaqus/CAE model-generation validation, and Abaqus solver validation.

## Definition of done

A task is complete only when requested behavior is implemented, relevant checks pass, unrelated behavior is unchanged, diagnostics/documentation are adequate, the diff is reviewed, and unverified Abaqus behavior is explicitly identified.
