# Architecture

This document describes the repository as it exists today. The final section is direction, not an implemented rewrite.

## Current components and runtime split

`AM_gui_v7.py` is the Python 3/PyQt5 entry point. It owns the window and tabs, settings, process workers, temporary patched-script creation, file selection, plotting/alignment utilities, batch submission, and ML training/prediction orchestration. GUI-side data work uses pandas/NumPy/matplotlib and the ML tab uses scikit-learn, Optuna, and joblib.

The Abaqus-side scripts run in the Abaqus/CAE 2021 environment, not in the GUI's Python interpreter:

- `build_cae.py`: legacy parametric model creation, including geometry, sets, steps, interactions, mesh, and temperatures.
- `import_and_partition.py`: imports STEP/IGES/SAT geometry, partitions consecutive layer slabs, and creates the imported-model sets.
- `apply_materials.py`: reads CSV law tables, creates sections, infers layers, creates steps, and adds `ModelChange` interactions.
- `apply_meshing.py`: seeds and meshes an imported part; the current implementation requests sweep/hex C3D8R by default and has tetrahedral fallback paths.
- `apply_boundary.py`: adds the temperature fields and axis-aware anti-rigid-body constraints after a mesh exists.
- `create_input.py`: writes Abaqus input files and generated UTEMP Fortran plus `submit.bat`.
- `data_extract.py`: Abaqus ODB extraction script used by the Data Extract tab.

## Current Build Model pipeline

For parametric mode, the GUI regex-patches `build_cae.py` and launches `abaqus cae noGUI=...`.

For imported CAD, the GUI chains Abaqus jobs in this order: import/partition; optionally apply materials (when both spreadsheets are supplied); mesh; optionally apply boundary conditions. Each script opens/saves the same CAE path. Missing spreadsheets skip materials, and a missing mesh script is logged before the boundary attempt. The imported path uses ASCII-safe temporary CAD/script paths because the Abaqus Python 2-era importer is sensitive to Unicode paths.

The imported partitioner creates assembly `set-0`, consecutive per-layer sets, and the aggregate `set-(N+1)`, plus part-level `BASE` and `BUILD_ALL`. The exact set and step contract is in [ABAQUS_MODEL_CONTRACT.md](ABAQUS_MODEL_CONTRACT.md).

## Input/UTEMP and jobs

The Input & UTEMP tab prepends runtime constants (CAE path, coordinate index, axis zero, heat-treatment flags, CPU/GPU counts), rewrites `openMdb`, injects grid parameters, and invokes `create_input(...)` under Abaqus. `create_input.py` creates jobs and writes `.inp` files, one UTEMP `.for` file per temperature/gradient combination, and a batch file using `abq2021`, `cpus=`, `gpus=`, and `user=`.

The Submit Jobs tab runs a selected batch file. Intel Fortran/Visual Studio setup is external and is needed when Abaqus must compile a user subroutine.

## Data extraction and ML

The Data Extract tab injects ODB directory, output, plane/IDW, variable, step/frame, and volume-averaging parameters into `data_extract.py`, then runs it with Abaqus. It produces CSV outputs. Data Alignment is GUI-side point-cloud transformation/interpolation and plotting. The ML tab writes and runs GUI-side training or prediction scripts and persists model artifacts through joblib.

## Settings and regex patching

Settings are loaded from the user configuration (with repository defaults in `am_gui_settings.json`) and include Abaqus command and helper-script paths. Runtime values are injected by line-oriented `re.sub`/`re.subn` replacements, with fallback parameter blocks and warnings when anchors are absent. This is current behavior and technical debt, not a new interface guarantee.

## Technical debt and target direction

The GUI is a large single module; workflow chaining, configuration injection, Abaqus execution, validation, visualization, and ML concerns are coupled. The intended direction is a structured configuration boundary, smaller orchestration modules, explicit validation before input generation, and a neutral Abaqus export consumed by a PyVista/PyVistaQt viewer. No such viewer or validation layer is implemented by Issue #1.
