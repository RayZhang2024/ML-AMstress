# Architecture

This document describes the repository as it exists today. The final section is direction, not an implemented rewrite.

## Current components and runtime split

`AM_gui_v7.py` is the Python 3/PyQt5 entry point. It owns the window and tabs, settings, process workers, temporary patched-script creation, file selection, plotting/alignment utilities, batch submission, and ML training/prediction orchestration. GUI-side data work uses pandas/NumPy/matplotlib and the ML tab uses scikit-learn, Optuna, and joblib.

The Abaqus-side scripts run in the Abaqus/CAE 2021 environment, not in the GUI's Python interpreter:

- `build_cae.py`: legacy parametric model creation, including geometry, sets, steps, interactions, mesh, and temperatures.
- `import_and_partition.py`: imports STEP/IGES/SAT geometry, partitions consecutive layer slabs, and creates the imported-model sets.
- `apply_materials.py`: reads CSV law tables, creates sections, infers layers, creates steps, and adds `ModelChange` interactions.
- `apply_meshing.py`: seeds and meshes an imported part with active `FREE + TET` controls and C3D10 by default (C3D4 only when explicitly requested). Earlier sweep/hex C3D8R and mixed strategies in the file are commented-out legacy code, not the active imported path.
- `apply_boundary.py`: adds the temperature fields and axis-aware anti-rigid-body constraints after a mesh exists.
- `create_input.py`: writes Abaqus input files and generated UTEMP Fortran plus `submit.bat`.
- `data_extract.py`: Abaqus ODB extraction script used by the Data Extract tab.

## Current Build Model pipeline

For parametric mode, the GUI regex-patches `build_cae.py` and launches `abaqus cae noGUI=...`.

For imported CAD, the GUI chains Abaqus jobs in this order: import/partition; optionally apply materials (when both spreadsheets are supplied); mesh; optionally apply boundary conditions. Each script opens/saves the same CAE path. Missing spreadsheets skip materials, and a missing mesh script is logged before the boundary attempt. The imported path uses ASCII-safe temporary CAD/script paths because the Abaqus Python 2-era importer is sensitive to Unicode paths.

The imported partitioner creates assembly `set-0`, consecutive per-layer sets, and the aggregate `set-(N+1)`, plus part-level `BASE` and `BUILD_ALL`. The exact set and step contract is in [ABAQUS_MODEL_CONTRACT.md](ABAQUS_MODEL_CONTRACT.md).

## Input/UTEMP and jobs

The Input & UTEMP tab prepends runtime constants such as the CAE path, coordinate index, axis zero, heat-treatment flags, and the visible CPU/GPU controls, rewrites `openMdb`, injects the temperature/gradient/layer grid parameters, and invokes `create_input(...)` under Abaqus. The selected CPU/GPU values are persisted in GUI settings and injected as `NUM_CPUS`/`NUM_GPUS`; `create_input.py` uses those same constants for both `mdb.Job(numCpus=..., numGPUs=...)` and every generated `submit.bat` command. `create_input.py` creates jobs and writes `.inp` files, one UTEMP `.for` file per temperature/gradient combination, and a batch file using `abq2021`, `cpus=`, `gpus=`, and `user=`.

The Submit Jobs tab runs a selected batch file. Intel Fortran/Visual Studio setup is external and is needed when Abaqus must compile a user subroutine.

## Data extraction and ML

The Data Extract tab injects ODB directory, output, plane/IDW, variable, step/frame, and volume-averaging parameters into `data_extract.py`, then runs it with Abaqus. It produces CSV outputs. Data Alignment is GUI-side point-cloud transformation/interpolation and plotting. The ML tab writes and runs GUI-side training or prediction scripts and persists model artifacts through joblib.

## Settings and helper resolution

The current `MainWindow` loads and saves `SCRIPT_DIR / "am_gui_settings.json"`. The six Abaqus helper scripts are application resources: normal execution resolves them from one runtime/resource directory associated with the running GUI. An advanced, explicit external-helper-directory override can resolve the complete fixed-name helper set from another directory; it is confirmed with Continue/Cancel before Abaqus starts. The GUI does not silently mix independently configured helper files. Existing six-path settings are archived and removed from active configuration: if their coherent root is the current runtime it collapses to normal application-relative mode; if it is another checkout, that root is retained only as an inactive recovery candidate. Legacy paths never enable external mode; only an explicit new-format `use_external_helper_root` setting may do so. Incomplete or mixed legacy values are inactive and reported, while unrelated settings are preserved. Missing required helper files fail before launch without falling back to another directory. Runtime values are still injected by line-oriented `re.sub`/`re.subn` replacements, with fallback parameter blocks and warnings when anchors are absent.

## Technical debt and target direction

The GUI is a large single module; workflow chaining, configuration injection, Abaqus execution, validation, visualization, and ML concerns are coupled. The intended direction is a structured configuration boundary, smaller orchestration modules, explicit validation before input generation, and a neutral Abaqus export consumed by a PyVista/PyVistaQt viewer. No such viewer or validation layer is implemented by Issue #1.
