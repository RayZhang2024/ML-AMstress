# Additive Manufacturing Simulation GUI (AM_gui_v7)

This repository contains a Python-based GUI and supporting Abaqus automation scripts for **residual stress and distortion simulation in additive manufacturing (AM)**.  
It enables a semi-automated workflow to import CAD models, partition into layers, assign materials, apply meshing, boundary conditions, thermal loading (UTEMP), and run jobs — all inside **Abaqus/CAE** with minimal manual effort.

---

## 📂 Repository Structure
```
AM_simulation_GUI/
│
├── AM_gui_v7.py            # Main PyQt5 GUI (Python 3.x)
├── build_cae.py            # Build parametric CAE model (Abaqus kernel, Py2.7)
├── import_and_partition.py # Import CAD & partition layers
├── apply_materials.py      # Apply materials/sections from CSV
├── apply_meshing.py        # Layer-aware meshing with base/build seeds
├── apply_boundary.py       # Rigid-body motion constraints + temperatures
├── create_input.py         # Generate jobs & UTEMP subroutines
├── data_extract.py         # Extract stress/temperature from ODB
├── requirements.txt        # Python 3-side packages (GUI only)
└── README.md               # This documentation
```

---

## ⚙️ Requirements

### GUI side (Python 3.x, e.g. Anaconda/Miniconda):
- `PyQt5`
- `pandas`
- `openpyxl`
- `matplotlib`

Install with:
```bash
pip install -r requirements.txt
```

### Abaqus side (CAE kernel, Python 2.7):
- Abaqus/CAE 2021 (tested)
- Scripts run with:
  ```bash
  abaqus cae noGUI=script.py
  ```

---

## 🚀 Workflow Overview

1. **Launch GUI**
   ```bash
   python AM_gui_v7.py
   ```
   - Import CAD (`.stp`, `.igs`, `.sat`) or build a parametric shape
   - Define build direction, layer thickness, seeds, and material files

2. **Import & Partition**
   - Runs `import_and_partition.py`
   - Creates part sets: `BASE`, `BUILD_ALL`
   - Creates assembly sets: `set-0` (base), `set-1..N` (layers), `set-(N+1)` (whole build)

3. **Apply Materials**
   - Runs `apply_materials.py`
   - Reads CSV material property files
   - Assigns `base_sec` and `additive_sec`
   - Generates steps and model-change interactions

4. **Apply Meshing**
   - Runs `apply_meshing.py`
   - Global seeding = base size
   - Re-seeding = build region (y > Y_ZERO)

5. **Apply Boundary Conditions**
   - Runs `apply_boundary.py`
   - Anti-rigid-body constraints:
     - **BC-1**: U1
     - **BC-2**: U2
     - **BC-3**: U3
   - Adds predefined temperatures (Initial 25°C, Step-1 UTEMP)

6. **Create Inputs**
   - Runs `create_input.py`
   - Generates `.inp` files + UTEMP subroutine (`.for`)
   - Produces `submit.bat` for batch job submission

7. **Run Analysis**
   - Submit jobs via `abaqus job=... user=...`

8. **Extract Data**
   - Runs `data_extract.py`
   - Extracts stress/temperature fields from `.odb` along planes
   - Saves results into CSV (per-ODB and aggregated)

---

## 📊 Output
- `.cae` models after each step
- `.inp` job files
- `.for` UTEMP subroutines
- `.odb` simulation results (not stored in repo)
- `.csv` extracted data (stress/temperature)

---

## 📜 License
Choose a license that fits your needs (MIT recommended for open collaboration).

---

## 🙋 Acknowledgements
- Developed by **Ruiyao Zhang** (ISIS Neutron & Muon Source, STFC RAL)  
- Scripts tested with Abaqus/CAE 2021  
- Inspired by neutron diffraction residual stress studies in additive manufacturing

---
