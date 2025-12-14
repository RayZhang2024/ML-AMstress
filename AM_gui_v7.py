# -*- coding: utf-8 -*-
"""
Additive Manufacturing Simulation GUI (v0.6.0)
==============================================
- Build Model:
  * Parametric primitives (existing flow), OR
  * Import CAD (STEP/IGES/SAT), slice along Y from Y=0, create sets:
      set-0  : base (Y <= 0)
      set-1..set-N : per-layer
      set-(N+1)    : whole build region
  * Optional: pick base/build material XLSX files; GUI converts to CSV and launches
    apply_materials_from_csv_gui.py to create materials, sections and assign.
  * Meshing with separate seeds for base/build.
  * (NEW) Apply anti-rigid-body BCs via apply_boundary.py after meshing.

- Input & UTEMP: unchanged core (executes your create_input script inside CAE).
- Submit Jobs: wrapper .bat + Stop kills entire process tree; run can be re-started immediately.

Notes:
- Requires pandas + openpyxl on the GUI side (Python 3) for XLSX→CSV conversion.
- Abaqus/CAE kernel runs Python 2.7: templates are ASCII-only with coding cookie.
"""

import os
import json
import re
import shutil
import subprocess
import sys
import tempfile
import platform
from pathlib import Path
import joblib
import numpy as np
from matplotlib import cm, colors
from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.backends.backend_qt5agg import NavigationToolbar2QT as NavigationToolbar
from matplotlib.figure import Figure

from PyQt5 import QtCore, QtGui, QtWidgets



__version__ = "0.6.0"
SCRIPT_DIR = Path(__file__).resolve().parent

DEFAULT_ABAQUS_CMD    = "C:/SIMULIA/Commands/abq2021.bat"
DEFAULT_BUILD_SCRIPT  = SCRIPT_DIR / "build_cae.py"
DEFAULT_INPUT_SCRIPT  = SCRIPT_DIR / "create_input.py"
DEFAULT_IMPORT_SCRIPT = SCRIPT_DIR / "import_and_partition.py"
DEFAULT_APPLY_MAT_SCRIPT = SCRIPT_DIR / "apply_materials.py"
DEFAULT_MESH_SCRIPT   = SCRIPT_DIR / "apply_meshing.py"
# NEW: boundary template
DEFAULT_APPLY_BC_SCRIPT = SCRIPT_DIR / "apply_boundary.py"


# ---------------------------- Worker (kill process tree) ----------------------------
class Worker(QtCore.QThread):
    output = QtCore.pyqtSignal(str)
    finished = QtCore.pyqtSignal(int)

    def __init__(self, cmd, cwd=None):
        super().__init__()
        self._cmd = cmd
        self._cwd = cwd
        self.proc = None

    def run(self):
        try:
            self.proc = subprocess.Popen(
                self._cmd,
                cwd=self._cwd,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                text=True,
                encoding='utf-8',
                errors='ignore',
                bufsize=1,
            )
        except Exception as e:
            self.output.emit(f"[Worker] 启动失败：{e}")
            self.finished.emit(-1)
            return

        for line in self.proc.stdout:
            self.output.emit(line.rstrip())

        self.proc.wait()
        self.finished.emit(self.proc.returncode)

    def stop(self, kill_tree: bool = False):
        if not self.proc or self.proc.poll() is not None:
            return
        try:
            if kill_tree and platform.system() == "Windows":
                self.output.emit(f"[Worker] taskkill /PID {self.proc.pid} /T /F")
                subprocess.run(
                    ["taskkill", "/PID", str(self.proc.pid), "/T", "/F"],
                    capture_output=True, text=True, timeout=10
                )
            else:
                self.output.emit("[Worker] 正在尝试 terminate() ...")
                self.proc.terminate()
                try:
                    self.proc.wait(timeout=5)
                except Exception:
                    self.output.emit("[Worker] 进程未退出，执行 kill() ...")
                    self.proc.kill()
        except Exception as e:
            self.output.emit(f"[Worker] 停止失败：{e}")

class LaunchMixin:
    def _launch(self, cmd, cwd, log, run_button, stop_button=None, on_finished_extra=None, clear_log=False):
        exe = cmd[0]
        if shutil.which(exe) is None:
            QtWidgets.QMessageBox.critical(self, "Executable not found",
                                            f"'{exe}' 不在 PATH。请在【设置】里配置正确的 Abaqus 命令。")
            return

        # ⬇️ do NOT wipe the log unless explicitly requested
        if clear_log:
            if hasattr(log, "clear"):
                log.clear()

        # add a visual separator so chained steps read nicely
        if hasattr(log, "appendPlainText"):
            log.appendPlainText("\n" + "="*80)
            log.appendPlainText("$ " + " ".join(map(str, cmd)))
        else:
            log.append("\n" + "="*80)
            log.append("$ " + " ".join(map(str, cmd)))

        run_button.setEnabled(False)
        if stop_button is not None:
            stop_button.setEnabled(True)

        self._worker = Worker(cmd, cwd)
        if hasattr(log, "appendPlainText"):
            self._worker.output.connect(log.appendPlainText)
        else:
            self._worker.output.connect(log.append)

        def _finish(code):
            run_button.setEnabled(True)
            if stop_button is not None:
                stop_button.setEnabled(False)
            if hasattr(log, "appendPlainText"):
                log.appendPlainText(f"\n=== finished (exit {code}) ===\n")
            else:
                log.append(f"\n=== finished (exit {code}) ===\n")
            if code == 0 and callable(on_finished_extra):
                on_finished_extra()

        self._worker.finished.connect(_finish)
        self._worker.start()


# --------------------------- Build Model Tab ---------------------------
class BuildModelTab(QtWidgets.QWidget, LaunchMixin):
    shapes = {"L shape": 1, "Square": 2, "Rectangle": 3, "Cylinder": 4, "Tube": 5}

    def __init__(self, settings):
        super().__init__()
        self.settings = settings
        self._build_tpl  = Path(self.settings.get("build_script", str(DEFAULT_BUILD_SCRIPT)))
        self._import_tpl = Path(self.settings.get("import_script", str(DEFAULT_IMPORT_SCRIPT)))
        self._apply_tpl  = Path(self.settings.get("apply_materials_script", str(DEFAULT_APPLY_MAT_SCRIPT)))
        self._mesh_tpl   = Path(self.settings.get("apply_meshing_script", str(DEFAULT_MESH_SCRIPT)))
        # NEW: boundary template path
        self._apply_bc_tpl = Path(self.settings.get("apply_boundary_script", str(DEFAULT_APPLY_BC_SCRIPT)))

        form = QtWidgets.QFormLayout()

        # Mode
        self.mode_cb = QtWidgets.QComboBox()
        self.mode_cb.addItems(["Create primitive (parametric)", "Import CAD (STEP/IGES/SAT)"])
        self.mode_cb.currentIndexChanged.connect(self._toggle_mode)
        form.addRow("Model source", self.mode_cb)

        # Parametric inputs
        self.shape_cb = QtWidgets.QComboBox(); self.shape_cb.addItems(self.shapes.keys())
        form.addRow("Shape", self.shape_cb)

        self.height_sp = QtWidgets.QDoubleSpinBox()
        self.height_sp.setRange(1.0, 3000.0); self.height_sp.setSuffix(" mm"); self.height_sp.setValue(12.0)
        form.addRow("Build height", self.height_sp)

        self.layer_sp = QtWidgets.QDoubleSpinBox()
        self.layer_sp.setDecimals(3); self.layer_sp.setRange(0.01, 5.0); self.layer_sp.setSuffix(" mm"); self.layer_sp.setValue(0.5)
        form.addRow("Layer thickness", self.layer_sp)

        self.bottom_remove_sp = QtWidgets.QSpinBox()
        self.bottom_remove_sp.setRange(0, 10)
        self.bottom_remove_sp.setValue(int(self.settings.get("bottom_layers_remove", 0)))
        form.addRow("Bottom layers to remove (0-10)", self.bottom_remove_sp)

        # Import inputs
        self.geom_le = QtWidgets.QLineEdit()
        self.geom_btn = QtWidgets.QPushButton("…"); self.geom_btn.clicked.connect(self._pick_geom)
        hlg = QtWidgets.QHBoxLayout(); hlg.addWidget(self.geom_le); hlg.addWidget(self.geom_btn)
        form.addRow("Geometry file", hlg)

        self.scale_sp = QtWidgets.QDoubleSpinBox()
        self.scale_sp.setDecimals(6); self.scale_sp.setRange(1e-6, 1e6); self.scale_sp.setValue(1.0)
        form.addRow("Import scale", self.scale_sp)

        # Material XLSX pickers (optional)
        self.base_xlsx_le = QtWidgets.QLineEdit(self.settings.get("base_xlsx", ""))
        btn_bx = QtWidgets.QPushButton("…"); btn_bx.clicked.connect(lambda: self._pick_xlsx(self.base_xlsx_le))
        hlbx = QtWidgets.QHBoxLayout(); hlbx.addWidget(self.base_xlsx_le); hlbx.addWidget(btn_bx)
        form.addRow("Base material .xlsx", hlbx)

        self.build_xlsx_le = QtWidgets.QLineEdit(self.settings.get("build_xlsx", ""))
        btn_bu = QtWidgets.QPushButton("…"); btn_bu.clicked.connect(lambda: self._pick_xlsx(self.build_xlsx_le))
        hlbu = QtWidgets.QHBoxLayout(); hlbu.addWidget(self.build_xlsx_le); hlbu.addWidget(btn_bu)
        form.addRow("Build material .xlsx", hlbu)
        
        # --- BuildModelTab.__init__ (add after the BC checkbox row) ---
        # Field output sampling interval for F-Output-1
        self.fout_dt = QtWidgets.QDoubleSpinBox()
        self.fout_dt.setDecimals(3)
        self.fout_dt.setRange(0.01, 4.0)   # ≤ 4.0
        self.fout_dt.setSingleStep(0.05)
        self.fout_dt.setSuffix(" (time units)")
        self.fout_dt.setValue(0.8)
        form.addRow("Field output interval (≤ 4.0):", self.fout_dt)


        # Meshing controls (Import mode)
        self.base_seed_sp = QtWidgets.QDoubleSpinBox()
        self.base_seed_sp.setDecimals(3); self.base_seed_sp.setRange(1e-3, 1e6); self.base_seed_sp.setValue(3.0)
        form.addRow("Base seed size", self.base_seed_sp)

        self.build_seed_sp = QtWidgets.QDoubleSpinBox()
        self.build_seed_sp.setDecimals(3); self.build_seed_sp.setRange(1e-3, 1e6); self.build_seed_sp.setValue(0.5)
        form.addRow("Build seed size", self.build_seed_sp)

        # Build direction (axis) and zero plane
        self.axis_cb = QtWidgets.QComboBox()
        self.axis_cb.addItems(["Y", "X", "Z"])  # default Y for back-compat
        self.axis_cb.setCurrentText(self.settings.get("build_axis", "Y"))
        form.addRow("Build axis", self.axis_cb)
        
        self.axis_zero_sp = QtWidgets.QDoubleSpinBox()
        self.axis_zero_sp.setRange(-1e9, 1e9)
        self.axis_zero_sp.setDecimals(6)
        self.axis_zero_sp.setSingleStep(0.1)
        self.axis_zero_sp.setValue(float(self.settings.get("axis_zero", 0.0)))
        form.addRow("Axis zero (plane)", self.axis_zero_sp)


        # NEW: Boundary condition toggle
        self.bc_chk = QtWidgets.QCheckBox("Apply anti-rigid-body BCs (U1/U2/U3)")
        self.bc_chk.setChecked(True)
        form.addRow("", self.bc_chk)

        # Post heat treatment (Build Model scope ONLY: creates extra step in apply_materials)
        self.ht_build_chk = QtWidgets.QCheckBox("Post heat treatment (adds final step in model)")
        self.ht_build_chk.setChecked(bool(self.settings.get("ht_build_enabled", False)))
        form.addRow("", self.ht_build_chk)


        # Output dir
        self.dir_le = QtWidgets.QLineEdit(self.settings.get("default_save_dir", str(SCRIPT_DIR)))
        btn = QtWidgets.QPushButton("…"); btn.clicked.connect(self._pick_dir)
        hl = QtWidgets.QHBoxLayout(); hl.addWidget(self.dir_le); hl.addWidget(btn)
        form.addRow("Save dir", hl)

        # Run / Stop / Log
        self.run_btn = QtWidgets.QPushButton("Generate CAE →"); self.run_btn.clicked.connect(self._run)
        self.stop_btn = QtWidgets.QPushButton("Stop"); self.stop_btn.setEnabled(False); self.stop_btn.clicked.connect(self._stop_running)
        hb = QtWidgets.QHBoxLayout(); hb.addWidget(self.run_btn); hb.addWidget(self.stop_btn)

        self.log = QtWidgets.QPlainTextEdit(); self.log.setReadOnly(True)

        vbox = QtWidgets.QVBoxLayout(self)
        vbox.addLayout(form)
        vbox.addLayout(hb)
        vbox.addWidget(self.log, 1)

        self._tmpdir = None
        self._toggle_mode()

    # ---- helpers ----
    def _toggle_mode(self):
        import_mode = (self.mode_cb.currentIndex() == 1)
        self.shape_cb.setEnabled(not import_mode)
        self.geom_le.setEnabled(import_mode); self.geom_btn.setEnabled(import_mode)
        self.scale_sp.setEnabled(import_mode)
        self.base_xlsx_le.setEnabled(import_mode); self.build_xlsx_le.setEnabled(import_mode)
        self.base_seed_sp.setEnabled(import_mode)
        self.build_seed_sp.setEnabled(import_mode)
        self.bc_chk.setEnabled(import_mode)
        self.axis_cb.setEnabled(import_mode)
        self.axis_zero_sp.setEnabled(import_mode)

    def _fout_mode_and_dt(self):
        if self.rb_fout_endstep.isChecked():
            return "END_STEP", 4.0  # dt ignored in this mode
        # interval mode
        val = float(self.fout_dt_spin.value())
        if val < 0.0: val = 0.0
        if val > 4.0: val = 4.0
        return "INTERVAL", val
  

    def _pick_dir(self):
        d = QtWidgets.QFileDialog.getExistingDirectory(self, "Select directory")
        if d:
            self.dir_le.setText(d); self.settings["default_save_dir"] = d

    def _pick_geom(self):
        f, _ = QtWidgets.QFileDialog.getOpenFileName(self, "Select CAD file", "",
                                                      "CAD files (*.stp *.step *.igs *.iges *.sat);;All files (*)")
        if f:
            self.geom_le.setText(f)

    def _pick_xlsx(self, line):
        f, _ = QtWidgets.QFileDialog.getOpenFileName(self, "Select Excel file", "",
                                                      "Excel files (*.xlsx);;All files (*)")
        if f:
            line.setText(f)
            if line is self.base_xlsx_le:
                self.settings["base_xlsx"] = f
            else:
                self.settings["build_xlsx"] = f

    def _stop_running(self):
        if hasattr(self, "_worker") and self._worker.isRunning():
            self.log.appendPlainText("[GUI] 收到停止请求，正在终止整棵进程树 ...")
            self.run_btn.setEnabled(True); self.stop_btn.setEnabled(False)
            self._worker.stop(kill_tree=True)
        else:
            self.log.appendPlainText("[GUI] 当前无运行中的任务。")

    # ---- XLSX → CSV (GUI side, Py3) ----
    def _xlsx_to_csv_lawformat(self, xlsx_path: str, out_csv: Path) -> Path:
        """
        Convert an Excel (xlsx) to a CSV with canonical columns:
          law,E,nu,alpha,sigma,epsp,T,Tanneal
        Either:
          - sheets named ELASTIC/EXPANSION/PLASTIC/ANNEAL, or
          - a single sheet containing a 'law' column.
        """
        import pandas as pd

        def _clean(df):
            df = df.copy()
            df.columns = [str(c).strip().lower() for c in df.columns]
            for c in df.columns:
                if df[c].dtype == object:
                    df[c] = df[c].astype(str).str.strip()
            return df

        xl = pd.ExcelFile(xlsx_path)
        rows = []

        has_named = set([s.upper() for s in xl.sheet_names]) & set(['ELASTIC', 'EXPANSION', 'PLASTIC', 'ANNEAL'])
        if has_named:
            if 'ELASTIC' in xl.sheet_names:
                df = _clean(xl.parse('ELASTIC'))
                for _, r in df.iterrows():
                    E, nu, T = r.get('e'), r.get('nu'), r.get('t')
                    if pd.notnull(E) and pd.notnull(nu) and pd.notnull(T):
                        rows.append(['ELASTIC', E, nu, '', '', '', T, ''])
            if 'EXPANSION' in xl.sheet_names:
                df = _clean(xl.parse('EXPANSION'))
                for _, r in df.iterrows():
                    a, T = r.get('alpha'), r.get('t')
                    if pd.notnull(a) and pd.notnull(T):
                        rows.append(['EXPANSION', '', '', a, '', '', T, ''])
            if 'PLASTIC' in xl.sheet_names:
                df = _clean(xl.parse('PLASTIC'))
                for _, r in df.iterrows():
                    s, e, T = r.get('sigma'), r.get('epsp'), r.get('t')
                    if pd.notnull(s) and pd.notnull(e) and pd.notnull(T):
                        rows.append(['PLASTIC', '', '', '', s, e, T, ''])
            if 'ANNEAL' in xl.sheet_names:
                df = _clean(xl.parse('ANNEAL'))
                for _, r in df.iterrows():
                    Ta = r.get('tanneal')
                    if pd.notnull(Ta):
                        rows.append(['ANNEAL', '', '', '', '', '', '', Ta])
        else:
            df = _clean(xl.parse(xl.sheet_names[0]))
            if 'law' not in df.columns:
                raise ValueError("Excel需包含 sheets: ELASTIC/EXPANSION/PLASTIC/ANNEAL 或一个含 'law' 列的sheet。")
            for _, r in df.iterrows():
                law = str(r.get('law', '')).upper()
                if law == 'ELASTIC':
                    E, nu, T = r.get('e'), r.get('nu'), r.get('t')
                    if pd.notnull(E) and pd.notnull(nu) and pd.notnull(T):
                        rows.append(['ELASTIC', E, nu, '', '', '', T, ''])
                elif law == 'EXPANSION':
                    a, T = r.get('alpha'), r.get('t')
                    if pd.notnull(a) and pd.notnull(T):
                        rows.append(['EXPANSION', '', '', a, '', '', T, ''])
                elif law == 'PLASTIC':
                    s, e, T = r.get('sigma'), r.get('epsp'), r.get('t')
                    if pd.notnull(s) and pd.notnull(e) and pd.notnull(T):
                        rows.append(['PLASTIC', '', '', '', s, e, T, ''])
                elif law == 'ANNEAL':
                    Ta = r.get('tanneal')
                    if pd.notnull(Ta):
                        rows.append(['ANNEAL', '', '', '', '', '', '', Ta])

        # sort ELASTIC/EXPANSION/PLASTIC rows by T; keep ANNEAL at end
        def _key(r):  # r = [law, E, nu, alpha, sigma, epsp, T, Tanneal]
            try:
                return float(r[6]) if r[0] in ('ELASTIC', 'EXPANSION', 'PLASTIC') else 1e99
            except:
                return 1e99
        rows_sorted = sorted([r for r in rows if r[0] != 'ANNEAL'], key=_key) + [r for r in rows if r[0] == 'ANNEAL']

        out_csv.write_text(
            "law,E,nu,alpha,sigma,epsp,T,Tanneal\n" +
            "\n".join(",".join("" if v is None else str(v) for v in r) for r in rows_sorted),
            encoding="utf-8"
        )
        return out_csv

    # ---- main run ----
    def _run(self):
        save_dir = Path(self.dir_le.text()).expanduser().resolve()
        save_dir.mkdir(parents=True, exist_ok=True)
        self.settings["ht_build_enabled"] = bool(self.ht_build_chk.isChecked())
        self.settings["build_axis"] = self.axis_cb.currentText().upper()
        self.settings["axis_zero"]  = float(self.axis_zero_sp.value())
        self.settings["bottom_layers_remove"] = int(self.bottom_remove_sp.value())

        import_mode = (self.mode_cb.currentIndex() == 1)
        self._tmpdir = tempfile.TemporaryDirectory()

        if not import_mode:
            # ---- parametric path (patch your template) ----
            tpl = self._build_tpl.read_text("utf-8")
            warnings = []

            txt, n1 = re.subn(r"shape_index\s*=.*",
                              f"shape_index = {self.shapes[self.shape_cb.currentText()]}",
                              tpl, count=1)
            if n1 == 0: warnings.append("未找到 'shape_index ='，将采用末尾兜底块。")

            txt2, n2 = re.subn(r"build_height\s*=.*", f"build_height = {self.height_sp.value()}", txt, count=1)
            if n2 == 0: warnings.append("未找到 'build_height ='，将采用末尾兜底块。")

            txt3, n3 = re.subn(r"layer_thickness\s*=.*", f"layer_thickness = {self.layer_sp.value()}", txt2, count=1)
            if n3 == 0: warnings.append("未找到 'layer_thickness ='，将采用末尾兜底块。")

            txt4, n4 = re.subn(r"savepathName\s*=.*",
                                f"savepathName = r'{save_dir.as_posix()}/'",
                                txt3, count=1)
            if n4 == 0: warnings.append("未找到 'savepathName ='，将采用末尾兜底块。")

            txt5, n5 = re.subn(r"^BOTTOM_LAYER_REMOVAL\s*=.*",
                               f"BOTTOM_LAYER_REMOVAL = {int(self.bottom_remove_sp.value())}",
                               txt4, count=1, flags=re.M)
            if n5 == 0: warnings.append("未找到 'BOTTOM_LAYER_REMOVAL ='，将采用末尾兜底块。")

            if any(x.startswith("未找到") for x in warnings):
                txt5 += f"""

# ===== GUI injected parameters (fallback) =====
shape_index     = {self.shapes[self.shape_cb.currentText()]}
build_height    = {self.height_sp.value()}
layer_thickness = {self.layer_sp.value()}
savepathName    = r'{save_dir.as_posix()}/'
BOTTOM_LAYER_REMOVAL = {int(self.bottom_remove_sp.value())}
# =============================================
"""

            patched = Path(self._tmpdir.name) / "build_cae_patched.py"
            patched.write_text(txt5, "utf-8")
            for w in warnings: self.log.appendPlainText("[警告] " + w)

            cmd = [self.settings.get("abaqus_cmd", DEFAULT_ABAQUS_CMD), "cae", f"noGUI={patched}"]
            self._launch(cmd, save_dir, self.log, self.run_btn, stop_button=self.stop_btn)
            return

        # ---- import path: patch external template, run, then materials -> mesh -> boundary ----
        cad_path = self.geom_le.text().strip()
        if not cad_path:
            QtWidgets.QMessageBox.critical(self, "No file", "请先选择 CAD 文件（STEP/IGES/SAT）。")
            return

        src = Path(cad_path)
        ext = src.suffix.lower()
        if ext not in [".stp", ".step", ".igs", ".iges", ".sat"]:
            QtWidgets.QMessageBox.critical(self, "Unsupported", f"不支持的扩展名: {ext}")
            return

        # Copy CAD to ASCII-only temp path (Py2.7 importer dislikes unicode)
        ascii_cad = Path(self._tmpdir.name) / ("import_model" + ext)
        try:
            shutil.copy2(str(src), str(ascii_cad))
        except Exception as e:
            QtWidgets.QMessageBox.critical(self, "Copy failed", f"无法复制 CAD 文件到临时目录：\n{e}")
            return

        # Compute CAE output path (ASCII-safe)
        def _ascii_path(p: Path) -> Path:
            try:
                p.as_posix().encode("ascii")
                return p
            except UnicodeEncodeError:
                return p.parent / "imported.cae"

        proposed = save_dir / (src.stem + "_imported.cae")
        cae_out = _ascii_path(proposed)
        self._last_cae = cae_out  # keep for follow-up steps

        axis = self.axis_cb.currentText().upper()
        axis_zero = float(self.axis_zero_sp.value())

        # Patch import template constants
        tpl = self._import_tpl.read_text("utf-8")
        tpl = re.sub(r'^CAD_FILE\s*=.*',  'CAD_FILE = r"%s"' % ascii_cad.as_posix(), tpl, flags=re.M)
        tpl = re.sub(r'^SCALE\s*=.*',     'SCALE = %s' % float(self.scale_sp.value()), tpl, flags=re.M)
        tpl = re.sub(r'^LAYER_THK\s*=.*', 'LAYER_THK = %s' % float(self.layer_sp.value()), tpl, flags=re.M)
        tpl = re.sub(r'^BUILD_H\s*=.*',   'BUILD_H = %s' % float(self.height_sp.value()), tpl, flags=re.M)
        tpl = re.sub(r'^BUILD_AXIS\s*=.*', 'BUILD_AXIS = "%s"' % axis, tpl, flags=re.M)
        tpl = re.sub(r'^AXIS_ZERO\s*=.*',  'AXIS_ZERO  = %s' % axis_zero, tpl, flags=re.M)
        tpl = re.sub(r'^SAVE_AS\s*=.*',   'SAVE_AS = r"%s"' % cae_out.as_posix(), tpl, flags=re.M)
        tpl = re.sub(r'^TOL\s*=.*',       'TOL = %s' % 1.0e-9, tpl, flags=re.M)

        import_patched = Path(self._tmpdir.name) / "import_and_partition_patched.py"
        import_patched.write_text(tpl, "utf-8")

        cmd_import = [self.settings.get("abaqus_cmd", DEFAULT_ABAQUS_CMD), "cae", f"noGUI={import_patched}"]

        def _after_import():
            """Run materials (if XLSX provided), then ALWAYS run meshing, then (optionally) boundary BCs."""
            # --- inner: run BC after mesh ---
            def _run_bc():
                if not self.bc_chk.isChecked():
                    return
                try:
                    tpl3 = self._apply_bc_tpl.read_text("utf-8")
                except Exception as e:
                    self.log.appendPlainText("[边界条件] 无法读取 apply_boundary 脚本：" + str(e))
                    return

                # Patch constants for boundary script
                tpl3 = re.sub(r'^CAE_FILE\s*=.*',      'CAE_FILE = r"%s"' % self._last_cae.as_posix(), tpl3, flags=re.M)
                tpl3 = re.sub(r'^MODEL_NAME\s*=.*',    'MODEL_NAME = "Model-1"', tpl3, flags=re.M)
                tpl3 = re.sub(r'^INSTANCE_NAME\s*=.*', 'INSTANCE_NAME = "ImportedPart-1"', tpl3, flags=re.M)
                tpl3 = re.sub(r'^BASE_SEED\s*=.*',     'BASE_SEED = %s' % float(self.base_seed_sp.value()), tpl3, flags=re.M)
                tpl3 = re.sub(r'^BUILD_SEED\s*=.*',    'BUILD_SEED = %s' % float(self.build_seed_sp.value()), tpl3, flags=re.M)
                tpl3 = re.sub(r'^LAYER_THK\s*=.*',     'LAYER_THK = %s' % float(self.layer_sp.value()), tpl3, flags=re.M)
                # NEW: provide BUILD_AXIS / AXIS_ZERO to boundary script
                tpl3 = re.sub(r'^BUILD_AXIS\s*=.*', 'BUILD_AXIS = "%s"' % axis, tpl3, flags=re.M)
                tpl3 = re.sub(r'^AXIS_ZERO\s*=.*',  'AXIS_ZERO  = %s' % axis_zero, tpl3, flags=re.M)

                bc_patched = Path(self._tmpdir.name) / "apply_boundary_patched.py"
                bc_patched.write_text(tpl3, "utf-8")

                cmd3 = [self.settings.get("abaqus_cmd", DEFAULT_ABAQUS_CMD), "cae", f"noGUI={bc_patched}"]
                self._launch(cmd3, self._last_cae.parent, self.log, self.run_btn, stop_button=self.stop_btn)

            # --- inner: run meshing (chains to BC) ---
            def _run_mesh():
                try:
                    tplm = self._mesh_tpl.read_text("utf-8")
                except Exception as e:
                    self.log.appendPlainText("[网格] 无法读取 apply_meshing 脚本：" + str(e))
                    # 即便网格脚本不存在，也尝试直接跑边界，但边界脚本需要网格节点，通常会失败
                    _run_bc()
                    return
                # mode, dt = self._fout_mode_and_dt()

                # tplm = re.sub(r'^FOUT_MODE\s*=.*', 'FOUT_MODE = "%s"' % mode, tplm, flags=re.M)
                # tplm = re.sub(r'^FOUT_DT\s*=.*',   'FOUT_DT = %s' % float(dt), tplm, flags=re.M)

                tplm = re.sub(r'^CAE_FILE\s*=.*',   'CAE_FILE = r"%s"' % self._last_cae.as_posix(), tplm, flags=re.M)
                tplm = re.sub(r'^MODEL_NAME\s*=.*', 'MODEL_NAME = "Model-1"', tplm, flags=re.M)
                tplm = re.sub(r'^PART_NAME\s*=.*',  'PART_NAME = "ImportedPart"', tplm, flags=re.M)
                tplm = re.sub(r'^BASE_SEED\s*=.*',  'BASE_SEED = %s' % float(self.base_seed_sp.value()), tplm, flags=re.M)
                tplm = re.sub(r'^BUILD_SEED\s*=.*', 'BUILD_SEED = %s' % float(self.build_seed_sp.value()), tplm, flags=re.M)
                tplm = re.sub(r'^BUILD_AXIS\s*=.*', 'BUILD_AXIS = "%s"' % axis, tplm, flags=re.M)
                tplm = re.sub(r'^AXIS_ZERO\s*=.*',  'AXIS_ZERO  = %s' % axis_zero, tplm, flags=re.M)
                tplm = re.sub(r'^TOL\s*=.*',        'TOL = %s' % 1.0e-6, tplm, flags=re.M)

                mesh_patched = Path(self._tmpdir.name) / "apply_meshing_patched.py"
                mesh_patched.write_text(tplm, "utf-8")

                cmdm = [self.settings.get("abaqus_cmd", DEFAULT_ABAQUS_CMD), "cae", f"noGUI={mesh_patched}"]
                # 关键：网格完成后再跑边界
                self._launch(cmdm, self._last_cae.parent, self.log, self.run_btn,
                              stop_button=self.stop_btn, on_finished_extra=_run_bc)

            # ---- materials provided? then run them first, else mesh now ----
            base_xlsx = self.base_xlsx_le.text().strip()
            build_xlsx = self.build_xlsx_le.text().strip()
            if not base_xlsx or not build_xlsx:
                _run_mesh()
                return

            # Excel → CSV
            base_csv = Path(self._tmpdir.name) / "base_props.csv"
            build_csv = Path(self._tmpdir.name) / "build_props.csv"
            try:
                self._xlsx_to_csv_lawformat(base_xlsx, base_csv)
                self._xlsx_to_csv_lawformat(build_xlsx, build_csv)
            except Exception as e:
                self.log.appendPlainText("[材料] 读取/转换 XLSX 失败：" + str(e))
                _run_mesh()
                return

            # Patch apply-materials template
            tpl2 = self._apply_tpl.read_text("utf-8")
            tpl2 = re.sub(r'^CAE_FILE\s*=.*',      'CAE_FILE = r"%s"' % self._last_cae.as_posix(), tpl2, flags=re.M)
            tpl2 = re.sub(r'^BASE_CSV\s*=.*',      'BASE_CSV = r"%s"' % base_csv.as_posix(), tpl2, flags=re.M)
            tpl2 = re.sub(r'^BUILD_CSV\s*=.*',     'BUILD_CSV = r"%s"' % build_csv.as_posix(), tpl2, flags=re.M)
            tpl2 = re.sub(r'^BASE_MAT_NAME\s*=.*', 'BASE_MAT_NAME = "base_material"', tpl2, flags=re.M)
            tpl2 = re.sub(r'^BUILD_MAT_NAME\s*=.*','BUILD_MAT_NAME = "additive_material"', tpl2, flags=re.M)
            tpl2 = re.sub(r'^BASE_SEC_NAME\s*=.*', 'BASE_SEC_NAME = "base_sec"', tpl2, flags=re.M)
            tpl2 = re.sub(r'^BUILD_SEC_NAME\s*=.*','BUILD_SEC_NAME = "additive_sec"', tpl2, flags=re.M)
            tpl2 = re.sub(r'^MODEL_NAME\s*=.*',    'MODEL_NAME = "Model-1"', tpl2, flags=re.M)
            tpl2 = re.sub(r'^TOL\s*=.*',           'TOL = %s' % 1.0e-9, tpl2, flags=re.M)
            tpl2 = re.sub(r'^TIME_INTERVAL\s*=.*', 'TIME_INTERVAL = %s' % float(self.fout_dt.value()), tpl2, flags=re.M)
            tpl2 = re.sub(r'^HT_ENABLED\s*=.*',
                          'HT_ENABLED = %d' % (1 if self.ht_build_chk.isChecked() else 0),
                          tpl2, flags=re.M)
            tpl2 = re.sub(r'^HT_TEMP_C\s*=.*',
                          'HT_TEMP_C = %s' % float(self.settings.get("ht_temp_c", 650.0)),
                          tpl2, flags=re.M)
            tpl2 = re.sub(r'^BOTTOM_LAYER_REMOVAL\s*=.*',
                          'BOTTOM_LAYER_REMOVAL = %d' % int(self.bottom_remove_sp.value()),
                          tpl2, flags=re.M)

            apply_patched = Path(self._tmpdir.name) / "apply_materials_patched.py"
            apply_patched.write_text(tpl2, "utf-8")

            cmd2 = [self.settings.get("abaqus_cmd", DEFAULT_ABAQUS_CMD), "cae", f"noGUI={apply_patched}"]
            # 材料→网格→边界（on_finished_extra 串联）
            self._launch(cmd2, self._last_cae.parent, self.log, self.run_btn,
                          stop_button=self.stop_btn, on_finished_extra=_run_mesh)

        self._launch(cmd_import, save_dir, self.log, self.run_btn, stop_button=self.stop_btn, on_finished_extra=_after_import)


# --------------------------- Input & UTEMP Tab ---------------------------
class InputAndUtempTab(QtWidgets.QWidget, LaunchMixin):
    def __init__(self, settings):
        super().__init__()
        self.settings = settings
        self._tpl = Path(self.settings.get("input_script", str(DEFAULT_INPUT_SCRIPT)))

        form = QtWidgets.QFormLayout()

        self.cae_le = QtWidgets.QLineEdit()
        cae_btn = QtWidgets.QPushButton("…"); cae_btn.clicked.connect(self._pick_cae)
        hl_cae = QtWidgets.QHBoxLayout(); hl_cae.addWidget(self.cae_le); hl_cae.addWidget(cae_btn)
        form.addRow("CAE file", hl_cae)

        # --- Add this right after the CAE file picker rows in InputAndUtempTab.__init__ ---
        
        # Build axis + zero plane (for UTEMP)
        self.axis_cb = QtWidgets.QComboBox()
        # Prefer to mirror previously used setting if present, default to settings.get("build_axis", "Y")
        self.axis_cb.addItems(["X", "Y", "Z"])
        self.axis_cb.setCurrentText(self.settings.get("build_axis", "Y"))
        
        self.axis_zero_sp = QtWidgets.QDoubleSpinBox()
        self.axis_zero_sp.setRange(-1e12, 1e12)
        self.axis_zero_sp.setDecimals(6)
        self.axis_zero_sp.setSingleStep(0.1)
        self.axis_zero_sp.setValue(float(self.settings.get("axis_zero", 0.0)))
        
        form.addRow("Build axis (UTEMP)", self.axis_cb)
        form.addRow("Axis zero (plane)", self.axis_zero_sp)

        # ---- Post Heat Treatment (UTEMP scope ONLY) ----
        self.ht_input_chk = QtWidgets.QCheckBox("Enable post heat treatment in UTEMP")
        self.ht_input_chk.setChecked(bool(self.settings.get("ht_input_enabled", False)))
        form.addRow("", self.ht_input_chk)
        
        self.ht_temp_ds = QtWidgets.QDoubleSpinBox()
        self.ht_temp_ds.setDecimals(1); self.ht_temp_ds.setRange(25.0, 1200.0)
        self.ht_temp_ds.setSuffix(" °C")
        self.ht_temp_ds.setValue(float(self.settings.get("ht_temp_c", 650.0)))
        self.ht_temp_ds.setEnabled(self.ht_input_chk.isChecked())
        form.addRow("HT soak temperature", self.ht_temp_ds)
        
        def _toggle_ht_input(on):
            self.ht_temp_ds.setEnabled(on)
        self.ht_input_chk.toggled.connect(_toggle_ht_input)
        _toggle_ht_input(self.ht_input_chk.isChecked())

        self.temp_step = QtWidgets.QSpinBox(); self.temp_step.setRange(1, 100); self.temp_step.setValue(5)
        form.addRow("Temperature step", self.temp_step)

        self.temp_initial = QtWidgets.QSpinBox(); self.temp_initial.setRange(300, 2500); self.temp_initial.setValue(1100)
        form.addRow("Temperature start", self.temp_initial)

        self.temp_interval = QtWidgets.QSpinBox(); self.temp_interval.setRange(1, 200); self.temp_interval.setValue(50)
        form.addRow("Temperature interval", self.temp_interval)

        self.grad_step = QtWidgets.QSpinBox(); self.grad_step.setRange(1, 100); self.grad_step.setValue(5)
        form.addRow("T_gradient step", self.grad_step)

        self.grad_initial = QtWidgets.QSpinBox(); self.grad_initial.setRange(10, 500); self.grad_initial.setValue(100)
        form.addRow("T_gradient start", self.grad_initial)

        self.grad_interval = QtWidgets.QSpinBox(); self.grad_interval.setRange(1, 50); self.grad_interval.setValue(5)
        form.addRow("T_gradient interval", self.grad_interval)

        self.layer_n = QtWidgets.QSpinBox(); self.layer_n.setRange(1, 1000); self.layer_n.setValue(24)
        form.addRow("Layer number", self.layer_n)

        self.layer_sp = QtWidgets.QDoubleSpinBox()
        self.layer_sp.setDecimals(2); self.layer_sp.setRange(0.01, 5.0); self.layer_sp.setSuffix(" mm"); self.layer_sp.setValue(0.5)
        form.addRow("Layer thickness", self.layer_sp)

        self.dir_le = QtWidgets.QLineEdit(self.settings.get("default_save_dir", str(SCRIPT_DIR)))
        btn = QtWidgets.QPushButton("…"); btn.clicked.connect(self._pick_dir)
        hl = QtWidgets.QHBoxLayout(); hl.addWidget(self.dir_le); hl.addWidget(btn)
        form.addRow("Output dir", hl)

        self.run_btn = QtWidgets.QPushButton("Generate Input & UTEMP →"); self.run_btn.clicked.connect(self._run_all)
        self.stop_btn = QtWidgets.QPushButton("Stop"); self.stop_btn.setEnabled(False); self.stop_btn.clicked.connect(self._stop_running)
        hb = QtWidgets.QHBoxLayout(); hb.addWidget(self.run_btn); hb.addWidget(self.stop_btn)

        self.log = QtWidgets.QPlainTextEdit(); self.log.setReadOnly(True)

        layout = QtWidgets.QVBoxLayout(self)
        layout.addLayout(form)
        layout.addLayout(hb)
        layout.addWidget(self.log, 1)

        self._tmpdir = None

    def _pick_cae(self):
        f, _ = QtWidgets.QFileDialog.getOpenFileName(self, "Select CAE file", "", "CAE files (*.cae);;All files (*)")
        if f: self.cae_le.setText(f)

    def _pick_dir(self):
        d = QtWidgets.QFileDialog.getExistingDirectory(self, "Select directory")
        if d: self.dir_le.setText(d)

    def _stop_running(self):
        if hasattr(self, "_worker") and self._worker.isRunning():
            self.log.appendPlainText("[GUI] 收到停止请求，正在终止整棵进程树 ...")
            self.run_btn.setEnabled(True); self.stop_btn.setEnabled(False)
            self._worker.stop(kill_tree=True)
        else:
            self.log.appendPlainText("[GUI] 当前无运行中的任务。")

    def _run_all(self):
        out_dir = Path(self.dir_le.text()).expanduser().resolve(); out_dir.mkdir(parents=True, exist_ok=True)
        cae_file = self.cae_le.text().strip()
        if not cae_file:
            QtWidgets.QMessageBox.critical(self, "No CAE file", "请先选择 .cae 文件。"); return

        self._tmpdir = tempfile.TemporaryDirectory()
        patched = Path(self._tmpdir.name) / "create_input_patched.py"

        # persist for this session
        self.settings["ht_input_enabled"] = bool(self.ht_input_chk.isChecked())
        self.settings["ht_temp_c"] = float(self.ht_temp_ds.value())


        src = self._tpl.read_text("utf-8")
        warnings = []

        # Map axis → Abaqus coordinate index (1-based)
        axis_map = {"X": 1, "Y": 2, "Z": 3}
        axis_sel = self.axis_cb.currentText().upper()
        coord_idx = axis_map.get(axis_sel, 2)  # default Y if anything odd
        axis_zero = float(self.axis_zero_sp.value())
        
        # remember for next time
        self.settings["build_axis"] = axis_sel
        self.settings["axis_zero"]  = axis_zero
        
        txt = (
            f"CAE_FILE = r'{cae_file}'\n"
            f"COORD_IDX = {coord_idx}\n"
            f"AXIS_ZERO = {axis_zero}\n"
            f"HT_ENABLED = {1 if self.ht_input_chk.isChecked() else 0}\n"
            f"HT_TEMP_C  = {float(self.ht_temp_ds.value())}\n"
            + src
        )

        txt2, n_om = re.subn(r"openMdb\([^)]*\)", "openMdb(pathName=CAE_FILE)", txt, count=1)
        if n_om == 0:
            warnings.append("未在模板中找到 openMdb(...)；将在顶部注入。")
            lines = txt.splitlines(True)
            ins = "from abaqus import mdb\nfrom abaqus import *\nfrom caeModules import *\nopenMdb(pathName=CAE_FILE)\n"
            if lines: lines.insert(1, ins); txt2 = "".join(lines)
            else: txt2 = txt + "\n" + ins

        pattern_call = r"\bcreate_input\s*\(\s*\)"
        repl_call = ("create_input("
                      "temp_step, temp_initial, temp_interval, "
                      "grad_step, grad_initial, grad_interval)")
        txt3, n_ci = re.subn(pattern_call, repl_call, txt2, count=1)
        if n_ci == 0 and re.search(r"\bcreate_input\s*\(", txt2) is None:
            warnings.append("未找到 create_input() 调用；将在文件末尾追加。")
            txt3 = txt2 + f"\n# GUI fallback call\n{repl_call}\n"

        grid = f"""
# ===== GUI injected parameters =====
temp_step    = {self.temp_step.value()}
temp_initial = {self.temp_initial.value()}
temp_interval= {self.temp_interval.value()}
grad_step    = {self.grad_step.value()}
grad_initial = {self.grad_initial.value()}
grad_interval= {self.grad_interval.value()}
layer_n      = {self.layer_n.value()}
layer_sp     = {self.layer_sp.value()}
# ===================================
"""
        txt4, n_mod = re.subn(r"#\s*modification.*?\n", grid, txt3, count=1, flags=re.S)
        if n_mod == 0:
            warnings.append("未找到 '# modification' 锚点；参数块已追加到文件末尾。")
            txt4 = txt3 + "\n" + grid

        patched.write_text(txt4, "utf-8")
        for w in warnings: self.log.appendPlainText("[警告] " + w)

        cmd = [self.settings.get("abaqus_cmd", DEFAULT_ABAQUS_CMD), "cae", f"noGUI={patched}"]
        self._launch(cmd, out_dir, self.log, self.run_btn, stop_button=self.stop_btn)


# --------------------------- Data Extract Tab (NEW) ---------------------------
# imports near the top (ensure these exist)
import os, re, tempfile
from pathlib import Path


class GridBuilderDialog(QtWidgets.QDialog):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Grid builder")
        form = QtWidgets.QFormLayout(self)

        self.normal_cb = QtWidgets.QComboBox()
        self.normal_cb.addItems(["X", "Y", "Z"])
        form.addRow("Plane normal", self.normal_cb)

        self.plane_ds = QtWidgets.QDoubleSpinBox()
        self.plane_ds.setRange(-1e9, 1e9)
        self.plane_ds.setDecimals(6)
        self.plane_ds.setValue(0.0)
        form.addRow("Plane value", self.plane_ds)

        def _make_range(label_prefix):
            start = QtWidgets.QDoubleSpinBox(); start.setRange(-1e9, 1e9); start.setDecimals(6); start.setValue(0.0)
            end   = QtWidgets.QDoubleSpinBox(); end.setRange(-1e9, 1e9); end.setDecimals(6); end.setValue(1.0)
            step  = QtWidgets.QDoubleSpinBox(); step.setRange(1e-9, 1e9); step.setDecimals(6); step.setValue(0.1)
            row = QtWidgets.QHBoxLayout(); row.addWidget(QtWidgets.QLabel("Start")); row.addWidget(start)
            row.addWidget(QtWidgets.QLabel("End")); row.addWidget(end)
            row.addWidget(QtWidgets.QLabel("Step")); row.addWidget(step)
            return start, end, step, row, f"{label_prefix} range (start/end/step)"

        self.x_start, self.x_end, self.x_step, rowx, labx = _make_range("X")
        form.addRow(labx, rowx)
        self.y_start, self.y_end, self.y_step, rowy, laby = _make_range("Y")
        form.addRow(laby, rowy)
        self.z_start, self.z_end, self.z_step, rowz, labz = _make_range("Z")
        form.addRow(labz, rowz)

        def _toggle_ranges():
            n = self.normal_cb.currentText().upper()
            is_x = n == "X"
            is_y = n == "Y"
            is_z = n == "Z"
            # When normal is Z, disable Z range; use plane value for Z
            rowz.setEnabled(not is_z)
            # When normal is X, disable X range
            rowx.setEnabled(not is_x)
            # When normal is Y, disable Y range
            rowy.setEnabled(not is_y)

        self.normal_cb.currentIndexChanged.connect(_toggle_ranges)
        _toggle_ranges()

        bb = QtWidgets.QDialogButtonBox(QtWidgets.QDialogButtonBox.Ok | QtWidgets.QDialogButtonBox.Cancel)
        bb.accepted.connect(self.accept); bb.rejected.connect(self.reject)
        form.addRow(bb)

    def build_grid(self):
        normal = self.normal_cb.currentText().upper()
        # Validate steps
        def _vec(start, end, step):
            if step <= 0:
                raise ValueError("Step must be > 0")
            # Include end if it aligns within half a step
            return np.arange(start, end + step * 0.5, step)

        if normal == "Z":
            xs = _vec(self.x_start.value(), self.x_end.value(), self.x_step.value())
            ys = _vec(self.y_start.value(), self.y_end.value(), self.y_step.value())
            zz = float(self.plane_ds.value())
            X, Y = np.meshgrid(xs, ys, indexing="xy")
            coords = np.column_stack([X.ravel(), Y.ravel(), np.full(X.size, zz)])
        elif normal == "X":
            ys = _vec(self.y_start.value(), self.y_end.value(), self.y_step.value())
            zs = _vec(self.z_start.value(), self.z_end.value(), self.z_step.value())
            xx = float(self.plane_ds.value())
            Y, Z = np.meshgrid(ys, zs, indexing="xy")
            coords = np.column_stack([np.full(Y.size, xx), Y.ravel(), Z.ravel()])
        else:  # normal == "Y"
            xs = _vec(self.x_start.value(), self.x_end.value(), self.x_step.value())
            zs = _vec(self.z_start.value(), self.z_end.value(), self.z_step.value())
            yy = float(self.plane_ds.value())
            X, Z = np.meshgrid(xs, zs, indexing="xy")
            coords = np.column_stack([X.ravel(), np.full(X.size, yy), Z.ravel()])

        if coords.size == 0:
            raise ValueError("Empty grid (check ranges and steps).")
        return coords


class BuildMeasurementDialog(QtWidgets.QDialog):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Build measurement")
        form = QtWidgets.QFormLayout(self)

        self.meas_le = QtWidgets.QLineEdit()
        b_meas = QtWidgets.QPushButton("Pick")
        b_meas.clicked.connect(lambda: self._pick_file(self.meas_le, is_measure=True))
        row_meas = QtWidgets.QHBoxLayout(); row_meas.addWidget(self.meas_le); row_meas.addWidget(b_meas)
        form.addRow("Measurement CSV (x,y,value):", row_meas)

        self.grid_le = QtWidgets.QLineEdit()
        b_grid = QtWidgets.QPushButton("Pick")
        b_grid.clicked.connect(lambda: self._pick_file(self.grid_le, is_measure=False))
        row_grid = QtWidgets.QHBoxLayout(); row_grid.addWidget(self.grid_le); row_grid.addWidget(b_grid)
        form.addRow("Grid file (from Grid builder):", row_grid)

        self.out_dir_le = QtWidgets.QLineEdit()
        b_out = QtWidgets.QPushButton("Select")
        b_out.clicked.connect(self._pick_out_dir)
        row_out = QtWidgets.QHBoxLayout(); row_out.addWidget(self.out_dir_le); row_out.addWidget(b_out)
        form.addRow("Output folder:", row_out)

        self.run_btn = QtWidgets.QPushButton("Extract && Save")
        self.run_btn.clicked.connect(self._run_extraction)
        b_close = QtWidgets.QPushButton("Close"); b_close.clicked.connect(self.reject)
        row_btns = QtWidgets.QHBoxLayout(); row_btns.addWidget(self.run_btn); row_btns.addWidget(b_close)
        form.addRow("", row_btns)

        self.log = QtWidgets.QPlainTextEdit(); self.log.setReadOnly(True)
        self.log.setPlaceholderText("Status and file paths will appear here...")
        form.addRow("Log:", self.log)

    def _pick_file(self, line, is_measure=False):
        f, _ = QtWidgets.QFileDialog.getOpenFileName(self, "Select file", "", "CSV/TXT (*.csv *.txt);;All files (*)")
        if f:
            line.setText(f)
            if is_measure and not self.out_dir_le.text().strip():
                self.out_dir_le.setText(str(Path(f).parent))

    def _pick_out_dir(self):
        d = QtWidgets.QFileDialog.getExistingDirectory(self, "Select output folder")
        if d:
            self.out_dir_le.setText(d)

    def _load_measurement(self, path):
        import pandas as pd
        df = pd.read_csv(path, header=None, sep=None, engine="python", comment="#")
        if df.shape[1] < 3:
            raise ValueError("Expected at least 3 columns (x,y,value) in measurement CSV.")
        df = df.iloc[:, :3]
        df.columns = ["x", "y", "value"]
        for c in df.columns:
            df[c] = pd.to_numeric(df[c], errors="coerce")
        df = df.dropna()
        if df.empty:
            raise ValueError("No numeric rows found in measurement CSV.")
        return df

    def _load_grid(self, path):
        import pandas as pd
        df = pd.read_csv(path, header=None, sep=None, engine="python", comment="#")
        if df.shape[1] < 2:
            raise ValueError("Grid file must have at least two columns (x,y).")
        df = df.iloc[:, :2]
        df.columns = ["x", "y"]
        df["x"] = pd.to_numeric(df["x"], errors="coerce")
        df["y"] = pd.to_numeric(df["y"], errors="coerce")
        df = df.dropna()
        if df.empty:
            raise ValueError("No numeric rows found in grid file.")
        return df

    def _run_extraction(self):
        self.log.clear()
        try:
            import pandas as pd  # noqa: F401
            import csv
            import numpy as np
        except Exception as e:
            QtWidgets.QMessageBox.critical(self, "Missing dependency", f"pandas or csv import failed: {e}")
            return

        meas_path = Path(self.meas_le.text().strip())
        grid_path = Path(self.grid_le.text().strip())
        if not meas_path.is_file():
            QtWidgets.QMessageBox.warning(self, "Missing measurement file", "Please select a measurement CSV (x,y,value).")
            return
        if not grid_path.is_file():
            QtWidgets.QMessageBox.warning(self, "Missing grid file", "Please select a grid file generated by Grid Builder.")
            return

        out_dir_txt = self.out_dir_le.text().strip()
        out_dir = Path(out_dir_txt) if out_dir_txt else meas_path.parent
        try:
            out_dir.mkdir(parents=True, exist_ok=True)
        except Exception as e:
            QtWidgets.QMessageBox.critical(self, "Cannot create output folder", str(e))
            return

        try:
            meas_df = self._load_measurement(meas_path)
            grid_df = self._load_grid(grid_path)
        except Exception as e:
            QtWidgets.QMessageBox.critical(self, "Load error", str(e))
            return

        round_dec = 6  # align with grid builder output formatting
        meas_map = {}
        for _, r in meas_df.iterrows():
            key = (round(float(r["x"]), round_dec), round(float(r["y"]), round_dec))
            if key not in meas_map:
                meas_map[key] = r["value"]

        meas_xy = meas_df[["x", "y"]].to_numpy(dtype=float)
        meas_vals = meas_df["value"].to_numpy()

        rows = []
        values_only = []
        approx = 0
        for _, r in grid_df.iterrows():
            key = (round(float(r["x"]), round_dec), round(float(r["y"]), round_dec))
            if key in meas_map:
                v = meas_map[key]
            else:
                # Nearest-neighbour fallback in 2D to avoid blank outputs
                gx, gy = float(r["x"]), float(r["y"])
                diffs = meas_xy - np.array([gx, gy])
                d2 = np.sum(diffs * diffs, axis=1)
                idx = int(np.argmin(d2))
                v = meas_vals[idx]
                approx += 1
            rows.append((r["x"], r["y"], v))
            values_only.append(v)

        table_name = f"{meas_path.stem}__sampled_on_{grid_path.stem}.csv"
        table_path = out_dir / table_name
        try:
            with open(table_path, "w", newline="") as f:
                w = csv.writer(f)
                w.writerow(["x", "y", "value"])
                for x, y, v in rows:
                    w.writerow([x, y, v])
        except Exception as e:
            QtWidgets.QMessageBox.critical(self, "Save error", f"Failed to write sampled CSV: {e}")
            return

        formatted_vals = []
        for v in values_only:
            if v == "" or v is None:
                formatted_vals.append("")
            else:
                try:
                    formatted_vals.append("{:.6f}".format(float(v)))
                except Exception:
                    formatted_vals.append(str(v))

        values_only_name = f"values_only__measurement__{grid_path.stem}.csv"
        values_only_path = out_dir / values_only_name
        try:
            with open(values_only_path, "w", newline="") as f:
                w = csv.writer(f)
                w.writerow(["", ""] + formatted_vals)
        except Exception as e:
            QtWidgets.QMessageBox.critical(self, "Save error", f"Failed to write values-only CSV: {e}")
            return

        self.log.appendPlainText(f"Measurement rows: {len(meas_df)} | Grid points: {len(grid_df)} | Exact matches: {len(values_only) - approx} | Nearest-neighbour: {approx}")
        self.log.appendPlainText(f"Saved sampled CSV -> {table_path}")
        self.log.appendPlainText(f"Saved values-only CSV -> {values_only_path}")
        if approx:
            self.log.appendPlainText("[INFO] Some grid points used nearest-neighbour lookup (no exact x,y match).")
        QtWidgets.QMessageBox.information(self, "Build measurement", f"Done.\nSampled CSV:\n{table_path}\nValues-only CSV:\n{values_only_path}")

class DataExtractTab(QtWidgets.QWidget, LaunchMixin):
    def __init__(self, settings):
        super().__init__()
        self.settings = settings
        self._tmpdir = None
        self._setup_ui()

    def _setup_ui(self):
        form = QtWidgets.QFormLayout()

        # ODB folder
        self.odb_dir_le = QtWidgets.QLineEdit()
        b1 = QtWidgets.QPushButton("Select ODB Folder…"); b1.clicked.connect(self._pick_odb_dir)
        row1 = QtWidgets.QHBoxLayout(); row1.addWidget(self.odb_dir_le); row1.addWidget(b1)
        form.addRow("ODB Folder:", row1)

        # Plane, position, tolerance
        self.plane_cb = QtWidgets.QComboBox(); self.plane_cb.addItems(["XY","XZ","YZ"])
        self.pos_sb = QtWidgets.QDoubleSpinBox(); self.pos_sb.setRange(-1e12, 1e12); self.pos_sb.setDecimals(6); self.pos_sb.setValue(0.0)
        self.tol_sb = QtWidgets.QDoubleSpinBox(); self.tol_sb.setRange(0.0, 1e6); self.tol_sb.setDecimals(6); self.tol_sb.setValue(1e-3)
        form.addRow("Plane:", self.plane_cb)
        form.addRow("Plane position (model units):", self.pos_sb)
        form.addRow("Plane tolerance:", self.tol_sb)

        # Variable & position
        self.var_cb = QtWidgets.QComboBox(); self.var_cb.addItems(["NT11","Mises","S11","S22","S33", "U1", "U2", "U3", "UMAG"])
        self.posn_cb = QtWidgets.QComboBox(); self.posn_cb.addItems(["Unique Nodal","Integration Point"])
        form.addRow("Output Variable:", self.var_cb)
        form.addRow("Variable Position:", self.posn_cb)

        # Step / Frame
        self.step_le = QtWidgets.QLineEdit("last")   # allow "last" | name | index
        self.frame_le = QtWidgets.QLineEdit("last")  # allow "last" | index
        form.addRow("Step (last | name | index):", self.step_le)
        form.addRow("Frame (last | index):", self.frame_le)

        # Output folder
        self.out_dir_le = QtWidgets.QLineEdit()
        b2 = QtWidgets.QPushButton("Select Output Folder…"); b2.clicked.connect(self._pick_out_dir)
        row2 = QtWidgets.QHBoxLayout(); row2.addWidget(self.out_dir_le); row2.addWidget(b2)
        form.addRow("Output Folder:", row2)

        # --- NEW: Coordinate sampling (IDW) controls ---
        self.coord_file_le = QtWidgets.QLineEdit()
        b3 = QtWidgets.QPushButton("Pick…")
        b3.clicked.connect(lambda: self._pick_file(self.coord_file_le))
        b3b = QtWidgets.QPushButton("Grid builder…")
        b3b.clicked.connect(self._open_grid_builder)
        row3 = QtWidgets.QHBoxLayout(); row3.addWidget(self.coord_file_le); row3.addWidget(b3); row3.addWidget(b3b)
        form.addRow("Coordinate file (x,y,z per line):", row3)
        
        self.idw_k_sp = QtWidgets.QSpinBox()
        self.idw_k_sp.setRange(1, 64); self.idw_k_sp.setValue(4)
        form.addRow("IDW K (neighbours):", self.idw_k_sp)
        
        self.idw_radius_ds = QtWidgets.QDoubleSpinBox()
        self.idw_radius_ds.setDecimals(6); self.idw_radius_ds.setRange(0.0, 1e9); self.idw_radius_ds.setValue(1e-3)
        form.addRow("IDW radius (model units):", self.idw_radius_ds)
        
        self.idw_power_ds = QtWidgets.QDoubleSpinBox()
        self.idw_power_ds.setDecimals(2); self.idw_power_ds.setRange(0.1, 10.0); self.idw_power_ds.setValue(2.0)
        form.addRow("IDW power (p):", self.idw_power_ds)
        
        # Small hint so users know plane inputs are ignored when coord file is set
        hint = QtWidgets.QLabel("Hint: If a coordinate file is selected, plane selection is ignored (IDW mode).")
        hint.setStyleSheet("color: #888;")
        form.addRow("", hint)
        # -----------------------------------------------

        def _toggle_plane_vs_idw():
            use_idw = bool(self.coord_file_le.text().strip())
            # Plane widgets
            self.plane_cb.setEnabled(not use_idw)
            self.pos_sb.setEnabled(not use_idw)
            self.tol_sb.setEnabled(not use_idw)
            # IDW widgets
            self.idw_k_sp.setEnabled(True)
            self.idw_radius_ds.setEnabled(True)
            self.idw_power_ds.setEnabled(True)
        
        self.coord_file_le.textChanged.connect(lambda _: _toggle_plane_vs_idw())
        _toggle_plane_vs_idw()


        # Run/Stop
        self.run_btn = QtWidgets.QPushButton("Extract (one CSV per ODB)"); self.run_btn.clicked.connect(self._run_extraction)
        self.stop_btn = QtWidgets.QPushButton("Stop"); self.stop_btn.setEnabled(False); self.stop_btn.clicked.connect(self._stop_running)
        hb = QtWidgets.QHBoxLayout(); hb.addWidget(self.run_btn); hb.addWidget(self.stop_btn)

        self.log = QtWidgets.QPlainTextEdit(); self.log.setReadOnly(True)

        self.build_measure_btn = QtWidgets.QPushButton("Build measurement")
        self.build_measure_btn.clicked.connect(self._open_measurement_dialog)
        hb.addWidget(self.build_measure_btn)

        v = QtWidgets.QVBoxLayout(self); v.addLayout(form); v.addLayout(hb); v.addWidget(self.log, 1)

    def _pick_odb_dir(self):
        d = QtWidgets.QFileDialog.getExistingDirectory(self, "Select folder with .odb files")
        if d:
            self.odb_dir_le.setText(d)
            if not self.out_dir_le.text():
                self.out_dir_le.setText(d)

    def _pick_out_dir(self):
        d = QtWidgets.QFileDialog.getExistingDirectory(self, "Select output folder for CSVs")
        if d:
            self.out_dir_le.setText(d)

    def _pick_file(self, line):
        f, _ = QtWidgets.QFileDialog.getOpenFileName(self, "Select file", "", "Text/CSV (*.txt *.csv);;All files (*)")
        if f:
            line.setText(f)

    def _open_grid_builder(self):
        dlg = GridBuilderDialog(self)
        if dlg.exec_():
            try:
                coords = dlg.build_grid()
            except Exception as e:
                QtWidgets.QMessageBox.critical(self, "Grid error", str(e))
                return
            path, _ = QtWidgets.QFileDialog.getSaveFileName(
                self, "Save generated grid", "", "CSV files (*.csv);;Text files (*.txt);;All files (*)"
            )
            if not path:
                return
            try:
                if path.lower().endswith(".csv"):
                    np.savetxt(path, coords, fmt="%.6f", delimiter=",")
                else:
                    np.savetxt(path, coords, fmt="%.6f")
                self.coord_file_le.setText(path)
            except Exception as e:
                QtWidgets.QMessageBox.critical(self, "Save failed", str(e))

    def _open_measurement_dialog(self):
        dlg = BuildMeasurementDialog(self)
        dlg.exec_()
    def _stop_running(self):
        if hasattr(self, "_worker") and self._worker.isRunning():
            self.log.appendPlainText("[GUI] 收到停止请求，正在终止整棵进程树 ...")
            self.run_btn.setEnabled(True); self.stop_btn.setEnabled(False)
            self._worker.stop(kill_tree=True)
        else:
            self.log.appendPlainText("[GUI] 当前无运行中的任务。")

    def _inject_and_write(self, tpl_path, work_dir):
        txt = Path(tpl_path).read_text(encoding="utf-8")

        # normalize paths (forward slashes)
        odb_dir = Path(self.odb_dir_le.text()).expanduser().resolve().as_posix()
        out_dir = Path(self.out_dir_le.text()).expanduser().resolve().as_posix()
        plane   = self.plane_cb.currentText()
        pos     = float(self.pos_sb.value())
        tol     = float(self.tol_sb.value())
        var     = self.var_cb.currentText()
        vpos    = self.posn_cb.currentText()
        step    = self.step_le.text().strip() or "last"
        frame   = self.frame_le.text().strip() or "last"

        subs = {
            r'^ODB_DIR\s*=.*':       'ODB_DIR = r"%s"' % odb_dir,
            r'^OUTPUT_DIR\s*=.*':    'OUTPUT_DIR = r"%s"' % out_dir,
            r'^PLANE\s*=.*':         'PLANE = "%s"' % plane,
            r'^PLANE_POS\s*=.*':     'PLANE_POS = %s' % pos,
            r'^VARIABLE\s*=.*':      'VARIABLE = "%s"' % var,
            r'^VAR_POSITION\s*=.*':  'VAR_POSITION = "%s"' % vpos,
            r'^TOL\s*=.*':           'TOL = %s' % tol,
            r'^STEP_SELECT\s*=.*':   'STEP_SELECT = "%s"' % step,
            r'^FRAME_SELECT\s*=.*':  'FRAME_SELECT = "%s"' % frame,
            r'^COORD_FILE\s*=.*':   'COORD_FILE = r"%s"' % self.coord_file_le.text().strip().replace('\\','/'),
            r'^IDW_K\s*=.*':        'IDW_K = %d' % int(self.idw_k_sp.value()),
            r'^IDW_RADIUS\s*=.*':   'IDW_RADIUS = %s' % float(self.idw_radius_ds.value()),
            r'^IDW_POWER\s*=.*':    'IDW_POWER = %s' % float(self.idw_power_ds.value()),

        }
        # force UTF-8 cookie if any other cookie is present
        txt = re.sub(r'^\s*#\s*-\*-\s*coding\s*:\s*.*?-\*-\s*$', '# -*- coding: utf-8 -*-', txt, flags=re.M)
        for pat, rep in subs.items():
            txt = re.sub(pat, lambda m, rep=rep: rep, txt, flags=re.M)

        run_py = Path(work_dir) / "data_extract_run.py"
        run_py.write_text(txt, encoding="utf-8")
        return str(run_py)

    def _run_extraction(self):
        odb_dir = self.odb_dir_le.text().strip()
        out_dir = self.out_dir_le.text().strip()
        if not os.path.isdir(odb_dir):
            QtWidgets.QMessageBox.warning(self, "Missing ODB folder", "Please select a valid folder containing .odb files.")
            return
        if not out_dir:
            QtWidgets.QMessageBox.warning(self, "Missing output folder", "Please select an output folder for CSVs.")
            return
        if not os.path.isdir(out_dir):
            try:
                os.makedirs(out_dir)
            except Exception as e:
                QtWidgets.QMessageBox.critical(self, "Cannot create folder", str(e))
                return

        tpl_path = (Path(__file__).resolve().parent / "data_extract.py")
        if not tpl_path.exists():
            QtWidgets.QMessageBox.critical(self, "Missing file", "data_extract.py not found next to the GUI script.")
            return

        if not self._tmpdir:
            self._tmpdir = tempfile.TemporaryDirectory()

        run_py = self._inject_and_write(str(tpl_path), self._tmpdir.name)

        # Headless ODB API (no CAE session):
        cmd = [self.settings.get("abaqus_cmd", "abaqus"), "python", run_py]
        self._launch(cmd, Path(__file__).resolve().parent, self.log, self.run_btn, stop_button=self.stop_btn)


# --------------------------- Data Alignment Tab (NEW) ---------------------------
class DataAlignmentTab(QtWidgets.QWidget):
    def __init__(self, settings):
        super().__init__()
        self.settings = settings
        self.ref_points = None
        self.float_points = None
        self._scale_x = 1.0
        self._scale_y = 1.0
        self._ref_size = 18
        self._float_size = 18
        self._setup_ui()

    def _setup_ui(self):
        layout = QtWidgets.QHBoxLayout(self)

        # Left controls
        form = QtWidgets.QFormLayout()

        self.ref_le = QtWidgets.QLineEdit()
        self.ref_le.setPlaceholderText("No reference file selected")
        self.ref_le.setReadOnly(True)
        btn_ref = QtWidgets.QPushButton("Upload ref")
        btn_ref.clicked.connect(lambda: self._load_file(is_ref=True))
        hl_ref = QtWidgets.QHBoxLayout()
        hl_ref.addWidget(self.ref_le)
        hl_ref.addWidget(btn_ref)
        form.addRow("Reference (.txt)", hl_ref)

        self.float_le = QtWidgets.QLineEdit()
        self.float_le.setPlaceholderText("No float file selected")
        self.float_le.setReadOnly(True)
        btn_float = QtWidgets.QPushButton("Upload float")
        btn_float.clicked.connect(lambda: self._load_file(is_ref=False))
        hl_float = QtWidgets.QHBoxLayout()
        hl_float.addWidget(self.float_le)
        hl_float.addWidget(btn_float)
        form.addRow("Float (.txt)", hl_float)

        self.status_lbl = QtWidgets.QLabel("Load reference and float .txt files (x y z columns).")
        self.status_lbl.setWordWrap(True)
        form.addRow(self.status_lbl)

        # Symbol size controls
        self.ref_size_sp = QtWidgets.QSpinBox()
        self.ref_size_sp.setRange(2, 128)
        self.ref_size_sp.setValue(self._ref_size)
        self.ref_size_sp.setSuffix(" pt")
        self.ref_size_sp.valueChanged.connect(self._update_plot)
        form.addRow("Ref marker size", self.ref_size_sp)

        self.float_size_sp = QtWidgets.QSpinBox()
        self.float_size_sp.setRange(2, 128)
        self.float_size_sp.setValue(self._float_size)
        self.float_size_sp.setSuffix(" pt")
        self.float_size_sp.valueChanged.connect(self._update_plot)
        form.addRow("Float marker size", self.float_size_sp)

        # Float transform controls
        self.shift_x_ds = QtWidgets.QDoubleSpinBox()
        self.shift_x_ds.setRange(-1e9, 1e9)
        self.shift_x_ds.setDecimals(4)
        self.shift_x_ds.setSingleStep(0.1)
        self.shift_x_ds.valueChanged.connect(self._update_plot)
        form.addRow("Float shift X", self.shift_x_ds)

        self.shift_y_ds = QtWidgets.QDoubleSpinBox()
        self.shift_y_ds.setRange(-1e9, 1e9)
        self.shift_y_ds.setDecimals(4)
        self.shift_y_ds.setSingleStep(0.1)
        self.shift_y_ds.valueChanged.connect(self._update_plot)
        form.addRow("Float shift Y", self.shift_y_ds)

        self.scale_x_ds = QtWidgets.QDoubleSpinBox()
        self.scale_x_ds.setRange(0.001, 1e6)
        self.scale_x_ds.setDecimals(4)
        self.scale_x_ds.setSingleStep(0.05)
        self.scale_x_ds.setValue(self._scale_x)
        self.scale_x_ds.valueChanged.connect(self._update_plot)
        form.addRow("Float scale X", self.scale_x_ds)

        self.scale_y_ds = QtWidgets.QDoubleSpinBox()
        self.scale_y_ds.setRange(0.001, 1e6)
        self.scale_y_ds.setDecimals(4)
        self.scale_y_ds.setSingleStep(0.05)
        self.scale_y_ds.setValue(self._scale_y)
        self.scale_y_ds.valueChanged.connect(self._update_plot)
        form.addRow("Float scale Y", self.scale_y_ds)

        self.colorbar_chk = QtWidgets.QCheckBox("Color by Z (show colorbar)")
        self.colorbar_chk.setChecked(True)
        self.colorbar_chk.toggled.connect(self._update_plot)
        form.addRow("", self.colorbar_chk)

        self.rot_deg_ds = QtWidgets.QDoubleSpinBox()
        self.rot_deg_ds.setRange(-360.0, 360.0)
        self.rot_deg_ds.setDecimals(3)
        self.rot_deg_ds.setSingleStep(1.0)
        self.rot_deg_ds.setSuffix(" °")
        self.rot_deg_ds.valueChanged.connect(self._update_plot)
        form.addRow("Float rotate Z", self.rot_deg_ds)

        # Step sizes for keyboard nudges
        self.step_xy_ds = QtWidgets.QDoubleSpinBox()
        self.step_xy_ds.setRange(0.0001, 1e6)
        self.step_xy_ds.setDecimals(4)
        self.step_xy_ds.setSingleStep(0.1)
        self.step_xy_ds.setValue(0.1)
        form.addRow("Shift step (keys)", self.step_xy_ds)

        self.step_rot_ds = QtWidgets.QDoubleSpinBox()
        self.step_rot_ds.setRange(0.001, 360.0)
        self.step_rot_ds.setDecimals(3)
        self.step_rot_ds.setSingleStep(1.0)
        self.step_rot_ds.setValue(1.0)
        self.step_rot_ds.setSuffix(" °")
        form.addRow("Rotate step (keys)", self.step_rot_ds)

        save_btn = QtWidgets.QPushButton("Save transformed float…")
        save_btn.clicked.connect(self._save_transformed_float)
        form.addRow(save_btn)

        left_box = QtWidgets.QVBoxLayout()
        left_box.addLayout(form)
        left_box.addStretch(1)

        # Shortcut hint
        hint = QtWidgets.QLabel("Keys: arrows to shift float (X/Y), [ ] to rotate Z, 0 to reset.")
        hint.setStyleSheet("color: #666;")
        left_box.addWidget(hint)

        # Right plot area
        self.fig = Figure(figsize=(6, 5))
        self.ax = self.fig.add_subplot(111, projection="3d")
        self.canvas = FigureCanvas(self.fig)
        self.canvas.setMinimumSize(420, 320)
        self.toolbar = NavigationToolbar(self.canvas, self)
        self._mappable = cm.ScalarMappable(norm=colors.Normalize(0.0, 1.0), cmap=cm.viridis)
        self._colorbar = self.fig.colorbar(
            self._mappable, ax=self.ax, orientation="horizontal", fraction=0.05, pad=0.12
        )
        self._colorbar.set_label("Z value")

        # Default to a 3D view (can still rotate with mouse/toolbar)
        try:
            self.ax.view_init(elev=25, azim=-60)
            self.ax.set_proj_type("ortho")  # parallel projection
        except Exception:
            pass

        # Quick projection and clear buttons
        view_btns = QtWidgets.QHBoxLayout()
        for label in ["View X", "View Y", "View Z"]:
            b = QtWidgets.QPushButton(label)
            if label.endswith("X"):
                b.clicked.connect(lambda _, axis="X": self._set_view(axis))
            elif label.endswith("Y"):
                b.clicked.connect(lambda _, axis="Y": self._set_view(axis))
            else:
                b.clicked.connect(lambda _, axis="Z": self._set_view(axis))
            view_btns.addWidget(b)
        clear_btn = QtWidgets.QPushButton("Clear plot")
        clear_btn.clicked.connect(self._clear_plot)
        view_btns.addWidget(clear_btn)

        right = QtWidgets.QVBoxLayout()
        right.addLayout(view_btns)
        right.addWidget(self.toolbar)
        right.addWidget(self.canvas, 1)

        layout.addLayout(left_box, 0)
        layout.addLayout(right, 1)

        self._setup_shortcuts()
        self._update_plot()

    def _setup_shortcuts(self):
        # Translate with arrows; rotate with [ and ]; reset with 0
        def _nudge(spin, delta):
            spin.setValue(spin.value() + delta)

        shortcuts = [
            (QtGui.QKeySequence(QtCore.Qt.Key_Left),  lambda: _nudge(self.shift_x_ds, -float(self.step_xy_ds.value()))),
            (QtGui.QKeySequence(QtCore.Qt.Key_Right), lambda: _nudge(self.shift_x_ds,  float(self.step_xy_ds.value()))),
            (QtGui.QKeySequence(QtCore.Qt.Key_Up),    lambda: _nudge(self.shift_y_ds,  float(self.step_xy_ds.value()))),
            (QtGui.QKeySequence(QtCore.Qt.Key_Down),  lambda: _nudge(self.shift_y_ds, -float(self.step_xy_ds.value()))),
            (QtGui.QKeySequence("["),                 lambda: _nudge(self.rot_deg_ds, -float(self.step_rot_ds.value()))),
            (QtGui.QKeySequence("]"),                 lambda: _nudge(self.rot_deg_ds,  float(self.step_rot_ds.value()))),
            (QtGui.QKeySequence("0"),                 self._reset_transform),
        ]
        for seq, fn in shortcuts:
            sc = QtWidgets.QShortcut(seq, self)
            sc.activated.connect(fn)

    def _reset_transform(self):
        self.shift_x_ds.setValue(0.0)
        self.shift_y_ds.setValue(0.0)
        self.rot_deg_ds.setValue(0.0)
        self.scale_x_ds.setValue(1.0)
        self.scale_y_ds.setValue(1.0)

    def _set_view(self, axis: str):
        axis = axis.upper()
        views = {
            "X": (0.0, 0.0),    # look along +X
            "Y": (0.0, 90.0),   # along +Y
            "Z": (90.0, -90.0)  # top-down along +Z
        }
        elev, azim = views.get(axis, (25.0, -60.0))
        try:
            self.ax.view_init(elev=elev, azim=azim)
            self.ax.set_proj_type("ortho")
            self.canvas.draw_idle()
        except Exception:
            pass

    def _clear_plot(self):
        self.ref_points = None
        self.float_points = None
        self.ref_le.clear()
        self.float_le.clear()
        self._update_plot()

    def _read_xyz(self, path: str) -> np.ndarray:
        # Accept .txt or .csv; skip a header line if present
        first = Path(path).read_text(encoding="utf-8", errors="ignore").splitlines()
        first_line = first[0] if first else ""
        has_header = False
        delim = "," if "," in first_line else None
        try:
            parts = [float(x) for x in first_line.strip().split(delim)]
            if len(parts) != 3:
                has_header = True
        except Exception:
            has_header = True

        arr = np.loadtxt(path, delimiter=delim, skiprows=1 if has_header else 0, usecols=[0, 1, 2])
        if arr.ndim == 1:
            arr = arr.reshape(1, -1)
        if arr.shape[1] != 3:
            raise ValueError("Expected 3 columns (x y z).")
        return arr.astype(float)

    def _load_file(self, is_ref: bool):
        path, _ = QtWidgets.QFileDialog.getOpenFileName(
            self, "Select data file", "", "Data files (*.txt *.csv);;All files (*)"
        )
        if not path:
            return
        try:
            pts = self._read_xyz(path)
        except Exception as e:
            QtWidgets.QMessageBox.critical(self, "Invalid file", str(e))
            return

        if is_ref:
            self.ref_points = pts
            self.ref_le.setText(path)
            which = "reference"
        else:
            self.float_points = pts
            self.float_le.setText(path)
            which = "float"

        self.status_lbl.setText(f"Loaded {pts.shape[0]} points from {which} file.")
        self._update_plot()

    def _transform_float_points(self):
        if self.float_points is None:
            return None
        # Rotate around Z, then translate in X/Y
        theta = np.deg2rad(float(self.rot_deg_ds.value()))
        c, s = np.cos(theta), np.sin(theta)
        rot = np.array([[c, -s], [s, c]], dtype=float)
        scale_vec = np.array([float(self.scale_x_ds.value()), float(self.scale_y_ds.value())], dtype=float)
        xy = (self.float_points[:, :2] * scale_vec) @ rot.T
        xy[:, 0] += float(self.shift_x_ds.value())
        xy[:, 1] += float(self.shift_y_ds.value())
        out = self.float_points.copy()
        out[:, 0:2] = xy
        return out

    def _save_transformed_float(self):
        tf = self._transform_float_points()
        if tf is None:
            QtWidgets.QMessageBox.information(self, "No float data", "Load a float file first.")
            return
        path, _ = QtWidgets.QFileDialog.getSaveFileName(
            self,
            "Save transformed float",
            "",
            "CSV files (*.csv);;Text files (*.txt);;All files (*)",
        )
        if not path:
            return
        try:
            if path.lower().endswith(".csv"):
                np.savetxt(path, tf, fmt="%.6f", delimiter=",")
            else:
                np.savetxt(path, tf, fmt="%.6f")
            QtWidgets.QMessageBox.information(self, "Saved", f"Transformed float points saved to:\n{path}")
        except Exception as e:
            QtWidgets.QMessageBox.critical(self, "Save failed", str(e))

    def _set_equal_aspect(self):
        pts = []
        if self.ref_points is not None:
            pts.append(self.ref_points)
        tf = self._transform_float_points()
        if tf is not None:
            pts.append(tf)
        if not pts:
            return
        data = np.vstack(pts)
        mins = data.min(axis=0)
        maxs = data.max(axis=0)
        span = np.maximum(maxs - mins, 1e-9)
        # pad each axis a bit to avoid degenerate ranges without forcing a cube
        pad = np.maximum(span * 0.05, 1e-3)
        lower = mins - pad
        upper = maxs + pad
        self.ax.set_xlim(lower[0], upper[0])
        self.ax.set_ylim(lower[1], upper[1])
        self.ax.set_zlim(lower[2], upper[2])

    def _update_plot(self):
        self.ax.clear()
        tf = self._transform_float_points()

        # Build a shared color scale based on Z
        z_arrays = []
        if self.ref_points is not None:
            z_arrays.append(self.ref_points[:, 2])
        if tf is not None:
            z_arrays.append(tf[:, 2])

        norm = None
        use_cmap = bool(self.colorbar_chk.isChecked())
        if use_cmap and z_arrays:
            zcat = np.hstack(z_arrays)
            z_min, z_max = float(np.min(zcat)), float(np.max(zcat))
            if z_max == z_min:
                z_max = z_min + 1.0
            norm = colors.Normalize(vmin=z_min, vmax=z_max)

        handles = []
        if self.ref_points is not None:
            h = self.ax.scatter(
                self.ref_points[:, 0],
                self.ref_points[:, 1],
                self.ref_points[:, 2],
                s=float(self.ref_size_sp.value()),
                c=self.ref_points[:, 2] if norm is not None else "#1f77b4",
                cmap=cm.viridis if norm is not None else None,
                norm=norm,
                marker="o",
                label="Reference",
                alpha=0.85,
            )
            handles.append(h)
        if tf is not None:
            h = self.ax.scatter(
                tf[:, 0],
                tf[:, 1],
                tf[:, 2],
                s=float(self.float_size_sp.value()),
                c=tf[:, 2] if norm is not None else "#ff7f0e",
                cmap=cm.viridis if norm is not None else None,
                norm=norm,
                marker="^",
                label="Float",
                alpha=0.85,
            )
            handles.append(h)

        self.ax.set_xlabel("X")
        self.ax.set_ylabel("Y")
        self.ax.set_zlabel("Z")

        if handles:
            self.ax.legend(loc="upper right")
            self._set_equal_aspect()
        else:
            self.ax.text2D(
                0.5,
                0.5,
                "Load reference and float files to view points",
                transform=self.ax.transAxes,
                ha="center",
                va="center",
                fontsize=10,
                color="#666666",
            )

        if norm is not None:
            self._mappable.set_norm(norm)
            self._colorbar.ax.set_visible(True)
            self._colorbar.update_normal(self._mappable)
            self._colorbar.set_label("Z value")
        else:
            self._colorbar.ax.set_visible(False)

        self.canvas.draw_idle()
# --------------------------- Batch Submit Tab ---------------------------
class BatchSubmitTab(QtWidgets.QWidget, LaunchMixin):
    def __init__(self, settings):
        super().__init__()
        self.settings = settings

        form = QtWidgets.QFormLayout()

        self.bat_le = QtWidgets.QLineEdit()
        bat_btn = QtWidgets.QPushButton("…"); bat_btn.clicked.connect(self._pick_bat)
        hl_bat = QtWidgets.QHBoxLayout(); hl_bat.addWidget(self.bat_le); hl_bat.addWidget(bat_btn)
        form.addRow("Batch file (.bat)", hl_bat)

        self.run_btn = QtWidgets.QPushButton("Submit Jobs →"); self.run_btn.clicked.connect(self._run_bat)
        self.stop_btn = QtWidgets.QPushButton("Stop"); self.stop_btn.setEnabled(False); self.stop_btn.clicked.connect(self._stop_bat)
        hb = QtWidgets.QHBoxLayout(); hb.addWidget(self.run_btn); hb.addWidget(self.stop_btn)

        self.log = QtWidgets.QPlainTextEdit(); self.log.setReadOnly(True)

        layout = QtWidgets.QVBoxLayout(self)
        layout.addLayout(form)
        layout.addLayout(hb)
        layout.addWidget(self.log, 1)

    def _pick_bat(self):
        f, _ = QtWidgets.QFileDialog.getOpenFileName(self, "Select batch file", "", "Batch files (*.bat);;All files (*)")
        if f: self.bat_le.setText(f)

    def _run_bat(self):
        bat = self.bat_le.text().strip()
        if not bat:
            QtWidgets.QMessageBox.critical(self, "No file", "请先选择 .bat 文件。"); return
        if not Path(bat).exists():
            QtWidgets.QMessageBox.critical(self, "Not found", f"文件不存在：\n{bat}"); return

        ifortvars = r"C:\Program Files (x86)\IntelSWTools\compilers_and_libraries_2020.1.216\windows\bin\ifortvars.bat"
        wrapper = Path(bat).parent / "run_with_intel_env.bat"
        with open(wrapper, "w", encoding="utf-8") as f:
            f.write("@echo off\n")
            f.write(f'call "{ifortvars}" intel64 vs2019\n')
            f.write(f'call "{bat}"\n')
            f.write("pause\n")

        cmd = ["cmd", "/c", str(wrapper)]
        self._launch(cmd, Path(bat).parent, self.log, self.run_btn, stop_button=self.stop_btn)

    def _stop_bat(self):
        # immediately re-enable Run, disable Stop; kill process tree
        self.run_btn.setEnabled(True); self.stop_btn.setEnabled(False)
        if hasattr(self, "_worker") and self._worker.isRunning():
            self.log.appendPlainText("[GUI] 正在终止批处理进程树 ...")
            self._worker.stop(kill_tree=True)
        else:
            self.log.appendPlainText("[GUI] 未检测到正在运行的批处理进程。")

        # Also terminate jobs referenced in .bat (best-effort)
        bat_path = self.bat_le.text().strip()
        if not bat_path or not Path(bat_path).exists():
            self.log.appendPlainText("[GUI] 未找到 .bat 文件，跳过作业终止。"); return

        jobnames = []
        pat = re.compile(r"job=([^\s]+)")
        for line in Path(bat_path).read_text(encoding="utf-8", errors="ignore").splitlines():
            m = pat.search(line)
            if m: jobnames.append(m.group(1))

        if not jobnames:
            self.log.appendPlainText(">>> 在 .bat 中未发现 job=...，无需终止 <<<"); return

        for job in jobnames:
            term_cmd = [self.settings.get("abaqus_cmd", DEFAULT_ABAQUS_CMD), f"job={job}", "-terminate"]
            self.log.appendPlainText(f">>> Terminating job {job} ...")
            try:
                proc = subprocess.Popen(term_cmd, cwd=Path(bat_path).parent,
                                        stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                                        text=True, encoding="utf-8", errors="ignore")
                out, _ = proc.communicate(timeout=30)
                if out: self.log.appendPlainText(out.strip())
            except Exception as e:
                self.log.appendPlainText(f"Error terminating {job}: {e}")


# --------------------------- Machine Learning (GBM) Tab ---------------------------
class MachineLearningTab(QtWidgets.QWidget, LaunchMixin):
    """
    Train two GradientBoostingRegressor models (one per target column) with Optuna,
    save ONE combined artifact (models + scaling + metadata), and run predictions
    from that single artifact. The Predict pane shows best hyperparameters.
    """

    # ---------- Training template (one-file artifact) ----------
    TRAIN_TEMPLATE = u"""# -*- coding: utf-8 -*-
from __future__ import print_function
import os, sys, csv, pickle
import numpy as np
import pandas as pd
import joblib
import optuna
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.metrics import r2_score
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.exceptions import ConvergenceWarning
import warnings
warnings.filterwarnings("ignore", category=ConvergenceWarning)

# Non-interactive backend for saving PNGs
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

# ====== GUI-injected constants ======
DATA_FILE     = r"__DATA_FILE__"
OUTPUT_DIR    = r"__OUTPUT_DIR__"
ARTIFACT_NAME = r"__ARTIFACT_NAME__"
N_TRIALS      = __N_TRIALS__
TEST_SIZE     = __TEST_SIZE__
RAND_STATE    = __RAND_STATE__
LR_LOW        = __LR_LOW__
LR_HIGH       = __LR_HIGH__
# ====================================

def _ensure_out(d):
    if not os.path.isdir(d):
        os.makedirs(d)

def main():
    print("[TRAIN] Loading:", DATA_FILE)
    _ensure_out(OUTPUT_DIR)

    # Load CSV (no header). y = first two cols, X = from 3rd col
    data = pd.read_csv(DATA_FILE, header=None)
    X = data.iloc[:, 2:].values
    y = data.iloc[:, :2].values

    # Global min/max scaling
    global_min = float(X.min()) - 100.0
    global_max = float(X.max()) + 100.0
    denom = global_max - global_min
    if abs(denom) < 1e-12:
        print("[WARN] global_max == global_min; using denom=1")
        denom = 1.0
    X_scaled = (X - global_min) / denom

    # Split
    X_train, X_val, y_train, y_val = train_test_split(
        X_scaled, y, test_size=TEST_SIZE, random_state=RAND_STATE
    )

    print("[TRAIN] Shapes:", X_train.shape, y_train.shape, "|", X_val.shape, y_val.shape)

    # Optuna objective
    def objective(trial):
        n_estimators = trial.suggest_int('n_estimators', 100, 2000)
        max_depth    = trial.suggest_int('max_depth', 3, 20)
        learning_rate= trial.suggest_float('learning_rate', LR_LOW, LR_HIGH, log=True)
        subsample    = trial.suggest_float('subsample', 0.5, 1.0)
        min_split    = trial.suggest_int('min_samples_split', 2, 15)
        min_leaf     = trial.suggest_int('min_samples_leaf', 1, 5)
        max_features = trial.suggest_categorical('max_features', [1.0, 'sqrt', 'log2'])

        model_1 = GradientBoostingRegressor(
            n_estimators=n_estimators, max_depth=max_depth, learning_rate=learning_rate,
            subsample=subsample, min_samples_split=min_split, min_samples_leaf=min_leaf,
            max_features=max_features, random_state=RAND_STATE
        )
        model_2 = GradientBoostingRegressor(
            n_estimators=n_estimators, max_depth=max_depth, learning_rate=learning_rate,
            subsample=subsample, min_samples_split=min_split, min_samples_leaf=min_leaf,
            max_features=max_features, random_state=RAND_STATE
        )

        s1 = cross_val_score(model_1, X_train, y_train[:,0], cv=5, scoring='r2')
        s2 = cross_val_score(model_2, X_train, y_train[:,1], cv=5, scoring='r2')
        avg = (float(np.mean(s1)) + float(np.mean(s2))) / 2.0
        return -avg  # minimize

    print("[TRAIN] Optuna trials:", N_TRIALS)
    study = optuna.create_study(direction='minimize')
    study.optimize(objective, n_trials=N_TRIALS, n_jobs=-1)
    best = study.best_trial
    print("[TRAIN] Best params:", best.params)

    # Build & fit best models
    best_gb_1 = GradientBoostingRegressor(random_state=RAND_STATE, **best.params)
    best_gb_2 = GradientBoostingRegressor(random_state=RAND_STATE, **best.params)

    # Learning curve
    r2_1, r2_2, nlist = [], [], []
    maxN = int(best.params['n_estimators'])
    step = 10 if maxN >= 20 else 1
    for n in range(step, maxN+1, step):
        best_gb_1.set_params(n_estimators=n); best_gb_2.set_params(n_estimators=n)
        best_gb_1.fit(X_train, y_train[:,0]); best_gb_2.fit(X_train, y_train[:,1])
        p1 = best_gb_1.predict(X_val); p2 = best_gb_2.predict(X_val)
        r2_1.append(r2_score(y_val[:,0], p1)); r2_2.append(r2_score(y_val[:,1], p2)); nlist.append(n)

    # Final eval
    best_gb_1.set_params(n_estimators=maxN); best_gb_2.set_params(n_estimators=maxN)
    best_gb_1.fit(X_train, y_train[:,0]); best_gb_2.fit(X_train, y_train[:,1])
    pv1 = best_gb_1.predict(X_val); pv2 = best_gb_2.predict(X_val)
    r21 = r2_score(y_val[:,0], pv1); r22 = r2_score(y_val[:,1], pv2)
    r2avg = (r21 + r22) / 2.0
    print("[TRAIN] Final R2 target1=%.4f target2=%.4f avg=%.4f" % (r21, r22, r2avg))

    # Plots
    plt.figure(figsize=(7,5))
    plt.plot(nlist, r2_1, label='Target 1'); plt.plot(nlist, r2_2, label='Target 2')
    plt.xlabel('n_estimators'); plt.ylabel('R2'); plt.title('GBM: R2 vs n_estimators')
    plt.grid(True); plt.legend(); plt.tight_layout()
    plt.savefig(os.path.join(OUTPUT_DIR, 'gbm_r2_vs_estimators.png'), dpi=160); plt.close()

    for (ytrue, ypred, name) in [(y_val[:,0], pv1, 'target1'), (y_val[:,1], pv2, 'target2')]:
        plt.figure(figsize=(6,5))
        lo, hi = float(np.min(ytrue)), float(np.max(ytrue))
        plt.scatter(ytrue, ypred, alpha=0.6)
        plt.plot([lo,hi],[lo,hi], 'k--', lw=1)
        plt.xlabel('Actual'); plt.ylabel('Predicted'); plt.title('GBM Parity: %s' % name)
        plt.grid(True); plt.tight_layout()
        plt.savefig(os.path.join(OUTPUT_DIR, 'gbm_parity_%s.png' % name), dpi=160); plt.close()

    # Build artifact dict
    feat_imp_1 = getattr(best_gb_1, 'feature_importances_', None)
    feat_imp_2 = getattr(best_gb_2, 'feature_importances_', None)
    artifact = {
        'model1': best_gb_1,
        'model2': best_gb_2,
        'scaling': {'global_min': global_min, 'global_max': global_max},
        'best_params': dict(best.params),
        'val_r2': {'target1': float(r21), 'target2': float(r22), 'avg': float(r2avg)},
        'n_features': int(X.shape[1]),
        'feature_importances': {
            'target1': feat_imp_1.tolist() if feat_imp_1 is not None else None,
            'target2': feat_imp_2.tolist() if feat_imp_2 is not None else None
        }
    }

    # Save ONE artifact file
    artifact_path = os.path.join(OUTPUT_DIR, ARTIFACT_NAME)
    joblib.dump(artifact, artifact_path)
    print("[TRAIN] Artifact saved:", artifact_path)

if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        print("[ERROR]", e)
        raise
"""

    # ---------- Prediction template (load one artifact) ----------
    PRED_TEMPLATE = u"""# -*- coding: utf-8 -*-
from __future__ import print_function
import os, sys
import numpy as np
import pandas as pd
import joblib

# ====== GUI-injected constants ======
ARTIFACT  = r"__ARTIFACT__"
DATA_FILE = r"__DATA_FILE__"
OUT_CSV   = r"__OUT_CSV__"
HEADER_FLAG = __HEADER_FLAG__
# ====================================

def main():
    print("[PRED] Loading artifact:", ARTIFACT)
    art = joblib.load(ARTIFACT)
    m1 = art['model1']; m2 = art['model2']
    sc = art.get('scaling', {})
    gmin = float(sc.get('global_min', 0.0)); gmax = float(sc.get('global_max', 1.0))
    denom = gmax - gmin
    if abs(denom) < 1e-12:
        print("[WARN] global_max == global_min; using denom=1")
        denom = 1.0

    print("[PRED] Loading data:", DATA_FILE)
    df = pd.read_csv(DATA_FILE, header=None)
    if df.shape[1] < 3:
        raise ValueError("CSV must have at least 3 columns (two leading columns + features)")
    X = df.iloc[:, 2:].values

    # quick sanity vs. n_features
    nf = art.get('n_features', None)
    if nf is not None and X.shape[1] != int(nf):
        raise ValueError("Feature count mismatch: measured has %d, artifact expects %d" % (X.shape[1], nf))

    Xs = (X - gmin) / denom

    print("[PRED] Predicting...")
    y1 = m1.predict(Xs).reshape(-1, 1)
    y2 = m2.predict(Xs).reshape(-1, 1)
    Y  = np.hstack([y1, y2])

    out = pd.DataFrame(Y, columns=["target1_pred", "target2_pred"])
    out.to_csv(OUT_CSV, index=False, header=bool(HEADER_FLAG))
    print("[PRED] Saved:", OUT_CSV)

if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        print("[ERROR]", e)
        raise
"""

    def __init__(self, settings):
        super().__init__()
        self.settings = settings
        self._tmpdir = None
        self._setup_ui()

    # ---- UI ----
    def _setup_ui(self):
        layout = QtWidgets.QVBoxLayout(self)

        tabs = QtWidgets.QTabWidget()
        layout.addWidget(tabs, 1)

        # --- Train pane ---
        train = QtWidgets.QWidget(); tabs.addTab(train, "Train GBM")
        form_t = QtWidgets.QFormLayout(train)

        self.train_data_le = QtWidgets.QLineEdit()
        bt_td = QtWidgets.QPushButton("…"); bt_td.clicked.connect(lambda: self._pick_file(self.train_data_le, "CSV files (*.csv)"))
        h_td = QtWidgets.QHBoxLayout(); h_td.addWidget(self.train_data_le); h_td.addWidget(bt_td)
        form_t.addRow("Training CSV", h_td)

        self.train_outdir_le = QtWidgets.QLineEdit(self.settings.get("default_save_dir", str(SCRIPT_DIR)))
        bt_to = QtWidgets.QPushButton("…"); bt_to.clicked.connect(lambda: self._pick_dir(self.train_outdir_le))
        h_to = QtWidgets.QHBoxLayout(); h_to.addWidget(self.train_outdir_le); h_to.addWidget(bt_to)
        form_t.addRow("Output dir", h_to)

        self.artifact_name_le = QtWidgets.QLineEdit("gbm_artifact.pkl")
        form_t.addRow("Artifact filename", self.artifact_name_le)

        self.n_trials_sp = QtWidgets.QSpinBox(); self.n_trials_sp.setRange(1, 5000); self.n_trials_sp.setValue(50)
        form_t.addRow("Optuna trials", self.n_trials_sp)

        self.test_size_ds = QtWidgets.QDoubleSpinBox(); self.test_size_ds.setRange(0.05, 0.95); self.test_size_ds.setSingleStep(0.05); self.test_size_ds.setValue(0.2)
        form_t.addRow("Validation fraction", self.test_size_ds)

        self.lr_low_ds  = QtWidgets.QDoubleSpinBox(); self.lr_low_ds.setDecimals(4); self.lr_low_ds.setRange(1e-4, 1.0); self.lr_low_ds.setValue(0.01)
        self.lr_high_ds = QtWidgets.QDoubleSpinBox(); self.lr_high_ds.setDecimals(3); self.lr_high_ds.setRange(1e-4, 1.0); self.lr_high_ds.setValue(0.1)
        h_lr = QtWidgets.QHBoxLayout(); h_lr.addWidget(self.lr_low_ds); h_lr.addWidget(QtWidgets.QLabel("to")); h_lr.addWidget(self.lr_high_ds)
        form_t.addRow("Learning rate range (log)", h_lr)

        self.train_run_btn = QtWidgets.QPushButton("Train GB Models →"); self.train_run_btn.clicked.connect(self._run_train)
        self.train_stop_btn = QtWidgets.QPushButton("Stop"); self.train_stop_btn.setEnabled(False); self.train_stop_btn.clicked.connect(self._stop_train)
        h_tr = QtWidgets.QHBoxLayout(); h_tr.addWidget(self.train_run_btn); h_tr.addWidget(self.train_stop_btn)
        form_t.addRow(h_tr)

        self.train_log = QtWidgets.QPlainTextEdit(); self.train_log.setReadOnly(True)
        form_t.addRow(self.train_log)

        # --- Predict pane ---
        pred = QtWidgets.QWidget(); tabs.addTab(pred, "Predict")
        form_p = QtWidgets.QFormLayout(pred)

        self.artifact_le = QtWidgets.QLineEdit()
        b_art = QtWidgets.QPushButton("…"); b_art.clicked.connect(lambda: self._pick_file(self.artifact_le, "Pickle files (*.pkl)"))
        h_art = QtWidgets.QHBoxLayout(); h_art.addWidget(self.artifact_le); h_art.addWidget(b_art)
        form_p.addRow("Artifact (.pkl)", h_art)

        # Auto-show best params & metrics when artifact is chosen
        self.artifact_info = QtWidgets.QPlainTextEdit(); self.artifact_info.setReadOnly(True); self.artifact_info.setMaximumHeight(160)
        form_p.addRow("Best model summary", self.artifact_info)
        self.artifact_le.textChanged.connect(self._preview_artifact)

        self.pred_csv_le = QtWidgets.QLineEdit(); b_pc = QtWidgets.QPushButton("…"); b_pc.clicked.connect(lambda: self._pick_file(self.pred_csv_le, "CSV files (*.csv)"))
        h_pc = QtWidgets.QHBoxLayout(); h_pc.addWidget(self.pred_csv_le); h_pc.addWidget(b_pc)
        form_p.addRow("Measured CSV", h_pc)

        self.pred_out_le = QtWidgets.QLineEdit()
        b_po = QtWidgets.QPushButton("…"); b_po.clicked.connect(lambda: self._pick_save_csv(self.pred_out_le))
        h_po = QtWidgets.QHBoxLayout(); h_po.addWidget(self.pred_out_le); h_po.addWidget(b_po)
        form_p.addRow("Save predictions to", h_po)

        self.header_chk = QtWidgets.QCheckBox("Write header"); self.header_chk.setChecked(True)
        form_p.addRow("", self.header_chk)

        self.pred_run_btn = QtWidgets.QPushButton("Predict →"); self.pred_run_btn.clicked.connect(self._run_predict)
        self.pred_stop_btn = QtWidgets.QPushButton("Stop"); self.pred_stop_btn.setEnabled(False); self.pred_stop_btn.clicked.connect(self._stop_predict)
        h_pr = QtWidgets.QHBoxLayout(); h_pr.addWidget(self.pred_run_btn); h_pr.addWidget(self.pred_stop_btn)
        form_p.addRow(h_pr)

        self.pred_log = QtWidgets.QPlainTextEdit(); self.pred_log.setReadOnly(True)
        form_p.addRow(self.pred_log)

    # ---- helpers ----
    def _pick_file(self, line, pattern_desc):
        f, _ = QtWidgets.QFileDialog.getOpenFileName(self, "Select file", "", pattern_desc + ";;All files (*)")
        if f: line.setText(f)

    def _pick_dir(self, line):
        d = QtWidgets.QFileDialog.getExistingDirectory(self, "Select directory")
        if d: line.setText(d)

    def _pick_save_csv(self, line):
        f, _ = QtWidgets.QFileDialog.getSaveFileName(self, "Save CSV as", "", "CSV files (*.csv)")
        if f: line.setText(f)

    def _stop_train(self):
        if hasattr(self, "_worker_train") and self._worker_train.isRunning():
            self.train_log.appendPlainText("[GUI] Stopping training ...")
            self.train_run_btn.setEnabled(True); self.train_stop_btn.setEnabled(False)
            self._worker_train.stop(kill_tree=True)

    def _stop_predict(self):
        if hasattr(self, "_worker_pred") and self._worker_pred.isRunning():
            self.pred_log.appendPlainText("[GUI] Stopping prediction ...")
            self.pred_run_btn.setEnabled(True); self.pred_stop_btn.setEnabled(False)
            self._worker_pred.stop(kill_tree=True)

    def _write_script(self, template_text, replacements, work_dir, filename):
        # normalize coding cookie to utf-8
        txt = template_text
        txt = re.sub(r'^\s*#\s*-\*-\s*coding\s*:\s*.*?-\*-\s*$', '# -*- coding: utf-8 -*-', txt, flags=re.M)
        for pat, rep in replacements.items():
            txt = re.sub(pat, lambda m, rep=rep: rep, txt, flags=re.M)
        run_py = Path(work_dir) / filename
        run_py.write_text(txt, encoding="utf-8")
        return str(run_py)

    def _preview_artifact(self):
        path = self.artifact_le.text().strip()
        if not path or not Path(path).exists():
            self.artifact_info.setPlainText("")
            return
        try:
            art = joblib.load(path)
            bp = art.get('best_params', {})
            r2 = art.get('val_r2', {})
            nf = art.get('n_features', None)
            lines = []
            lines.append("Best hyperparameters:")
            for k in sorted(bp.keys()):
                lines.append("  {}: {}".format(k, bp[k]))
            lines.append("")
            lines.append("Validation R²:")
            lines.append("  target1: {}".format(r2.get('target1', 'n/a')))
            lines.append("  target2: {}".format(r2.get('target2', 'n/a')))
            lines.append("  average: {}".format(r2.get('avg', 'n/a')))
            if nf is not None:
                lines.append("")
                lines.append("Expected #features: {}".format(nf))
            self.artifact_info.setPlainText("\n".join(lines))
        except Exception as e:
            self.artifact_info.setPlainText("Failed to read artifact:\n{}".format(e))

    # ---- actions ----
    def _run_train(self):
        data = self.train_data_le.text().strip()
        outd = self.train_outdir_le.text().strip()
        aname = self.artifact_name_le.text().strip() or "gbm_artifact.pkl"
        if not data:
            QtWidgets.QMessageBox.warning(self, "Missing", "Select a training CSV.")
            return
        if not outd:
            QtWidgets.QMessageBox.warning(self, "Missing", "Select an output directory.")
            return

        if not self._tmpdir:
            self._tmpdir = tempfile.TemporaryDirectory()

        reps = {
            r'__DATA_FILE__'     : Path(data).expanduser().resolve().as_posix(),
            r'__OUTPUT_DIR__'    : Path(outd).expanduser().resolve().as_posix(),
            r'__ARTIFACT_NAME__' : aname,
            r'__N_TRIALS__'      : str(int(self.n_trials_sp.value())),
            r'__TEST_SIZE__'     : str(float(self.test_size_ds.value())),
            r'__RAND_STATE__'    : "42",
            r'__LR_LOW__'        : str(float(self.lr_low_ds.value())),
            r'__LR_HIGH__'       : str(float(self.lr_high_ds.value())),
        }
        run_py = self._write_script(self.TRAIN_TEMPLATE, reps, self._tmpdir.name, "gb_train_run.py")

        cmd = [self.settings.get("python_cmd", sys.executable), run_py]
        self.train_log.clear()
        self._worker_train = Worker(cmd, cwd=Path(outd))
        self._worker_train.output.connect(self.train_log.appendPlainText)
        def _finish(code):
            self.train_run_btn.setEnabled(True); self.train_stop_btn.setEnabled(False)
            self.train_log.appendPlainText("\n=== training finished (exit %s) ===" % code)
        self.train_run_btn.setEnabled(False); self.train_stop_btn.setEnabled(True)
        self._worker_train.finished.connect(_finish)
        self._worker_train.start()

    def _run_predict(self):
        art = self.artifact_le.text().strip()
        df  = self.pred_csv_le.text().strip()
        out = self.pred_out_le.text().strip()

        if not (art and df):
            QtWidgets.QMessageBox.warning(self, "Missing", "Select an artifact (.pkl) and a measured CSV.")
            return
        if not out:
            out = str(Path(df).with_suffix("")) + "__gb_predictions.csv"
            self.pred_out_le.setText(out)

        if not self._tmpdir:
            self._tmpdir = tempfile.TemporaryDirectory()

        reps = {
            r'__ARTIFACT__'  : Path(art).expanduser().resolve().as_posix(),
            r'__DATA_FILE__' : Path(df).expanduser().resolve().as_posix(),
            r'__OUT_CSV__'   : Path(out).expanduser().resolve().as_posix(),
            r'__HEADER_FLAG__': "1" if self.header_chk.isChecked() else "0",
        }
        run_py = self._write_script(self.PRED_TEMPLATE, reps, self._tmpdir.name, "gb_pred_run.py")

        cmd = [self.settings.get("python_cmd", sys.executable), run_py]
        self.pred_log.clear()
        cwd = Path(out).expanduser().resolve().parent
        self._worker_pred = Worker(cmd, cwd=cwd)
        self._worker_pred.output.connect(self.pred_log.appendPlainText)
        def _finish(code):
            self.pred_run_btn.setEnabled(True); self.pred_stop_btn.setEnabled(False)
            self.pred_log.appendPlainText("\n=== prediction finished (exit %s) ===" % code)
        self.pred_run_btn.setEnabled(False); self.pred_stop_btn.setEnabled(True)
        self._worker_pred.finished.connect(_finish)
        self._worker_pred.start()


# --------------------------- Settings & Main ---------------------------
class MainWindow(QtWidgets.QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("AM Simulation GUI")
        self.resize(1040, 720)

        self._settings_path = SCRIPT_DIR / "am_gui_settings.json"
        self.settings = self._load_settings()

        tabs = QtWidgets.QTabWidget(); self.setCentralWidget(tabs)
        tabs.addTab(BuildModelTab(self.settings), "Build Model")
        tabs.addTab(InputAndUtempTab(self.settings), "Input & UTEMP")
        tabs.addTab(DataExtractTab(self.settings), "Data Extract")  # NEW
        tabs.addTab(DataAlignmentTab(self.settings), "Data alignment")
        tabs.addTab(BatchSubmitTab(self.settings), "Submit Jobs")
        tabs.addTab(MachineLearningTab(self.settings), "ML (GBM)")


        tb = self.addToolBar("Tools")
        act = QtWidgets.QAction("Settings", self); act.triggered.connect(self._edit_settings)
        tb.addAction(act)

    def _load_settings(self):
        if self._settings_path.exists():
            s = json.loads(self._settings_path.read_text("utf-8"))
            # back-compat defaults
            s.setdefault("abaqus_cmd", DEFAULT_ABAQUS_CMD)
            s.setdefault("build_script", str(DEFAULT_BUILD_SCRIPT))
            s.setdefault("input_script", str(DEFAULT_INPUT_SCRIPT))
            s.setdefault("import_script", str(DEFAULT_IMPORT_SCRIPT))
            s.setdefault("apply_materials_script", str(DEFAULT_APPLY_MAT_SCRIPT))
            s.setdefault("apply_meshing_script", str(DEFAULT_MESH_SCRIPT))
            s.setdefault("apply_boundary_script", str(DEFAULT_APPLY_BC_SCRIPT))  # NEW
            s.setdefault("default_save_dir", str(SCRIPT_DIR))
            s.setdefault("base_xlsx", "")
            s.setdefault("build_xlsx", "")
            s.setdefault("build_axis", "Y")
            s.setdefault("axis_zero", 0.0)
            # inside the if self._settings_path.exists(): block (back-compat)
            s.setdefault("ht_build_enabled", False)
            s.setdefault("ht_input_enabled", False)
            s.setdefault("ht_temp_c", 650.0)
            
            # Back-compat: if old 'ht_enabled' exists, seed both (one-time effect)
            if "ht_enabled" in s:
                s["ht_build_enabled"] = bool(s.get("ht_build_enabled", s["ht_enabled"]))
                s["ht_input_enabled"] = bool(s.get("ht_input_enabled", s["ht_enabled"]))



            return s
        return {
            "abaqus_cmd": DEFAULT_ABAQUS_CMD,
            "build_script": str(DEFAULT_BUILD_SCRIPT),
            "input_script": str(DEFAULT_INPUT_SCRIPT),
            "import_script": str(DEFAULT_IMPORT_SCRIPT),
            "apply_materials_script": str(DEFAULT_APPLY_MAT_SCRIPT),
            "apply_meshing_script": str(DEFAULT_MESH_SCRIPT),
            "apply_boundary_script": str(DEFAULT_APPLY_BC_SCRIPT),  # NEW
            "default_save_dir": str(SCRIPT_DIR),
            "base_xlsx": "",
            "build_xlsx": "",
            "build_axis": "Y",
            "axis_zero": 0.0,
            # inside the return { ... } defaults block (else branch)
            "ht_build_enabled": False,
            "ht_input_enabled": False,
            "ht_temp_c": 650.0,


        }

    def closeEvent(self, ev):
        # Save settings
        self._settings_path.write_text(json.dumps(self.settings, indent=2), encoding="utf-8")
        super().closeEvent(ev)

    def _edit_settings(self):
        dlg = SettingsDialog(self.settings, self)
        if dlg.exec_():
            self.settings.update(dlg.values)


class SettingsDialog(QtWidgets.QDialog):
    def __init__(self, current, parent=None):
        super().__init__(parent)
        self.values = current.copy()
        self.setWindowTitle("Settings")
        form = QtWidgets.QFormLayout(self)

        self.abaqus_le = QtWidgets.QLineEdit(self.values.get("abaqus_cmd", DEFAULT_ABAQUS_CMD))
        form.addRow("Abaqus command", self.abaqus_le)

        self.build_le = QtWidgets.QLineEdit(self.values.get("build_script", str(DEFAULT_BUILD_SCRIPT)))
        b_btn = QtWidgets.QPushButton("…"); b_btn.clicked.connect(lambda: self._pick(self.build_le))
        hl1 = QtWidgets.QHBoxLayout(); hl1.addWidget(self.build_le); hl1.addWidget(b_btn)
        form.addRow("build_cae script", hl1)

        self.input_le = QtWidgets.QLineEdit(self.values.get("input_script", str(DEFAULT_INPUT_SCRIPT)))
        i_btn = QtWidgets.QPushButton("…"); i_btn.clicked.connect(lambda: self._pick(self.input_le))
        hl2 = QtWidgets.QHBoxLayout(); hl2.addWidget(self.input_le); hl2.addWidget(i_btn)
        form.addRow("create_input script", hl2)

        self.import_le = QtWidgets.QLineEdit(self.values.get("import_script", str(DEFAULT_IMPORT_SCRIPT)))
        im_btn = QtWidgets.QPushButton("…"); im_btn.clicked.connect(lambda: self._pick(self.import_le))
        hl3 = QtWidgets.QHBoxLayout(); hl3.addWidget(self.import_le); hl3.addWidget(im_btn)
        form.addRow("import_and_partition script", hl3)

        self.apply_le = QtWidgets.QLineEdit(self.values.get("apply_materials_script", str(DEFAULT_APPLY_MAT_SCRIPT)))
        ap_btn = QtWidgets.QPushButton("…"); ap_btn.clicked.connect(lambda: self._pick(self.apply_le))
        hl4 = QtWidgets.QHBoxLayout(); hl4.addWidget(self.apply_le); hl4.addWidget(ap_btn)
        form.addRow("apply_materials script", hl4)

        self.mesh_le = QtWidgets.QLineEdit(self.values.get("apply_meshing_script", str(DEFAULT_MESH_SCRIPT)))
        me_btn = QtWidgets.QPushButton("…"); me_btn.clicked.connect(lambda: self._pick(self.mesh_le))
        hl5 = QtWidgets.QHBoxLayout(); hl5.addWidget(self.mesh_le); hl5.addWidget(me_btn)
        form.addRow("apply_meshing script", hl5)

        # NEW: boundary script picker
        self.bc_le = QtWidgets.QLineEdit(self.values.get("apply_boundary_script", str(DEFAULT_APPLY_BC_SCRIPT)))
        bc_btn = QtWidgets.QPushButton("…"); bc_btn.clicked.connect(lambda: self._pick(self.bc_le))
        hl6 = QtWidgets.QHBoxLayout(); hl6.addWidget(self.bc_le); hl6.addWidget(bc_btn)
        form.addRow("apply_boundary script", hl6)

        bb = QtWidgets.QDialogButtonBox(QtWidgets.QDialogButtonBox.Ok | QtWidgets.QDialogButtonBox.Cancel)
        bb.accepted.connect(self.accept); bb.rejected.connect(self.reject)
        form.addRow(bb)

    def _pick(self, line):
        f, _ = QtWidgets.QFileDialog.getOpenFileName(self, "Python file", line.text())
        if f: line.setText(f)

    def accept(self):
        self.values["abaqus_cmd"] = self.abaqus_le.text().strip() or DEFAULT_ABAQUS_CMD
        self.values["build_script"] = self.build_le.text().strip() or str(DEFAULT_BUILD_SCRIPT)
        self.values["input_script"] = self.input_le.text().strip() or str(DEFAULT_INPUT_SCRIPT)
        self.values["import_script"] = self.import_le.text().strip() or str(DEFAULT_IMPORT_SCRIPT)
        self.values["apply_materials_script"] = self.apply_le.text().strip() or str(DEFAULT_APPLY_MAT_SCRIPT)
        self.values["apply_meshing_script"] = self.mesh_le.text().strip() or str(DEFAULT_MESH_SCRIPT)
        self.values["apply_boundary_script"] = self.bc_le.text().strip() or str(DEFAULT_APPLY_BC_SCRIPT)
        super().accept()


def main(argv=None):
    app = QtWidgets.QApplication(argv or sys.argv)
    app.setApplicationName("AM Simulation GUI")
    app.setWindowIcon(QtGui.QIcon.fromTheme("applications-engineering"))
    win = MainWindow(); win.show()
    sys.exit(app.exec_())


if __name__ == "__main__":
    main()
