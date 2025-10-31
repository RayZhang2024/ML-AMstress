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

from PyQt5 import QtCore, QtGui, QtWidgets



__version__ = "0.6.0"
SCRIPT_DIR = Path(__file__).resolve().parent

DEFAULT_ABAQUS_CMD    = "abaqus"
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

            if any(x.startswith("未找到") for x in warnings):
                txt4 += f"""

# ===== GUI injected parameters (fallback) =====
shape_index     = {self.shapes[self.shape_cb.currentText()]}
build_height    = {self.height_sp.value()}
layer_thickness = {self.layer_sp.value()}
savepathName    = r'{save_dir.as_posix()}/'
# =============================================
"""

            patched = Path(self._tmpdir.name) / "build_cae_patched.py"
            patched.write_text(txt4, "utf-8")
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
        row3 = QtWidgets.QHBoxLayout(); row3.addWidget(self.coord_file_le); row3.addWidget(b3)
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
