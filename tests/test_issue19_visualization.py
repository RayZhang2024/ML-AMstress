import ast
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


class VisualizationFoundationTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.panel_path = ROOT / "visualization_panel.py"
        cls.panel_source = cls.panel_path.read_text(encoding="utf-8")
        cls.gui_source = (ROOT / "AM_gui_v7.py").read_text(encoding="utf-8")

    def test_panel_is_a_parseable_reusable_component_with_lifecycle_api(self):
        tree = ast.parse(self.panel_source, filename=str(self.panel_path))
        classes = [node for node in tree.body if isinstance(node, ast.ClassDef)]
        panel = next(node for node in classes if node.name == "VisualizationPanel")
        methods = {node.name for node in panel.body if isinstance(node, ast.FunctionDef)}
        self.assertTrue({"clear", "reset_camera", "show_points"}.issubset(methods))
        self.assertIn("from PyQt5 import QtCore, QtWidgets", self.panel_source)
        self.assertIn("from pyvistaqt import QtInteractor as _QtInteractor", self.panel_source)
        self.assertIn("except Exception:", self.panel_source)
        self.assertIn("_pv = None", self.panel_source)

    def test_data_alignment_uses_independent_horizontal_splitter_and_viewer(self):
        self.assertIn("from visualization_panel import VisualizationPanel", self.gui_source)
        self.assertIn("self.viewer = VisualizationPanel(self)", self.gui_source)
        self.assertIn("QtWidgets.QSplitter(QtCore.Qt.Horizontal)", self.gui_source)
        self.assertIn("self._splitter.addWidget(left_widget)", self.gui_source)
        self.assertIn("self._splitter.addWidget(right_widget)", self.gui_source)
        self.assertIn("self.viewer.clear()", self.gui_source)
        self.assertIn("self.viewer.reset_camera", self.gui_source)
        self.assertIn("self.viewer.show_points(np.vstack(point_sets))", self.gui_source)

    def test_optional_backend_is_separately_documented(self):
        optional = (ROOT / "requirements-visualization.txt").read_text(encoding="utf-8")
        self.assertIn("pyvista", optional)
        self.assertIn("pyvistaqt", optional)
        self.assertIn("vtk", optional)
        architecture = (ROOT / "docs" / "ARCHITECTURE.md").read_text(encoding="utf-8")
        self.assertIn("does not read Abaqus `.cae` files", architecture)


if __name__ == "__main__":
    unittest.main()
