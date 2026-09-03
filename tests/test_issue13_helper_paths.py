import tempfile
import unittest
from pathlib import Path

import AM_gui_v7


class _DummyLaunch(AM_gui_v7.LaunchMixin):
    pass


class _FakeMessageBox:
    Warning = object()
    AcceptRole = object()
    RejectRole = object()
    next_choice = "continue"
    last = None

    def __init__(self, parent):
        self.parent = parent
        self.buttons = []
        self.informative_text = ""
        self.clicked = None
        _FakeMessageBox.last = self

    def setIcon(self, icon):
        self.icon = icon

    def setWindowTitle(self, title):
        self.title = title

    def setText(self, text):
        self.text = text

    def setInformativeText(self, text):
        self.informative_text = text

    def addButton(self, label, role):
        button = (label, role)
        self.buttons.append(button)
        return button

    def setDefaultButton(self, button):
        self.default_button = button

    def exec_(self):
        self.clicked = self.buttons[0] if self.next_choice == "continue" else self.buttons[1]

    def clickedButton(self):
        return self.clicked


class HelperPathTests(unittest.TestCase):
    def test_same_directory_and_nested_paths_are_not_mismatches(self):
        with tempfile.TemporaryDirectory() as tmp:
            runtime = Path(tmp) / "gui"
            same = runtime / "create_input.py"
            nested = runtime / "helpers" / "build_cae.py"
            resolved_runtime, mismatches = AM_gui_v7._external_helper_paths(
                [("create_input.py", same), ("build_cae.py", nested)], runtime
            )

        self.assertEqual(resolved_runtime, runtime.resolve())
        self.assertEqual(mismatches, [])

    def test_external_paths_are_reported_with_labels(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            runtime = root / "gui"
            external = root / "other-checkout" / "create_input.py"
            _, mismatches = AM_gui_v7._external_helper_paths(
                [("create_input.py", external)], runtime
            )

        self.assertEqual(len(mismatches), 1)
        self.assertEqual(mismatches[0][0], "create_input.py")
        self.assertEqual(mismatches[0][1], external.resolve())

    def test_warning_can_cancel_or_continue_without_rewriting_paths(self):
        original = AM_gui_v7.QtWidgets.QMessageBox
        original_script_dir = AM_gui_v7.SCRIPT_DIR
        AM_gui_v7.QtWidgets.QMessageBox = _FakeMessageBox
        try:
            with tempfile.TemporaryDirectory() as tmp:
                runtime = Path(tmp) / "gui"
                external = Path(tmp) / "other-checkout" / "build_cae.py"
                dummy = _DummyLaunch()
                AM_gui_v7.SCRIPT_DIR = runtime

                _FakeMessageBox.next_choice = "cancel"
                self.assertFalse(dummy._confirm_helper_paths(
                    "Build Model", [("build_cae.py", external)]
                ))
                first_warning = _FakeMessageBox.last
                self.assertIn(str(runtime.resolve()), first_warning.informative_text)
                self.assertIn(str(external.resolve()), first_warning.informative_text)

                _FakeMessageBox.next_choice = "continue"
                self.assertTrue(dummy._confirm_helper_paths(
                    "Build Model", [("build_cae.py", external)]
                ))
                self.assertEqual(external, Path(external))
        finally:
            AM_gui_v7.QtWidgets.QMessageBox = original
            AM_gui_v7.SCRIPT_DIR = original_script_dir


if __name__ == "__main__":
    unittest.main()
