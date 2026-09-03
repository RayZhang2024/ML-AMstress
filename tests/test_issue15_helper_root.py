import tempfile
import unittest
from pathlib import Path

import AM_gui_v7


class HelperRootTests(unittest.TestCase):
    def test_default_root_uses_runtime_and_fixed_filenames(self):
        with tempfile.TemporaryDirectory() as tmp:
            runtime = Path(tmp) / "gui"
            paths, external = AM_gui_v7._helper_paths_for_names(
                {}, AM_gui_v7.HELPER_FILENAMES, runtime
            )

        self.assertFalse(external)
        self.assertEqual([name for name, _ in paths], list(AM_gui_v7.HELPER_FILENAMES))
        self.assertTrue(all(path.parent == runtime.resolve() for _, path in paths))

    def test_external_root_is_one_coherent_override(self):
        with tempfile.TemporaryDirectory() as tmp:
            external_root = Path(tmp) / "other-checkout"
            settings = {
                "use_external_helper_root": True,
                "external_helper_root": str(external_root),
            }
            paths, external = AM_gui_v7._helper_paths_for_names(
                settings, ["create_input.py", "apply_meshing.py"], Path(tmp) / "gui"
            )

        self.assertTrue(external)
        self.assertEqual(
            paths,
            [
                ("create_input.py", external_root.resolve() / "create_input.py"),
                ("apply_meshing.py", external_root.resolve() / "apply_meshing.py"),
            ],
        )

    def test_missing_helper_files_are_detected_without_fallback(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp) / "helpers"
            root.mkdir()
            (root / "create_input.py").write_text("# helper", encoding="utf-8")
            paths, _ = AM_gui_v7._helper_paths_for_names(
                {"use_external_helper_root": True, "external_helper_root": str(root)},
                ["create_input.py", "apply_meshing.py"],
                Path(tmp) / "gui",
            )
            missing = AM_gui_v7._missing_helper_paths(paths)

        self.assertEqual(missing, [("apply_meshing.py", root / "apply_meshing.py")])

    def test_build_workflow_mapping_is_conditional(self):
        self.assertEqual(
            AM_gui_v7._build_helper_names(False),
            ["build_cae.py"],
        )
        self.assertEqual(
            AM_gui_v7._build_helper_names(True),
            ["import_and_partition.py", "apply_meshing.py"],
        )
        self.assertEqual(
            AM_gui_v7._build_helper_names(True, materials_enabled=True,
                                          boundary_enabled=True),
            [
                "import_and_partition.py",
                "apply_materials.py",
                "apply_meshing.py",
                "apply_boundary.py",
            ],
        )

    def test_coherent_legacy_external_paths_remain_inactive_candidate(self):
        with tempfile.TemporaryDirectory() as tmp:
            runtime = Path(tmp) / "gui"
            external = Path(tmp) / "other-checkout"
            settings = {
                key: str(external / filename)
                for key, filename in zip(
                    AM_gui_v7.HELPER_SETTING_KEYS, AM_gui_v7.HELPER_FILENAMES
                )
            }
            AM_gui_v7._migrate_legacy_helper_settings(settings, runtime)

        self.assertFalse(settings["use_external_helper_root"])
        self.assertNotIn("external_helper_root", settings)
        self.assertEqual(settings["_legacy_helper_root_candidate"],
                         str(external.resolve()))
        self.assertTrue(settings["_legacy_helper_paths_migrated"])
        self.assertNotIn("build_script", settings)
        self.assertEqual(settings["_legacy_helper_paths"]["input_script"],
                         str(external / "create_input.py"))

    def test_legacy_paths_from_other_checkout_use_current_runtime_helpers(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            runtime = root / "ML-AMstress-prXX"
            legacy_root = root / "ML-AMstress"
            settings = {
                key: str(legacy_root / filename)
                for key, filename in zip(
                    AM_gui_v7.HELPER_SETTING_KEYS, AM_gui_v7.HELPER_FILENAMES
                )
            }
            AM_gui_v7._migrate_legacy_helper_settings(settings, runtime)
            paths, external = AM_gui_v7._helper_paths_for_names(
                settings, ["build_cae.py", "create_input.py"], runtime
            )

        self.assertFalse(settings["use_external_helper_root"])
        self.assertFalse(external)
        self.assertEqual(
            paths,
            [(name, runtime.resolve() / name)
             for name in ("build_cae.py", "create_input.py")],
        )

    def test_explicit_new_format_external_override_is_preserved(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            runtime = root / "ML-AMstress-prXX"
            legacy_root = root / "ML-AMstress"
            explicit_root = root / "approved-helpers"
            settings = {
                "use_external_helper_root": True,
                "external_helper_root": str(explicit_root),
            }
            settings.update({
                key: str(legacy_root / filename)
                for key, filename in zip(
                    AM_gui_v7.HELPER_SETTING_KEYS, AM_gui_v7.HELPER_FILENAMES
                )
            })
            AM_gui_v7._migrate_legacy_helper_settings(settings, runtime)
            paths, external = AM_gui_v7._helper_paths_for_names(
                settings, ["create_input.py"], runtime
            )

        self.assertTrue(settings["use_external_helper_root"])
        self.assertTrue(external)
        self.assertEqual(paths[0][1], explicit_root.resolve() / "create_input.py")
        self.assertEqual(settings["_legacy_helper_root_candidate"],
                         str(legacy_root.resolve()))

    def test_coherent_legacy_runtime_paths_select_default_root(self):
        with tempfile.TemporaryDirectory() as tmp:
            runtime = Path(tmp) / "gui"
            settings = {
                key: str(runtime / filename)
                for key, filename in zip(
                    AM_gui_v7.HELPER_SETTING_KEYS, AM_gui_v7.HELPER_FILENAMES
                )
            }
            AM_gui_v7._migrate_legacy_helper_settings(settings, runtime)

        self.assertFalse(settings["use_external_helper_root"])
        self.assertNotIn("external_helper_root", settings)
        self.assertTrue(settings["_legacy_helper_paths_migrated"])

    def test_incoherent_legacy_paths_become_inactive_with_diagnostic(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            settings = {"abaqus_cmd": "abq2021"}
            settings.update({
                key: str((root / ("one" if index == 0 else "two")) / filename)
                for index, (key, filename) in enumerate(
                    zip(AM_gui_v7.HELPER_SETTING_KEYS, AM_gui_v7.HELPER_FILENAMES)
                )
            })
            AM_gui_v7._migrate_legacy_helper_settings(settings, root / "gui")

        self.assertFalse(settings["use_external_helper_root"])
        self.assertEqual(settings["abaqus_cmd"], "abq2021")
        effective_root, external = AM_gui_v7._effective_helper_root(settings, root / "gui")
        self.assertFalse(external)
        self.assertEqual(effective_root, (root / "gui").resolve())
        self.assertIn("inactive", settings["_legacy_helper_paths_warning"])
        self.assertNotIn("apply_boundary_script", settings)
        self.assertEqual(len(settings["_legacy_helper_paths"]), 6)


if __name__ == "__main__":
    unittest.main()
