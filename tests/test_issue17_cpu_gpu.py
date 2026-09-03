import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


class CpuGpuResourceTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.gui_source = (ROOT / "AM_gui_v7.py").read_text(encoding="utf-8")
        cls.input_source = (ROOT / "create_input.py").read_text(encoding="utf-8")

    def test_input_tab_exposes_valid_cpu_gpu_ranges_and_persistence(self):
        self.assertIn("self.cpu_count_sp.setRange(1, 1000000)", self.gui_source)
        self.assertIn("self.gpu_count_sp.setRange(0, 1000000)", self.gui_source)
        self.assertIn('self.settings["num_cpus"] = num_cpus', self.gui_source)
        self.assertIn('self.settings["num_gpus"] = num_gpus', self.gui_source)
        self.assertIn('s.setdefault("num_cpus", 1)', self.gui_source)
        self.assertIn('s.setdefault("num_gpus", 0)', self.gui_source)

    def test_cpu_only_and_non_default_values_are_dynamically_injected(self):
        self.assertIn('f"NUM_CPUS = {num_cpus}\\n"', self.gui_source)
        self.assertIn('f"NUM_GPUS = {num_gpus}\\n"', self.gui_source)
        for cpu, gpu in ((1, 0), (12, 1)):
            injected = "NUM_CPUS = %d\\nNUM_GPUS = %d\\n" % (cpu, gpu)
            self.assertRegex(
                injected,
                r"NUM_CPUS = %d\\nNUM_GPUS = %d\\n" % (cpu, gpu),
            )

    def test_create_input_uses_same_constants_for_jobs_and_submission(self):
        self.assertIn("numCpus=NUM_CPUS", self.input_source)
        self.assertIn("numGPUs=NUM_GPUS", self.input_source)
        self.assertIn('" cpus="+str(NUM_CPUS)+" gpus="+str(NUM_GPUS)',
                      self.input_source)
        self.assertNotIn("cpus=14 gpus=1", self.input_source)


if __name__ == "__main__":
    unittest.main()
