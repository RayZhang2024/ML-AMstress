# -*- coding: utf-8 -*-
"""A7.2 controlled acceptance fixture for import_and_partition.py.

This Abaqus/CAE noGUI fixture creates one deterministic solid, exports it as a
runtime STEP file, executes the same-checkout production partitioning script,
and writes the success sentinel only after the expected layer-set invariants
are proven from the current in-process model database reacquired after
production returns.
"""

from abaqusConstants import *
from caeModules import *
import abaqus
import imp
import os
import sys
import tempfile


SUCCESS_SENTINEL = "A7.2_PARTITION_LAYER_SETS_REGRESSION_PASSED"
SUCCESS_ENV = "A7_TARGET_SENTINEL_FILE"
DIAGNOSTIC_ENV = "A7_TARGET_DIAGNOSTIC_FILE"
DIAGNOSTIC_TOKENS = set((
    "fixture-setup",
    "production-load",
    "production-execution",
    "model-state-read",
    "model-state-invariant",
    "fixture-internal",
))

CAD_X_MIN = 0.0
CAD_X_MAX = 1.0
CAD_Y_MIN = -1.0
CAD_Y_MAX = 10.3
CAD_Z_MIN = 0.0
CAD_Z_MAX = 1.0

LAYER_THK = 0.5
BUILD_H = 10.3
BUILD_AXIS = "Y"
AXIS_ZERO = 0.0
EXPECTED_LAYER_COUNT = 21
EXPECTED_PART_SETS = ("BASE", "BUILD_ALL")
EXPECTED_ASSEMBLY_SETS = tuple("set-%d" % index for index in range(23))
EXPECTED_PARTIAL_LAYER_SET = "set-21"
EXPECTED_WHOLE_BUILD_SET = "set-22"
ABS_TOL = 1.0e-5


def _write_diagnostic(stage):
    if stage not in DIAGNOSTIC_TOKENS:
        stage = "fixture-internal"
    path = os.environ.get(DIAGNOSTIC_ENV, "")
    if path:
        with open(path, "wb") as stream:
            stream.write(stage.encode("ascii"))


def _write_success():
    path = os.environ.get(SUCCESS_ENV, "")
    if not path:
        raise RuntimeError("success sentinel path is unavailable")
    with open(path, "wb") as stream:
        stream.write(SUCCESS_SENTINEL.encode("ascii"))


def _repo_root():
    here = os.path.abspath(__file__)
    return os.path.abspath(os.path.join(os.path.dirname(here), os.pardir, os.pardir))


def _runtime_path(name, suffix):
    directory = tempfile.mkdtemp(prefix="ml-amstress-a7-2-")
    return os.path.join(directory, name + suffix)


def _current_mdb():
    import abaqus as abaqus_module
    current = getattr(abaqus_module, "mdb", None)
    if current is None:
        raise RuntimeError("current Abaqus model database is unavailable")
    return current


def _create_runtime_step(step_path):
    abaqus.Mdb()
    current = _current_mdb()
    model = current.models["Model-1"]
    sketch = model.ConstrainedSketch(name="A7_2_profile", sheetSize=20.0)
    sketch.rectangle(point1=(CAD_X_MIN, CAD_Y_MIN), point2=(CAD_X_MAX, CAD_Y_MAX))
    part = model.Part(name="A7_2_STEP_EXPORT_SOURCE", dimensionality=THREE_D,
                      type=DEFORMABLE_BODY)
    part.BaseSolidExtrude(sketch=sketch, depth=(CAD_Z_MAX - CAD_Z_MIN))
    part.writeStepFile(fileName=step_path)
    if not os.path.isfile(step_path):
        raise RuntimeError("runtime STEP export did not create a file")
    return step_path


def _load_production_module(repo_root):
    production_path = os.path.join(repo_root, "import_and_partition.py")
    if not os.path.isfile(production_path):
        raise RuntimeError("production import_and_partition.py is missing")
    return imp.load_source("a7_2_same_checkout_import_and_partition", production_path)


def _run_production(module, step_path, save_as_path):
    module.CAD_FILE = step_path
    module.SCALE = 1.0
    module.LAYER_THK = LAYER_THK
    module.BUILD_H = BUILD_H
    module.BUILD_AXIS = BUILD_AXIS
    module.AXIS_ZERO = AXIS_ZERO
    module.SAVE_AS = save_as_path
    module.main()


def _first_point_on_cell(cell):
    point = cell.pointOn[0]
    return float(point[0]), float(point[1]), float(point[2])


def _cell_y_span(cells):
    try:
        bbox = cells.getBoundingBox()
        return float(bbox["low"][1]), float(bbox["high"][1])
    except Exception:
        values = []
        for cell in cells:
            values.append(_first_point_on_cell(cell)[1])
        if not values:
            raise RuntimeError("set has no cell geometry")
        return min(values), max(values)


def _cell_count(cells):
    try:
        return len(cells)
    except TypeError:
        count = 0
        for _cell in cells:
            count += 1
        return count


def _require_non_empty_set(container, name):
    if name not in container.sets.keys():
        raise RuntimeError("missing set " + name)
    cells = container.sets[name].cells
    if _cell_count(cells) <= 0:
        raise RuntimeError("empty set " + name)
    return cells


def _approximately(value, expected):
    return abs(value - expected) <= ABS_TOL


def _assert_span(name, cells, expected_min, expected_max):
    actual_min, actual_max = _cell_y_span(cells)
    if not _approximately(actual_min, expected_min) or not _approximately(actual_max, expected_max):
        raise RuntimeError("%s span %.9f..%.9f did not match %.9f..%.9f"
                           % (name, actual_min, actual_max, expected_min, expected_max))


def _validate_model_state(current):
    model = current.models["Model-1"]
    if "ImportedPart" not in model.parts.keys():
        raise RuntimeError("ImportedPart is missing")
    part = model.parts["ImportedPart"]
    assembly = model.rootAssembly

    for name in EXPECTED_PART_SETS:
        _require_non_empty_set(part, name)

    assembly_set_names = tuple(sorted(
        [name for name in assembly.sets.keys() if name.startswith("set-")],
        key=lambda item: int(item.split("-", 1)[1])
    ))
    if assembly_set_names != EXPECTED_ASSEMBLY_SETS:
        raise RuntimeError("assembly set family is not set-0..set-22")

    _require_non_empty_set(assembly, "set-0")
    for index in range(1, EXPECTED_LAYER_COUNT + 1):
        _require_non_empty_set(assembly, "set-%d" % index)
    _require_non_empty_set(assembly, EXPECTED_WHOLE_BUILD_SET)

    _assert_span(EXPECTED_PARTIAL_LAYER_SET,
                 assembly.sets[EXPECTED_PARTIAL_LAYER_SET].cells, 10.0, 10.3)
    _assert_span(EXPECTED_WHOLE_BUILD_SET,
                 assembly.sets[EXPECTED_WHOLE_BUILD_SET].cells, 0.0, 10.3)


def main():
    step_path = _runtime_path("a7_2_partition_source", ".stp")
    save_as_path = _runtime_path("a7_2_partition_result", ".cae")
    try:
        _create_runtime_step(step_path)
    except Exception:
        _write_diagnostic("fixture-setup")
        return 1

    try:
        module = _load_production_module(_repo_root())
    except Exception:
        _write_diagnostic("production-load")
        return 1

    try:
        _run_production(module, step_path, save_as_path)
    except Exception:
        _write_diagnostic("production-execution")
        return 1

    try:
        current = _current_mdb()
        if "Model-1" not in current.models.keys():
            raise RuntimeError("Model-1 is missing")
    except Exception:
        _write_diagnostic("model-state-read")
        return 1

    try:
        _validate_model_state(current)
    except Exception:
        _write_diagnostic("model-state-invariant")
        return 1

    try:
        _write_success()
    except Exception:
        _write_diagnostic("fixture-internal")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
