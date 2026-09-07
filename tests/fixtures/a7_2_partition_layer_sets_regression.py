# -*- coding: utf-8 -*-
"""A7.2 live Abaqus acceptance fixture for import_and_partition.py.

This fixture intentionally exercises only CAD import and geometric partition
set creation.  It is executed by Abaqus/CAE 2021 Python, not normal Python.
"""

from abaqus import *
from abaqusConstants import *
from caeModules import *
import abaqus as abaqus_module
import imp
import os


SENTINEL_ENVIRONMENT = "A7_TARGET_SENTINEL_FILE"
SUCCESS_MARKER = "A7.2_PARTITION_LAYER_SETS_REGRESSION_PASSED"
FIXTURE_TOLERANCE = 2.0e-4


def _require_sentinel_path():
    path = os.environ.get(SENTINEL_ENVIRONMENT, "")
    if not path:
        raise RuntimeError("Missing required sentinel path")
    return path


def _repo_root():
    here = os.path.abspath(__file__)
    return os.path.dirname(os.path.dirname(os.path.dirname(here)))


def _runtime_path(directory, filename):
    return os.path.join(directory, filename)


def _runtime_directory(sentinel_path):
    directory = os.path.dirname(os.path.abspath(sentinel_path))
    if not directory:
        directory = os.getcwd()
    path = os.path.join(directory, "a7_2_partition_layer_sets_runtime")
    if not os.path.isdir(path):
        os.makedirs(path)
    return path


def _generate_controlled_acis(acis_path):
    Mdb()
    model = mdb.models["Model-1"]
    sketch = model.ConstrainedSketch(name="__a7_2_controlled_profile__", sheetSize=20.0)
    sketch.rectangle(point1=(0.0, -1.0), point2=(1.0, 10.3))
    part = model.Part(name="A7_2_ControlledSolid", dimensionality=THREE_D, type=DEFORMABLE_BODY)
    part.BaseSolidExtrude(sketch=sketch, depth=1.0)
    del model.sketches["__a7_2_controlled_profile__"]
    part.writeAcisFile(fileName=acis_path)


def _load_production_module(production_path):
    return imp.load_source("a7_2_same_checkout_import_and_partition", production_path)


def _configure_and_run_production(production, acis_path, save_path):
    production.CAD_FILE = acis_path
    production.SCALE = 1.0
    production.LAYER_THK = 0.5
    production.BUILD_H = 10.3
    production.BUILD_AXIS = "Y"
    production.AXIS_ZERO = 0.0
    production.SAVE_AS = save_path
    production.main()


def _keys(mapping):
    return list(mapping.keys())


def _require_key(mapping, key, label):
    if key not in mapping:
        raise RuntimeError("Missing %s: %s" % (label, key))
    return mapping[key]


def _cell_array(region, label):
    cells = getattr(region, "cells", None)
    if cells is None:
        raise RuntimeError("%s has no cells" % label)
    if len(cells) == 0:
        raise RuntimeError("%s is empty" % label)
    if not hasattr(cells, "getBoundingBox"):
        raise RuntimeError("%s cells cannot provide a direct bounding box" % label)
    return cells


def _cell_y_bounds(region, label):
    cells = _cell_array(region, label)
    bounds = cells.getBoundingBox()
    low = bounds["low"]
    high = bounds["high"]
    return low[1], high[1]


def _assert_close(actual, expected, label):
    if abs(actual - expected) > FIXTURE_TOLERANCE:
        raise RuntimeError("%s expected %.9f, got %.9f" % (label, expected, actual))


def _assert_y_span(region, low_expected, high_expected, label):
    low, high = _cell_y_bounds(region, label)
    _assert_close(low, low_expected, label + " low Y")
    _assert_close(high, high_expected, label + " high Y")


def _assert_exact_assembly_sets(assembly):
    expected = ["set-%d" % index for index in range(23)]
    actual = _keys(assembly.sets)
    if set(actual) != set(expected):
        raise RuntimeError("Assembly sets mismatch: expected %s, got %s" % (expected, actual))


def _validate_model_state():
    current_mdb = abaqus_module.mdb
    model = _require_key(current_mdb.models, "Model-1", "model")
    part = _require_key(model.parts, "ImportedPart", "part")
    assembly = model.rootAssembly
    instance = _require_key(assembly.instances, "ImportedPart-1", "assembly instance")

    if len(instance.cells) == 0:
        raise RuntimeError("ImportedPart-1 instance is empty")

    base_part_set = _require_key(part.sets, "BASE", "part set")
    build_all_part_set = _require_key(part.sets, "BUILD_ALL", "part set")
    _assert_y_span(base_part_set, -1.0, 0.0, "part BASE")
    _assert_y_span(build_all_part_set, 0.0, 10.3, "part BUILD_ALL")

    _assert_exact_assembly_sets(assembly)
    _assert_y_span(_require_key(assembly.sets, "set-0", "assembly set"), -1.0, 0.0, "assembly set-0")
    _assert_y_span(_require_key(assembly.sets, "set-21", "assembly set"), 10.0, 10.3, "assembly set-21")
    _assert_y_span(_require_key(assembly.sets, "set-22", "assembly set"), 0.0, 10.3, "assembly set-22")


def _write_success_marker(path):
    directory = os.path.dirname(os.path.abspath(path))
    if directory and not os.path.isdir(directory):
        os.makedirs(directory)
    stream = open(path, "wb")
    try:
        stream.write(SUCCESS_MARKER.encode("ascii"))
    finally:
        stream.close()


def main():
    sentinel_path = _require_sentinel_path()
    if os.path.exists(sentinel_path):
        os.remove(sentinel_path)

    root = _repo_root()
    production_path = os.path.join(root, "import_and_partition.py")
    if not os.path.isfile(production_path):
        raise RuntimeError("Missing production script")

    runtime_dir = _runtime_directory(sentinel_path)
    acis_path = _runtime_path(runtime_dir, "a7_2_controlled_rectangular_solid.sat")
    save_path = _runtime_path(runtime_dir, "a7_2_partition_layer_sets.cae")

    _generate_controlled_acis(acis_path)
    production = _load_production_module(production_path)
    _configure_and_run_production(production, acis_path, save_path)
    _validate_model_state()
    _write_success_marker(sentinel_path)


if __name__ == "__main__":
    main()
