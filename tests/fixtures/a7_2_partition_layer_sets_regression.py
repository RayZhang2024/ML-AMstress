# -*- coding: utf-8 -*-
"""Controlled A7.2 import/partition layer-set regression fixture.

This script is executed by Abaqus/CAE from the trusted A7.2 controller.  It
generates one deterministic solid CAD file at runtime, runs the same-checkout
production import_and_partition.py, and writes the controller sentinel only
after checking the resulting in-process model state.
"""

from abaqus import *
from abaqusConstants import *
from caeModules import *

import imp
import os
import shutil
import tempfile


SENTINEL_ENV = "A7_TARGET_SENTINEL_FILE"
PASS_SENTINEL = "A7.2_PARTITION_LAYER_SETS_REGRESSION_PASSED"

PART_NAME = "ImportedPart"
INSTANCE_NAME = "ImportedPart-1"
BUILD_AXIS = "Y"
AXIS_ZERO = 0.0
BUILD_H = 10.3
LAYER_THK = 0.5
EXPECTED_LAYER_COUNT = 21
EXPECTED_PART_SETS = ("BASE", "BUILD_ALL")
EXPECTED_ASSEMBLY_SETS = tuple("set-%d" % index for index in range(23))
EXPECTED_PARTIAL_SET = "set-21"
EXPECTED_WHOLE_BUILD_SET = "set-22"
TOL = 1.0e-6


def _repository_root():
    here = os.path.abspath(__file__)
    return os.path.dirname(os.path.dirname(os.path.dirname(here)))


def _require_sentinel_path():
    marker = os.environ.get(SENTINEL_ENV, "")
    if not marker:
        raise RuntimeError("%s is required" % SENTINEL_ENV)
    if os.path.exists(marker):
        os.remove(marker)
    return marker


def _write_marker(marker):
    directory = os.path.dirname(os.path.abspath(marker))
    if directory and not os.path.isdir(directory):
        os.makedirs(directory)
    handle = open(marker, "wb")
    try:
        handle.write(PASS_SENTINEL.encode("ascii"))
    finally:
        handle.close()


def _generate_controlled_acis(path):
    Mdb()
    model = mdb.models["Model-1"]
    sketch = model.ConstrainedSketch(name="__a7_2_profile__", sheetSize=20.0)
    sketch.rectangle(point1=(0.0, -1.0), point2=(1.0, 10.3))
    part = model.Part(name="A7_2_ControlledSolid", dimensionality=THREE_D,
                      type=DEFORMABLE_BODY)
    part.BaseSolidExtrude(sketch=sketch, depth=1.0)
    del model.sketches["__a7_2_profile__"]

    assembly = model.rootAssembly
    assembly.DatumCsysByDefault(CARTESIAN)
    assembly.Instance(name="A7_2_ControlledSolid-1", part=part, dependent=ON)
    assembly.regenerate()
    assembly.writeAcisFile(fileName=path)


def _load_production_module(repo_root):
    production_path = os.path.join(repo_root, "import_and_partition.py")
    if not os.path.isfile(production_path):
        raise RuntimeError("missing production script: import_and_partition.py")
    return imp.load_source("a7_2_same_checkout_import_and_partition",
                           production_path)


def _run_production(module, cad_path, cae_path):
    module.CAD_FILE = cad_path
    module.SCALE = 1.0
    module.LAYER_THK = LAYER_THK
    module.BUILD_H = BUILD_H
    module.BUILD_AXIS = BUILD_AXIS
    module.AXIS_ZERO = AXIS_ZERO
    module.SAVE_AS = cae_path
    module.main()


def _assert(condition, message):
    if not condition:
        raise RuntimeError(message)


def _cell_indices(cells):
    indices = []
    _append_cell_indices(cells, indices)
    return tuple(sorted(indices))


def _append_cell_indices(value, indices):
    if hasattr(value, "index"):
        indices.append(value.index)
        return
    for item in value:
        _append_cell_indices(item, indices)


def _bbox_for_y(instance, lo_y, hi_y):
    points = []
    for vertex in instance.vertices:
        points.append(vertex.pointOn[0])
    _assert(points, "ImportedPart-1 has no vertices")

    xs = [point[0] for point in points]
    zs = [point[2] for point in points]
    span = max(max(xs) - min(xs), hi_y - lo_y, max(zs) - min(zs))
    pad = max(TOL, 1.0e-6 * span)
    return (min(xs) - pad, lo_y - pad, min(zs) - pad,
            max(xs) + pad, hi_y + pad, max(zs) + pad)


def _assert_same_cells(label, actual_cells, expected_cells):
    actual = _cell_indices(actual_cells)
    expected = _cell_indices(expected_cells)
    _assert(actual, "%s is empty" % label)
    _assert(actual == expected,
            "%s cells %s do not match expected %s" % (label, actual, expected))


def _validate_model_state():
    model = mdb.models["Model-1"]
    _assert(PART_NAME in model.parts, "part ImportedPart is missing")
    part = model.parts[PART_NAME]

    part_set_names = set(part.sets.keys())
    for name in EXPECTED_PART_SETS:
        _assert(name in part_set_names, "part set %s is missing" % name)
        _assert(len(part.sets[name].cells) > 0, "part set %s is empty" % name)

    assembly = model.rootAssembly
    _assert(INSTANCE_NAME in assembly.instances,
            "assembly instance ImportedPart-1 is missing")
    instance = assembly.instances[INSTANCE_NAME]

    assembly_set_names = set(assembly.sets.keys())
    _assert(assembly_set_names == set(EXPECTED_ASSEMBLY_SETS),
            "assembly sets %s do not match %s" %
            (assembly_set_names, EXPECTED_ASSEMBLY_SETS))

    for index in range(0, EXPECTED_LAYER_COUNT + 2):
        name = "set-%d" % index
        _assert(len(assembly.sets[name].cells) > 0,
                "assembly set %s is empty" % name)

    top_layer_cells = instance.cells.getByBoundingBox(
        *_bbox_for_y(instance, 10.0, 10.3))
    _assert_same_cells(EXPECTED_PARTIAL_SET,
                       assembly.sets[EXPECTED_PARTIAL_SET].cells,
                       top_layer_cells)

    whole_build_cells = instance.cells.getByBoundingBox(
        *_bbox_for_y(instance, AXIS_ZERO, AXIS_ZERO + BUILD_H))
    _assert_same_cells(EXPECTED_WHOLE_BUILD_SET,
                       assembly.sets[EXPECTED_WHOLE_BUILD_SET].cells,
                       whole_build_cells)

    base_cells = instance.cells.getByBoundingBox(
        *_bbox_for_y(instance, -1.0, AXIS_ZERO))
    _assert_same_cells("set-0", assembly.sets["set-0"].cells, base_cells)


def main():
    marker = _require_sentinel_path()
    repo_root = _repository_root()
    module = _load_production_module(repo_root)
    temp_root = tempfile.mkdtemp(prefix="a7_2_partition_")
    try:
        cad_path = os.path.join(temp_root, "controlled_solid.sat")
        cae_path = os.path.join(temp_root, "controlled_partition.cae")
        _generate_controlled_acis(cad_path)
        _run_production(module, cad_path, cae_path)
        _validate_model_state()
        _write_marker(marker)
    finally:
        try:
            shutil.rmtree(temp_root)
        except:
            pass


if __name__ == "__main__":
    main()
