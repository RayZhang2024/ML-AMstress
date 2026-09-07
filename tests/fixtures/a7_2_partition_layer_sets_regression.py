# -*- coding: utf-8 -*-
"""Controlled A7.2 import/partition acceptance fixture.

Run only under Abaqus/CAE noGUI through the trusted
partition-layer-sets-regression profile.
"""

from __future__ import print_function

import imp
import os
import tempfile


SUCCESS_SENTINEL = "A7.2_PARTITION_LAYER_SETS_REGRESSION_PASSED"
DIAGNOSTIC_TOKENS = (
    "fixture-setup",
    "production-load",
    "production-execution",
    "model-state-read",
    "model-state-invariant",
    "fixture-internal",
)

SCALE = 1.0
LAYER_THK = 0.5
BUILD_H = 10.3
BUILD_AXIS = "Y"
AXIS_ZERO = 0.0

EXPECTED_LAYER_COUNT = 21
EXPECTED_ASSEMBLY_SETS = tuple("set-%d" % index for index in range(23))
TOL = 1.0e-6

_diagnostic_written = False


class FixtureStageError(RuntimeError):
    pass


def _write_exact(path, text):
    stream = open(path, "wb")
    try:
        stream.write(text.encode("ascii"))
    finally:
        stream.close()


def _write_diagnostic_once(path, token):
    global _diagnostic_written
    if _diagnostic_written:
        return
    if token not in DIAGNOSTIC_TOKENS:
        token = "fixture-internal"
    _write_exact(path, token)
    _diagnostic_written = True


def _fail(stage, message):
    raise FixtureStageError("%s: %s" % (stage, message))


def _repo_root():
    fixture_path = os.path.abspath(__file__)
    fixtures_dir = os.path.dirname(fixture_path)
    tests_dir = os.path.dirname(fixtures_dir)
    return os.path.dirname(tests_dir)


def _require_controller_paths():
    sentinel_path = os.environ.get("A7_TARGET_SENTINEL_FILE")
    diagnostic_path = os.environ.get("A7_TARGET_DIAGNOSTIC_FILE")
    return sentinel_path, diagnostic_path


def _assert_controller_paths(sentinel_path, diagnostic_path):
    if not sentinel_path:
        _fail("fixture-setup", "missing A7_TARGET_SENTINEL_FILE")
    if not diagnostic_path:
        _fail("fixture-setup", "missing A7_TARGET_DIAGNOSTIC_FILE")


def _make_temp_paths():
    temp_dir = tempfile.mkdtemp(prefix="a7_2_partition_fixture_")
    cad_path = os.path.join(temp_dir, "a7_2_box.sat")
    cae_path = os.path.join(temp_dir, "a7_2_partitioned.cae")
    return cad_path, cae_path


def _generate_acis_solid(cad_path):
    from abaqus import Mdb
    from abaqusConstants import DEFORMABLE_BODY, THREE_D

    Mdb()
    from abaqus import mdb

    model = mdb.models["Model-1"]
    sketch = model.ConstrainedSketch(name="A72SolidProfile", sheetSize=20.0)
    sketch.rectangle(point1=(0.0, -1.0), point2=(1.0, 10.3))
    part = model.Part(
        name="A72FixtureSolid",
        dimensionality=THREE_D,
        type=DEFORMABLE_BODY,
    )
    part.BaseSolidExtrude(sketch=sketch, depth=1.0)
    part.writeAcisFile(fileName=cad_path)


def _load_production_module(repo_root):
    production_path = os.path.join(repo_root, "import_and_partition.py")
    if not os.path.isfile(production_path):
        _fail("production-load", "missing same-checkout production script")
    return imp.load_source("a7_2_import_and_partition_under_test", production_path)


def _run_production(module, cad_path, save_as_path):
    module.SCALE = SCALE
    module.LAYER_THK = LAYER_THK
    module.BUILD_H = BUILD_H
    module.BUILD_AXIS = BUILD_AXIS
    module.AXIS_ZERO = AXIS_ZERO
    module.CAD_FILE = cad_path
    module.SAVE_AS = save_as_path
    module.main()


def _current_model_state():
    from abaqus import mdb

    try:
        model = mdb.models["Model-1"]
        part = model.parts["ImportedPart"]
        assembly = model.rootAssembly
    except Exception:
        _fail("model-state-read", "cannot reacquire imported model state")
    return model, part, assembly


def _has_cells(region):
    try:
        return len(region.cells) > 0
    except Exception:
        return False


def _bbox(cells):
    try:
        bounds = cells.getBoundingBox()
        return bounds["low"], bounds["high"]
    except Exception:
        _fail("model-state-invariant", "cannot compute set-owned cell bounds")


def _assert_close(actual, expected, label):
    if abs(actual - expected) > TOL:
        _fail(
            "model-state-invariant",
            "%s %.12g != %.12g" % (label, actual, expected),
        )


def _assert_interval(cells, lo, hi, label):
    low, high = _bbox(cells)
    _assert_close(low[1], lo, label + " low-y")
    _assert_close(high[1], hi, label + " high-y")


def _assert_part_sets(part):
    for name in ("BASE", "BUILD_ALL"):
        if name not in part.sets:
            _fail("model-state-invariant", "missing part set " + name)
        if not _has_cells(part.sets[name]):
            _fail("model-state-invariant", "empty part set " + name)
    _assert_interval(part.sets["BASE"].cells, -1.0, 0.0, "BASE")
    _assert_interval(part.sets["BUILD_ALL"].cells, 0.0, 10.3, "BUILD_ALL")


def _assert_assembly_sets(assembly):
    actual_names = tuple(sorted(assembly.sets.keys(), key=_set_sort_key))
    if actual_names != EXPECTED_ASSEMBLY_SETS:
        _fail("model-state-invariant", "assembly set family mismatch")

    for name in EXPECTED_ASSEMBLY_SETS:
        if not _has_cells(assembly.sets[name]):
            _fail("model-state-invariant", "empty assembly set " + name)

    _assert_interval(assembly.sets["set-0"].cells, -1.0, 0.0, "set-0")
    for index in range(1, EXPECTED_LAYER_COUNT + 1):
        lo = (index - 1) * LAYER_THK
        hi = min(index * LAYER_THK, BUILD_H)
        _assert_interval(assembly.sets["set-%d" % index].cells, lo, hi, "set-%d" % index)
    _assert_interval(assembly.sets["set-21"].cells, 10.0, 10.3, "set-21")
    _assert_interval(assembly.sets["set-22"].cells, 0.0, 10.3, "set-22")


def _set_sort_key(name):
    prefix = "set-"
    if not name.startswith(prefix):
        _fail("model-state-invariant", "unexpected assembly set " + name)
    try:
        return int(name[len(prefix):])
    except Exception:
        _fail("model-state-invariant", "unexpected assembly set " + name)


def _assert_model_invariants():
    unused_model, part, assembly = _current_model_state()
    _assert_part_sets(part)
    _assert_assembly_sets(assembly)


def main():
    sentinel_path = None
    diagnostic_path = None
    current_stage = "fixture-setup"
    try:
        sentinel_path, diagnostic_path = _require_controller_paths()
        _assert_controller_paths(sentinel_path, diagnostic_path)
        repo_root = _repo_root()
        production_path = os.path.join(repo_root, "import_and_partition.py")
        if not os.path.isfile(production_path):
            _fail("production-load", "missing same-checkout production script")

        cad_path, save_as_path = _make_temp_paths()
        _generate_acis_solid(cad_path)
        if not os.path.isfile(cad_path):
            _fail("fixture-setup", "ACIS export did not create CAD file")

        current_stage = "production-load"
        module = _load_production_module(repo_root)

        current_stage = "production-execution"
        _run_production(module, cad_path, save_as_path)

        current_stage = "model-state-read"
        _current_model_state()

        current_stage = "model-state-invariant"
        _assert_model_invariants()

        current_stage = "fixture-internal"
        _write_exact(sentinel_path, SUCCESS_SENTINEL)
    except FixtureStageError as error:
        stage = str(error).split(":", 1)[0]
        if diagnostic_path:
            _write_diagnostic_once(diagnostic_path, stage)
        raise
    except Exception:
        if current_stage not in DIAGNOSTIC_TOKENS:
            current_stage = "fixture-internal"
        if diagnostic_path:
            _write_diagnostic_once(diagnostic_path, current_stage)
        raise


if __name__ == "__main__":
    main()
