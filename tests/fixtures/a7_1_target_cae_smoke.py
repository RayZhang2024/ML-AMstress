import os


SENTINEL = b"A7.1_ISOLATED_TARGET_CAE_SMOKE_PASSED"


sentinel_path = os.environ.get("A7_TARGET_SENTINEL_FILE")
if not sentinel_path:
    raise SystemExit(1)

with open(sentinel_path, "wb") as sentinel_file:
    sentinel_file.write(SENTINEL)
