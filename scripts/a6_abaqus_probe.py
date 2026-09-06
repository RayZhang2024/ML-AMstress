"""A6.1 Abaqus/CAE noGUI runtime probe.

This is deliberately not a production helper.  It creates no model, geometry,
mesh, job, or project artifact; the trusted caller runs it in a temporary
directory and captures only the fixed marker below.
"""

from __future__ import print_function

import os


MARKER = "A6.1_ABAQUS_CAE_PROBE_PASSED"
marker_path = os.environ.get("A6_PROBE_MARKER_FILE")
if not marker_path:
    raise RuntimeError("A6.1 probe marker path is unavailable")
with open(marker_path, "wb") as marker_file:
    marker_file.write(MARKER.encode("ascii"))
