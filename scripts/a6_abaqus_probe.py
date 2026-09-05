"""A6.1 Abaqus/CAE noGUI runtime probe.

This is deliberately not a production helper.  It creates no model, geometry,
mesh, job, or project artifact; the trusted caller runs it in a temporary
directory and captures only the fixed marker below.
"""

from __future__ import print_function


print("A6.1_ABAQUS_CAE_PROBE_PASSED")
