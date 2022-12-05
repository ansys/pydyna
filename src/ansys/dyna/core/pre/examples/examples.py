"""

Result Files Examples
=====================
Examples result files.
"""

import inspect
import os

_module_path = os.path.dirname(inspect.getfile(inspect.currentframe()))

# this files can be imported with from `ansys.dpf.core import examples`:
airbag_deploy = os.path.join(_module_path, "airbag", "airbag_deploy")
wing = os.path.join(_module_path, "coupling", "wing")
em_railgun = os.path.join(_module_path, "em", "em_railgun")
belted_dummy = os.path.join(_module_path, "explicit", "belted_dummy")
icfd_cylinderflow = os.path.join(_module_path, "icfd", "icfd_cylinderflow")
iga_sample = os.path.join(_module_path, "iga", "iga_sample")
camry_rc = os.path.join(_module_path, "implicit", "camry_rc")
sale_efp = os.path.join(_module_path, "sale", "sale_efp")
isph_rigidtest = os.path.join(_module_path, "isph", "rigidtest")
nvh_frf_plate_damping = os.path.join(_module_path, "nvh", "frf_plate_damping")
thermal_stress = os.path.join(_module_path, "thermal", "thermal_stress")
