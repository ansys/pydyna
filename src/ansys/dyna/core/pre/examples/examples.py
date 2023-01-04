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
em_railgun = os.path.join(_module_path, "em", "em_railgun")
belted_dummy = os.path.join(_module_path, "explicit", "belted_dummy")
# ICFD
icfd_cylinderflow = os.path.join(_module_path, "icfd", "icfd_cylinderflow")
internal_3d_flow = os.path.join(_module_path, "icfd", "internal_3d_flow")
plate_flow = os.path.join(_module_path, "icfd", "plate_flow")
mesh_size = os.path.join(_module_path, "icfd", "mesh_size")
thermal_flow = os.path.join(_module_path, "icfd", "thermal_flow")
free_convection_flow = os.path.join(_module_path, "icfd", "free_convection_flow")
dam_break = os.path.join(_module_path, "icfd", "dam_break")
driven_cavity = os.path.join(_module_path, "icfd", "driven_cavity")
weak_fsi = os.path.join(_module_path, "icfd", "weak_fsi")
strong_fsi = os.path.join(_module_path, "icfd", "strong_fsi")
imposed_move = os.path.join(_module_path, "icfd", "imposed_move")
mesh_adaptivity = os.path.join(_module_path, "icfd", "mesh_adaptivity")
dem_coupling = os.path.join(_module_path, "icfd", "dem_coupling")

iga_sample = os.path.join(_module_path, "iga", "iga_sample")
camry_rc = os.path.join(_module_path, "implicit", "camry_rc")
sale_efp = os.path.join(_module_path, "sale", "sale_efp")
isph_rigidtest = os.path.join(_module_path, "isph", "rigidtest")
nvh_frf_plate_damping = os.path.join(_module_path, "nvh", "frf_plate_damping")
thermal_stress = os.path.join(_module_path, "thermal", "thermal_stress")
