"""

Result Files Examples
=====================
Examples result files.
"""

import os
import inspect

_module_path = os.path.dirname(inspect.getfile(inspect.currentframe()))

# this files can be imported with from `ansys.dpf.core import examples`:
airbag_deploy = os.path.join(_module_path, "airbag","airbag_deploy")
wing = os.path.join(_module_path,"coupling","wing")
em_railgun = os.path.join(_module_path,"em", "em_railgun")
belted_dummy = os.path.join(_module_path, "explicit","belted_dummy")
icfd_cylinderflow = os.path.join(_module_path, "icfd", "icfd_cylinderflow")
iga_sample = os.path.join(_module_path, "iga", "iga_sample")
camry_rc = os.path.join(_module_path, "implicit", "camry_rc")
sale_efp = os.path.join(_module_path, "sale","sale_efp")