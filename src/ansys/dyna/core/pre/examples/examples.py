# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

"""Examples result files."""

import inspect
import os

_module_path = os.path.dirname(inspect.getfile(inspect.currentframe()))

# this files can be imported with from `ansys.dpf.core import examples`:
airbag_deploy = os.path.join(_module_path, "airbag", "airbag_deploy")
# EM
em_railgun = os.path.join(_module_path, "em", "em_railgun")
em_resistive_heating = os.path.join(_module_path, "em", "em_resistive_heating")
em_resistive_heating_2d = os.path.join(_module_path, "em", "em_resistive_heating_2d")
em_resistive_heating_2d_isopots = os.path.join(_module_path, "em", "em_resistive_heating_2d_isopots")
em_resistive_heating_2d_multi_isopots = os.path.join(_module_path, "em", "em_resistive_heating_2d_multi_isopots")
em_rlc_isopotential = os.path.join(_module_path, "em", "em_rlc_isopotential")
em_rlc_define_func = os.path.join(_module_path, "em", "em_rlc_define_func")

belted_dummy = os.path.join(_module_path, "explicit", "belted_dummy")
ball_plate = os.path.join(_module_path, "explicit", "ball_plate")
# ICFD
cylinder_flow = os.path.join(_module_path, "icfd", "cylinder_flow")
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
mesh_morphing = os.path.join(_module_path, "icfd", "mesh_morphing")
dem_coupling = os.path.join(_module_path, "icfd", "dem_coupling")
sloshing = os.path.join(_module_path, "icfd", "sloshing")

camry_rc = os.path.join(_module_path, "implicit", "camry_rc")
sale_efp = os.path.join(_module_path, "sale", "sale_efp")
isph_rigidtest = os.path.join(_module_path, "isph", "rigidtest")
# NVH
nvh_frf_plate_damping = os.path.join(_module_path, "nvh", "frf_plate_damping")
nvh_frf_solid = os.path.join(_module_path, "nvh", "frf_solid")

thermal_stress = os.path.join(_module_path, "thermal", "thermal_stress")
