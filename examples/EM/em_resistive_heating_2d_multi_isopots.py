# Copyright (C) 2021 - 2025 ANSYS, Inc. and/or its affiliates.
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

"""
Resistive heating 2D multiple-connect isopotential
==================================================
This example shows how to define complex circuits as inlet boundary conditions.
The executable file for LS-DYNA is ``ls-dyna_smp_d_R13.1_138-g8429c8a10f_winx64_ifort190.exe``.

"""

import os
import sys

from em_set_data import rogoseg

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynaem import (
    Curve,
    DynaEM,
    EMDimension,
    EMType,
    Isopotential,
    Isopotential_ConnType,
    NodeSet,
    PartSet,
    RogoCoil,
    SegmentSet,
    ShellFormulation,
    ShellPart,
    ThermalAnalysis,
    ThermalAnalysisType,
)
from ansys.dyna.core.pre.dynamaterial import EMMATTYPE, MatRigid, MatThermalIsotropic
from ansys.dyna.core.pre.misc import check_valid_ip

# sphinx_gallery_thumbnail_path = '_static/pre/em/resistive_heating_2d_isopots.png'

hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]

solution = launch_dynapre(ip=hostname)
fns = []
path = examples.em_resistive_heating_2d_multi_isopots + os.sep
fns.append(path + "em_resistive_heating_2d_multi_isopots.k")
solution.open_files(fns)
solution.set_termination(termination_time=0.0101)
solution.create_database_binary(dt=1e-4)

emobj = DynaEM()
solution.add(emobj)

emobj.set_timestep(tssfac=1, timestep_size_for_mass_scaled=1e-4)

emobj.analysis.set_timestep(timestep=1e-4)
emobj.analysis.set_em_solver(type=EMType.RESISTIVE_HEATING, dimtype=EMDimension.PLANAR_2D)

tanalysis = ThermalAnalysis()
tanalysis.set_timestep(initial_timestep=1e-4)
tanalysis.set_solver(analysis_type=ThermalAnalysisType.TRANSIENT)
emobj.add(tanalysis)

matrigid = MatRigid(mass_density=1, young_modulus=2e11)
matrigid.set_em_resistive_heating_2d(material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=1e4)

matthermaliso = MatThermalIsotropic(density=100, specific_heat=10, conductivity=7)

part = ShellPart(1)
part.set_material(matrigid, matthermaliso)
part.set_element_formulation(ShellFormulation.PLANE_STRESS)
emobj.parts.add(part)

emobj.boundaryconditions.create_imposed_motion(PartSet([1]), Curve(x=[0, 10], y=[10, 10]))
emobj.set_init_temperature(temp=25)

crv = Curve(func="-5./0.01*EXP(-TIME/((5.e-4+0.05+0.01)*0.04))")
nset1 = NodeSet([521, 517, 513, 509, 525])
nset2 = NodeSet([549, 548, 577, 597, 617])
nset3 = NodeSet([653, 644, 626, 627, 662])
nset4 = NodeSet([642, 652, 661, 670, 643])
emobj.connect_isopotential(
    contype=Isopotential_ConnType.CURRENT_SOURCE,
    isopotential1=Isopotential(nset4),
    isopotential2=Isopotential(nset2),
    curve=crv,
)
emobj.connect_isopotential(
    contype=Isopotential_ConnType.RESISTANCE,
    isopotential1=Isopotential(nset4),
    isopotential2=Isopotential(nset2),
    value=0.01,
)
emobj.connect_isopotential(
    contype=Isopotential_ConnType.RESISTANCE,
    isopotential1=Isopotential(nset3),
    isopotential2=Isopotential(nset1),
    value=0.05,
)
emobj.add(RogoCoil(SegmentSet(rogoseg)))

emobj.create_em_output(mats=2, matf=2, sols=2, solf=2)

solution.save_file()
