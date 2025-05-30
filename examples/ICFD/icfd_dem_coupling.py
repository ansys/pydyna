# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
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
DEM coupling
============

This example shows how to couple the ICFD fluid solver with DEM particles.
The executable file for LS-DYNA is ``ls-dyna_smp_d_R101_winx64_ifort131.exe``.
"""

import os
import sys

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynadem import DEMAnalysis
from ansys.dyna.core.pre.dynaicfd import (
    ICFDDOF,
    Curve,
    DynaICFD,
    ICFD_CouplingForm,
    ICFDAnalysis,
    ICFDPart,
    ICFDVolumePart,
    MatICFD,
    MeshedVolume,
    Point,
    SolidFormulation,
    SolidPart,
    Velocity,
)
from ansys.dyna.core.pre.dynamaterial import MatRigidDiscrete
from ansys.dyna.core.pre.misc import check_valid_ip

# sphinx_gallery_thumbnail_path = '_static/pre/icfd/dem_coupling.png'


hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]

solution = launch_dynapre(ip=hostname)
# Import the initial mesh data(nodes and elements)
fns = []
path = examples.dem_coupling + os.sep
fns.append(path + "dem_coupling.k")
solution.open_files(fns)
solution.set_termination(termination_time=100)
icfd = DynaICFD()
solution.add(icfd)

icfd.set_timestep(tssfac=0.8)

icfdanalysis = ICFDAnalysis()
icfdanalysis.set_timestep(0.05)
icfdanalysis.set_coupling_dem(formulation=ICFD_CouplingForm.FORCE_USING_FLUID_PRESSURE_GRADIENT)
icfd.add(icfdanalysis)

demanalysis = DEMAnalysis()
demanalysis.set_des(
    normal_damping_coeff=0.9, tangential_damping_coeff=0.9, static_friction_coeff=0.3, rolling_friction_coeff=0.001
)
icfd.add(demanalysis)

# define model
mat = MatICFD(flow_density=2.0, dynamic_viscosity=0.01)

part_inflow = ICFDPart(1)
part_inflow.set_material(mat)
part_inflow.set_prescribed_velocity(dof=ICFDDOF.X, motion=Curve(x=[0, 10000], y=[1, 1]))
icfd.parts.add(part_inflow)

part_outflow = ICFDPart(2)
part_outflow.set_material(mat)
part_outflow.set_prescribed_pressure(pressure=Curve(x=[0, 10000], y=[0, 0]))
icfd.parts.add(part_outflow)

part_symmetric = ICFDPart(3)
part_symmetric.set_material(mat)
part_symmetric.set_free_slip()
icfd.parts.add(part_symmetric)

icfd.set_initial(velocity=Velocity(1, 0, 0))

partvol = ICFDVolumePart(surfaces=[1, 2, 3])
partvol.set_material(mat)
icfd.parts.add(partvol)
# define the volume space that will be meshed,The boundaries
# of the volume are the surfaces "spids"
meshvol = MeshedVolume(surfaces=[1, 2, 3])
meshvol.meshsize_box(size=0.05, min_point=Point(-1, -1, -1), max_point=Point(1, 1, 1))
icfd.add(meshvol)

# define rigid cylinder
matrigid = MatRigidDiscrete(mass_density=1000, young_modulus=1e4)
disc = SolidPart(101)
disc.set_material(matrigid)
disc.set_element_formulation(SolidFormulation.ONE_POINT_COROTATIONAL)
icfd.parts.add(disc)

solution.create_database_binary(dt=1.0)
solution.save_file()
