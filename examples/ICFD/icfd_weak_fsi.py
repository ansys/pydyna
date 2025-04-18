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
Weak FSI
========

This example shows a simple FSI (fluid-structure interaction) coupling problem
using weak/loose/explicit FSI. The executable file for LS-DYNA is
``ls-dyna_smp_d_R13.1_138-g8429c8a10f_winx64_ifort190.exe``.
"""

import os
import sys

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynaicfd import (
    DOF,
    ICFDDOF,
    Curve,
    DynaICFD,
    ICFDAnalysis,
    ICFDPart,
    ICFDVolumePart,
    MatICFD,
    MeshedVolume,
    Motion,
    PartSet,
    ShellFormulation,
    ShellPart,
)
from ansys.dyna.core.pre.dynamaterial import MatRigid
from ansys.dyna.core.pre.misc import check_valid_ip

# sphinx_gallery_thumbnail_path = '_static/pre/icfd/weak_fsi.png'

hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]

solution = launch_dynapre(ip=hostname)
# Import the initial mesh data(nodes and elements)
fns = []
path = examples.weak_fsi + os.sep
fns.append(path + "weak_fsi.k")
solution.open_files(fns)
solution.set_termination(termination_time=40)
icfd = DynaICFD()
solution.add(icfd)

icfd.set_timestep(tssfac=0.9, max_timestep=Curve(x=[0, 10000], y=[0.05, 0.05]))

icfdanalysis = ICFDAnalysis()
icfdanalysis.set_timestep(0.05)
icfdanalysis.set_fsi()
icfd.add(icfdanalysis)

# define model
mat = MatICFD(flow_density=1.0, dynamic_viscosity=0.005)

part_inflow = ICFDPart(1)
part_inflow.set_material(mat)
part_inflow.set_prescribed_velocity(dof=ICFDDOF.X, motion=Curve(x=[0, 5, 6, 10000], y=[0, 0, 1, 1]))
part_inflow.set_prescribed_velocity(dof=ICFDDOF.Y, motion=Curve(x=[0, 10000], y=[0, 0]))
icfd.parts.add(part_inflow)

part_outflow = ICFDPart(2)
part_outflow.set_material(mat)
part_outflow.set_prescribed_pressure(pressure=Curve(x=[0, 10000], y=[0, 0]))
icfd.parts.add(part_outflow)

part_symmetric = ICFDPart(3)
part_symmetric.set_material(mat)
part_symmetric.set_free_slip()
icfd.parts.add(part_symmetric)

part_wall = ICFDPart(4)
part_wall.set_material(mat)
part_wall.set_non_slip()
part_wall.set_fsi()
part_wall.compute_drag_force()
part_wall.set_boundary_layer(number=3)
icfd.parts.add(part_wall)

partvol = ICFDVolumePart(surfaces=[1, 2, 3, 4])
partvol.set_material(mat)
icfd.parts.add(partvol)
# define the volume space that will be meshed,The boundaries
# of the volume are the surfaces "spids"
meshvol = MeshedVolume(surfaces=[1, 2, 3, 4])
icfd.add(meshvol)

# define rigid cylinder
matrigid = MatRigid(mass_density=1000, young_modulus=2e11, poisson_ratio=0.3)
cylinder = ShellPart(1)
cylinder.set_material(matrigid)
cylinder.set_element_formulation(ShellFormulation.PLANE_STRESS)
icfd.parts.add(cylinder)
# Define boundary conddition
icfd.boundaryconditions.create_imposed_motion(
    PartSet([1]), Curve(func="2*3.14/10*sin(2*3.14/10*TIME+3.14/2)"), dof=DOF.Y_TRANSLATIONAL, motion=Motion.VELOCITY
)

solution.create_database_binary(dt=0.2)
solution.save_file()
