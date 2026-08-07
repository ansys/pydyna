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
Driven cavity
=============

This example shows the universally famous driven-cavity case tested with
the second-order steady solver and for ``Re=1000``. The executable file
for LS-DYNA is ``ls-dyna_smp_d_R13.1_138-g8429c8a10f_winx64_ifort190.exe``.
"""

import os
import sys

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynaicfd import (
    ICFDDOF,
    Curve,
    DynaICFD,
    ICFD_AnalysisType,
    ICFD_MessageLevel,
    ICFDAnalysis,
    ICFDPart,
    ICFDVolumePart,
    MatICFD,
    MeshedVolume,
)
from ansys.dyna.core.pre.misc import check_valid_ip

# sphinx_gallery_thumbnail_path = '_static/pre/icfd/driven_cavity.png'

hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]

solution = launch_dynapre(ip=hostname)
# Import the initial mesh data(nodes and elements)
fns = []
path = examples.driven_cavity + os.sep
fns.append(path + "driven_cavity.k")
solution.open_files(fns)
icfd = DynaICFD()
solution.add(icfd)

icfdanalysis = ICFDAnalysis()
icfdanalysis.set_type(analysis_type=ICFD_AnalysisType.STEADY_STATE_ANALYSIS)
icfdanalysis.set_output(messagelevel=ICFD_MessageLevel.FULL_OUTPUT_INFORMATION, iteration_interval=250)
icfdanalysis.set_steady_state(
    max_iteration=2500, momentum_tol_limit=1e-8, pressure_tol_limit=1e-8, velocity_relax_param=1, pressure_relax_param=1
)
icfd.add(icfdanalysis)

# define model
mat = MatICFD(flow_density=1, dynamic_viscosity=0.001)

part1 = ICFDPart(1)
part1.set_material(mat)
part1.set_prescribed_velocity(dof=ICFDDOF.X, motion=Curve(x=[0, 10000], y=[1, 1]))
icfd.parts.add(part1)

part2 = ICFDPart(2)
part2.set_material(mat)
part2.set_non_slip()
icfd.parts.add(part2)

partvol = ICFDVolumePart(surfaces=[1, 2])
partvol.set_material(mat)
icfd.parts.add(partvol)
# define the volume space that will be meshed,The boundaries
# of the volume are the surfaces "spids"
meshvol = MeshedVolume(surfaces=[1, 2])
icfd.add(meshvol)

solution.create_database_binary(dt=250)
solution.save_file()
