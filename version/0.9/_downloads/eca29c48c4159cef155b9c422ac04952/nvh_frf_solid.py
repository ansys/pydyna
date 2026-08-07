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
FRF for a colume model with a hole
==================================

This example shows a column with a hole using solid elements.
The executable file for LS-DYNA is ``ls-dyna_smp_d_R13.1_138-g8429c8a10f_winx64_ifort190.exe``.

"""

import os
import sys

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynamaterial import MatPiecewiseLinearPlasticity
from ansys.dyna.core.pre.dynanvh import (
    DynaNVH,
    EnergyFlag,
    ExcitationDOF,
    ExcitationType,
    FrequencyDomain,
    NodeSet,
    OutputEcho,
    ResponseDOF,
    ResponseType,
    SolidFormulation,
    SolidPart,
)
from ansys.dyna.core.pre.misc import check_valid_ip

# sphinx_gallery_thumbnail_path = '_static/pre/nvh/frf_column_hole.png'

hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]
solution = launch_dynapre(ip=hostname)
fns = []
path = examples.nvh_frf_solid + os.sep
fns.append(path + "frf_solid.k")
solution.open_files(fns)
solution.set_termination(termination_time=1)

nvhobj = DynaNVH()
solution.add(nvhobj)

nvhobj.set_energy(hourglass_energy=EnergyFlag.COMPUTED)
nvhobj.set_output(print_suppression_d3hsp=True, print_suppression_echo=OutputEcho.SUPPRESSED_NODAL_AND_ELEMENT_PRINTING)

nvhobj.implicitanalysis.set_initial_timestep_size(1.0)
nvhobj.implicitanalysis.set_eigenvalue(number_eigenvalues=100)
nvhobj.implicitanalysis.set_solution(solution_method=1)

fd = FrequencyDomain()

outputset = [
    290,
    292,
    294,
    296,
    298,
    300,
    302,
    304,
    306,
    380,
    382,
    384,
    386,
    388,
    390,
    482,
    484,
    486,
    488,
    490,
    492,
    578,
    580,
    582,
    638,
    640,
    706,
    708,
]
fd.set_frequency_response_function(
    excitation_input_dof=ExcitationDOF.X,
    excitation_input_type=ExcitationType.BASE_ACCELERATION,
    max_natural_frequency=20,
    modal_damping_coefficient=0.01,
    response_output_set=NodeSet(outputset),
    response_output_dof=ResponseDOF.X,
    response_output_type=ResponseType.BASE_ACCELERATION,
    frf_output_min_frequency=0.01,
    frf_output_max_frequency=10,
    frf_output_num_frequency=1000,
)
nvhobj.add(fd)

plastic1 = MatPiecewiseLinearPlasticity(
    mass_density=4.99e-07, young_modulus=11.37, poisson_ratio=0.32, yield_stress=0.0468
)
plastic2 = MatPiecewiseLinearPlasticity(
    mass_density=4.99e-07, young_modulus=110.37, poisson_ratio=0.32, yield_stress=0.0468
)

lower = SolidPart(4)
lower.set_material(plastic1)
lower.set_element_formulation(SolidFormulation.IMPLICIT_9_POINT_ENHANCED_STRAIN)
nvhobj.parts.add(lower)

upper = SolidPart(5)
upper.set_material(plastic2)
upper.set_element_formulation(SolidFormulation.IMPLICIT_9_POINT_ENHANCED_STRAIN)
nvhobj.parts.add(upper)

spc = [
    163,
    166,
    169,
    172,
    175,
    178,
    181,
    184,
    187,
    307,
    310,
    313,
    316,
    319,
    322,
    391,
    394,
    397,
    400,
    403,
    406,
    493,
    496,
    499,
    589,
    592,
    645,
    648,
]
nvhobj.boundaryconditions.create_spc(NodeSet(spc))

solution.create_database_binary(dt=0.1)
solution.set_output_database(
    glstat=0.1,
    matsum=0.1,
)
solution.save_file()
