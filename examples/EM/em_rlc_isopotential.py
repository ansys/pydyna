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
RLC circuit by isopotential
===========================
This example shows how to use the corresponding ``contype`` in the
``connect_isopotential()`` method to define an RLC circuit as an
inlet boundary condition. The executable file for LS-DYNA is
``ls-dyna_smp_d_R13.1_138-g8429c8a10f_winx64_ifort190.exe``.

"""

import os
import sys

from em_set_data import rlc_rogoseg

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynaem import (
    DynaEM,
    EMType,
    Isopotential,
    Isopotential_ConnType,
    NodeSet,
    RogoCoil,
    SegmentSet,
    SolidFormulation,
    SolidPart,
)
from ansys.dyna.core.pre.dynamaterial import EMMATTYPE, MatRigid
from ansys.dyna.core.pre.misc import check_valid_ip

# sphinx_gallery_thumbnail_path = '_static/pre/em/rlc_isopotential.png'

hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]

solution = launch_dynapre(ip=hostname)
fns = []
path = examples.em_rlc_isopotential + os.sep
fns.append(path + "em_rlc_isopotential.k")
solution.open_files(fns)
solution.set_termination(termination_time=0.01)
solution.create_database_binary(dt=1e-4)

emobj = DynaEM()
solution.add(emobj)

emobj.set_timestep(tssfac=1, timestep_size_for_mass_scaled=1e-4)

emobj.analysis.set_timestep(timestep=1e-4)
emobj.analysis.set_em_solver(type=EMType.RESISTIVE_HEATING)

matrigid = MatRigid(
    mass_density=7000,
    young_modulus=2e11,
    center_of_mass_constraint=1,
    translational_constraint=7,
    rotational_constraint=7,
)
matrigid.set_em_permeability_equal(material_type=EMMATTYPE.CONDUCTOR, initial_conductivity=1e4)

part1 = SolidPart(1)
part1.set_material(matrigid)
part1.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
emobj.parts.add(part1)

nset1 = NodeSet(
    [
        429,
        433,
        437,
        441,
        445,
        449,
        453,
        457,
        461,
        465,
        469,
        473,
        477,
        481,
        485,
        489,
        493,
        497,
        501,
        505,
        509,
        513,
        517,
        521,
        525,
    ]
)
nset2 = NodeSet(
    [26, 31, 36, 41, 46, 51, 56, 61, 66, 71, 76, 81, 86, 91, 96, 101, 106, 111, 116, 121, 126, 131, 136, 141, 146]
)
isopos_conn1 = Isopotential(nset1)
isopos_conn2 = Isopotential(nset2)
emobj.connect_isopotential(
    contype=Isopotential_ConnType.RLC_CIRCUIT,
    isopotential1=isopos_conn1,
    value=5e-4,
    inductance=7.8e-5,
    capacity=0.0363,
    initial_voltage=5000,
)
emobj.connect_isopotential(contype=Isopotential_ConnType.VOLTAGE_SOURCE, isopotential1=isopos_conn2)
emobj.add(RogoCoil(SegmentSet(rlc_rogoseg)))

emobj.create_em_output(mats=2, matf=2, sols=2, solf=2)

solution.save_file()
