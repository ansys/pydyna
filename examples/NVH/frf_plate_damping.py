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
FRF for a rectangular plate
===========================

This example shows how to set up the keywords for computation of a FRF (frequency response function).
The executable file for LS-DYNA is ``ls-dyna_smp_d_R13.1_138-g8429c8a10f_winx64_ifort190.exe``.

"""

###############################################################################
# Perform required imports
# ~~~~~~~~~~~~~~~~~~~~~~~~
# Perform the required imports.
#
import os
import sys

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynamaterial import MatElastic
from ansys.dyna.core.pre.dynanvh import (
    Curve,
    DynaNVH,
    ExcitationDOF,
    FrequencyDomain,
    NodeSet,
    ResponseDOF,
    ResponseType,
    ShellFormulation,
    ShellPart,
)
from ansys.dyna.core.pre.misc import check_valid_ip

# sphinx_gallery_thumbnail_path = '_static/pre/nvh/frf_plate_damping.png'
###############################################################################
# Start the ``pre`` service
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Before starting the ``pre`` service, you must ensure that the Docker container
# for this service has been started. For more information, see "Start the Docker
# container for the ``pre`` service" in https://dyna.docs.pyansys.com/version/stable/index.html.
#
# The ``pre`` service can also be started locally, please download the latest version of
# ansys-pydyna-pre-server.zip package from https://github.com/ansys/pydyna/releases and start it
# referring to the README.rst file in this server package.
#
# Once the ``pre`` service is running, you can connect a client to it using
# the hostname and port. This example uses the default localhost and port
# (``"localhost"`` and ``"50051"`` respectively).
#
hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]
solution = launch_dynapre(ip=hostname)
###############################################################################
# Import mesh data (nodes and elements)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import initial mesh data, which includes the predefined *NODE*, *ELEMENT_* and *PART*
# cards.
#
fns = []
path = examples.nvh_frf_plate_damping + os.sep
fns.append(path + "frf_plate_damping.k")
solution.open_files(fns)

###############################################################################
# Define global control cards
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define the global control cards.

nvhobj = DynaNVH()
solution.add(nvhobj)
###############################################################################
# Set initial timestep size
# ~~~~~~~~~~~~~~~~~~~~~~~~~
# Set the initial timestep size in CONTROL_IMPLICIT_GENERAL.
#
nvhobj.implicitanalysis.set_initial_timestep_size(1.0)

###############################################################################
# Set number of eigen modes
# ~~~~~~~~~~~~~~~~~~~~~~~~~
# Set the number of eigen modes to ``100``.
#
nvhobj.implicitanalysis.set_eigenvalue(number_eigenvalues=100)

###############################################################################
# Define linear solver
# ~~~~~~~~~~~~~~~~~~~~
# Define the linear solver by setting NSOLVR to ``1`` in CONTROL_IMPLICIT_SOLUTION.
nvhobj.implicitanalysis.set_solution(solution_method=1)

###############################################################################
# Define frequency domain cards
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# *FREQUENCY_DOMAIN_FRF* is used to compute the frequency response function due to
# nodal excitations. In this case, a base velocity is defined as an input at node 131.
# The base acceleration response is measured at nodes 131 and 651. The maximum
# natural frequency employed in FRF is limited to 2000 Hz.
fd = FrequencyDomain()
crv = Curve(
    x=[1, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 200],
    y=[0, 0, 0.0045, 0.00713, 0.00386, 0.00328, 0.0034, 0.00624, 7.2e-4, 8.3e-4, 0, 0],
)
fd.set_frequency_response_function(
    excitation_input_set=NodeSet([131]),
    excitation_input_dof=ExcitationDOF.Z,
    max_natural_frequency=2000,
    modal_damping_coefficient_curve_type=1,
    modal_damping_coefficient_curve=crv,
    response_output_set=NodeSet([131, 651]),
    response_output_dof=ResponseDOF.Z,
    response_output_type=ResponseType.BASE_ACCELERATION,
    frf_output_min_frequency=1,
    frf_output_max_frequency=400,
    frf_output_num_frequency=400,
)
nvhobj.add(fd)

###############################################################################
# Define material and section properties
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define the ``MAT_ELASTIC`` material. Set shell formulation to ``SR_HUGHES_LIU``.
#
matelastic = MatElastic(mass_density=7870, young_modulus=2.07e11, poisson_ratio=0.292)

boxshell = ShellPart(1)
boxshell.set_material(matelastic)
boxshell.set_element_formulation(ShellFormulation.SR_HUGHES_LIU)
boxshell.set_thickness(0.002)
boxshell.set_shear_factor(0.833)

###############################################################################
# Set printout property and save input deck
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use the ``set_printout()`` method to set the printout property to ``3``.
# Save the input deck.
#
boxshell.set_printout(3)
nvhobj.parts.add(boxshell)

solution.save_file()
