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

"""
Ball plate
==========
This example shows how to use the PyDYNA ``pre`` service to create
a ball plate model. The executable file for LS-DYNA is
``ls-dyna_smp_d_R13.0_365-gf8a97bda2a_winx64_ifort190.exe``.

"""

###############################################################################
# Perform required imports
# ~~~~~~~~~~~~~~~~~~~~~~~~
# Perform the required imports.
import os
import sys

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynamaterial import MatPiecewiseLinearPlasticity, MatRigid
from ansys.dyna.core.pre.dynamech import (
    AnalysisType,
    Contact,
    ContactSurface,
    ContactType,
    DynaMech,
    NodeSet,
    PartSet,
    ShellFormulation,
    ShellPart,
    SolidFormulation,
    SolidPart,
    Velocity,
)
from ansys.dyna.core.pre.misc import check_valid_ip

# sphinx_gallery_thumbnail_path = '_static/pre/explicit/ball_plate.png'

###############################################################################
# Start the ``pre`` service
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Before starting the ``pre`` service, you must ensure that the Docker container
# for this service has been started. For more information, see "Start the Docker
# container for the ``pre`` service" in https://dyna.docs.pyansys.com/version/stable/index.html.
#
# The ``pre`` service can also be started locally, please download the latest version of
# ansys-pydyna-pre-server.zip package from https://github.com/ansys/pydyna/releases and start it
# refefring to the README.rst file in this server package.
#
# Once the ``pre`` service is running, you can connect a client to it using
# the host name and port. This code uses the default localhost and port
# (``"localhost"`` and ``"50051"`` respectively).
#
hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]
solution = launch_dynapre(ip=hostname)

###############################################################################
# Start the solution workflow
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NODES and ELEMENTS are read in from the ``ball_plate.k`` file. This file
# also has the *PART* defined in it, but the section and material fields are
# empty to begin with.
#
fns = []
path = examples.ball_plate + os.sep
fns.append(path + "ball_plate.k")
solution.open_files(fns)

###############################################################################
# Create database and control cards
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# For the D3plots, set simulation termination time, simulation timestep, and
# output frequency.

solution.set_termination(termination_time=10)

ballplate = DynaMech(AnalysisType.NONE)
solution.add(ballplate)

###############################################################################
# Define materials
# ~~~~~~~~~~~~~~~~
# The ``dynamaterials`` class is used to define these materials: ``MAT_RIGID``,
# ``MAT_PIECEWISE_LINEAR_PLASTICITY``,

matrigid = MatRigid(mass_density=7.83e-6, young_modulus=207, poisson_ratio=0.3)
matplastic = MatPiecewiseLinearPlasticity(mass_density=7.83e-6, young_modulus=207, yield_stress=0.2, tangent_modulus=2)


###############################################################################
# Define section properties and assign materials
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Now that you have materials with the material ID corresponding to
# the Part ID, you can assign these materials to the
# parts. You can also define section properties, element
# formulations, and constraints.
#

plate = ShellPart(1)
plate.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
plate.set_material(matplastic)
plate.set_thickness(1)
plate.set_integration_points(5)
ballplate.parts.add(plate)

ball = SolidPart(2)
ball.set_material(matrigid)
ball.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
ballplate.parts.add(ball)


###############################################################################
# Define surface-to-surface contacts
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define a single-surface contact between a predefined part set.

selfcontact = Contact(type=ContactType.AUTOMATIC)
surf1 = ContactSurface(PartSet([1, 2]))
selfcontact.set_slave_surface(surf1)
ballplate.contacts.add(selfcontact)

###############################################################################
# Define nodal single point constraints.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Constrain the nodes in a list of single point constraints (spc).

spc = [
    34,
    35,
    51,
    52,
    68,
    69,
    85,
    86,
    102,
    103,
    119,
    120,
    136,
    137,
    153,
    154,
    170,
    171,
    187,
    188,
    204,
    205,
    221,
    222,
    238,
    239,
    255,
    256,
]
for i in range(1, 19):
    spc.append(i)
for i in range(272, 290):
    spc.append(i)
ballplate.boundaryconditions.create_spc(NodeSet(spc), rx=False, ry=False, rz=False)

###############################################################################
# Define initial condition.
# ~~~~~~~~~~~~~~~~~~~~~~~~~
# Use the ``create_velocity_node`` method
# to initialize the velocity components in the desired direction.
for i in range(1, 1652):
    ballplate.initialconditions.create_velocity_node(i, trans=Velocity(0, 0, -10))

###############################################################################
# Define database outputs
# ~~~~~~~~~~~~~~~~~~~~~~~
# Define the frequency for the D3PLOT file and write out the input file.
#
solution.set_output_database(glstat=0.1, matsum=0.1, sleout=0.1)
solution.create_database_binary(dt=1)
serverpath = solution.save_file()

###############################################################################
# Download output file
# ~~~~~~~~~~~~~~~~~~~~
# Download output file from Docker image for the server to
# your local ``<working directory>/output/`` location.

serveroutfile = "/".join((serverpath, "ball_plate.k"))
downloadpath = os.path.join(os.getcwd(), "output")
if not os.path.exists(downloadpath):
    os.makedirs(downloadpath)
downloadfile = os.path.join(downloadpath, "ball_plate.k")
solution.download(serveroutfile, downloadfile)
