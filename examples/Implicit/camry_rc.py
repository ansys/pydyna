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
Implicit model
==============
This example shows how to create and use an implicit dynamic roof crush model.

"""

###############################################################################
# Perform required imports
# ~~~~~~~~~~~~~~~~~~~~~~~~
# Perform the required imports.
#
import os
import sys

from camry_rc_data import (
    beamparts,
    biw,
    cnrbs,
    partswithmat180,
    partswithmat250,
    partswithmat300,
    partswithmat360,
    partswithmat400,
    partswithmat450,
    partswithmat500,
    platen,
    shellparts,
    spc,
    spotweld,
    spotweldbeams,
    spotweldsurfaces,
    vehicle,
)

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynamaterial import (
    MatModifiedPiecewiseLinearPlasticity,
    MatNull,
    MatPiecewiseLinearPlasticity,
    MatRigid,
    MatSpotweld,
)
from ansys.dyna.core.pre.dynamech import (
    DOF,
    AnalysisType,
    BeamPart,
    Contact,
    ContactCategory,
    ContactSurface,
    ContactType,
    Curve,
    DynaMech,
    NodeSet,
    OffsetType,
    PartSet,
    ShellPart,
    TimestepCtrol,
)
from ansys.dyna.core.pre.misc import check_valid_ip

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
camry_solution = launch_dynapre(ip=hostname)

###############################################################################
# Import initial mesh data
# ~~~~~~~~~~~~~~~~~~~~~~~~
# Import the initial mesh data (nodes and elements), which includes the
# vehicle data, weld data, and platen data.
#
fns = []
path = examples.camry_rc + os.sep
fns.append(path + "Camry_RC_main.k")
fns.append(path + "501_RIG_BAR_roof_crush_platen5.key")
fns.append(path + "Camry_V1_NoSusAndPowerTrain_impl7.k")
fns.append(path + "Camry_V1_NoSusAndPowerTrain_impl7_nodes.k")
fns.append(path + "roof_welds.k")
fns.append(path + "weld7.k")
fns.append(path + "xtra_sw.k")
camry_solution.open_files(fns)

###############################################################################
# Define global control cards
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Because roof crush is a quasi-static loading case, you must run this model
# as an implicit dynamic solution. Define the global control cards. From
# the ``dynasolution`` class, set the termination time and the frequency for
# the database ASCII options.
#
camry_solution.set_termination(10)

###############################################################################
# Use the implicit analysis methods in the ``dynamech`` class to define
# the IMPLICIT control cards.
#
camry = DynaMech(analysis=AnalysisType.EXPLICIT)
camry_solution.add(camry)

###############################################################################
# Set the automatic timestep control flag
# and the optimal equilibrium iteration count per timestep.
#
camry.implicitanalysis.set_initial_timestep_size(0.1)
camry.implicitanalysis.set_timestep(
    control_flag=TimestepCtrol.AUTOMATICALLY_ADJUST_TIMESTEP_SIZE,
    Optimum_equilibrium_iteration_count=511,
)

###############################################################################
# Use the ``set_dynamic()`` method to set the IMASS value to 1 and assign the
# gamma and beta values.
#
camry.implicitanalysis.set_dynamic(gamma=0.6, beta=0.38)
###############################################################################
# If normal modes must be extracted, use the ``set_eigenvalue()`` method.
# The ``set_solution()`` method defines NSOLVR as 12 (Nolinear with BFGS update).
#
camry.implicitanalysis.set_eigenvalue()
camry.implicitanalysis.set_solution(
    iteration_limit=1, stiffness_reformation_limit=50, absolute_convergence_tolerance=-100
)

###############################################################################
# Define materials
# ~~~~~~~~~~~~~~~~
# This model uses four classes of material: ``MAT_NULL``, ``MAT_RIGID,``
# ``MAT_SPOTWELD``, and ``MAT_PIECEWISE_LINEAR_PLASTICITY``. Use the ``dynamaterial``
# class to define these materials.
#
matnull = MatNull(mass_density=6e-11)
matrigid = MatRigid(mass_density=7.890e-09, young_modulus=2.100e05, poisson_ratio=0.3)
matplaten = MatRigid(
    mass_density=7.80e-09,
    young_modulus=2.00e05,
    poisson_ratio=0.3,
    center_of_mass_constraint=1,
    rotational_constraint=7,
)
spotweldharden2100 = MatSpotweld(
    mass_density=7.850e-09,
    young_modulus=2.100e05,
    poisson_ratio=0.3,
    yield_stress=510,
    plastic_hardening_modulus=2100,
)
spotweldharden2200 = MatSpotweld(
    mass_density=7.850e-09,
    young_modulus=2.100e05,
    poisson_ratio=0.3,
    yield_stress=510,
    plastic_hardening_modulus=2200,
)
windowshield = MatModifiedPiecewiseLinearPlasticity(
    mass_density=2.355e-09,
    young_modulus=7.000e04,
    poisson_ratio=0.22,
    yield_stress=30,
    tangent_modulus=1400,
    plastic_strain_to_failure=0.015,
    integration_points_number=1,
)
windowsrear = MatModifiedPiecewiseLinearPlasticity(
    mass_density=2.425e-09,
    young_modulus=7.000e04,
    poisson_ratio=0.22,
    yield_stress=30,
    tangent_modulus=1400,
    plastic_strain_to_failure=0.015,
    integration_points_number=1,
)
plastic300 = MatPiecewiseLinearPlasticity(
    mass_density=7.890e-09, young_modulus=210000, yield_stress=300, tangent_modulus=5000
)
plastic250 = MatPiecewiseLinearPlasticity(
    mass_density=7.890e-09, young_modulus=210000, yield_stress=250, tangent_modulus=5000
)
plastic360 = MatPiecewiseLinearPlasticity(
    mass_density=7.890e-09, young_modulus=210000, yield_stress=360, tangent_modulus=5000
)
plastic180 = MatPiecewiseLinearPlasticity(
    mass_density=7.850e-09, young_modulus=210000, yield_stress=180, tangent_modulus=5000
)
plastic450 = MatPiecewiseLinearPlasticity(
    mass_density=7.850e-09, young_modulus=210000, yield_stress=450, tangent_modulus=5000
)
plastic1300 = MatPiecewiseLinearPlasticity(
    mass_density=7.850e-09, young_modulus=210000, yield_stress=1300, tangent_modulus=5000
)
plastic400 = MatPiecewiseLinearPlasticity(
    mass_density=7.890e-09, young_modulus=210000, yield_stress=400, tangent_modulus=5000
)
plastic500 = MatPiecewiseLinearPlasticity(
    mass_density=7.890e-09, young_modulus=210000, yield_stress=500, tangent_modulus=5000
)
plastic675 = MatPiecewiseLinearPlasticity(
    mass_density=7.850e-09, young_modulus=210000, yield_stress=675, tangent_modulus=5000
)
plastic310 = MatPiecewiseLinearPlasticity(
    mass_density=2.255e-09, young_modulus=70000, yield_stress=310, tangent_modulus=5000
)
plastic220 = MatPiecewiseLinearPlasticity(
    mass_density=7.890e-09, young_modulus=210000, yield_stress=220, tangent_modulus=5000
)
plastic220_410 = MatPiecewiseLinearPlasticity(
    mass_density=7.890e-09, young_modulus=210000, yield_stress=220, tangent_modulus=410
)

###############################################################################
# Assign section and material properties
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Once all materials are explicitly defined, these material IDs must be cross-referenced
# in the *PART* card. You use the ``set_material()`` method for this. Because many parts
# share common materials, the assignment happens within a loop. Within the loop, the
# ``set_element_formulation()`` method is called to assign the elform for the beam and
# shell elements. Accordingly, either the beam diameter or the shell thickness is also
# defined. To identify the part ID that has a particular material type, a predefined
# list is made available in the ``camry_rc_data.py`` file, which this script reads.
#
for bpart in beamparts:
    part = BeamPart(bpart[0])
    if part.id in [50000002]:
        part.set_material(spotweldharden2200)
    else:
        part.set_material(spotweldharden2100)
    part.set_element_formulation(bpart[1])
    part.set_diameter(bpart[2])
    camry.parts.add(part)

for spart in shellparts:
    part = ShellPart(spart[0])
    if part.id in [1463, 1464]:
        part.set_material(matnull)
    elif part.id in [417, 419, 530, 532, 585, 586, 587, 588]:
        part.set_material(matrigid)
    elif part.id in [50000001]:
        part.set_material(matplaten)
    elif part.id in [290]:
        part.set_material(windowshield)
    elif part.id in [291]:
        part.set_material(windowsrear)
    elif part.id in partswithmat300:
        part.set_material(plastic300)
    elif part.id in partswithmat250:
        part.set_material(plastic250)
    elif part.id in partswithmat360:
        part.set_material(plastic360)
    elif part.id in partswithmat180:
        part.set_material(plastic180)
    elif part.id in partswithmat450:
        part.set_material(plastic450)
    elif part.id in partswithmat400:
        part.set_material(plastic400)
    elif part.id in partswithmat500:
        part.set_material(plastic500)
    elif part.id in [57]:
        part.set_material(plastic1300)
    elif part.id in [286]:
        part.set_material(plastic675)
    elif part.id in [5000000]:
        part.set_material(plastic310)
    elif part.id in [5000006]:
        part.set_material(plastic220)
    elif part.id in [5000007]:
        part.set_material(plastic220_410)
    else:
        pass
    part.set_element_formulation(spart[1])
    part.set_thickness(spart[2])
    camry.parts.add(part)

###############################################################################
# Generate keywords for spotwelds and nodal rigid bodies
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The ``camry_rc_data.py`` file contains the predefined node pairs and node sets
# required for the *CONSTRAINED_SPOTWELD* and *CONSTRAINED_NODAL_RIGID_BODY*
# definitions. Loop through these lists to generate the appropriate keywords.
#
for sw in spotweld:
    camry.constraints.create_spotweld(nodeid1=sw[0], nodeid2=sw[1])

for cnrb in cnrbs:
    camry.constraints.create_cnrb(nodeset=NodeSet(cnrb))

###############################################################################
# Define contacts
# ~~~~~~~~~~~~~~~
# There are three contacts defined in this model:
#
# - Automatic single surface contact for the BIW self contact
# - Surface-to-surface contact between the platen and the BIW self contact
# - Tied contact for the spotweld beams
#
# Use the ``ContactSurface()`` method to set the SSTYPE and MSTYPE.
# The ``PartSet()`` method accepts the name of a list and converts it to
# *PART_SET_LIST*. Notice how the contact type and category can be used
# to create the three different type of contacts for this model.
#
selfcontact = Contact(type=ContactType.AUTOMATIC)
selfcontact.set_mortar()
selfcontact.set_friction_coefficient(static=0.2)
surf1 = ContactSurface(PartSet(vehicle))
selfcontact.set_slave_surface(surf1)
camry.contacts.add(selfcontact)

platebiw = Contact(type=ContactType.AUTOMATIC, category=ContactCategory.SURFACE_TO_SURFACE_CONTACT)
platebiw.set_mortar()
platebiw.set_friction_coefficient(static=0.2, dynamic=0.2)
surf1 = ContactSurface(PartSet(biw))
surf2 = ContactSurface(PartSet(platen))
platebiw.set_slave_surface(surf1)
platebiw.set_master_surface(surf2)
camry.contacts.add(platebiw)

swcontact = Contact(
    type=ContactType.TIED,
    category=ContactCategory.SHELL_EDGE_TO_SURFACE_CONTACT,
    offset=OffsetType.CONSTRAINED_OFFSET,
)
spotweldbeam = ContactSurface(PartSet(spotweldbeams))
spotweldbeam.set_contact_thickness(thickness=-0.9)
spotweldsurface = ContactSurface(PartSet(spotweldsurfaces))
spotweldsurface.set_contact_thickness(thickness=-0.9)
swcontact.set_slave_surface(spotweldbeam)
swcontact.set_master_surface(spotweldsurface)
camry.contacts.add(swcontact)

###############################################################################
# Define SPCs
# ~~~~~~~~~~~
# You can use the ``boundaryconditions`` class to define both SPCs and
# prescribed motions. The bottom of the BIW is SPCed by selecting a few nodes.
# The prescribed motion is then assigned to the platen.
camry.boundaryconditions.create_spc(NodeSet(spc))

crv = Curve(
    x=[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 9.77, 100],
    y=[0, 13, 26, 39, 52, 65, 78, 91, 104, 117, 127, 127],
)
platen = PartSet([50000001])
camry.boundaryconditions.create_imposed_motion(platen, crv, dof=DOF.X_TRANSLATIONAL, scalefactor=-0.0802216)
camry.boundaryconditions.create_imposed_motion(platen, crv, dof=DOF.Y_TRANSLATIONAL, scalefactor=-0.0802216)
camry.boundaryconditions.create_imposed_motion(platen, crv, dof=DOF.Z_TRANSLATIONAL, scalefactor=-0.0802216)

###############################################################################
# Define database cards and save input file
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define the frequency of output for the binary and ASCII database outputs
# and save the input file.
#
camry_solution.create_database_binary(dt=0.001)
camry_solution.set_output_database(
    elout=0.0001,
    glstat=0.0001,
    matsum=0.0001,
    nodout=0.0001,
    rbdout=0.0001,
    rcforc=0.0001,
    secforc=0.0001,
)

camry_solution.save_file()
