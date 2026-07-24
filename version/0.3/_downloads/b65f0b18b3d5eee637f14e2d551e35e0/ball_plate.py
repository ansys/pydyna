"""
Ball Plate
==========
This example shows how to use the PyDYNA ``pre`` service to create
a ball plate model. The executable file for LS-DYNA is
``ls-dyna_smp_d_R13.0_365-gf8a97bda2a_winx64_ifort190.exe``.

"""
###############################################################################
# Perform required imports
# ~~~~~~~~~~~~~~~~~~~~~~~~
# Peform required imports.
import os
import sys


from ansys.dyna.core.pre.dynasolution import DynaSolution
from ansys.dyna.core.pre.dynamech import (
    DynaMech,
    Velocity,
    PartSet,
    ShellPart,
    SolidPart,
    NodeSet,
    Contact,
    ContactSurface,
    ShellFormulation,
    SolidFormulation,
    ContactType,
    AnalysisType
)
from ansys.dyna.core.pre.dynamaterial import (
    MatRigid,
    MatPiecewiseLinearPlasticity,
)
from ansys.dyna.core.pre import examples
# sphinx_gallery_thumbnail_path = '_static/pre/explicit/ball_plate.png'

###############################################################################
# Start the ``pre`` service
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Prerequisite: make sure the ``pre`` service docker have been started.
# Refer to: https://dyna.docs.pyansys.com/version/stable/getting-started/index.html

# Once the ``pre`` servic is running, you can connect a client to it using
# the host name and the port. This example uses the default local host and port
# (``"localhost"`` and ``"50051"`` respectively).
#
hostname = "localhost"
if len(sys.argv) > 1:
    hostname = sys.argv[1]
solution = DynaSolution(hostname)

###############################################################################
# Start the solution workflow
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NODES and ELEMENTS are read in from the ``ball_plate.k`` file. This file
# also has the *PART* defined in it, but the section and material fields are
# empty to begin with.
#
fns = []
path = examples.ball_plate + os.sep
fns.append(path+"ball_plate.k")
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
# The ``dynamaterials`` class
# are used to define these materials: ``MAT_RIGID``, ``MAT_PIECEWISE_LINEAR_PLASTICITY``,

matrigid = MatRigid(mass_density=7.83e-6, young_modulus=207, poisson_ratio=0.3)
matplastic = MatPiecewiseLinearPlasticity(mass_density=7.83e-6, young_modulus=207, yield_stress=0.2, tangent_modulus=2)
 

###############################################################################
# Define section properties and assign materials
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Now that you have materials with the material ID corresponding to
# the Part ID, you can assign these materials to the
# parts. also define the section properties, element
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
# Define single-surface contact between predefined part set.

selfcontact = Contact(type=ContactType.AUTOMATIC)
surf1 = ContactSurface(PartSet([1, 2]))
selfcontact.set_slave_surface(surf1)
ballplate.contacts.add(selfcontact)

###############################################################################
# Define nodal single point constraints.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Constrain the nodes in the spc list

spc = [34,35,51,52,68,69,85,86,102,103,119,120,136,137,153,154,170,171,187,188,204,205,221,222,238,239,255,256]
for i in range(1,19):
    spc.append(i)
for i in range(272,290):
    spc.append(i)
ballplate.boundaryconditions.create_spc(NodeSet(spc),rx=False,ry=False,rz=False)

###############################################################################
# Define initial condition.
# ~~~~~~~~~~~~~~~~~~~~~~~~~
# Use the ``create_velocity_node`` method 
# to initialize the velocity components in the desired direction.
for i in range(1,1652):
    ballplate.initialconditions.create_velocity_node(i,trans=Velocity(0, 0, -10))

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
# Download output file from service docker to local <working directory>/output/.

serveroutfile = '/'.join((serverpath,"ball_plate.k"))
downloadpath = os.path.join(os.getcwd(), "output")
if not os.path.exists(downloadpath):
    os.makedirs(downloadpath)
downloadfile = os.path.join(downloadpath,"ball_plate.k")
solution.download(serveroutfile,downloadfile)
