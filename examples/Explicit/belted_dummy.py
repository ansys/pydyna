"""
Belted dummy
============
This example shows how to use the PyDYNA ``pre`` service to create
a belted dummy model. The executable file for LS-DYNA is
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
    Curve,
    ShellPart,
    DiscretePart,
    NodeSet,
    SegmentSet,
    DRO,
    Contact,
    ContactSurface,
    ContactCategory,
    Motion,
    Gravity,
    GravityOption,
    ShellFormulation,
)
from ansys.dyna.core.pre.dynamaterial import (
    MatRigid,
    MatElastic,
    MatSpringNonlinearElastic,
    MatDamperViscous,
    MatDamperNonlinearViscous,
)
from belted_dummy_data import *
from ansys.dyna.core.pre import examples
# sphinx_gallery_thumbnail_path = '_static/pre/explicit/belted_dummy.png'

###############################################################################
# Manually start the ``pre`` service
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Copy the ``pyDyna/src/ansys/dyna/core/pre/Server``folder to a desired location.
# Start the ``pre``service at this location by running this command:
#
# ``python kwserver.py``
#
# Once the ``pre`` servic is running, you can connect a client to it using
# the hostname and the port. This example uses the default local host and port
# (``"localhost"`` and ``"50051"`` respectively).
#
hostname = "localhost"
if len(sys.argv) > 1:
    hostname = sys.argv[1]
dummy_solution = DynaSolution(hostname)

###############################################################################
# Start the solution workflow
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NODES and ELEMENTS are read in from the ``belted_dummy.k`` file. This file
# also has the *PART* defined in it, but the section and material fields are
# empty to begin with.
#
fns = []
path = examples.belted_dummy + os.sep
fns.append(path + "belted_dummy.k")
dummy_solution.open_files(fns)

###############################################################################
# Create database and control cards
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# For the D3plots, set simulation termination time, simulation timestep, and
# output frequency. Use the ``set_init_velocity`` method in the ``dynamech``
# class to initialize the velocity components in the desired direction.
#
dummy_solution.set_termination(termination_time=0.12)
dummy_solution.create_database_binary(dt=2.5e-3)

dummy = DynaMech()
dummy_solution.add(dummy)

dummy.set_timestep(tssfac=0.8)
dummy.set_init_velocity(Velocity(14.8, 0, 0))

###############################################################################
# Define materials for the model
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# In this model, many parts share common material types. Thus, these materials
# are generated in a loop and a list of these materials are created. This list
# can then be used later to assign materials to parts. The ``dynamaterials`` class
# are used to define these materials: ``MAT_RIGID``, ``MAT_ELASTIC``,
# ``MAT_SPRING_NONLINEAR_ELASTIC,`` ``MAT_DAMPER_VISCOUS``, and
# ``MAT_DAMPER_NONLINEAR_VISCOUS``.
#
shellmatlist = []
for i in range(15):
    matrigid = MatRigid(
        mass_density=rigidmats[i][0], young_modulus=rigidmats[i][1], poisson_ratio=0.3
    )
    shellmatlist.append(matrigid)

for i in range(16, 23):
    index = i - 16
    matelastic = MatElastic(
        mass_density=elasticmats[index][0], young_modulus=elasticmats[index][1], poisson_ratio=0.3
    )
    shellmatlist.append(matelastic)

discmatlist = []
for i in range(101, 143):
    index = i - 101
    mat = MatSpringNonlinearElastic(curve=Curve(x=curvedata[index][0], y=curvedata[index][1]))
    discmatlist.append(mat)

for i in range(143, 185):
    index = i - 143
    mat = MatDamperViscous(damping_constant=dampingconst[index])
    discmatlist.append(mat)

for i in range(185, 209):
    index = i - 185
    mat = MatDamperNonlinearViscous(
        curve=Curve(x=curvedata[lcidlist[index]][0], y=curvedata[lcidlist[index]][1])
    )
    discmatlist.append(mat)

###############################################################################
# Define section properties and assign appropriate materials
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Now that you have a list of materials with the material ID corresponding to
# the Part ID, you can loop through the list and assign these materials to the
# parts. While in the loop, the section properties, element formulations, and
# constraints are also defined.
#
for i in range(1, 23):
    part = ShellPart(i)
    part.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
    part.set_material(shellmatlist[i - 1])
    part.set_thickness(shellsec[i - 1][0])
    part.set_integration_points(shellsec[i - 1][1])
    if i in range(1, 16):
        part.set_extra_nodes(NodeSet(extra_nodes[i - 1]))
    dummy.parts.add(part)

for i in range(101, 209):
    index = i - 101
    part = DiscretePart(i)
    part.set_material(discmatlist[index])
    part.set_displacement_option(displacement_option=DRO.DESCRIBES_TORSIONAL_SPRING)
    dummy.parts.add(part)

###############################################################################
# Define surface-to-surface contacts
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define several surface-to-surface contacts between predefined segment set pairs
# such that each contact has a specific friction defined between the master and slave.

fslist = [0.62, 0.62, 0.62, 0.8, 1, 0.8, 0.88, 0.88, 0.16, 0.88, 0]
for i in range(11):
    contact = Contact(category=ContactCategory.SURFACE_TO_SURFACE_CONTACT)
    contact.set_friction_coefficient(static=fslist[i])
    surf1 = ContactSurface(SegmentSet(segments[2 * i]))
    surf2 = ContactSurface(SegmentSet(segments[2 * i + 1]))
    contact.set_slave_surface(surf1)
    contact.set_master_surface(surf2)
    dummy.contacts.add(contact)

###############################################################################
# Define spherical joints
# ~~~~~~~~~~~~~~~~~~~~~~~
# A spherical joint is one of the simpler joint types which. You need only define
# a coincident node pair. Read the node pairs from the ``jointlist`` array defined in
# the ``belted_dummy_data.py`` file.
#
for i in range(42):
    id = i + 1
    dummy.create_defineorientation(
        vid=id, iop=2, vector=vector, node1=nlist[i][0], node2=nlist[i][1]
    )

for i in range(14):
    dummy.constraints.create_joint_spherical(nodes=jointlist[i])

###############################################################################
# Define a prescribed motion on a node set
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use the ``create_imposed_motion()`` method to define a prescribed motion
# on a node set.
#
dummy.boundaryconditions.create_imposed_motion(
    NodeSet(motion_nodes),
    Curve(x=motion_curve_x, y=motion_curve_y,sfo=0.1),
    motion=Motion.ACCELERATION,
    scalefactor=-1,
)
###############################################################################
# Define the gravity
# ~~~~~~~~~~~~~~~~~~
# Use the ``Gravity()`` method in the ``dynabase`` class
# to define the gravity load, direction of the load, and the curve.
# 
g = Gravity(dir=GravityOption.DIR_Z, load=Curve(x=[0, 0.152], y=[9.81, 9.81]))
dummy.add(g)

###############################################################################
# Define database outputs
# ~~~~~~~~~~~~~~~~~~~~~~~
# Define the frequency for the D3PLOT file and write out the input file.
#
dummy_solution.create_database_binary(dt=2.5e-3)
dummy_solution.save_file()
