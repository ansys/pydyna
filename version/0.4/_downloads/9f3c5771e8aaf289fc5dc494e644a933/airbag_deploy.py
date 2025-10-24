"""
Airbag deploy
-------------
This example shows how to create an airbag deploy model with the PyDNYA ``pre`` service.
The executable file for LS-DYNA is ``ls-dyna_smp_d_R13.0_365-gf8a97bda2a_winx64_ifort190.exe``.

"""
###############################################################################
# Perform required imports
# ~~~~~~~~~~~~~~~~~~~~~~~~
# Peform required imports.

import os
import sys


from ansys.dyna.core.pre import launch_dynapre
from ansys.dyna.core.pre.dynamech import (
    DynaMech,
    Airbag,
    PartSet,
    Curve,
    Point,
    RigidwallPlanar,
    Contact,
    ContactSurface,
    ContactCategory,
    ShellPart,
    ShellFormulation,
)

###############################################################################
# Start the ``pre`` service
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Before starting the ``pre`` service, you must ensure that the Docker container
# for this service has been started. For more information, see "Start the Docker
# container for the ``pre`` service" in https://dyna.docs.pyansys.com/version/stable/index.html.
# 
# The ``pre`` service can also be started locally, please download the latest version of 
# ansys-pydyna-pre-server.zip package from https://github.com/ansys/pydyna/releases and start it 
# refering to the README.rst file in this server package.
#
# Once the ``pre`` service is running, you can connect a client to it using
# the hostname and port. This example uses the default localhost and port
# (``"localhost"`` and ``"50051"`` respectively).
#
from ansys.dyna.core.pre.dynamaterial import MatRigid, MatFabric
from ansys.dyna.core.pre import examples
from ansys.dyna.core.pre.misc import check_valid_ip
# sphinx_gallery_thumbnail_path = '_static/pre/airbag/airbag.png'

hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]

###############################################################################
# Start the solution workflow
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The ``DynaSolution`` class is like a workflow orchestrator.
# It inherits methods from other classes and helps create a complete workflow.
#
airbag_solution = launch_dynapre(ip = hostname)
fns = []
# path = sys.path[0] + os.sep + "input" + os.sep + "airbag_deploy" + os.sep
path = examples.airbag_deploy + os.sep
fns.append(path + "airbag_deploy.k")
airbag_solution.open_files(fns)

###############################################################################
# Create standard explicit control cards
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The following code uses the ``set_termination`` method to set the termination time
# to ``0.03`` in *CONTROL_TERMINATION*. The ``DynaMech`` class
# automatically generates the common control cards used in
# explicit problems. ``CONTROL_ACCURACY``, ``CONTACT``, ``BULK VISCOCITY``,
# and ``CONTACT`` are all automatically generated.
#
airbag_solution.set_termination(0.03)

airbagdeploy = DynaMech()
airbag_solution.add(airbagdeploy)

###############################################################################
# Define a keyword
# ~~~~~~~~~~~~~~~~
# Use the ``Airbag`` function in the ``DynaMech`` class to define
# *AIRBAG_SIMPLE_AIRBAG_MODEL* as a keyword. While LS-DYNA has many different
# airbag models, PyDYNA currently supports only one: SIMPLE_AIRBAG_MODEL.
# If you have an urgent need for PyDYNA to support another airbag model, email
# `pyansys.core@ansys.com <mailto:pyansys.core@ansys.com>`_ with your request.

airbag = Airbag(
    set=PartSet([3]),
    heat_capacity_at_constant_volume=1.736e3,
    heat_capacity_at_constant_pressure=2.43e3,
    input_gas_temperature=1.2e3,
    input_mass_flow_rate=Curve(x=[0, 0.032, 0.045, 0.08], y=[0, 26, 0.6, 0.1]),
    shape_factor_for_exit_hole=0.7,
    ambient_pressure=14.7,
    ambient_density=3.821e-6,
)
airbagdeploy.add(airbag)

###############################################################################
# Generate an infinite planar rigid wall
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# To generate an infinite planar rigidwall, define the coordinates of the heat
# vector and the tail vector of the plane.
#
rigidwall = RigidwallPlanar(Point(0, 0, 0), Point(0, 1, 0), coulomb_friction_coefficient=0.5)
airbagdeploy.add(rigidwall)

###############################################################################
# Define a node-to-surface contact
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define a node-to-surface contact by passing a master part set and a slave
# part set.

contact = Contact(category=ContactCategory.NODES_TO_SURFACE)
contact.set_friction_coefficient(static=0.5, dynamic=0.5)
surf1 = ContactSurface(PartSet([3]))
surf2 = ContactSurface(PartSet([2]))
surf2.set_penalty_stiffness_scale_factor(0.06667)
contact.set_slave_surface(surf1)
contact.set_master_surface(surf2)
airbagdeploy.contacts.add(contact)

###############################################################################
# Define material cards
# ~~~~~~~~~~~~~~~~~~~~~
# LS-DYNA has over 300 materials that are used for varied applications.
# While PyDYNA does not yet support all material cards, it does support
# most commonly used materials, including ``FABRIC``, ``MAT_ELASTIC``,
# ``PIECEWISE_LINEAR_PLASTICITY``, and ``RIGID``. All supported materials
# are accessed from the ``dynamaterial`` class. In the following code,
# ``MAT_RIGID`` is defined as the material  for the cylindrical tube and the
# bottom plate. ``MAT_FABRIC`` is defined as the material for the airbag volume.
# Note that ``platemat`` also has contraints defined.

platemat = MatRigid(
    mass_density=7.84e-4,
    young_modulus=30e6,
    center_of_mass_constraint=1,
    translational_constraint=7,
    rotational_constraint=7,
)
cylindermat = MatRigid(mass_density=1.96e-4, young_modulus=30e6)
airbagmat = MatFabric(
    mass_density=1e-4,
    young_modulus_longitudinal_direction=2e6,
    young_modulus_transverse_direction=2e6,
    shear_modulus=1.53e6,
)

###############################################################################
# Define sectional properties
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The following code defines the sectional properties of the parts. This example
# has three shell parts. Each shell part is initialized as ``ShellPart`` with a
# unique ID and an appropriate shell formulation is assigned. Again,
# PyDYNA does not yet support all element formulations. You can find the
# supported formulations in the ``dynabase`` class.

plate = ShellPart(1)
plate.set_material(platemat)
plate.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
plate.set_thickness(0.5)
airbagdeploy.parts.add(plate)

cylinder = ShellPart(2)
cylinder.set_material(cylindermat)
cylinder.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
cylinder.set_thickness(0.5)
airbagdeploy.parts.add(cylinder)

airbagpart = ShellPart(3)
airbagpart.set_material(airbagmat)
airbagpart.set_element_formulation(ShellFormulation.FULLY_INTEGRATED_BELYTSCHKO_TSAY_MEMBRANE)
airbagpart.set_thickness(0.015)
airbagpart.set_integration_points(4)
airbagdeploy.parts.add(airbagpart)

###############################################################################
# Define database outputs
# ~~~~~~~~~~~~~~~~~~~~~~~
# Use the ``set_output_database()`` and ``create_database_binary()`` methods to define the
# output frequency of the ASCII and binary D3PLOT files. Then, use the ``save_file()``
# method to write out the model as an input DYNA key file.

airbag_solution.set_output_database(
    abstat=2.0e-4, glstat=2.0e-4, matsum=2.0e-4, rcforc=2.0e-4, rbdout=2.0e-4, rwforc=2.0e-4
)
airbag_solution.create_database_binary(dt=5e-4, ieverp=1)
airbag_solution.save_file()
