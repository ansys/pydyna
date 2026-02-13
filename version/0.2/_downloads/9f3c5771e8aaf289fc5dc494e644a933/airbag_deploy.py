"""
Airbag deploy example
=====================

This example show how to create an Airbag deploy model with Pydyna-pre module. \n
LS-DYNA version : ls-dyna_smp_d_R13.0_365-gf8a97bda2a_winx64_ifort190.exe
"""

import os
import sys


from ansys.dyna.core.pre.dynasolution import DynaSolution
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
from ansys.dyna.core.pre.dynamaterial import MatRigid, MatFabric
from ansys.dyna.core.pre import examples
# sphinx_gallery_thumbnail_path = '_static/pre/airbag/airbag.png'

hostname = "localhost"
if len(sys.argv) > 1:
    hostname = sys.argv[1]
airbag_solution = DynaSolution(hostname)
fns = []
# path = sys.path[0] + os.sep + "input" + os.sep + "airbag_deploy" + os.sep
path = examples.airbag_deploy + os.sep
fns.append(path + "airbag_deploy.k")
airbag_solution.open_files(fns)

airbag_solution.set_termination(0.03)

airbagdeploy = DynaMech()
airbag_solution.add(airbagdeploy)

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

rigidwall = RigidwallPlanar(Point(0, 0, 0), Point(0, 1, 0), coulomb_friction_coefficient=0.5)
airbagdeploy.add(rigidwall)

contact = Contact(category=ContactCategory.NODES_TO_SURFACE)
contact.set_friction_coefficient(static=0.5, dynamic=0.5)
surf1 = ContactSurface(PartSet([3]))
surf2 = ContactSurface(PartSet([2]))
surf2.set_penalty_stiffness_scale_factor(0.06667)
contact.set_slave_surface(surf1)
contact.set_master_surface(surf2)
airbagdeploy.contacts.add(contact)

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

airbag_solution.set_output_database(
    abstat=2.0e-4, glstat=2.0e-4, matsum=2.0e-4, rcforc=2.0e-4, rbdout=2.0e-4, rwforc=2.0e-4
)
airbag_solution.create_database_binary(dt=5e-4, ieverp=1)
airbag_solution.save_file()
