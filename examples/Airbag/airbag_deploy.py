"""
Airbag deploy example
=====================

This example show how to create an Airbag deploy model with Pydyna-pre module
"""

import os
import sys

sys.path.append(os.path.join(os.path.dirname(__file__),'../../'))
from ansys.dyna.pre.dynasolution import *
from ansys.dyna.pre.dynamech import *
from ansys.dyna.pre.dynamaterial import *

if __name__ == "__main__":
    hostname = "localhost"
    if len(sys.argv) > 1:
        hostname = sys.argv[1]

    airbag_solution = DynaSolution(hostname)
    fns = []
    path = os.getcwd() + os.sep + "input" + os.sep + "airbag_deploy" + os.sep
    fns.append(path + "airbag_deploy.k")
    airbag_solution.open_files(fns)

    airbag_solution.set_termination(0.03)
    airbag_solution.create_database_binary(dt=5e-4, ieverp=1)
    
    airbagdeploy = DynaMech()
    airbag_solution.add(airbagdeploy)

    airbag = Airbag(set=PartSet([3]),
        heat_capacity_at_constant_volume=1.736e3,
        heat_capacity_at_constant_pressure=2.43e3,
        input_gas_temperature=1.2e3,
        input_mass_flow_rate = Curve(x=[0, 0.032, 0.045, 0.08],y=[0, 26, 0.6, 0.1]),
        shape_factor_for_exit_hole=0.7,
        ambient_pressure=14.7,
        ambient_density=3.821e-6)

    rigidwall = RigidwallPlanar(Point(0, 0, 0),Point(0, 1, 0),coulomb_friction_coefficient=0.5)

    contact = Contact(category=ContactCategory.NODES_TO_SURFACE)
    contact.set_friction_coefficient(static=0.5,dynamic=0.5)
    surf1=ContactSurface(PartSet([3]))
    surf2=ContactSurface(PartSet([2]))
    surf2.set_penalty_stiffness_scale_factor(0.06667)
    contact.set_slave_surface(surf1)
    contact.set_master_surface(surf2)

    platemat = MatRigid(mass_density=7.84e-4,young_modulus=30e6,center_of_mass_constraint=1,translational_constraint=7,rotational_constraint=7)
    cylindermat = MatRigid(mass_density=1.96e-4,young_modulus=30e6)
    airbagmat = MatFabric(mass_density=1e-4,young_modulus_longitudinal_direction=2e6,young_modulus_transverse_direction=2e6,shear_modulus=1.53e6)
    
    plate = ShellPart(1)
    plate.set_material(platemat)
    plate.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
    plate.set_thickness(0.5)

    cylinder = ShellPart(2)
    cylinder.set_material(cylindermat)
    cylinder.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
    cylinder.set_thickness(0.5)

    airbagpart = ShellPart(3)
    airbagpart.set_material(airbagmat)
    airbagpart.set_element_formulation(ShellFormulation.FULLY_INTEGRATED_BELYTSCHKO_TSAY_MEMBRANE)
    airbagpart.set_thickness(0.015)
    airbagpart.set_integration_points(4)
    
    airbagdeploy.set_output_database(abstat=2.0e-4,glstat=2.0e-4,matsum=2.0e-4,rcforc=2.0e-4,rbdout=2.0e-4,rwforc=2.0e-4)

    airbag_solution.save_file()
