import os
import sys

from pydyna.dynaairbag import DynaAirbag
from pydyna.dynamaterial import *

if __name__ == "__main__":
    hostname = "localhost"
    if len(sys.argv) > 1:
        hostname = sys.argv[1]
    airbagdeploy = DynaAirbag(hostname=hostname)
    fns = []
    path = os.getcwd() + os.sep + "input" + os.sep + "airbag_deploy" + os.sep
    fns.append(path + "airbag_deploy.k")
    airbagdeploy.open_files(fns)

    airbagdeploy.set_termination(0.03)
    airbagdeploy.create_control_energy(hgen=2, rwen=2, slnten=2)
    airbagdeploy.create_control_output(npopt=1, neecho=3)

    abs = [0, 0.032, 0.045, 0.08]
    ord = [0, 26, 0.6, 0.1]
    airbagdeploy.create_definecurve(lcid=1, sfo=1, abscissa=abs, ordinate=ord)

    pids = [3]
    airbagdeploy.create_partset(sid=1, pids=pids)

    airbagdeploy.create_simple_airbag_model(
        modeltype="SIMPLE_AIRBAG_MODEL",
        sid=1,
        sidtyp=1,
        cv=1.736e3,
        cp=2.43e3,
        t=1.2e3,
        lcid=1,
        mu=0.7,
        area=0,
        pe=14.7,
        ro=3.821e-6,
    )

    tail = [0, 0, 0]
    head = [0, 1, 0]
    airbagdeploy.create_rigidwall_planar(nsid=0, tail=tail, head=head, fric=0.5)


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

    airbag = ShellPart(3)
    airbag.set_material(airbagmat)
    airbag.set_element_formulation(ShellFormulation.FULLY_INTEGRATED_BELYTSCHKO_TSAY_MEMBRANE)
    airbag.set_thickness(0.015)
    airbag.set_integration_points(4)

    airbag.create_database_binary(dt=5e-4, ieverp=1)
    airbag.create_database_binary(filetype="D3THDT", dt=999999)
    airbag.set_output_database(abstat=2.0e-4,glstat=2.0e-4,matsum=2.0e-4,rcforc=2.0e-4,rbdout=2.0e-4,rwforc=2.0e-4)

    airbag.save_file()
