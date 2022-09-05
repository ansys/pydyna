import os
import sys

sys.path.append(os.path.join(os.path.dirname(__file__),'../../ansys/dyna'))
from pre.dynaiga import *
from pre.dynamaterial import *
from iga_sample_data import *

if __name__ == "__main__":
    hostname = "localhost"
    if len(sys.argv) > 1:
        hostname = sys.argv[1]
    iga = DynaIGA(hostname=hostname)
    fns = []
    path = os.getcwd() + os.sep + "input" + os.sep
    fns.append(path + "maino.k")
    fns.append(path + "rkrwelds.key")
    fns.append(path + "27parts.key")
    iga.open_files(fns)

    iga.set_timestep(timestep_size_for_mass_scaled=-0.0004)
    iga.set_termination(20)
    
    #define material
    plastic = MatPiecewiseLinearPlasticity(mass_density=7.830e-06,young_modulus=200,yield_stress=1.5,tangent_modulus=0.5)
    swmatlist = []
    for mat in materialdata:
            spotweld = MatSpotweld(mass_density=mat[0],young_modulus=mat[1],poisson_ratio=mat[2],yield_stress=mat[3],plastic_hardening_modulus=mat[4],axial_force_resultant_at_failure=mat[5],force_resultant_nrs_at_failure=mat[6],force_resultant_nrt_at_failure=mat[7])
            swmatlist.append(spotweld)
    
    for id in igaparts:
        part = IGAPart(id)
        part.set_material(plastic)
        part.set_element_formulation(IGAFormulation.REISSNER_MINDLIN_FIBERS_AT_CONTROL_POINTS)
        part.set_thickness(1.0)

    for index in range(len(spotwelds)):
        part = SolidPart(spotwelds[index])
        if index != 1:
            part.set_hourglass(type = HourglassType.BELYTSCHKO_BINDEMAN)
        part.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
        part.set_material(swmatlist[index])

    cylinder1 = RigidwallCylinder(Point(2472.37, -600.000, 1270.98),Point(2472.37, -600.000, 2668.53),100,1000)
    cylinder2 = RigidwallCylinder(Point(3580.25, -600.000, 1261.37),Point(3580.25, -600.000, 3130.49),100,1000)
    cylinder3 = RigidwallCylinder(Point(3090.59, -955.35, 1299.42),Point(3090.59, -955.35, 2958.43),100,1000)
    cylinder3.set_motion(Curve(x=[0,100],y=[20,20]),dir = Direction(0,1,0))

    #define contact
    selfcontact = Contact(type=ContactType.AUTOMATIC)
    selfcontact.set_friction_coefficient(static=0.2)
    surf1=ContactSurface(PartSet(igaparts))
    selfcontact.set_slave_surface(surf1)

    swcontact = Contact(type=ContactType.TIED,category=ContactCategory.SHELL_EDGE_TO_SURFACE_CONTACT,offset=OffsetType.OFFSET)
    spotweldsolid=ContactSurface(PartSet(spotwelds))
    spotweldsurface=ContactSurface(PartSet(igaparts))
    swcontact.set_slave_surface(spotweldsolid)
    swcontact.set_master_surface(spotweldsurface)

    iga.create_database_binary(dt=0.1)
    iga.save_file()
