"""
Crash example
=====================

This example demonstrates how to create a simple crash input deck.
"""

import os
from re import X
import sys

sys.path.append(os.path.join(os.path.dirname(__file__),'../../ansys/dyna'))
from pre.dynabase import *
from pre.dynamaterial import *
from camry_rc_data import *

if __name__ == "__main__":
    hostname = "localhost"
    if len(sys.argv) > 1:
        hostname = sys.argv[1]
    camry = DynaBase(hostname=hostname)
    #Import the initial mesh data(nodes and elements)
    fns = []
    path = os.getcwd() + os.sep + "input" + os.sep+ "camry_rc" + os.sep
    fns.append(path + "Camry_RC_main.k")
    fns.append(path + "501_RIG_BAR_roof_crush_platen5.key")
    fns.append(path + "Camry_V1_NoSusAndPowerTrain_impl7.k")
    fns.append(path + "Camry_V1_NoSusAndPowerTrain_impl7_nodes.k")
    fns.append(path + "roof_welds.k")
    fns.append(path + "weld7.k")
    fns.append(path + "xtra_sw.k")
    camry.open_files(fns)

    #global setting
    camry.set_termination(10)
    
    analysis = ImplicitAnalysis(initial_timestep_size=0.1)
    analysis.set_timestep(control_flag=TimestepCtrol.AUTOMATICALLY_ADJUST_TIMESTEP_SIZE,Optimum_equilibrium_iteration_count=511)
    analysis.set_dynamic(gamma=0.6,beta=0.38)
    analysis.set_eigenvalue()
    analysis.set_solution(stiffness_reformation_limit=55,absolute_convergence_tolerance=-100)
    
    #create material
    matnull = MatNull(mass_density=6e-11)
    matrigid = MatRigid(mass_density=7.890e-09,young_modulus=2.100e+05,poisson_ratio=0.3)
    matplaten = MatRigid(mass_density=7.80e-09,young_modulus=2.00e+05,poisson_ratio=0.3,center_of_mass_constraint=1,rotational_constraint=7)
    spotweldharden2100 = MatSpotweld(mass_density=7.850e-09,young_modulus=2.100e+05,poisson_ratio=0.3,yield_stress=510,plastic_hardening_modulus=2100)
    spotweldharden2200 = MatSpotweld(mass_density=7.850e-09,young_modulus=2.100e+05,poisson_ratio=0.3,yield_stress=510,plastic_hardening_modulus=2200)
    windowshield = MatModifiedPiecewiseLinearPlasticity(mass_density=2.355e-09,young_modulus=7.000e+04,poisson_ratio=0.22,yield_stress=30,tangent_modulus=1400,plastic_strain_to_failure=0.015,integration_points_number=1)
    windowsrear = MatModifiedPiecewiseLinearPlasticity(mass_density=2.425e-09,young_modulus=7.000e+04,poisson_ratio=0.22,yield_stress=30,tangent_modulus=1400,plastic_strain_to_failure=0.015,integration_points_number=1)
    plastic300 = MatPiecewiseLinearPlasticity(mass_density=7.890e-09,young_modulus=210000,yield_stress=300,tangent_modulus=5000)
    plastic250 = MatPiecewiseLinearPlasticity(mass_density=7.890e-09,young_modulus=210000,yield_stress=250,tangent_modulus=5000)
    plastic360 = MatPiecewiseLinearPlasticity(mass_density=7.890e-09,young_modulus=210000,yield_stress=360,tangent_modulus=5000)
    plastic180 = MatPiecewiseLinearPlasticity(mass_density=7.850e-09,young_modulus=210000,yield_stress=180,tangent_modulus=5000)
    plastic450 = MatPiecewiseLinearPlasticity(mass_density=7.850e-09,young_modulus=210000,yield_stress=450,tangent_modulus=5000)
    plastic1300 = MatPiecewiseLinearPlasticity(mass_density=7.850e-09,young_modulus=210000,yield_stress=1300,tangent_modulus=5000)
    plastic400 = MatPiecewiseLinearPlasticity(mass_density=7.890e-09,young_modulus=210000,yield_stress=400,tangent_modulus=5000)
    plastic500 = MatPiecewiseLinearPlasticity(mass_density=7.890e-09,young_modulus=210000,yield_stress=500,tangent_modulus=5000)
    plastic675 = MatPiecewiseLinearPlasticity(mass_density=7.850e-09,young_modulus=210000,yield_stress=675,tangent_modulus=5000)
    plastic310 = MatPiecewiseLinearPlasticity(mass_density=2.255e-09,young_modulus=70000,yield_stress=310,tangent_modulus=5000)
    plastic220 = MatPiecewiseLinearPlasticity(mass_density=7.890e-09,young_modulus=210000,yield_stress=220,tangent_modulus=5000)
    plastic220_410 = MatPiecewiseLinearPlasticity(mass_density=7.890e-09,young_modulus=210000,yield_stress=220,tangent_modulus=410)
    
    #set model
    for bpart in beamparts:
        part = BeamPart(bpart[0])
        if part.id in [50000002]:
            part.set_material(spotweldharden2200)
        else:
            part.set_material(spotweldharden2100)
        part.set_element_formulation(bpart[1])
        part.set_diameter(bpart[2])

    for spart in shellparts:
        part = ShellPart(spart[0])
        if part.id in [1463,1464]:
            part.set_material(matnull)
        elif part.id in [417,419,530,532,585,586,587,588]:
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
    
    #define constrained
    cons = Constraint()
    for sw in spotweld:
        cons.create_spotweld(nodeid1=sw[0],nodeid2=sw[1])

    for cnrb in cnrbs:
        cons.create_cnrb(nodeset=NodeSet(cnrb))

    #define contact
    selfcontact = Contact(type=ContactType.AUTOMATIC)
    selfcontact.set_mortar()
    selfcontact.set_friction_coefficient(static=0.2)
    surf1=ContactSurface(PartSet(vehicle))
    selfcontact.set_slave_surface(surf1)

    platebiw = Contact(type=ContactType.AUTOMATIC,category=ContactCategory.SURFACE_TO_SURFACE_CONTACT)
    platebiw.set_mortar()
    platebiw.set_friction_coefficient(static=0.2,dynamic=0.2)
    surf1=ContactSurface(PartSet(biw))
    surf2=ContactSurface(PartSet(platen))
    platebiw.set_slave_surface(surf1)
    platebiw.set_master_surface(surf2)

    swcontact = Contact(type=ContactType.TIED,category=ContactCategory.SHELL_EDGE_TO_SURFACE_CONTACT,offset=OffsetType.CONSTRAINED_OFFSET)
    spotweldbeam=ContactSurface(PartSet(spotweldbeams))
    spotweldbeam.set_contact_thickness(thickness=-0.9)
    spotweldsurface=ContactSurface(PartSet(spotweldsurfaces))
    spotweldsurface.set_contact_thickness(thickness=-0.9)
    swcontact.set_slave_surface(spotweldbeam)
    swcontact.set_master_surface(spotweldsurface)

    #define boundary
    bdy = BoundaryCondition()
    bdy.create_spc(NodeSet(spc))
    
    crv = Curve(x=[0,1,2,3,4,5,6,7,8,9,9.77,100],
                y=[0,13,26,39,52,65,78,91,104,117,127,127])
    platen=PartSet([50000001])
    bdy.create_imposed_motion(platen,crv,dof=DOF.X_TRANSLATIONAL,scalefactor=-0.0802216)
    bdy.create_imposed_motion(platen,crv,dof=DOF.Y_TRANSLATIONAL,scalefactor=-0.0802216)
    bdy.create_imposed_motion(platen,crv,dof=DOF.Z_TRANSLATIONAL,scalefactor=-0.0802216)

    camry.create_database_binary(dt=0.001)
    camry.set_output_database(elout=0.0001,glstat=0.0001,matsum=0.0001,nodout=0.0001,rbdout=0.0001,rcforc=0.0001,secforc=0.0001)

    camry.save_file()
