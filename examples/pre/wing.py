"""
Wing example
=============

This example demonstrates how to create an Wing input deck.

"""

import os
import sys

sys.path.append(os.path.join(os.path.dirname(__file__),'../../ansys/dyna'))
from pre.dynaicfd import *
from pre.dynadem import *

if __name__ == "__main__":
    hostname = "localhost"
    if len(sys.argv) > 1:
        hostname = sys.argv[1]
    icfd = DynaICFD(hostname = hostname)
    dem = DynaDEM(hostname = hostname)
    fns = []
    path = os.getcwd() + os.sep + "input" + os.sep + "icfd_dem" + os.sep
    fns.append(path + "main.k")
    fns.append(path + "fe-rig_mesh2.k")
    fns.append(path + "p22a-unts.k")
    fns.append(path + "tunnel-rig_cfd.K")
    icfd.open_files(fns)

    icfd.create_damping_global(valdmp=0.1)
    icfd.set_termination(0.6)
    icfd.create_timestep(tssfac=0.9, dt2ms=-1e-5)
    icfd.create_control_contact(rwpnal=0, shlthk=2, ssthk=1, ignore=2)
    icfd.create_control_dem_coupling(dt=0)

    # ---DEM
    dem.create_control_des(ndamp=0.99, tdamp=0.99, frics=0.9, fricr=0.9)
    dem.create_define_de_mesh_surface(
        sid=6, type=1, despid=1500, desxid=1500, nquad=4, nsid=150, rsf=-1
    )
    dem.create_define_de_mesh_surface(
        sid=113, type=1, despid=160, desxid=160, nquad=3, nsid=160, rsf=-1
    )

    icfd.create_control_general(dvcl=1)
    icfd.create_control_time(tim=20, dt=0.25e-3)
    icfd.create_control_output(msgl=3)
    icfd.create_control_turbulence(tmod=2)
    icfd.create_solver_tol_mmov(atol=1e-12, rtol=1e-12)

    #icfd.create_section_icfd(sid=1)
    mat = MatICFD(flow_density=1.28e-9,dynamic_viscosity=17e-9)

    pids = [1, 2, 3]
    icfd.mesh_embed_shell(volid=1, pids=pids)
    parameter = [-950, -80, -200, -600, 150, 30]
    icfd.mesh_create_size_shape(
        "BOX", force=1, method=0, msize=3.2, parameter=parameter
    )


    slot = ICFDPart(129)
    slot.set_material(mat)
    slot.set_prescribed_velocity(dof=DOF.X,motion=Curve(x=[0, 1e3],y=[0, 0]))
    slot.set_prescribed_velocity(dof=DOF.Y,motion=Curve(x=[0, 1e3],y=[0, 0]))
    slot.set_prescribed_velocity(dof=DOF.Z,motion=Curve(x=[0.0001, 0.02, 1e3],y=[10, 200000, 200000]))
    
    exit = ICFDPart(127)
    exit.set_material(mat)
    exit.set_prescribed_pressure(pressure = Curve(x=[0, 1e3],y=[0, 0]))
    
    bottom = ICFDPart(125)
    bottom.set_material(mat)
    bottom.set_free_slip()

    side = ICFDPart(126)
    side.set_material(mat)
    side.set_free_slip()

    top= ICFDPart(128)
    top.set_material(mat)
    top.set_non_slip()

    msshell1 = ICFDPart(1)
    msshell1.set_material(mat)
    msshell1.set_non_slip()

    msshell1 = ICFDPart(2)
    msshell1.set_material(mat)
    msshell1.set_non_slip()

    msshell1 = ICFDPart(3)
    msshell1.set_material(mat)
    msshell1.set_non_slip()

    partvol = ICFDVolumePart(surfaces=[125, 126, 127, 128, 129])
    partvol.set_material(mat) 

    meshvol = MeshedVolume(surfaces = [125, 126, 127, 128, 129])

    # fe-rig.k
    icfd.create_control_accuracy(osu=1, inn=4)
    icfd.create_control_energy(hgen=2)
    icfd.create_control_shell(wrpang=20, esort=1, irnxx=-1, bwc=1, proj=0, irquad=2)
    icfd.create_control_solid(esort=2)
    icfd.set_output_database(bndout=0.001,glstat=0.001,matsum=0.001,nodfor=0.001,rcforc=0.001,sleout=0.001)

    abs = [0.05, 1e18]
    ord = [1, 1]
    icfd.create_definecurve(lcid=11, sfo=1, abscissa=abs, ordinate=ord)
    icfd.create_load_body("Z", lcid=11)

    icfd.create_init_vel_rigidbody(pid=5, vyr=107.527)
    icfd.create_boundary_prescribed_motion(
        id=5,
        heading="Roll",
        option="RIGID",
        typeid=5,
        dof=6,
        lcid=11,
        sf=-107.57,
        vid=1,
    )

    mat1 = MatRigid(mass_density=2e-6,young_modulus=1000,poisson_ratio=0.34,center_of_mass_constraint=1,translational_constraint=7,rotational_constraint=7)
    mat5 = MatRigid(mass_density=2e-6,young_modulus=1000,poisson_ratio=0.34,center_of_mass_constraint=1,translational_constraint=7,rotational_constraint=6)
    
    foldingboard1 = ShellPart(1)
    foldingboard1.set_material(mat1)
    foldingboard1.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
    foldingboard1.set_thickness(1)
    foldingboard1.set_shear_factor(0.8333)

    roof = ShellPart(2)
    roof.set_material(mat1)
    roof.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
    roof.set_thickness(1)
    roof.set_shear_factor(0.8333)

    foldingboard2 = ShellPart(3)
    foldingboard2.set_material(mat1)
    foldingboard2.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
    foldingboard2.set_thickness(1)
    foldingboard2.set_shear_factor(0.8333)

    roll = ShellPart(3)
    roll.set_material(mat5)
    roll.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
    roll.set_thickness(1)
    roll.set_shear_factor(0.8333)

    # p22a-unts.k
    abs = [0, 1e18]
    ord = [1, 1]
    icfd.create_definecurve(lcid=111, sfo=1, abscissa=abs, ordinate=ord)
    tail = [0, 0, 0]
    head = [1, 0, 0]
    icfd.create_definevector(vid=1, tail=tail, head=head, title="FirstSpan")
    icfd.create_boundary_prescribed_motion(
        id=4,
        heading="Conveyor-yellow-shell-move-along-x",
        option="RIGID",
        typeid=9,
        dof=1,
        lcid=111,
        sf=6666,
        vid=1,
        birth=0.2,
    )


    CoreToChassis = Contact(type=ContactType.AUTOMATIC,category=ContactCategory.SURFACE_TO_SURFACE_CONTACT)
    CoreToChassis.set_friction_coefficient(static=0.2,dynamic=0.2)
    CoreToChassis.set_penalty_algorithm(ContactFormulation.SEGMENT_BASED_CONTACT_PENALTY,SBOPT.WRAPED_SEGMENT_CHECKING)
    CoreToChassis.set_tiebreak()
    surf1=ContactSurface(PartSet([6]))
    surf1.set_contact_thickness(2)
    surf2=ContactSurface(PartSet([112]))
    CoreToChassis.set_slave_surface(surf1)
    CoreToChassis.set_master_surface(surf2)

    TapeToChassis = Contact(type=ContactType.TIED,category=ContactCategory.SHELL_EDGE_TO_SURFACE_CONTACT)
    chassissurf=ContactSurface(PartSet([6]))
    chassissurf.set_contact_thickness(thickness=2)
    tapesurf=ContactSurface(PartSet([113]))
    tapesurf.set_contact_thickness(thickness=2)
    TapeToChassis.set_slave_surface(chassissurf)
    TapeToChassis.set_master_surface(tapesurf)

    TapeToWing1= Contact(type=ContactType.AUTOMATIC,category=ContactCategory.SURFACE_TO_SURFACE_CONTACT)
    TapeToWing1.set_active_time(birth_time=0.252)
    tapesurf=ContactSurface(SegmentSet(tapesegs))
    tapesurf.set_contact_thickness(thickness=2)
    wingsurf1=ContactSurface(PartSet([113]))
    wingsurf1.set_contact_thickness(thickness=3)
    TapeToWing1.set_slave_surface(tapesurf)
    TapeToWing1.set_master_surface(wingsurf1)

    TapeToWing2= Contact(type=ContactType.AUTOMATIC,category=ContactCategory.SURFACE_TO_SURFACE_CONTACT)
    TapeToWing2.set_active_time(birth_time=0.261)
    tapesurf=ContactSurface(SegmentSet(tapesegs))
    tapesurf.set_contact_thickness(thickness=2)
    wingsurf1=ContactSurface(PartSet([113]))
    wingsurf1.set_contact_thickness(thickness=3)
    TapeToWing2.set_slave_surface(tapesurf)
    TapeToWing2.set_master_surface(wingsurf1)

    ChassisToConveyor= Contact(type=ContactType.AUTOMATIC,category=ContactCategory.SURFACE_TO_SURFACE_CONTACT)
    ChassisToConveyor.set_tiebreak()
    ChassisToConveyor.set_penalty_algorithm(ContactFormulation.SEGMENT_BASED_CONTACT_PENALTY,SBOPT.WRAPED_SEGMENT_CHECKING)
    conveyorsurf=ContactSurface(PartSet([9]))
    conveyorsurf.set_contact_thickness(thickness=2)
    chasissurf=ContactSurface(SegmentSet(segs13))
    chasissurf.set_contact_thickness(thickness=2)
    ChassisToConveyor.set_slave_surface(conveyorsurf)
    ChassisToConveyor.set_master_surface(chasissurf)

    selfcontact = Contact()
    selfcontact.set_friction_coefficient(static=0.01,dynamic=0.01)
    selfcontact.set_penalty_algorithm(ContactFormulation.SEGMENT_BASED_CONTACT_PENALTY,SBOPT.WRAPED_SEGMENT_CHECKING)
    surf1=ContactSurface(PartSet([1, 2, 3, 5, 6, 113, 112]))
    surf1.set_contact_thickness(1.5)
    selfcontact.set_slave_surface(surf1)


    pids = [6]
    icfd.set_part_damping_stiffness(pids=pids, coef=0.1)
    pids = [113]
    icfd.set_part_damping_stiffness(pids=pids, coef=0.1)

    icfd.create_boundary_spc(
        option1="SET",
        birthdeath=True,
        nid=5,
        dofx=1,
        dofy=1,
        dofz=1,
        dofrx=1,
        dofry=1,
        dofrz=1,
        birth=0.2,
    )

    pids = [6, 9, 112, 113]
    icfd.create_partset(sid=5, pids=pids)
    icfd.create_init_vel_bodies(id=5, styp=1, vx=6666, nx=1, phase=1, stime=0.2)


    
    matelastic = MatElastic(mass_density=3.77e-8,young_modulus=320,poisson_ratio=0.1)
    matrigid = MatRigid(mass_density=2.71e-8,young_modulus=100,poisson_ratio=0.34,center_of_mass_constraint=1,translational_constraint=5,rotational_constraint=7)    

    chassis = ShellPart(6)
    chassis.set_material(matelastic)
    chassis.set_element_formulation(ShellFormulation.FULLY_INTEGRATED)
    chassis.set_thickness(0.72)
    chassis.set_shear_factor(0.8333)
    chassis.set_hourglass(HourglassType.ACTIVATES_FULL_PROJECTION_WARPING_STIFFNESS)

    conveyor = ShellPart(9)
    conveyor.set_material(matrigid)
    conveyor.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
    conveyor.set_thickness(1)
    conveyor.set_shear_factor(0.8333)

    core = SolidPart(112)
    core.set_material(matelastic)
    core.set_element_formulation(SolidFormulation.EIGHT_POINT_ENHANCED_STRAIN_SOLID_ELEMENT)

    tape = ShellPart(113)
    tape.set_material(matelastic)
    tape.set_element_formulation(ShellFormulation.FULLY_INTEGRATED)
    tape.set_thickness(0.72)
    tape.set_shear_factor(0.8333)
    tape.set_hourglass(HourglassType.ACTIVATES_FULL_PROJECTION_WARPING_STIFFNESS)

    ents = [4]
    icfd.create_nodeset(option="GENERAL", sid=1, genoption="SET_SHELL", entities=ents)
    icfd.create_constrained_extra_nodes(option="SET", pid=9, nid=1)

    icfd.create_database_binary(dt=0.005, maxint=1, dcomp=2, nintsld=1)
    icfd.save_file()
