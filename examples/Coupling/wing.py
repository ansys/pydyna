"""
Wing example
=============

This example demonstrates how to create an Wing input deck.

"""

import os
import sys


from ansys.dyna.core.pre.dynasolution import *
from ansys.dyna.core.pre.dynamaterial import *
from ansys.dyna.core.pre.dynaicfd import *
from ansys.dyna.core.pre.dynadem import *
from wing_data import *
from ansys.dyna.core.pre import examples

hostname = "localhost"
if len(sys.argv) > 1:
    hostname = sys.argv[1]

wing_solution = DynaSolution(hostname)

fns = []
path = examples.wing + os.sep
fns.append(path + "main.k")
fns.append(path + "fe-rig_mesh2.k")
fns.append(path + "p22a-unts.k")
fns.append(path + "tunnel-rig_cfd.K")
wing_solution.open_files(fns)
wing_solution.set_termination(0.6)

icfd = DynaICFD()
dem = DynaDEM()
wing_solution.add(icfd)
wing_solution.add(dem)

# ICFD Control
icfd.create_control_dem_coupling(dt=0)
icfd.create_control_general(dvcl=1)
icfd.set_timestep(timestep=0.25e-3)
icfd.set_termination(termination_time=20)
icfd.create_control_output(msgl=3)
icfd.create_control_turbulence(tmod=2)
icfd.create_solver_tol_mmov(atol=1e-12, rtol=1e-12)

# Set properties for icfd parts
mat = MatICFD(flow_density=1.28e-9, dynamic_viscosity=17e-9)

slot = ICFDPart(129)
slot.set_material(mat)
slot.set_prescribed_velocity(dof=ICFDDOF.X, motion=Curve(x=[0, 1e3], y=[0, 0]))
slot.set_prescribed_velocity(dof=ICFDDOF.Y, motion=Curve(x=[0, 1e3], y=[0, 0]))
slot.set_prescribed_velocity(
    dof=ICFDDOF.Z, motion=Curve(x=[0.0001, 0.02, 1e3], y=[10, 200000, 200000])
)
icfd.parts.add(slot)

exit = ICFDPart(127)
exit.set_material(mat)
exit.set_prescribed_pressure(pressure=Curve(x=[0, 1e3], y=[0, 0]))
icfd.parts.add(exit)

bottom = ICFDPart(125)
bottom.set_material(mat)
bottom.set_free_slip()
icfd.parts.add(bottom)

side = ICFDPart(126)
side.set_material(mat)
side.set_free_slip()
icfd.parts.add(side)

top = ICFDPart(128)
top.set_material(mat)
top.set_non_slip()
icfd.parts.add(top)

msshell1 = ICFDPart(1)
msshell1.set_material(mat)
msshell1.set_non_slip()
icfd.parts.add(msshell1)

msshell2 = ICFDPart(2)
msshell2.set_material(mat)
msshell2.set_non_slip()
icfd.parts.add(msshell2)

msshell3 = ICFDPart(3)
msshell3.set_material(mat)
msshell3.set_non_slip()
icfd.parts.add(msshell3)

partvol = ICFDVolumePart(surfaces=[125, 126, 127, 128, 129])
partvol.set_material(mat)
icfd.parts.add(partvol)

meshvol = MeshedVolume(surfaces=[125, 126, 127, 128, 129])
meshvol.embed_shell([1, 2, 3])
meshvol.meshsize_box(size=3.2, min_point=Point(-950, -80, -200), max_point=Point(-600, 150, 30))
icfd.add(meshvol)

# ---DEM
dem.create_control_shell(wrpang=20, esort=1, irnxx=-1, bwc=1, proj=0, irquad=2)
dem.create_control_solid(esort=2)
dem.set_timestep(timestep_size_for_mass_scaled=-1e-5)
dem.create_control_contact(rwpnal=0, shlthk=2, ssthk=1, ignore=2)
dem.create_damping_global(valdmp=0.1)
dem.set_des(ndamp=0.99, tdamp=0.99, frics=0.9, fricr=0.9)

# Set properties for parts
mat1 = MatRigid(
    mass_density=2e-6,
    young_modulus=1000,
    poisson_ratio=0.34,
    center_of_mass_constraint=1,
    translational_constraint=7,
    rotational_constraint=7,
)
mat5 = MatRigid(
    mass_density=2e-6,
    young_modulus=1000,
    poisson_ratio=0.34,
    center_of_mass_constraint=1,
    translational_constraint=7,
    rotational_constraint=6,
)

foldingboard1 = ShellPart(1)
foldingboard1.set_material(mat1)
foldingboard1.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
foldingboard1.set_thickness(1)
foldingboard1.set_shear_factor(0.8333)
dem.parts.add(foldingboard1)

roof = ShellPart(2)
roof.set_material(mat1)
roof.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
roof.set_thickness(1)
roof.set_shear_factor(0.8333)
dem.parts.add(roof)

foldingboard2 = ShellPart(3)
foldingboard2.set_material(mat1)
foldingboard2.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
foldingboard2.set_thickness(1)
foldingboard2.set_shear_factor(0.8333)
dem.parts.add(foldingboard2)

roll = ShellPart(5)
roll.set_material(mat5)
roll.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
roll.set_thickness(1)
roll.set_shear_factor(0.8333)
roll.set_rigidbody_initial_velocity(rotation=RotVelocity(0, 107.527, 0))
dem.parts.add(roll)

matelastic = MatElastic(mass_density=3.77e-8, young_modulus=320, poisson_ratio=0.1)
matrigid = MatRigid(
    mass_density=2.71e-8,
    young_modulus=100,
    poisson_ratio=0.34,
    center_of_mass_constraint=1,
    translational_constraint=5,
    rotational_constraint=7,
)

chassis = ShellPart(6)
chassis.set_material(matelastic)
chassis.set_element_formulation(ShellFormulation.FULLY_INTEGRATED)
chassis.set_thickness(0.72)
chassis.set_shear_factor(0.8333)
chassis.set_hourglass(HourglassType.ACTIVATES_FULL_PROJECTION_WARPING_STIFFNESS)
chassis.set_stiffness_damping_coefficient(0.1)
chassis.set_des_surface(despid=1500, desxid=1500, nquad=4, nsid=150)
dem.parts.add(chassis)

conveyor = ShellPart(9)
conveyor.set_material(matrigid)
conveyor.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
conveyor.set_thickness(1)
conveyor.set_shear_factor(0.8333)
nset = NodesetGeneral(settype=SetType.SHELL, setids=[4])
conveyor.set_extra_nodes(nset)
dem.parts.add(conveyor)

core = SolidPart(112)
core.set_material(matelastic)
core.set_element_formulation(SolidFormulation.EIGHT_POINT_ENHANCED_STRAIN_SOLID_ELEMENT)
dem.parts.add(core)

tape = ShellPart(113)
tape.set_material(matelastic)
tape.set_element_formulation(ShellFormulation.FULLY_INTEGRATED)
tape.set_thickness(0.72)
tape.set_shear_factor(0.8333)
tape.set_hourglass(HourglassType.ACTIVATES_FULL_PROJECTION_WARPING_STIFFNESS)
tape.set_stiffness_damping_coefficient(0.1)
tape.set_des_surface(despid=160, desxid=160, nquad=3, nsid=160)
dem.parts.add(tape)

# Define contact
CoreToChassis = Contact(
    type=ContactType.AUTOMATIC, category=ContactCategory.SURFACE_TO_SURFACE_CONTACT
)
CoreToChassis.set_penalty_algorithm(
    ContactFormulation.SEGMENT_BASED_CONTACT_PENALTY, SBOPT.WRAPED_SEGMENT_CHECKING
)
CoreToChassis.set_tiebreak()
surf1 = ContactSurface(PartSet([6]))
surf1.set_contact_thickness(2)
surf2 = ContactSurface(PartSet([112]))
CoreToChassis.set_slave_surface(surf1)
CoreToChassis.set_master_surface(surf2)
dem.contacts.add(CoreToChassis)

TapeToChassis = Contact(
    type=ContactType.TIED, category=ContactCategory.SHELL_EDGE_TO_SURFACE_CONTACT
)
chassissurf = ContactSurface(PartSet([6]))
chassissurf.set_contact_thickness(thickness=2)
tapesurf = ContactSurface(PartSet([113]))
tapesurf.set_contact_thickness(thickness=2)
TapeToChassis.set_slave_surface(chassissurf)
TapeToChassis.set_master_surface(tapesurf)
dem.contacts.add(TapeToChassis)

TapeToWing1 = Contact(
    type=ContactType.AUTOMATIC, category=ContactCategory.SURFACE_TO_SURFACE_CONTACT
)
TapeToWing1.set_tiebreak()
TapeToWing1.set_active_time(birth_time=0.252)
tapesurf = ContactSurface(SegmentSet(tapesegs))
tapesurf.set_contact_thickness(thickness=2)
wingsurf1 = ContactSurface(PartSet([113]))
wingsurf1.set_contact_thickness(thickness=3)
TapeToWing1.set_slave_surface(tapesurf)
TapeToWing1.set_master_surface(wingsurf1)
dem.contacts.add(TapeToWing1)

TapeToWing2 = Contact(
    type=ContactType.AUTOMATIC, category=ContactCategory.SURFACE_TO_SURFACE_CONTACT
)
TapeToWing2.set_tiebreak()
TapeToWing2.set_active_time(birth_time=0.261)
tapesurf = ContactSurface(SegmentSet(tapesegs))
tapesurf.set_contact_thickness(thickness=2)
wingsurf1 = ContactSurface(PartSet([113]))
wingsurf1.set_contact_thickness(thickness=3)
TapeToWing2.set_slave_surface(tapesurf)
TapeToWing2.set_master_surface(wingsurf1)
dem.contacts.add(TapeToWing2)

ChassisToConveyor = Contact(
    type=ContactType.AUTOMATIC, category=ContactCategory.SURFACE_TO_SURFACE_CONTACT
)
ChassisToConveyor.set_tiebreak()
ChassisToConveyor.set_penalty_algorithm(
    ContactFormulation.SEGMENT_BASED_CONTACT_PENALTY, SBOPT.WRAPED_SEGMENT_CHECKING
)
conveyorsurf = ContactSurface(PartSet([9]))
conveyorsurf.set_contact_thickness(thickness=2)
chasissurf = ContactSurface(SegmentSet(segs13))
chasissurf.set_contact_thickness(thickness=2)
ChassisToConveyor.set_slave_surface(conveyorsurf)
ChassisToConveyor.set_master_surface(chasissurf)
dem.contacts.add(ChassisToConveyor)

selfcontact = Contact(type=ContactType.AUTOMATIC)
selfcontact.set_friction_coefficient(static=0.01, dynamic=0.01)
selfcontact.set_penalty_algorithm(
    ContactFormulation.SEGMENT_BASED_CONTACT_PENALTY, SBOPT.WRAPED_SEGMENT_CHECKING
)
surf1 = ContactSurface(PartSet([1, 2, 3, 5, 6, 113, 112]))
surf1.set_contact_thickness(1.5)
selfcontact.set_slave_surface(surf1)
dem.contacts.add(selfcontact)

# Define boundary conddition
dem.boundaryconditions.create_imposed_motion(
    PartSet([5]),
    Curve(x=[0.05, 1e18], y=[1, 1]),
    dof=DOF.Y_ROTATIONAL,
    motion=Motion.VELOCITY,
    scalefactor=-107.57,
)
dem.boundaryconditions.create_imposed_motion(
    PartSet([9]),
    Curve(x=[0, 1e18], y=[1, 1]),
    dof=DOF.X_TRANSLATIONAL,
    motion=Motion.VELOCITY,
    scalefactor=6666,
    birthtime=0.2,
)

# Define initial conddition
dem.initialconditions.create_velocity(
    PartSet([6, 9, 112, 113]),
    velocity=Velocity(6666, 0, 0),
    direction=Direction(1, 0, 0),
    stime=0.2,
)

# Load
g = Gravity(dir=GravityOption.DIR_Z, load=Curve(x=[0.05, 1e18], y=[1, 1]))
dem.add(g)

wing_solution.set_output_database(
    bndout=0.001, glstat=0.001, matsum=0.001, nodfor=0.001, rcforc=0.001, sleout=0.001
)
wing_solution.create_database_binary(dt=0.005, maxint=1, dcomp=2, nintsld=1)

wing_solution.save_file()
