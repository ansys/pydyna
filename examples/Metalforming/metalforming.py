"""
Metalforming
============
This example shows how to use the PyDYNA ``pre`` service to create
a Metalforming model. The executable file for LS-DYNA is
``ls-dyna_smp_d_R13.0_365-gf8a97bda2a_winx64_ifort190.exe``.

"""

###############################################################################
# Perform required imports
# ~~~~~~~~~~~~~~~~~~~~~~~~
# Peform the required imports.
import os
import sys

from ansys.dyna.core.pre import examples, launch_dyna
from ansys.dyna.core.pre.dynamaterial import MatRigid, MatTransverselyAnisotropicElasticPlastic
from ansys.dyna.core.pre.dynamech import (
    DOF,
    AnalysisType,
    BulkViscosity,
    Contact,
    ContactCategory,
    ContactSurface,
    ContactType,
    Curve,
    DynaMech,
    EnergyFlag,
    HourglassControl,
    MetalFormingAnalysis,
    Motion,
    NodeSet,
    PartSet,
    ShellFormulation,
    ShellPart,
)
from ansys.dyna.core.pre.misc import check_valid_ip

# sphinx_gallery_thumbnail_path = '_static/pre/explicit/ball_plate.png'

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
# the host name and port. This code uses the default localhost and port
# (``"localhost"`` and ``"50051"`` respectively).
#
hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]
solution = launch_dyna(ip=hostname)

###############################################################################
# Start the solution workflow
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NODES and ELEMENTS are read in from the ``model.k`` file. This file
# also has the *PART* defined in it, but the section and material fields are
# empty to begin with.
#
fns = []
path = examples.mf_simple_roll + os.sep
fns.append(path + "model.k")
solution.open_files(fns)

###############################################################################
# Create database and control cards
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# For the D3plots, set simulation termination time, simulation timestep, and
# output frequency.

solution.set_termination(termination_time=0.05)

mf = DynaMech(AnalysisType.NONE)
solution.add(mf)

mfanalysis = MetalFormingAnalysis()
mfanalysis.set_springback(PartSet([1]), 100)
mfanalysis.set_rigid_body_nodes_fast_update(fast_update=1)
mf.add(mfanalysis)

mf.set_timestep(timestep_size_for_mass_scaled=-7e-7)
mf.set_accuracy()
mf.set_bulk_viscosity(bulk_viscosity_type=BulkViscosity.STANDARD_BULK_VISCOSITY_SHELL)
mf.set_energy(
    hourglass_energy=EnergyFlag.COMPUTED,
    rigidwall_energy=EnergyFlag.NOT_COMPUTED,
    sliding_interface_energy=EnergyFlag.COMPUTED,
)
mf.set_hourglass(HourglassControl.FLANAGAN_BELYTSCHKO_EXACT_VOLUME_INTEGRATION_SOLID)
mf.set_output(print_suppression_d3hsp=True)
mf.create_control_shell(esort=1, istupd=1)
mf.create_control_contact(
    initial_penetration_check=2, shlthk=1, penalty_stiffness_option=4, orien=4, penetration_check_multiplier=1.0
)
mf.set_adaptive(
    time_interval_refinement=2.5e-4,
    adaptive_error_tolerance=5.0,
    adaptive_type=2,
    generate_adaptive_mesh_at_exit=1,
    min_shell_size=1.83,
    h_adaptivity_pass_flag=1,
    shell_h_adapt=5.0,
    fission_control_flag=-1,
)

###############################################################################
# Define materials
# ~~~~~~~~~~~~~~~~
# The ``dynamaterials`` class is used to define these materials: ``MAT_RIGID``,
# ``MAT_TRANSVERSELY_ANISOTROPIC_ELASTIC_PLASTIC``,

mat_upper_punch = MatRigid(
    mass_density=7.83e-9,
    young_modulus=2.07e5,
    poisson_ratio=0.28,
    center_of_mass_constraint=1,
    translational_constraint=7,
    rotational_constraint=6,
)
mat_lower_cavity = MatRigid(
    mass_density=7.83e-9,
    young_modulus=2.07e5,
    poisson_ratio=0.28,
    center_of_mass_constraint=1,
    translational_constraint=7,
    rotational_constraint=6,
)
mat_binder = MatRigid(
    mass_density=7.83e-9,
    young_modulus=2.07e5,
    poisson_ratio=0.28,
    center_of_mass_constraint=1,
    translational_constraint=7,
    rotational_constraint=7,
)
crv = Curve(x=[0, 1], y=[0, 13])
matblank = MatTransverselyAnisotropicElasticPlastic(
    mass_density=7.9e-09,
    young_modulus=2.07e5,
    yield_stress=201.3,
    anisotropic_hardening_parameter=-1.5930001,
    curve_stress=crv,
)


###############################################################################
# Define section properties and assign materials
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Now that you have materials with the material ID corresponding to
# the Part ID, you can assign these materials to the
# parts. You can also define section properties, element
# formulations, and constraints.
#

blank = ShellPart(1)
blank.set_element_formulation(ShellFormulation.FULLY_INTEGRATED_FAST)
blank.set_material(matblank)
blank.set_thickness(1.5)
blank.set_integration_points(5)
blank.set_shear_factor(0.833)
mf.parts.add(blank)

upper_punch = ShellPart(2)
upper_punch.set_material(mat_upper_punch)
upper_punch.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
upper_punch.set_integration_points(3)
mf.parts.add(upper_punch)

lower_cavity = ShellPart(3)
lower_cavity.set_material(mat_lower_cavity)
lower_cavity.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
lower_cavity.set_integration_points(3)
mf.parts.add(lower_cavity)

upper_binder = ShellPart(4)
upper_binder.set_material(mat_binder)
upper_binder.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
upper_binder.set_integration_points(3)
mf.parts.add(upper_binder)

lower_binder = ShellPart(5)
lower_binder.set_material(mat_binder)
lower_binder.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
lower_binder.set_integration_points(3)
mf.parts.add(lower_binder)


###############################################################################
# Define one_way_surface_to_surface contacts
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mfcontact = Contact(type=ContactType.FORMING, category=ContactCategory.ONE_WAY_SURFACE_TO_SURFACE)
mfcontact.set_friction_coefficient(static=0.125, dynamic=0)
mfcontact.set_extra_coefficient(viscous_damping=20)
surf1 = ContactSurface(PartSet([1]), save_interface_force=1)
surf2 = ContactSurface(PartSet([2]), save_interface_force=1)
mfcontact.set_slave_surface(surf1)
mfcontact.set_master_surface(surf2)
mf.contacts.add(mfcontact)

mfcontact = Contact(type=ContactType.FORMING, category=ContactCategory.ONE_WAY_SURFACE_TO_SURFACE)
mfcontact.set_friction_coefficient(static=0.125, dynamic=0)
mfcontact.set_extra_coefficient(viscous_damping=20)
surf1 = ContactSurface(PartSet([1]), save_interface_force=1)
surf2 = ContactSurface(PartSet([3]), save_interface_force=1)
mfcontact.set_slave_surface(surf1)
mfcontact.set_master_surface(surf2)
mf.contacts.add(mfcontact)

mfcontact = Contact(type=ContactType.FORMING, category=ContactCategory.ONE_WAY_SURFACE_TO_SURFACE)
mfcontact.set_friction_coefficient(static=0.125, dynamic=0)
mfcontact.set_extra_coefficient(viscous_damping=20)
surf1 = ContactSurface(PartSet([1]), save_interface_force=1)
surf2 = ContactSurface(PartSet([4]), save_interface_force=1)
mfcontact.set_slave_surface(surf1)
mfcontact.set_master_surface(surf2)
mf.contacts.add(mfcontact)

mfcontact = Contact(type=ContactType.FORMING, category=ContactCategory.ONE_WAY_SURFACE_TO_SURFACE)
mfcontact.set_friction_coefficient(static=0.125, dynamic=0)
mfcontact.set_extra_coefficient(viscous_damping=20)
surf1 = ContactSurface(PartSet([1]), save_interface_force=1)
surf2 = ContactSurface(PartSet([5]), save_interface_force=1)
mfcontact.set_slave_surface(surf1)
mfcontact.set_master_surface(surf2)
mf.contacts.add(mfcontact)

###############################################################################
# Define nodal single point constraints.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Constrain the nodes in a list of single point constraints (spc).

spc = [600, 593]
mf.boundaryconditions.create_spc(NodeSet(spc), tx=False, tz=False, ry=False)

mf.boundaryconditions.create_imposed_motion(
    PartSet([2]),
    Curve(x=[0, 100], y=[600, 600]),
    dof=DOF.Y_ROTATIONAL,
    motion=Motion.VELOCITY,
    scalefactor=-1,
)
mf.boundaryconditions.create_imposed_motion(
    PartSet([3]),
    Curve(x=[0, 100], y=[600, 600]),
    dof=DOF.Y_ROTATIONAL,
    motion=Motion.VELOCITY,
    scalefactor=1,
)
###############################################################################
# Define applied forces.
# ~~~~~~~~~~~~~~~~~~~~~~~~~

mf.loads.create_nodal_force(
    NodeSet([695, 696, 697, 698, 694, 693, 692]), load_curve=Curve(x=[0, 0.015, 0.016, 100], y=[100, 100, 0, 0])
)

###############################################################################
# Define database outputs
# ~~~~~~~~~~~~~~~~~~~~~~~
# Define the frequency for the D3PLOT file and write out the input file.
#
solution.set_output_database(glstat=0.00025, matsum=0.00025, rcforc=0.00025)
solution.create_database_binary(dt=5e-4)
serverpath = solution.save_file()

###############################################################################
# Download output file
# ~~~~~~~~~~~~~~~~~~~~
# Download output file from Docker image for the server to
# your local ``<working directory>/output/`` location.

serveroutfile = "/".join((serverpath, "model.k"))
downloadpath = os.path.join(os.getcwd(), "output")
if not os.path.exists(downloadpath):
    os.makedirs(downloadpath)
downloadfile = os.path.join(downloadpath, "model.k")
solution.download(serveroutfile, downloadfile)
