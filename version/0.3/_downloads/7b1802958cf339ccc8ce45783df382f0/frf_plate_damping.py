"""
FRF for a rectangular plate
===========================

This example shows how to set up the keywords for computation of a FRF (frequency response function).
The executable file for LS-DYNA is ``ls-dyna_smp_d_R13.1_138-g8429c8a10f_winx64_ifort190.exe``.

"""
###############################################################################
# Perform required imports
# ~~~~~~~~~~~~~~~~~~~~~~~~
# Peform required imports.
#
import os
import sys


from ansys.dyna.core.pre.dynasolution import DynaSolution
from ansys.dyna.core.pre.dynamaterial import MatElastic
from ansys.dyna.core.pre.dynanvh import (
    DynaNVH,
    FrequencyDomain,
    ExcitationDOF,
    Curve,
    NodeSet,
    ShellPart,
    ShellFormulation,
    ResponseDOF,
    ResponseType,
)
from ansys.dyna.core.pre import examples
# sphinx_gallery_thumbnail_path = '_static/pre/nvh/frf_plate_damping.png'
###############################################################################
# Manually start the ``pre`` service
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Copy the ``pyDyna/src/ansys/dyna/core/pre/Server``folder to a desired location.
# Start the ``pre`` service at this location by running this command:
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
solution = DynaSolution(hostname)
###############################################################################
# Import the initial mesh data (nodes and elements)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import mesh data, which includes the predefined *NODE*, *ELEMENT_* and *PART*
# cards.
#
fns = []
path = examples.nvh_frf_plate_damping + os.sep
fns.append(path + "frf_plate_damping.k")
solution.open_files(fns)

###############################################################################
# Define global control cards
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define the global control cards.

nvhobj = DynaNVH()
solution.add(nvhobj)
###############################################################################
# Set initial timestep size
# ~~~~~~~~~~~~~~~~~~~~~~~~~
# Set the initial timestep size in CONTROL_IMPLICIT_GENERAL.
#
nvhobj.implicitanalysis.set_initial_timestep_size(1.0)

###############################################################################
# Set number of eigen modes
# ~~~~~~~~~~~~~~~~~~~~~~~~~
# Set the number of eigen modes to ``100``.
#
nvhobj.implicitanalysis.set_eigenvalue(number_eigenvalues=100)

###############################################################################
# Define linear solver
# ~~~~~~~~~~~~~~~~~~~~
# Define the linear solver by setting NSOLVR to ``1`` in CONTROL_IMPLICIT_SOLUTION.
nvhobj.implicitanalysis.set_solution(solution_method=1)

###############################################################################
# Define frequency domain cards
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# *FREQUENCY_DOMAIN_FRF* is used to compute the frequency response function due to
# nodal excitations. In this case, a base velocity is define as an input at node 131.
# The base acceleration response is measured at nodes 131 and 651. The maximum
# natural frequency employed in FRF is limited to 2000 Hz.
fd = FrequencyDomain()
crv = Curve(
    x=[1, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 200],
    y=[0, 0, 0.0045, 0.00713, 0.00386, 0.00328, 0.0034, 0.00624, 7.2e-4, 8.3e-4, 0, 0],
)
fd.set_frequency_response_function(
    excitation_input_set=NodeSet([131]),
    excitation_input_dof=ExcitationDOF.Z,
    max_natural_frequency=2000,
    modal_damping_coefficient_curve_type=1,
    modal_damping_coefficient_curve=crv,
    response_output_set=NodeSet([131, 651]),
    response_output_dof=ResponseDOF.Z,
    response_output_type=ResponseType.BASE_ACCELERATION,
    frf_output_min_frequency=1,
    frf_output_max_frequency=400,
    frf_output_num_frequency=400,
)
nvhobj.add(fd)

###############################################################################
# Define material and section properties
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define the ``MAT_ELASTIC`` material. Set shell formulation to ``SR_HUGHES_LIU.
#
matelastic = MatElastic(mass_density=7870, young_modulus=2.07e11, poisson_ratio=0.292)

boxshell = ShellPart(1)
boxshell.set_material(matelastic)
boxshell.set_element_formulation(ShellFormulation.SR_HUGHES_LIU)
boxshell.set_thickness(0.002)
boxshell.set_shear_factor(0.833)

###############################################################################
# Set printout property and save input deck 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use the ``set_printout()`` method to set the printout property to ``3``.
# Save the input deck.
#
boxshell.set_printout(3)
nvhobj.parts.add(boxshell)

solution.save_file()