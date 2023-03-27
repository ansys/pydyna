"""
.. _ref_frf:
FRF for a rectangular plate
===========================

This example shows how to setup the keywords for FRF computation. \n
LS-DYNA version : ls-dyna_smp_d_R13.1_138-g8429c8a10f_winx64_ifort190.exe
"""

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
# Manually start the dyna.core.pre server
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Copy the folder pyDyna/src/ansys/dyna/core/pre/Server to a desired location
# Start the dyna.core.pre server at this location as shown below
#
# python kwserver.py
#
# Now the pre server is up and running and is waiting to be connected to the client
# Connect to the server using the hostname and the port. In this example, default
# "localhost" and port "50051" are used
hostname = "localhost"
if len(sys.argv) > 1:
    hostname = sys.argv[1]
solution = DynaSolution(hostname)
###############################################################################
# Import the initial mesh data(nodes and elements)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Mesh data is imported which includes the *NODE, *ELEMENT_ and *PART cards predefined
fns = []
path = examples.nvh_frf_plate_damping + os.sep
fns.append(path + "frf_plate_damping.k")
solution.open_files(fns)

###############################################################################
# Global Control Cards
# ~~~~~~~~~~~~~~~~~~~~

nvhobj = DynaNVH()
solution.add(nvhobj)
###############################################################################
# Below we are setting the initial timestep size in CONTROL_IMPLICIT_GENERAL
nvhobj.implicitanalysis.set_initial_timestep_size(1.0)
###############################################################################
# Number of eigen modes requested is 100
nvhobj.implicitanalysis.set_eigenvalue(number_eigenvalues=100)
###############################################################################
# Linear solver is defined by setting NSOLVR to 1 in CONTROL_IMPLICIT_SOLUTION
nvhobj.implicitanalysis.set_solution(solution_method=1)

###############################################################################
# Frequency Domain Cards
# ~~~~~~~~~~~~~~~~~~~~~~
# *FREQUENCY_DOMAIN_FRF is used to compute the frequency response function due to nodal excitations.
# In this case a base velocity is define as an input at node 131. The base acceleration response is measured at
# nodes 131 and 651. The max natural frequency employed in FRF is limited to 2000Hz.
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
# Material and Section
# ~~~~~~~~~~~~~~~~~~~~
# Linear MAT_ELASTIC is defined. S/R Hughes-Lui Elform 6 is chosen for the shell formulation.
# PROPT is set to 3 using set_printout() method. Finally save the input deck.
matelastic = MatElastic(mass_density=7870, young_modulus=2.07e11, poisson_ratio=0.292)

boxshell = ShellPart(1)
boxshell.set_material(matelastic)
boxshell.set_element_formulation(ShellFormulation.SR_HUGHES_LIU)
boxshell.set_thickness(0.002)
boxshell.set_shear_factor(0.833)
boxshell.set_printout(3)
nvhobj.parts.add(boxshell)

solution.save_file()
