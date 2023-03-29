"""
.. _ref_thermal_stress:
Thermal stress example
======================

This example show how to create a thermal stress model with Pydyna-pre module. \n
LS-DYNA version : ls-dyna_smp_s_R13.0_365-gf8a97bda2a_winx64_ifort190.exe
"""

import os
import sys


from ansys.dyna.core.pre.dynasolution import DynaSolution
from ansys.dyna.core.pre.dynamech import (
    DynaMech,
    ThermalAnalysis,
    ThermalAnalysisType,
    SolidPart,
    SolidFormulation,
    NodeSet
)
from ansys.dyna.core.pre.dynamaterial import MatElasticPlasticThermal
from ansys.dyna.core.pre import examples
# sphinx_gallery_thumbnail_path = '_static/pre/thermal/thermal.png'
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
# Start the Solution workflow
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NODES and ELEMENTS are read in from the "thermal_stress.k" file. This file also has the
# *PART defined in it but the section and material fields are empty to begin with
fns = []
path = examples.thermal_stress + os.sep
fns.append(path + "thermal_stress.k")
solution.open_files(fns)
###############################################################################
# Setting simulation termination time
solution.set_termination(3.0)
###############################################################################
# To invoke the transient thermal solver, the thermal analysis type in CONTROL_SOLUTION is
# being set to 2 by ThermalAnalysisType.TRANSIENT.
ts = DynaMech()
solution.add(ts)

tanalysis = ThermalAnalysis()
tanalysis.set_timestep(initial_timestep=0.1)
tanalysis.set_solver(analysis_type=ThermalAnalysisType.TRANSIENT)
ts.add(tanalysis)

ts.set_timestep(timestep_size_for_mass_scaled=0.01)
###############################################################################
# Material and Section
# ~~~~~~~~~~~~~~~~~~~~
# MAT_4 is defined here which can have temperature dependent material properties. Specific heat, thermal conductivity and
# the thermal generation rate are defined in MAT_THERMAL_ISOTROPIC and associated with the same part.
mat = MatElasticPlasticThermal(
    mass_density=1.0,
    temperatures=(0,10,20,30,40,50),
    young_modulus=(1e10,1e10,1e10,1e10,1e10,1e10),
    poisson_ratio=(0.3,0.3,0.3,0.3,0.3,0.3),
    thermal_expansion=(0,2e-6,4e-6,6e-6,8e-6,1e-5),
    yield_stress = (1e20,1e20,1e20,1e20,1e20,1e20)
)
mat.set_thermal_isotropic(density=1,generation_rate_multiplier=10,specific_heat=1,conductivity=1)

slab = SolidPart(1)
slab.set_material(mat)
slab.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
ts.parts.add(slab)

###############################################################################
# Initial Condition
# ~~~~~~~~~~~~~~~~~
# Nodes 1 through 8 are initialized with a temperature of 10 deg
for i in range(1,9):
    ts.initialconditions.create_temperature(NodeSet([i]),temperature=10)
###############################################################################
# Output frequencies are defined and the input file is saved to disk.
solution.set_output_database(glstat=0.03)
solution.create_database_binary(dt=0.01)
solution.save_file()
