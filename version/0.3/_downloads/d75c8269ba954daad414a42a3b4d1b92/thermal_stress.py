"""
Thermal stress example
======================

This example shows how to create a thermal stress model with the PyDYNA ``pre`` service.
The executable file for LS-DYNA is ``ls-dyna_smp_s_R13.0_365-gf8a97bda2a_winx64_ifort190.exe``.

"""
###############################################################################
# Perform required imports
# ~~~~~~~~~~~~~~~~~~~~~~~~
# Peform required imports.
#
import os
import sys


from ansys.dyna.core.pre.dynasolution import DynaSolution
from ansys.dyna.core.pre.dynamech import (
    DynaMech,
    ThermalAnalysis,
    ThermalAnalysisType,
    SolidPart,
    SolidFormulation,
    NodeSet,
    AnalysisType
)
from ansys.dyna.core.pre.dynamaterial import MatElasticPlasticThermal
from ansys.dyna.core.pre import examples
# sphinx_gallery_thumbnail_path = '_static/pre/thermal/thermal.png'
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
# Start the solution workflow
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NODES and ELEMENTS are read in from the ``thermal_stress.k`` file. This file also has the
# *PART* defined in it, but the section and material fields are empty to begin with.
fns = []
path = examples.thermal_stress + os.sep
fns.append(path + "thermal_stress.k")
solution.open_files(fns)

###############################################################################
# Set simulation termination time
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set the simulation termination time.
#
solution.set_termination(3.0)

###############################################################################
# To invoke the transient thermal solver, set the thermal analysis type for
# ``CONTROL_SOLUTION`` to 2 by ``ThermalAnalysisType.TRANSIENT``.
#
ts = DynaMech(analysis=AnalysisType.EXPLICIT)
solution.add(ts)

tanalysis = ThermalAnalysis()
tanalysis.set_timestep(initial_timestep=0.1)
tanalysis.set_solver(analysis_type=ThermalAnalysisType.TRANSIENT)
ts.add(tanalysis)

ts.set_timestep(timestep_size_for_mass_scaled=0.01)

###############################################################################
# Define material and section properties
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define the ``MAT_4`` material, which can have temperature-dependent
# properties. For the ``MAT_THERMAL_ISOTROPIC`` property, which is associated
# with the same part, define the specific heat, thermal conductivity, and thermal
# generation rate.
#
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
# Set initial conditions
# ~~~~~~~~~~~~~~~~~~~~~~~
# Initialize nodes 1 through 8 with a temperature of 10 degrees.
#
for i in range(1,9):
    ts.initialconditions.create_temperature(NodeSet([i]),temperature=10)

###############################################################################
# Define output frequencies and save input file
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define output frequencies and save the input file to disk.
#
solution.set_output_database(glstat=0.03)
solution.create_database_binary(dt=0.01)
solution.save_file()
