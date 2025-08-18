# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
# SPDX-License-Identifier: MIT
#
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# %% [markdown]
# # FRF for a Column Model with a Hole (Solid Elements)
#
# This notebook demonstrates how to set up and solve a Frequency Response Function (FRF) problem for a column with a hole using solid elements in LS-DYNA via the PyDyna API. The workflow includes mesh import, control card setup, frequency domain configuration, material and section definition, boundary conditions, and saving the input deck. Each step is explained for clarity and educational use.

# %% [markdown]
# ## 1. Perform Required Imports
# Import all necessary modules and classes for the FRF solid column simulation.

# %%
import os
import sys

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynamaterial import MatPiecewiseLinearPlasticity
from ansys.dyna.core.pre.dynanvh import (
    DynaNVH,
    EnergyFlag,
    ExcitationDOF,
    ExcitationType,
    FrequencyDomain,
    NodeSet,
    OutputEcho,
    ResponseDOF,
    ResponseType,
    SolidFormulation,
    SolidPart,
)
from ansys.dyna.core.pre.misc import check_valid_ip

# %% [markdown]
# ## 2. Start the Pre-Service
# Ensure the Docker container or local server for the `pre` service is running. Connect to it using the default hostname and port, or override via command line.

# %%
hostname = "localhost"
if len(sys.argv) > 1 and check_valid_ip(sys.argv[1]):
    hostname = sys.argv[1]
solution = launch_dynapre(ip=hostname)

# %% [markdown]
# ## 3. Import Mesh Data
# Import the mesh data (nodes, elements, and parts) for the FRF solid column example.

# %%
fns = []
path = examples.nvh_frf_solid + os.sep
fns.append(path + "frf_solid.k")
solution.open_files(fns)
solution.set_termination(termination_time=1)

# %% [markdown]
# ## 4. Define Global Control Cards
# Set up the global control cards and add the NVH object to the solution.

# %%
nvhobj = DynaNVH()
solution.add(nvhobj)

nvhobj.set_energy(hourglass_energy=EnergyFlag.COMPUTED)
nvhobj.set_output(print_suppression_d3hsp=True, print_suppression_echo=OutputEcho.SUPPRESSED_NODAL_AND_ELEMENT_PRINTING)

# %% [markdown]
# ## 5. Set Initial Timestep Size and Eigenvalue Parameters
# Set the initial timestep size and number of eigenvalues for the implicit analysis.

# %%
nvhobj.implicitanalysis.set_initial_timestep_size(1.0)
nvhobj.implicitanalysis.set_eigenvalue(number_eigenvalues=100)
nvhobj.implicitanalysis.set_solution(solution_method=1)

# %% [markdown]
# ## 6. Define Frequency Domain Cards
# Configure the frequency response function (FRF) for nodal excitations and responses.

# %%
fd = FrequencyDomain()
outputset = [
    290,
    292,
    294,
    296,
    298,
    300,
    302,
    304,
    306,
    380,
    382,
    384,
    386,
    388,
    390,
    482,
    484,
    486,
    488,
    490,
    492,
    578,
    580,
    582,
    638,
    640,
    706,
    708,
]
fd.set_frequency_response_function(
    excitation_input_dof=ExcitationDOF.X,
    excitation_input_type=ExcitationType.BASE_ACCELERATION,
    max_natural_frequency=20,
    modal_damping_coefficient=0.01,
    response_output_set=NodeSet(outputset),
    response_output_dof=ResponseDOF.X,
    response_output_type=ResponseType.BASE_ACCELERATION,
    frf_output_min_frequency=0.01,
    frf_output_max_frequency=10,
    frf_output_num_frequency=1000,
)
nvhobj.add(fd)

# %% [markdown]
# ## 7. Define Material and Section Properties
# Define the piecewise linear plasticity materials and assign them to the lower and upper solid parts.

# %%
plastic1 = MatPiecewiseLinearPlasticity(
    mass_density=4.99e-07, young_modulus=11.37, poisson_ratio=0.32, yield_stress=0.0468
)
plastic2 = MatPiecewiseLinearPlasticity(
    mass_density=4.99e-07, young_modulus=110.37, poisson_ratio=0.32, yield_stress=0.0468
)

lower = SolidPart(4)
lower.set_material(plastic1)
lower.set_element_formulation(SolidFormulation.IMPLICIT_9_POINT_ENHANCED_STRAIN)
nvhobj.parts.add(lower)

upper = SolidPart(5)
upper.set_material(plastic2)
upper.set_element_formulation(SolidFormulation.IMPLICIT_9_POINT_ENHANCED_STRAIN)
nvhobj.parts.add(upper)

# %% [markdown]
# ## 8. Define Boundary Conditions
# Apply single-point constraints (SPCs) to the specified node set.

# %%
spc = [
    163,
    166,
    169,
    172,
    175,
    178,
    181,
    184,
    187,
    307,
    310,
    313,
    316,
    319,
    322,
    391,
    394,
    397,
    400,
    403,
    406,
    493,
    496,
    499,
    589,
    592,
    645,
    648,
]
nvhobj.boundaryconditions.create_spc(NodeSet(spc))

# %% [markdown]
# ## 9. Set Output Database and Save Input File
# Configure output database frequencies and save the input file for LS-DYNA.

# %%
solution.create_database_binary(dt=0.1)
solution.set_output_database(
    glstat=0.1,
    matsum=0.1,
)
solution.save_file()

# %% [markdown]
# ## 10. Conclusion
#
# This notebook has demonstrated the setup and solution of a frequency response function (FRF) problem for a column with a hole using solid elements in LS-DYNA and PyDyna. The workflow included mesh import, control card setup, frequency domain configuration, material and section definition, boundary conditions, and saving the input deck. This approach can be adapted for other NVH analyses, providing a clear, modular, and scriptable workflow for advanced simulations.
