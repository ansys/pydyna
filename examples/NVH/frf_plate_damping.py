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
# # FRF Plate Damping Example
#
# This notebook demonstrates how to set up and solve a Frequency Response Function (FRF) plate damping problem using the PyDyna API for LS-DYNA. The workflow includes mesh import, control card setup, frequency domain configuration, material and section definition, and saving the input deck. Each step is explained for clarity and educational use.

# %% [markdown]
# ## 1. Perform Required Imports
# Import all necessary modules and classes for the FRF plate damping simulation.

# %%
import os
import sys

from ansys.dyna.core.pre import examples, launch_dynapre
from ansys.dyna.core.pre.dynamaterial import MatElastic
from ansys.dyna.core.pre.dynanvh import (
    Curve,
    DynaNVH,
    ExcitationDOF,
    FrequencyDomain,
    NodeSet,
    ResponseDOF,
    ResponseType,
    ShellFormulation,
    ShellPart,
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
# Import the mesh data (nodes, elements, and parts) for the FRF plate damping example.

# %%
fns = []
path = examples.nvh_frf_plate_damping + os.sep
fns.append(path + "frf_plate_damping.k")
solution.open_files(fns)

# %% [markdown]
# ## 4. Define Global Control Cards
# Set up the global control cards and add the NVH object to the solution.

# %%
nvhobj = DynaNVH()
solution.add(nvhobj)

# %% [markdown]
# ## 5. Set Initial Timestep Size
# Set the initial timestep size in CONTROL_IMPLICIT_GENERAL.

# %%
nvhobj.implicitanalysis.set_initial_timestep_size(1.0)

# %% [markdown]
# ## 6. Set Number of Eigen Modes
# Set the number of eigen modes to 100.

# %%
nvhobj.implicitanalysis.set_eigenvalue(number_eigenvalues=100)

# %% [markdown]
# ## 7. Define Linear Solver
# Set the linear solver method (NSOLVR) to 1 in CONTROL_IMPLICIT_SOLUTION.

# %%
nvhobj.implicitanalysis.set_solution(solution_method=1)

# %% [markdown]
# ## 8. Define Frequency Domain Cards
# Configure the frequency response function (FRF) for nodal excitations and responses.

# %%
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

# %% [markdown]
# ## 9. Define Material and Section Properties
# Define the elastic material and shell part properties for the plate.

# %%
matelastic = MatElastic(mass_density=7870, young_modulus=2.07e11, poisson_ratio=0.292)

boxshell = ShellPart(1)
boxshell.set_material(matelastic)
boxshell.set_element_formulation(ShellFormulation.SR_HUGHES_LIU)
boxshell.set_thickness(0.002)
boxshell.set_shear_factor(0.833)

# %% [markdown]
# ## 10. Set Printout Property and Save Input Deck
# Set the printout property and save the input deck for LS-DYNA.

# %%
boxshell.set_printout(3)
nvhobj.parts.add(boxshell)

solution.save_file()

# %% [markdown]
# ## 11. Conclusion
#
# This notebook has demonstrated the setup and solution of a frequency response function (FRF) plate damping example using PyDyna and LS-DYNA. The workflow included mesh import, control card setup, frequency domain configuration, material and section definition, and saving the input deck. This approach can be adapted for other NVH analyses, providing a clear, modular, and scriptable workflow for advanced simulations.
#
