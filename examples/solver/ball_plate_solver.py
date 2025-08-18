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
# # Ball-Plate Solver Example
#
# This notebook demonstrates how to launch and control an LS-DYNA simulation for a ball impacting a plate using the PyDyna solver API. The workflow includes connecting to the solver service, uploading the input file, starting the solver, and running the simulation. Each step is explained with theoretical background for educational use.
#
# **Background:**
# In explicit dynamics, the ball-plate impact problem is a classic benchmark for studying contact, large deformation, and wave propagation in solids. LS-DYNA's parallel solver (MPP) is used here to efficiently simulate the transient response of the system.

# %% [markdown]
# ## 1. Import the Solver API
# Import the PyDyna solver module, which provides a Python interface to control LS-DYNA solver instances.
#
# The solver API allows you to connect to a running LS-DYNA container or server, manage input/output files, and execute simulations programmatically.

# %%
import ansys.dyna.core.solver as solver

# %% [markdown]
# ## 2. Connect to the LS-DYNA Solver Service
# Define the hostname and port for the LS-DYNA solver service and establish a connection.
#
# The solver service can be run in a Docker container or on a remote server. Here, we use the default localhost and port 5000.

# %%
hostname = "localhost"
port = "5000"
dyna = solver.DynaSolver(hostname, port)  # connect to the container

# %% [markdown]
# ## 3. Upload the Input File
# Push the prepared LS-DYNA keyword input file to the solver's working directory.
#
# The input file defines the geometry, materials, boundary conditions, and analysis parameters for the ball-plate impact simulation.

# %%
dyna.push("./output/ball_plate.k")  # push an input file

# %% [markdown]
# ## 4. Start the Solver with Multiple Ranks
# Start the LS-DYNA solver using multiple ranks (parallel processes) for efficient computation.
#
# Parallel processing (MPP) is essential for large or complex simulations, as it distributes the workload across multiple CPU cores or nodes.

# %%
dyna.start(4)  # start 4 ranks of mppdyna

# %% [markdown]
# ## 5. Run the Simulation
# Execute the LS-DYNA simulation with the specified input file and runtime options.
#
# The run command specifies the input file, memory allocation, and number of cycles. Adjust these parameters as needed for your simulation.

# %%
dyna.run("i=ball_plate.k memory=10m ncycle=20000")  # begin execution

# %% [markdown]
# ## 6. Conclusion
#
# This notebook has demonstrated how to launch and control an LS-DYNA simulation for a ball-plate impact using the PyDyna solver API. The workflow included connecting to the solver, uploading the input file, starting the solver in parallel, and running the simulation. This approach can be adapted for other explicit dynamics problems, providing a clear, modular, and scriptable workflow for advanced simulations.
