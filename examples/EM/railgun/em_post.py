# %% [markdown]
# # EM Railgun Multi-Physics Post-Processing with LS-DYNA and DPF
#
# This notebook demonstrates how to post-process electromagnetic (EM) railgun simulation results using
# LS-DYNA and DPF (Data Processing Framework). The workflow includes connecting to DPF, loading multi-physics
# results, extracting electromagnetic field data, and visualizing electric field distributions. Each step is
# explained for clarity and educational use.
#
# ## Theory and Background
#
# Railgun simulations involve complex electromagnetic-structural coupling where electric current flows through
# conductors, generating magnetic fields that produce Lorentz forces. These forces accelerate projectiles and
# deform the rail structure. Post-processing such multi-physics results requires specialized tools like DPF
# that can handle both structural and electromagnetic field data simultaneously.
#
# This example shows how to animate the d3plot results and display the electric field distribution in the
# railgun geometry, providing insights into the electromagnetic behavior during the launch sequence.

# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
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
# ## 1. Import Required Modules
# Import the DPF (Data Processing Framework) module for post-processing LS-DYNA results.

# %%
from ansys.dpf import core as dpf

# %% [markdown]
# ## 2. Connect to DPF Server
# Establish connection to the DPF server for processing simulation results.

# %%
dpf.connect_to_server()

# %% [markdown]
# ## 3. Load the Multi-Physics Model
# Load the model and print the contents. Since this is a multi-physics problem, the default results
# returned are the structural results.

# %%
ds = dpf.DataSources()
ds.set_result_file_path(r"D:\PYDYNA_BETA_V.0.1\example-data\pydyna\EM\d3plot", "d3plot")
model = dpf.Model(ds)
print(model)

# %% [markdown]
# ## 4. Extract EM Mesh Data
# Define an operator to extract the mesh from the EM solver. The "lsdyna::ms::meshes_provider" operator
# is used to get the mesh. Since the meshes container contains the mesh for all time states, we scope it
# to the desired time state.

# %%
meshOP = dpf.Operator("lsdyna::ms::meshes_provider")
meshOP.inputs.data_sources.connect(ds)
timeScoping = dpf.Scoping()
timeScoping.ids = list(range(1, 21))
meshOP.inputs.time_scoping.connect(timeScoping)
meshes = meshOP.outputs.meshes()
mesh = meshes.get_mesh({"time": 1})

# %% [markdown]
# ## 5. Get EM Result Information
# Use the "result_info_provider" operator to list all the variables available in the container.

# %%
resultInfoOp = dpf.Operator("lsdyna::ms::result_info_provider")
resultInfoOp.inputs.data_sources(ds)
result_info = resultInfoOp.outputs.result_info()
print(result_info)

# %% [markdown]
# ## 6. Extract Electric Field Variable
# Extract the electric field variable from the available results using the "lsdyna::ms::results" operator.
# The variable of interest is the electric field, which can be retrieved by specifying the correct Domain ID
# and Variable ID. In this case, Domain=0 and Variable=1014 represents the electric field.

# %%
ms_op = dpf.Operator("lsdyna::ms::results")
ms_op.inputs.data_sources(ds)
ms_op.inputs.time_scoping([44])
fields = ms_op.outputs.results()
for f in fields:
    f.meshed_region = mesh
field0 = fields.get_field({"domain_id": 0, "variable_id": 1014})
print(field0)

# %% [markdown]
# ## 7. Visualize the Electric Field
# Plot the electric field at the specified time state. To display the mesh at that state, we extract the
# displacement field and deform the electric field visualization by the displacement to show the coupled
# electromagnetic-structural response.

# %%
disp = model.results.displacement(time_scoping=[44]).eval()
c_pos = [
    (346.9131285482345, 313.2551112639297, 39.299903249251045),
    (101.24994659423828, 0.0, 0.0),
    (-0.09694442015878338, -0.04868966261897064, 0.9940981320544406),
]
field0.plot(deform_by=disp[0], show_edges=False, cpos=c_pos)

# %% [markdown]
# ## 8. Conclusion
# This notebook demonstrated the post-processing of electromagnetic railgun simulation results using LS-DYNA
# and DPF. The workflow included connecting to DPF, loading multi-physics data, extracting electromagnetic
# field variables, and visualizing electric field distributions with structural deformation. This approach
# provides powerful capabilities for analyzing complex electromagnetic-structural coupling in railgun systems.
