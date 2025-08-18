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
# ## 1. Perform Required Imports
# Import required modules for DPF postprocessing.

# %%
from ansys.dpf import core as dpf

# %% [markdown]
# ## 2. Connect to DPF Server
# Connect to the DPF Server.

# %%
dpf.connect_to_server()

# %% [markdown]
# ## 3. Load Model
# Load the model and print its contents. All parts in the model are shell parts. Model information includes the result components and the number of states available in the d3plot.

# %%
ds = dpf.DataSources()
ds.set_result_file_path(r"D:\PYDYNA_BETA_V.0.1\example-data\pydyna\Airbag\d3plot", "d3plot")
model = dpf.Model(ds)
print(model)

# %% [markdown]
# ## 4. Extract Stress on All Parts
# Extract the stress on all the parts. The stress fields container is scoped to all time frequencies to enable animation of the change in stress on the airbag fabric.

# %%
stress = model.results.stress.on_all_time_freqs()
stress.inputs.data_sources(ds)
stress.inputs.requested_location.connect("Nodal")
fieldsStr = stress.outputs.fields_container()

# %% [markdown]
# ## 5. Extract Stress on Mid Integration Point
# In the d3plot file, the shell stress is reported at three through-thickness points by default. Extract the stress on the mid integration point.

# %%
shell_layer_extract = dpf.operators.utility.change_shell_layers()
shell_layer_extract.inputs.fields_container.connect(fieldsStr)
shell_layer_extract.inputs.e_shell_layer.connect(dpf.common.shell_layers.mid.value)
fields_top = shell_layer_extract.outputs.fields_container_as_fields_container()

# %% [markdown]
# ## 6. Plot Deformed State
# Plot the deformed state at 9 milliseconds.

# %%
N = fields_top[19]
D = model.results.displacement(time_scoping=[19]).eval()
N.plot(deform_by=D[0], show_edges=False)

# %% [markdown]
# ## 7. Display Stress Field and Animate
# Display the stress field and set the mesh to deform by the displacement of the nodes.

# %%
disp = model.results.displacement.on_all_time_freqs.eval()
fields_top.animate(deform_by=disp, show_edges=False)

# %% [markdown]
# ## 8. Conclusion
# This notebook demonstrated postprocessing of an airbag deployment simulation using LS-DYNA and PyDyna/DPF. The workflow included connecting to the DPF server, loading the model, extracting and visualizing stress results, and animating the deformed state. This approach provides a clear, modular, and scriptable workflow for advanced safety simulation postprocessing.
