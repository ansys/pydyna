# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
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

"""
Airbag deploy postprocessing
----------------------------
This example shows how to animate the d3plot and display the stress on the airbag.

"""

###############################################################################
# Perform required imports
# ~~~~~~~~~~~~~~~~~~~~~~~~
# Import  required imports.
#
# from ansys.dpf import core as dpf

###############################################################################
# Connect to DPF Server
# ~~~~~~~~~~~~~~~~~~~~~
# Connect to the DPF Server.
#
# dpf.connect_to_server()

###############################################################################
# Load model
# ~~~~~~~~~~
# Load the model and print the contents of the model. All parts in the model
# are shell parts. Model information includes the result components and the
# number of states available in the d3plot.
#
""" ds = dpf.DataSources()
ds.set_result_file_path(r'D:\PYDYNA_BETA_V.0.1\example-data\pydyna\Airbag\d3plot', 'd3plot')
model = dpf.Model(ds)
print(model) """

###############################################################################
# Extract stress on all parts
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract the stress on all the parts. The stress fields container is scoped to
# all time frequencies to be able to animate the change in stress on the
# airbag fabric.
#
""" stress = model.results.stress.on_all_time_freqs()
stress.inputs.data_sources(ds)
stress.inputs.requested_location.connect("Nodal")
fieldsStr = stress.outputs.fields_container()"""

###############################################################################
# Extract stress on mid integration point
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# In the d3plot file, the shell stress is reported at three through-thickness
# points by default. Extract the stress on the mid integration point.
#
""" shell_layer_extract = dpf.operators.utility.change_shell_layers()
shell_layer_extract.inputs.fields_container.connect(fieldsStr)
shell_layer_extract.inputs.e_shell_layer.connect(dpf.common.shell_layers.mid.value)
fields_top = shell_layer_extract.outputs.fields_container_as_fields_container() """

###############################################################################
# Plot deformed state
# ~~~~~~~~~~~~~~~~~~~
# Plot the deformed state at 9 milliseconds.
#
""" N = fields_top[19]
D = model.results.displacement(time_scoping=[19]).eval()
N.plot(deform_by=D[0],show_edges=False) """

###############################################################################
# Display stress field and set mesh
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Display the stress field and set the mesh to deform by the displacement of
# the nodes.
#
""" disp = model.results.displacement.on_all_time_freqs.eval()
fields_top.animate(deform_by=disp,show_edges=False) """
