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
.. _ref_em_railgun_post:
EM multi physics post processing example
----------------------------------------

This example show how to animate the d3plot and display the electric field in the railgun.

"""

from ansys.dpf import core as dpf

###############################################################################
# Connect to DPF
# ~~~~~~~~~~~~~~
dpf.connect_to_server()

###############################################################################
# Load the model
# ~~~~~~~~~~~~~~
# Load the model and print the contents of the model. Since this is a multiphysics problem
# the default results returned is the structural results.
#
ds = dpf.DataSources()
ds.set_result_file_path(r"D:\PYDYNA_BETA_V.0.1\example-data\pydyna\EM\d3plot", "d3plot")
model = dpf.Model(ds)
print(model)

###############################################################################
# Get MS mesh
# ~~~~~~~~~~~
# We now define an operator to extract the mesh from the EM solver.
# "lsdyna::ms::meshs_provider" is the operator we can connect to get the mesh.
# Since the meshes container contains the mesh for all time states, we need to scope it to the
# desired timestate.
#
meshOP = dpf.Operator("lsdyna::ms::meshes_provider")
meshOP.inputs.data_sources.connect(ds)
timeScoping = dpf.Scoping()
timeScoping.ids = list(range(1, 21))
meshOP.inputs.time_scoping.connect(timeScoping)
meshes = meshOP.outputs.meshes()
mesh = meshes.get_mesh({"time": 1})

###############################################################################
# MS Result Info
# ~~~~~~~~~~~~~~
# "result_info_provider" lets us list all the variables available in the container.
#
resultInfoOp = dpf.Operator("lsdyna::ms::result_info_provider")
resultInfoOp.inputs.data_sources(ds)
result_info = resultInfoOp.outputs.result_info()
print(result_info)

###############################################################################
# Get Field Variable from the available results
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The actual field variable of interest is extracted from the "lsdyna::ms::results" operator.
# The fields extracted from this operator is first associated with a mesh. The variable of interest is
# the electric field in this case. This can be retrieved from specifying the right Domain ID and the Variable ID
# In this case Domain=0 and Variable=1014 represents the electric field as seen from the output above.
# electric_fielddomain_id__0__variable_id__1014: Elemental Electric Field(domain Id: 0, Variable Id: 1014)
#
ms_op = dpf.Operator("lsdyna::ms::results")
ms_op.inputs.data_sources(ds)
ms_op.inputs.time_scoping([44])
fields = ms_op.outputs.results()
for f in fields:
    f.meshed_region = mesh
field0 = fields.get_field({"domain_id": 0, "variable_id": 1014})
print(field0)

###############################################################################
# Plot the Electric Field
# ~~~~~~~~~~~~~~~~~~~~~~~
# Now that we have the field of interest, we can plot it at any given state. In order to display the mesh at that
# state, we need to extract the displacement and deform the field by the displacement field which is shown below.
#
disp = model.results.displacement(time_scoping=[44]).eval()
c_pos = [
    (346.9131285482345, 313.2551112639297, 39.299903249251045),
    (101.24994659423828, 0.0, 0.0),
    (-0.09694442015878338, -0.04868966261897064, 0.9940981320544406),
]
field0.plot(deform_by=disp[0], show_edges=False, cpos=c_pos)
