"""
.. _ref_airbag_deploy_post:
Airbag deploy post processing example
-------------------------------------

This example show how to animate the d3plot and display the stress on the airbag.

"""
from ansys.dpf import core as dpf
from ansys.dpf.core import examples
###############################################################################
# Connect to DPF
# ~~~~~~~~~~~~~~
dpf.connect_to_server()

###############################################################################
# Load the model
# ~~~~~~~~~~~~~~
# Load the model and print the contents of the model. All parts in the model are shell parts.
# Model info lists the result components as well as the number of states available in the d3plot

ds = dpf.DataSources()
ds.set_result_file_path(r'D:\PYDYNA_BETA_V.0.1\example-data\pydyna\Airbag\d3plot', 'd3plot')
model = dpf.Model(ds)
print(model)
###############################################################################
# Let's extract the stress on all the parts. The stress field container is scoped to all time frequencies
# so as to be able to animate the change in stress on the airbag fabric.
stress = model.results.stress.on_all_time_freqs()
stress.inputs.data_sources(ds)
stress.inputs.requested_location.connect("Nodal")
fieldsStr = stress.outputs.fields_container()

###############################################################################
# Since the shell stress is reported at three through thickness points as default
# in the d3plot file, the next few lines depicts how the stress
# can be extracted on the mid integration point.
shell_layer_extract = dpf.operators.utility.change_shell_layers()
shell_layer_extract.inputs.fields_container.connect(fieldsStr)
shell_layer_extract.inputs.e_shell_layer.connect(dpf.common.shell_layers.mid.value)
fields_top = shell_layer_extract.outputs.fields_container_as_fields_container()
###############################################################################
# Plot the deformed state at 9ms
N = fields_top[19]
D = model.results.displacement(time_scoping=[19]).eval()
N.plot(deform_by=D[0],show_edges=False)
###############################################################################
# Finally display the stress field and set the mesh to deform by the displacement of the nodes.
disp = model.results.displacement.on_all_time_freqs.eval()
fields_top.animate(deform_by=disp,show_edges=False)