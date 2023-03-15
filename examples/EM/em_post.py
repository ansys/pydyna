"""
.. _ref_em_railgun_post:
EM multi physics post processing example
--------------------------------------------

This example show how to animate the d3plot and display the electric field in the railgun.

"""
from ansys.dpf import core as dpf
from ansys.dpf.core import examples
###############################################################################
# Load the model
# ~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the model and print the contents of the model. All parts in the model are shell parts.
# Model info lists the result components as well as the number of states available in the d3plot
ds = dpf.DataSources()
ds.set_result_file_path(r'D:\PYDYNA_BETA_V.0.1\example-data\pydyna\EM\d3plot', 'd3plot')
model = dpf.Model(ds)
print(model)

mesh_scoping = dpf.mesh_scoping_factory.nodal_scoping(model.metadata.meshed_region.nodes.scoping)
time_scoping = dpf.time_freq_scoping_factory.scoping_on_all_time_freqs(model)
displacement_op = model.results.displacement
displacement_op = displacement_op.on_time_scoping(time_scoping)
displacement_fields = displacement_op.eval()

resultInfoOp = dpf.Operator("lsdyna::ms::result_info_provider")
resultInfoOp.inputs.data_sources(ds)
result_info = resultInfoOp.outputs.result_info()
print(result_info)

ms_op = dpf.Operator("lsdyna::ms::results")
ms_op.inputs.data_sources(ds)
ms_op.inputs.time_scoping(time_scoping)
fc = ms_op.eval()
print(fc)
for f in fc:
   f.meshed_region = model.metadata.meshed_region
#tfq = fc.time_freq_support
# operator needs to be evaluated to get the mesh
print("*****************************************")
fcc = dpf.FieldsContainer()
#print(len(fcc))
for i,f in enumerate(fc):
   #ms_op.inputs.time_scoping([i+1])
   if i < 62:
      f.meshed_region = model.metadata.meshed_region
      print(fc.get_field({"domain_id":2, "variable_id":1020,"time":i+1}))
      out = fc.get_field({"domain_id":2, "variable_id":1020,"time":i+1})
      fcc.add_field_by_time_id(out,i+1)
for f in fcc:
   f.meshed_region = model.metadata.meshed_region
fcc.time_freq_support = model.metadata.time_freq_support
#fcc[10].plot(title='Force field')
camera_pos = fcc.animate(scale_factor=1.,
                                         save_as="em_zoom.gif",
                                         return_cpos=True,
                                         off_screen=True,
                                         deform_by=displacement_op,
                                         cpos=c_pos,
                                         notebook=False,
                                         show_axes=True,)
# ###############################################################################
# # Let's extract the stress on all the parts. The stress field container is scoped to all time frequencies
# # so as to be able to animate the change in stress on the airbag fabric.
# stress = model.results.stress.on_all_time_freqs()
# stress.inputs.data_sources(ds)
# stress.inputs.requested_location.connect("Nodal")
# fieldsStr = stress.outputs.fields_container()
#
# ###############################################################################
# # Since the shell stress is reported at three through thickness points as default
# # in the d3plot file, the next few lines depicts how the stress
# # can be extracted on the mid integration point.
# shell_layer_extract = dpf.operators.utility.change_shell_layers()
# shell_layer_extract.inputs.fields_container.connect(fieldsStr)
# shell_layer_extract.inputs.e_shell_layer.connect(dpf.common.shell_layers.mid.value)
# fields_top = shell_layer_extract.outputs.fields_container_as_fields_container()
# ###############################################################################
# # Plot the deformed state at 9ms
# N = fields_top[19]
# D = model.results.displacement(time_scoping=[19]).eval()
# N.plot(deform_by=D[0],show_edges=False)
# ###############################################################################
# # Finally display the stress field and set the mesh to deform by the displacement of the nodes.
# disp = model.results.displacement.on_all_time_freqs.eval()
# fields_top.animate(deform_by=disp,show_edges=False)