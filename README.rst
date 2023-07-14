Overview
========
PyDYNA is a Pythonic package for providing a more convenient and complete way to
build an Ansys DYNA input deck, submit it to the Ansys LS-DYNA solver, and
finally postprocess the results. 

In the PyDYNA installation, the ``docker`` directory has two child
directories:

- ``pre``: Provides the interface for creating DYNA input decks.
  This service includes highly abstracted APIs for setting up
  LS-DYNA input decks. Included are DynaMech, DynaIGA, DynaICFD,
  DynaSALE, DynaEM, and DynaAirbag.
- ``solver``: Contains the code for interfacing directly with
  the Ansys LS-DYNA solver. Because LS-DYNA is primarily a batch
  solver with very limited interactive capabilities, the code in
  this directory is similarly limited. The target use case is that
  LS-DYNA is running in a container environment such as Docker or
  Kubernetes. The code in the ``solver`` directory allows you to push
  input files to the container, start LS-DYNA and monitor its progress,
  and then retrieve results (RST) files.`

Once you have results, you can use the Ansys Data Processing Framework (DPF),
which is designed to provide numerical simulation users and engineers
with a toolbox for accessing and transforming simulation data. DPF
can access data from Ansys solver result files and from several
files with neutral formats, including CSV, HDF5, and VTK. Using DPF's
various operators, you can manipulate and transform this data.

The `ansys-dpf-post package <https://github.com/ansys/pydpf-post>`_ provides
a simplified Python interface to DPF, thus enabling rapid postprocessing
without ever leaving a Python environment. For more information on DPF-Post,
see the `DPF-Post documentation <https://post.docs.pyansys.com>`_.

Documentation
=============
For comprehesive information on PyDYNA, see the latest release
`documentation <https://dyna.docs.pyansys.com/>`_.

On the `PyDyna Issues <https://github.com.mcas.ms/pyansys/pyDyna/issues>`_ page, you can create
issues to submit questions, report bugs, and request new features. To reach
the PyAnsys support team, email `pyansys.support@ansys.com <pyansys.support@ansys.com>`_.

Usage
=====
Here is a basic preprocessing example:

.. code:: python

    import os
	import sys
	from ansys.dyna.core.pre.dynasolution import DynaSolution
	from ansys.dyna.core.pre.dynaicfd import (
		DynaICFD,
		ICFDAnalysis,
		MatICFD,
		ICFDPart,
		ICFDDOF,
		Curve,
		ICFDVolumePart,
		MeshedVolume,
	)
	from ansys.dyna.core.pre import examples
	# sphinx_gallery_thumbnail_path = '_static/pre/icfd/cylinderflow.png'

	hostname = "localhost"
	if len(sys.argv) > 1:
		hostname = sys.argv[1]

	icfd_solution = DynaSolution(hostname)
	# Import the initial mesh data(nodes and elements)
	fns = []
	path = os.getcwd()+os.sep
	fns.append(path+"cylinder_flow.k")
	icfd_solution.open_files(fns)
	# Set total time of simulation
	icfd_solution.set_termination(termination_time=100)

	icfd = DynaICFD()
	icfd_solution.add(icfd)

	icfdanalysis = ICFDAnalysis()
	icfdanalysis.set_timestep()
	icfd.add(icfdanalysis)

	# define model
	mat = MatICFD(flow_density=1.0, dynamic_viscosity=0.005)

	part_inflow = ICFDPart(1)
	part_inflow.set_material(mat)
	part_inflow.set_prescribed_velocity(dof=ICFDDOF.X, motion=Curve(x=[0, 10000], y=[1, 1]))
	part_inflow.set_prescribed_velocity(dof=ICFDDOF.Y, motion=Curve(x=[0, 10000], y=[0, 0]))
	icfd.parts.add(part_inflow)

	part_outflow = ICFDPart(2)
	part_outflow.set_material(mat)
	part_outflow.set_prescribed_pressure(pressure=Curve(x=[0, 10000], y=[0, 0]))
	icfd.parts.add(part_outflow)

	part_symmetric = ICFDPart(3)
	part_symmetric.set_material(mat)
	part_symmetric.set_free_slip()
	icfd.parts.add(part_symmetric)

	part_wall = ICFDPart(4)
	part_wall.set_material(mat)
	part_wall.set_non_slip()
	part_wall.compute_drag_force()
	part_wall.set_boundary_layer(number=3)
	icfd.parts.add(part_wall)

	partvol = ICFDVolumePart(surfaces=[1, 2, 3, 4])
	partvol.set_material(mat)
	icfd.parts.add(partvol)
	# define the volume space that will be meshed,The boundaries
	# of the volume are the surfaces "spids"
	meshvol = MeshedVolume(surfaces=[1, 2, 3, 4])
	icfd.add(meshvol)

	icfd_solution.create_database_binary(dt=1)
	serverpath = icfd_solution.save_file()
	serveroutfile = '/'.join((serverpath,"cylinder_flow.k"))
	downloadpath = os.path.join(os.getcwd(), "output")
	if not os.path.exists(downloadpath):
		os.makedirs(downloadpath)
	downloadfile = os.path.join(downloadpath,"cylinder_flow.k")
	icfd_solution.download(serveroutfile,downloadfile)
	
Here is a basic solving example:

.. code:: python
   >>> hostname = "localhost"
   >>> port = "5000"
   >>> import ansys.dyna.core.solver as solver
   >>> dyna=solver.DynaSolver(hostname,port)           # connect to the container
   >>> dyna.push("cylinder_flow.k")                            # push an input file
   >>> dyna.start(4)                                   # start 4 ranks of mppdyna
   >>> dyna.run("i=./output/cylinder_flow.k memory=10m ncycle=20000")   # begin execution

Here is a basic postprocessing example:

.. code:: python

	 from ansys.dpf import core as dpf

	 ds = dpf.DataSources()
	 ds.set_result_file_path(r'./d3plot', 'd3plot')

	 resultOp = dpf.Operator("lsdyna::d3plot::stress_von_mises")
	 resultOp.inputs.data_sources(ds)
	 # set the time
	 resultOp.inputs.time_scoping.connect([3])
	 result = resultOp.outputs.stress_von_mises()

For more examples, see `Examples <https://dyna.docs.pyansys.com/version/stable/examples/index.html>`_
in the PyDYNA documentation.

License
=======
PyDYNA is licensed under the MIT license.

PyDYNA makes no commercial claim over Ansys whatsoever. This libray extends the functionality of
Ansys LS-DYNA by adding a Python interface to LS-DYNA without changing the core behavior or
license of the original software. The use of the interactive control of PyDYNA requires a legally
licensed local copy of LS-DYNA.

For more information on LS-DYNA, see the
`Ansys LS-DYNA <https://www.ansys.com/products/structures/ansys-ls-dyna>`_
page on the Ansys website.

.. LINKS AND REFERENCES
.. _pip: https://pypi.org/project/pip/
.. _PyAnsys Developer's Guide: https://dev.docs.pyansys.com/