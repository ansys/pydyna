PyDyna
######

PyDyna is a pythonic dyna package that aims to provide user a more convenient and complete way to
build up ansys-dyna input deck, submit to solver and finally post processing their results. 


Project Overview
----------------
There are 3 related packages here, pre and solver are all under the ansys/pydyna/ directory,
while pyDPF is used for post-processing.

pre contains highly abstracted APIs for setting up a LS-DYNA input deck, so far, 
it includes DynaMech, DynaIGA, DynaICFD, DynaSALE, DynaEM, DynaAirbag and so on.

solver contains code for interfacing with the LS-DYNA solver directly.
As LS-DYNA is primarily a batch solver with very limited interactive
capabilities, the code here is similarly limited.  The target
use case is that LS-DYNA will be running in a container environment
such as Docker or Kubernetes.  The code here then allows for pushing
input files to the container, starting LS-DYNA and monitoring its
progress, and retrieving results files.

The Data Processing Framework (DPF) is designed to provide numerical
simulation users/engineers with a toolbox for accessing and
transforming simulation data. DPF can access data from solver result
files as well as several neutral formats (csv, hdf5, vtk,
etc.). Various operators are available allowing the manipulation and
the transformation of this data.

The Python `ansys-dpf-post` package provides a simplified Python
interface to DPF, thus enabling rapid postprocessing without ever
leaving a Python environment. 

Visit the `DPF-Post Documentation <https://postdocs.pyansys.com>`_ for a
detailed description of the package.

Install PyDyna-Pre Docker
-------------------------

Launching the PyDyna-Pre service locally,the only requirement are that:

* Docker is installed on your machine.

.. caution::

   The PyDyna-Pre service is currently available only as a Linux Docker image. 
   make sure that your Docker engine is configured to run Linux Docker images.

Please refer to /docker/README.rst to install PyDyna-Pre service docker container

Install PyDyna-Solver Docker
----------------------------

Once pydyna is installed, the docker-compose.yml file to build and launch the dyna solver docker can be located
under /docker . The yml file can be copied locally. To run the docker the following command can be used

.. code:: bash
    
	docker-compose up


Install the package
-------------------

PyDyna has three installation modes: user, developer, and offline.

Install in user mode
^^^^^^^^^^^^^^^^^^^^

Before installing PyDyna in user mode, make sure you have the latest version of
`pip`_ with:

.. code:: bash

   python -m pip install -U pip

Then, install PyDyna with:

.. code:: bash

   python -m pip install ansys-dyna-core

.. caution::

    PyDyna is currently hosted in a private PyPI repository. You must provide the index
    URL to the private PyPI repository:

    * Index URL: ``https://pkgs.dev.azure.com/pyansys/_packaging/pyansys/pypi/simple/``

    If access to this package registry is needed, email `pyansys.support@ansys.com <mailto:pyansys.support@ansys.com>`_
    to request access. The PyAnsys team can provide you a read-only token to be inserted in ``${PRIVATE_PYPI_ACCESS_TOKEN}``.
    Once you have it, run the following command:

    .. code:: bash

        pip install ansys-dyna-core --index-url=https://${PRIVATE_PYPI_ACCESS_TOKEN}@pkgs.dev.azure.com/pyansys/_packaging/pyansys/pypi/simple/

Install in developer mode
^^^^^^^^^^^^^^^^^^^^^^^^^

Installing PyDyna in developer mode allows you to modify the source and enhance it.

.. note::
   
    Before contributing to the project, ensure that you are thoroughly familiar
    with the `PyAnsys Developer's Guide`_.

Start by cloning the repository

.. code::

   git clone https://github.com/pyansys/pyDyna

and copy the required files.

Install in offline mode
^^^^^^^^^^^^^^^^^^^^^^^

If you lack an internet connection on your installation machine (or you do not have access to the
private Ansys PyPI packages repository), you should install PyDyna by downloading the wheelhouse
archive from the `Releases Page <https://github.com/pyansys/pydyna/releases>`_ for your
corresponding machine architecture.

Each wheelhouse archive contains all the Python wheels necessary to install PyDyna from scratch on Windows
and Linux, from Python 3.6,3.8 and 3.9. You can install this on an isolated system with a fresh Python
installation or on a virtual environment.

For example, on Linux with Python 3.8, unzip the wheelhouse archive and install it with:

.. code:: bash

    unzip ansys-dyna-core-v0.3.dev0-wheelhouse-Linux-3.8.zip wheelhouse
    pip install ansys-dyna-core -f wheelhouse --no-index --upgrade --ignore-installed

If you are on Windows with Python 3.9, unzip the corresponding wheelhouse to a wheelhouse directory
and install using the preceding command.

Consider installing using a `virtual environment <https://docs.python.org/3/library/venv.html>`_.

Documentation
-------------
For comprehesive information on PyDyna, see the latest release
`documentation <https://dyna.docs.pyansys.com/>`_.

On the `PyDyna Issues <https://github.com.mcas.ms/pyansys/pyDyna/issues>`_ page, you can create
issues to submit questions, report bugs, and request new features. To reach
the PyAnsys support team, email `pyansys.support@ansys.com <pyansys.support@ansys.com>`_.

Usage
-----
Here is a basic pre-processing example:

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
	path = examples.cylinder_flow + os.sep
	fns.append(path + "cylinder_flow.k")
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
	icfd_solution.save_file()
	
For more: https://dyna.docs.pyansys.com/version/stable/examples/index.html

Here is a basic solving example:

.. code:: python

   >>> import ansys.dyna.core.solver as solver
   >>> dyna=solver.DynaSovler(hostname,port)           # connect to the container
   >>> dyna.push("input.k")                            # push an input file
   >>> dyna.start(4)                                   # start 4 ranks of mppdyna
   >>> dyna.run("i=input.k memory=10m ncycle=20000")   # begin execution

Here is a basic post-processing example:

lsdyna::d3plot::stress_von_mises

.. code:: python

	 from ansys.dpf import core as dpf

	 ds = dpf.DataSources()
	 ds.set_result_file_path(r'./d3plot', 'd3plot')

	 resultOp = dpf.Operator("lsdyna::d3plot::stress_von_mises")
	 resultOp.inputs.data_sources(ds)
	 # set the time
	 resultOp.inputs.time_scoping.connect([3])
	 result = resultOp.outputs.stress_von_mises()

License
-------
Distributed under the MIT license.  See LICENSE in the root directory
of the repository for details.

.. LINKS AND REFERENCES
.. _pip: https://pypi.org/project/pip/
.. _PyAnsys Developer's Guide: https://dev.docs.pyansys.com/