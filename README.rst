PyDyna
#############

PyDyna is a pythonic dyna package that aims to provide user a more convenient and complete way to
build up ansys-dyna input deck, submit to solver and finally post processing their results. 


Project Overview
----------------
There are 3 related packages here, pre and solver are all under the ansys/dyna/ directory,
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

Installation
------------
This package is not yet available on PyPI, so for now the only real
option is for you to download the code from GitHub:

.. code::

   git clone https://github.com/pyansys/pyDyna

and copy the required files

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

    from ansys.dyna.core.pre.dynasolution import *
    from ansys.dyna.core.pre.dynaiga import *
    from ansys.dyna.core.pre.dynamaterial import *
    hostname = "localhost"
    iga_solution = DynaSolution(hostname)
    fns = []
    path = os.getcwd() + os.sep + "input" + os.sep + "iga_sample" + os.sep
    fns.append(path + "maino.k")
    fns.append(path + "rkrwelds.key")
    fns.append(path + "27parts.key")
    iga_solution.open_files(fns)
    iga_solution.set_termination(20)
    iga_solution.create_database_binary(dt=0.1)
    iga = DynaIGA()
    iga_solution.add(iga)
    iga.set_timestep(timestep_size_for_mass_scaled=-0.0004) 
    ...
    selfcontact = Contact(type=ContactType.AUTOMATIC)
    selfcontact.set_friction_coefficient(static=0.2)
    surf1=ContactSurface(PartSet(igaparts))
    selfcontact.set_slave_surface(surf1)
    iga_solution.save_file()

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
