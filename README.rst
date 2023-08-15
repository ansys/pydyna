PyDYNA
========
|pyansys| |python| |pypi| |GH-CI| |codecov| |MIT| |black|

.. |pyansys| image:: https://img.shields.io/badge/Py-Ansys-ffc107.svg?logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAIAAACQkWg2AAABDklEQVQ4jWNgoDfg5mD8vE7q/3bpVyskbW0sMRUwofHD7Dh5OBkZGBgW7/3W2tZpa2tLQEOyOzeEsfumlK2tbVpaGj4N6jIs1lpsDAwMJ278sveMY2BgCA0NFRISwqkhyQ1q/Nyd3zg4OBgYGNjZ2ePi4rB5loGBhZnhxTLJ/9ulv26Q4uVk1NXV/f///////69du4Zdg78lx//t0v+3S88rFISInD59GqIH2esIJ8G9O2/XVwhjzpw5EAam1xkkBJn/bJX+v1365hxxuCAfH9+3b9/+////48cPuNehNsS7cDEzMTAwMMzb+Q2u4dOnT2vWrMHu9ZtzxP9vl/69RVpCkBlZ3N7enoDXBwEAAA+YYitOilMVAAAAAElFTkSuQmCC
   :target: https://docs.pyansys.com/
   :alt: PyAnsys

.. |python| image:: https://img.shields.io/pypi/pyversions/ansys-dyna-core?logo=pypi
   :target: https://pypi.org/project/ansys-dyna-core/
   :alt: Python

.. |pypi| image:: https://img.shields.io/pypi/v/ansys-dyna-core.svg?logo=python&logoColor=white
   :target: https://pypi.org/project/ansys-dyna-core
   :alt: PyPI

.. |codecov| image:: https://codecov.io/gh/ansys/ansys-dyna-core/branch/main/graph/badge.svg
   :target: https://codecov.io/gh/ansys/pydyna
   :alt: Codecov

.. |GH-CI| image:: https://github.com/ansys/pydyna/actions/workflows/ci_cd.yml/badge.svg
   :target: https://github.com/ansys/pydyna/actions/workflows/ci_cd.yml
   :alt: GH-CI

.. |MIT| image:: https://img.shields.io/badge/License-MIT-yellow.svg
   :target: https://opensource.org/licenses/MIT
   :alt: MIT

.. |black| image:: https://img.shields.io/badge/code%20style-black-000000.svg?style=flat
   :target: https://github.com/psf/black
   :alt: Black

Overview
========
PyDYNA is a Pythonic package for providing a more convenient and complete way to
build an Ansys DYNA input deck, submit it to the Ansys LS-DYNA solver, and
finally postprocess the results. 

In the PyDYNA installation, the ``docker`` directory has two child
directories:

- ``pre``: Contains the package with the ``ls-pre`` Docker image for the
  ``pre`` service. This service provides highly abstracted APIs for creating and
  setting up DYNA input decks for DynaMech, DynaIGA, DynaICFD, DynaSALE, DynaEM,
  and DynaAirbag.
- ``solver``: Contains the package with the ``dynasolver`` Docker image
  for the ``solver`` service. This service provides highly abstracted
  APIs for interacting directly with the Ansys LS-DYNA solver. Because LS-DYNA
  is primarily a batch solver with very limited interactive capabilities, the
  ``solver`` service is similarly limited. The target use case is that LS-DYNA is
  running in a container environment such as Docker or Kubernetes. Using this
  service, you can push input files to the container, start LS-DYNA
  and monitor its progress, and then retrieve Ansys solver results (RST)
  files.

Once you have results, you can use the Ansys Data Processing Framework (DPF),
which is designed to provide numerical simulation users and engineers
with a toolbox for accessing and transforming simulation data. DPF
can access data from Ansys solver RST files and from several
files with neutral formats, including CSV, HDF5, and VTK. Using DPF's
various operators, you can manipulate and transform this data.

The `ansys-dpf-post package <https://github.com/ansys/pydpf-post>`_ provides
a simplified Python interface to DPF, thus enabling rapid postprocessing
without ever leaving a Python environment. For more information on DPF-Post,
see the `DPF-Post documentation <https://post.docs.pyansys.com>`_.

Documentation and issues
========================
Documentation for the latest stable release of PyDyna is hosted at `PyDYNA documentation
<https://dyna.docs.pyansys.com/version/stable//>`_.

In the upper right corner of the documentation's title bar, there is an option for switching from
viewing the documentation for the latest stable release to viewing the documentation for the
development version or previously released versions.

On the `PyDYNA Issues <https://github.com/ansys/pydyna/issues>`_ page, you can create issues to
report bugs and request new features. On the `Discussions <https://discuss.ansys.com/>`_
page on the Ansys Developer portal, you can post questions, share ideas, and get community feedback. 

To reach the project support team, email `pyansys.core@ansys.com <pyansys.core@ansys.com>`_.

Usage
=====
The next few sections show how to preprocess, solve, and postprocess a ball plate example.

Preprocess
----------
The following code preprocesses a ball plate example. In the repository, you can get the
input file from ``src/ansys/dyna/core/pre/examples/explicit/ball_plate/ball_plate.k`` and
the Python file from ``examples/Explicit/ball_plate.py``.

.. code:: python

    import os
    import sys
    from ansys.dyna.core.pre.dynasolution import DynaSolution
    from ansys.dyna.core.pre.dynamech import (
        DynaMech,
        Velocity,
        PartSet,
        ShellPart,
        SolidPart,
        NodeSet,
        Contact,
        ContactSurface,
        ShellFormulation,
        SolidFormulation,
        ContactType,
        AnalysisType
    )
    from ansys.dyna.core.pre.dynamaterial import (
        MatRigid,
        MatPiecewiseLinearPlasticity,
    )
    from ansys.dyna.core.pre import examples

    hostname = "localhost"
    if len(sys.argv) > 1:
        hostname = sys.argv[1]
    solution = DynaSolution(hostname)

    fns = []
    path = examples.ball_plate + os.sep
    fns.append(path+"ball_plate.k")
    solution.open_files(fns)

    solution.set_termination(termination_time=10)

    ballplate = DynaMech(AnalysisType.NONE)
    solution.add(ballplate)

    matrigid = MatRigid(mass_density=7.83e-6, young_modulus=207, poisson_ratio=0.3)
    matplastic = MatPiecewiseLinearPlasticity(mass_density=7.83e-6, young_modulus=207, yield_stress=0.2, tangent_modulus=2)

    plate = ShellPart(1)
    plate.set_element_formulation(ShellFormulation.BELYTSCHKO_TSAY)
    plate.set_material(matplastic)
    plate.set_thickness(1)
    plate.set_integration_points(5)
    ballplate.parts.add(plate)

    ball = SolidPart(2)
    ball.set_material(matrigid)
    ball.set_element_formulation(SolidFormulation.CONSTANT_STRESS_SOLID_ELEMENT)
    ballplate.parts.add(ball)

    selfcontact = Contact(type=ContactType.AUTOMATIC)
    surf1 = ContactSurface(PartSet([1, 2]))
    selfcontact.set_slave_surface(surf1)
    ballplate.contacts.add(selfcontact)

    spc = [34,35,51,52,68,69,85,86,102,103,119,120,136,137,153,154,170,171,187,188,204,205,221,222,238,239,255,256]
    for i in range(1,19):
        spc.append(i)
    for i in range(272,290):
        spc.append(i)
    ballplate.boundaryconditions.create_spc(NodeSet(spc),rx=False,ry=False,rz=False)

    for i in range(1,1652):
        ballplate.initialconditions.create_velocity_node(i,trans=Velocity(0, 0, -10))

    solution.set_output_database(glstat=0.1, matsum=0.1, sleout=0.1)
    solution.create_database_binary(dt=1)
    serverpath = solution.save_file()

    serveroutfile = '/'.join((serverpath,"ball_plate.k"))
    downloadpath = os.path.join(os.getcwd(), "output")
    if not os.path.exists(downloadpath):
        os.makedirs(downloadpath)
    downloadfile = os.path.join(downloadpath,"ball_plate.k")
    solution.download(serveroutfile,downloadfile)
    
Solve
-----
The following code solves this basic ball plate example. In the repository,
you can get the Python file from ``examples/solver/ball_plate_solver.py``.

.. code:: python

    import ansys.dyna.core.solver as solver

    hostname = "localhost"
    port = "5000"
    dyna=solver.DynaSolver(hostname,port)           # connect to the container
    dyna.push("./output/ball_plate.k")                            # push an input file
    dyna.start(4)                                   # start 4 ranks of mppdyna
    dyna.run("i=ball_plate.k memory=10m ncycle=20000")   # begin execution


Postprocess
-----------
The following code postprocesses results from the solve of this basic ball plate example:

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
