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
Firstly, get the input file from pydyna\src\ansys\dyna\core\pre\examples\explicit\ball_plate\ball_plate.k
The follow example can be obtained from pydyna\examples\Explicit\ball_plate.py

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
    
Here is a basic solving example:
The follow example can be obtained from pydyna\examples\solver\ball_plate_solver.py
.. code:: python

    import ansys.dyna.core.solver as solver

    hostname = "localhost"
    port = "5000"
    dyna=solver.DynaSolver(hostname,port)           # connect to the container
    dyna.push("./output/ball_plate.k")                            # push an input file
    dyna.start(4)                                   # start 4 ranks of mppdyna
    dyna.run("i=ball_plate.k memory=10m ncycle=20000")   # begin execution

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