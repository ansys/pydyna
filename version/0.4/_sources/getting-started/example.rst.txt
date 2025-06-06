The next few sections show how to preprocessing, solve, and postprocessing a ball plate example.

Preprocessing
~~~~~~~~~~~~~
The following code processes a ball plate example. In the repository, you can get the
input file from ``src/ansys/dyna/core/pre/examples/explicit/ball_plate/ball_plate.k`` and
the Python file from ``examples/Explicit/ball_plate.py``.

.. code:: python

    import os
    import sys
    from ansys.dyna.core.pre import launch_dynapre
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
    solution = launch_dynapre(ip = hostname)

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
~~~~~
The following code solves this basic ball plate example. In the repository,
you can get the Python file from ``examples/solver/ball_plate_solver.py``.

.. code:: python

    import ansys.dyna.core.solver as solver

    hostname = "localhost"
    port = "5000"
    dyna=launch_dyna(ip = hostname,port = port)            # connect to the container
    dyna.push("./output/ball_plate.k")                            # push an input file
    dyna.start(4)                                   # start 4 ranks of mppdyna
    dyna.run("i=ball_plate.k memory=10m ncycle=20000")   # begin execution


Post processing
~~~~~~~~~~~~~~~
The following code processes results from the solve of this basic ball plate example:

.. code:: python

    from ansys.dpf import core as dpf
    import os

    ds = dpf.DataSources()
    data_path = os.path.join(os.getcwd(), 'd3plot')
    ds.set_result_file_path(data_path, 'd3plot')

    model = dpf.Model(ds)
    # Extract displacements for all time steps from d3plot
    D = model.results.displacement.on_all_time_freqs().eval()
    D.animate()

    stress = dpf.operators.result.stress()
    stress.inputs.data_sources(ds)
    stress.inputs.time_scoping([12])
    stress.connect(25, [1])
    stress.inputs.requested_location.connect("Nodal")
    fields = stress.outputs.fields_container()

    shell_layer_extract = dpf.operators.utility.change_shell_layers()
    shell_layer_extract.inputs.fields_container.connect(fields)
    print(shell_layer_extract.inputs.e_shell_layer)
    shell_layer_extract.inputs.e_shell_layer.connect(0)
    fields_top = shell_layer_extract.outputs.fields_container_as_fields_container()
    print(fields_top)
    fields_top.animate()

For more examples, see `Examples <https://dyna.docs.pyansys.com/version/stable/examples/index.html>`_
in the PyDYNA documentation.
