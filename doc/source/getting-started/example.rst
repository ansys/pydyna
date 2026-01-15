
Keyword and run example
=======================

The next few sections show how to generate, preview, solve, and review a Taylor bar impact problem.
An example of a sweep over impact velocities for this problem can be found in this repository at
``examples/Taylor_Bar/plot_taylor_bar_example.py``.

Preprocessing
-------------
The following code describes an LS-DYNA Model for a Taylor bar impact problem. It assumes that the mesh file
`taylor_bar_mesh.k` exists in the working directory. This mesh file can be found in this repository at
``examples/Taylor_Bar/taylor_bar_mesh.k``.

.. code:: python

    import pandas as pd

    from ansys.dyna.core import Deck, keywords as kwd

    # construct a new Deck
    deck = Deck()

    # Define material
    mat_1 = kwd.Mat003(mid=1)
    mat_1.ro = 7.85000e-9
    mat_1.e = 150000.0
    mat_1.pr = 0.34
    mat_1.sigy = 390.0
    mat_1.etan = 90.0

    # Define section
    sec_1 = kwd.SectionSolid(secid=1)
    sec_1.elform = 1

    # Define part
    part_1 = kwd.Part()
    part_1.parts = pd.DataFrame({"pid": [1], "mid": [mat_1.mid], "secid": [sec_1.secid]})

    # Define coordinate system
    cs_1 = kwd.DefineCoordinateSystem(cid=1)
    cs_1.xl = 1.0
    cs_1.yp = 1.0

     # Define initial velocity
    init_vel = kwd.InitialVelocityGeneration()
    init_vel.id = part_1.parts["pid"][0]
    init_vel.styp = 2
    init_vel.vy = 300.0e3 # mm/s
    init_vel.icid = cs_1.cid

    # Define box for node set
    box_1 = kwd.DefineBox(boxid=1, xmn=-500, xmx=500, ymn=39.0, ymx=40.1, zmn=-500, zmx=500)

    # Create node set
    set_node_1 = kwd.SetNodeGeneral()
    set_node_1.sid = 1
    set_node_1.option = "BOX"
    set_node_1.e1 = box_1.boxid

    # Define rigid wall
    rw = kwd.RigidwallPlanar(id=1)
    rw.nsid = set_node_1.sid
    rw.yt = box_1.ymx
    rw.yh = box_1.ymn

    # Define control termination
    control_term = kwd.ControlTermination(endtim=8.00000e-5, dtmin=0.001)

    # Define database cards
    deck_dt_out = 8.00000e-8
    deck_glstat = kwd.DatabaseGlstat(dt=deck_dt_out, binary=3)
    deck_matsum = kwd.DatabaseMatsum(dt=deck_dt_out, binary=3)
    deck_nodout = kwd.DatabaseNodout(dt=deck_dt_out, binary=3)
    deck_elout = kwd.DatabaseElout(dt=deck_dt_out, binary=3)
    deck_rwforc = kwd.DatabaseRwforc(dt=deck_dt_out, binary=3)
    deck_d3plot = kwd.DatabaseBinaryD3Plot(dt=4.00000e-6)

    # Define deck history node
    deck_hist_node_1 = kwd.DatabaseHistoryNodeSet(id1=set_node_1.sid)

    # Insert all these cards into the Deck
    deck.extend(
        [
            deck_glstat,
            deck_matsum,
            deck_nodout,
            deck_elout,
            deck_rwforc,
            deck_d3plot,
            set_node_1,
            control_term,
            rw,
            box_1,
            init_vel,
            cs_1,
            part_1,
            mat_1,
            sec_1,
            deck_hist_node_1,
        ]
    )

    # Add keyword that imports the mesh
    deck.append(kwd.Include(filename="taylor_bar_mesh.k"))


Preview
~~~~~~~
The following code opens a 3D graphics window to preview the mesh for the LS-DYNA Model

.. code:: python

    # Preview the model
    deck.plot()


Write to file
~~~~~~~~~~~~~
The following code writes the LS-DYNA model to an `input.k` keyword file in the working directory.

.. code:: python

    # Convert deck to string
    deck_string = deck.write()

    # Create LS-DYNA input deck
    with open("input.k", "w") as file_handle:
        file_handle.write(deck_string)

Solve
~~~~~
The following code runs LS-DYNA using the `input.k` file.

.. code:: python

    import os

    from ansys.dyna.core.run import run_dyna

    # Run LS-DYNA
    run_dyna("input.k")

    # Confirm that the results exist
    assert os.path.isfile("d3plot")
    assert os.path.isfile("lsrun.out.txt")


Post processing
~~~~~~~~~~~~~~~
The following code processes results and generates a line chart of Time vs. Energy from the impact. This requires an installation
of a ``matplotlib`` backend.

.. code:: python

    import matplotlib.pyplot as plt
    import ansys.dpf.core as dpf

    ds = dpf.DataSources()
    ds.set_result_file_path("d3plot", "d3plot")
    model = dpf.Model(ds)

    gke_op = dpf.operators.result.global_kinetic_energy()
    gke_op.inputs.data_sources.connect(ds)
    gke = gke_op.eval()
    field = gke.get_field(0)
    ke_data = field.data

    time_data = model.metadata.time_freq_support.time_frequencies.data_as_list

    plt.plot(time_data, ke_data, "b", label="Kinetic Energy")
    plt.xlabel("Time (s)")
    plt.ylabel("Energy (mJ)")
    plt.show()



Pre and solver example
----------------------
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
    
