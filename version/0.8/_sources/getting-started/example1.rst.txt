The next few sections show how to generate, preview, solve, and review a Taylor bar impact problem.
An example of a sweep over impact velocities for this problem can be found in this repository at
``examples/Taylor_Bar/plot_taylor_bar_example.py``.

Preprocessing
~~~~~~~~~~~~~
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

