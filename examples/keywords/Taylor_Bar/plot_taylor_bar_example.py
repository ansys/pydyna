# Copyright (C) 2021 - 2024 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

"""
Taylor bar example
------------------
This example is inspired by the “Taylor Bar” example on the
`LS-DYNA Knowledge Base <_ls_dyna_knowledge_base>`_ site. It shows how
to use PyDyna to create a keyword file for LS-DYNA and solve it within
a Pythonic environment.

.. LINKS AND REFERENCES
.. _ls_dyna_knowledge_base: https://lsdyna.ansys.com/knowledge-base/
"""

###############################################################################
# Perform required imports
# ~~~~~~~~~~~~~~~~~~~~~~~~
# Import required packages, including those for the keywords, deck, and solver.

# sphinx_gallery_thumbnail_number = 1

import datetime
import os
import shutil
import tempfile
import time

import matplotlib.pyplot as plt
import pandas as pd

import ansys.dpf.core as dpf
from ansys.dyna.core import Deck, keywords as kwd
from ansys.dyna.core.run import run_dyna


###############################################################################
# Create a deck and keywords
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a deck, which is the container for all the keywords.
# Then, create and append individual keywords to the deck.


def create_input_deck(**kwargs):
    initial_velocity = kwargs.get("initial_velocity")
    wd = kwargs.get("wd")
    if not all((initial_velocity, wd)):
        raise Exception("Missing input!")

    deck = Deck()
    deck.title = "Taylor-Bar Velocity - %s - Unit: t-mm-s" % initial_velocity

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
    init_vel.vy = initial_velocity
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
    deck_hist_node_1 = kwd.DatabaseHistoryNodeSet()
    deck_hist_node_1.id1 = set_node_1.sid

    # Append all cards to input deck
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

    # Import mesh
    #    note: this assumes that `taylor_bar_mesh` is in the working directory
    #          an absolute path is used becuase the problem is solved in another
    #          working directory, and the `Include` keyword will need to point to
    #          the absolute path.  You may choose to solve this problem in a different
    #          way, for example by copying the mesh file to that working directory
    #          before solving and using only the file name without an absolute path
    deck.append(kwd.Include(filename=os.path.abspath("taylor_bar_mesh.k")))

    # Convert deck to string
    deck_string = deck.write()

    # Create LS-DYNA input deck
    os.makedirs(wd, exist_ok=True)
    with open(os.path.join(wd, "input.k"), "w") as file_handle:
        file_handle.write(deck_string)

    return deck


###############################################################################
# Define the Dyna solver function
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#


def run(**kwargs):
    wd = kwargs.get("wd")
    shutil.copy("taylor_bar_mesh.k", f"{wd}/taylor_bar_mesh.k")
    inputfile = os.path.join(wd, "input.k")
    run_dyna(inputfile)
    assert os.path.isfile(os.path.join(wd, "d3plot"))
    assert os.path.isfile(os.path.join(wd, "lsrun.out.txt"))


###############################################################################
# Define the DPF output function
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#


def get_global_ke(**kwargs):
    wd = kwargs.get("wd")
    ds = dpf.DataSources()
    ds.set_result_file_path(os.path.join(wd, "d3plot"), "d3plot")
    model = dpf.Model(ds)

    gke_op = dpf.operators.result.global_kinetic_energy()
    gke_op.inputs.data_sources.connect(ds)
    gke = gke_op.eval()
    field = gke.get_field(0)
    ke_data = field.data

    time_data = model.metadata.time_freq_support.time_frequencies.data_as_list
    return time_data, ke_data


###############################################################################
# View the model
# ~~~~~~~~~~~~~~
# etc etc
deck_for_graphic = create_input_deck(initial_velocity=300e3, wd="run")
deck_for_graphic.plot(cwd="run")
###############################################################################
# Run a parametric solve
# ~~~~~~~~~~~~~~~~~~~~~~
# etc etc


# Define base working directory

root_out_dir = tempfile.gettempdir()
stamp = datetime.datetime.fromtimestamp(time.time()).strftime("%Y-%m-%d_%H_%M_%S")
base_wd = os.path.join(root_out_dir, "PyDyna.%s" % stamp)
os.mkdir(base_wd)

color = ["b", "r", "g", "y"]
# Specify different velocities in mm/s
initial_velocities = [275.0e3, 300.0e3, 325.0e3, 350.0e3]

for index, initial_velocity in enumerate(initial_velocities):
    # Create a folder for each parameter
    wd = os.path.join(base_wd, "tb_vel_%s" % initial_velocity)
    os.mkdir(wd)
    # Create LS-Dyna input deck
    create_input_deck(initial_velocity=initial_velocity, wd=wd)
    # Run Solver
    try:
        run(wd=wd)
        # Run PyDPF Post
        time_data, ke_data = get_global_ke(initial_velocity=initial_velocity, wd=wd)
        # Add series to the plot
        plt.plot(time_data, ke_data, color[index], label="KE at vel. %s mm/s" % initial_velocity)

    except Exception as e:
        print(e)
    # sphinx_gallery_defer_figures

shutil.rmtree(base_wd, True)

plt.xlabel("Time (s)")
plt.ylabel("Energy (mJ)")

###############################################################################
# Generate graphical output
# ~~~~~~~~~~~~~~~~~~~~~~~~~
# etc etc

plt.show()
