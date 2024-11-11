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

import pathlib
import os
import shutil

import matplotlib.pyplot as plt
import pandas as pd

import ansys.dpf.core as dpf
from ansys.dyna.core import Deck, keywords as kwd
from ansys.dyna.core.run import run_dyna
from ansys.dyna.core.pre.examples.download_utilities import DownloadManager, EXAMPLES_PATH

thisdir = os.path.abspath(os.path.dirname(__file__))
workdir = os.path.join(thisdir, "workdir")

pathlib.Path(workdir).mkdir(exist_ok=True)
mesh_file = DownloadManager().download_file("taylor_bar_mesh.k", "ls-dyna", "Taylor_Bar", destination=os.path.join(EXAMPLES_PATH, "Taylor_Bar"))

###############################################################################
# Create a deck and keywords
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a deck, which is the container for all the keywords.
# Then, create and append individual keywords to the deck.

def create_input_deck(initial_velocity):
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
    deck.append(kwd.Include(filename="taylor_bar_mesh.k"))



    return deck

def write_input_deck(**kwargs):
    initial_velocity = kwargs.get("initial_velocity")
    wd = kwargs.get("wd")
    if not all((initial_velocity, wd)):
        raise Exception("Missing input!")
    deck = create_input_deck(initial_velocity)

    deck.append(kwd.Include(filename="taylor_bar_mesh.k"))

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


def run(directory):
    shutil.copy(mesh_file, directory)
    result = run_dyna("input.k", working_directory=directory, stream=False)
    assert os.path.isfile(os.path.join(directory, "d3plot")), "No result file found"
    return result

###############################################################################
# Define the DPF output function
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#


def get_global_ke(directory):
    ds = dpf.DataSources()
    result_file = os.path.join(directory, "d3plot")
    assert os.path.isfile(result_file)
    ds.set_result_file_path(result_file, "d3plot")
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
deck_for_graphic = create_input_deck(300e3)
#deck_for_graphic.plot(cwd=thisdir)

###############################################################################
# Run a parametric solve
# ~~~~~~~~~~~~~~~~~~~~~~
# etc etc


# Define base working directory

color = ["b", "r", "g", "y"]
# Specify different velocities in mm/s
initial_velocities = [275.0e3, 300.0e3, 325.0e3, 350.0e3]

for index, initial_velocity in enumerate(initial_velocities):
    # Create a folder for each parameter
    wd = os.path.join(workdir, "tb_vel_%s" % initial_velocity)
    pathlib.Path(wd).mkdir(exist_ok=True)
    # Create LS-Dyna input deck
    write_input_deck(initial_velocity=initial_velocity, wd=wd)
    # Run Solver
    try:
        run(wd)
        # Run PyDPF Post
        time_data, ke_data = get_global_ke(wd)
        # Add series to the plot
        plt.plot(time_data, ke_data, color[index], label="KE at vel. %s mm/s" % initial_velocity)

    except Exception as e:
        print(e)
    # sphinx_gallery_defer_figures

plt.xlabel("Time (s)")
plt.ylabel("Energy (mJ)")

###############################################################################
# Generate graphical output
# ~~~~~~~~~~~~~~~~~~~~~~~~~
# etc etc

plt.show()
