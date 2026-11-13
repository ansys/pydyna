# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
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
Plate Thickness Optimization
------------------
This example shows how to use PyDyna to import a mesh, set up a plate thickness
optimization analysis, run the analysis iteratively until a target displacement
is reached,and then display the results of that optimization.

.. LINKS AND REFERENCES
.. _ls_dyna_knowledge_base: https://lsdyna.ansys.com/knowledge-base/
"""

###############################################################################
# Perform required imports
# ~~~~~~~~~~~~~~~~~~~~~~~~
# Import required packages, including those for the keywords, deck, and solver.

import os
import pathlib
import shutil
import tempfile

import ansys.dpf.core as dpf
from ansys.dpf.core import operators as ops
import matplotlib.pyplot as plt
import pandas as pd

from ansys.dyna.core import Deck, keywords as kwd
from ansys.dyna.core.pre.examples.download_utilities import EXAMPLES_PATH, DownloadManager

# from ansys.dyna.core.run.linux_runner import LinuxRunner
from ansys.dyna.core.run.local_solver import run_dyna
from ansys.dyna.core.run.options import MemoryUnit, MpiOption, Precision

# from ansys.dyna.core.run.windows_runner import WindowsRunner

# sphinx_gallery_thumbnail_path = '_static/pre/opt/plate_thickness.png'


workdir = tempfile.TemporaryDirectory()

mesh_file_name = "bar_impact_mesh.k"

mesh_file = DownloadManager().download_file(
    mesh_file_name, "ls-dyna", "Bar_Impact", destination=os.path.join(EXAMPLES_PATH, "Bar_Impact")
)

# If you'd like to insert your own path to a local mesh file you can do so by replacing the line
# above with:
# mesh_file = "C:\Path\to\file\\bar_impact_mesh.k"

###############################################################################
# Set analysis parameters
# ~~~~~~~~~~~~~~~~~~~~~~~~
# Define the number of iterations, thickness increment, target displacement,
# and initial velocity for the analysis

max_iterations = 20
initial_thickness = 0.1
thickness_increment = 0.05
target_displacement = 1.0
initial_velocity = 275.0e2

###############################################################################
# Create a deck and keywords
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a deck, which is the container for all the keywords.
# Then, create and append individual keywords to the deck.


def create_input_deck(thickness):
    deck = Deck()
    deck.title = "Bar Thickness - %.4s" % thickness

    # Define bar material
    mat_1 = kwd.Mat003(mid=1)
    mat_1.ro = 7.85000e-9
    mat_1.e = 150000.0
    mat_1.pr = 0.34
    mat_1.sigy = 390.0
    mat_1.etan = 90.0

    # Define bar section
    sec_1 = kwd.SectionSolid(secid=1)
    sec_1.elform = 1

    # Define plate material
    mat_2 = kwd.Mat003(mid=2)
    mat_2.ro = 7.85000e-9
    mat_2.e = 1500000.0
    mat_2.pr = 0.34
    mat_2.sigy = 3900.0
    mat_2.etan = 900.0

    # Define plate section
    sec_2 = kwd.SectionShell(secid=2)
    sec_2.elform = 2
    sec_2.t1 = thickness
    sec_2.t2 = thickness
    sec_2.t3 = thickness
    sec_2.t4 = thickness

    # Define bar part
    part_1 = kwd.Part()
    part_1.parts = pd.DataFrame({"pid": [1], "mid": [mat_1.mid], "secid": [sec_1.secid]})

    # Define plate part
    part_2 = kwd.Part()
    part_2.parts = pd.DataFrame({"pid": [2], "mid": [mat_2.mid], "secid": [sec_2.secid]})

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

    # Define boxes for boundary conditions
    box_plate_zN = kwd.DefineBox(boxid=2, xmn=-0.1, xmx=10.1, ymn=41.0, ymx=43.0, zmn=-10.1, zmx=-9.9)
    box_plate_zP = kwd.DefineBox(boxid=3, xmn=0.1, xmx=9.9, ymn=41.0, ymx=43.0, zmn=-0.1, zmx=0.1)
    box_plate_xP = kwd.DefineBox(boxid=4, xmn=9.9, xmx=10.1, ymn=41.0, ymx=43.0, zmn=-10.1, zmx=0.1)
    box_plate_xN = kwd.DefineBox(boxid=5, xmn=-0.1, xmx=0.1, ymn=41.0, ymx=43.0, zmn=-9.9, zmx=-0.1)

    # Create node set for fixed BC
    set_node_Fixed = kwd.SetNodeGeneral()
    set_node_Fixed.sid = 2
    set_node_Fixed.option = "BOX"
    set_node_Fixed.e1 = box_plate_zN.boxid
    set_node_Fixed.e2 = box_plate_xP.boxid

    # Define fixed Boundary Conditions
    fixed_bc = kwd.BoundarySpcSet(dofx=1, dofy=1, dofz=1, dofrx=1, dofry=1, dofrz=1)
    fixed_bc.nsid = set_node_Fixed.sid

    # Create node set for symmetric BC normal to Z Axis
    set_node_zNormal = kwd.SetNodeGeneral()
    set_node_zNormal.sid = 3
    set_node_zNormal.option = "BOX"
    set_node_zNormal.e1 = box_plate_zP.boxid

    # Define zNormal Boundary Conditions
    zNormal_bc = kwd.BoundarySpcSet(dofx=0, dofy=0, dofz=1, dofrx=1, dofry=1, dofrz=0)
    zNormal_bc.nsid = set_node_zNormal.sid

    # Create node set for symmetric BC normal to X Axis
    set_node_xNormal = kwd.SetNodeGeneral()
    set_node_xNormal.sid = 4
    set_node_xNormal.option = "BOX"
    set_node_xNormal.e1 = box_plate_xN.boxid

    # Define xNormal Boundary Conditions
    xNormal_bc = kwd.BoundarySpcSet(dofx=1, dofy=0, dofz=0, dofrx=0, dofry=1, dofrz=1)
    xNormal_bc.nsid = set_node_xNormal.sid

    # Define box for node set of plate
    box_plate = kwd.DefineBox(boxid=6, xmn=-1, xmx=11, ymn=39.0, ymx=40.1, zmn=-11, zmx=1)

    # Create node set for plate
    set_node_plate = kwd.SetNodeGeneral()
    set_node_plate.sid = 5
    set_node_plate.option = "BOX"
    set_node_plate.e1 = box_plate.boxid

    # Define contact
    contact = kwd.ContactAutomaticSingleSurface(surfa=0)
    contact.fs = 0.3

    # Define control termination
    control_term = kwd.ControlTermination(endtim=2.00000e-4, dtmin=0.001)

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
            contact,
            box_1,
            box_plate_zN,
            box_plate_zP,
            box_plate_xP,
            box_plate_xN,
            box_plate,
            set_node_Fixed,
            set_node_zNormal,
            set_node_xNormal,
            set_node_plate,
            fixed_bc,
            zNormal_bc,
            xNormal_bc,
            init_vel,
            cs_1,
            part_1,
            mat_1,
            sec_1,
            part_2,
            mat_2,
            sec_2,
            deck_hist_node_1,
        ]
    )

    return deck


def write_input_deck(**kwargs):
    thickness = kwargs.get("thickness")
    wd = kwargs.get("wd")
    if not all((thickness, wd)):
        raise Exception("Missing input!")
    deck = create_input_deck(thickness)
    # Import mesh
    deck.append(kwd.Include(filename=mesh_file_name))

    # Write LS-DYNA input deck
    os.makedirs(wd, exist_ok=True)
    deck.export_file(os.path.join(wd, "input.k"))
    shutil.copyfile(mesh_file, os.path.join(wd, mesh_file_name))


###############################################################################
# Define the Dyna solver function
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#


def run_job(directory):
    run_dyna(
        "input.k",
        working_directory=directory,
        ncpu=2,
        memory=2,
        precision=Precision.SINGLE,
        mpi_option=MpiOption.MPP_INTEL_MPI,
        memory_unit=MemoryUnit.MB,
    )
    assert os.path.isfile(os.path.join(directory, "d3plot")), "No result file found"


###############################################################################
# Define the DPF output function
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#


def get_plate_displacement(directory):
    ds = dpf.DataSources()
    result_file = os.path.join(directory, "d3plot")
    assert os.path.isfile(result_file)
    ds.set_result_file_path(result_file, "d3plot")
    model = dpf.Model(ds)

    # Create mesh operator
    mes_op = dpf.operators.mesh.mesh_provider()
    mes_op.inputs.data_sources.connect(ds)
    # Isolate Part 2
    mes_op.connect(25, [2])
    # Extract mesh
    part_mesh = mes_op.outputs.mesh()

    # Create scoping operator from mesh using from_mesh scoping operator
    part_mesh_op = dpf.operators.scoping.from_mesh()
    part_mesh_op.inputs.mesh.connect(part_mesh)
    part_scoping = part_mesh_op.outputs.scoping()

    # create displacement entity, apply part scoping
    disp = model.results.displacement
    disp.on_mesh_scoping(part_scoping)

    disp_op = disp.on_all_time_freqs()

    # Find min and max displacement
    min_max_op = ops.min_max.min_max_fc(ops.math.norm_fc(disp_op))

    min_displ = min_max_op.outputs.field_min()
    max_displ = min_max_op.outputs.field_max()

    max_disp_data = max_displ.data
    min_disp_data = min_displ.data

    tdata = model.metadata.time_freq_support.time_frequencies.data

    return tdata, max_disp_data, min_disp_data


###############################################################################
# Run solver iteratively until target displacement is reached
# ~~~~~~~~~~~~~~~~~~~~~~
#

all_results = []

for iteration in range(max_iterations):
    # Define thickness for this iteration
    thickness = initial_thickness + thickness_increment * iteration
    wd = os.path.join(workdir.name, f"thickness_{thickness:.4f}")
    pathlib.Path(wd).mkdir(exist_ok=True)
    # Create LS-Dyna input deck with new thickness
    write_input_deck(thickness=thickness, wd=wd)
    try:
        # Run solver
        run_job(wd)
        # Post-process displacement
        time_data, max_disp_data, min_disp_data = get_plate_displacement(wd)
        reduced_time_data = [t * 1000 for t in time_data]  # Convert to ms
        # Store result
        all_results.append({"thickness": thickness, "time": reduced_time_data, "max_disp": max_disp_data})
        # Check if target displacement is reached
        if max(max_disp_data) <= target_displacement:
            print(f"Target displacement reached at thickness {thickness:.4f}")
            break

    except Exception as e:
        print(f"Iteration {iteration} failed:", e)

###############################################################################
# Generate graphical output
# ~~~~~~~~~~~~~~~~~~~~~~~~~
#

# Now plot all results
plt.figure(figsize=(8, 5))
for res in all_results:
    thickness = res["thickness"]
    time_data = res["time"]
    max_disp_data = res["max_disp"]
    color = "r" if max(max_disp_data) <= target_displacement else "b"
    label = f"Thickness {thickness:.4f}"
    plt.plot(time_data, max_disp_data, color=color, label=label)
plt.xlabel("Time (ms)")
plt.ylabel("Displacement (mm)")
plt.title("Plate Displacement vs Time for Different Thicknesses")
plt.grid(True)
plt.show()
