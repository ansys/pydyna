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
Buckling of beer can example
----------------------------
This example is inspired by the "Buckling of Beer Can" example on the
`LS-DYNA Knowledge Base <_ls_dyna_knowledge_base>`_ site. It shows how to
use PyDyna to create a keyword file for LS-DYNA and then solve it from
Python.

.. LINKS AND REFERENCES
.. _ls_dyna_knowledge_base: https://lsdyna.ansys.com/knowledge-base/
"""

###############################################################################
# Perform required imports
# ~~~~~~~~~~~~~~~~~~~~~~~~
# Import required packages, including those for the keywords, deck, and solver.

import os
import shutil

# subprocess is used to run LS-DYNA commands, excluding bandit warning
import subprocess  # nosec: B404
import tempfile

import numpy as np
import pandas as pd

from ansys.dyna.core import Deck, keywords as kwd
from ansys.dyna.core.run import MemoryUnit, MpiOption, run_dyna
from ansys.dyna.core.utils.download_utilities import EXAMPLES_PATH, DownloadManager

rundir = tempfile.TemporaryDirectory()
mesh_file_name = "mesh.k"
mesh_file = DownloadManager().download_file(
    mesh_file_name, "ls-dyna", "Buckling_Beer_Can", destination=os.path.join(EXAMPLES_PATH, "Buckling_Beer_Can")
)

dynafile = "beer_can.k"

###############################################################################
# Create a deck and keywords
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a deck, which is the container for all the keywords.
# Then, create and append individual keywords to the deck.


def write_deck(filepath):
    deck = Deck()

    # Append control keywords
    contact_auto = kwd.ContactAutomaticSingleSurfaceMortar(cid=1)
    contact_auto.options["ID"].active = True
    contact_auto.heading = "Single-Surface Mortar Contact (The New Explicit/Implicit Standard)"
    deck.extend(
        [
            contact_auto,
            kwd.ControlAccuracy(iacc=1),
            kwd.ControlImplicitAuto(iauto=1, dtmax=0.01),
            kwd.ControlImplicitDynamics(imass=1, gamma=0.6, beta=0.38),
            kwd.ControlImplicitGeneral(imflag=1, dt0=0.01),
            kwd.ControlImplicitSolution(nlprint=2),
            kwd.ControlShell(esort=2, theory=-16, intgrd=1, nfail4=1, irquad=0),
            kwd.ControlTermination(endtim=1.0),
        ]
    )

    # Append database keywords
    deck.extend(
        [
            kwd.DatabaseGlstat(dt=1.0e-4, binary=3, ioopt=0),
            kwd.DatabaseSpcforc(dt=1e-4, binary=3, ioopt=0),
            kwd.DatabaseBinaryD3Plot(dt=1.0e-4),
            kwd.DatabaseExtentBinary(maxint=-3, nintsld=1),
        ]
    )

    # Part keywords
    can_part = kwd.Part(heading="Beer Can", pid=1, secid=1, mid=1, eosid=0)
    floor_part = kwd.Part(heading="Floor", pid=2, secid=2, mid=1)

    # Material keywords
    mat_elastic = kwd.MatElastic(mid=1, ro=2.59e-4, e=1.0e7, pr=0.33, title="Aluminum")
    mat_elastic.options["TITLE"].active = True

    # Section keywords
    can_shell = kwd.SectionShell(secid=1, elform=-16, shrf=0.8333, nip=3, t1=0.002, propt=0.0, title="Beer Can")
    can_shell.options["TITLE"].active = True

    floor_shell = kwd.SectionShell(secid=2, elform=-16, shrf=0.833, t1=0.01, propt=0.0)
    floor_shell.options["TITLE"].active = True
    floor_shell.title = "Floor - Just for Contact (Rigid Wall Would Have Worked Also)"

    deck.extend(
        [
            can_part,
            can_shell,
            floor_part,
            floor_shell,
            mat_elastic,
        ]
    )

    # Load curve
    load_curve = kwd.DefineCurve(lcid=1, curves=pd.DataFrame({"a1": [0.00, 1.00], "o1": [0.0, 1.000]}))
    load_curve.options["TITLE"].active = True
    load_curve.title = "Load vs. Time"
    deck.append(load_curve)

    # Define boundary conditions
    load_nodes = [
        50,
        621,
        670,
        671,
        672,
        673,
        674,
        675,
        676,
        677,
        678,
        679,
        680,
        681,
        682,
        683,
        684,
        685,
        686,
        687,
        31,
        32,
        33,
        34,
        35,
        36,
        37,
        38,
        39,
        40,
        41,
        42,
        43,
        44,
        45,
        46,
        47,
        48,
        49,
        1229,
        1230,
        1231,
        1232,
        1233,
        1234,
        1235,
        1236,
        1237,
        1238,
        1239,
        1240,
        1241,
        1242,
        1243,
        1244,
        1245,
        1246,
        1247,
        1799,
        1800,
        1801,
        1802,
        1803,
        1804,
        1805,
        1806,
        1807,
        1808,
        1809,
        1810,
        1811,
        1812,
        1813,
        1814,
        1815,
        1816,
    ]

    count = len(load_nodes)
    zeros = np.zeros(count)

    load_node_point = kwd.LoadNodePoint(
        nodes=pd.DataFrame(
            {
                "nid": load_nodes,
                "dof": np.full((count), 3),
                "lcid": np.full((count), 1),
                "sf": np.full((count), -13.1579),
                "cid": zeros,
                "m1": zeros,
                "m2": zeros,
                "m3": zeros,
            }
        )
    )

    deck.append(load_node_point)

    nid = [
        1,
        31,
        32,
        33,
        34,
        35,
        36,
        37,
        38,
        39,
        40,
        41,
        42,
        43,
        44,
        45,
        46,
        47,
        48,
        49,
        50,
        80,
        81,
        82,
        83,
        84,
        85,
        86,
        87,
        88,
        89,
        90,
        91,
        92,
        93,
        94,
        95,
        96,
        97,
        98,
        621,
        651,
        652,
        653,
        654,
        655,
        656,
        657,
        658,
        659,
        660,
        661,
        662,
        663,
        664,
        665,
        666,
        667,
        668,
        669,
        670,
        671,
        672,
        673,
        674,
        675,
        676,
        677,
        678,
        679,
        680,
        681,
        682,
        683,
        684,
        685,
        686,
        687,
        1210,
        1211,
        1212,
        1213,
        1214,
        1215,
        1216,
        1217,
        1218,
        1219,
        1220,
        1221,
        1222,
        1223,
        1224,
        1225,
        1226,
        1227,
        1228,
        1229,
        1230,
        1231,
        1232,
        1233,
        1234,
        1235,
        1236,
        1237,
        1238,
        1239,
        1240,
        1241,
        1242,
        1243,
        1244,
        1245,
        1246,
        1247,
        1799,
        1800,
        1801,
        1802,
        1803,
        1804,
        1805,
        1806,
        1807,
        1808,
        1809,
        1810,
        1811,
        1812,
        1813,
        1814,
        1815,
        1816,
        1817,
        1818,
        1819,
        1820,
        1821,
        1822,
        1823,
        1824,
        1825,
        1826,
        1827,
        1828,
        1829,
        1830,
        1831,
        1832,
        1833,
        1834,
    ]

    count = len(nid)
    zeros = np.zeros(count)
    ones = np.full((count), 1)

    dofz = [
        1,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        0,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
    ]

    boundary_spc_node = kwd.BoundarySpcNode(
        nodes=pd.DataFrame(
            {
                "nid": nid,
                "cid": zeros,
                "dofx": ones,
                "dofy": ones,
                "dofz": dofz,
                "dofrx": ones,
                "dofry": ones,
                "dofrz": ones,
            }
        )
    )

    deck.append(boundary_spc_node)

    # Define nodes and elements
    deck.append(kwd.Include(filename=mesh_file_name))

    deck.export_file(filepath)
    return deck


def run_post(filepath):
    pass


shutil.copy(mesh_file, os.path.join(rundir.name, mesh_file_name))
deck = write_deck(os.path.join(rundir.name, dynafile))

###############################################################################
# View the model
# ~~~~~~~~~~~~~~
# You can use the PyVista ``plot`` method in the ``deck`` class to view
# the model.

deck.plot(cwd=rundir.name)

###############################################################################
# Run the Dyna solver
# ~~~~~~~~~~~~~~~~~~~
#


try:
    run_dyna(
        dynafile,
        working_directory=rundir.name,
        ncpu=2,
        mpi_option=MpiOption.MPP_INTEL_MPI,
        memory=20,
        memory_unit=MemoryUnit.MB,
    )
except subprocess.CalledProcessError:
    # this example doesn't run to completion because it is a highly nonlinear buckling
    pass

run_post(rundir.name)
