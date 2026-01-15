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
John Reid pendulum example
--------------------------
This example is inspired by John Reid's "Pendulum" example on the
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

import os
import shutil
import tempfile

import pandas as pd

from ansys.dyna.core import Deck
from ansys.dyna.core import keywords as kwd
from ansys.dyna.core.run import run_dyna
from ansys.dyna.core.utils.download_utilities import EXAMPLES_PATH, DownloadManager

mesh_file_name = "nodes.k"
mesh_file = DownloadManager().download_file(
    mesh_file_name, "ls-dyna", "John_Reid_Pendulum", destination=os.path.join(EXAMPLES_PATH, "John_Reid_Pendulum")
)

rundir = tempfile.TemporaryDirectory()

dynafile = "pendulum.k"


###############################################################################
# Create a deck and keywords
# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a deck, which is the container for all the keywords.
# Then, create and append individual keywords to the deck.


def write_deck(filepath):
    deck = Deck()

    # Append control keywords
    deck.extend(
        [
            kwd.ControlTermination(endtim=11.0),
            kwd.ControlEnergy(hgen=2, rwen=2),
            kwd.ControlOutput(npopt=1, neecho=3),
            kwd.ControlShell(istupd=1, theory=2),
        ]
    )

    # Append database keywords
    deck.extend(
        [
            kwd.DatabaseBinaryD3Plot(dt=1.00),
            kwd.DatabaseExtentBinary(ieverp=1),
            kwd.DatabaseBinaryD3Thdt(dt=999999),
            kwd.DatabaseGlstat(dt=0.10),
            kwd.DatabaseMatsum(dt=0.10),
            kwd.DatabaseNodout(dt=0.10),
            kwd.DatabaseHistoryNode(id1=350, id2=374, id3=678, id4=713),
            kwd.DatabaseRbdout(dt=0.10),
            kwd.DatabaseRcforc(dt=0.10),
        ]
    )

    # Define contacts
    deck.extend([kwd.ContactAutomaticSingleSurface(ssid=0, fs=0.08, fd=0.08), kwd.ControlContact(shlthk=2)])

    # Define gravity
    curve1 = kwd.DefineCurve(lcid=1)
    curve1.curves = pd.DataFrame({"a1": [0.00, 10000.00], "o1": [1.000, 1.000]})

    deck.extend([kwd.LoadBodyY(lcid=1, sf=0.00981), curve1])

    # Define boundary conditions

    # BoundarySpcNode edited needs to redo code gen
    BoundarySpcNode = kwd.BoundarySpcNode()
    BoundarySpcNode.nodes = pd.DataFrame(
        {
            "nid": [45004, 45005, 45010, 45011],
            "cid": [0, 0, 0, 0],
            "dofx": [1, 1, 1, 1],
            "dofy": [1, 1, 1, 1],
            "dofz": [1, 1, 1, 1],
            "dofrx": [0, 0, 0, 0],
            "dofry": [0, 0, 0, 0],
            "dofrz": [0, 0, 0, 0],
        }
    )

    deck.extend(
        [
            BoundarySpcNode,
            kwd.InitialVelocity(boxid=5, vx=0.0, vy=-12.0, vz=0.0),
            kwd.DefineBox(boxid=5, xmn=-120.0, xmx=-80.0, ymn=80.0, ymx=120.0, zmn=-30.0, zmx=30.0),
        ]
    )

    # Define parts and materials

    spherePart = kwd.Part()
    spherePart.parts = pd.DataFrame({"heading": ["sphere1", "sphere2"], "pid": [1, 2], "secid": [1, 2], "mid": [1, 1]})
    beamPart = kwd.Part(heading="Pendulum Wires - Elastic Beams", pid=45, secid=45, mid=45)

    deck.extend(
        [
            spherePart,
            # Aluminium
            kwd.MatPlasticKinematic(mid=1, ro=2.7e-6, e=68.9, pr=0.330, sigy=0.286, etan=0.00689),
            # Sections
            kwd.SectionShell(secid=1, elfrom=2, t1=1.0, t2=1.0, t3=1.0, t4=1.0),
            kwd.SectionShell(secid=2, elfrom=2, t1=1.0, t2=1.0, t3=1.0, t4=1.0),
            # Pendu Wires
            beamPart,
            kwd.SectionBeam(secid=45, elform=3, shrf=1.00000, qr_irid=1.0, a=10.0),
            kwd.MatElastic(mid=45, ro=7.86e-6, e=210.0, pr=0.30),
        ]
    )

    # Define deformable switching
    deck.extend([kwd.DeformableToRigid(pid=1), kwd.DeformableToRigid(pid=2)])

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

filepath = run_dyna(dynafile, working_directory=rundir.name)
run_post(rundir.name)
