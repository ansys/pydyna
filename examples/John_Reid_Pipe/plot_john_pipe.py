# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
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


# %% [markdown]
# # John Reid Pipe Example
#
# This notebook demonstrates how to use PyDyna to create and solve a pipe example inspired by John Reid's classic LS-DYNA model. The workflow includes deck and keyword creation, mesh import, part and material definition, and running the LS-DYNA solver. Each step is explained for clarity and educational use.

# %% [markdown]
# ## 1. Perform Required Imports
# Import all necessary modules and classes for the pipe simulation.

# %%
import os
import shutil
import tempfile

import pandas as pd

from ansys.dyna.core import Deck
from ansys.dyna.core import keywords as kwd
from ansys.dyna.core.pre.examples.download_utilities import EXAMPLES_PATH, DownloadManager
from ansys.dyna.core.run import run_dyna

# %% [markdown]
# ## 2. Download and Prepare Mesh File
# Download the mesh file for the pipe example and set up a temporary run directory.

# %%
mesh_file_name = "nodes.k"
mesh_file = DownloadManager().download_file(
    mesh_file_name, "ls-dyna", "John_Reid_Pipe", destination=os.path.join(EXAMPLES_PATH, "John_Reid_Pipe")
)
rundir = tempfile.TemporaryDirectory()
dynafile = "pipe.k"

# %% [markdown]
# ## 3. Create Deck and Keywords
# Create a deck (container for all keywords), then define and append all necessary keywords for the simulation.


# %%
def write_deck(filepath):
    deck = Deck()
    # Append control keywords
    deck.extend(
        [
            kwd.ControlTermination(endtim=20.0),
            kwd.ControlEnergy(hgen=2, rwen=2, slnten=2),
            kwd.ControlOutput(npopt=1, neecho=3),
            kwd.ControlShell(istupd=1),
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
            kwd.DatabaseJntforc(dt=0.10),
            kwd.DatabaseRbdout(dt=0.10),
            kwd.DatabaseRcforc(dt=0.10),
        ]
    )
    # Define contacts - sliding interfaces
    deck.extend(
        [
            kwd.ContactForceTransducerPenalty(surfa=1, surfatyp=3),
            kwd.ContactAutomaticSingleSurface(ssid=3, sstyp=2, fs=0.30, fd=0.30),
            kwd.SetPartList(sid=3, parts=[1, 2]),
        ]
    )
    # Define initial conditions
    deck.extend(
        [
            kwd.InitialVelocityGeneration(id=5, omega=-0.082, xc=-78.50, yc=-610.13, zc=5.69, nx=1.0),
            kwd.SetPartList(sid=5, parts=[1, 2]),
        ]
    )
    # Define pipe parts and materials
    pipe_parts = kwd.Part()
    pipe_parts.parts = pd.DataFrame(
        {
            "heading": ["Deformable-Pipe", "Pipe-End", "Rigid-Pipe"],
            "pid": [1, 2, 3],
            "secid": [1, 2, 2],
            "mid": [1, 2, 1],
        }
    )
    deck.extend(
        [
            pipe_parts,
            # Aluminium
            kwd.MatPlasticKinematic(mid=1, ro=7.86e-6, e=200.0, pr=0.30, sigy=0.250, etan=0.00689),
            kwd.MatRigid(mid=2, ro=7.86e-6, e=200.0, pr=0.30),
            # Sections
            kwd.SectionShell(secid=1, elfrom=2, nip=5, t1=11.0, t2=11.0, t3=11.0, t4=11.0),
            kwd.SectionShell(secid=2, elfrom=2, nip=3, t1=11.0, t2=11.0, t3=11.0, t4=11.0),
        ]
    )
    # Define bracket parts and materials
    bracket_parts = kwd.Part(heading="Bracket", pid=4, secid=4, mid=4)
    deck.extend(
        [
            bracket_parts,
            kwd.MatRigid(mid=4, ro=7.86e-6, e=200.0, pr=0.30, cmo=1, con1=7, con2=7),
            kwd.SectionSolid(secid=4),
        ]
    )
    # Define deformable switching
    deck.extend(
        [
            kwd.DeformableToRigid(pid=1),
            kwd.DeformableToRigid(pid=2),
        ]
    )
    # Define nodes and elements
    deck.extend(
        [
            kwd.Include(filename=mesh_file_name),
        ]
    )
    deck.export_file(filepath)
    return deck


def run_post(filepath):
    pass


# %% [markdown]
# ## 4. Copy Mesh File and Write Deck
# Copy the mesh file to the run directory and write the deck to file.

# %%
shutil.copy(mesh_file, os.path.join(rundir.name, mesh_file_name))
deck = write_deck(os.path.join(rundir.name, dynafile))

# %% [markdown]
# ## 5. View the Model
# Use the PyVista `plot` method in the `deck` class to visualize the model.

# %%
deck.plot(cwd=rundir.name, show_edges=True)

# %% [markdown]
# ## 6. Run the LS-DYNA Solver
# Run the LS-DYNA solver on the generated input file.

# %%
run_dyna(dynafile, working_directory=rundir.name)
run_post(rundir.name)

# %% [markdown]
# ## 7. Conclusion
#
# This notebook has demonstrated the setup and solution of a pipe example using PyDyna and LS-DYNA. The workflow included deck and keyword creation, mesh import, part and material definition, and running the solver. This approach can be adapted for other LS-DYNA analyses, providing a clear, modular, and scriptable workflow for advanced simulations.
