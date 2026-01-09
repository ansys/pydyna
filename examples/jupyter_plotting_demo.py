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
Jupyter notebook plotting
==========================

This example demonstrates the Jupyter notebook support for deck.plot().
The plotting system automatically detects Jupyter environments and adjusts
the PyVista backend accordingly.

When running in a Jupyter notebook, PyDyna automatically uses the 'static'
backend for screenshots, or you can explicitly choose 'server' for
interactive widgets. Outside Jupyter, it uses the standard PyVista behavior.

See GitHub issue #601 for details.
"""

###############################################################################
# Perform required imports
# ~~~~~~~~~~~~~~~~~~~~~~~~
# Import required packages.

import pandas as pd

from ansys.dyna.core import Deck
from ansys.dyna.core import keywords as kwd

###############################################################################
# Create a simple deck
# ~~~~~~~~~~~~~~~~~~~~
# Create a deck with 8 nodes forming a hex element.

deck = Deck()

# Create 8 nodes for a hex element
node_kwd = kwd.Node()
nodes_data = pd.DataFrame(
    {
        "nid": [1, 2, 3, 4, 5, 6, 7, 8],
        "x": [0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0],
        "y": [0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 1.0],
        "z": [0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0],
    }
)
node_kwd.nodes = nodes_data
deck.append(node_kwd)

# Create a solid element
solid_kwd = kwd.ElementSolid()
elements_data = pd.DataFrame(
    {"eid": [1], "pid": [1], "n1": [1], "n2": [2], "n3": [3], "n4": [4], "n5": [5], "n6": [6], "n7": [7], "n8": [8]}
)
solid_kwd.elements = elements_data
deck.append(solid_kwd)

# Add a section
section_kwd = kwd.SectionSolid(secid=1)
section_kwd.elform = 1
deck.append(section_kwd)

print(f"Deck created with {len(deck)} keywords")

###############################################################################
# Plot with auto-detection
# ~~~~~~~~~~~~~~~~~~~~~~~~~
# In Jupyter notebooks, this automatically uses the 'static' backend.
# Outside Jupyter, it uses the default PyVista behavior.

# deck.plot()

###############################################################################
# Plot with explicit backend
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# You can explicitly specify the backend:
# - 'static': screenshot (fast, works everywhere)
# - 'server': interactive widget (requires jupyter-server-proxy)
# - 'trame': trame-based viewer (experimental)
# - None: disable Jupyter handling, use standard PyVista

# deck.plot(jupyter_backend='static')

###############################################################################
# Plot with custom styling
# ~~~~~~~~~~~~~~~~~~~~~~~~~
# All standard PyVista plot options work with Jupyter:

# deck.plot(jupyter_backend='static', color='lightblue', show_edges=True)
