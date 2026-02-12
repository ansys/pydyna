# Copyright (C) 2021 - 2026 ANSYS, Inc. and/or its affiliates.
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

"""Test RIGIDWALL_GEOMETRIC_*_ID keywords.

This test ensures that RIGIDWALL_GEOMETRIC keywords have the _ID variant
that includes the ID/TITLE card, as required by LS-DYNA and LS-PrePost.
"""

from ansys.dyna.core import keywords as kwd

import pytest


def test_rigidwall_geometric_flat_id():
    """Test RigidwallGeometricFlat with ID option writes correct keyword title."""
    wall = kwd.RigidwallGeometricFlat()
    wall.id = 1  # Setting id activates the ID option
    wall.nsid = 1
    wall.xt = 0.0
    wall.yt = 0.0
    wall.zt = -60.0
    wall.xh = 0.0
    wall.yh = 0.0
    wall.zh = -59.0
    wall.xhev = 1.0
    wall.yhev = 0.0
    wall.zhev = -60.0
    wall.fric = 0.0

    output = wall.write()
    assert "*RIGIDWALL_GEOMETRIC_FLAT_ID" in output
    assert "id" in output.lower()


def test_rigidwall_geometric_flat_no_id():
    """Test RigidwallGeometricFlat writes without ID card."""
    wall = kwd.RigidwallGeometricFlat()
    wall.nsid = 1

    output = wall.write()
    assert "*RIGIDWALL_GEOMETRIC_FLAT\n" in output
    assert "*RIGIDWALL_GEOMETRIC_FLAT_ID" not in output
    # The output should not have an ID/TITLE card (first card is NSID)
    lines = output.split("\n")
    assert "nsid" in lines[1].lower()


def test_rigidwall_geometric_variants_exist():
    """Test that all main GEOMETRIC variants exist and ID option works."""
    variants = [
        ("RigidwallGeometricCylinder", "*RIGIDWALL_GEOMETRIC_CYLINDER_ID"),
        ("RigidwallGeometricFlat", "*RIGIDWALL_GEOMETRIC_FLAT_ID"),
        ("RigidwallGeometricPrism", "*RIGIDWALL_GEOMETRIC_PRISM_ID"),
        ("RigidwallGeometricSphere", "*RIGIDWALL_GEOMETRIC_SPHERE_ID"),
    ]

    for class_name, expected_title in variants:
        cls = getattr(kwd, class_name)
        obj = cls()
        obj.id = 1  # Setting id activates the ID option
        output = obj.write()
        title_line = output.split("\n")[0]
        assert title_line == expected_title, f"Expected {expected_title}, got {title_line}"
