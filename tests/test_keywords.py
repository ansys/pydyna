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
Test the keyword library
"""

import math

import numpy as np
import pandas as pd

from ansys.dyna.core.lib.config import disable_lspp_defaults
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core import keywords as kwd

import pytest


@pytest.mark.keywords
def test_mesh(ref_string):
    ref_node = ref_string.test_mesh_string
    nodes = kwd.Node()
    nodes.nodes = pd.DataFrame(
        {
            "nid": [100, 101, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113],
            "x": [
                -0.2969848,
                -0.2687006,
                -0.160727,
                -0.1454197,
                -0.2969848,
                -0.2687006,
                -0.1454197,
                -0.160727,
                -0.2969848,
                -0.2687006,
                -0.1454197,
                -0.160727,
            ],
            "y": [
                0.2969848,
                0.2687006,
                0.3880294,
                0.3510742,
                0.2969848,
                0.2687006,
                0.3510742,
                0.3880294,
                0.2969848,
                0.2687006,
                0.3510742,
                0.3880294,
            ],
            "z": [0.0, 0.0, 0.0, 0.0, 0.25, 0.25, 0.25, 0.25, 0.5, 0.5, 0.5, 0.5],
        }
    )
    node_string = nodes.write()
    assert node_string == ref_node, "node block does not match"

    ref_element = """*ELEMENT_SOLID
$#   eid     pid      n1      n2      n3      n4      n5      n6      n7      n8
       1       1     100     101     105     104     106     107     108     109
       2       1     106     107     108     109     110     111     112     113"""

    elements = kwd.ElementSolid()
    elements.set_legacy_format()
    elements.elements = pd.DataFrame(
        {
            "eid": [1, 2],
            "pid": [1, 1],
            "n1": [100, 106],
            "n2": [101, 107],
            "n3": [105, 108],
            "n4": [104, 109],
            "n5": [106, 110],
            "n6": [107, 111],
            "n7": [108, 112],
            "n8": [109, 113],
        }
    )
    element_string = elements.write()
    assert element_string == ref_element, "element solid block does not match"


@pytest.mark.keywords
def test_read_node(ref_string):
    n = kwd.Node()
    n.loads(ref_string.test_node_long_id)
    assert n.nodes["nid"][0] == 69000001
    assert n.write() == ref_string.test_node_long_id


@pytest.mark.keywords
def test_nodes_from_dataframe():
    node_ids = np.arange(30) + 1
    xs = np.zeros(30) + 0.1
    ys = np.zeros(30) + 0.2
    zs = np.zeros(30) + 0.3
    df = pd.DataFrame({"nid": node_ids, "x": xs, "y": ys, "z": zs})
    n = kwd.Node()
    n.nodes = df
    table = n.nodes
    assert (len(table)) == 30
    for column in ["nid", "x", "y", "z"]:
        assert len(df[column]) == len(table[column]), (
            f"Length of {column} column doesn't match"
        )
        assert len(df[column].compare(table[column])) == 0, (
            f"{column} column values don't match"
        )


@pytest.mark.keywords
def test_read_segment():
    segment_text = """*SET_SEGMENT
$#     sid       da1       da2       da3       da4    solver
         2       0.0       0.0       0.0       0.0MECH
$#      n1        n2        n3        n4        a1        a2        a3        a4
      2145      2124      2004      2045       0.0       0.0       0.0       0.0
       262       265       264       263       0.0       0.0       0.0       0.0
       304       262       263       305       0.0       0.0       0.0       0.0
       263       264       385       384       0.0       0.0       0.0       0.0
       344       345       265       262       0.0       0.0       0.0       0.0
       265       445       444       264       0.0       0.0       0.0       0.0
      1265      1285       405       525       0.0       0.0       0.0       0.0
      1865      1844      1784      1805       0.0       0.0       0.0       0.0
       444       584       665       545       0.0       0.0       0.0       0.0
       905       845       803       344       0.0       0.0       0.0       0.0
       182      2204      2084      2165       0.0       0.0       0.0       0.0"""
    seg = kwd.SetSegment()
    seg.loads(segment_text)
    assert (seg.sid, seg.da1, seg.da2, seg.da3, seg.da4, seg.solver) == (
        2,
        0.0,
        0.0,
        0.0,
        0.0,
        "MECH",
    )


@pytest.mark.keywords
def test_read_keyword_no_data():
    seatbelt_text = """*SECTION_SEATBELT
$#   secid      area     thick
                              """
    belt = kwd.SectionSeatbelt()
    # ensure that it does not throw when the data line is empty
    belt.loads(seatbelt_text)

    seatbelt_text = seatbelt_text + "..."
    # ensure that it does not throw when extra characters are added
    with pytest.warns(UserWarning, match="Detected out of bound card characters"):
        belt.loads(seatbelt_text)


@pytest.mark.keywords
def test_read_nodes(ref_string):
    node_text = """*NODE
$#   nid               x               y               z      tc      rc
 2000000
 2000001   -2772.1652832     643.8095703     376.7990417
 2000002   -3093.8891602     685.0078125     811.2246704       2       5"""
    node = kwd.Node()
    node.loads(node_text)
    node_table = node.nodes
    assert len(node_table) == 3
    node_repr = """       nid            x           y           z    tc    rc
0  2000000          NaN         NaN         NaN  <NA>  <NA>
1  2000001 -2772.165283  643.809570  376.799042  <NA>  <NA>
2  2000002 -3093.889160  685.007812  811.224670     2     5"""
    assert repr(node_table) == node_repr
    assert node_table["x"][1] == -2772.1652832
    assert node_table["rc"][2] == 5
    assert pd.isna(node_table["tc"][0])
    node_table.loc[2, "tc"] = 1
    node_text = ref_string.test_read_nodes_string
    assert repr(node) == node_text
    assert node.write() == node_text


@pytest.mark.keywords
def test_read_keyword_no_defaults():
    m = kwd.MatHyperelasticRubber()
    assert m.n == 0  # LSPP default for `n` is 0.
    assert m.pr is None  # No LSPP default for `pr`
    with disable_lspp_defaults():
        m = kwd.MatHyperelasticRubber()
        assert m.n == None  # LSPP default for `n` is 0.


@pytest.mark.keywords
def test_boundary_prescribed_motion_set(ref_string):
    b = kwd.BoundaryPrescribedMotionSet()
    assert b.write() == ref_string.test_boundary_prescribed_motion_set
    b.loads(ref_string.test_boundary_prescribed_motion_set2)
    assert b.lcid == 100


@pytest.mark.keywords
def test_define_curve_defaults():
    curve = kwd.DefineCurve()
    curve.curves = pd.DataFrame({"o1": [2, 4, 6]})
    table = curve.curves
    assert len(table) == 3
    assert table["o1"][0] == 2
    assert table["a1"][0] == 0.0

    with disable_lspp_defaults():
        curve = kwd.DefineCurve()
        curve.curves = pd.DataFrame({"o1": [2, 4, 6]})
        table = curve.curves
        assert pd.isna(table["a1"][0])


@pytest.mark.keywords
def test_constrained_beam_in_solid(ref_string):
    b = kwd.ConstrainedBeamInSolid(ncoup=1)
    b.coupid = 12
    assert b.write() == ref_string.test_constrained_beam_in_solid
    b.options["ID"].active = True
    assert b.options["ID"].active == True
    assert b.options["TITLE"].active == False
    assert b.write() == ref_string.test_constrained_beam_in_solid_id
    b.options["TITLE"].active = True
    assert b.options["TITLE"].active == True
    assert b.options["ID"].active == False
    assert b.write() == ref_string.test_constrained_beam_in_solid_title


@pytest.mark.keywords
def test_hourglass(ref_string):
    h = kwd.Hourglass()
    h.options["TITLE"].active = True
    assert h.get_title() == "*HOURGLASS_TITLE"
    h.add_set(title="hello")
    assert h.write() == ref_string.test_hourglass_title
    h = kwd.Hourglass()
    assert len(h.sets) == 0
    h.loads(ref_string.test_hourglass_title)
    assert len(h.sets) == 1


@pytest.mark.keywords
def test_load_segment(ref_string):
    seg = kwd.LoadSegment()
    assert seg.write() == ref_string.test_load_segment_string
    seg = kwd.LoadSegmentId()
    assert seg.write() == ref_string.test_load_segment_id_string


@pytest.mark.keywords
def test_section_shell(ref_string):
    shell = kwd.SectionShell(secid=1, elfrom=2, t1=1.0, t2=1.0, t4=1.0, nip=5)
    shell.t3 = 1.0
    shell_string = shell.write()
    assert shell_string == ref_string.test_section_shell_one_set
    assert shell.sets[0].secid == 1
    assert shell.secid == 1
    shell.add_set(secid=2, elform=3, nip=5, marea=1.0)
    assert shell.write() == ref_string.test_section_shell_two_sets
    with pytest.raises(LookupError):
        x = shell.secid


@pytest.mark.keywords
def test_section_tshell(ref_string):
    tshell = kwd.SectionTShell(format=format_type.long)
    # check that the keyword wries using the long format
    assert tshell.format == format_type.long
    assert tshell.write() == ref_string.test_section_shell_long

    tshell.format = format_type.standard
    assert tshell.write(format=format_type.long) == ref_string.test_section_shell_long

    # check that the series card is saved and loaded correctly
    tshell = kwd.SectionTShell(icomp=1, nip=9)
    tshell.bi[0] = 1.0
    tshell.bi[1] = 2.0
    tshell.bi[2] = 0.0
    tshell.bi[8] = 12.0
    tshell_string = tshell.write(format=format_type.long)
    tshell = kwd.SectionTShell()
    tshell.loads(tshell_string)
    assert tshell.icomp == 1
    assert tshell.nip == 9
    assert tshell.bi[0] == 1.0
    assert tshell.bi[2] == 0.0
    assert math.isnan(tshell.bi[3])
    assert tshell.bi[8] == 12.0


@pytest.mark.keywords
def test_section_solid(ref_string):
    ss = kwd.SectionSolid()
    # check the default output
    assert ss.write() == ref_string.test_ss_string
    # check the output with elform = 101
    ss.elform = 101
    assert ss.write() == ref_string.test_ss_elform_101_string
    # check the output with elform = 101, nip=2
    ss.nip = 2
    ss.integration_points = pd.DataFrame(
        {"xi": [1, 0], "eta": [2, np.nan], "zeta": [3, 3], "wgt": [np.nan, 5.0]}
    )
    assert ss.write() == ref_string.test_ss_elform_101_nip_2_string
    # check the output with elform = 101, nip = 2, lmc=9
    ss.lmc = 9
    ss.pi[0] = 22
    ss.pi[8] = 3.7
    assert ss.write() == ref_string.test_ss_elform_101_nip_2_lmc_9_string


@pytest.mark.keywords
def test_initial_strain_shell(ref_string):
    # test read
    ref = """*INITIAL_STRAIN_SHELL
         1         1         5
 1.96E-003 7.65E-003-9.61E-003-2.29E-004-5.79E-004 1.41E-004-1.00E+000
 2.30E-003 7.56E-003-9.86E-003-6.10E-004-5.89E-004 1.61E-004-5.00E-001
 2.63E-003 7.47E-003-1.01E-002-9.91E-004-5.98E-004 1.80E-004 0.00E+000
 2.97E-003 7.36E-003-1.03E-002-1.34E-003-6.07E-004 1.98E-004 5.00E-001
 3.32E-003 7.26E-003-1.06E-002-1.68E-003-6.15E-004 2.17E-004 1.00E+000
         2         1         5
 1.96E-003 7.27E-003-9.23E-003-3.36E-004-3.96E-004-1.32E-005-1.00E+000
 2.11E-003 7.05E-003-9.16E-003-5.73E-004-3.88E-004-7.72E-006-5.00E-001
 2.27E-003 6.82E-003-9.09E-003-8.10E-004-3.81E-004-2.20E-006 0.00E+000
 2.45E-003 6.58E-003-9.03E-003-1.04E-003-3.73E-004 3.18E-006 5.00E-001
 2.63E-003 6.34E-003-8.96E-003-1.28E-003-3.65E-004 8.55E-006 1.00E+000
         3         1         5
 2.30E-003 7.28E-003-9.58E-003 1.63E-004-3.67E-004-2.03E-005-1.00E+000
 2.35E-003 7.14E-003-9.48E-003 1.57E-004-3.62E-004-2.01E-005-5.00E-001
 2.39E-003 7.00E-003-9.39E-003 1.51E-004-3.57E-004-1.99E-005 0.00E+000
 2.43E-003 6.88E-003-9.31E-003 1.73E-004-3.52E-004-2.03E-005 5.00E-001
 2.47E-003 6.75E-003-9.22E-003 1.96E-004-3.48E-004-2.07E-005 1.00E+000"""
    i = kwd.InitialStrainShell()
    i.loads(ref)
    assert i.sets[1].eid == 2
    assert i.sets[2].strains[0].epszz == -9.58e-003

    # test write
    i = kwd.InitialStrainShell()
    i.add_set(eid=1, nplane=1, nthick=5, large=0)
    for idx in range(5):
        i.sets[0].strains[idx].epsxx = 1.0
    assert 1 == i.sets[0].eid
    i.add_set(eid=2, nplane=1, nthick=5, large=0)
    for idx, val in enumerate([22, 2, 2, 2, 2]):
        i.sets[1].strains[idx].epsxy = val
    assert i.write() == ref_string.test_initial_strain_shell_string


@pytest.mark.keywords
def test_initial_strain_shell_large_format():
    """Test INITIAL_STRAIN_SHELL with LARGE format (width=20 fields).
    
    LARGE=1 format splits strain data across 2 cards:
    - Card 1: EPSXX, EPSYY, EPSZZ, EPSXY, EPSYZ (5 fields × 20 chars = 100 chars)
    - Card 2: EPSZX, T (2 fields × 20 chars = 40 chars)
    """
    i = kwd.InitialStrainShell()
    i.add_set(eid=1, nplane=1, nthick=2, large=1)

    # Set strain values for first integration point
    i.sets[0].large_strains[0].epsxx = 1.23456789012345e-3
    i.sets[0].large_strains[0].epsyy = 2.34567890123456e-3
    i.sets[0].large_strains[0].epszz = 3.45678901234567e-3
    i.sets[0].large_strains[0].epsxy = 4.56789012345678e-3
    i.sets[0].large_strains[0].epsyz = 5.67890123456789e-3
    i.sets[0].large_strains[0].epszx = 6.78901234567890e-3
    i.sets[0].large_strains[0].t = -1.0

    # Write and verify LARGE format
    output = i.write()
    lines = [l for l in output.split('\n') if l.strip() and not l.strip().startswith('$')]

    # Check that large=1 is set in header
    assert 'INITIAL_STRAIN_SHELL' in output
    eid_line_idx = next(i for i, l in enumerate(lines) if l.strip().startswith('1'))
    
    # Card 1: EPSXX, EPSYY, EPSZZ, EPSXY, EPSYZ (5 fields, 100 chars)
    card1_idx = eid_line_idx + 1
    card1 = lines[card1_idx]
    assert len(card1) == 100, f"Card 1 should be 100 chars, got {len(card1)}: '{card1}'"
    assert '0.0012345' in card1 or '1.2345' in card1  # EPSXX
    
    # Card 2: EPSZX, T (2 fields, 40 chars)
    card2_idx = eid_line_idx + 2
    card2 = lines[card2_idx]
    assert len(card2) <= 40, f"Card 2 should be ≤40 chars, got {len(card2)}: '{card2}'"
    assert '-1.0' in card2  # T value

    # Test round-trip
    i2 = kwd.InitialStrainShell()
    i2.loads(output)
    assert len(i2.sets) == 1
    assert i2.sets[0].large == 1
    assert len(i2.sets[0].large_strains) == 2  # nplane * nthick = 1 * 2
    # Verify precision is preserved with LARGE format
    assert abs(i2.sets[0].large_strains[0].epsxx - 1.23456789012345e-3) < 1e-15


@pytest.mark.keywords
def test_initial_stress_shell_single_element_single_layer(ref_string):
    i = kwd.InitialStressShell()
    i.loads(ref_string.test_initial_stress_shell_string_single_element_single_layer)
    assert len(i.sets) == 1
    element0 = i.sets[0]
    assert element0.eid == 1
    assert element0.nhisv == 19
    assert element0.large == 0
    assert element0.nthick == 1
    assert element0.nplane == 1
    assert len(element0.sets) == 1
    assert len(i.sets) == 1
    stress0 = element0.sets[0]
    assert stress0.sigxx == 0.0
    assert stress0.t == -1.0
    assert stress0.eps == 0.194
    assert len(stress0.hisv) == element0.nhisv
    assert stress0.hisv[18] == 0.311


@pytest.mark.keywords
def test_initial_stress_shell_single_element_multiple_layers(ref_string):
    i = kwd.InitialStressShell()
    i.loads(ref_string.test_initial_stress_shell_string_single_element_multiple_layers)
    assert len(i.sets) == 1
    element0 = i.sets[0]
    assert element0.nhisv == 19
    assert len(element0.sets) == 5
    stress1 = element0.sets[1]
    assert stress1.t == -0.5
    assert stress1.hisv[18] == 0.32


@pytest.mark.keywords
def test_initial_stress_shell(ref_string):
    i = kwd.InitialStressShell()
    i.loads(ref_string.test_initial_stress_shell_string)
    assert len(i.sets) == 3
    element1 = i.sets[1]
    assert element1.eid == 2
    assert element1.nhisv == 19
    assert len(element1.sets) == 5
    stress4 = element1.sets[4]
    assert stress4.t == 1.0
    assert len(stress4.hisv) == 19
    assert stress4.hisv[5] == 0.163


@pytest.mark.keywords
def test_initial_stress_shell_large_format():
    """Test INITIAL_STRESS_SHELL with LARGE format (width=20 fields).
    
    LARGE=1 format splits stress data across 2 cards:
    - Card 1: T, SIGXX, SIGYY, SIGZZ, SIGXY (5 fields × 20 chars = 100 chars)
    - Card 2: SIGYZ, SIGZX, EPS (3 fields × 20 chars = 60 chars)
    - History variables: 5 per card × 20 chars = 100 chars per card
    """
    i = kwd.InitialStressShell()
    i.add_set(eid=1, nplane=1, nthick=2, nhisv=6, large=1)
    # For LARGE format, use large_sets instead of sets
    i.sets[0].large_sets[0].t = -1.0
    i.sets[0].large_sets[0].sigxx = 1.23456789012345
    i.sets[0].large_sets[0].sigyy = 2.34567890123456
    i.sets[0].large_sets[0].sigzz = 3.45
    i.sets[0].large_sets[0].sigxy = 4.56
    i.sets[0].large_sets[0].sigyz = 5.67
    i.sets[0].large_sets[0].sigzx = 6.78
    i.sets[0].large_sets[0].eps = 0.5
    i.sets[0].large_sets[0].hisv.data = [10.1, 20.2, 30.3, 40.4, 50.5, 60.6]

    # Write and verify LARGE format structure
    output = i.write()
    lines = [l for l in output.split('\n') if l.strip() and not l.strip().startswith('$')]  # Remove empty/comment lines

    # Check header
    assert 'INITIAL_STRESS_SHELL' in output

    # Find the main card with EID, NPLANE, NTHICK, NHISV, LARGE
    eid_line_idx = next(i for i, l in enumerate(lines) if l.strip().startswith('1'))

    # Card 1: T, SIGXX, SIGYY, SIGZZ, SIGXY (5 fields, 100 chars)
    card1_idx = eid_line_idx + 1
    card1 = lines[card1_idx]
    assert len(card1) == 100, f"Card 1 should be 100 chars, got {len(card1)}: '{card1}'"
    assert '-1.0' in card1  # T value
    assert '1.23456' in card1  # SIGXX
    assert '2.34567' in card1  # SIGYY

    # Card 2: SIGYZ, SIGZX, EPS (3 fields, 60 chars)
    card2_idx = eid_line_idx + 2
    card2 = lines[card2_idx]
    assert len(card2) <= 60, f"Card 2 should be ≤60 chars, got {len(card2)}: '{card2}'"
    assert '5.66' in card2 or '5.67' in card2  # SIGYZ
    assert '6.78' in card2  # SIGZX
    assert '0.5' in card2   # EPS

    # History variable cards: 5 values per card (100 chars each)
    hisv_card1_idx = eid_line_idx + 3
    hisv_card1 = lines[hisv_card1_idx]
    assert len(hisv_card1) == 100, f"HISV card 1 should be 100 chars, got {len(hisv_card1)}: '{hisv_card1}'"
    assert '10.0' in hisv_card1 or '10.1' in hisv_card1  # Check for HISV value (allow fp precision)
    
    # Test round-trip: read back what we wrote
    i2 = kwd.InitialStressShell()
    i2.loads(output)
    assert len(i2.sets) == 1
    assert i2.sets[0].large == 1
    assert i2.sets[0].large_sets[0].sigxx == 1.23456789012345
    assert i2.sets[0].large_sets[0].sigyy == 2.34567890123456
    assert len(i2.sets[0].large_sets[0].hisv.data) == 6


@pytest.mark.keywords
def test_initial_temperature(ref_string):
    tin = kwd.InitialTemperatureNode()
    tin.loads(ref_string.test_initial_temperature_node_string)
    assert tin.write() == ref_string.test_initial_temperature_node_string
    tis = kwd.InitialTemperatureSet()
    tis.loads(ref_string.test_initial_temperature_set_string)
    assert tis.write() == ref_string.test_initial_temperature_set_string


@pytest.mark.keywords
def test_element_shell_thickness(ref_string):
    ref_element = """*ELEMENT_SHELL_THICKNESS
       1       1       1     105       2       2
 1.98036024E+000 1.97992622E+000 1.97992622E+000 1.97992622E+000 1.49965326E+002
       2       1     136     133    2834    2834
 1.98166233E+000 1.98166233E+000 1.98296441E+000 1.98296441E+000 1.46006557E+002
       3       1     141     146     135     135
 1.98187934E+000 1.97949219E+000 1.98280165E+000 1.98280165E+000 9.00245614E+001"""
    elements = kwd.ElementShellThickness()
    elements.loads(ref_element)
    assert elements.elements.loc[0, "eid"] == 1
    assert elements.elements.loc[0, "thic1"] == 1.98036024
    elements.elements.loc[0, "thic1"] = 2
    assert elements.elements.loc[0, "thic1"] == 2
    assert elements.write() == ref_string.element_shell_thickness_string


@pytest.mark.keywords
def test_element_solid_ortho(ref_string):
    # assign with one dataframe for all cards
    elements = kwd.ElementSolidOrtho()
    elements.set_legacy_format()
    df_elements = pd.DataFrame(
        {
            "eid": [1, 2],
            "pid": [1, 1],
            "n1": [100, 106],
            "n2": [101, 107],
            "n3": [105, 108],
            "n4": [104, 109],
            "n5": [106, 110],
            "n6": [107, 111],
            "n7": [108, 112],
            "n8": [109, 113],
            "a1": [0.4, 0.1],
            "a2": [0.3, 0.9],
            "a3": [0.1, 0.6],
            "d1": [0.1, 0.0],
            "d2": [0.8, 0.0],
            "d3": [0.2, 0.1],
        }
    )
    elements.elements = df_elements
    element_string = elements.write()
    assert element_string == ref_string.element_solid_ortho_legacy, (
        "element solid ortho block does not match"
    )

    # reading using the non-legacy format
    elements = kwd.ElementSolidOrtho()
    elements.loads(ref_string.element_solid_ortho)
    assert len(elements.elements) == 25

    # reading using the legacy format
    elements = kwd.ElementSolidOrtho()
    elements.loads(element_string)
    assert len(elements.elements) == 2
    assert len(elements.cards[0]._cards) == 3


@pytest.mark.keywords
def test_control_mpp_decomposition_transformation(ref_string):
    """Read CONTROL_MPP_DECOMPOSITION_TRANSFORMATION"""
    c = kwd.ControlMppDecompositionTransformation()
    c.loads(ref_string.test_control_mpp_decomposition_transformation_string_read)
    assert (
        ref_string.test_control_mpp_decomposition_transformation_string_write
        == c.write()
    )


@pytest.mark.keywords
def test_control_implicit_eigenvalue(ref_string):
    """Read CONTROL_MPP_DECOMPOSITION_TRANSFORMATION"""
    c = kwd.ControlImplicitEigenvalue(neig=100)
    assert (ref_string.test_control_implicit_eigenvalue_1 == c.write())
    c.ishell = 1
    assert (ref_string.test_control_implicit_eigenvalue_2 == c.write())
    c.ishell = 0
    c.eigmth = 102
    assert (ref_string.test_control_implicit_eigenvalue_3 == c.write())


@pytest.mark.keywords
def test_control_time_step_read(ref_string):
    """Read CONTROL_TIME_STEP"""
    c = kwd.ControlTimeStep()
    c.loads(ref_string.test_control_time_step_string)
    assert c.tssfac == 1.0


@pytest.mark.keywords
def test_control_timestep_read(ref_string):
    """Read CONTROL_TIMESTEP"""
    c = kwd.ControlTimestep()
    c.loads(ref_string.test_control_timestep_string)
    assert c.tssfac == 1.0


@pytest.mark.keywords
def test_mat_plastic_kinematic_read(ref_string):
    ref_mat_plastic_kinematic_string = ref_string.test_mat_plastic_kinematic_ref
    m = kwd.MatPlasticKinematic()
    m.loads(ref_mat_plastic_kinematic_string)
    assert m.write() == ref_mat_plastic_kinematic_string


@pytest.mark.keywords
def test_mat_null_read(ref_string):
    ref_mat_null_string = ref_string.test_mat_null_ref
    m = kwd.MatNull()
    m.loads(ref_mat_null_string)
    assert m.write() == ref_mat_null_string


@pytest.mark.keywords
def test_mat_piecewise_linear_plasticity_read(ref_string):
    ref_mat_piecewise_linear_plasticity_string = (
        ref_string.test_mat_piecewise_linear_plasticity_ref
    )
    m = kwd.MatPiecewiseLinearPlasticity()
    m.loads(ref_mat_piecewise_linear_plasticity_string)
    assert m.write() == ref_mat_piecewise_linear_plasticity_string


"""@pytest.mark.keywords
def test_mat_piecewise_linear_plasticity_2d_read(ref_string):
    ref_mat_piecewise_linear_plasticity_2d_string = \
        ref_string.test_mat_piecewise_linear_plasticity_2d_ref
    m = kwd.MatPiecewiseLinearPlasticity2d()
    m.loads(ref_mat_piecewise_linear_plasticity_2d_string)
    assert m.write() == ref_mat_piecewise_linear_plasticity_2d_string"""


@pytest.mark.keywords
def test_mat_piecewise_linear_plasticity_haz_read(ref_string):
    ref_mat_piecewise_linear_plasticity_haz_string = (
        ref_string.test_mat_piecewise_linear_plasticity_haz_ref
    )
    m = kwd.MatPiecewiseLinearPlasticityHaz()
    m.loads(ref_mat_piecewise_linear_plasticity_haz_string)
    assert m.write() == ref_mat_piecewise_linear_plasticity_haz_string


@pytest.mark.keywords
def test_mat_piecewise_linear_plasticity_log_interpolation_read(ref_string):
    ref_mat_piecewise_linear_plasticity_log_interpolation_string = (
        ref_string.test_mat_piecewise_linear_plasticity_log_interpolation_ref
    )
    m = kwd.MatPiecewiseLinearPlasticityLogInterpolation()
    m.loads(ref_mat_piecewise_linear_plasticity_log_interpolation_string)
    assert m.write() == ref_mat_piecewise_linear_plasticity_log_interpolation_string


@pytest.mark.keywords
def test_mat_piecewise_linear_plasticity_midfail_read(ref_string):
    ref_mat_piecewise_linear_plasticity_midfail_string = (
        ref_string.test_mat_piecewise_linear_plasticity_midfail_ref
    )
    m = kwd.MatPiecewiseLinearPlasticityMidfail()
    m.loads(ref_mat_piecewise_linear_plasticity_midfail_string)
    assert m.write() == ref_mat_piecewise_linear_plasticity_midfail_string


@pytest.mark.keywords
def test_mat_piecewise_linear_plasticity_stochastic_read(ref_string):
    ref_mat_piecewise_linear_plasticity_stochastic_string = (
        ref_string.test_mat_piecewise_linear_plasticity_stochastic_ref
    )
    m = kwd.MatPiecewiseLinearPlasticityStochastic()
    m.loads(ref_mat_piecewise_linear_plasticity_stochastic_string)
    assert m.write() == ref_mat_piecewise_linear_plasticity_stochastic_string


@pytest.mark.keywords
def test_mat_laminated_composite_fabric_read(ref_string):
    ref_mat_laminated_composite_fabric_string = (
        ref_string.test_mat_laminated_composite_fabric_ref
    )
    m = kwd.MatLaminatedCompositeFabric()
    m.loads(ref_mat_laminated_composite_fabric_string)
    assert m.write() == ref_mat_laminated_composite_fabric_string


@pytest.mark.keywords
def test_mat_laminated_composite_fabric_solid_read(ref_string):
    ref_mat_laminated_composite_fabric_solid_string = (
        ref_string.test_mat_laminated_composite_fabric_solid_ref
    )
    m = kwd.MatLaminatedCompositeFabricSolid()
    m.loads(ref_mat_laminated_composite_fabric_solid_string)
    assert m.write() == ref_mat_laminated_composite_fabric_solid_string


@pytest.mark.keywords
def test_mat_hyperelastic_rubber_read(ref_string):
    ref_mat_hyperelastic_rubber_string = ref_string.test_mat_hyperelastic_rubber_ref
    m = kwd.MatHyperelasticRubber()
    m.loads(ref_mat_hyperelastic_rubber_string)
    assert m.write() == ref_mat_hyperelastic_rubber_string
    m.pr = -1
    assert m.cards[1].active
    m.n = 1
    assert m.cards[2].active
    m.n = 0
    assert m.cards[3].active


@pytest.mark.keywords
def test_mat_ogden_rubber_read(ref_string):
    ref_mat_ogden_rubber_string = ref_string.test_mat_ogden_rubber_ref
    m = kwd.MatOgdenRubber()
    m.loads(ref_mat_ogden_rubber_string)
    assert m.write() == ref_mat_ogden_rubber_string
    m.pr = -1
    assert m.cards[1].active
    m.n = 1
    assert m.cards[2].active
    m.n = 0
    assert m.cards[3].active


@pytest.mark.keywords
def test_mat_fu_chang_foam_read(ref_string):
    ref_mat_fu_chang_foam_string = ref_string.test_mat_fu_chang_foam_ref
    m = kwd.MatFuChangFoam()
    m.loads(ref_mat_fu_chang_foam_string)
    assert m.write() == ref_mat_fu_chang_foam_string


@pytest.mark.keywords
def test_mat_fu_chang_foam_damage_decay_read(ref_string):
    ref_mat_fu_chang_foam_damage_decay_string = (
        ref_string.test_mat_fu_chang_foam_damage_decay_ref
    )
    m = kwd.MatFuChangFoamDamageDecay()
    m.loads(ref_mat_fu_chang_foam_damage_decay_string)
    assert m.write() == ref_mat_fu_chang_foam_damage_decay_string


@pytest.mark.keywords
def test_mat_fu_chang_foam_log_log_interpolation_read(ref_string):
    ref_mat_fu_chang_foam_log_log_interpolation_string = (
        ref_string.test_mat_fu_chang_foam_log_log_interpolation_ref
    )
    m = kwd.MatFuChangFoamLogLogInterpolation()
    m.loads(ref_mat_fu_chang_foam_log_log_interpolation_string)
    assert m.write() == ref_mat_fu_chang_foam_log_log_interpolation_string


@pytest.mark.keywords
def test_mat_modified_johnson_cook_read(ref_string):
    ref_mat_modified_johnson_cook_string = ref_string.test_mat_modified_johnson_cook_ref
    m = kwd.MatModifiedJohnsonCook()
    m.loads(ref_mat_modified_johnson_cook_string)
    assert m.write() == ref_mat_modified_johnson_cook_string


@pytest.mark.keywords
def test_mat_modified_piecewise_linear_plasticity_read(ref_string):
    ref_mat_modified_piecewise_linear_plasticity_string = (
        ref_string.test_mat_modified_piecewise_linear_plasticity_ref
    )
    m = kwd.MatModifiedPiecewiseLinearPlasticity()
    m.loads(ref_mat_modified_piecewise_linear_plasticity_string)
    assert m.write() == ref_mat_modified_piecewise_linear_plasticity_string


@pytest.mark.keywords
def test_mat_modified_piecewise_linear_plasticity_log_interpolation_read(ref_string):
    ref_mat_modified_piecewise_linear_plasticity_log_interpolation_string = (
        ref_string.test_mat_modified_piecewise_linear_plasticity_log_interpolation_ref
    )
    m = kwd.MatModifiedPiecewiseLinearPlasticityLogInterpolation()
    m.loads(ref_mat_modified_piecewise_linear_plasticity_log_interpolation_string)
    assert (
        m.write()
        == ref_mat_modified_piecewise_linear_plasticity_log_interpolation_string
    )


@pytest.mark.keywords
def test_mat_modified_piecewise_linear_plasticity_prestrain_read(ref_string):
    ref_mat_modified_piecewise_linear_plasticity_prestrain_string = (
        ref_string.test_mat_modified_piecewise_linear_plasticity_prestrain_ref
    )
    m = kwd.MatModifiedPiecewiseLinearPlasticityPrestrain()
    m.loads(ref_mat_modified_piecewise_linear_plasticity_prestrain_string)
    assert m.write() == ref_mat_modified_piecewise_linear_plasticity_prestrain_string


@pytest.mark.keywords
def test_mat_modified_piecewise_linear_plasticity_rate_read(ref_string):
    ref_mat_modified_piecewise_linear_plasticity_rate_string = (
        ref_string.test_mat_modified_piecewise_linear_plasticity_rate_ref
    )
    m = kwd.MatModifiedPiecewiseLinearPlasticityRate()
    m.loads(ref_mat_modified_piecewise_linear_plasticity_rate_string)
    assert m.write() == ref_mat_modified_piecewise_linear_plasticity_rate_string


@pytest.mark.keywords
def test_mat_modified_piecewise_linear_plasticity_rtcl_read(ref_string):
    ref_mat_modified_piecewise_linear_plasticity_rtcl_string = (
        ref_string.test_mat_modified_piecewise_linear_plasticity_rtcl_ref
    )
    m = kwd.MatModifiedPiecewiseLinearPlasticityRtcl()
    m.loads(ref_mat_modified_piecewise_linear_plasticity_rtcl_string)
    assert m.write() == ref_mat_modified_piecewise_linear_plasticity_rtcl_string


@pytest.mark.keywords
def test_mat_modified_piecewise_linear_plasticity_stochastic_read(ref_string):
    ref_mat_modified_piecewise_linear_plasticity_stochastic_string = (
        ref_string.test_mat_modified_piecewise_linear_plasticity_stochastic_ref
    )
    m = kwd.MatModifiedPiecewiseLinearPlasticityStochastic()
    m.loads(ref_mat_modified_piecewise_linear_plasticity_stochastic_string)
    assert m.write() == ref_mat_modified_piecewise_linear_plasticity_stochastic_string


@pytest.mark.keywords
def test_mat_plasticity_compression_tension_read(ref_string):
    ref_mat_plasticity_compression_tension_string = (
        ref_string.test_mat_plasticity_compression_tension_ref
    )
    m = kwd.MatPlasticityCompressionTension()
    m.loads(ref_mat_plasticity_compression_tension_string)
    assert m.write() == ref_mat_plasticity_compression_tension_string


@pytest.mark.keywords
def test_mat_cohesive_mixed_mode_read(ref_string):
    ref_mat_cohesive_mixed_mode_string = ref_string.test_mat_cohesive_mixed_mode_ref
    m = kwd.MatCohesiveMixedMode()
    m.loads(ref_mat_cohesive_mixed_mode_string)
    assert m.write() == ref_mat_cohesive_mixed_mode_string


@pytest.mark.keywords
def test_mat_simplified_rubber_foam_read(ref_string):
    ref_mat_simplified_rubber_foam_string = (
        ref_string.test_mat_simplified_rubber_foam_ref
    )
    m = kwd.MatSimplifiedRubberFoam()
    m.loads(ref_mat_simplified_rubber_foam_string)
    assert m.write() == ref_mat_simplified_rubber_foam_string


@pytest.mark.keywords
def test_mat_simplified_rubber_foam_log_log_interpolation_read(ref_string):
    ref_mat_simplified_rubber_foam_log_log_interpolation_string = (
        ref_string.test_mat_simplified_rubber_foam_log_log_interpolation_ref
    )
    m = kwd.MatSimplifiedRubberFoamLogLogInterpolation()
    m.loads(ref_mat_simplified_rubber_foam_log_log_interpolation_string)
    assert m.write() == ref_mat_simplified_rubber_foam_log_log_interpolation_string


@pytest.mark.keywords
def test_mat_simplified_rubber_foam_with_failure_read(ref_string):
    ref_mat_simplified_rubber_foam_with_failure_string = (
        ref_string.test_mat_simplified_rubber_foam_with_failure_ref
    )
    m = kwd.MatSimplifiedRubberFoamWithFailure()
    m.loads(ref_mat_simplified_rubber_foam_with_failure_string)
    assert m.write() == ref_mat_simplified_rubber_foam_with_failure_string


@pytest.mark.keywords
def test_mat_simplified_rubber_foam_with_failure_log_log_interpolation_read(ref_string):
    ref_mat_simplified_rubber_foam_with_failure_log_log_interpolation_string = ref_string.test_mat_simplified_rubber_foam_with_failure_log_log_interpolation_ref
    m = kwd.MatSimplifiedRubberFoamWithFailureLogLogInterpolation()
    m.loads(ref_mat_simplified_rubber_foam_with_failure_log_log_interpolation_string)
    assert (
        m.write()
        == ref_mat_simplified_rubber_foam_with_failure_log_log_interpolation_string
    )


def test_mat_196_read(ref_string):
    m = kwd.Mat196()
    m_alias = kwd.MatGeneralSpringDiscreteBeam()
    m.loads(ref_string.test_mat_196_ref_in)
    m_alias.loads(ref_string.test_mat_general_spring_discrete_beam_ref_in)
    assert (m.springs["dof"] == m_alias.springs["dof"]).all() == True
    assert m.write() == ref_string.test_mat_196_ref_out
    assert m_alias.write() == ref_string.test_mat_general_spring_discrete_beam_ref_out


@pytest.mark.keywords
def test_mat295_legacy_read(ref_string):
    """Round trip test of reading MAT_295 using legacy 0.9.1 API (backward compatibility)."""
    from ansys.dyna.core.keywords.keyword_classes.manual.mat_295_version_0_9_1 import Mat295Legacy

    import warnings

    ref_mat295_string = ref_string.test_mat_295_ref
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", DeprecationWarning)
        m = Mat295Legacy()
    m.loads(ref_mat295_string)
    assert m.ftype == 1
    assert m.rho == 0.001
    assert len(m.anisotropic_settings) == 1
    assert m.actype == 1
    assert m.l == 1.85
    assert m.k2 == 1.75
    assert m.write() == ref_mat295_string


@pytest.mark.keywords
def test_mat295_legacy_iso_read(ref_string):
    """Round trip test of reading MAT_295 ISO using legacy 0.9.1 API (backward compatibility)."""
    from ansys.dyna.core.keywords.keyword_classes.manual.mat_295_version_0_9_1 import Mat295Legacy

    import warnings

    with warnings.catch_warnings():
        warnings.simplefilter("ignore", DeprecationWarning)
        m = Mat295Legacy(mid=1, rho=0.01, aopt=2, itype=1, beta=2.0, nu=0.49, mu1=1, alpha1=2)
    assert m.ftype is None
    assert m.actype is None
    ref_mat295_string = ref_string.test_mat_295_iso
    assert m.write() == ref_mat295_string


@pytest.mark.keywords
def test_mat295_read(ref_string):
    """Round trip test of reading MAT_295 using idiomatic fiber_families API."""
    ref_mat295_string = ref_string.test_mat_295_ref
    m = kwd.Mat295()
    m.loads(ref_mat295_string)
    assert m.rho == 0.001
    assert len(m.fiber_families) == 1
    assert m.fiber_families[0].ftype == 1
    assert m.actype == 1
    assert m.l == 1.85
    assert m.k2 == 1.75  # k2 from itype=3 (Holzapfel-Ogden) card
    assert m.write() == ref_mat295_string


@pytest.mark.keywords
def test_mat295_iso_read(ref_string):
    """Round trip test of reading MAT_295 ISO (no fiber families)."""
    m = kwd.Mat295(mid=1, rho=0.01, aopt=2, itype=1, beta=2.0, nu=0.49, mu1=1, alpha1=2)
    assert len(m.fiber_families) == 0
    assert m.actype is None
    ref_mat295_string = ref_string.test_mat_295_iso
    assert m.write() == ref_mat295_string


@pytest.mark.keywords
def test_constrained_adaptivity(ref_string):
    ca = kwd.ConstrainedAdaptivity()
    ca.loads(ref_string.test_constrained_adaptivity)
    assert ca.constrains.shape == (5, 3)


@pytest.mark.keywords
def test_constrained_nodal_rigid_body_inertia_title(ref_string):
    c = kwd.ConstrainedNodalRigidBodyInertia()
    c.loads(ref_string.test_constrained_nodal_rigid_body_inertia_title)


@pytest.mark.keywords
def test_constrained_rigid_bodies(ref_string):
    crb = kwd.ConstrainedRigidBodies()
    crb.loads(ref_string.test_constrained_rigid_bodies)
    assert crb.pairs.shape == (2, 3)


@pytest.mark.keywords
def test_mat_piecewise_linear_plasticity_title(ref_string):
    m = kwd.MatPiecewiseLinearPlasticity()
    m.loads(ref_string.test_mat_piecewise_linear_plasticity_title)


@pytest.mark.keywords
def test_set_node_title(ref_string):
    s = kwd.SetNode()
    s.loads(ref_string.test_set_node_title)


@pytest.mark.keywords
def test_set_node_list(ref_string):
    s = kwd.SetNodeList()
    with pytest.warns(UserWarning, match="Detected out of bound card characters"):
        s.loads(ref_string.test_set_node_list)
    assert len(s.nodes) == 8


@pytest.mark.keywords
def test_contact_id_options(ref_string):
    c = kwd.ContactAutomaticSingleSurface()
    c.options["ID"].active = True
    c.options["MPP"].active = True
    assert c.write() == ref_string.test_contact_automatic_single_surface_id_mpp1_mpp2
    c.options["ID"].active = False
    c.mpp2 = False
    assert c.write() == ref_string.test_contact_automatic_single_surface_1d_mpp1


@pytest.mark.keywords
def test_parameter_expression(ref_string):
    p = kwd.ParameterExpression()
    p.loads(ref_string.test_parameter_expression_ref)
    assert len(p.parameters) == 3
    assert "PE_300" in p.parameters["prmr"][1]
    assert p.write() == ref_string.test_parameter_expression_ref


@pytest.mark.keywords
def test_define_transformation(ref_string):
    d = kwd.DefineTransformation()
    d.loads(ref_string.test_define_transformation_ref)
    assert d.tranid == 1
    assert len(d.transforms) == 3
    assert "TRANSL" in d.transforms["option"][1]
    assert 3 == d.transforms["a1"][2]
    assert d.write() == ref_string.test_define_transformation_ref


@pytest.mark.keywords
def test_contact_automatic_single_surface(ref_string):
    c = kwd.ContactAutomaticSingleSurface(ssid=1)
    assert c.write() == ref_string.test_contact_automatic_single_surface


@pytest.mark.keywords
def test_define_function(ref_string):
    ref_function_string = ref_string.test_define_function_string
    d = kwd.DefineFunction()
    d.function = """1,x-velo
x(t)=1000*sin(100*t)
*DEFINE_FUNCTION
2,z-velo
a(t)=x(t)+200"""
    function_string = d.write()
    assert function_string == ref_function_string


@pytest.mark.keywords
def test_set_shell_intersect(ref_string):
    s = kwd.SetShellIntersect()
    assert s.write() == ref_string.test_set_shell_intersect_ref_1
    s.options["TITLE"].active = True
    assert s.write() == ref_string.test_set_shell_intersect_ref_2
    s.title = "hello"
    assert s.write() == ref_string.test_set_shell_intersect_ref_3


@pytest.mark.keywords
def test_set_part_list(ref_string):
    """Test formatting of set part list (uses series card with ints)."""
    s = kwd.SetPartList()
    s.sid = 1
    s.parts = [1, 2, 3]
    ref = ref_string.test_set_part_list_ref
    assert s.write() == ref


# uncomment when duplicate card groups can be assigned in this way
#@pytest.mark.keywords
#def test_part_assign(ref_string):
#    """Test formatting of set part list (uses series card with ints)."""
#    part = kwd.Part(heading="My part", pid=1, secid=1, mid=1, eosid=0)
#    ref = ref_string.test_part_assign_ref
#    test = part.write()
#    assert test == ref

@pytest.mark.keywords
def test_element_beam_assign(ref_string):
    """Test formatting of set part list (uses series card with ints)."""
    beam = kwd.ElementBeam(pid=1, n1=1, n2=0, local=1)
    ref = ref_string.test_element_beam_assign_ref
    assert beam.write() == ref


@pytest.mark.keywords
def test_define_table(ref_string):
    """Test formatting of set part list (uses series card with ints)."""
    table = kwd.DefineTable()
    table.loads(ref_string.test_define_table_ref)
    assert len(table.points) == 5
    assert table.points[2] == 0.001
    assert table.tbid == 10000001


@pytest.mark.keywords
def test_icfd_part(ref_string):
    """Test formatting of set part list (uses series card with ints)."""
    part = kwd.IcfdPart(pid=1,secid=2,mid=3)
    part.options["TITLE"].active = True
    part.title = "PART TITLE"
    s = part.write()
    assert s == ref_string.test_icfd_part_ref


@pytest.mark.keywords
def test_set_part_list_generate(ref_string):
    s = kwd.SetPartListGenerate()
    s.loads(ref_string.test_set_part_list_generate_ref1)
    assert len(s.block_ranges) == 3
    assert s.block_ranges[1].bend == 2000200

    s.loads(ref_string.test_set_part_list_generate_ref2)
    assert len(s.block_ranges) == 5
    assert s.block_ranges[3].bbeg == 2000700

    s.block_ranges.append((10, 12))
    assert len(s.block_ranges) == 6
    assert s.block_ranges[5].bbeg == 10

    s.block_ranges.data = [(0, 2), [2, 3]]
    assert len(s.block_ranges) == 2
    assert s.block_ranges[0] == kwd.SetPartListGenerate.BlockRange(0, 2)
    with pytest.raises(TypeError):
        s.block_ranges.append((0, 3, 2))

    s.block_ranges.append([200])
    assert len(s.block_ranges) == 3
    with pytest.raises(IndexError):
        _ = s.block_ranges[4]
    assert s.block_ranges[2].bbeg == 200
    assert pd.isna(s.block_ranges[2].bend)

    s.block_ranges[2].bend = 201
    for block_range in s.block_ranges:
        assert block_range.bbeg < block_range.bend

    s.loads(ref_string.test_set_part_list_generate_ref3)
    assert len(s.block_ranges) == 3
    assert s.block_ranges[2].bbeg == 15010000


@pytest.mark.keywords
def test_contact_tied_shell_edge_to_surface_id(ref_string):
    """Test reading CONTACT_TIED_SHELL_EDGE_TO_SURFACE_ID."""
    s = kwd.ContactTiedShellEdgeToSurface()
    ref = ref_string.test_contact_tied_shell_edge_to_surface_id
    s.loads(ref)


@pytest.mark.keywords
def test_control_energy_no_hgen():
    """Test the three ways to remove the hgen field from *CONTROL_ENERGY"""
    with disable_lspp_defaults():
        c = kwd.ControlEnergy()
    assert c.hgen is None
    c = kwd.ControlEnergy(hgen=None)
    assert c.hgen is None
    c = kwd.ControlEnergy()
    c.hgen = None
    assert c.hgen is None


@pytest.mark.keywords
def test_em_control(ref_string):
    """Test formatting of EM control."""
    s = kwd.EmControl()
    ref = ref_string.test_em_control_string
    assert s.write() == ref


@pytest.mark.keywords
def test_format_symbol():
    """Test all permutations of the keyword format symbol given the deck format."""
    node = kwd.Node()

    def _get_write_format_and_format_symbol(format, deck_format):
        """Used in tests..."""
        return node._get_write_format(format, deck_format), node._get_symbol(
            format, deck_format
        )

    # default deck format
    assert _get_write_format_and_format_symbol(
        format_type.long, format_type.default
    ) == (format_type.long, "+")
    assert _get_write_format_and_format_symbol(
        format_type.standard, format_type.default
    ) == (
        format_type.standard,
        "-",
    )
    assert _get_write_format_and_format_symbol(
        format_type.default, format_type.default
    ) == (
        format_type.default,
        "",
    )

    # long deck format
    assert _get_write_format_and_format_symbol(format_type.long, format_type.long) == (
        format_type.long,
        "",
    )
    assert _get_write_format_and_format_symbol(
        format_type.standard, format_type.long
    ) == (
        format_type.standard,
        "-",
    )
    assert _get_write_format_and_format_symbol(
        format_type.default, format_type.long
    ) == (format_type.long, "")

    # standard deck format
    assert _get_write_format_and_format_symbol(
        format_type.long, format_type.standard
    ) == (format_type.long, "+")
    assert _get_write_format_and_format_symbol(
        format_type.standard, format_type.standard
    ) == (
        format_type.standard,
        "",
    )
    assert _get_write_format_and_format_symbol(
        format_type.default, format_type.standard
    ) == (
        format_type.standard,
        "",
    )


@pytest.mark.keywords
def test_em_isopotential_connect(ref_string):
    """Test default card of EM_ISOPOTENTIAL_CONNECT and conditional in case contype=6"""
    s = kwd.EmIsopotentialConnect()
    assert s.write() == ref_string.test_default_card_em_isopotential_connect_string
    s.contype = 6
    assert s.write() == ref_string.test_conditional_card_em_isopotential_connect_string


@pytest.mark.keywords
def test_contact_force_transducer_penalty(ref_string):
    c = kwd.ContactForceTransducerPenalty()
    c.loads(ref_string.test_contact_force_transducer_penalty)
    val = c.write()
    assert val == ref_string.test_contact_force_transducer_penalty
    c.options["ID"].active = True
    val = c.write()
    assert val == ref_string.test_contact_force_transducer_penalty_id


@pytest.mark.keywords
def test_contact_automatic_general_id_mpp(ref_string):
    c = kwd.ContactAutomaticGeneral()
    c.options["ID"].active = True
    c.options["MPP"].active = True
    val = c.write()
    assert val == ref_string.test_contact_automatic_general_id_mpp
    c = kwd.ContactAutomaticGeneral()
    c.loads(ref_string.test_contact_automatic_general_id_mpp)
    assert c.options["MPP"].active is True

    c = kwd.ContactAutomaticGeneral()
    c.loads(ref_string.test_contact_automatic_general_id_mpp1)
    assert c.options["MPP"].active is True
    assert c.mpp2 is False
    assert c.surfa == 11


@pytest.mark.keywords
def test_contact_tied_shell_edge_to_surface_beam_offset_opt_cards(ref_string):
    """Test to read optional contact cards"""
    # These are for the option A, B, C, D, E, F, G
    # These do not affect the title, and appear in order at the end of the keyword
    s1 = kwd.ContactTiedShellEdgeToSurfaceBeamOffset()
    s1.loads(ref_string.test_contact_tied_shell_edge_to_surface_beam_offset_opt_cards1)
    assert (
        s1.write()
        == ref_string.test_contact_tied_shell_edge_to_surface_beam_offset_opt_cards1
    )
    s2 = kwd.ContactTiedShellEdgeToSurfaceBeamOffset()
    s2.loads(ref_string.test_contact_tied_shell_edge_to_surface_beam_offset_opt_cards2)
    assert (
        s2.write()
        == ref_string.test_contact_tied_shell_edge_to_surface_beam_offset_opt_cards2
    )


@pytest.mark.keywords
def test_em_randles_batmac_rdltype(ref_string):
    s = kwd.EmRandlesBatmac(rdltype=1)
    assert s.write() == ref_string.test_em_randles_batmac_rdltype_0_1
    s = kwd.EmRandlesBatmac(rdltype=3)
    assert s.write() == ref_string.test_em_randles_batmac_rdltype_2_3


@pytest.mark.keywords
def test_multiline_include_keyword(ref_string):
    filename = "a" * 60 + ".k"
    i = kwd.Include(filename=filename)
    assert i.write() == ref_string.test_one_line_include1
    assert kwd.Include().loads(ref_string.test_one_line_include1).filename == filename
    filename = "a" * 78 + ".k"
    i.filename = filename
    assert i.write() == ref_string.test_one_line_include2
    assert kwd.Include().loads(ref_string.test_one_line_include2).filename == filename
    filename = "a" * 80 + ".k"
    i.filename = filename
    assert i.write() == ref_string.test_two_line_include1
    assert kwd.Include().loads(ref_string.test_two_line_include1).filename == filename
    filename = "a" * 156 + ".k"
    i.filename = filename
    assert i.write() == ref_string.test_two_line_include2
    assert kwd.Include().loads(ref_string.test_two_line_include2).filename == filename
    filename = "a" * 160 + ".k"
    i.filename = filename
    assert i.write() == ref_string.test_three_line_include1
    assert kwd.Include().loads(ref_string.test_three_line_include1).filename == filename
    filename = "a" * 234 + ".k"
    i.filename = filename
    assert i.write() == ref_string.test_three_line_include2
    assert kwd.Include().loads(ref_string.test_three_line_include2).filename == filename
    with pytest.raises(Exception):
        i.filename = "a" * 300 + ".k"
        i.write()


@pytest.mark.keywords
def test_em_randles_solid_rdltype(ref_string):
    s = kwd.EmRandlesSolid(rdltype=1)
    assert s.write() == ref_string.test_em_randles_solid_rdltype_0_1
    s = kwd.EmRandlesSolid(rdltype=3)
    assert s.write() == ref_string.test_em_randles_solid_rdltype_2_3


@pytest.mark.keywords
def test_em_randles_tshell_rdltype(ref_string):
    s = kwd.EmRandlesTshell(rdltype=1)
    assert s.write() == ref_string.test_em_randles_tshell_rdltype_0_1
    s = kwd.EmRandlesTshell(rdltype=3)
    assert s.write() == ref_string.test_em_randles_tshell_rdltype_2_3
