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
Tests for *AIRBAG_PARTICLE card structure.

The reference input is a CPM airbag deck. That keyword block has exactly
4 data lines (TSTOP/SFIAIR4 cards are absent because their values are all
defaults, which is legal in LS-DYNA):

    Card 1 → SID1, STYPE1, SID2, STYPE2, BLOCK, NPDATA, FRIC, IRDP
    Card 2 → NP, UNIT, VISFLG, TATM, PATM, NVENT, TEND, TSW
    Card 3 → IAIR, NGAS, NORIF, NID1, NID2, NID3, CHM, CD_EXT
    Card 4 → PAIR, TAIR, XMAIR, AAIR, BAIR, CAIR, NPAIR, NPRLX

Background
----------
AirbagParticle has optional TSTOP and SFIAIR4 cards (Cards 3–4 in the
card list). When those cards are absent from the input, the reader must
skip them and assign data lines to the correct subsequent cards (IAIR
and PAIR). These tests verify that card ordering and field assignment
behave correctly when optional cards are omitted.

To regenerate the keyword class after a manifest change, run:
    python codegen/generate.py -k AIRBAG_PARTICLE
"""

import pandas as pd
import pytest

from ansys.dyna.core import keywords as kwd


# ---------------------------------------------------------------------------
# Reference input — a minimal CPM airbag block (the simplest real-world case):
#   ngas=0, norif=0, nvent=0, npdata=0, unit=0 (no repeating rows,
#   no unit-conversion card).  Only 4 data lines are present; the
#   TSTOP and SFIAIR4 cards are intentionally absent (all-default).
# ---------------------------------------------------------------------------

_AIRBAG_CPM_BLOCK = """\
*AIRBAG_PARTICLE
$#    sid1    stype1      sid2    stype2     block    npdata      fric      irdp
         1         0         0         0         0         0       0.0         0
$#      np      unit    visflg      tatm      patm     nvent      tend       tsw
      2000         0         2     293.01.00000E-4         01.00000E101.00000E10
$#    iair      ngas     norif      nid1      nid2      nid3       chm    cd_ext
         2         0         0         0         0         0         0       0.0
$#    pair      tair     xmair      aair      bair      cair     npair     nprlx
1.00000E-4     293.0     0.035      26.77.00000E-4-1.0000E-6         00
"""


def _data_lines(output: str) -> list:
    """Return non-empty, non-comment, non-keyword lines from write() output."""
    return [
        line for line in output.splitlines()
        if line.strip()
        and not line.strip().startswith("*")
        and not line.strip().startswith("$")
    ]


# ---------------------------------------------------------------------------
# 1. Card ordering: data line 3 must be consumed by IAIR, not TSTOP.
#
#    When TSTOP and SFIAIR4 cards are absent from the input, the reader
#    must skip them so that data line 3 goes to the IAIR card and data
#    line 4 goes to the PAIR card.
# ---------------------------------------------------------------------------

def test_iair_reads_correctly_from_cpm_file():
    """iair must be 2 after reading the CPM file."""
    ab = kwd.AirbagParticle()
    ab.loads(_AIRBAG_CPM_BLOCK)
    assert ab.iair == 2, f"iair should be 2, got {ab.iair}"


def test_tstop_is_default_when_absent_from_input():
    """tstop must keep its default (1e11) when the TSTOP card is absent from the input."""
    ab = kwd.AirbagParticle()
    ab.loads(_AIRBAG_CPM_BLOCK)
    assert ab.tstop == pytest.approx(1e11), (
        f"tstop should be 1e11 (default, card absent from input), got {ab.tstop}"
    )


def test_pair_reads_correctly_from_cpm_file():
    """pair must be 1e-4 after reading the CPM file."""
    ab = kwd.AirbagParticle()
    ab.loads(_AIRBAG_CPM_BLOCK)
    assert ab.pair == pytest.approx(1.0e-4), f"pair should be 1e-4, got {ab.pair}"


def test_sfiair4_is_default_when_absent_from_input():
    """sfiair4 must keep its default (1.0) when the SFIAIR4 card is absent from the input."""
    ab = kwd.AirbagParticle()
    ab.loads(_AIRBAG_CPM_BLOCK)
    assert ab.sfiair4 == pytest.approx(1.0), (
        f"sfiair4 should be 1.0 (default, card absent from input), got {ab.sfiair4}"
    )


# ---------------------------------------------------------------------------
# 2. All fields from the CPM file read with correct values
# ---------------------------------------------------------------------------

def test_cpm_card1_fields():
    """Card 1 scalar fields must match the CPM file values."""
    ab = kwd.AirbagParticle()
    ab.loads(_AIRBAG_CPM_BLOCK)
    assert ab.sid1 == 1
    assert ab.stype1 == 0
    assert ab.sid2 == 0
    assert ab.stype2 == 0
    assert ab.block == 0
    assert ab.npdata == 0
    assert ab.fric == pytest.approx(0.0)
    assert ab.irdp == 0


def test_cpm_card2_fields():
    """Card 2 (NP/UNIT/VISFLG...) fields must match the CPM file values."""
    ab = kwd.AirbagParticle()
    ab.loads(_AIRBAG_CPM_BLOCK)
    assert ab.np == 2000
    assert ab.unit == 0
    assert ab.visflg == 2
    assert ab.tatm == pytest.approx(293.0)
    assert ab.patm == pytest.approx(1.0e-4)
    assert ab.nvent == 0
    assert ab.tend == pytest.approx(1.0e10)
    assert ab.tsw == pytest.approx(1.0e10)


def test_cpm_iair_card_fields():
    """IAIR card fields (data line 3 in the CPM file) must read correctly."""
    ab = kwd.AirbagParticle()
    ab.loads(_AIRBAG_CPM_BLOCK)
    assert ab.iair == 2
    assert ab.ngas == 0
    assert ab.norif == 0
    assert ab.nid1 == 0
    assert ab.nid2 == 0
    assert ab.nid3 == 0
    assert ab.chm == 0
    assert ab.cd_ext == pytest.approx(0.0)


def test_cpm_pair_card_fields():
    """PAIR card fields (data line 4 in the CPM file) must read correctly."""
    ab = kwd.AirbagParticle()
    ab.loads(_AIRBAG_CPM_BLOCK)
    assert ab.pair == pytest.approx(1.0e-4)
    assert ab.tair == pytest.approx(293.0)
    assert ab.xmair == pytest.approx(0.035)
    assert ab.aair == pytest.approx(26.7)
    assert ab.bair == pytest.approx(7.0e-4)
    assert ab.cair == pytest.approx(-1.0e-6)
    assert ab.npair == 0
    assert ab.nprlx == "0"


# ---------------------------------------------------------------------------
# 3. Table properties must exist and be empty (ngas=norif=nvent=npdata=0)
# ---------------------------------------------------------------------------

def test_cpm_table_properties_exist():
    """The four table properties must exist after the manifest fix."""
    ab = kwd.AirbagParticle()
    ab.loads(_AIRBAG_CPM_BLOCK)
    for prop in ("gas_components", "orifices", "vents", "parts_data"):
        assert hasattr(ab, prop), (
            f"{prop} property missing — manifest fix not applied or codegen not re-run"
        )
        assert isinstance(getattr(ab, prop), pd.DataFrame)


def test_cpm_table_properties_are_empty():
    """All table properties must be empty DataFrames (ngas=norif=nvent=npdata=0)."""
    ab = kwd.AirbagParticle()
    ab.loads(_AIRBAG_CPM_BLOCK)
    assert len(ab.gas_components) == 0, f"Expected 0 gas rows, got {len(ab.gas_components)}"
    assert len(ab.orifices) == 0, f"Expected 0 orifice rows, got {len(ab.orifices)}"
    assert len(ab.vents) == 0, f"Expected 0 vent rows, got {len(ab.vents)}"
    assert len(ab.parts_data) == 0, f"Expected 0 parts_data rows, got {len(ab.parts_data)}"


# ---------------------------------------------------------------------------
# 4. Unit-conversion card absent (unit=0)
#
#    The unit-conversion card has active_func=lambda: self.unit == 3, so it
#    is suppressed when unit=0.
# ---------------------------------------------------------------------------

def test_unit_conversion_card_absent_for_unit_0():
    """Unit-conversion card must NOT appear in write() output when unit=0."""
    ab = kwd.AirbagParticle()
    ab.loads(_AIRBAG_CPM_BLOCK)
    output = ab.write()
    # The unit-conversion card writes the mass/time/length values explicitly.
    # When unit=0 that card is suppressed; none of its field values appear.
    data_lines = _data_lines(output)
    # mass=1.0 / time=0.001 / length=0.001 are the sentinel values used in the
    # unit=3 variant below — they must not appear in the unit=0 output.
    assert not any("     1.0" in line and "   0.001" in line for line in data_lines), (
        "Unit-conversion card values found in unit=0 output; the active_func guard may be missing."
    )

    # Complementary: setting unit=3 must add exactly one extra data line that
    # contains the mass/time/length values.
    ab3 = kwd.AirbagParticle()
    ab3.loads(_AIRBAG_CPM_BLOCK)
    ab3.unit = 3
    ab3.mass = 1.0
    ab3.time = 0.001
    ab3.length = 0.001
    output3 = ab3.write()
    lines_unit0 = len(data_lines)
    lines_unit3 = len(_data_lines(output3))
    assert lines_unit3 == lines_unit0 + 1, (
        f"UNIT=3 should produce exactly 1 more data line than UNIT=0. "
        f"Got UNIT=0: {lines_unit0}, UNIT=3: {lines_unit3}."
    )
    assert any("     1.0" in line or "   0.001" in line for line in _data_lines(output3)), (
        "Unit-conversion card line not found in unit=3 output."
    )


# ---------------------------------------------------------------------------
# 5. Round-trip: write() then loads() reproduces identical field values
# ---------------------------------------------------------------------------

def test_cpm_round_trip_scalar_fields():
    """Write then re-read must reproduce all scalar field values exactly."""
    ab = kwd.AirbagParticle()
    ab.loads(_AIRBAG_CPM_BLOCK)
    output = ab.write()

    ab2 = kwd.AirbagParticle()
    ab2.loads(output)

    assert ab2.sid1 == 1
    assert ab2.np == 2000
    assert ab2.unit == 0
    assert ab2.visflg == 2
    assert ab2.tatm == pytest.approx(293.0)
    assert ab2.patm == pytest.approx(1.0e-4)
    assert ab2.nvent == 0
    assert ab2.iair == 2
    assert ab2.ngas == 0
    assert ab2.norif == 0
    assert ab2.pair == pytest.approx(1.0e-4)
    assert ab2.tair == pytest.approx(293.0)
    assert ab2.xmair == pytest.approx(0.035)
    assert ab2.aair == pytest.approx(26.7)
    assert ab2.bair == pytest.approx(7.0e-4)
    assert ab2.cair == pytest.approx(-1.0e-6)
    assert ab2.npair == 0
    assert ab2.nprlx == "0"


# ---------------------------------------------------------------------------
# 6. Multi-row cases (not in the CPM file but critical for the fix)
#    These verify the repeating-card logic is correct for ngas>0 / norif>0.
# ---------------------------------------------------------------------------

def test_two_gas_components_round_trip():
    """NGAS=2 must produce and read back two gas-component rows."""
    ab = kwd.AirbagParticle()
    ab.loads(_AIRBAG_CPM_BLOCK)
    ab.ngas = 2
    ab.gas_components = pd.DataFrame({
        "lcmi": [101, 102], "lcti": [201, 202],
        "xmi": [28.0, 32.0], "ai": [1000.0, 900.0],
        "bi": [0.0, 0.0], "ci": [0.0, 0.0], "infgi": [1, 1],
    })
    output = ab.write()
    ab2 = kwd.AirbagParticle()
    ab2.loads(output)

    assert ab2.ngas == 2, f"Expected ngas=2, got {ab2.ngas}"
    assert len(ab2.gas_components) == 2, (
        f"Expected 2 gas rows, got {len(ab2.gas_components)}"
    )
    assert ab2.gas_components["lcmi"].tolist() == [101, 102]
    assert ab2.gas_components["xmi"].tolist() == [28.0, 32.0]


def test_two_orifices_round_trip():
    """NORIF=2 must produce and read back two orifice rows."""
    ab = kwd.AirbagParticle()
    ab.loads(_AIRBAG_CPM_BLOCK)
    ab.norif = 2
    ab.orifices = pd.DataFrame({
        "nidi": [1001, 1002], "ani": [50.0, 60.0],
        "vdi": [1, 2], "cai": [30.0, 30.0],
        "infoi": [1, 1], "imom": [0, 0], "iang": [0, 0], "chm_id": [None, None],
    })
    output = ab.write()
    ab2 = kwd.AirbagParticle()
    ab2.loads(output)

    assert ab2.norif == 2, f"Expected norif=2, got {ab2.norif}"
    assert len(ab2.orifices) == 2, (
        f"Expected 2 orifice rows, got {len(ab2.orifices)}"
    )
    assert ab2.orifices["nidi"].tolist() == [1001, 1002]
    assert ab2.orifices["ani"].tolist() == [50.0, 60.0]


def test_ngas_drives_output_line_count():
    """Each additional gas component must add exactly one data line to write()."""
    def make_ab(ngas):
        ab = kwd.AirbagParticle()
        ab.loads(_AIRBAG_CPM_BLOCK)
        ab.ngas = ngas
        if ngas > 0:
            ab.gas_components = pd.DataFrame({
                "lcmi": list(range(100, 100 + ngas)),
                "lcti": list(range(200, 200 + ngas)),
                "xmi": [28.0] * ngas, "ai": [1000.0] * ngas,
                "bi": [0.0] * ngas, "ci": [0.0] * ngas, "infgi": [1] * ngas,
            })
        return ab

    n0 = len(_data_lines(make_ab(0).write()))
    n1 = len(_data_lines(make_ab(1).write()))
    n2 = len(_data_lines(make_ab(2).write()))

    assert n1 == n0 + 1, f"ngas=1 should add 1 line vs ngas=0. Got {n0} vs {n1}."
    assert n2 == n0 + 2, f"ngas=2 should add 2 lines vs ngas=0. Got {n0} vs {n2}."


def test_two_vents_round_trip():
    """NVENT=2 must produce and read back two vent rows."""
    ab = kwd.AirbagParticle()
    ab.loads(_AIRBAG_CPM_BLOCK)
    ab.nvent = 2
    ab.vents = pd.DataFrame({
        "sid3": [501, 502],
        "stype3": [0, 0],
        "c23": [0.5, 0.6],
        "lctc23": [0, 0],
        "lcpc23": [0, 0],
        "enh_v": [0, 0],
        "ppop": [0.0, 0.0],
    })
    output = ab.write()
    ab2 = kwd.AirbagParticle()
    ab2.loads(output)

    assert ab2.nvent == 2, f"Expected nvent=2, got {ab2.nvent}"
    assert len(ab2.vents) == 2, (
        f"Expected 2 vent rows, got {len(ab2.vents)}"
    )
    assert ab2.vents["sid3"].tolist() == [501, 502]
    assert ab2.vents["c23"].tolist() == [0.5, 0.6]

