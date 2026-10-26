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

"""Optional ID, TITLE, and joint cards are not part of the plain keyword layout.

Plain keyword blocks keep their first data card. Suffixes such as _ID, _TITLE,
_LOCAL, and _FAILURE select the optional cards described in the keyword manual.
"""

from ansys.dyna.core import Deck
from ansys.dyna.core import keywords as kwd
from ansys.dyna.core.lib.option_card import OptionCardSet

import pytest

# Field names that belong on an optional ID/TITLE card, not on card 1.
_OPTIONAL_FIRST_FIELDS = {"wid", "coupid", "id", "did", "bagid"}

# These keywords have a real TITLE card in addition to the ID card.
_SEPARATE_TITLE_OPTION = {
    "DefineAdaptiveSolidToDes",
    "DefineAdaptiveSolidToSph",
    "DefineSphDeCoupling",
}

# One class from each card-count group, plus the keywords whose option card
# comes from that keyword's own definition.
_OPTION_CASES = [
    ("ConstrainedGeneralizedWeldButt", ("ID",)),
    ("ConstrainedGeneralizedWeldCombined", ("ID",)),
    ("ConstrainedGeneralizedWeldCrossFillet", ("ID",)),
    ("ConstrainedGeneralizedWeldFillet", ("ID",)),
    ("ConstrainedGeneralizedWeldSpot", ("ID",)),
    ("ConstrainedSpotweld", ("ID",)),
    ("ConstrainedSpotweldFilteredForce", ("ID",)),
    ("ConstrainedJointRevolute", ("LOCAL", "ID", "FAILURE")),
    ("ConstrainedJointHarmonic", ("LOCAL", "ID", "FAILURE")),
    ("ConstrainedJointGears", ("LOCAL", "ID", "FAILURE")),
    ("ConstrainedJointCoorRevolute", ("LOCAL", "ID", "FAILURE")),
    ("ConstrainedJointCoorHarmonic", ("LOCAL", "ID", "FAILURE")),
    ("ConstrainedLagrangeInSolid", ("TITLE",)),
    ("ConstrainedLagrangeInSolidEdge", ("TITLE",)),
    ("ConstrainedLagrangeInSolidEdges", ("TITLE",)),
    ("ConstrainedLocal", ("ID",)),
    ("ConstrainedBeamInSolidPenalty", ("ID",)),
    ("ConstrainedShellInSolid", ("ID",)),
    ("ConstrainedShellInSolidPenalty", ("ID",)),
    ("ConstrainedSolidInSolid", ("ID",)),
    ("ConstrainedSolidInSolidPenalty", ("ID",)),
    ("AleCouplingNodalDrag", ("ID",)),
    ("AleCouplingNodalPenalty", ("ID",)),
    ("AleFailSwitchMmg", ("ID",)),
    ("AleFsiSwitchMmg", ("ID",)),
    ("AleStructuredFsi", ("TITLE",)),
    ("AleStructuredFsiAbext", ("TITLE",)),
    ("AleStructuredFsiAbint", ("TITLE",)),
    ("InterfaceComponentNode", ("TITLE",)),
    ("InterfaceComponentSegment", ("TITLE",)),
    ("InterfaceComponentSph", ("TITLE",)),
    ("ContactGuidedCable", ("ID",)),
    ("ContactGuidedCableSet", ("ID",)),
    # DEFINE keywords also keep the generic TITLE option from the DEFINE wildcard.
    ("DefineAdaptiveSolidToDes", ("TITLE", "ID")),
    ("DefineAdaptiveSolidToSph", ("TITLE", "ID")),
    ("DefineSphDeCoupling", ("TITLE", "ID")),
    ("LoadAleConvection", ("ID",)),
    ("LoadNurbsShell", ("ID",)),
    ("LoadSegmentNonuniform", ("ID",)),
    ("LoadSegmentSetNonuniform", ("ID",)),
    ("LoadShellElement", ("ID",)),
    ("LoadShellSet", ("ID",)),
    ("AirbagParticle", ("ID",)),
    ("AirbagParticleDecomposition", ("ID",)),
    ("AirbagParticleJet", ("ID",)),
    ("AirbagParticleJetSegment", ("ID",)),
    ("AirbagParticleJetSegmentTime", ("ID",)),
    ("AirbagParticleMpp", ("ID",)),
    ("AirbagParticleMppInflation", ("ID",)),
    ("AirbagParticleMppJet", ("ID",)),
    ("AirbagParticleMppJetSegment", ("ID",)),
    ("AirbagParticleMppJetSegmentTime", ("ID",)),
]


def _load_one(text):
    deck = Deck()
    deck.loads(text)
    assert deck.string_keywords == []
    assert len(deck.keywords) == 1
    return deck.keywords[0]


def _comment_line(text, field_name):
    for line in text.splitlines():
        if not line.startswith("$"):
            continue
        tokens = line.lower().replace("#", " ").split()
        if field_name in tokens:
            return line
    raise AssertionError(f"{field_name} comment not found in:\n{text}")


@pytest.mark.parametrize("class_name,option_names", _OPTION_CASES)
def test_plain_keyword_does_not_lead_with_the_optional_card(class_name, option_names):
    """The optional card is an option, so a plain title writes the real first card."""
    obj = getattr(kwd, class_name)()
    for card in obj._cards:
        if isinstance(card, OptionCardSet) or not hasattr(card, "_fields"):
            continue
        assert card._fields[0].name not in _OPTIONAL_FIRST_FIELDS
    specs = list(obj.option_specs)
    assert tuple(spec.name for spec in specs) == option_names
    for spec in specs:
        if spec.name == "ID" and class_name not in _SEPARATE_TITLE_OPTION:
            if spec.aliases:
                assert spec.aliases == ("TITLE",)
                assert obj.options["TITLE"].active is False
            else:
                assert spec.aliases == ()
        else:
            assert spec.aliases == ()
    assert obj.get_title() == f"*{obj.keyword}_{obj.subkeyword}"
    for name in option_names:
        assert obj.options[name].active is False


def test_title_suffix_is_not_an_id_alias_unless_declared():
    """A keyword that documents only ID does not treat _TITLE as that card."""
    loaded = _load_one(
        """\
*CONSTRAINED_GENERALIZED_WELD_SPOT_TITLE
         4
1.0000E+20       0.5
"""
    )
    assert type(loaded) is kwd.ConstrainedGeneralizedWeldSpot
    assert loaded.is_option_active("ID") is False
    assert loaded.is_option_active("TITLE") is False
    assert loaded.nsid == 4
    assert loaded.wid is None
    assert loaded.get_title() == "*CONSTRAINED_GENERALIZED_WELD_SPOT"


def test_generalized_weld_spot_id_card_is_optional():
    """*CONSTRAINED_GENERALIZED_WELD_SPOT reads NSID from the first data line."""
    plain = _load_one(
        """\
*CONSTRAINED_GENERALIZED_WELD_SPOT
         4
1.0000E+20       0.5
"""
    )
    assert type(plain) is kwd.ConstrainedGeneralizedWeldSpot
    assert plain.nsid == 4
    assert plain.tfail == pytest.approx(1e20)
    assert plain.epsf == pytest.approx(0.5)
    assert plain.wid is None
    assert plain.options["ID"].active is False

    written = plain.write()
    assert written.splitlines()[0] == "*CONSTRAINED_GENERALIZED_WELD_SPOT"
    assert "nsid" in written.splitlines()[1].lower()

    plain.options["ID"].active = True
    plain.wid = 77
    written = plain.write()
    assert written.splitlines()[0] == "*CONSTRAINED_GENERALIZED_WELD_SPOT_ID"
    assert _comment_line(written, "wid")

    loaded = _load_one(written)
    assert loaded.wid == 77
    assert loaded.nsid == 4
    assert loaded.epsf == pytest.approx(0.5)
    assert loaded.options["ID"].active is True


def test_spotweld_id_and_filtered_force_id():
    """ID is optional on both spotweld forms, and the filter card stays with FILTERED_FORCE."""
    plain = _load_one(
        """\
*CONSTRAINED_SPOTWELD
       101       102     100.0     200.0
"""
    )
    assert plain.n1 == 101
    assert plain.n2 == 102
    assert plain.sn == pytest.approx(100.0)
    assert plain.ss == pytest.approx(200.0)
    assert plain.wid is None

    filtered = _load_one(
        """\
*CONSTRAINED_SPOTWELD_FILTERED_FORCE_ID
        77
       101       102     100.0     200.0
         3       0.1
"""
    )
    assert type(filtered) is kwd.ConstrainedSpotweldFilteredForce
    assert filtered.wid == 77
    assert filtered.n1 == 101
    assert filtered.sn == pytest.approx(100.0)
    assert filtered.nf == 3
    assert filtered.tw == pytest.approx(0.1)
    assert filtered.options["ID"].active is True


def test_lagrange_in_solid_title_card_is_optional():
    """The current manual defines TITLE for this keyword, not a separate ID option."""
    plain = _load_one(
        """\
*CONSTRAINED_LAGRANGE_IN_SOLID
         5         6
"""
    )
    assert plain.lstrsid == 5
    assert plain.alesid == 6
    assert plain.coupid is None
    assert plain.options["TITLE"].active is False
    assert not any(spec.name == "ID" for spec in plain.option_specs)

    titled = _load_one(
        """\
*CONSTRAINED_LAGRANGE_IN_SOLID_TITLE
         9heading
         5         6
"""
    )
    assert type(titled) is kwd.ConstrainedLagrangeInSolid
    assert titled.coupid == 9
    assert titled.title.strip().startswith("heading")
    assert titled.lstrsid == 5
    assert titled.alesid == 6
    assert titled.options["TITLE"].active is True


def test_constrained_local_id_card_is_optional():
    """*CONSTRAINED_LOCAL_ID carries the ID and heading card before the local constraint."""
    plain = _load_one(
        """\
*CONSTRAINED_LOCAL
         1
"""
    )
    assert plain.tc == 1
    assert plain.id is None

    loaded = _load_one(
        """\
*CONSTRAINED_LOCAL_ID
         9my local
         1
"""
    )
    assert loaded.id == 9
    assert loaded.heading.strip().startswith("my local")
    assert loaded.tc == 1
    assert loaded.options["ID"].active is True


def test_coupling_id_and_title_are_mutually_exclusive():
    """ID and TITLE share one card and the same title position, as on beam-in-solid."""
    plain = _load_one(
        """\
*CONSTRAINED_SHELL_IN_SOLID
         3
"""
    )
    assert type(plain) is kwd.ConstrainedShellInSolid
    assert plain.shsid == 3
    assert plain.coupid is None
    assert plain.options["ID"].active is False
    assert plain.options["TITLE"].active is False

    loaded = _load_one(
        """\
*CONSTRAINED_SHELL_IN_SOLID_ID
         8shell id
         3
"""
    )
    assert loaded.coupid == 8
    assert loaded.title.strip().startswith("shell id")
    assert loaded.shsid == 3
    assert loaded.options["ID"].active is True
    assert loaded.options["TITLE"].active is False

    loaded.options["TITLE"].active = True
    assert loaded.options["ID"].active is False
    written = loaded.write()
    assert written.splitlines()[0] == "*CONSTRAINED_SHELL_IN_SOLID_TITLE"
    assert loaded.coupid == 8


def test_id_and_title_are_spellings_of_one_card():
    """The file's suffix is kept, and choosing the other spelling does not add a card."""
    titled = _load_one(
        """\
*CONSTRAINED_SHELL_IN_SOLID_TITLE
         8shell id
         3
"""
    )
    assert titled.options["TITLE"].active is True
    assert titled.options["ID"].active is False
    assert titled.get_title() == "*CONSTRAINED_SHELL_IN_SOLID_TITLE"
    titled.coupid = 9
    titled.title = "renamed"
    assert titled.get_title() == "*CONSTRAINED_SHELL_IN_SOLID_TITLE"
    assert titled.coupid == 9
    assert titled.title.strip().startswith("renamed")

    titled.options["ID"].active = True
    assert titled.options["TITLE"].active is False
    assert titled.get_title() == "*CONSTRAINED_SHELL_IN_SOLID_ID"
    assert titled.coupid == 9
    titled.options["TITLE"].active = False
    assert titled.get_title() == "*CONSTRAINED_SHELL_IN_SOLID"
    assert "coupid" not in titled.write().lower()

    last_spelling = _load_one(
        """\
*CONSTRAINED_SHELL_IN_SOLID_ID_TITLE
         8shell id
         3
"""
    )
    assert last_spelling.options["TITLE"].active is True
    assert last_spelling.options["ID"].active is False
    assert last_spelling.coupid == 8
    assert last_spelling.shsid == 3


def test_beam_in_solid_penalty_id_card_is_optional():
    """Penalty uses the same optional COUPID card as the beam-in-solid keyword."""
    beam = kwd.ConstrainedBeamInSolidPenalty()
    beam.bsid = 3
    written = beam.write()
    assert written.splitlines()[0] == "*CONSTRAINED_BEAM_IN_SOLID_PENALTY"
    assert "bsid" in written.splitlines()[1].lower()
    assert "coupid" not in written.lower()

    beam.options["ID"].active = True
    beam.coupid = 12
    written = beam.write()
    assert written.splitlines()[0] == "*CONSTRAINED_BEAM_IN_SOLID_PENALTY_ID"
    assert "coupid" in written.lower()


def test_airbag_particle_jet_title_uses_the_option_on_the_plain_class():
    """_TITLE has no dedicated class. _ID still selects the existing dedicated class."""
    plain = _load_one(
        """\
*AIRBAG_PARTICLE_JET
       101
"""
    )
    assert type(plain) is kwd.AirbagParticleJet
    assert plain.sid1 == 101
    assert plain.id is None

    titled = _load_one(
        """\
*AIRBAG_PARTICLE_JET_TITLE
        55heading
       101
"""
    )
    assert type(titled) is kwd.AirbagParticleJet
    assert titled.id == 55
    assert titled.title.strip().startswith("heading")
    assert titled.sid1 == 101
    assert titled.options["TITLE"].active is True

    dedicated = _load_one(
        """\
*AIRBAG_PARTICLE_JET_ID
        55
       101
"""
    )
    assert type(dedicated) is kwd.AirbagParticleJetId
    assert dedicated.id == 55
    assert dedicated.sid1 == 101


def test_airbag_particle_mpp_title_card_follows_the_mpp_card():
    """On MPP airbags the ID/TITLE card comes after SX, SY, SZ."""
    plain = _load_one(
        """\
*AIRBAG_PARTICLE_MPP
       1.0       2.0       3.0
       101
"""
    )
    assert plain.sx == pytest.approx(1.0)
    assert plain.sy == pytest.approx(2.0)
    assert plain.sz == pytest.approx(3.0)
    assert plain.sid1 == 101
    assert plain.id is None

    titled = _load_one(
        """\
*AIRBAG_PARTICLE_MPP_TITLE
       1.0       2.0       3.0
        55heading
       101
"""
    )
    assert type(titled) is kwd.AirbagParticleMpp
    assert titled.sx == pytest.approx(1.0)
    assert titled.id == 55
    assert titled.title.strip().startswith("heading")
    assert titled.sid1 == 101
    assert titled.options["TITLE"].active is True


def test_revolute_joint_id_local_and_failure_options():
    """Joint suffixes select cards that are absent from the plain keyword."""
    plain = kwd.ConstrainedJointRevolute()
    plain.n1 = 101
    written = plain.write()
    assert written.splitlines()[0] == "*CONSTRAINED_JOINT_REVOLUTE"
    assert "jid" not in written.lower()

    identified = _load_one(
        """\
*CONSTRAINED_JOINT_REVOLUTE_ID
        10
       101       201
"""
    )
    assert identified.jid == 10
    assert identified.n1 == 101
    assert identified.n2 == 201
    assert not (identified.heading or "").strip()

    named = _load_one(
        """\
*CONSTRAINED_JOINT_REVOLUTE_ID
        10hinge
       101       201
"""
    )
    assert type(named) is kwd.ConstrainedJointRevolute
    assert named.jid == 10
    assert named.heading.strip().startswith("hinge")
    assert named.n1 == 101

    local = _load_one(
        """\
*CONSTRAINED_JOINT_REVOLUTE_LOCAL
       101       201
        12         1
"""
    )
    assert local.n1 == 101
    assert local.raid == 12
    assert local.lst == 1
    assert local.get_title() == "*CONSTRAINED_JOINT_REVOLUTE_LOCAL"

    failed = _load_one(
        """\
*CONSTRAINED_JOINT_REVOLUTE_FAILURE
       101       201
         4       1.5       0.2
       9.0       8.0       7.0       6.0       5.0       4.0
"""
    )
    assert failed.n1 == 101
    assert failed.cid == 4
    assert failed.tfail == pytest.approx(1.5)
    assert failed.coupl == pytest.approx(0.2)
    assert failed.nxx == pytest.approx(9.0)
    assert failed.nyy == pytest.approx(8.0)
    assert failed.nzz == pytest.approx(7.0)
    assert failed.mxx == pytest.approx(6.0)
    assert failed.myy == pytest.approx(5.0)
    assert failed.mzz == pytest.approx(4.0)

    joint = kwd.ConstrainedJointRevolute()
    joint.n1 = 101
    joint.n2 = 201
    joint.jid = 10
    joint.heading = "hinge"
    joint.raid = 12
    joint.lst = 1
    joint.cid = 4
    joint.tfail = 1.5
    joint.coupl = 0.2
    joint.nxx = 9.0
    joint.options["LOCAL"].active = True
    joint.options["ID"].active = True
    joint.options["FAILURE"].active = True
    written = joint.write()
    assert written.splitlines()[0] == "*CONSTRAINED_JOINT_REVOLUTE_LOCAL_ID_FAILURE"
    text_lines = written.splitlines()
    assert text_lines.index(_comment_line(written, "jid")) < text_lines.index(_comment_line(written, "n1"))
    assert text_lines.index(_comment_line(written, "n1")) < text_lines.index(_comment_line(written, "raid"))
    assert text_lines.index(_comment_line(written, "raid")) < text_lines.index(_comment_line(written, "nxx"))

    reloaded = _load_one(written)
    assert reloaded.jid == 10
    assert reloaded.heading.strip().startswith("hinge")
    assert reloaded.n1 == 101
    assert reloaded.raid == 12
    assert reloaded.lst == 1
    assert reloaded.cid == 4
    assert reloaded.nxx == pytest.approx(9.0)
    assert reloaded.options["LOCAL"].active is True
    assert reloaded.options["ID"].active is True
    assert reloaded.options["FAILURE"].active is True


def test_coor_revolute_joint_id_is_not_read_as_the_rigid_body_id():
    """Coordinate joints use the same ID card ahead of the coordinate cards."""
    loaded = _load_one(
        """\
*CONSTRAINED_JOINT_COOR_REVOLUTE_ID
        10
         8         9
"""
    )
    assert type(loaded) is kwd.ConstrainedJointCoorRevolute
    assert loaded.jid == 10
    assert loaded.rbid_a == 8
    assert loaded.rbid_b == 9
