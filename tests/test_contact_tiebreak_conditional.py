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

"""Tests for tiebreak contact cards 4.1b/4.2b conditional on OPTION=13 or 14.

"""
import pytest

from ansys.dyna.core.keywords.keyword_classes.auto.contact.contact_automatic_general_tiebreak import (
    ContactAutomaticGeneralTiebreak,
)
from ansys.dyna.core.keywords.keyword_classes.auto.contact.contact_automatic_general_tiebreak_beam_offset import (
    ContactAutomaticGeneralTiebreakBeamOffset,
)
from ansys.dyna.core.keywords.keyword_classes.auto.contact.contact_automatic_one_way_surface_to_surface_tiebreak import (
    ContactAutomaticOneWaySurfaceToSurfaceTiebreak,
)
from ansys.dyna.core.keywords.keyword_classes.auto.contact.contact_automatic_one_way_surface_to_surface_tiebreak_damping import (
    ContactAutomaticOneWaySurfaceToSurfaceTiebreakDamping,
)
from ansys.dyna.core.keywords.keyword_classes.auto.contact.contact_automatic_single_surface_tiebreak import (
    ContactAutomaticSingleSurfaceTiebreak,
)
from ansys.dyna.core.keywords.keyword_classes.auto.contact.contact_automatic_single_surface_tiebreak_beam_offset import (
    ContactAutomaticSingleSurfaceTiebreakBeamOffset,
)
from ansys.dyna.core.keywords.keyword_classes.auto.contact.contact_automatic_surface_to_surface_tiebreak import (
    ContactAutomaticSurfaceToSurfaceTiebreak,
)
from ansys.dyna.core.keywords.keyword_classes.auto.contact.contact_automatic_surface_to_surface_tiebreak_damping import (
    ContactAutomaticSurfaceToSurfaceTiebreakDamping,
)


TIEBREAK_CLASSES = [
    ContactAutomaticSurfaceToSurfaceTiebreak,
    ContactAutomaticOneWaySurfaceToSurfaceTiebreak,
    ContactAutomaticSingleSurfaceTiebreak,
    ContactAutomaticSingleSurfaceTiebreakBeamOffset,
    ContactAutomaticGeneralTiebreak,
    ContactAutomaticGeneralTiebreakBeamOffset,
    ContactAutomaticSurfaceToSurfaceTiebreakDamping,
    ContactAutomaticOneWaySurfaceToSurfaceTiebreakDamping,
]


@pytest.mark.parametrize("cls", TIEBREAK_CLASSES, ids=lambda c: c.__name__)
@pytest.mark.parametrize("opt_value", [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11])
def test_g1c_g2c_cards_inactive_when_option_not_13_14(cls, opt_value):
    """Cards 4.1b (G1C) and 4.2b (G2C) must NOT appear when OPTION != 13 or 14."""
    card = cls(option=opt_value)
    output = card.write()
    assert "g1c_0" not in output, (
        f"{cls.__name__} with option={opt_value}: G1C card should not be written"
    )
    assert "g2c_0" not in output, (
        f"{cls.__name__} with option={opt_value}: G2C card should not be written"
    )


@pytest.mark.parametrize("cls", TIEBREAK_CLASSES, ids=lambda c: c.__name__)
@pytest.mark.parametrize("opt_value", [13, 14])
def test_g1c_g2c_cards_active_when_option_13_or_14(cls, opt_value):
    """Cards 4.1b (G1C) and 4.2b (G2C) MUST appear when OPTION == 13 or 14."""
    card = cls(option=opt_value)
    output = card.write()
    assert "g1c_0" in output, (
        f"{cls.__name__} with option={opt_value}: G1C card should be written"
    )
    assert "g2c_0" in output, (
        f"{cls.__name__} with option={opt_value}: G2C card should be written"
    )


def test_issue_1178_repro():
    """Exact reproduction from the issue report: option=9 should not have G1C/G2C cards."""
    card = ContactAutomaticSurfaceToSurfaceTiebreak(option=9)
    output = str(card)
    lines = output.strip().split("\n")

    # The OPTION card line with value 9 should be present
    assert any("9" in line and "option" not in line.lower() for line in lines), (
        "The OPTION value 9 should be present in the output"
    )

    # G1C/G2C comment lines should NOT be present
    assert "g1c_0" not in output, "G1C card should not appear when option=9"
    assert "g2c_0" not in output, "G2C card should not appear when option=9"
    assert "edot_g1" not in output, "G1C fields should not appear when option=9"
    assert "edot_g2" not in output, "G2C fields should not appear when option=9"


def test_issue_1178_option_13_has_g1c_g2c():
    """With option=13, G1C and G2C cards must be present."""
    card = ContactAutomaticSurfaceToSurfaceTiebreak(option=13)
    output = str(card)

    assert "g1c_0" in output, "G1C card should appear when option=13"
    assert "g2c_0" in output, "G2C card should appear when option=13"
    assert "edot_g1" in output, "G1C fields should appear when option=13"
    assert "edot_g2" in output, "G2C fields should appear when option=13"
