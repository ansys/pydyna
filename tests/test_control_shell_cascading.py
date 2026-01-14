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

"""Tests for CONTROL_SHELL cascading optional cards.

*CONTROL_SHELL has 5 cards total:
- Card 0: Always written (required)
- Cards 1-4: Optional cascading cards - written only if any field on that card
  or a subsequent card has been explicitly set.

This follows the LS-DYNA manual where cards 2-5 are optional but implicitly
required if a later card has values set.
"""

import pytest

from ansys.dyna.core.keywords.keyword_classes.auto.control.control_shell import ControlShell


@pytest.mark.keywords
def test_control_shell_default_writes_only_required_card():
    """Test that a default ControlShell writes only the required card 0."""
    kwd = ControlShell()
    output = kwd.write()

    # Should have the keyword title
    assert "*CONTROL_SHELL" in output

    # Count the number of data lines (excluding title and comments)
    lines = [l for l in output.strip().split("\n") if not l.startswith("$") and not l.startswith("*")]

    # Should only have 1 data line (card 0)
    assert len(lines) == 1, f"Expected 1 data line, got {len(lines)}: {lines}"


@pytest.mark.keywords
def test_control_shell_setting_card0_writes_only_card0():
    """Test that setting a field on card 0 still writes only card 0."""
    kwd = ControlShell()
    kwd.wrpang = 30.0  # Card 0 field

    output = kwd.write()
    lines = [l for l in output.strip().split("\n") if not l.startswith("$") and not l.startswith("*")]

    assert len(lines) == 1, f"Expected 1 data line, got {len(lines)}"


@pytest.mark.keywords
def test_control_shell_setting_card1_writes_cards_0_and_1():
    """Test that setting a field on card 1 writes cards 0 and 1."""
    kwd = ControlShell()
    kwd.rotascl = 2.0  # Card 1 field

    output = kwd.write()
    lines = [l for l in output.strip().split("\n") if not l.startswith("$") and not l.startswith("*")]

    assert len(lines) == 2, f"Expected 2 data lines, got {len(lines)}"


@pytest.mark.keywords
def test_control_shell_setting_card2_writes_cards_0_1_2():
    """Test that setting a field on card 2 writes cards 0, 1, and 2."""
    kwd = ControlShell()
    kwd.cntco = 1  # Card 2 field (cntco is an int with no restrictions)

    output = kwd.write()
    lines = [l for l in output.strip().split("\n") if not l.startswith("$") and not l.startswith("*")]

    assert len(lines) == 3, f"Expected 3 data lines, got {len(lines)}"


@pytest.mark.keywords
def test_control_shell_setting_card3_writes_cards_0_to_3():
    """Test that setting a field on card 3 writes cards 0-3."""
    kwd = ControlShell()
    kwd.keepcs = 1  # Card 3 field

    output = kwd.write()
    lines = [l for l in output.strip().split("\n") if not l.startswith("$") and not l.startswith("*")]

    assert len(lines) == 4, f"Expected 4 data lines, got {len(lines)}"


@pytest.mark.keywords
def test_control_shell_setting_card4_writes_all_cards():
    """Test that setting a field on card 4 writes all 5 cards."""
    kwd = ControlShell()
    kwd.nlocdt = 1  # Card 4 field

    output = kwd.write()
    lines = [l for l in output.strip().split("\n") if not l.startswith("$") and not l.startswith("*")]

    assert len(lines) == 5, f"Expected 5 data lines, got {len(lines)}"


@pytest.mark.keywords
def test_control_shell_cascading_from_last_card():
    """Test that setting the last card activates all preceding cards."""
    kwd = ControlShell()
    kwd.drcmth = 1  # Card 4, first field

    # All cards should now be active
    assert kwd._cards[0].active is True
    assert kwd._cards[1].active is True
    assert kwd._cards[2].active is True
    assert kwd._cards[3].active is True
    assert kwd._cards[4].active is True


@pytest.mark.keywords
def test_control_shell_cascading_from_middle_card():
    """Test that setting a middle card only activates up to that card."""
    kwd = ControlShell()
    kwd.cntco = 1  # Card 2 field

    # Cards 0, 1, 2 should be active
    assert kwd._cards[0].active is True
    assert kwd._cards[1].active is True
    assert kwd._cards[2].active is True

    # Cards 3, 4 should NOT be active
    assert kwd._cards[3].active is False
    assert kwd._cards[4].active is False


@pytest.mark.keywords
def test_control_shell_card_active_property():
    """Test that cards report correct active state."""
    kwd = ControlShell()

    # Initially, only card 0 should be active
    assert kwd._cards[0].active is True
    assert kwd._cards[1].active is False
    assert kwd._cards[2].active is False
    assert kwd._cards[3].active is False
    assert kwd._cards[4].active is False


@pytest.mark.keywords
def test_control_shell_multiple_fields_same_card():
    """Test setting multiple fields on the same card."""
    kwd = ControlShell()
    kwd.rotascl = 2.0  # Card 1
    kwd.intgrd = 1     # Card 1

    output = kwd.write()
    lines = [l for l in output.strip().split("\n") if not l.startswith("$") and not l.startswith("*")]

    assert len(lines) == 2, f"Expected 2 data lines, got {len(lines)}"


@pytest.mark.keywords
def test_control_shell_fields_on_multiple_cards():
    """Test setting fields on non-consecutive cards."""
    kwd = ControlShell()
    kwd.wrpang = 25.0  # Card 0
    kwd.keepcs = 1     # Card 3

    output = kwd.write()
    lines = [l for l in output.strip().split("\n") if not l.startswith("$") and not l.startswith("*")]

    # Should have 4 cards (0-3)
    assert len(lines) == 4, f"Expected 4 data lines, got {len(lines)}"
