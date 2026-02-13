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

"""Tests for the legacy Card(fields=[...]) API.

This module tests the backward-compatible Card constructor that accepts
a list of Field objects. This API is deprecated but maintained for users
who have created custom keywords using the Field-based approach.

The preferred approach for new code is to use:
- Card.from_field_schemas() - fast path using FieldSchema tuples
- Card.from_field_schemas_with_defaults() - for lspp_defaults support
"""

import pytest

from ansys.dyna.core.lib.card import Card, Field
from ansys.dyna.core.lib.field import Flag
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.parameters import ParameterSet


def test_legacy_card_deprecation_warning():
    """Test that using Card(fields=[...]) emits a deprecation warning."""
    fields = [
        Field("foo", int, 0, 10, None),
        Field("bar", int, 10, 10, None),
    ]
    with pytest.warns(DeprecationWarning, match="Card.*fields.*deprecated"):
        Card(fields)



def test_load_card_errors(string_utils):
    """Error test for loading a card using legacy API."""
    fields = [
        Field("foo", int, 0, 10, None),
        Field("bar", int, 10, 10, None),
    ]

    with pytest.warns(DeprecationWarning):
        card = Card(fields)

    with pytest.raises(Exception):
        # cards can only load a readable buffer
        card.read("")

    with pytest.warns(UserWarning, match="Detected out of bound card characters"):
        # error if the line that is too long
        buf = "                                           "
        card.read(string_utils.as_buffer(buf))



def test_load_card_parameters(string_utils):
    """Test loading a card with parameters using legacy API."""
    fields = [
        Field("a", float, 0, 10, None),
        Field("b", float, 10, 10, None),
        Field("c", float, 20, 10, None),
        Field("d", float, 30, 10, None),
        Field("e", float, 40, 10, None),
        Field("f", float, 50, 10, None),
        Field("g", float, 60, 10, None),
        Field("h", float, 70, 10, None),
    ]

    with pytest.warns(DeprecationWarning):
        card = Card(fields)

    parameter_set = ParameterSet()
    parameter_set.add("vdct", 1.12)
    buf = "                                             &vdct"
    card.read(string_utils.as_buffer(buf), parameter_set)
    assert card.get_value("e") == 1.12



def test_load_card_basic(string_utils):
    """Test basic card loading using legacy API."""
    fields = [
        Field("foo", int, 0, 10, None),
        Field("bar", int, 10, 10, None),
    ]

    with pytest.warns(DeprecationWarning):
        card = Card(fields)

    card.read(string_utils.as_buffer("                    "))
    assert card.get_value("foo") is None
    assert card.get_value("bar") is None

    with pytest.warns(DeprecationWarning):
        card = Card(fields)

    card.read(string_utils.as_buffer("         8         4"))
    assert card.get_value("foo") == 8
    assert card.get_value("bar") == 4



def test_load_card_long(string_utils):
    """Test loading card in long format using legacy API."""
    fields = [
        Field("foo", int, 0, 10, None),
        Field("bar", int, 10, 10, None),
    ]

    with pytest.warns(DeprecationWarning):
        card = Card(fields, format=format_type.long)

    buf = string_utils.as_buffer("                                       4")
    card.read(buf)
    assert card.get_value("foo") is None
    assert card.get_value("bar") == 4



def test_write_inactive_card():
    """Test writing inactive card using legacy API."""
    fields = [
        Field("foo", int, 0, 10, None),
        Field("bar", int, 10, 10, None),
    ]

    with pytest.warns(DeprecationWarning):
        card = Card(fields, lambda: False, format=format_type.long)

    assert card.write() == ""



def test_legacy_card_with_flag_field(string_utils):
    """Test legacy Card with Flag field."""
    flag = Flag(value=None, true_value="YES", false_value="NO")
    fields = [
        Field("enabled", str, 0, 10, flag),
        Field("count", int, 10, 10, None),
    ]

    with pytest.warns(DeprecationWarning):
        card = Card(fields)

    card.read(string_utils.as_buffer("       YES         5"))
    assert card.get_value("count") == 5



def test_legacy_card_get_set_value():
    """Test get_value and set_value on legacy Card."""
    fields = [
        Field("x", int, 0, 10, 10),
        Field("y", float, 10, 10, 2.5),
        Field("z", str, 20, 10, "hello"),
    ]

    with pytest.warns(DeprecationWarning):
        card = Card(fields)

    # Test get_value
    assert card.get_value("x") == 10
    assert card.get_value("y") == 2.5
    assert card.get_value("z") == "hello"

    # Test set_value
    card.set_value("x", 100)
    card.set_value("y", 3.14)
    card.set_value("z", "world")

    assert card.get_value("x") == 100
    assert card.get_value("y") == 3.14
    assert card.get_value("z") == "world"



def test_legacy_card_write():
    """Test write on legacy Card."""
    fields = [
        Field("a", int, 0, 10, 42),
        Field("b", int, 10, 10, 99),
    ]

    with pytest.warns(DeprecationWarning):
        card = Card(fields)

    output = card.write(comment=False)
    assert "42" in output
    assert "99" in output



def test_legacy_card_active_func():
    """Test active_func on legacy Card."""
    fields = [
        Field("x", int, 0, 10, 1),
    ]
    is_active = True

    with pytest.warns(DeprecationWarning):
        card = Card(fields, active_func=lambda: is_active)

    assert card.active is True

    is_active = False
    assert card.active is False
