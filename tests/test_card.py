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

import pytest

from ansys.dyna.core.lib.card import Card, Field
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.parameter_set import ParameterSet


@pytest.mark.keywords
def test_load_card_errors(string_utils):
    """Error test for loading a card."""
    fields = [
        Field("foo", int, 0, 10, None),
        Field("bar", int, 10, 10, None),
    ]

    card = Card(fields)
    with pytest.raises(Exception):
        # cards can only load a readable buffer
        card.read("")

    with pytest.warns(UserWarning, match="Detected out of bound card characters"):
        # error if the line that is too long
        buf = "                                           "
        card.read(string_utils.as_buffer(buf))


@pytest.mark.keywords
def test_load_card_parameters(string_utils):
    """Error test for loading a card."""
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

    card = Card(fields)
    parameter_set = ParameterSet()
    parameter_set.add("vdct", 1.12)
    buf = "                                             &vdct"
    card.read(string_utils.as_buffer(buf), parameter_set)
    assert card.get_value("e") == 1.12


@pytest.mark.keywords
def test_load_card_basic(string_utils):
    fields = [
        Field("foo", int, 0, 10, None),
        Field("bar", int, 10, 10, None),
    ]
    card = Card(fields)
    card.read(string_utils.as_buffer("                    "))
    assert card.get_value("foo") == None
    assert card.get_value("bar") == None
    card = Card(fields)
    card.read(string_utils.as_buffer("         8         4"))
    assert card.get_value("foo") == 8
    assert card.get_value("bar") == 4


@pytest.mark.keywords
def test_load_card_long(string_utils):
    fields = [
        Field("foo", int, 0, 10, None),
        Field("bar", int, 10, 10, None),
    ]
    card = Card(fields, format=format_type.long)
    buf = string_utils.as_buffer("                                       4")
    card.read(buf)
    assert card.get_value("foo") == None
    assert card.get_value("bar") == 4


@pytest.mark.keywords
def test_write_inactive_card():
    fields = [
        Field("foo", int, 0, 10, None),
        Field("bar", int, 10, 10, None),
    ]
    card = Card(fields, lambda: False, format=format_type.long)
    assert card.write() == ""
