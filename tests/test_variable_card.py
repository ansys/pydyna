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

import math

from ansys.dyna.core import Deck
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.variable_card import VariableCard

import pytest


@pytest.mark.keywords
def test_variable_card_length_function():
    """test variable card"""
    v = VariableCard("bi", 8, 10, float, lambda: 4)
    assert len(v) == 4
    assert (0, 4) == v._get_card_range(0), "card range incorrect"
    assert v._is_last_card(0), "first card should be last"
    assert v._get_comment(format_type.default) == "$#      bi        bi        bi        bi"
    v._length_func = lambda: 9
    assert (0, 8) == v._get_card_range(0), "card range incorrect"
    assert (8, 9) == v._get_card_range(1), "card range incorrect"
    assert not v._is_last_card(0), "first card should be last"
    assert (
        v._get_comment(format_type.default)
        == "$#      bi        bi        bi        bi        bi        bi        bi        bi"
    )


@pytest.mark.keywords
def test_variable_card_read(string_utils):
    """test loading from buffer"""
    string = "       1.0       2.0          "
    v = VariableCard("bi", 8, 10, float, lambda: 3)
    v.read(string_utils.as_buffer(string))
    assert v[0] == 1.0
    assert v[1] == 2.0
    assert math.isnan(v[2])


@pytest.mark.keywords
def test_variable_card_unbounded(string_utils):
    """test unbounded variable card"""
    v = VariableCard("bi", 8, 10, float)
    string = "       1.0       2.0          "
    v.read(string_utils.as_buffer(string))
    assert v._num_rows() == 1
    assert len(v) == 3
    v[2] = 3.0
    assert v.write(format_type.default) == "$#      bi        bi        bi\n       1.0       2.0       3.0"
    for i in range(10):
        v.append(i)
    assert len(v) == 13
    assert v._num_rows() == 2


@pytest.mark.keywords
def test_write_inactive_variable_card():
    card = VariableCard("bi", 8, 10, float, None, lambda: False)
    assert card.write() == ""


@pytest.mark.keywords
def test_write_empty_variable_card():
    card = VariableCard("bi", 8, 10, float, lambda: 0)
    assert card.write() == ""


@pytest.mark.keywords
def test_variable_card_set_single_value():
    """test setting single value of bounded variable card"""
    v = VariableCard("pi", 8, 10, float, lambda: 3)
    v[0] = 22
    rowdata = v._get_row_data(0, format_type.default)
    assert rowdata.startswith("      22.0"), f"incorrect rowdata: {rowdata}"


@pytest.mark.keywords
def test_variable_car_write_long(ref_string):
    """test writing unbounded long variable card with two rows."""
    v = VariableCard("bi", 8, 10, float)
    v.format_type = format_type.long
    for i in range(10):
        v.append(i)
    assert v._num_rows() == 2
    assert v.write(format_type.long) == ref_string.test_variable_card_string


@pytest.mark.keywords
def test_variable_card_read_long(string_utils):
    """test reading unbounded long variable card with one row."""
    v = VariableCard("bi", 8, 10, float)
    v.format = format_type.long
    string = "                 1.0                 2.0                    "
    v.read(string_utils.as_buffer(string))
    assert v._num_rows() == 1
    assert len(v) == 3
    assert v[0] == 1.0
    assert v[1] == 2.0
    assert math.isnan(v[2])


@pytest.mark.xfail(reason = "Keyword module not yet available.")
@pytest.mark.keywords
def test_variable_card_read_write_set(ref_string):
    """test to read and write variable cards, especially checking case where last card contains all fields"""
    set_string = ref_string.test_variable_card_sets_string
    input_deck = Deck()
    input_deck.loads(set_string)
    assert input_deck.write() == set_string
