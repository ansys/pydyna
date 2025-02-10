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

import dataclasses
import math

from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.variable_card import VariableCard

import pytest

@dataclasses.dataclass
class bi:
    foo: float = None
    bar: float = None

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
def test_variable_card_read_struct(string_utils):
    """Test a variable card using the struct type definition."""
    v = VariableCard("bi", 8, 10, bi)
    string = "     113.2    -50.01"
    v.read(string_utils.as_buffer(string))
    assert len(v) == 1
    assert v[0].foo == 113.2
    assert v[0].bar == -50.01

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
def test_variable_card_set_single_value_struct():
    """test setting single value of bounded variable card"""
    v = VariableCard("bi", 8, 10, bi, lambda: 3)
    v[0] = bi(0.8, 0.9)
    rowdata = v._get_row_data(0, format_type.default)
    assert rowdata.startswith("       0.8       0.9"), f"incorrect rowdata: {rowdata}"
    v[0] = (0.1, 1.2)
    rowdata = v._get_row_data(0, format_type.default)
    assert rowdata.startswith("       0.1       1.2"), f"incorrect rowdata: {rowdata}"
    v[0] = [0.6, math.nan]
    rowdata = v._get_row_data(0, format_type.default)
    assert rowdata.startswith("       0.6          "), f"incorrect rowdata: {rowdata}"

@pytest.mark.keywords
def test_variable_card_append_value_struct():
    """test setting single value of bounded variable card"""
    v = VariableCard("bi", 8, 10, bi)
    v.append(bi(0.8, 0.9))
    v.extend([
        (0.1, 1.2),
        [math.nan, 0.2]
    ])
    assert len(v) == 3
    assert v[0] == bi(0.8,0.9)
    assert v[1] == bi(0.1,1.2)
    assert v[2] == bi(math.nan,0.2)

@pytest.mark.keywords
def test_variable_card_write_long(ref_string):
    """test writing unbounded long variable card with two rows."""
    v = VariableCard("bi", 8, 10, float)
    for i in range(10):
        v.append(i)
    assert v._num_rows() == 2
    assert v.write(format_type.long) == ref_string.test_variable_card_string_long
    v.format = format_type.long
    assert v.write() == ref_string.test_variable_card_string_long
    assert v.write(format_type.default) == ref_string.test_variable_card_string

@pytest.mark.keywords
def test_variable_card_write_long_struct(ref_string):
    """Test a variable card using the struct type definition."""
    v = VariableCard("bi", 8, 10, bi)
    for i in range(9):
        val = i+1
        v.append(bi(val, val/2))
    assert v._num_rows() == 3

    v_comment = "$#     foo       bar       foo       bar       foo       bar       foo       bar"
    assert v._get_comment(format_type.default) == v_comment
    standard_output = v.write()
    assert standard_output == ref_string.test_variable_card_struct_string
    long_output = v.write(format = format_type.long)
    assert long_output == ref_string.test_variable_card_struct_string_long

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

@pytest.mark.keywords
def test_variable_card_read_long_struct(ref_string, string_utils):
    """Test a variable card using the struct type definition."""
    v = VariableCard("bi", 8, 10, bi)
    v.format = format_type.long
    string = "               113.2              -50.01                                        "
    v.read(string_utils.as_buffer(string))
    assert len(v) == 2
    assert v[0].foo == 113.2
    assert v[0].bar == -50.01
    assert math.isnan(v[1].foo)
    assert math.isnan(v[1].bar)

    v = VariableCard("bi", 8, 10, bi)
    v.read(string_utils.as_buffer(ref_string.test_variable_card_struct_string))
    assert len(v) == 9

    v = VariableCard("bi", 8, 10, bi)
    v.format = format_type.long
    v.read(string_utils.as_buffer(ref_string.test_variable_card_struct_string_long))
    assert len(v) == 9

@pytest.mark.keywords
def test_variable_card_write_struct(string_utils):
    """Test a variable card using the struct type definition."""

    string = "       1.0       2.0       3.0"
    v = VariableCard("bi", 8, 10, bi, lambda: 2)
    v.read(string_utils.as_buffer(string))
    assert len(v) == 2
    assert v[0].foo == 1.0
    assert v[0].bar == 2.0
    assert v[1].foo == 3.0
    assert math.isnan(v[1].bar)
    assert v._num_rows() == 1
    result = v.write(comment=False)
    string = "       1.0       2.0       3.0          "
    assert result == string
    result = v.write(comment=True)
    string = "$#     foo       bar       foo       bar\n       1.0       2.0       3.0          "
    assert result == string
