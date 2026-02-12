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
import io
import math

from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.parameters import ParameterSet
from ansys.dyna.core.lib.series_card import SeriesCard

import pytest

@dataclasses.dataclass
class bi:
    foo: float = None
    bar: float = None


def test_series_card_length_function():
    """test series card"""
    v = SeriesCard("bi", 8, 10, float, lambda: 4)
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



def test_series_card_read(string_utils):
    """test loading from buffer"""
    string = "       1.0       2.0          "
    v = SeriesCard("bi", 8, 10, float, lambda: 3)
    v.read(string_utils.as_buffer(string))
    assert v[0] == 1.0
    assert v[1] == 2.0
    assert math.isnan(v[2])


def test_series_card_read_out_of_bounds(string_utils):
    """test loading from buffer"""
    string = "       1.0       2.0          "
    v = SeriesCard("bi", 8, 10, float)
    v.read(string_utils.as_buffer(string))
    assert len(v) == 2
    assert v[0] == 1.0
    assert v[1] == 2.0


def test_series_card_read_struct(string_utils):
    """Test a series card using the struct type definition."""
    v = SeriesCard("bi", 8, 10, bi)
    string = "     113.2    -50.01"
    v.read(string_utils.as_buffer(string))
    assert len(v) == 1
    assert v[0].foo == 113.2
    assert v[0].bar == -50.01


def test_series_card_unbounded(string_utils):
    """test unbounded series card"""
    v = SeriesCard("bi", 8, 10, float)
    string = "       1.0       2.0          "
    v.read(string_utils.as_buffer(string))
    assert v._num_rows() == 1
    assert len(v) == 2
    v.append(3.0)
    assert v.write(format_type.default) == "$#      bi        bi        bi\n       1.0       2.0       3.0"
    for i in range(10):
        v.append(i)
    assert len(v) == 13
    assert v._num_rows() == 2



def test_write_inactive_series_card():
    card = SeriesCard("bi", 8, 10, float, None, lambda: False)
    assert card.write() == ""



def test_write_empty_series_card():
    card = SeriesCard("bi", 8, 10, float, lambda: 0)
    assert card.write() == ""



def test_series_card_set_single_value():
    """test setting single value of bounded series card"""
    v = SeriesCard("pi", 8, 10, float, lambda: 3)
    v[0] = 22
    rowdata = v._get_row_data(0, format_type.default)
    assert rowdata.startswith("      22.0"), f"incorrect rowdata: {rowdata}"


def test_series_card_set_single_value_struct():
    """test setting single value of bounded series card"""
    v = SeriesCard("bi", 8, 10, bi, lambda: 3)
    v[0] = bi(0.8, 0.9)
    rowdata = v._get_row_data(0, format_type.default)
    assert rowdata.startswith("       0.8       0.9"), f"incorrect rowdata: {rowdata}"
    v[0] = (0.1, 1.2)
    rowdata = v._get_row_data(0, format_type.default)
    assert rowdata.startswith("       0.1       1.2"), f"incorrect rowdata: {rowdata}"
    v[0] = [0.6, math.nan]
    rowdata = v._get_row_data(0, format_type.default)
    assert rowdata.startswith("       0.6          "), f"incorrect rowdata: {rowdata}"


def test_series_card_append_value_struct():
    """test setting single value of bounded series card"""
    v = SeriesCard("bi", 8, 10, bi)
    v.append(bi(0.8, 0.9))
    v.extend([
        (0.1, 1.2),
        [math.nan, 0.2]
    ])
    assert len(v) == 3
    assert v[0] == bi(0.8,0.9)
    assert v[1] == bi(0.1,1.2)
    assert math.isnan(v[2].foo)
    assert v[2].bar == 0.2


def test_series_card_write_long(ref_string):
    """test writing unbounded long series card with two rows."""
    v = SeriesCard("bi", 8, 10, float)
    for i in range(10):
        v.append(i)
    assert v._num_rows() == 2
    assert v.write(format_type.long) == ref_string.test_series_card_string_long
    v.format = format_type.long
    assert v.write() == ref_string.test_series_card_string_long
    assert v.write(format_type.default) == ref_string.test_series_card_string


def test_series_card_write_long_struct(ref_string):
    """Test a series card using the struct type definition."""
    v = SeriesCard("bi", 8, 10, bi)
    for i in range(9):
        val = i+1
        v.append(bi(val, val/2))
    assert v._num_rows() == 3

    v_comment = "$#     foo       bar       foo       bar       foo       bar       foo       bar"
    assert v._get_comment(format_type.default) == v_comment
    standard_output = v.write()
    assert standard_output == ref_string.test_series_card_struct_string
    long_output = v.write(format = format_type.long)
    assert long_output == ref_string.test_series_card_struct_string_long


def test_series_card_read_long(string_utils):
    """test reading unbounded long series card with one row."""
    v = SeriesCard("bi", 8, 10, float)
    v.format = format_type.long
    string = "                 1.0                 2.0                    "
    v.read(string_utils.as_buffer(string))
    assert v._num_rows() == 1
    assert len(v) == 2
    assert v[0] == 1.0
    assert v[1] == 2.0


def test_series_card_read_long_struct(ref_string, string_utils):
    """Test a series card using the struct type definition."""
    v = SeriesCard("bi", 8, 10, bi)
    v.format = format_type.long
    string = "               113.2              -50.01                                        "
    v.read(string_utils.as_buffer(string))
    assert len(v) == 1
    assert v[0].foo == 113.2
    assert v[0].bar == -50.01

    v = SeriesCard("bi", 8, 10, bi)
    v.read(string_utils.as_buffer(ref_string.test_series_card_struct_string))
    assert len(v) == 9

    v = SeriesCard("bi", 8, 10, bi)
    v.format = format_type.long
    v.read(string_utils.as_buffer(ref_string.test_series_card_struct_string_long))
    assert len(v) == 9


def test_series_card_write_struct(string_utils):
    """Test a series card using the struct type definition."""

    string = "       1.0       2.0       3.0"
    v = SeriesCard("bi", 8, 10, bi, lambda: 2)
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


# =============================================================================
# Parameter Retention Tests for SeriesCard
# =============================================================================


class TestSeriesCardParameterRetention:
    """Test parameter retention (write-back) for SeriesCard."""

    
    def test_series_card_write_with_parameter_retention_single_line(self):
        """Test that a single-line series card retains parameter references when writing."""
        parameter_set = ParameterSet()
        parameter_set.add("val1", 100.0)
        parameter_set.add("val2", 200.0)

        series = SeriesCard(
            name="nodes",
            fields_per_card=8,
            element_width=10,
            input_type=int,
            length_func=None,  # Unbounded
        )

        card_text = "       100     &val1       102     &val2"
        buf = io.StringIO(card_text)
        series.read(buf, parameter_set)

        # Values should be resolved
        assert series.data[0] == 100
        assert series.data[1] == 100  # &val1 resolved to 100
        assert series.data[2] == 102
        assert series.data[3] == 200  # &val2 resolved to 200

        # Write with retain_parameters=True should show parameter refs
        # Empty uri_prefix since at unit test level there's no outer scope
        output = series.write(
            comment=False,
            retain_parameters=True,
            parameter_set=parameter_set,
            uri_prefix="",
        )
        assert "&val1" in output, f"Expected '&val1' in output: {output}"
        assert "&val2" in output, f"Expected '&val2' in output: {output}"

    
    def test_series_card_write_with_parameter_retention_multi_line(self):
        """Test that multi-line series card retains parameter references when writing."""
        parameter_set = ParameterSet()
        parameter_set.add("n1", 102)
        parameter_set.add("n2", 107)

        series = SeriesCard(
            name="nodes",
            fields_per_card=8,
            element_width=10,
            input_type=int,
            length_func=None,  # Unbounded
        )

        # Two lines of data with parameters spread across them
        card_text = """       100       101      &n1       103       104       105       106      &n2
       108       109       110"""
        buf = io.StringIO(card_text)
        series.read(buf, parameter_set)

        # Values should be resolved
        assert len(series.data) == 11
        assert series.data[2] == 102  # &n1 resolved
        assert series.data[7] == 107  # &n2 resolved

        # Write with retain_parameters=True should show parameter refs
        # Empty uri_prefix since at unit test level there's no outer scope
        output = series.write(
            comment=False,
            retain_parameters=True,
            parameter_set=parameter_set,
            uri_prefix="",
        )
        assert "&n1" in output, f"Expected '&n1' in output: {output}"
        assert "&n2" in output, f"Expected '&n2' in output: {output}"

    
    def test_series_card_write_without_parameter_retention(self):
        """Test that series card without retain_parameters writes resolved values."""
        parameter_set = ParameterSet()
        parameter_set.add("val1", 100.0)

        series = SeriesCard(
            name="values",
            fields_per_card=8,
            element_width=10,
            input_type=float,
            length_func=None,
        )

        card_text = "       1.0     &val1       3.0"
        buf = io.StringIO(card_text)
        series.read(buf, parameter_set)

        # Write without retain_parameters should show resolved values
        output = series.write(comment=False)
        assert "&val1" not in output
        assert "100.0" in output

    
    def test_series_card_write_with_negative_parameter(self):
        """Test that negative parameter references are retained."""
        parameter_set = ParameterSet()
        parameter_set.add("offs", 50.0)

        series = SeriesCard(
            name="values",
            fields_per_card=8,
            element_width=10,
            input_type=float,
            length_func=None,
        )

        card_text = "       1.0    -&offs       3.0"
        buf = io.StringIO(card_text)
        series.read(buf, parameter_set)

        # Value should be negated
        assert series.data[1] == -50.0

        # Write with retain_parameters should show -&offs
        # Empty uri_prefix since at unit test level there's no outer scope
        output = series.write(
            comment=False,
            retain_parameters=True,
            parameter_set=parameter_set,
            uri_prefix="",
        )
        assert "-&offs" in output, f"Expected '-&offs' in output: {output}"
