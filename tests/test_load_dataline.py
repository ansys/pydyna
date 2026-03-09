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
import warnings

import pytest

from ansys.dyna.core.lib.kwd_line_formatter import load_dataline, parse_dataline
from ansys.dyna.core.lib.parameters import ParameterSet
from ansys.dyna.core.lib.field import Flag

@dataclasses.dataclass
class bi:
    foo: float = None
    bar: float = None

def test_load_dataline_001():
    """test loading a line of right-justified int and string"""
    values, _ = load_dataline([(0, 10, int), (10, 10, str)], "         1     hello")
    assert values == (1, "hello")

def test_load_dataline_002():
    """test loading a line of right-justified int and left-justified string"""
    values, _ = load_dataline([(0, 10, int), (10, 10, str)], "         1hello     ")
    assert values == (1, "hello")


def test_load_dataline_003():
    """test loading a line of float and int, where the int is written as a float"""
    values, _ = load_dataline([(0, 8, int), (8, 8, float)], "     0.0     1.0")
    assert values == (0, 1.0)


def test_load_dataline_004():
    """test loading a partial line"""
    values, _ = load_dataline([(0, 8, int), (8, 8, float), (16, 8, str)], "     0.0     1.0")
    assert values == (0, 1.0, None)


def test_load_dataline_005():
    """test loading a partial line with missing float"""
    values, _ = load_dataline([(0, 8, int), (8, 8, float), (16, 8, float)], "     0.0     1.0")
    a, b, c = values
    assert a == 0 and b == 1.0 and math.isnan(c)


def test_load_dataline_006():
    """test loading a data line that is too long."""
    values, line_warnings = load_dataline(
        [(0, 8, int), (8, 8, float), (16, 8, float)], "                                          "
    )
    assert len(line_warnings) > 0
    with pytest.warns(UserWarning, match="Detected out of bound card characters"):
        for msg in line_warnings:
            warnings.warn(msg)


def test_load_dataline_007():
    """Test loading a data line with a parameter."""
    spec = [(0, 10, float), (10, 10, float), (20, 10, float), (30, 10, float), (40, 10, float), (50, 10, int), (60, 10, float), (70, 10, float)]
    dataline = "                                             &vdct"

    parameter_set = ParameterSet()
    parameter_set.add("vdct", 1.12)
    res, _ = load_dataline(spec, dataline, parameter_set)
    assert res[4] == 1.12

def test_load_dataline_008():
    """Test loading a data line with a struct type."""
    spec = [(0, 10, bi)]
    dataline = "         2        50"
    res, _ = load_dataline(spec, dataline)
    assert len(res) == 1
    assert res[0].foo == 2
    assert res[0].bar == 50

def test_load_dataline_009():
    """Test loading a data line with two struct types."""
    dataline = "         2        50         1       3.1"
    spec = [(0, 10, bi), (20, 10, bi)]
    res, _ = load_dataline(spec, dataline)
    assert len(res) == 2
    assert res[0].foo == 2
    assert res[0].bar == 50
    assert res[1].foo == 1
    assert res[1].bar == 3.1

def test_load_dataline_010():
    """Test loading a data line with two struct types and a missing value."""
    dataline = "         2        50         1"
    spec = [(0, 10, bi), (20, 10, bi)]
    res, _ = load_dataline(spec, dataline)
    assert len(res) == 2
    assert res[0].foo == 2
    assert res[0].bar == 50
    assert res[1].foo == 1
    assert math.isnan(res[1].bar)

def test_load_dataline_011():
    """Test reading flags"""
    dataline = "         1&         "
    spec = [(0, 10, int), (10, 10, Flag(True, "&", ""))]
    res, _ = load_dataline(spec, dataline)
    assert len(res) == 2
    assert res[0] == 1
    assert res[1] == True
    dataline = "         1          "
    res, _ = load_dataline(spec, dataline)
    assert len(res) == 2
    assert res[0] == 1
    assert res[1] == False


def test_load_dataline_flag_ampersand_csv():
    """Test reading flag with & as true_value in CSV format.

    Flags with true_value="&" (e.g. mpp2 in contact keywords) must be
    correctly parsed even in CSV format, where & could be confused with
    parameter references.
    """
    spec = [(0, 10, int), (10, 10, Flag(True, "&", ""))]
    # CSV format: "1,&" should parse as (1, True)
    res, _ = load_dataline(spec, "1,&")
    assert len(res) == 2
    assert res[0] == 1
    assert res[1] is True

    # CSV format: "1," (empty) should parse as (1, False)
    res, _ = load_dataline(spec, "1,")
    assert len(res) == 2
    assert res[0] == 1
    assert res[1] is False

    # CSV format: "1" (missing field) should parse as (1, False)
    res, _ = load_dataline(spec, "1")
    assert len(res) == 2
    assert res[0] == 1
    assert res[1] is False

def test_load_dataline_012():
    """Test loading a data line with parameter type mismatches."""
    spec = [(0, 10, float), (10, 10, float), (20, 10, float), (30, 10, float), (40, 10, int)]
    dataline = "                                             &vdct"

    parameter_set = ParameterSet()
    parameter_set.add("vdct", 1.12)
    with pytest.raises(TypeError):
        load_dataline(spec, dataline, parameter_set)

    parameter_set = ParameterSet()
    parameter_set.add("vdct", 1.0)
    res, _ = load_dataline(spec, dataline, parameter_set)
    assert res[4] == 1


def test_load_dataline_013():
    """Test loading LS-DYNA compact scientific notation (no E)."""
    # Format: "1.00000+4" means 1.00000E+4 = 10000
    # This is the actual format from bird_B.k: "         1 1.0000000 1.00000+4 0.0000000"
    spec = [(0, 10, int), (10, 10, float), (20, 10, float), (30, 10, float)]
    dataline = "         1 1.00000001.00000+4 0.0000000"
    res, _ = load_dataline(spec, dataline)
    assert res[0] == 1
    assert res[1] == 1.0
    assert res[2] == 10000.0
    assert res[3] == 0.0


def test_load_dataline_014():
    """Test loading LS-DYNA compact scientific notation with negative exponent."""
    spec = [(0, 10, float), (10, 10, float)]
    dataline = "   3.5-10      1.0"
    res, _ = load_dataline(spec, dataline)
    assert res[0] == 3.5e-10
    assert res[1] == 1.0


def test_load_dataline_015():
    """Test loading LS-DYNA compact notation for integers."""
    spec = [(0, 10, int), (10, 10, int)]
    # 1+4 = 1E4 = 10000
    dataline = "       1+4       2+3"
    res, _ = load_dataline(spec, dataline)
    assert res[0] == 10000
    assert res[1] == 2000


def test_load_dataline_016():
    """Test Fortran D notation (1.0D+4 -> 1.0E+4)."""
    spec = [(0, 10, float), (10, 10, float)]
    dataline = "  1.0D+04  2.5d-03"
    res, _ = load_dataline(spec, dataline)
    assert res[0] == 10000.0
    assert res[1] == 0.0025


def test_load_dataline_017():
    """Test compact scientific notation in CSV format."""
    spec = [(0, 10, float), (10, 10, float), (20, 10, float)]
    dataline = "1.0,1.00000+4,0.0"
    res, _ = load_dataline(spec, dataline)
    assert res[0] == 1.0
    assert res[1] == 10000.0
    assert res[2] == 0.0


class TestParseDatalineRawMode:
    """Tests for parse_dataline (raw mode) and load_dataline(spec, line, None).

    Parameter-defining keywords use raw parsing: '&' is literal, not a parameter reference.
    """

    def test_parse_dataline_str_field_with_ampersand_is_literal(self):
        """str fields with & are preserved as literal (e.g. PARAMETER_EXPRESSION)."""
        spec = [(0, 10, str), (10, 60, str)]
        line = "R   F_calc  cos(&angle_d*pi/180.0)+sin(&angle_d*pi/180.0)"
        values, _ = parse_dataline(spec, line)
        assert values[0].strip().startswith("R")
        assert "&angle_d" in values[1]
        assert values[1].strip() == "cos(&angle_d*pi/180.0)+sin(&angle_d*pi/180.0)"

    def test_parse_dataline_simple_param_ref_in_str_is_literal(self):
        """Simple &param in str field is literal in raw mode."""
        spec = [(0, 10, str), (10, 20, str)]
        line = "R         &base+5"
        values, _ = parse_dataline(spec, line)
        assert values[1].strip() == "&base+5"

    def test_parse_dataline_float_field_with_param_returns_nan(self):
        """Float fields with & in raw mode return nan (graceful loading)."""
        spec = [(0, 8, int), (8, 16, float)]
        line = "       1     &param"
        values, _ = parse_dataline(spec, line)
        assert values[0] == 1
        assert math.isnan(values[1])

    def test_parse_dataline_csv_format(self):
        """parse_dataline handles CSV format."""
        spec = [(0, 10, int), (10, 10, str), (20, 10, str)]
        line = "1,hello,cos(&x)"
        values, _ = parse_dataline(spec, line)
        assert values == (1, "hello", "cos(&x)")

    def test_load_dataline_none_delegates_to_parse_dataline(self):
        """load_dataline(spec, line, None) behaves like parse_dataline (raw mode)."""
        spec = [(0, 10, str), (10, 60, str)]
        line = "R   F_calc  cos(&angle_d*pi/180.0)"
        parse_vals, _ = parse_dataline(spec, line)
        load_vals, _ = load_dataline(spec, line, None)
        assert parse_vals == load_vals
        assert "&angle_d" in load_vals[1]

    def test_load_dataline_with_param_set_substitutes(self):
        """load_dataline(spec, line, parameter_set) performs substitution."""
        spec = [(0, 10, float), (10, 10, float)]
        line = "     0.0     &val"
        params = ParameterSet()
        params.add("val", 42.0)
        values, _ = load_dataline(spec, line, params)
        assert values[0] == 0.0
        assert values[1] == 42.0

    def test_parse_dataline_fixed_width_basic(self):
        """parse_dataline handles fixed-width format."""
        spec = [(0, 10, int), (10, 10, str)]
        line = "         1     hello"
        values, _ = parse_dataline(spec, line)
        assert values == (1, "hello")

    def test_parse_dataline_flag_with_ampersand_csv(self):
        """Flags with & as true_value work in CSV format even in raw mode."""
        spec = [(0, 10, int), (10, 10, Flag(True, "&", ""))]
        # Even in raw mode (parse_dataline), flags should be handled correctly
        values, _ = parse_dataline(spec, "1,&")
        assert values[0] == 1
        assert values[1] is True

    def test_parse_dataline_flag_with_ampersand_fixed(self):
        """Flags with & as true_value work in fixed-width format."""
        spec = [(0, 10, int), (10, 10, Flag(True, "&", ""))]
        values, _ = parse_dataline(spec, "         1&         ")
        assert values[0] == 1
        assert values[1] is True
