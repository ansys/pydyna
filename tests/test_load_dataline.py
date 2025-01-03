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

import pytest

from ansys.dyna.core.lib.kwd_line_formatter import load_dataline
from ansys.dyna.core.lib.parameter_set import ParameterSet

@pytest.mark.keywords
def test_load_dataline_001():
    """test loading a line of right-justified int and string"""
    x = load_dataline([(0, 10, int), (10, 10, str)], "         1     hello")
    assert x == (1, "hello")


@pytest.mark.keywords
def test_load_dataline_002():
    """test loading a line of right-justified int and left-justified string"""
    x = load_dataline([(0, 10, int), (10, 10, str)], "         1hello     ")
    assert x == (1, "hello")


@pytest.mark.keywords
def test_load_dataline_003():
    """test loading a line of float and int, where the int is written as a float"""
    x = load_dataline([(0, 8, int), (8, 8, float)], "     0.0     1.0")
    assert x == (0, 1.0)


@pytest.mark.keywords
def test_load_dataline_004():
    """test loading a partial line"""
    x = load_dataline([(0, 8, int), (8, 8, float), (16, 8, str)], "     0.0     1.0")
    assert x == (0, 1.0, None)


@pytest.mark.keywords
def test_load_dataline_005():
    """test loading a partial line with missing float"""
    a, b, c = load_dataline([(0, 8, int), (8, 8, float), (16, 8, float)], "     0.0     1.0")
    assert a == 0 and b == 1.0 and math.isnan(c)


@pytest.mark.keywords
def test_load_dataline_006():
    """test loading a data line that is too long."""
    with pytest.warns(UserWarning, match="Detected out of bound card characters"):
        load_dataline([(0, 8, int), (8, 8, float), (16, 8, float)], "                                          ")


@pytest.mark.keywords
def test_load_dataline_007():
    """Test loading a data line with a parameter."""
    spec = [(0, 10, float), (10, 10, float), (20, 10, float), (30, 10, float), (40, 10, float), (50, 10, int), (60, 10, float), (70, 10, float)]
    dataline = "                                             &vdct"

    parameter_set = ParameterSet()
    parameter_set.add("vdct", 1.12)
    res = load_dataline(spec, dataline, parameter_set)
    assert res[4] == 1.12
