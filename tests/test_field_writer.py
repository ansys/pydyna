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

import io
import typing

from ansys.dyna.core.lib.field import Field
import ansys.dyna.core.lib.field_writer as field_writer
from ansys.dyna.core.lib.format_type import format_type


import pytest

def _get_comment_line(fields: typing.List[Field]) -> str:
    s = io.StringIO()
    field_writer.write_comment_line(s, fields)
    return s.getvalue()


def _get_field_value(fields: typing.List[Field], long: bool = False) -> str:
    s = io.StringIO()
    format = format_type.default
    if long:
        format = format_type.long
    field_writer.write_fields(s, fields, None, format)
    return s.getvalue()

@pytest.mark.keywords
def test_comment():
    fields = [Field("a", int, 0, 10, 1), Field("b", str, 10, 10, "hello")]
    result = _get_comment_line(fields)
    assert result == "$#       a         b"


@pytest.mark.keywords
def test_comment_with_gap():
    """test writing comment line with a gap"""
    fields = [
        Field("secid", int, 0, 10, None),
        Field("elform", int, 10, 10, 1),
        Field("aet", int, 20, 10, 0),
        Field("cohoff", float, 60, 10, None),
        Field("gaskeit", float, 70, 10, None),
    ]
    result = _get_comment_line(fields)
    assert result == "$#   secid    elform       aet                                  cohoff   gaskeit"


@pytest.mark.keywords
def test_comment_with_offset():
    """test writing comment line with an offset at the beginning"""
    fields = [
        Field("elform", int, 10, 10, 1),
        Field("aet", int, 20, 10, 0),
        Field("cohoff", float, 60, 10, None),
        Field("gaskeit", float, 70, 10, None),
    ]
    result = _get_comment_line(fields)
    assert result == "$#            elform       aet                                  cohoff   gaskeit"


@pytest.mark.keywords
def test_field_values_int_string():
    """test integer and string field values"""
    fields = [Field("a", int, 0, 10, 1), Field("b", str, 10, 10, "hello")]
    result = _get_field_value(fields)
    assert result == "         1hello     "


@pytest.mark.keywords
def test_field_values_int_string_gap():
    """test integer and string field values with a gap"""
    fields = [Field("a", int, 0, 10, 1), Field("b", str, 20, 10, "hello")]
    result = _get_field_value(fields)
    assert result == "         1          hello     "


@pytest.mark.keywords
def test_field_values_int_float_string():
    fields = [Field("a", int, 0, 10, 1), Field("b", float, 10, 10, 2.0), Field("c", str, 20, 10, "hello")]
    result = _get_field_value(fields)
    assert result == "         1       2.0hello     "


@pytest.mark.keywords
def test_field_values_with_nan():
    fields = [Field("a", int, 0, 10, 1), Field("b", float, 10, 10, float("nan")), Field("c", str, 20, 10, "hello")]
    result = _get_field_value(fields)
    assert result == "         1          hello     "


@pytest.mark.keywords
def test_field_overriden_values():
    fields = [Field("a", int, 0, 10, 1), Field("b", float, 10, 10, float("nan")), Field("c", str, 20, 10, "hello")]
    s = io.StringIO()
    values = [12, 2.2109, "bye"]
    field_writer.write_fields(s, fields, values)
    result = s.getvalue()
    assert result == "        12    2.2109bye       "


@pytest.mark.keywords
def test_field_overriden_values_with_gap():
    fields = [Field("a", int, 0, 10, 1), Field("c", str, 20, 10, "hello")]
    s = io.StringIO()
    values = [12, "bye"]
    field_writer.write_fields(s, fields, values)
    result = s.getvalue()
    assert result == "        12          bye       "


@pytest.mark.keywords
def test_field_values_int_string_long():
    """test long format for integer and string field values"""
    fields = [Field("a", int, 0, 10, 1), Field("b", str, 10, 10, "hello")]
    result = _get_field_value(fields, True)
    assert result == "                   1hello               "