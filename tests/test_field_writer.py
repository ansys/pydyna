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
import typing

from ansys.dyna.core.lib.field import Field, Flag
import ansys.dyna.core.lib.field_writer as field_writer
from ansys.dyna.core.lib.format_type import format_type


import pytest


@dataclasses.dataclass
class bi:
    foo: float = None
    bar: float = None


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
def test_write_field_flag():
    """Test writing flags"""
    mpp = True
    fields = [Field("a", int, 0, 10, 1), Field("b", str, 10, 10, Flag(mpp, "&", ""))]
    assert fields[1].value is True
    result = _get_field_value(fields)
    assert result == "         1&         "
    fields[1].value = False
    result = _get_field_value(fields)
    assert result == "         1          "


@pytest.mark.keywords
def test_comment():
    """Test writing the comment line."""
    fields = [Field("a", int, 0, 10, 1), Field("b", str, 10, 10, "hello")]
    result = _get_comment_line(fields)
    assert result == "$#       a         b"


@pytest.mark.keywords
def test_comment_struct():
    """Test writing the comment line for a struct type."""
    fields = [Field("a", bi, 0, 10, None), Field("a", bi, 20, 10, None)]
    result = _get_comment_line(fields)
    assert result == "$#     foo       bar       foo       bar"


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
def test_field_values_struct_single():
    """Test writing fields with a single struct value type"""

    # single value
    bi1 = bi(1.0, 2.0)
    fields = [Field("a", bi, 0, 10, bi1)]
    result = _get_field_value(fields)
    assert result == "       1.0       2.0"


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
def test_field_write_struct_multiple_missing_value():
    """Test writing fields with multiple values of struct type with a missing value."""
    bi1, bi2 = bi(1.0, 2.0), bi(1.3, math.nan)
    fields = [Field("a", bi, 0, 10, bi1), Field("a", bi, 20, 10, bi2)]
    result = _get_field_value(fields)
    assert result == "       1.0       2.0       1.3          "


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


@pytest.mark.keywords
def test_field_write_struct_multiple():
    """Test writing fields with multiple values of struct type."""
    bi1, bi2 = bi(1.0, 2.0), bi(1.3, 2.9)
    fields = [Field("a", bi, 0, 10, bi1), Field("a", bi, 20, 10, bi2)]
    result = _get_field_value(fields)
    assert result == "       1.0       2.0       1.3       2.9"


@pytest.mark.keywords
def test_field_write_struct_overridden_value():
    # overriden struct values
    s = io.StringIO()
    values = [bi(1.8, math.nan), bi(0.1, 3.14)]
    fields = [Field("a", bi, 0, 10, None), Field("a", bi, 20, 10, None)]
    field_writer.write_fields(s, fields, values)
    result = s.getvalue()
    assert result == "       1.8                 0.1      3.14"


@pytest.mark.keywords
def test_field_write_struct_comment():
    # comment line
    fields = [Field("a", bi, 0, 10, None), Field("a", bi, 20, 10, None)]
    result = _get_comment_line(fields)
    assert result == "$#     foo       bar       foo       bar"


@pytest.mark.keywords
class TestLegacyFloatFormat:
    """Tests for legacy float formatting functionality."""

    def test_format_legacy_float_large_value(self):
        """Test legacy format for large values requiring scientific notation."""
        result = field_writer._format_legacy_float(1e8, 10)
        assert result == "1.000000E8"

    def test_format_legacy_float_very_large_value(self):
        """Test legacy format for very large values."""
        result = field_writer._format_legacy_float(1e12, 10)
        assert result == "1.00000E12"

    def test_format_legacy_float_small_value_uses_fixed(self):
        """Test that normal-range floats use fixed-point notation."""
        result = field_writer._format_legacy_float(1.5, 10)
        assert result == "       1.5"

    def test_format_legacy_float_zero(self):
        """Test that zero uses fixed-point notation."""
        result = field_writer._format_legacy_float(0.0, 10)
        assert result == "       0.0"

    def test_format_legacy_float_small_decimal(self):
        """Test small decimal values use fixed-point notation."""
        result = field_writer._format_legacy_float(0.06, 10)
        assert result == "      0.06"

    def test_format_legacy_float_very_small_value(self):
        """Test very small values use scientific notation."""
        result = field_writer._format_legacy_float(1e-8, 10)
        assert result == "1.00000E-8"

    def test_format_legacy_float_negative_large(self):
        """Test negative large values."""
        result = field_writer._format_legacy_float(-1e8, 10)
        # Negative values need more space, may have less precision
        assert "E8" in result or "E+8" in result

    def test_legacy_float_context_manager(self):
        """Test that the legacy_float_format context manager affects write_field_c."""
        from ansys.dyna.core.lib.config import legacy_float_format, use_legacy_float_format

        # Default should be False
        assert use_legacy_float_format() is False

        # Inside context manager should be True
        with legacy_float_format():
            assert use_legacy_float_format() is True

        # After context manager should be False again
        assert use_legacy_float_format() is False

    def test_write_field_c_default_format(self):
        """Test write_field_c uses hollerith format by default."""
        buf = io.StringIO()
        field_writer.write_field_c(buf, float, 1e8, 10)
        result = buf.getvalue()
        # Default format uses hollerith which produces "1e+08" style
        assert result == "     1e+08"

    def test_write_field_c_legacy_format(self):
        """Test write_field_c uses legacy format when context manager is active."""
        from ansys.dyna.core.lib.config import legacy_float_format

        buf = io.StringIO()
        with legacy_float_format():
            field_writer.write_field_c(buf, float, 1e8, 10)
        result = buf.getvalue()
        # Legacy format produces "1.000000E8" style
        assert result == "1.000000E8"

    def test_write_fields_legacy_format(self):
        """Test that write_fields respects legacy float format."""
        from ansys.dyna.core.lib.config import legacy_float_format

        fields = [
            Field("a", float, 0, 10, 0.0),
            Field("b", float, 10, 10, 1e8),
        ]

        # Default format
        buf1 = io.StringIO()
        field_writer.write_fields(buf1, fields)
        default_result = buf1.getvalue()
        assert "1e+08" in default_result

        # Legacy format
        buf2 = io.StringIO()
        with legacy_float_format():
            field_writer.write_fields(buf2, fields)
        legacy_result = buf2.getvalue()
        assert "1.000000E8" in legacy_result

    def test_legacy_format_preserves_normal_floats(self):
        """Test that legacy format doesn't change normal-range floats."""
        from ansys.dyna.core.lib.config import legacy_float_format

        fields = [
            Field("a", float, 0, 10, 1.5),
            Field("b", float, 10, 10, 0.06),
        ]

        # Both formats should produce same output for normal floats
        buf1 = io.StringIO()
        field_writer.write_fields(buf1, fields)
        default_result = buf1.getvalue()

        buf2 = io.StringIO()
        with legacy_float_format():
            field_writer.write_fields(buf2, fields)
        legacy_result = buf2.getvalue()

        assert default_result == legacy_result
        assert "1.5" in default_result
        assert "0.06" in default_result