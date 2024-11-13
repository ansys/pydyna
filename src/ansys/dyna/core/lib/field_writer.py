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

import hollerith as holler
import pandas as pd
import pandas._libs.missing as libmissing

from ansys.dyna.core.lib.field import Field, to_long
from ansys.dyna.core.lib.format_type import format_type


def _write_string_right(value, width):
    # TODO - put a left/right flag in hollerith
    return f"{{0:>{width}}}".format(value)


def _write_null(width):
    buffer = io.StringIO()
    holler.write_spaces(buffer, width)
    return buffer.getvalue()


def _field_iterator(fields: typing.List[Field], long_format: bool) -> typing.Iterable[Field]:
    assert len(fields) > 0, "at least one field is needed"
    if fields[0].offset > 0:
        # insert a blank field in the beginning up to the offset
        blank_field = Field(name=None, type=None, offset=0, width=fields[0].offset)
        fields = [blank_field] + fields

    offset = 0
    for field in fields:
        if long_format:
            field = to_long(field, offset)
        pos, width = (field.offset, field.width)
        # check pos, add a null if its not correct
        assert pos >= offset
        if pos != offset:
            empty_width = pos - offset
            yield Field(name=None, type=None, offset=offset, width=empty_width)
            offset += empty_width
        assert pos == offset
        yield field
        offset += width


def check_field_type(field_type: type):
    assert field_type == str or field_type == int or field_type == float, "Unexpected type"


def write_field_c(buf: typing.IO[typing.AnyStr], field_type: type, value: typing.Any, width: int) -> None:
    if libmissing.checknull(value):
        holler.write_spaces(buf, width)
    elif field_type == str:
        holler.write_string(buf, value, width)
    elif field_type == int:
        holler.write_int(buf, value, width)
    elif field_type == float:
        holler.write_float(buf, value, width)


def write_field(buf: typing.IO[typing.AnyStr], field_type: type, value: typing.Any, width: int) -> None:
    check_field_type(field_type)
    write_field_c(buf, field_type, value, width)


def write_c_dataframe(
    buf: typing.IO[typing.AnyStr], fields: typing.List[Field], table: pd.DataFrame, format: format_type
) -> None:
    def convert_field(field: Field) -> holler.Field:
        return holler.Field(type=field.type, width=field.width)

    long_format = format == format_type.long
    converted_fields = list(_field_iterator(fields, long_format))
    spec = [convert_field(field) for field in converted_fields]
    num_defined_rows = len(table)
    index = 0
    any_none = False
    for field in converted_fields:
        if field.type == None:
            any_none = True
    if any_none:
        full_table = pd.DataFrame()
        for field in converted_fields:
            if field.type == None:
                values = [None] * num_defined_rows
                index += 1
                field.name = f"unused {index}"
            else:
                values = table[field.name]
            full_table[field.name] = values
        table = full_table
    holler.write_table(buf, table, num_defined_rows, spec)


def write_fields(
    buf: typing.IO[typing.AnyStr],
    fields: typing.List[Field],
    values: typing.Optional[typing.List[typing.Any]] = None,
    format: typing.Optional[format_type] = format_type.default,
) -> None:
    """Write `fields` representing a line of a keyword to `buf`.

    Use the fixed column offsets and width

    Parameters
    ----------
    buf: IO
        buffer to write to
    fields: List
        fields to write
    values: List
        optional - list of values for the field. If not set, use the value property of each field.
        used by DuplicateCard
    format: format_type
        optional - format to write

    >>> s=io.String()
    >>> fields = [
    ...     Field("a", int, 0, 10, 1),
    ...     Field("b", str, 10, 10, "hello")
    ... ]
    >>> write_fields(s, fields)
    >>> s.getvalue()
    '         1     hello'
    """
    index = 0

    for field in _field_iterator(fields, format == format_type.long):
        if field.type is None:
            buf.write(_write_null(field.width))
        else:
            field_value, field_type = field.io_info()
            if values != None:
                field_value = values[index]
                index += 1
            write_field(buf, field_type, field_value, field.width)


def write_comment_line(
    buf: typing.IO[typing.AnyStr],
    fields: typing.List[Field],
    format: typing.Optional[format_type] = format_type.default,
) -> None:
    """Writes the comment line to the buffer.

    Parameters
    ----------
    buf: IO
        buffer to write to
    fields: List
        fields to write
    format: format_type
        format to write in

    >>> s=io.String()
    >>> fields = [
    ...     Field("a", int, 0, 10, 1),
    ...     Field("b", str, 10, 10, "hello")
    ... ]
    >>> write_comment_line(s, fields)
    >>> s.getvalue()
    '         a         b'
    """
    pos = buf.tell()
    for field in _field_iterator(fields, format == format_type.long):
        if field.name is None:
            buf.write(_write_null(field.width))
        else:
            buf.write(_write_string_right(field.name, field.width))
    endpos = buf.tell()
    buf.seek(pos)
    buf.write("$#")
    buf.seek(endpos)
