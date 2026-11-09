# Copyright (C) 2023 - 2026 Synopsys, Inc. and ANSYS, Inc. All rights reserved.
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

"""Module for handling table cards."""

import io
import typing

import numpy as np
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field
from ansys.dyna.core.lib.card_interface import ReadResult
from ansys.dyna.core.lib.field_schema import CardSchema, FieldSchema
from ansys.dyna.core.lib.field_writer import write_c_dataframe
from ansys.dyna.core.lib.format_type import card_format, format_type
from ansys.dyna.core.lib.io_utils import is_dataframe, write_or_return
from ansys.dyna.core.lib.kwd_line_formatter import buffer_to_lines
from ansys.dyna.core.lib.parameters import ParameterSet

CHECK_TYPE = True


def _check_type(value):
    global CHECK_TYPE
    if CHECK_TYPE:
        if not is_dataframe(value):
            raise TypeError("value must be a DataFrame")


def try_initialize_table(card, name: str, **kwargs):
    """Card is a TableCard or a TableCardGroup"""
    if name is not None:
        data = kwargs.get(name, None)
        if data is not None:
            card.table = data
            return True
    return False


def _get_initial_columns(names: typing.Iterable[str], **kwargs) -> typing.Dict[str, typing.Any]:
    """Get the column values given in the constructor kwargs, keyed by column name."""
    return {name: kwargs[name] for name in names if name in kwargs}


def _is_scalar(value: typing.Any) -> bool:
    return not isinstance(value, pd.Series) and np.ndim(value) == 0


def _initial_length(columns: typing.Dict[str, typing.Any]) -> int:
    """Return the number of rows needed to hold the initial column values of an unbounded table.

    A scalar needs one row, an array-like needs one row per value.
    """
    return max((1 if _is_scalar(value) else len(value) for value in columns.values()), default=0)


def _column_dtype(field_type: type):
    if field_type == float:
        return np.float64
    if field_type == int:
        return pd.Int32Dtype()
    return object


def _cast_column(column: pd.Series, field_type: type) -> pd.Series:
    """Cast ``column`` to the dtype of ``field_type``.

    A string column holds ``str`` values, with NaN for missing values.
    """
    if field_type == str:
        return column.astype(str).astype(object).where(column.notna(), np.nan)
    return column.astype(_column_dtype(field_type))


def _field_default(field: Field) -> typing.Optional[typing.Any]:
    if field.value is not None:
        return field.value
    if field.type in (float, str):
        return np.nan
    return None


def _grow_table(table: pd.DataFrame, fields: typing.List[Field], length: int) -> pd.DataFrame:
    """Return ``table`` grown to ``length`` rows, with the schema defaults in the new rows."""
    num_rows = len(table)
    if length <= num_rows:
        return table
    fields_by_name = {}
    for field in fields:
        fields_by_name.setdefault(field.name, field)
    columns = {}
    for name in table.columns:
        field = fields_by_name[name]
        dtype = _column_dtype(field.type)
        new_rows = pd.Series([_field_default(field)] * (length - num_rows), dtype=dtype)
        columns[name] = pd.concat([table[name].astype(dtype), new_rows], ignore_index=True)
    return pd.DataFrame(columns)


def _pad_column(column: pd.Series, length: int, dtype) -> pd.Series:
    """Return ``column`` padded with missing values to ``length``."""
    missing = length - len(column)
    if missing <= 0:
        return column
    return pd.concat([column, pd.Series([np.nan] * missing, dtype=dtype)], ignore_index=True)


def _set_column(
    table: pd.DataFrame, fields: typing.List[Field], name: str, value: typing.Any, bounded: bool
) -> pd.DataFrame:
    """Return ``table`` with column ``name`` set to ``value``.

    A scalar is copied to every row; an unbounded table with no rows gets one row.
    An array-like is assigned by position. If it is shorter than the table, the remaining
    rows are missing; if it is longer, the table grows, unless it is bounded.
    A Series must have a default index (0, 1, ..., n-1).
    The values are cast to the dtype of the column's schema.
    """
    field = next((f for f in fields if f.name == name), None)
    if field is None:
        raise KeyError(f"'{name}' is not a column of the table.")
    dtype = _column_dtype(field.type)
    num_rows = len(table)
    if _is_scalar(value):
        length = num_rows if bounded or num_rows > 0 else 1
        column = _cast_column(pd.Series([value] * length, dtype=object), field.type)
    else:
        if isinstance(value, pd.Series):
            if not value.index.equals(pd.RangeIndex(len(value))):
                raise ValueError(f"The index of the Series for '{name}' must be a default index 0, 1, ..., n-1.")
            value = value.array
        column = _cast_column(pd.Series(value, dtype=dtype), field.type)
        length = max(num_rows, len(column))
        if bounded and length > num_rows:
            raise ValueError(f"Cannot set '{name}' with {len(column)} values: the table is bounded to {num_rows} rows.")
        column = _pad_column(column, length, dtype)
    table = _grow_table(table, fields, length)
    column.index = table.index
    table[name] = column
    return table


class TableCard(Card):
    def __init__(
        self,
        fields: typing.List[Field],
        length_func,
        active_func: typing.Optional[typing.Callable] = None,
        name: str = None,
        format: format_type = format_type.default,
        **kwargs,
    ):
        # Inline Card initialization (avoid deprecated constructor path)
        field_schemas = tuple(FieldSchema.from_field(f) for f in fields)
        name_to_index = {f.name: i for i, f in enumerate(fields)}
        self._schema = CardSchema(field_schemas, name_to_index)
        self._signature = id(self._schema)
        self._values = [f.value for f in fields]
        self._active_func = active_func
        self._format_type = format
        self._card_format = card_format.fixed

        # TableCard-specific initialization
        self._format = [(field.offset, field.width) for field in self._fields]
        if length_func == None:
            self._bounded = False
            self._length_func = lambda: len(self.table)
        else:
            self._bounded = True
            self._length_func = length_func

        self._format_type = format
        self._initial_columns = {}
        self._initialized = try_initialize_table(self, name, **kwargs)
        if not self._initialized:
            self._initial_columns = _get_initial_columns((f.name for f in fields), **kwargs)

    @classmethod
    def from_field_schemas(
        cls,
        field_schemas: typing.Tuple[FieldSchema, ...],
        length_func: typing.Callable,
        active_func: typing.Optional[typing.Callable] = None,
        name: str = None,
        format: format_type = format_type.default,
        **kwargs,
    ) -> "TableCard":
        """Create a TableCard from FieldSchema tuples.

        This is the preferred way to create TableCard instances as it avoids
        creating intermediate Field objects.

        Parameters
        ----------
        field_schemas : Tuple[FieldSchema, ...]
            Tuple of FieldSchema objects defining the card structure.
        length_func : callable
            Function returning the number of rows, or None for unbounded.
        active_func : callable, optional
            Function returning bool to determine if card is active.
        name : str, optional
            Name for table initialization from kwargs.
        format : format_type, optional
            The format type (default, standard, or long).

        Returns
        -------
        TableCard
            A new TableCard instance.
        """
        instance = cls.__new__(cls)
        name_to_index = {fs.name: i for i, fs in enumerate(field_schemas)}
        instance._schema = CardSchema(field_schemas, name_to_index)
        instance._signature = id(instance._schema)
        instance._values = [fs.default if not fs.is_flag() else fs.default.value for fs in field_schemas]
        instance._active_func = active_func
        instance._format_type = format
        instance._card_format = card_format.fixed

        # TableCard-specific initialization
        instance._format = [(fs.offset, fs.width) for fs in field_schemas]
        if length_func is None:
            instance._bounded = False
            instance._length_func = lambda: len(instance.table)
        else:
            instance._bounded = True
            instance._length_func = length_func

        instance._initial_columns = {}
        instance._initialized = try_initialize_table(instance, name, **kwargs)
        if not instance._initialized:
            instance._initial_columns = _get_initial_columns((fs.name for fs in field_schemas), **kwargs)

        return instance

    def _initialize(self, check: bool = False):
        """Lazy initialization routine for the data frame.

        This can not be done in the constructor because the length function
        might be unusable until after the keyword is fully initialized.

        If the card is bounded, the table is initialized with the bounded length.
        Otherwise, it is initialized with as many rows as the longest column value
        given in the constructor **kwargs, counting a scalar as one row.
        The column values are then set as if by ``set_column``.
        """
        if self._bounded:
            length = self._length_func()
        else:
            length = _initial_length(self._initial_columns)
        self._initialize_data(length)
        self._initialized = True
        initial_columns, self._initial_columns = self._initial_columns, {}
        for name, value in initial_columns.items():
            self.set_column(name, value)

    def get_column(self, name: str) -> pd.Series:
        """Get the column ``name`` of the table."""
        return self.table[name]

    def set_column(self, name: str, value: typing.Any) -> None:
        """Set the column ``name`` of the table from a scalar, a Series, or an array-like.

        A scalar is copied to every row; an unbounded table with no rows gets one row.
        An array-like is assigned by position. If it is shorter than the table, the remaining
        rows are missing; if it is longer, the table grows, with the schema defaults in the
        other columns of the new rows. A bounded table can not grow.
        A Series must have a default index (0, 1, ..., n-1).
        """
        self._table = _set_column(self.table, self._fields, name, value, self._bounded)

    @property
    def table(self) -> pd.DataFrame:
        """Get the table as a pandas DataFrame."""
        if not self._initialized:
            self._initialize()
        return self._table

    @table.setter
    def table(self, value: pd.DataFrame):
        """Set the table from a pandas DataFrame."""
        _check_type(value)
        # Build columns dict first, then create DataFrame (pandas 2.3+ compatibility)
        columns = {}
        for field in self._fields:
            if field.name in value:
                columns[field.name] = _cast_column(value[field.name], field.type)
            else:
                columns[field.name] = self._make_column(field, len(value))
        self._table = pd.DataFrame(columns).reset_index(drop=True)
        self._initialized = True

    @property
    def format(self) -> format_type:
        """Format type of the table card."""
        return self._format_type

    @format.setter
    def format(self, value: format_type) -> None:
        """Set the format type of the table card."""
        self._format_type = value

    def _get_default_value(self, field: Field) -> typing.Optional[typing.Any]:
        return _field_default(field)

    def _make_column(self, field, length):
        default_value = self._get_default_value(field)
        if field.type == float:
            arr = np.empty((length,))
            arr[:] = default_value
            return arr
        elif field.type == str:
            return [default_value] * length
        elif field.type == int:
            return pd.Series([default_value] * length, dtype=pd.Int32Dtype())
        raise Exception("unexpected type")

    def _initialize_data(self, length):
        data = {}
        num_fields = len(self._fields)
        column_names = np.ndarray(num_fields, "object")
        for index in range(num_fields):
            field = self._fields[index]
            value = self._make_column(field, length)
            column_names[index] = field.name
            data[field.name] = value
        self._table = pd.DataFrame(data, columns=column_names)

    def _get_row_values(self, index: int) -> list:
        # Used by Duplicate Card Group only
        if index >= len(self.table):
            return [None] * len(self._fields)
        values = []
        for key in self.table.keys():
            col = self.table[key]
            val = col[index]
            values.append(val)
        return values

    def _get_read_options(self):
        fields = self._get_fields()
        colspecs = [(field.offset, field.offset + field.width) for field in fields]
        type_mapping = {float: np.float64, int: pd.Int32Dtype(), str: str}
        dtype = {field.name: type_mapping[field.type] for field in fields}
        names = [field.name for field in fields]
        # Comment lines are already filtered by buffer_to_lines/read_line before
        # data reaches this point, so we can skip pandas comment checking entirely.
        # This provides ~30-40% speedup for large files (see GitHub issue #592)
        options = {"names": names, "colspecs": colspecs, "dtype": dtype, "comment": None}
        return options

    def _read_buffer_as_dataframe(
        self,
        buffer: typing.TextIO,
        fields: typing.Iterable[Field],
        parameter_set: ParameterSet,
    ) -> pd.DataFrame:
        read_options = self._get_read_options()
        df = pd.read_fwf(buffer, **read_options)
        return df

    def _get_fields(self) -> typing.List[Field]:
        fields = self._fields
        if self.format == format_type.long:
            fields = self._convert_fields_to_long_format()
        return fields

    def _load_bounded_from_buffer(self, buf: typing.TextIO, parameter_set: ParameterSet) -> typing.List[str]:
        """Load bounded table data from buffer.

        Returns
        -------
        list of str
            Warning messages generated during parsing.
        """
        # For bounded cards, read all lines and use same logic as unbounded
        data_lines = buffer_to_lines(buf, self._num_rows())
        return self._load_lines(data_lines, parameter_set)

    def _load_unbounded_from_buffer(self, buf: typing.TextIO, parameter_set: ParameterSet) -> typing.List[str]:
        """Load unbounded table data from buffer.

        Returns
        -------
        list of str
            Warning messages generated during parsing.
        """
        data_lines = buffer_to_lines(buf)
        return self._load_lines(data_lines, parameter_set)

    def read(self, buf: typing.TextIO, parameter_set: ParameterSet = None) -> ReadResult:
        """Read the table card content from a buffer.

        Returns
        -------
        ReadResult
            Result containing warnings generated during parsing.
        """
        if parameter_set is None:
            parameter_set = ParameterSet()
        result = ReadResult()
        if self.bounded:
            self._initialized = True
            warnings_list = self._load_bounded_from_buffer(buf, parameter_set)
            result.warnings.extend(warnings_list)
        else:
            self._initialize_data(0)
            self._initialized = True
            warnings_list = self._load_unbounded_from_buffer(buf, parameter_set)
            result.warnings.extend(warnings_list)
        return result

    def _has_parameters(self, data_lines: typing.List[str]) -> bool:
        """Check if any data lines contain parameter references."""
        return any("&" in line for line in data_lines)

    def _has_csv_format(self, data_lines: typing.List[str]) -> bool:
        """Check if any data lines use comma-delimited format.

        Uses the same detection logic as load_dataline to avoid false positives
        on lines that contain commas but aren't CSV format (e.g., expressions
        like min(1,2,3) in PARAMETER_EXPRESSION).
        """
        from ansys.dyna.core.lib.kwd_line_formatter import _is_comma_delimited

        num_fields = len(self._fields)
        return any(_is_comma_delimited(line, num_fields) for line in data_lines)

    def _load_lines_with_parameters(
        self,
        data_lines: typing.List[str],
        parameter_set: ParameterSet,
        row_indices: typing.Optional[typing.Sequence[int]] = None,
    ) -> typing.List[str]:
        """Load lines using load_dataline for parameter substitution.

        Used when a ParameterSet is provided and data contains parameter refs or CSV format.
        Parameter refs are recorded under ``row_indices[i]`` for line ``i``, or under ``i``
        when ``row_indices`` is not given.
        """
        from ansys.dyna.core.lib.kwd_line_formatter import load_dataline

        fields = self._get_fields()
        format_spec = [(f.offset, f.width, f.type) for f in fields]

        rows = []
        all_warnings = []
        for position, line in enumerate(data_lines):
            row_index = position if row_indices is None else row_indices[position]
            with parameter_set.scope(f"row{row_index}"):
                values, line_warnings = load_dataline(format_spec, line, parameter_set)
            all_warnings.extend(line_warnings)
            row_dict = {field.name: value for field, value in zip(fields, values)}
            rows.append(row_dict)

        self._table = pd.DataFrame(rows)
        self._initialized = True
        return all_warnings

    def _load_lines(
        self,
        data_lines: typing.List[str],
        parameter_set: ParameterSet,
        row_indices: typing.Optional[typing.Sequence[int]] = None,
    ) -> typing.List[str]:
        """Load data lines into the table with parameter substitution.

        ``row_indices`` gives the row that each line belongs to, for recording parameter refs.
        """
        has_params = self._has_parameters(data_lines)
        has_csv = self._has_csv_format(data_lines)

        if has_params or has_csv:
            return self._load_lines_with_parameters(data_lines, parameter_set, row_indices)

        # Fast pandas path for fixed-width, parameter-using but no refs in this data
        fields = self._get_fields()
        buffer = io.StringIO()
        [(buffer.write(line), buffer.write("\n")) for line in data_lines]
        buffer.seek(0)
        self._table = self._read_buffer_as_dataframe(buffer, fields, parameter_set)
        self._initialized = True
        return []

    def write(
        self,
        format: typing.Optional[format_type] = None,
        buf: typing.Optional[typing.TextIO] = None,
        comment: typing.Optional[bool] = True,
        retain_parameters: bool = False,
        parameter_set: ParameterSet = None,
        uri_prefix: str = None,
        **kwargs,
    ) -> str:
        """Write the table card to a string or buffer."""
        if format == None:
            format = self._format_type

        def _write(buf: typing.TextIO):
            if self._num_rows() > 0:
                if comment:
                    buf.write(self._get_comment(format))
                    buf.write("\n")
                table = self.table
                self._write_rows(buf, format, table, range(len(table)), retain_parameters, parameter_set, uri_prefix)

        return write_or_return(buf, _write)

    def _write_rows(
        self,
        buf: typing.TextIO,
        format: format_type,
        table: pd.DataFrame,
        row_indices: typing.Sequence[int],
        retain_parameters: bool = False,
        parameter_set: ParameterSet = None,
        uri_prefix: str = None,
    ) -> None:
        """Write the data lines of ``table``, a subset of this card's columns.

        ``row_indices`` gives the row of each line, under which its parameter refs were recorded.
        """
        if retain_parameters and parameter_set is not None and uri_prefix is not None:
            # Write row-by-row with parameter ref substitution
            self._write_with_parameters(buf, format, table, row_indices, parameter_set, uri_prefix)
        else:
            # Fast path: use holler.write_table
            write_c_dataframe(buf, self._fields, table, format)

    def _write_with_parameters(
        self,
        buf: typing.TextIO,
        format: format_type,
        table: pd.DataFrame,
        row_indices: typing.Sequence[int],
        parameter_set: ParameterSet,
        uri_prefix: str,
    ) -> None:
        """Write table data row-by-row, substituting parameter refs where recorded."""
        from ansys.dyna.core.lib.field_writer import write_fields

        fields = self._get_fields()
        if format == format_type.long:
            fields = self._convert_fields_to_long_format()

        for position, row_index in enumerate(row_indices):
            # Get row values from DataFrame
            row_values = [table[field.name].iloc[position] for field in fields if field.name in table]

            # Check for parameter refs and substitute
            row_fields, row_values = self._substitute_parameter_refs(
                fields, row_values, parameter_set, uri_prefix, row_index
            )

            write_fields(buf, row_fields, row_values, format)
            buf.write("\n")

    def _substitute_parameter_refs(
        self,
        fields: typing.List[Field],
        values: typing.List,
        parameter_set: ParameterSet,
        uri_prefix: str,
        row_index: int,
    ) -> typing.Tuple[typing.List[Field], typing.List]:
        """Substitute parameter references for field values where applicable.

        Creates new Field objects with parameter reference strings as values
        for fields that were originally read from parameters.

        Returns both modified fields and values lists.
        """
        result_fields = []
        result_values = list(values)  # Make a copy
        for i, field in enumerate(fields):
            # Skip None-type fields (unused fields)
            if field.type is None:
                result_fields.append(field)
                continue

            # Look up ref using the URI pattern: uri_prefix/row{row_index}/{field_index}
            # Filter out empty segments to handle empty uri_prefix
            segments = [s for s in [uri_prefix, f"row{row_index}", str(i)] if s]
            ref = parameter_set.get_ref(*segments)
            if ref is not None:
                # Create a new field with the reference string as the value
                # The reference will be written as-is (e.g., "&myvar")
                new_field = Field(field.name, str, field.offset, field.width, ref)
                result_fields.append(new_field)
                # Also update the value to be the ref string
                if i < len(result_values):
                    result_values[i] = ref
            else:
                result_fields.append(field)
        return result_fields, result_values

    @property
    def bounded(self) -> bool:
        """Indicates whether the card is bounded."""
        return self._bounded

    def _num_rows(self) -> int:
        if not self.active:
            return 0
        return self._length_func()

    def __repr__(self) -> str:
        """Returns a console-friendly representation of the desired parameters for the card"""
        content_lines = []
        content_lines.append(self._get_comment(self._format_type))
        output = "\n".join(content_lines)
        return "TableCard: \n" + output
