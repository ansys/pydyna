# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE USE OR OTHER DEALINGS IN THE SOFTWARE.

"""Helpers for parameter-defining keywords that require raw parsing.

Expression keywords (PARAMETER_EXPRESSION, etc.) contain &param references as
expression operands, not parameter substitutions. These helpers load table
and card data without parameter substitution, treating '&' as literal text.
"""

import typing

import numpy as np
import pandas as pd

from ansys.dyna.core.lib.card import Field
from ansys.dyna.core.lib.field_schema import CardSchema
from ansys.dyna.core.lib.kwd_line_formatter import buffer_to_lines, parse_dataline


def load_table_lines_raw(
    data_lines: typing.List[str],
    fields: typing.List[Field],
) -> typing.Tuple[pd.DataFrame, typing.List[str]]:
    """Load table lines without parameter substitution.

    Used by parameter-defining keywords where '&' is literal expression syntax.

    Parameters
    ----------
    data_lines : list of str
        Raw data lines (no comment filtering; caller must handle).
    fields : list of Field
        Field definitions (offset, width, type) for parsing.

    Returns
    -------
    tuple of (DataFrame, list of str)
        Parsed table and any warning messages.
    """
    format_spec = [(f.offset, f.width, f.type) for f in fields]

    rows = []
    all_warnings = []
    for line in data_lines:
        values, line_warnings = parse_dataline(format_spec, line)
        all_warnings.extend(line_warnings)
        row_dict = {}
        for field, value in zip(fields, values):
            # Use np.nan for empty str to match pandas read_fwf behavior
            if field.type is str and value is None:
                row_dict[field.name] = np.nan
            else:
                row_dict[field.name] = value
        rows.append(row_dict)

    return pd.DataFrame(rows), all_warnings


def load_card_line_raw(
    line: str,
    schema: CardSchema,
) -> typing.Tuple[typing.Tuple, typing.List[str]]:
    """Load a single card line without parameter substitution.

    Used by parameter-defining keywords (e.g. PARAMETER_EXPRESSION_NOECHO)
    where '&' is literal expression syntax.

    Parameters
    ----------
    line : str
        Raw data line.
    schema : CardSchema
        Card schema with field definitions.

    Returns
    -------
    tuple of (values tuple, list of str)
        Parsed values in schema order and any warning messages.
    """
    items = []
    for fs in schema.fields:
        if fs.is_flag():
            item_type = fs.default
        else:
            item_type = fs.type
        items.append((fs.offset, fs.width, item_type))

    return parse_dataline(items, line)


def _read_parameter_expression_table(
    buf: typing.TextIO,
    card,
) -> typing.List[str]:
    """Read table data with raw parsing and set card table.

    For keywords with a TableCard (PARAMETER_EXPRESSION, PARAMETER_EXPRESSION_LOCAL).
    """
    data_lines = buffer_to_lines(buf)
    fields = card._get_fields()
    df, warnings = load_table_lines_raw(data_lines, fields)
    card._table = df
    card._initialized = True
    return warnings


def _read_parameter_expression_card(
    buf: typing.TextIO,
    card,
) -> typing.List[str]:
    """Read single card line with raw parsing and set card values.

    For keywords with a single Card (PARAMETER_EXPRESSION_NOECHO).
    """
    from ansys.dyna.core.lib.kwd_line_formatter import read_line

    line, to_exit = read_line(buf)
    if to_exit:
        return []

    values, warnings = load_card_line_raw(line, card._schema)
    for i in range(len(card._schema)):
        card._values[i] = values[i]
    return warnings
