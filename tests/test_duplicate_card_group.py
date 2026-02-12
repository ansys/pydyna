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

import pandas as pd

from ansys.dyna.core.lib.card import Card, Field
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card_group import TableCard, TableCardGroup
from ansys.dyna.core.lib.field_writer import write_fields
from ansys.dyna.core.lib.format_type import format_type

import pytest


def _get_test_table_card_group(bounded: bool, default_size: int=2, name = None, **kwargs) -> TableCardGroup:
    if bounded:
        lengthfunc = lambda: default_size
    else:
        lengthfunc = None
    return TableCardGroup(
        [
            (
                FieldSchema("eid", int, 0, 8, None),
                FieldSchema("pid", int, 8, 8, None),
                FieldSchema("n1", int, 16, 8, None),
                FieldSchema("n2", int, 24, 8, None),
                FieldSchema("n3", int, 32, 8, None),
                FieldSchema("n4", int, 40, 8, None),
                FieldSchema("n5", int, 48, 8, None),
                FieldSchema("n6", int, 56, 8, None),
                FieldSchema("n7", int, 64, 8, None),
                FieldSchema("n8", int, 72, 8, None),
            ),
            (
                FieldSchema("a1", float, 0, 16, None),
                FieldSchema("a2", float, 16, 16, None),
                FieldSchema("a3", float, 32, 16, None),
            ),
            (
                FieldSchema("d1", float, 0, 16, None),
                FieldSchema("d2", float, 16, 16, None),
                FieldSchema("d3", float, 32, 16, None),
            ),
        ],
        lengthfunc,
        name=name,
        **kwargs
    )


def _get_row_data(dcg: TableCardGroup, index: int) -> str:
    which_card_index = dcg._get_index_of_which_card(index)
    card_index = dcg._get_index_of_given_card(index)
    card = dcg._get_active_cards()[which_card_index]

    def _get_card_row_data(card: TableCard, index: int, format: format_type) -> str:
        """this is not optimized - used only by test cases"""
        values = card._get_row_values(index)
        s = io.StringIO()
        write_fields(s, card._fields, values, format)
        return s.getvalue()

    return _get_card_row_data(card, card_index, dcg.format)



def test_table_card_group_bounded_empty():
    """test bounded table group"""
    d = _get_test_table_card_group(True)
    # the length is 6 even before data is assigned
    assert d._num_rows() == 6

    # row data are just blank lines
    assert _get_row_data(d, 0) == "                                                                                "
    assert _get_row_data(d, 1) == "                                                "
    assert _get_row_data(d, 2) == "                                                "
    assert _get_row_data(d, 3) == "                                                                                "
    assert _get_row_data(d, 4) == "                                                "
    assert _get_row_data(d, 5) == "                                                "



def test_table_card_group_unbounded_read(string_utils):
    """test reading row data into table group"""
    d = _get_test_table_card_group(False)
    card_text = """       1       2       1       2       3       4       5       6       7       8
             0.1             0.2             0.3
             0.3             0.4             0.5
       1       2       5       6       7       8       1       3       2       4
             0.2             0.3             0.4
             0.4             0.5             0.6"""
    d.read(string_utils.as_buffer(card_text))
    tables = [card.table for card in d._cards]
    for table in tables:
        assert len(table) == 2

    assert tables[0]["eid"][0] == 1
    assert tables[0]["eid"][1] == 1
    assert tables[0]["n1"][0] == 1
    assert tables[0]["n8"][0] == 8
    assert tables[0]["n4"][1] == 8
    table0_repr = """   eid  pid  n1  n2  n3  n4  n5  n6  n7  n8
0    1    2   1   2   3   4   5   6   7   8
1    1    2   5   6   7   8   1   3   2   4"""
    assert repr(tables[0]) == table0_repr

    assert d._num_rows() == 6



def test_table_card_group_unbounded_empty():
    """test unbounded table group"""
    d = _get_test_table_card_group(False)
    d.table = pd.DataFrame(
        {
            "eid": [1, 1],
            "pid": [2, 2],
            "n1": [1, 5],
            "n2": [2, 6],
            "n3": [3, 7],
            "n4": [4, 8],
            "n5": [5, 1],
            "n6": [6, 3],
            "n7": [7, 2],
            "n8": [8, 4],
            "a1": [0.1, 0.2],
            "a2": [0.2, 0.3],
            "a3": [0.3, 0.4],
            "d1": [0.3, 0.4],
            "d2": [0.4, 0.5],
            "d3": [0.5, 0.6],
        }
    )

    # there are 2 in each card, so the total length should be 6
    assert d._num_rows() == 6



def test_table_card_group_unbounded_read_long(string_utils, ref_string):
    """test reading long row data into table group"""
    d = _get_test_table_card_group(False)
    card_text = ref_string.test_table_card_group_long
    d.format = format_type.long
    d.read(string_utils.as_buffer(card_text))
    tables = [card.table for card in d._cards]
    for table in tables:
        assert len(table) == 2

    assert tables[0]["eid"][0] == 1
    assert tables[0]["eid"][1] == 1
    assert tables[0]["n1"][0] == 1
    assert tables[0]["n8"][0] == 8
    assert tables[0]["n4"][1] == 8
    table0_repr = """   eid  pid  n1  n2  n3  n4  n5  n6  n7  n8
0    1    2   1   2   3   4   5   6   7   8
1    1    2   5   6   7   8   1   3   2   4"""
    assert repr(tables[0]) == table0_repr

    assert d._num_rows() == 6



def test_table_card_group_assign_column():
    d = _get_test_table_card_group(False)
    d.table = pd.DataFrame(
        {
            "eid": [1, 1],
            "pid": [2, 2],
            "n1": [1, 5],
            "n2": [2, 6],
            "n3": [3, 7],
            "n4": [4, 8],
            "n5": [5, 1],
            "n6": [6, 3],
            "n7": [7, 2],
            "n8": [8, 4],
            "a1": [0.1, 0.2],
            "a2": [0.2, 0.3],
            "a3": [0.3, 0.4],
            "d1": [0.3, 0.4],
            "d2": [0.4, 0.5],
            "d3": [0.5, 0.6],
        }
    )
    assert d.table["a1"][0] == 0.1
    d.table.loc[0, "a1"] = 0.2
    assert d.table["a1"][0] == 0.2



def test_write_inactive_table_card_group():
    card = _get_test_table_card_group(False)
    card._active_func = lambda: False
    assert card.write() == ""
    card = _get_test_table_card_group(True)
    card._active_func = lambda: False
    assert card.write() == ""



def test_write_empty_table_card_group():
    d = _get_test_table_card_group(False)
    assert d.write() == ""



def test_table_card_group_init_data_table():
    df = pd.DataFrame(
        {
            "eid": [1, 1],
            "pid": [2, 2],
            "n1": [1, 5],
            "n2": [2, 6],
            "n3": [3, 7],
            "n4": [4, 8],
            "n5": [5, 1],
            "n6": [6, 3],
            "n7": [7, 2],
            "n8": [8, 4],
            "a1": [0.1, 0.2],
            "a2": [0.2, 0.3],
            "a3": [0.3, 0.4],
            "d1": [0.3, 0.4],
            "d2": [0.4, 0.5],
            "d3": [0.5, 0.6],
        }
    )

    data = {
        "foo": df
    }
    card = _get_test_table_card_group(False, name="foo", **data)
    table = card.table
    assert (len(table)) == 2
    for column in ["eid", "n7", "a1", "d2"]:
        assert len(df[column]) == len(table[column]), f"Length of {column} column doesn't match"
        assert len(df[column].compare(table[column])) == 0, f"{column} column values don't match"


def test_table_card_group_init_data_scalar():

    def _verify_dataframe(df):
        assert (len(df)) == 1
        assert df["eid"][0] == 1
        assert df["n7"][0] == 3
        assert df["d2"][0] == -0.3
        assert pd.isna(df["a1"][0])

    data = {
        "eid": 1,
        "n7": 3,
        "d2": -0.3
    }

    # bounded with a length of 0
    card = _get_test_table_card_group(True, default_size=0, **data)
    assert len(card.table) == 0

    # bounded with a length of 1
    card = _get_test_table_card_group(True, default_size=1, **data)

    _verify_dataframe(card.table)

    # unbounded
    card = _get_test_table_card_group(False, **data)

    _verify_dataframe(card.table)
