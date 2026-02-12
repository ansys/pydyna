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

import numpy as np
import pandas as pd
import pytest

from ansys.dyna.core.lib.card import Field
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.format_type import format_type



def test_table_card_read_bounded(string_utils):
    """test reading fixed number of lines"""
    d = TableCard(
        [
            Field("nid", int, 0, 8),
            Field("x", float, 8, 16),
            Field("y", float, 24, 16),
            Field("z", float, 40, 16),
            Field("tc", int, 56, 8),
            Field("rc", int, 64, 8),
        ],
        lambda: 3,
    )

    card_text = """ 2000000
 2000001   -2772.1652832     643.8095703     376.7990417
 2000002   -3093.8891602     685.0078125     811.2246704       2       5"""
    d.read(string_utils.as_buffer(card_text))
    table = d.table
    assert len(table) == 3
    node_repr = """       nid            x           y           z    tc    rc
0  2000000          NaN         NaN         NaN  <NA>  <NA>
1  2000001 -2772.165283  643.809570  376.799042  <NA>  <NA>
2  2000002 -3093.889160  685.007812  811.224670     2     5"""
    assert repr(table) == node_repr
    assert table["x"][1] == -2772.1652832
    assert table["rc"][2] == 5



def test_table_card_read_unbounded(string_utils):
    """test reading an unknown number of lines into an unbounded card"""
    d = TableCard(
        [
            Field("nid", int, 0, 8),
            Field("x", float, 8, 16),
            Field("y", float, 24, 16),
            Field("z", float, 40, 16),
            Field("tc", int, 56, 8),
            Field("rc", int, 64, 8),
        ],
        None,
    )
    card_text = """ 2000000
 2000001   -2772.1652832     643.8095703     376.7990417
 2000002   -3093.8891602     685.0078125     811.2246704       2       5"""
    d.read(string_utils.as_buffer(card_text))
    table = d.table
    assert len(table) == 3
    node_repr = """       nid            x           y           z    tc    rc
0  2000000          NaN         NaN         NaN  <NA>  <NA>
1  2000001 -2772.165283  643.809570  376.799042  <NA>  <NA>
2  2000002 -3093.889160  685.007812  811.224670     2     5"""
    assert repr(table) == node_repr
    assert table["x"][1] == -2772.1652832
    assert table["rc"][2] == 5



def test_table_card_assign():
    """test assigning dataframe to table card"""
    d = TableCard(
        [
            Field("nid", int, 0, 8),
            Field("x", float, 8, 16),
            Field("y", float, 24, 16),
            Field("z", float, 40, 16),
            Field("tc", int, 56, 8),
            Field("rc", int, 64, 8, 0.0),
        ],
        None,
    )

    node_ids = np.arange(30) + 1
    xs = np.zeros(30) + 0.1
    ys = np.zeros(30) + 0.2
    zs = np.zeros(30) + 0.3
    df = pd.DataFrame({"nid": node_ids, "x": xs, "y": ys, "z": zs})
    d.table = df  # assign the dataframe
    table = d.table  # get the dataframe, see if the contents match what was assigned
    assert (len(table)) == 30
    for column in ["nid", "x", "y", "z"]:
        assert len(df[column]) == len(table[column]), f"Length of {column} column doesn't match"
        assert len(df[column].compare(table[column])) == 0, f"{column} column values don't match"
    assert table["nid"][4] == 5
    assert table["z"][4] == 0.3
    assert pd.isna(table["tc"][4])
    assert table["rc"][4] == 0.0



def test_table_card_assign_wrong_types():
    """test assigning wrong type as dataframe to table card"""

    def assign():
        d = TableCard(
            [
                Field("nid", int, 0, 8),
                Field("x", float, 8, 16),
                Field("y", float, 24, 16),
                Field("z", float, 40, 16),
                Field("tc", int, 56, 8),
                Field("rc", int, 64, 8),
            ],
            None,
        )
        node_ids = np.arange(30) + 1
        d.table = node_ids

    pytest.raises(TypeError, assign)



def test_table_card_write_long_format(string_utils, ref_string):
    """Test writing a table card with the long format."""
    d = TableCard(
        [
            Field("nid", int, 0, 8),
            Field("x", float, 8, 16),
            Field("y", float, 24, 16),
            Field("z", float, 40, 16),
            Field("tc", int, 56, 8),
            Field("rc", int, 64, 8),
        ],
        None,
    )
    card_text = """ 2000000
 2000001   -2772.1652832     643.8095703     376.7990417
 2000002   -3093.8891602     685.0078125     811.2246704       2       5"""
    d.read(string_utils.as_buffer(card_text))
    d_str = d.write(format=format_type.long)
    assert d_str == ref_string.test_mesh_string_long



def test_table_card_read_long(string_utils, ref_string):
    """Test writing a table card with the long format."""
    d = TableCard(
        [
            Field("nid", int, 0, 8),
            Field("x", float, 8, 16),
            Field("y", float, 24, 16),
            Field("z", float, 40, 16),
            Field("tc", int, 56, 8),
            Field("rc", int, 64, 8),
        ],
        None,
    )
    d.format = format_type.long
    d.read(string_utils.as_buffer(ref_string.test_mesh_string_long))
    table = d.table
    assert len(table) == 3
    assert table["x"][1] == -2772.1652832
    assert pd.isna(table["x"][0])



def test_table_card_write_inactive():
    card = TableCard(
        [
            Field("nid", int, 0, 8),
            Field("x", float, 8, 16),
            Field("y", float, 24, 16),
            Field("z", float, 40, 16),
            Field("tc", int, 56, 8),
            Field("rc", int, 64, 8),
        ],
        None,
        lambda: False,
    )
    assert card.write() == ""



def test_table_card_write_empty():
    card = TableCard(
        [
            Field("nid", int, 0, 8),
            Field("x", float, 8, 16),
            Field("y", float, 24, 16),
            Field("z", float, 40, 16),
            Field("tc", int, 56, 8),
            Field("rc", int, 64, 8),
        ],
        lambda: 0,
        lambda: True,
    )
    assert card.write() == ""


def test_table_card_init_data_table():
    node_ids = np.arange(30) + 1
    xs = np.zeros(30) + 0.1
    ys = np.zeros(30) + 0.2
    zs = np.zeros(30) + 0.3
    df = pd.DataFrame({"nid": node_ids, "x": xs, "y": ys, "z": zs})

    data = {
        "foo": df
    }
    card = TableCard(
        [
            Field("nid", int, 0, 8),
            Field("x", float, 8, 16),
            Field("y", float, 24, 16),
            Field("z", float, 40, 16),
            Field("tc", int, 56, 8),
            Field("rc", int, 64, 8),
        ],
        lambda: 0,
        lambda: True,
        "foo",
        **data
    )
    table = card.table
    assert (len(table)) == 30
    for column in ["nid", "x", "y", "z"]:
        assert len(df[column]) == len(table[column]), f"Length of {column} column doesn't match"
        assert len(df[column].compare(table[column])) == 0, f"{column} column values don't match"



def test_table_card_init_data_scalar():

    def _verify_dataframe(df):
        assert (len(df)) == 1
        assert df["nid"][0] == 1
        assert df["x"][0] == 0.1
        assert pd.isna(df["y"][0])
        assert df["rc"][0] == 0.0

    data = {
        "nid": 1,
        "x": 0.1
    }

    fields = [
        Field("nid", int, 0, 8),
        Field("x", float, 8, 16),
        Field("y", float, 24, 16),
        Field("z", float, 40, 16),
        Field("tc", int, 56, 8),
        Field("rc", int, 64, 8, 0.0),
    ]

    # bounded with a length of 0
    card = TableCard(
        fields,
        lambda: 0,
        lambda: True,
        **data
    )

    assert len(card.table) == 0

    # bounded with a length of 1
    card = TableCard(
        fields,
        lambda: 1,
        lambda: True,
        **data
    )

    _verify_dataframe(card.table)

    # unbounded
    card = TableCard(
        fields,
        None,
        **data
    )

    _verify_dataframe(card.table)
