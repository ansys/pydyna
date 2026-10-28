# Copyright (C) 2021 - 2026 ANSYS, Inc. and/or its affiliates.
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

"""Row conditions of TableCardGroup: sub-cards whose line depends on each row's values.

The group used here has the layout of *ELEMENT_SHELL_THICKNESS, whose THIC5-THIC8
line exists only for elements with mid-side nodes (N5 set).
"""

import io

import numpy as np
import pandas as pd
import pytest

from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.parameters import ParameterSet
from ansys.dyna.core.lib.row_condition import RowCondition, is_missing
from ansys.dyna.core.lib.table_card_group import TableCardGroup

_ELEMENT_FIELDS = ["eid", "pid", "n1", "n2", "n3", "n4", "n5", "n6", "n7", "n8"]
_ELEMENT_CARD = tuple(FieldSchema(name, int, 8 * i, 8, None) for i, name in enumerate(_ELEMENT_FIELDS))
_THICKNESS_CARD = tuple(
    FieldSchema(name, float, 16 * i, 16, 0.0) for i, name in enumerate(["thic1", "thic2", "thic3", "thic4", "beta"])
)
_MIDSIDE_THICKNESS_CARD = tuple(
    FieldSchema(name, float, 16 * i, 16, 0.0) for i, name in enumerate(["thic5", "thic6", "thic7", "thic8"])
)

_HAS_MIDSIDE_NODES = RowCondition(
    ("n5",),
    lambda n5: not is_missing(n5) and n5 != 0,
    lambda table: (table["n5"].notna() & table["n5"].ne(0)).to_numpy(dtype=bool),
)
_HAS_MIDSIDE_NODES_ROW_BY_ROW = RowCondition(("n5",), _HAS_MIDSIDE_NODES.evaluate_row)


@pytest.fixture(params=[_HAS_MIDSIDE_NODES, _HAS_MIDSIDE_NODES_ROW_BY_ROW], ids=["vectorized", "row-by-row"])
def condition(request):
    return request.param


def _group(condition=_HAS_MIDSIDE_NODES, length_func=None, card_active_funcs=None) -> TableCardGroup:
    return TableCardGroup(
        [_ELEMENT_CARD, _THICKNESS_CARD, _MIDSIDE_THICKNESS_CARD],
        length_func,
        name="elements",
        card_active_funcs=card_active_funcs,
        row_conditions=[None, None, condition],
    )


def _ints(*values, width=8) -> str:
    return "".join(" " * width if value is None else f"{value:>{width}}" for value in values)


def _floats(*values, width=16) -> str:
    return "".join(" " * width if value is None else f"{value:>{width}}" for value in values)


def _shell8(eid, thickness, long=False) -> list:
    """Lines of an 8-node shell, every field 20 characters wide in long format."""
    int_width, float_width = (20, 20) if long else (8, 16)
    return [
        _ints(eid, 1, 1, 2, 3, 4, 5, 6, 7, 8, width=int_width),
        _floats(1.0, 1.0, 1.0, 1.0, 0.0, width=float_width),
        _floats(*[thickness] * 4, width=float_width),
    ]


def _shell4(eid, long=False) -> list:
    int_width, float_width = (20, 20) if long else (8, 16)
    return [
        _ints(eid, 1, 11, 12, 13, 14, None, None, None, None, width=int_width),
        _floats(2.0, 2.0, 2.0, 2.0, 0.0, width=float_width),
    ]


def _read(group: TableCardGroup, lines: list, parameter_set: ParameterSet = None) -> pd.DataFrame:
    result = group.read(io.StringIO("\n".join(lines)), parameter_set)
    assert result.warnings == []
    return group.table


_BLOCKS = [
    pytest.param(_shell4(1) + _shell4(2), [1, 2], [np.nan, np.nan], id="4-node shells"),
    pytest.param(_shell8(1, 1.5), [1], [1.5], id="8-node shell"),
    pytest.param(_shell8(1, 2.0), [1], [2.0], id="8-node shell with whole THIC5-8"),
    pytest.param(_shell8(1, 1.5) + _shell8(2, 2.5), [1, 2], [1.5, 2.5], id="8-node shells"),
    pytest.param(_shell8(1, 1.5) + _shell4(2), [1, 2], [1.5, np.nan], id="8-node, 4-node"),
    pytest.param(_shell4(1) + _shell8(2, 1.5) + _shell4(3), [1, 2, 3], [np.nan, 1.5, np.nan], id="4-, 8-, 4-node"),
]


class TestRowCondition:
    def test_requires_a_field(self):
        with pytest.raises(ValueError, match="at least one field"):
            RowCondition((), lambda: True)

    @pytest.mark.parametrize("value", [None, pd.NA, float("nan"), np.float64("nan")])
    def test_missing_values(self, value):
        assert is_missing(value)

    @pytest.mark.parametrize("value", [0, 0.0, 5, np.int32(5), "RX", ""])
    def test_set_values(self, value):
        assert not is_missing(value)

    def test_evaluate_gives_one_boolean_per_row(self, condition):
        table = pd.DataFrame({"n5": pd.array([5, None, 0], dtype="Int32")})
        assert condition.evaluate(table).tolist() == [True, False, False]

    def test_evaluate_float_column_treats_nan_as_missing(self, condition):
        table = pd.DataFrame({"n5": [5.0, np.nan, 0.0]})
        assert condition.evaluate(table).tolist() == [True, False, False]

    def test_evaluate_empty_table(self, condition):
        result = condition.evaluate(pd.DataFrame({"n5": pd.array([], dtype="Int32")}))
        assert result.dtype == bool and len(result) == 0

    @pytest.mark.parametrize(
        "result", [[True], pd.array([True, None], dtype="boolean")], ids=["wrong length", "missing values"]
    )
    def test_evaluate_rejects_invalid_results(self, result):
        condition = RowCondition(("n5",), lambda n5: True, lambda table: result)
        with pytest.raises(ValueError, match="must give 2 booleans"):
            condition.evaluate(pd.DataFrame({"n5": [1, 2]}))


class TestConstruction:
    def test_one_row_condition_per_sub_card(self):
        with pytest.raises(ValueError, match="one row condition per sub-card"):
            TableCardGroup([_ELEMENT_CARD, _THICKNESS_CARD], None, row_conditions=[None])

    @pytest.mark.parametrize("field", ["thic1", "thic5", "unknown"], ids=["same sub-card", "later sub-card", "unknown"])
    def test_condition_reads_earlier_sub_cards(self, field):
        condition = RowCondition((field,), lambda value: True)
        with pytest.raises(ValueError, match="not fields of an earlier sub-card"):
            TableCardGroup(
                [_ELEMENT_CARD, _THICKNESS_CARD, _MIDSIDE_THICKNESS_CARD], None, row_conditions=[None, condition, None]
            )


class TestRead:
    @pytest.mark.parametrize("lines, eids, thic5", _BLOCKS)
    def test_midside_line_only_in_rows_with_n5(self, condition, lines, eids, thic5):
        table = _read(_group(condition), lines)
        assert table["eid"].tolist() == eids
        np.testing.assert_array_equal(table["thic5"].to_numpy(), thic5)
        assert table["thic1"].notna().all()

    def test_long_format(self, condition):
        group = _group(condition)
        group.format = format_type.long
        table = _read(group, _shell8(1, 1.5, long=True) + _shell4(2, long=True))
        assert table["eid"].tolist() == [1, 2]
        np.testing.assert_array_equal(table["thic1"].to_numpy(), [1.0, 2.0])
        np.testing.assert_array_equal(table["thic8"].to_numpy(), [1.5, np.nan])

    def test_condition_field_from_parameter(self):
        parameter_set = ParameterSet()
        parameter_set.add("mid", 5)
        lines = _shell8(1, 1.5) + _shell4(2)
        lines[0] = _ints(1, 1, 1, 2, 3, 4) + "    &mid" + _ints(6, 7, 8)
        table = _read(_group(), lines, parameter_set)
        assert table["n5"][0] == 5
        np.testing.assert_array_equal(table["thic5"].to_numpy(), [1.5, np.nan])

    def test_condition_field_from_comma_delimited_line(self):
        lines = ["1,1,1,2,3,4,5,6,7,8"] + _shell8(1, 1.5)[1:] + _shell4(2)
        table = _read(_group(), lines, ParameterSet())
        assert table["eid"].tolist() == [1, 2]
        np.testing.assert_array_equal(table["thic5"].to_numpy(), [1.5, np.nan])

    def test_bounded_group_stops_after_its_rows(self):
        next_card = _ints(99)
        buf = io.StringIO("\n".join(_shell8(1, 1.5) + _shell4(2) + [next_card]))
        group = _group(length_func=lambda: 2)
        group.read(buf)
        assert group.table["eid"].tolist() == [1, 2]
        assert buf.readline() == next_card

    def test_missing_midside_line_at_end(self):
        table = _read(_group(), _shell8(1, 1.5)[:2])
        assert table["eid"].tolist() == [1]
        assert np.isnan(table["thic5"][0])

    def test_inactive_sub_card_has_no_lines(self):
        group = _group(card_active_funcs=[None, None, lambda: False])
        table = _read(group, _shell8(1, 1.5)[:2] + _shell8(2, 1.5)[:2])
        assert table["eid"].tolist() == [1, 2]


class TestWrite:
    @pytest.mark.parametrize("lines, eids, thic5", _BLOCKS)
    def test_round_trip(self, condition, lines, eids, thic5):
        group = _group(condition)
        _read(group, lines)
        assert group.write(comment=False) == "\n".join(lines)

    def test_long_format_round_trip(self):
        lines = _shell4(1, long=True) + _shell8(2, 1.5, long=True)
        group = _group()
        group.format = format_type.long
        _read(group, lines)
        assert group.write(comment=False) == "\n".join(lines)

    def test_comment_line_before_first_line_of_each_sub_card(self):
        group = _group()
        _read(group, _shell4(1) + _shell8(2, 1.5))
        lines = group.write().split("\n")
        assert [line.startswith("$#") for line in lines] == [True, False, True, False, False, False, True, False]
        assert "thic5" in lines[6]

    def test_no_comment_line_for_sub_card_without_lines(self):
        group = _group()
        _read(group, _shell4(1) + _shell4(2))
        assert "thic5" not in group.write()

    def test_follows_in_place_edits_of_the_table(self, condition):
        group = _group(condition)
        _read(group, _shell4(1) + _shell8(2, 1.5))
        group.table.loc[0, "n5"] = 15
        group.table.loc[1, "n5"] = 0
        lines = group.write(comment=False).split("\n")
        assert lines[2] == " " * 64
        assert len(lines) == 5

    def test_table_without_condition_columns(self):
        group = _group()
        group.table = pd.DataFrame({"eid": [1], "pid": [1], "n1": [1], "n2": [2], "n3": [3], "n4": [4]})
        assert group.write(comment=False).split("\n") == [
            _ints(1, 1, 1, 2, 3, 4, None, None, None, None),
            _floats(0.0, 0.0, 0.0, 0.0, 0.0),
        ]

    def test_inactive_sub_card_has_no_lines(self):
        group = _group(card_active_funcs=[None, None, lambda: False])
        _read(group, _shell8(1, 1.5)[:2])
        assert group.write(comment=False) == "\n".join(_shell8(1, 1.5)[:2])
        assert group._num_rows() == 2

    def test_num_rows_counts_data_lines(self):
        group = _group()
        _read(group, _shell8(1, 1.5) + _shell4(2))
        assert group._num_rows() == 5

    def test_parameter_refs_stay_with_their_row(self):
        parameter_set = ParameterSet()
        parameter_set.add("t", 1.5)
        lines = _shell4(1) + _shell8(2, 1.5)
        lines[-1] = "              &t" + _floats(1.5, 1.5, 1.5)
        group = _group()
        _read(group, lines, parameter_set)
        retained = "&t".ljust(16) + _floats(1.5, 1.5, 1.5)

        def midside_lines():
            output = group.write(comment=False, retain_parameters=True, parameter_set=parameter_set, uri_prefix="")
            return [line for line in output.split("\n") if len(line) == 64]

        assert midside_lines() == [retained]
        group.table.loc[0, "n5"] = 15
        assert midside_lines() == [" " * 64, retained]
