# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
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

"""
Comprehensive tests for parameter substitution across all card types.

These tests verify that &parameter references work correctly in:
- SeriesCard (bounded and unbounded)
- TableCard (bounded and unbounded)
- TableCardGroup
"""

import math
import io
import pytest
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field
from ansys.dyna.core.lib.series_card import SeriesCard
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.table_card_group import TableCardGroup
from ansys.dyna.core.lib.parameters import ParameterSet
from ansys.dyna.core.lib.format_type import format_type


class TestSeriesCardParameters:
    """Test parameter substitution in SeriesCard."""

    @pytest.mark.keywords
    def test_series_card_unbounded_with_single_parameter(self):
        """Test unbounded SeriesCard with one parameter reference."""
        parameter_set = ParameterSet()
        parameter_set.add("dens", 7850.0)

        series = SeriesCard(
            name="values",
            fields_per_card=8,
            element_width=10,
            input_type=float,
            length_func=None,  # Unbounded
        )

        card_text = """       1.0       2.0     &dens       4.0"""
        buf = io.StringIO(card_text)
        series.read(buf, parameter_set)

        assert len(series.data) == 4
        assert series.data[0] == 1.0
        assert series.data[1] == 2.0
        assert series.data[2] == 7850.0
        assert series.data[3] == 4.0

    @pytest.mark.keywords
    def test_series_card_unbounded_with_negative_parameter(self):
        """Test unbounded SeriesCard with negative parameter (-&param)."""
        parameter_set = ParameterSet()
        parameter_set.add("offs", 100.0)

        series = SeriesCard(
            name="values",
            fields_per_card=8,
            element_width=10,
            input_type=float,
            length_func=None,
        )

        card_text = """       1.0    -&offs       3.0"""
        buf = io.StringIO(card_text)
        series.read(buf, parameter_set)

        assert len(series.data) == 3
        assert series.data[0] == 1.0
        assert series.data[1] == -100.0
        assert series.data[2] == 3.0

    @pytest.mark.keywords
    def test_series_card_bounded_with_parameters(self):
        """Test bounded SeriesCard with parameters."""
        parameter_set = ParameterSet()
        parameter_set.add("val1", 10.0)
        parameter_set.add("val2", 20.0)

        series = SeriesCard(
            name="values",
            fields_per_card=4,
            element_width=10,
            input_type=float,
            length_func=lambda: 6,  # Bounded to 6 elements
        )

        card_text = """     &val1     &val2       3.0       4.0
       5.0       6.0"""
        buf = io.StringIO(card_text)
        series.read(buf, parameter_set)

        assert len(series.data) == 6
        assert series.data[0] == 10.0
        assert series.data[1] == 20.0
        assert series.data[2] == 3.0
        assert series.data[3] == 4.0
        assert series.data[4] == 5.0
        assert series.data[5] == 6.0

    @pytest.mark.keywords
    def test_series_card_integer_parameter(self):
        """Test SeriesCard with integer parameter."""
        parameter_set = ParameterSet()
        parameter_set.add("count", 42)

        series = SeriesCard(
            name="ids",
            fields_per_card=4,
            element_width=10,
            input_type=int,
            length_func=None,
        )

        card_text = """         1    &count         3"""
        buf = io.StringIO(card_text)
        series.read(buf, parameter_set)

        assert len(series.data) == 3
        assert series.data[0] == 1
        assert series.data[1] == 42
        assert series.data[2] == 3

    @pytest.mark.keywords
    def test_series_card_multiple_parameters_on_multiple_lines(self):
        """Test SeriesCard with parameters spread across multiple lines."""
        parameter_set = ParameterSet()
        parameter_set.add("x1", 1.5)
        parameter_set.add("y1", 2.5)
        parameter_set.add("x2", 3.5)
        parameter_set.add("y2", 4.5)

        series = SeriesCard(
            name="coords",
            fields_per_card=4,
            element_width=10,
            input_type=float,
            length_func=None,
        )

        card_text = """      &x1       &y1       0.0       0.0
      &x2       &y2       0.0       0.0"""
        buf = io.StringIO(card_text)
        series.read(buf, parameter_set)

        assert len(series.data) == 8
        assert series.data[0] == 1.5
        assert series.data[1] == 2.5
        assert series.data[4] == 3.5
        assert series.data[5] == 4.5


class TestTableCardParameters:
    """Test parameter substitution in TableCard."""

    @pytest.mark.keywords
    def test_table_card_unbounded_with_parameter(self):
        """Test unbounded TableCard with parameter in one field."""
        parameter_set = ParameterSet()
        parameter_set.add("xcoord", 100.5)

        table = TableCard(
            [
                Field("nid", int, 0, 8),
                Field("x", float, 8, 16),
                Field("y", float, 24, 16),
                Field("z", float, 40, 16),
            ],
            None,  # Unbounded
        )

        card_text = """       1   &xcoord            10.0            20.0
       2      200.5            30.0            40.0"""
        buf = io.StringIO(card_text)
        table.read(buf, parameter_set)

        assert len(table.table) == 2
        assert table.table["nid"][0] == 1
        assert table.table["x"][0] == 100.5
        assert table.table["y"][0] == 10.0
        assert table.table["z"][0] == 20.0
        assert table.table["nid"][1] == 2
        assert table.table["x"][1] == 200.5

    @pytest.mark.keywords
    def test_table_card_bounded_with_parameters(self):
        """Test bounded TableCard with parameters."""
        parameter_set = ParameterSet()
        parameter_set.add("x1", 10.0)
        parameter_set.add("y1", 20.0)
        parameter_set.add("z1", 30.0)

        table = TableCard(
            [
                Field("nid", int, 0, 8),
                Field("x", float, 8, 16),
                Field("y", float, 24, 16),
                Field("z", float, 40, 16),
            ],
            lambda: 3,  # Bounded to 3 rows
        )

        card_text = """       1        &x1            &y1            &z1
       2       11.0            21.0            31.0
       3       12.0            22.0            32.0"""
        buf = io.StringIO(card_text)
        table.read(buf, parameter_set)

        assert len(table.table) == 3
        assert table.table["x"][0] == 10.0
        assert table.table["y"][0] == 20.0
        assert table.table["z"][0] == 30.0

    @pytest.mark.keywords
    def test_table_card_with_negative_parameter(self):
        """Test TableCard with negative parameter."""
        parameter_set = ParameterSet()
        parameter_set.add("offset", 50.0)

        table = TableCard(
            [
                Field("id", int, 0, 8),
                Field("value", float, 8, 16),
            ],
            None,
        )

        card_text = """       1      -&offset
       2          25.0"""
        buf = io.StringIO(card_text)
        table.read(buf, parameter_set)

        assert len(table.table) == 2
        assert table.table["value"][0] == -50.0
        assert table.table["value"][1] == 25.0

    @pytest.mark.keywords
    def test_table_card_mixed_parameters_and_literals(self):
        """Test TableCard with some parameters and some literal values."""
        parameter_set = ParameterSet()
        parameter_set.add("x_param", 100.0)
        parameter_set.add("z_param", 300.0)

        table = TableCard(
            [
                Field("nid", int, 0, 8),
                Field("x", float, 8, 16),
                Field("y", float, 24, 16),
                Field("z", float, 40, 16),
            ],
            None,
        )

        card_text = """       1    &x_param            50.0        &z_param
       2       200.0            60.0           400.0"""
        buf = io.StringIO(card_text)
        table.read(buf, parameter_set)

        assert len(table.table) == 2
        assert table.table["x"][0] == 100.0
        assert table.table["y"][0] == 50.0
        assert table.table["z"][0] == 300.0
        assert table.table["x"][1] == 200.0
        assert table.table["y"][1] == 60.0
        assert table.table["z"][1] == 400.0

    @pytest.mark.keywords
    def test_table_card_without_parameters_still_works(self):
        """Ensure TableCard without parameters still works (no regression)."""
        table = TableCard(
            [
                Field("nid", int, 0, 8),
                Field("x", float, 8, 16),
            ],
            None,
        )

        card_text = """       1       100.0
       2       200.0"""
        buf = io.StringIO(card_text)
        table.read(buf, None)

        assert len(table.table) == 2
        assert table.table["x"][0] == 100.0
        assert table.table["x"][1] == 200.0

    @pytest.mark.keywords
    def test_table_card_integer_parameter(self):
        """Test TableCard with integer parameter."""
        parameter_set = ParameterSet()
        parameter_set.add("nid", 1000)

        table = TableCard(
            [
                Field("nid", int, 0, 8),
                Field("x", float, 8, 16),
            ],
            None,
        )

        card_text = """    &nid       100.0"""
        buf = io.StringIO(card_text)
        table.read(buf, parameter_set)

        assert len(table.table) == 1
        assert table.table["nid"][0] == 1000
        assert table.table["x"][0] == 100.0


class TestTableCardGroupParameters:
    """Test parameter substitution in TableCardGroup."""

    @pytest.mark.keywords
    def test_table_card_group_with_parameters(self):
        """Test TableCardGroup with parameters across sub-cards."""
        from ansys.dyna.core.lib.field_schema import FieldSchema

        parameter_set = ParameterSet()
        parameter_set.add("id1", 100)
        parameter_set.add("val1", 10.5)
        parameter_set.add("val2", 20.5)

        card_schemas = [
            (FieldSchema("id", int, 0, 8, None), FieldSchema("value1", float, 8, 16, None)),
            (FieldSchema("value2", float, 0, 16, None), FieldSchema("value3", float, 16, 16, None)),
        ]

        group = TableCardGroup(
            card_schemas,
            lambda: 2,  # 2 rows
        )

        card_text = """    &id1      &val1
     &val2            5.0
     200       11.5
      21.5            6.0"""
        buf = io.StringIO(card_text)
        group.read(buf, parameter_set)

        assert len(group.table) == 2
        assert group.table["id"][0] == 100
        assert group.table["value1"][0] == 10.5
        assert group.table["value2"][0] == 20.5
        assert group.table["value3"][0] == 5.0
        assert group.table["id"][1] == 200
        assert group.table["value1"][1] == 11.5

    @pytest.mark.keywords
    def test_table_card_group_unbounded_with_parameters(self):
        """Test unbounded TableCardGroup with parameters."""
        from ansys.dyna.core.lib.field_schema import FieldSchema

        parameter_set = ParameterSet()
        parameter_set.add("x", 1.0)
        parameter_set.add("y", 2.0)

        card_schemas = [
            (FieldSchema("a", float, 0, 10, None), FieldSchema("b", float, 10, 10, None)),
            (FieldSchema("c", float, 0, 10, None), FieldSchema("d", float, 10, 10, None)),
        ]

        group = TableCardGroup(card_schemas, None)  # Unbounded

        card_text = """       &x       &y
       3.0       4.0
       5.0       6.0
       7.0       8.0"""
        buf = io.StringIO(card_text)
        group.read(buf, parameter_set)

        assert len(group.table) == 2
        assert group.table["a"][0] == 1.0
        assert group.table["b"][0] == 2.0
        assert group.table["c"][0] == 3.0
        assert group.table["d"][0] == 4.0
        assert group.table["a"][1] == 5.0
        assert group.table["b"][1] == 6.0


class TestCSVFormatParameterSubstitution:
    """Test parameter substitution in CSV (comma-delimited) format."""

    @pytest.mark.keywords
    def test_table_card_csv_format_with_parameter(self):
        """Test TableCard with CSV format and parameter reference."""
        parameter_set = ParameterSet()
        parameter_set.add("xcoord", 100.5)

        table = TableCard(
            [
                Field("nid", int, 0, 8),
                Field("x", float, 8, 16),
                Field("y", float, 24, 16),
                Field("z", float, 40, 16),
            ],
            None,
        )

        # CSV format - fields separated by commas
        card_text = """1,&xcoord,10.0,20.0
2,200.5,30.0,40.0"""
        buf = io.StringIO(card_text)
        table.read(buf, parameter_set)

        assert len(table.table) == 2
        assert table.table["nid"][0] == 1
        assert table.table["x"][0] == 100.5  # &xcoord substituted
        assert table.table["y"][0] == 10.0
        assert table.table["z"][0] == 20.0
        assert table.table["nid"][1] == 2
        assert table.table["x"][1] == 200.5

    @pytest.mark.keywords
    def test_table_card_csv_format_with_negative_parameter(self):
        """Test TableCard CSV format with negative parameter (-&param)."""
        parameter_set = ParameterSet()
        parameter_set.add("offset", 50.0)

        table = TableCard(
            [
                Field("id", int, 0, 8),
                Field("value", float, 8, 16),
            ],
            None,
        )

        card_text = """1,-&offset
2,25.0"""
        buf = io.StringIO(card_text)
        table.read(buf, parameter_set)

        assert len(table.table) == 2
        assert table.table["value"][0] == -50.0  # -&offset = -50.0
        assert table.table["value"][1] == 25.0

    @pytest.mark.keywords
    def test_table_card_csv_format_multiple_parameters(self):
        """Test TableCard CSV format with multiple parameters in one row."""
        parameter_set = ParameterSet()
        parameter_set.add("x1", 1.0)
        parameter_set.add("y1", 2.0)
        parameter_set.add("z1", 3.0)

        table = TableCard(
            [
                Field("nid", int, 0, 8),
                Field("x", float, 8, 16),
                Field("y", float, 24, 16),
                Field("z", float, 40, 16),
            ],
            None,
        )

        card_text = """1,&x1,&y1,&z1"""
        buf = io.StringIO(card_text)
        table.read(buf, parameter_set)

        assert len(table.table) == 1
        assert table.table["x"][0] == 1.0
        assert table.table["y"][0] == 2.0
        assert table.table["z"][0] == 3.0

    @pytest.mark.keywords
    def test_table_card_csv_format_integer_parameter(self):
        """Test TableCard CSV format with integer parameter."""
        parameter_set = ParameterSet()
        parameter_set.add("nodeid", 1000)

        table = TableCard(
            [
                Field("nid", int, 0, 8),
                Field("x", float, 8, 16),
            ],
            None,
        )

        card_text = """&nodeid,100.0"""
        buf = io.StringIO(card_text)
        table.read(buf, parameter_set)

        assert len(table.table) == 1
        assert table.table["nid"][0] == 1000  # &nodeid substituted
        assert table.table["x"][0] == 100.0

    @pytest.mark.keywords
    def test_table_card_csv_format_missing_parameter_raises_error(self):
        """Test TableCard CSV format with missing parameter raises KeyError."""
        parameter_set = ParameterSet()
        # Don't add the parameter

        table = TableCard(
            [
                Field("id", int, 0, 8),
                Field("val", float, 8, 16),
            ],
            None,
        )

        card_text = """1,&missing"""
        buf = io.StringIO(card_text)

        with pytest.raises(KeyError):
            table.read(buf, parameter_set)


class TestParameterSubstitutionEdgeCases:
    """Test edge cases and error conditions."""

    @pytest.mark.keywords
    def test_series_card_missing_parameter_raises_error(self):
        """Test that missing parameter raises KeyError."""
        parameter_set = ParameterSet()
        # Don't add the parameter

        series = SeriesCard(
            name="values",
            fields_per_card=4,
            element_width=10,
            input_type=float,
            length_func=None,
        )

        card_text = """       1.0  &missing       3.0"""
        buf = io.StringIO(card_text)

        with pytest.raises(KeyError):
            series.read(buf, parameter_set)

    @pytest.mark.keywords
    def test_table_card_missing_parameter_raises_error(self):
        """Test that TableCard with missing parameter raises KeyError."""
        parameter_set = ParameterSet()
        # Don't add the parameter

        table = TableCard(
            [Field("id", int, 0, 8), Field("val", float, 8, 16)],
            None,
        )

        card_text = """       1  &missing"""
        buf = io.StringIO(card_text)

        with pytest.raises(KeyError):
            table.read(buf, parameter_set)

    @pytest.mark.keywords
    def test_series_card_type_mismatch_raises_error(self):
        """Test that parameter type mismatch raises TypeError."""
        parameter_set = ParameterSet()
        parameter_set.add("floatval", 1.5)  # Float value

        series = SeriesCard(
            name="ids",
            fields_per_card=4,
            element_width=10,
            input_type=int,  # Expecting int
            length_func=None,
        )

        card_text = """         1 &floatval         3"""
        buf = io.StringIO(card_text)

        with pytest.raises(TypeError):
            series.read(buf, parameter_set)

    @pytest.mark.keywords
    def test_table_card_parameter_none_set_raises_error(self):
        """Test that parameter reference without parameter_set raises error."""
        table = TableCard(
            [Field("id", int, 0, 8), Field("val", float, 8, 16)],
            None,
        )

        card_text = """       1     &param"""
        buf = io.StringIO(card_text)

        with pytest.raises(ValueError):
            table.read(buf, None)  # No parameter set provided
