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

"""Unit tests for TableCardGroup parameter retention."""

import io

import pytest

from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.parameters import ParameterSet
from ansys.dyna.core.lib.table_card_group import TableCardGroup


class TestTableCardGroupParameterRetention:
    """Test parameter retention (write-back) for TableCardGroup."""

    
    def test_table_card_group_write_with_parameter_retention(self):
        """Test that TableCardGroup retains parameter references when writing.

        TableCardGroup contains multiple sub-cards that form a row group.
        Parameters in different sub-cards should be tracked separately.
        """
        parameter_set = ParameterSet()
        parameter_set.add("myeid", 100)
        parameter_set.add("mythic", 2.5)

        # Create a TableCardGroup with two sub-cards (like ELEMENT_SHELL_THICKNESS)
        # Sub-card 0: element data (eid, pid, n1, n2)
        # Sub-card 1: thickness data (thic1, thic2)
        group = TableCardGroup(
            card_schemas=[
                (
                    FieldSchema("eid", int, 0, 10, None),
                    FieldSchema("pid", int, 10, 10, None),
                    FieldSchema("n1", int, 20, 10, None),
                    FieldSchema("n2", int, 30, 10, None),
                ),
                (
                    FieldSchema("thic1", float, 0, 16, 0.0),
                    FieldSchema("thic2", float, 16, 16, 0.0),
                ),
            ],
            length_func=None,  # Unbounded
            name="elements",
        )

        # Two rows with parameters in both sub-cards
        # Row format: subcard0-row0, subcard1-row0, subcard0-row1, subcard1-row1
        card_text = """    &myeid         1         2         3
           1.0         &mythic
       200         2         4         5
           3.0             4.0"""
        buf = io.StringIO(card_text)
        group.read(buf, parameter_set)

        # Values should be resolved
        assert group.table["eid"].iloc[0] == 100  # &myeid resolved
        assert group.table["thic2"].iloc[0] == 2.5  # &mythic resolved
        assert group.table["eid"].iloc[1] == 200  # literal
        assert group.table["thic1"].iloc[1] == 3.0  # literal

        # Write with retain_parameters=True should show parameter refs
        output = group.write(
            comment=False,
            retain_parameters=True,
            parameter_set=parameter_set,
            uri_prefix="",
        )
        assert "&myeid" in output, f"Expected '&myeid' in output: {output}"
        assert "&mythic" in output, f"Expected '&mythic' in output: {output}"

    
    def test_table_card_group_write_without_parameter_retention(self):
        """Test that TableCardGroup writes resolved values without retain_parameters."""
        parameter_set = ParameterSet()
        parameter_set.add("myeid", 100)

        group = TableCardGroup(
            card_schemas=[
                (
                    FieldSchema("eid", int, 0, 10, None),
                    FieldSchema("pid", int, 10, 10, None),
                ),
                (
                    FieldSchema("thic1", float, 0, 16, 0.0),
                ),
            ],
            length_func=None,
            name="elements",
        )

        card_text = """    &myeid         1
           2.5"""
        buf = io.StringIO(card_text)
        group.read(buf, parameter_set)

        # Write without retain_parameters should show resolved values
        output = group.write(comment=False)
        assert "&myeid" not in output
        assert "100" in output

    
    def test_table_card_group_parameter_in_second_subcard(self):
        """Test that parameters in the second sub-card are correctly tracked."""
        parameter_set = ParameterSet()
        parameter_set.add("thick", 5.0)

        group = TableCardGroup(
            card_schemas=[
                (
                    FieldSchema("eid", int, 0, 10, None),
                    FieldSchema("pid", int, 10, 10, None),
                ),
                (
                    FieldSchema("thic1", float, 0, 16, 0.0),
                    FieldSchema("thic2", float, 16, 16, 0.0),
                ),
            ],
            length_func=None,
            name="elements",
        )

        # Parameter only in second sub-card
        card_text = """       1         2
          &thick           3.0"""
        buf = io.StringIO(card_text)
        group.read(buf, parameter_set)

        # Value should be resolved
        assert group.table["thic1"].iloc[0] == 5.0

        # Write with retain_parameters=True should show parameter ref
        output = group.write(
            comment=False,
            retain_parameters=True,
            parameter_set=parameter_set,
            uri_prefix="",
        )
        assert "&thick" in output, f"Expected '&thick' in output: {output}"

    
    def test_table_card_group_multiple_rows_with_parameters(self):
        """Test that parameters in multiple rows are correctly tracked."""
        parameter_set = ParameterSet()
        parameter_set.add("e1", 10)
        parameter_set.add("e2", 20)

        group = TableCardGroup(
            card_schemas=[
                (
                    FieldSchema("eid", int, 0, 10, None),
                ),
                (
                    FieldSchema("val", float, 0, 16, 0.0),
                ),
            ],
            length_func=None,
            name="data",
        )

        # Two rows with parameters in first sub-card
        card_text = """       &e1
           1.0
       &e2
           2.0"""
        buf = io.StringIO(card_text)
        group.read(buf, parameter_set)

        # Values should be resolved
        assert group.table["eid"].iloc[0] == 10
        assert group.table["eid"].iloc[1] == 20

        # Write with retain_parameters=True should show parameter refs
        output = group.write(
            comment=False,
            retain_parameters=True,
            parameter_set=parameter_set,
            uri_prefix="",
        )
        assert "&e1" in output, f"Expected '&e1' in output: {output}"
        assert "&e2" in output, f"Expected '&e2' in output: {output}"
