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

"""Tests for TableCard parameter retention (write-back).

These tests verify that parameter references (e.g., &myvar) read from table
card data can be retained when writing the card back out, rather than being
replaced with their resolved values.
"""

import io

import pytest

from ansys.dyna.core.lib.card import Field
from ansys.dyna.core.lib.parameters import ParameterSet
from ansys.dyna.core.lib.table_card import TableCard


# =============================================================================
# Parameter Retention Tests for TableCard
# =============================================================================


class TestTableCardParameterRetention:
    """Test parameter retention (write-back) for TableCard."""

    @pytest.mark.keywords
    def test_table_card_write_with_parameter_retention_single_row(self):
        """Test that a single-row table card retains parameter references when writing."""
        parameter_set = ParameterSet()
        parameter_set.add("x1", -0.25)
        parameter_set.add("y1", 0.30)

        table = TableCard(
            [
                Field("nid", int, 0, 8),
                Field("x", float, 8, 16),
                Field("y", float, 24, 16),
                Field("z", float, 40, 16),
            ],
            None,  # Unbounded
        )

        card_text = "     100            &x1            &y1             0.0"
        buf = io.StringIO(card_text)
        table.read(buf, parameter_set)

        # Values should be resolved
        assert table.table["nid"][0] == 100
        assert table.table["x"][0] == -0.25
        assert table.table["y"][0] == 0.30
        assert table.table["z"][0] == 0.0

        # Write with retain_parameters=True should show parameter refs
        # Empty uri_prefix since at unit test level there's no outer scope
        output = table.write(
            comment=False,
            retain_parameters=True,
            parameter_set=parameter_set,
            uri_prefix="",
        )
        assert "&x1" in output, f"Expected '&x1' in output: {output}"
        assert "&y1" in output, f"Expected '&y1' in output: {output}"

    @pytest.mark.keywords
    def test_table_card_write_with_parameter_retention_multi_row(self):
        """Test that multi-row table card retains parameter references when writing."""
        parameter_set = ParameterSet()
        parameter_set.add("x1", -0.25)
        parameter_set.add("y1", 0.30)
        parameter_set.add("x2", 0.50)

        table = TableCard(
            [
                Field("nid", int, 0, 8),
                Field("x", float, 8, 16),
                Field("y", float, 24, 16),
                Field("z", float, 40, 16),
            ],
            None,  # Unbounded
        )

        card_text = """     100            &x1            &y1             0.0
     101            &x2            0.0             0.0
     102            1.0            2.0             3.0"""
        buf = io.StringIO(card_text)
        table.read(buf, parameter_set)

        # Values should be resolved
        assert len(table.table) == 3
        assert table.table["x"][0] == -0.25
        assert table.table["y"][0] == 0.30
        assert table.table["x"][1] == 0.50

        # Write with retain_parameters=True should show parameter refs
        # Empty uri_prefix since at unit test level there's no outer scope
        output = table.write(
            comment=False,
            retain_parameters=True,
            parameter_set=parameter_set,
            uri_prefix="",
        )
        assert "&x1" in output, f"Expected '&x1' in output: {output}"
        assert "&y1" in output, f"Expected '&y1' in output: {output}"
        assert "&x2" in output, f"Expected '&x2' in output: {output}"

    @pytest.mark.keywords
    def test_table_card_write_without_parameter_retention(self):
        """Test that table card without retain_parameters writes resolved values."""
        parameter_set = ParameterSet()
        parameter_set.add("x1", -0.25)

        table = TableCard(
            [
                Field("nid", int, 0, 8),
                Field("x", float, 8, 16),
                Field("y", float, 24, 16),
                Field("z", float, 40, 16),
            ],
            None,
        )

        card_text = "     100            &x1            0.0             0.0"
        buf = io.StringIO(card_text)
        table.read(buf, parameter_set)

        # Write without retain_parameters should show resolved values
        output = table.write(comment=False)
        assert "&x1" not in output
        assert "-0.25" in output

    @pytest.mark.keywords
    def test_table_card_write_with_negative_parameter(self):
        """Test that negative parameter references are retained."""
        parameter_set = ParameterSet()
        parameter_set.add("off", 50.0)

        table = TableCard(
            [
                Field("nid", int, 0, 8),
                Field("x", float, 8, 16),
                Field("y", float, 24, 16),
                Field("z", float, 40, 16),
            ],
            None,
        )

        # Proper spacing: nid(8), x(16), y(16), z(16)
        # nid:   "     100" (8 chars)
        # x:     "           -&off" (16 chars, -&off right-aligned)
        # y:     "             0.0" (16 chars)
        # z:     "             0.0" (16 chars)
        card_text = "     100           -&off             0.0             0.0"
        buf = io.StringIO(card_text)
        table.read(buf, parameter_set)

        # Value should be negated
        assert table.table["x"][0] == -50.0

        # Write with retain_parameters should show -&off
        # Empty uri_prefix since at unit test level there's no outer scope
        output = table.write(
            comment=False,
            retain_parameters=True,
            parameter_set=parameter_set,
            uri_prefix="",
        )
        assert "-&off" in output, f"Expected '-&off' in output: {output}"

    @pytest.mark.keywords
    def test_table_card_write_mixed_parameters_and_literals(self):
        """Test table card with both parameters and literal values retains only parameters."""
        parameter_set = ParameterSet()
        parameter_set.add("x1", 100.0)

        table = TableCard(
            [
                Field("nid", int, 0, 8),
                Field("x", float, 8, 16),
                Field("y", float, 24, 16),
                Field("z", float, 40, 16),
            ],
            None,
        )

        card_text = "     100            &x1           50.0            75.0"
        buf = io.StringIO(card_text)
        table.read(buf, parameter_set)

        # Write with retain_parameters should show &x1 but not y/z as params
        # Empty uri_prefix since at unit test level there's no outer scope
        output = table.write(
            comment=False,
            retain_parameters=True,
            parameter_set=parameter_set,
            uri_prefix="",
        )
        assert "&x1" in output, f"Expected '&x1' in output: {output}"
        # The literal values 50.0 and 75.0 should be present as values
        assert "50.0" in output or "50" in output
        assert "75.0" in output or "75" in output
