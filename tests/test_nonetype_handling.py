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

"""Tests for NoneType handling in keyword parsing.

These tests verify that keywords with missing fields (which result in None values
for integer fields) are handled gracefully without raising TypeErrors.
"""

import pytest

from ansys.dyna.core.lib.card import Card
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.series_card import SeriesCard


class TestCardActiveWithNone:
    """Tests for Card.active property handling None values in active_func."""

    @pytest.mark.keywords
    def test_active_func_comparison_with_none_returns_false(self):
        """Card.active should return False when active_func compares with None."""
        # Create a simple holder for the value
        class ValueHolder:
            value = None

        holder = ValueHolder()

        schema = (FieldSchema("field1", int, 0, 10, None),)
        card = Card.from_field_schemas(schema, active_func=lambda: holder.value > 0)

        # When holder.value is None, comparison would raise TypeError
        # but Card.active should catch this and return False
        assert card.active is False

        # When holder.value is set, comparison should work normally
        holder.value = 5
        assert card.active is True

        holder.value = 0
        assert card.active is False

        holder.value = -1
        assert card.active is False

    @pytest.mark.keywords
    def test_active_func_equality_with_none_returns_false(self):
        """Card.active should return False when active_func does equality check with None."""

        class ValueHolder:
            value = None

        holder = ValueHolder()

        schema = (FieldSchema("field1", int, 0, 10, None),)
        card = Card.from_field_schemas(schema, active_func=lambda: holder.value == 1)

        # None == 1 is False, not a TypeError, so this should work
        assert card.active is False

        holder.value = 1
        assert card.active is True

    @pytest.mark.keywords
    def test_active_func_in_check_with_none_returns_false(self):
        """Card.active should return False when active_func does 'in' check with None."""

        class ValueHolder:
            value = None

        holder = ValueHolder()

        schema = (FieldSchema("field1", int, 0, 10, None),)
        card = Card.from_field_schemas(schema, active_func=lambda: holder.value in [1, 2, 3])

        # None in [1,2,3] is False, not a TypeError
        assert card.active is False

        holder.value = 2
        assert card.active is True

    @pytest.mark.keywords
    def test_active_func_arithmetic_with_none_returns_false(self):
        """Card.active should return False when active_func does arithmetic with None."""

        class ValueHolder:
            value = None

        holder = ValueHolder()

        schema = (FieldSchema("field1", int, 0, 10, None),)
        # This would raise TypeError: unsupported operand type(s) for +: 'NoneType' and 'int'
        card = Card.from_field_schemas(schema, active_func=lambda: (holder.value + 1) > 0)

        # Should not raise, should return False
        assert card.active is False

        holder.value = 5
        assert card.active is True


class TestSeriesCardNumRowsWithNone:
    """Tests for SeriesCard._num_rows handling None from length_func."""

    @pytest.mark.keywords
    def test_num_rows_with_none_length_returns_zero(self):
        """SeriesCard._num_rows should return 0 when length_func returns None."""

        class ValueHolder:
            value = None

        holder = ValueHolder()

        series = SeriesCard(
            name="test_series",
            fields_per_card=8,
            element_width=10,
            input_type=float,
            length_func=lambda: holder.value,
        )

        # When length_func returns None, _num_rows should return 0
        assert series._num_rows() == 0

        # When length_func returns a valid value, _num_rows should calculate correctly
        holder.value = 4
        assert series._num_rows() == 1  # 4 values, 8 fields per card = 1 row

        holder.value = 10
        assert series._num_rows() == 2  # 10 values, 8 fields per card = 2 rows

    @pytest.mark.keywords
    def test_num_rows_with_zero_length_returns_zero(self):
        """SeriesCard._num_rows should return 0 when length_func returns 0."""
        series = SeriesCard(
            name="test_series",
            fields_per_card=8,
            element_width=10,
            input_type=float,
            length_func=lambda: 0,
        )

        assert series._num_rows() == 0

    @pytest.mark.keywords
    def test_series_card_read_with_none_length(self):
        """SeriesCard.read should handle None from length_func gracefully."""
        import io

        class ValueHolder:
            value = None

        holder = ValueHolder()

        series = SeriesCard(
            name="test_series",
            fields_per_card=8,
            element_width=10,
            input_type=float,
            length_func=lambda: holder.value,
        )

        # Reading with None length should not raise
        buf = io.StringIO("1.0 2.0 3.0 4.0\n")
        # This should not raise TypeError
        series.read(buf)

        # Data should be empty since length is None (0 rows)
        assert len(series.data) == 0


class TestKeywordLoadingWithMissingFields:
    """Integration tests for keyword loading with missing fields."""

    @pytest.mark.keywords
    def test_section_shell_with_minimal_data(self):
        """SECTION_SHELL should load without error when only secid is provided."""
        from ansys.dyna.core.keywords.keyword_classes.auto.section.section_shell import SectionShell

        kwd = SectionShell()

        # Minimal data - only secid on first line, t1-t4 on second line
        # This is missing elform, nip, etc. on the first card
        data = """*SECTION_SHELL
         1
 0.1000000 0.1000000 0.1000000 0.1000000"""

        # Should not raise TypeError about NoneType
        kwd.loads(data)

        # secid should be loaded
        assert kwd.secid == 1

    @pytest.mark.keywords
    def test_initial_velocity_with_missing_nsidex(self):
        """INITIAL_VELOCITY should load without error when nsidex field is missing."""
        from ansys.dyna.core.keywords.keyword_classes.auto.boundary.initial_velocity import (
            InitialVelocity,
        )

        kwd = InitialVelocity()

        # Data with just nsid, missing nsidex and other fields
        # The active_func for card 2 is `lambda: self.nsidex > 0`
        # which would fail if nsidex is None
        data = """*INITIAL_VELOCITY
95
5.,-1.,0.,0.,0.,100."""

        # Should not raise TypeError about NoneType comparison
        kwd.loads(data)

        # nsid should be loaded
        assert kwd.nsid == 95

    @pytest.mark.keywords
    def test_mat_077_h_with_missing_n_field(self):
        """MAT_077_H should load without error when n field is missing."""
        from ansys.dyna.core.keywords.keyword_classes.auto.mat.mat_077_h import Mat077H

        kwd = Mat077H()

        # Minimal data - the active_func for some cards is `lambda: self.n > 0`
        data = """*MAT_077_H
         1     1.0     0.3"""

        # Should not raise TypeError
        kwd.loads(data)

        assert kwd.mid == 1
