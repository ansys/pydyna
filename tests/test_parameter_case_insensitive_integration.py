# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT

"""Integration tests for case-insensitive parameters with deck operations."""

import pytest

from ansys.dyna.core.lib.deck import Deck
from ansys.dyna.core.lib.parameters import ParameterSet


class TestCaseInsensitiveParameterIntegration:
    """Test case-insensitive parameters with deck operations."""

    def test_case_insensitive_parameter_expression(self):
        """Test expression evaluation with case-insensitive parameter references."""
        deck_text = """*KEYWORD
*PARAMETER
$#    prmr                                                            expression
R base       100.0
I count      5
*PARAMETER_EXPRESSION
$#    prmr                                                            expression
R result     base + count
*END"""

        deck = Deck()
        deck.loads(deck_text)

        # Check that parameters were resolved correctly
        assert deck.parameters.get("base") == 100.0
        assert deck.parameters.get("count") == 5
        assert deck.parameters.get("result") == 105.0

    def test_case_insensitive_expression_uppercase_reference(self):
        """Test expression with uppercase parameter references."""
        deck_text = """*KEYWORD
*PARAMETER
$#    prmr                                                            expression
R base       100.0
I count      5
*PARAMETER_EXPRESSION
$#    prmr                                                            expression
R result     BASE + COUNT
*END"""

        deck = Deck()
        deck.loads(deck_text)

        # Should work even with uppercase in expression (case-insensitive)
        assert deck.parameters.get("result") == 105.0

    def test_case_insensitive_expression_mixed_case_reference(self):
        """Test expression with mixed-case parameter references."""
        deck_text = """*KEYWORD
*PARAMETER
$#    prmr                                                            expression
R val1       100.0
R val2       50.0
*PARAMETER_EXPRESSION
$#    prmr                                                            expression
R result     VAL1 + VAL2
*END"""

        deck = Deck()
        deck.loads(deck_text)

        assert deck.parameters.get("result") == 150.0
        assert deck.parameters.get("Result") == 150.0

    def test_case_insensitive_with_local_parameters(self):
        """Test case-insensitive lookup with local parameters."""
        deck_text = """*KEYWORD
*PARAMETER
$#    prmr                                                            expression
R global     999.0
*PARAMETER_LOCAL
$#    prmr                                                            expression
R local      555.0
*END"""

        deck = Deck()
        deck.loads(deck_text)

        # Both should be accessible with any casing
        assert deck.parameters.get("global") == 999.0
        assert deck.parameters.get("GLOBAL") == 999.0
        assert deck.parameters.get("local") == 555.0
        assert deck.parameters.get("LOCAL") == 555.0

    def test_case_insensitive_duplicate_parameter_last_wins(self):
        """Test that last definition wins when parameters differ only in casing."""
        deck_text = """*KEYWORD
*PARAMETER
$#    prmr                                                            expression
R Param      100.0
R param      200.0
*END"""

        deck = Deck()
        deck.loads(deck_text)

        # The last one should win (case-insensitive overwrite)
        assert deck.parameters.get("param") == 200.0
        assert deck.parameters.get("Param") == 200.0
        assert deck.parameters.get("PARAM") == 200.0


class TestCaseInsensitiveLoadDataline:
    """Test case-insensitive parameter lookup in load_dataline."""

    def test_load_dataline_with_case_insensitive_parameter(self):
        """Test that load_dataline can resolve parameters case-insensitively."""
        from ansys.dyna.core.lib.kwd_line_formatter import load_dataline

        params = ParameterSet()
        params.add("rho", 10.0)

        # Spec: [(offset, width, type), ...]
        spec = [(0, 10, float)]

        # Load with different casing
        result, _ = load_dataline(spec, "     &rho", params)
        assert result[0] == 10.0

        # Also test uppercase
        result2, _ = load_dataline(spec, "     &RHO", params)
        assert result2[0] == 10.0

    def test_load_dataline_with_multiple_case_variants(self):
        """Test multiple parameters with different casings."""
        from ansys.dyna.core.lib.kwd_line_formatter import load_dataline

        params = ParameterSet()
        params.add("rho", 7850.0)
        params.add("h", 0.1)

        spec = [(0, 10, float), (10, 10, float)]

        # Load with lowercase and uppercase (fixed-width format)
        # Fixed-width fields are 10 chars each
        result, _ = load_dataline(spec, "      &rho       &h", params)
        assert result[0] == 7850.0
        assert result[1] == 0.1

    def test_load_dataline_csv_with_case_insensitive_parameter(self):
        """Test CSV format with case-insensitive parameters."""
        from ansys.dyna.core.lib.kwd_line_formatter import load_dataline

        params = ParameterSet()
        params.add("Value1", 100.0)
        params.add("VALUE2", 200.0)

        spec = [(0, 10, float), (10, 10, float)]

        # CSV format with comma-separated values
        result, _ = load_dataline(spec, "&value1,&value2", params)
        assert result[0] == 100.0
        assert result[1] == 200.0


class TestCaseInsensitiveExpressionEvaluator:
    """Test case-insensitive parameter references in expression evaluator - bonus tests."""

    def test_parameter_lookup_in_series_card_case_insensitive(self):
        """Test that SeriesCard parameter resolution is case-insensitive."""
        import io
        from ansys.dyna.core.lib.series_card import SeriesCard

        parameter_set = ParameterSet()
        parameter_set.add("x", 7850.0)

        series = SeriesCard(
            name="values",
            fields_per_card=4,
            element_width=10,
            input_type=float,
            length_func=None,
        )

        # Use different casing when referencing the parameter
        card_text = """       1.0       &x       3.0"""
        buf = io.StringIO(card_text)
        series.read(buf, parameter_set)

        assert len(series.data) == 3
        assert series.data[0] == 1.0
        assert series.data[1] == 7850.0
        assert series.data[2] == 3.0
