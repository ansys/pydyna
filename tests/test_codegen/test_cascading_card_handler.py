# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
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

"""Tests for the cascading-card handler."""

import pytest

from keyword_generation.handlers.cascading_card import (
    CascadingCardHandler,
    CascadingCardSettings,
)
from keyword_generation.data_model.keyword_data import Card, Field, KeywordData


class TestCascadingCardSettings:
    """Tests for CascadingCardSettings dataclass."""

    def test_settings_basic(self):
        """Test creating settings with start and end."""
        settings = CascadingCardSettings(start=1, end=4)
        assert settings.start == 1
        assert settings.end == 4

    def test_from_dict_basic(self):
        """Test creating settings from dict."""
        data = {"start": 2, "end": 5}
        settings = CascadingCardSettings.from_dict(data)
        assert settings.start == 2
        assert settings.end == 5

    def test_from_dict_missing_start_raises(self):
        """Test that missing start raises KeyError."""
        data = {"end": 4}
        with pytest.raises(KeyError):
            CascadingCardSettings.from_dict(data)

    def test_from_dict_missing_end_raises(self):
        """Test that missing end raises KeyError."""
        data = {"start": 1}
        with pytest.raises(KeyError):
            CascadingCardSettings.from_dict(data)

    def test_from_dict_with_zero_indices(self):
        """Test that zero indices are valid."""
        data = {"start": 0, "end": 0}
        settings = CascadingCardSettings.from_dict(data)
        assert settings.start == 0
        assert settings.end == 0


class TestCascadingCardHandler:
    """Tests for CascadingCardHandler."""

    @pytest.fixture
    def sample_kwd_data(self):
        """Create sample KeywordData with 5 cards (like CONTROL_SHELL)."""
        return KeywordData(
            keyword="CONTROL",
            subkeyword="SHELL",
            title="*CONTROL_SHELL",
            cards=[
                Card(index=0, fields=[Field(name="wrpang", type="float", position=0, width=10)]),
                Card(index=1, fields=[Field(name="rotascl", type="float", position=0, width=10)]),
                Card(index=2, fields=[Field(name="psstupd", type="int", position=0, width=10)]),
                Card(index=3, fields=[Field(name="nfail1", type="int", position=0, width=10)]),
                Card(index=4, fields=[Field(name="drcmth", type="int", position=0, width=10)]),
            ],
        )

    @pytest.fixture
    def handler(self):
        """Create a CascadingCardHandler instance."""
        return CascadingCardHandler()

    def test_handle_basic_cascade(self, handler, sample_kwd_data):
        """Test handler adds cascading func to cards 1-4."""
        settings = [{"start": 1, "end": 4}]
        handler.handle(sample_kwd_data, settings)

        # Card 0 should not have func
        assert sample_kwd_data.cards[0].func is None

        # Card 1: checks itself or card 2
        assert sample_kwd_data.cards[1].func == "self._cards[1].has_nondefault_values() or self._cards[2].active"

        # Card 2: checks itself or card 3
        assert sample_kwd_data.cards[2].func == "self._cards[2].has_nondefault_values() or self._cards[3].active"

        # Card 3: checks itself or card 4
        assert sample_kwd_data.cards[3].func == "self._cards[3].has_nondefault_values() or self._cards[4].active"

        # Card 4 (last): only checks itself
        assert sample_kwd_data.cards[4].func == "self._cards[4].has_nondefault_values()"

    def test_handle_single_card_cascade(self, handler, sample_kwd_data):
        """Test handler with start == end (single card cascade)."""
        settings = [{"start": 2, "end": 2}]
        handler.handle(sample_kwd_data, settings)

        # Only card 2 should have func
        assert sample_kwd_data.cards[0].func is None
        assert sample_kwd_data.cards[1].func is None
        assert sample_kwd_data.cards[2].func == "self._cards[2].has_nondefault_values()"
        assert sample_kwd_data.cards[3].func is None
        assert sample_kwd_data.cards[4].func is None

    def test_handle_two_card_cascade(self, handler, sample_kwd_data):
        """Test handler with two cards in cascade."""
        settings = [{"start": 3, "end": 4}]
        handler.handle(sample_kwd_data, settings)

        assert sample_kwd_data.cards[0].func is None
        assert sample_kwd_data.cards[1].func is None
        assert sample_kwd_data.cards[2].func is None
        assert sample_kwd_data.cards[3].func == "self._cards[3].has_nondefault_values() or self._cards[4].active"
        assert sample_kwd_data.cards[4].func == "self._cards[4].has_nondefault_values()"

    def test_handle_cascade_starting_at_zero(self, handler, sample_kwd_data):
        """Test handler with cascade starting at card 0."""
        settings = [{"start": 0, "end": 2}]
        handler.handle(sample_kwd_data, settings)

        assert sample_kwd_data.cards[0].func == "self._cards[0].has_nondefault_values() or self._cards[1].active"
        assert sample_kwd_data.cards[1].func == "self._cards[1].has_nondefault_values() or self._cards[2].active"
        assert sample_kwd_data.cards[2].func == "self._cards[2].has_nondefault_values()"
        assert sample_kwd_data.cards[3].func is None
        assert sample_kwd_data.cards[4].func is None

    def test_handle_multiple_cascades(self, handler, sample_kwd_data):
        """Test handler with multiple non-overlapping cascades."""
        settings = [{"start": 1, "end": 2}, {"start": 3, "end": 4}]
        handler.handle(sample_kwd_data, settings)

        assert sample_kwd_data.cards[0].func is None
        assert sample_kwd_data.cards[1].func == "self._cards[1].has_nondefault_values() or self._cards[2].active"
        assert sample_kwd_data.cards[2].func == "self._cards[2].has_nondefault_values()"
        assert sample_kwd_data.cards[3].func == "self._cards[3].has_nondefault_values() or self._cards[4].active"
        assert sample_kwd_data.cards[4].func == "self._cards[4].has_nondefault_values()"

    def test_handle_start_out_of_range_raises(self, handler, sample_kwd_data):
        """Test handler raises ValueError for start index out of range."""
        settings = [{"start": 10, "end": 12}]
        with pytest.raises(ValueError, match="start index 10 out of range"):
            handler.handle(sample_kwd_data, settings)

    def test_handle_end_out_of_range_raises(self, handler, sample_kwd_data):
        """Test handler raises ValueError for end index out of range."""
        settings = [{"start": 1, "end": 10}]
        with pytest.raises(ValueError, match="end index 10 out of range"):
            handler.handle(sample_kwd_data, settings)

    def test_handle_negative_start_raises(self, handler, sample_kwd_data):
        """Test handler raises ValueError for negative start index."""
        settings = [{"start": -1, "end": 2}]
        with pytest.raises(ValueError, match="start index -1 out of range"):
            handler.handle(sample_kwd_data, settings)

    def test_handle_negative_end_raises(self, handler, sample_kwd_data):
        """Test handler raises ValueError for negative end index."""
        settings = [{"start": 0, "end": -1}]
        with pytest.raises(ValueError, match="end index -1 out of range"):
            handler.handle(sample_kwd_data, settings)

    def test_handle_start_greater_than_end_raises(self, handler, sample_kwd_data):
        """Test handler raises ValueError when start > end."""
        settings = [{"start": 3, "end": 1}]
        with pytest.raises(ValueError, match="start.*must be <= end"):
            handler.handle(sample_kwd_data, settings)

    def test_handle_empty_settings(self, handler, sample_kwd_data):
        """Test handler with empty settings does nothing."""
        settings = []
        handler.handle(sample_kwd_data, settings)

        # No cards should have func set
        for card in sample_kwd_data.cards:
            assert card.func is None

    def test_handle_overwrites_existing_func(self, handler, sample_kwd_data):
        """Test that handler overwrites any existing func on cards."""
        # Pre-set a func on card 2
        sample_kwd_data.cards[2].func = "some_existing_func()"

        settings = [{"start": 1, "end": 3}]
        handler.handle(sample_kwd_data, settings)

        # Card 2's func should be overwritten
        assert sample_kwd_data.cards[2].func == "self._cards[2].has_nondefault_values() or self._cards[3].active"


class TestCascadingCardHandlerEdgeCases:
    """Edge case tests for CascadingCardHandler."""

    @pytest.fixture
    def handler(self):
        """Create a CascadingCardHandler instance."""
        return CascadingCardHandler()

    def test_handle_with_single_card_keyword(self, handler):
        """Test handler with keyword that has only one card."""
        kwd_data = KeywordData(
            keyword="SIMPLE",
            subkeyword="KEYWORD",
            title="*SIMPLE_KEYWORD",
            cards=[Card(index=0, fields=[Field(name="value", type="int", position=0, width=10)])],
        )

        settings = [{"start": 0, "end": 0}]
        handler.handle(kwd_data, settings)

        assert kwd_data.cards[0].func == "self._cards[0].has_nondefault_values()"

    def test_handle_boundary_at_last_card(self, handler):
        """Test handler cascade ending exactly at the last card."""
        kwd_data = KeywordData(
            keyword="TEST",
            subkeyword="KW",
            title="*TEST_KW",
            cards=[
                Card(index=0, fields=[Field(name="a", type="int", position=0, width=10)]),
                Card(index=1, fields=[Field(name="b", type="int", position=0, width=10)]),
                Card(index=2, fields=[Field(name="c", type="int", position=0, width=10)]),
            ],
        )

        settings = [{"start": 0, "end": 2}]
        handler.handle(kwd_data, settings)

        assert kwd_data.cards[0].func == "self._cards[0].has_nondefault_values() or self._cards[1].active"
        assert kwd_data.cards[1].func == "self._cards[1].has_nondefault_values() or self._cards[2].active"
        assert kwd_data.cards[2].func == "self._cards[2].has_nondefault_values()"

    def test_handle_overlapping_cascades(self, handler):
        """Test handler with overlapping cascades (last one wins)."""
        kwd_data = KeywordData(
            keyword="TEST",
            subkeyword="KW",
            title="*TEST_KW",
            cards=[
                Card(index=0, fields=[Field(name="a", type="int", position=0, width=10)]),
                Card(index=1, fields=[Field(name="b", type="int", position=0, width=10)]),
                Card(index=2, fields=[Field(name="c", type="int", position=0, width=10)]),
                Card(index=3, fields=[Field(name="d", type="int", position=0, width=10)]),
            ],
        )

        # First cascade: 0-2, Second cascade: 1-3 (overlaps at 1, 2)
        settings = [{"start": 0, "end": 2}, {"start": 1, "end": 3}]
        handler.handle(kwd_data, settings)

        # Card 0 from first cascade
        assert kwd_data.cards[0].func == "self._cards[0].has_nondefault_values() or self._cards[1].active"
        # Cards 1, 2 overwritten by second cascade
        assert kwd_data.cards[1].func == "self._cards[1].has_nondefault_values() or self._cards[2].active"
        assert kwd_data.cards[2].func == "self._cards[2].has_nondefault_values() or self._cards[3].active"
        # Card 3 from second cascade
        assert kwd_data.cards[3].func == "self._cards[3].has_nondefault_values()"


class TestCascadingCardHandlerIntegration:
    """Integration tests for CascadingCardHandler with the registry."""

    def test_handler_registered_in_default_registry(self):
        """Test that cascading-card handler is registered in the default registry."""
        from keyword_generation.handlers.registry import create_default_registry

        registry = create_default_registry()
        handler_names = registry.get_handler_names()

        assert "cascading-card" in handler_names

    def test_handler_order_after_conditional_card(self):
        """Test that cascading-card runs after conditional-card."""
        from keyword_generation.handlers.registry import create_default_registry

        registry = create_default_registry()
        handler_names = registry.get_handler_names()

        conditional_idx = handler_names.index("conditional-card")
        cascading_idx = handler_names.index("cascading-card")

        assert cascading_idx > conditional_idx, "cascading-card should run after conditional-card"
