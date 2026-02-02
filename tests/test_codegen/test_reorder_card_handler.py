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

"""Tests for the reorder-card handler."""

import pytest

from keyword_generation.data_model.keyword_data import Card, Field, KeywordData
from keyword_generation.data_model.label_registry import LabelRegistry
from keyword_generation.handlers.reorder_card import ReorderCardHandler, ReorderCardSettings


class TestReorderCardSettings:
    """Test ReorderCardSettings dataclass functionality."""

    def test_settings_creation(self):
        """Test creating settings with order list."""
        settings = ReorderCardSettings(order=[0, 2, 1, 3])
        assert settings.order == [0, 2, 1, 3]

    def test_from_dict(self):
        """Test from_dict parsing."""
        data = {"order": [1, 0, 3, 2]}
        settings = ReorderCardSettings.from_dict(data)
        assert settings.order == [1, 0, 3, 2]

    def test_from_dict_missing_order_raises(self):
        """Test from_dict raises KeyError if order is missing."""
        with pytest.raises(KeyError):
            ReorderCardSettings.from_dict({})


class TestReorderCardHandler:
    """Test ReorderCardHandler functionality."""

    @pytest.fixture
    def sample_kwd_data(self):
        """Create sample KeywordData with 5 cards."""
        return KeywordData(
            keyword="SECTION",
            subkeyword="BEAM",
            title="*SECTION_BEAM",
            cards=[
                Card(index=0, fields=[Field(name="secid", type="int", position=0, width=10)]),
                Card(index=1, fields=[Field(name="elform", type="int", position=0, width=10)]),
                Card(index=2, fields=[Field(name="shrf", type="float", position=0, width=10)]),
                Card(index=3, fields=[Field(name="qr_irid", type="int", position=0, width=10)]),
                Card(index=4, fields=[Field(name="ts1", type="float", position=0, width=10)]),
            ],
        )

    @pytest.fixture
    def handler(self):
        """Create a ReorderCardHandler instance."""
        return ReorderCardHandler()

    def test_handle_basic_reorder(self, handler, sample_kwd_data):
        """Test basic card reordering."""
        settings = [{"order": [0, 2, 1, 3, 4]}]

        original_cards = sample_kwd_data.cards.copy()
        handler.handle(sample_kwd_data, settings)

        # Card at position 0 should be original card 0
        assert sample_kwd_data.cards[0] is original_cards[0]
        # Card at position 1 should be original card 2
        assert sample_kwd_data.cards[1] is original_cards[2]
        # Card at position 2 should be original card 1
        assert sample_kwd_data.cards[2] is original_cards[1]
        # Card at position 3 should be original card 3
        assert sample_kwd_data.cards[3] is original_cards[3]
        # Card at position 4 should be original card 4
        assert sample_kwd_data.cards[4] is original_cards[4]

    def test_handle_reverse_order(self, handler, sample_kwd_data):
        """Test reversing card order."""
        settings = [{"order": [4, 3, 2, 1, 0]}]

        original_cards = sample_kwd_data.cards.copy()
        handler.handle(sample_kwd_data, settings)

        # Verify cards are in reverse order
        for i in range(5):
            assert sample_kwd_data.cards[i] is original_cards[4 - i]

    def test_handle_label_registry_tracks_reordered_cards(self, handler, sample_kwd_data):
        """Test that label registry correctly tracks cards after reordering."""
        cards = sample_kwd_data.cards

        # Create registry with initial labels
        initial_labels = {
            "header": 0,
            "dimensions": 2,
            "material": 4,
        }
        registry = LabelRegistry.from_cards(
            cards, keyword="SECTION.BEAM", initial_labels=initial_labels
        )
        sample_kwd_data.label_registry = registry

        # Reorder: [0, 2, 1, 3, 4] -> header stays at 0, dimensions moves to 1, material stays at 4
        settings = [{"order": [0, 2, 1, 3, 4]}]
        handler.handle(sample_kwd_data, settings)

        # Verify registry resolves to new indices
        assert registry.resolve_index("header", sample_kwd_data.cards) == 0
        assert registry.resolve_index("dimensions", sample_kwd_data.cards) == 1
        assert registry.resolve_index("material", sample_kwd_data.cards) == 4

    def test_handle_preserves_card_identity(self, handler, sample_kwd_data):
        """Test that reordering preserves card object identity (no copies made)."""
        original_cards = sample_kwd_data.cards.copy()
        settings = [{"order": [2, 0, 4, 1, 3]}]

        handler.handle(sample_kwd_data, settings)

        # Verify we still have the same card objects (by identity)
        assert len(sample_kwd_data.cards) == 5
        for orig_card in original_cards:
            assert orig_card in sample_kwd_data.cards

    def test_handle_single_setting_only(self, handler, sample_kwd_data):
        """Test that handler expects exactly one settings dict."""
        # Multiple settings should trigger assertion
        settings = [{"order": [0, 1, 2, 3, 4]}, {"order": [4, 3, 2, 1, 0]}]

        with pytest.raises(AssertionError, match="expects exactly 1 settings dict"):
            handler.handle(sample_kwd_data, settings)

    def test_post_process_noop(self, handler, sample_kwd_data):
        """Test that post_process does nothing."""
        original_cards = sample_kwd_data.cards.copy()
        handler.post_process(sample_kwd_data)
        assert sample_kwd_data.cards == original_cards
