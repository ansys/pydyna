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

"""Tests for LabelRegistry - object-reference based card labeling."""

import pytest

from keyword_generation.data_model.label_registry import (
    CardNotFoundError,
    DuplicateLabelError,
    LabelRegistry,
    UndefinedLabelError,
)


class TestLabelRegistry:
    """Test LabelRegistry functionality."""

    def test_registry_creation(self):
        """Test basic registry creation."""
        registry = LabelRegistry(_keyword="TEST")
        assert registry._keyword == "TEST"
        assert len(registry._labels) == 0

    def test_register_and_resolve(self):
        """Test registering a card and resolving it."""
        registry = LabelRegistry(_keyword="TEST")
        card = {"fields": [{"name": "test"}]}

        registry.register("my_card", card)

        resolved = registry.resolve("my_card")
        assert resolved is card

    def test_resolve_index(self):
        """Test resolving a label to an index in a cards list."""
        registry = LabelRegistry(_keyword="TEST")
        card0 = {"fields": [{"name": "a"}]}
        card1 = {"fields": [{"name": "b"}]}
        card2 = {"fields": [{"name": "c"}]}
        cards = [card0, card1, card2]

        registry.register("card_b", card1)

        index = registry.resolve_index("card_b", cards)
        assert index == 1

    def test_resolve_index_after_insert(self):
        """Test that resolve_index finds correct position after insertion."""
        registry = LabelRegistry(_keyword="TEST")
        card0 = {"fields": [{"name": "a"}]}
        card1 = {"fields": [{"name": "b"}]}
        card2 = {"fields": [{"name": "c"}]}
        cards = [card0, card1, card2]

        # Register label for card1 (currently at index 1)
        registry.register("card_b", card1)
        assert registry.resolve_index("card_b", cards) == 1

        # Insert a new card at the beginning
        new_card = {"fields": [{"name": "new"}]}
        cards.insert(0, new_card)

        # card_b should now be at index 2
        assert registry.resolve_index("card_b", cards) == 2

    def test_resolve_index_after_remove(self):
        """Test that resolve_index finds correct position after removal."""
        registry = LabelRegistry(_keyword="TEST")
        card0 = {"fields": [{"name": "a"}]}
        card1 = {"fields": [{"name": "b"}]}
        card2 = {"fields": [{"name": "c"}]}
        cards = [card0, card1, card2]

        # Register label for card2 (currently at index 2)
        registry.register("card_c", card2)
        assert registry.resolve_index("card_c", cards) == 2

        # Remove card0
        cards.remove(card0)

        # card_c should now be at index 1
        assert registry.resolve_index("card_c", cards) == 1

    def test_duplicate_label_raises(self):
        """Test that registering duplicate label raises error."""
        registry = LabelRegistry(_keyword="TEST")
        card1 = {"fields": [{"name": "a"}]}
        card2 = {"fields": [{"name": "b"}]}

        registry.register("my_card", card1)

        with pytest.raises(DuplicateLabelError, match="already registered"):
            registry.register("my_card", card2)

    def test_undefined_label_raises(self):
        """Test that resolving undefined label raises error."""
        registry = LabelRegistry(_keyword="TEST")

        with pytest.raises(UndefinedLabelError, match="Undefined label"):
            registry.resolve("nonexistent")

    def test_card_not_found_raises(self):
        """Test that resolving index for removed card raises error."""
        registry = LabelRegistry(_keyword="TEST")
        card = {"fields": [{"name": "a"}]}
        other_card = {"fields": [{"name": "b"}]}
        cards = [other_card]  # card is NOT in the list

        registry.register("my_card", card)

        with pytest.raises(CardNotFoundError, match="not found in cards list"):
            registry.resolve_index("my_card", cards)

    def test_has_label(self):
        """Test has_label method."""
        registry = LabelRegistry(_keyword="TEST")
        card = {"fields": [{"name": "a"}]}

        assert not registry.has_label("my_card")
        registry.register("my_card", card)
        assert registry.has_label("my_card")

    def test_get_all_labels(self):
        """Test get_all_labels returns copy."""
        registry = LabelRegistry(_keyword="TEST")
        card1 = {"fields": [{"name": "a"}]}
        card2 = {"fields": [{"name": "b"}]}

        registry.register("card_a", card1)
        registry.register("card_b", card2)

        labels = registry.get_all_labels()
        assert len(labels) == 2
        assert labels["card_a"] is card1
        assert labels["card_b"] is card2

        # Verify it's a copy
        labels["card_c"] = {"new": True}
        assert not registry.has_label("card_c")

    def test_label_format_validation(self):
        """Test that invalid label formats are rejected."""
        registry = LabelRegistry(_keyword="TEST")
        card = {"fields": [{"name": "a"}]}

        # Valid labels
        registry.register("card_0", card)
        registry.register("_private", {"fields": []})
        registry.register("CamelCase", {"fields": []})

        # Invalid labels
        with pytest.raises(ValueError, match="Invalid label format"):
            registry.register("123start", {"fields": []})

        with pytest.raises(ValueError, match="Invalid label format"):
            registry.register("has space", {"fields": []})

        with pytest.raises(ValueError, match="cannot be empty"):
            registry.register("", {"fields": []})


class TestLabelRegistryFromCards:
    """Test LabelRegistry.from_cards factory method."""

    def test_from_cards_with_initial_labels(self):
        """Test creating registry from cards with initial labels."""
        card0 = {"fields": [{"name": "a"}]}
        card1 = {"fields": [{"name": "b"}]}
        card2 = {"fields": [{"name": "c"}]}
        cards = [card0, card1, card2]

        initial_labels = {
            "first_card": 0,
            "middle_card": 1,
        }

        registry = LabelRegistry.from_cards(cards, keyword="TEST", initial_labels=initial_labels)

        # Verify labels resolve to correct cards
        assert registry.resolve("first_card") is card0
        assert registry.resolve("middle_card") is card1

        # Verify index resolution works
        assert registry.resolve_index("first_card", cards) == 0
        assert registry.resolve_index("middle_card", cards) == 1

    def test_from_cards_empty_labels(self):
        """Test creating registry with no initial labels."""
        card0 = {"fields": [{"name": "a"}]}
        cards = [card0]

        registry = LabelRegistry.from_cards(cards, keyword="TEST")

        # No labels should be registered
        assert len(registry.get_all_labels()) == 0

    def test_from_cards_invalid_index_raises(self):
        """Test that invalid index in initial_labels raises error."""
        card0 = {"fields": [{"name": "a"}]}
        cards = [card0]

        with pytest.raises(ValueError, match="Invalid index"):
            LabelRegistry.from_cards(cards, keyword="TEST", initial_labels={"bad": 5})
