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

"""Tests for the insert-card handler."""

from unittest.mock import patch

import pytest

from keyword_generation.data_model.keyword_data import Card, Field, KeywordData
from keyword_generation.data_model.label_registry import LabelRegistry
from keyword_generation.handlers.insert_card import (
    InsertCardError,
    InsertCardHandler,
    InsertCardSettings,
)


class TestInsertCardSettings:
    """Test InsertCardSettings dataclass functionality."""

    def test_settings_with_after(self):
        """Test creating settings with after."""
        settings = InsertCardSettings(
            after="my_card",
            card={"source": "additional-cards", "card-name": "TEST_CARD"},
            label="new_card",
        )
        assert settings.after == "my_card"
        assert settings.before is None
        assert settings.label == "new_card"

    def test_settings_with_before(self):
        """Test creating settings with before."""
        settings = InsertCardSettings(
            before="target_card",
            card={"source": "additional-cards", "card-name": "TEST_CARD"},
            label="new_card",
        )
        assert settings.before == "target_card"
        assert settings.after is None
        assert settings.label == "new_card"

    def test_from_dict_with_after(self):
        """Test from_dict with after key."""
        settings = InsertCardSettings.from_dict({
            "after": "ref_card",
            "card": {"source": "additional-cards", "card-name": "TEST_CARD"},
            "label": "new_card",
        })
        assert settings.after == "ref_card"
        assert settings.before is None
        assert settings.label == "new_card"

    def test_from_dict_with_before(self):
        """Test from_dict with before key."""
        settings = InsertCardSettings.from_dict({
            "before": "target_card",
            "card": {"source": "additional-cards", "card-name": "TEST_CARD"},
            "label": "inserted_card",
        })
        assert settings.before == "target_card"
        assert settings.after is None
        assert settings.label == "inserted_card"

    def test_from_dict_missing_position_raises(self):
        """Test that missing both after and before raises ValueError."""
        with pytest.raises(ValueError, match="requires 'after' or 'before'"):
            InsertCardSettings.from_dict({
                "card": {"source": "additional-cards", "card-name": "TEST_CARD"},
                "label": "new_card",
            })

    def test_from_dict_both_positions_raises(self):
        """Test that having both after and before raises ValueError."""
        with pytest.raises(ValueError, match="cannot have both"):
            InsertCardSettings.from_dict({
                "after": "card_a",
                "before": "card_b",
                "card": {"source": "additional-cards", "card-name": "TEST_CARD"},
                "label": "new_card",
            })

    def test_from_dict_missing_label_raises(self):
        """Test that missing label raises ValueError."""
        with pytest.raises(ValueError, match="requires 'label'"):
            InsertCardSettings.from_dict({
                "after": "ref_card",
                "card": {"source": "additional-cards", "card-name": "TEST_CARD"},
            })


def _make_card(name: str, index: int = 0) -> Card:
    """Helper to create a card with a single field."""
    return Card(index=index, fields=[Field(name=name, type="int", position=0, width=10)])


class TestInsertCardHandler:
    """Test InsertCardHandler functionality."""

    @pytest.fixture
    def sample_kwd_data(self):
        """Create sample KeywordData with multiple cards and registry."""
        cards = [
            _make_card("field0", 0),
            _make_card("field1", 1),
            _make_card("field2", 2),
        ]
        registry = LabelRegistry.from_cards(
            cards,
            keyword="TEST.KW",
            initial_labels={"card_0": 0, "card_1": 1, "card_2": 2},
        )
        return KeywordData(
            keyword="TEST",
            subkeyword="KW",
            title="*TEST_KW",
            cards=cards,
            label_registry=registry,
        )

    @pytest.fixture
    def handler(self):
        """Create an InsertCardHandler instance."""
        return InsertCardHandler()

    @pytest.fixture
    def mock_get_card(self):
        """Mock get_card to return a simple card."""
        with patch("keyword_generation.handlers.insert_card.get_card") as mock:
            mock.side_effect = lambda cfg: _make_card(f"inserted_{cfg['card-name']}")
            yield mock

    def test_handle_insert_before(self, handler, sample_kwd_data, mock_get_card):
        """Test inserting a card before a labeled card."""
        settings = [{
            "before": "card_1",
            "card": {"source": "additional-cards", "card-name": "NEW"},
            "label": "new_card",
        }]

        handler.handle(sample_kwd_data, settings)

        # Should have 4 cards now
        assert len(sample_kwd_data.cards) == 4
        # New card should be at position 1 (before card_1)
        assert sample_kwd_data.cards[1].fields[0].name == "inserted_NEW"
        # Original card_1 should now be at position 2
        assert sample_kwd_data.cards[2].fields[0].name == "field1"
        # Label should be registered
        assert sample_kwd_data.label_registry.has_label("new_card")

    def test_handle_insert_before_first_card(self, handler, sample_kwd_data, mock_get_card):
        """Test inserting before the first card (equivalent to old 'START')."""
        settings = [{
            "before": "card_0",
            "card": {"source": "additional-cards", "card-name": "FIRST"},
            "label": "very_first",
        }]

        handler.handle(sample_kwd_data, settings)

        # New card should be at position 0
        assert len(sample_kwd_data.cards) == 4
        assert sample_kwd_data.cards[0].fields[0].name == "inserted_FIRST"
        # Original card_0 should now be at position 1
        assert sample_kwd_data.cards[1].fields[0].name == "field0"

    def test_handle_insert_after(self, handler, sample_kwd_data, mock_get_card):
        """Test inserting a card after a labeled card."""
        settings = [{
            "after": "card_1",
            "card": {"source": "additional-cards", "card-name": "NEW"},
            "label": "new_card",
        }]

        handler.handle(sample_kwd_data, settings)

        # Should have 4 cards now
        assert len(sample_kwd_data.cards) == 4
        # New card should be at position 2 (after card_1)
        assert sample_kwd_data.cards[2].fields[0].name == "inserted_NEW"
        # Original card_2 should now be at position 3
        assert sample_kwd_data.cards[3].fields[0].name == "field2"

    def test_handle_insert_after_end(self, handler, sample_kwd_data, mock_get_card):
        """Test inserting at the end of cards list."""
        settings = [{
            "after": "END",
            "card": {"source": "additional-cards", "card-name": "LAST"},
            "label": "last_card",
        }]

        handler.handle(sample_kwd_data, settings)

        # New card should be at the end
        assert len(sample_kwd_data.cards) == 4
        assert sample_kwd_data.cards[3].fields[0].name == "inserted_LAST"

    def test_handle_multiple_inserts_chained(self, handler, sample_kwd_data, mock_get_card):
        """Test multiple insertions where later ones reference earlier ones."""
        settings = [
            {
                "before": "card_0",
                "card": {"source": "additional-cards", "card-name": "A"},
                "label": "inserted_a",
            },
            {
                "after": "inserted_a",
                "card": {"source": "additional-cards", "card-name": "B"},
                "label": "inserted_b",
            },
        ]

        handler.handle(sample_kwd_data, settings)

        # Should have 5 cards now
        assert len(sample_kwd_data.cards) == 5
        # Order: inserted_a, inserted_b, card_0, card_1, card_2
        assert sample_kwd_data.cards[0].fields[0].name == "inserted_A"
        assert sample_kwd_data.cards[1].fields[0].name == "inserted_B"
        assert sample_kwd_data.cards[2].fields[0].name == "field0"

    def test_handle_without_registry_raises(self, handler):
        """Test that handler raises if no registry is set."""
        kwd_data = KeywordData(
            keyword="TEST",
            subkeyword="KW",
            title="*TEST_KW",
            cards=[_make_card("field0")],
            label_registry=None,
        )
        settings = [{
            "before": "card_0",
            "card": {"source": "additional-cards", "card-name": "NEW"},
            "label": "new_card",
        }]

        with pytest.raises(InsertCardError, match="LabelRegistry must be initialized"):
            handler.handle(kwd_data, settings)

    def test_handle_undefined_label_raises(self, handler, sample_kwd_data, mock_get_card):
        """Test that undefined label reference raises error."""
        settings = [{
            "before": "nonexistent_card",
            "card": {"source": "additional-cards", "card-name": "NEW"},
            "label": "new_card",
        }]

        with pytest.raises(Exception):  # UndefinedLabelError
            handler.handle(sample_kwd_data, settings)

    def test_post_process_noop(self, handler, sample_kwd_data):
        """Test that post_process is a no-op."""
        original_len = len(sample_kwd_data.cards)
        handler.post_process(sample_kwd_data)
        assert len(sample_kwd_data.cards) == original_len


class TestInsertCardIntegration:
    """Integration tests for insert-card with label-based positioning."""

    def test_before_replaces_start(self):
        """Test that 'before' on first card is equivalent to old 'START' behavior."""
        # This mimics what CONTACT_AUTOMATIC_SINGLE_SURFACE does in the manifest
        cards = [
            _make_card("surface", 0),
            _make_card("friction", 1),
            _make_card("scale_factor", 2),
        ]
        registry = LabelRegistry.from_cards(
            cards,
            keyword="CONTACT.AUTOMATIC_SINGLE_SURFACE",
            initial_labels={"surface_card": 0, "friction_card": 1, "scale_factor_card": 2},
        )
        kwd_data = KeywordData(
            keyword="CONTACT",
            subkeyword="AUTOMATIC_SINGLE_SURFACE",
            title="*CONTACT_AUTOMATIC_SINGLE_SURFACE",
            cards=cards,
            label_registry=registry,
        )

        handler = InsertCardHandler()

        # Mock get_card to return test cards
        with patch("keyword_generation.handlers.insert_card.get_card") as mock_get_card:
            mock_get_card.side_effect = lambda cfg: _make_card(f"contact_{cfg['card-name'][-1]}")

            # Insert 3 cards before surface_card, then chain them
            settings = [
                {
                    "before": "surface_card",
                    "card": {"source": "additional-cards", "card-name": "CONTACT_CARD_1"},
                    "label": "contact_card_1",
                },
                {
                    "after": "contact_card_1",
                    "card": {"source": "additional-cards", "card-name": "CONTACT_CARD_2"},
                    "label": "contact_card_2",
                },
                {
                    "after": "contact_card_2",
                    "card": {"source": "additional-cards", "card-name": "CONTACT_CARD_3"},
                    "label": "contact_card_3",
                },
            ]

            handler.handle(kwd_data, settings)

        # Should have 6 cards: contact_1, contact_2, contact_3, surface, friction, scale_factor
        assert len(kwd_data.cards) == 6
        assert kwd_data.cards[0].fields[0].name == "contact_1"
        assert kwd_data.cards[1].fields[0].name == "contact_2"
        assert kwd_data.cards[2].fields[0].name == "contact_3"
        assert kwd_data.cards[3].fields[0].name == "surface"

        # Verify all labels are registered and resolve correctly
        assert registry.resolve_index("contact_card_1", kwd_data.cards) == 0
        assert registry.resolve_index("contact_card_2", kwd_data.cards) == 1
        assert registry.resolve_index("contact_card_3", kwd_data.cards) == 2
        assert registry.resolve_index("surface_card", kwd_data.cards) == 3
