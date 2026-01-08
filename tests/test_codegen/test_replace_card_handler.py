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

"""Tests for the replace-card handler."""

from unittest.mock import patch

import pytest

from keyword_generation.data_model.keyword_data import Card, Field, KeywordData
from keyword_generation.data_model.label_registry import LabelRegistry, UndefinedLabelError
from keyword_generation.handlers.replace_card import ReplaceCardHandler, ReplaceCardSettings


class TestReplaceCardSettings:
    """Test ReplaceCardSettings dataclass functionality."""

    def test_settings_creation(self):
        """Test creating settings with ref and card dict."""
        card_spec = {"source": "additional-cards", "card-name": "BLANK"}
        settings = ReplaceCardSettings(ref="target_card", card=card_spec)
        assert settings.ref == "target_card"
        assert settings.card == card_spec

    def test_from_dict(self):
        """Test from_dict parsing."""
        data = {
            "ref": "header_card",
            "card": {"source": "additional-cards", "card-name": "TITLE"}
        }
        settings = ReplaceCardSettings.from_dict(data)
        assert settings.ref == "header_card"
        assert settings.card == {"source": "additional-cards", "card-name": "TITLE"}

    def test_from_dict_missing_ref_raises(self):
        """Test from_dict raises KeyError if ref is missing."""
        with pytest.raises(KeyError):
            ReplaceCardSettings.from_dict({"card": {}})

    def test_from_dict_missing_card_raises(self):
        """Test from_dict raises KeyError if card is missing."""
        with pytest.raises(KeyError):
            ReplaceCardSettings.from_dict({"ref": "card1"})

    def test_resolve_index(self):
        """Test resolve_index resolves label to card index."""
        cards = [
            Card(index=i, fields=[Field(name=f"f{i}", type="int", position=0, width=10)])
            for i in range(3)
        ]
        registry = LabelRegistry(_keyword="TEST.KW")
        registry.register("target", cards[1])

        settings = ReplaceCardSettings(ref="target", card={})
        assert settings.resolve_index(registry, cards) == 1


class TestReplaceCardHandler:
    """Test ReplaceCardHandler functionality."""

    @pytest.fixture
    def sample_kwd_data(self):
        """Create sample KeywordData with labeled cards."""
        cards = [
            Card(
                index=0,
                fields=[
                    Field(name="pid", type="int", position=0, width=10),
                    Field(name="secid", type="int", position=10, width=10),
                ],
            ),
            Card(
                index=1,
                fields=[
                    Field(name="t1", type="float", position=0, width=10),
                    Field(name="t2", type="float", position=10, width=10),
                ],
            ),
            Card(
                index=2,
                fields=[
                    Field(name="vol", type="float", position=0, width=10),
                ],
            ),
        ]
        kwd_data = KeywordData(
            keyword="SECTION",
            subkeyword="SHELL",
            title="*SECTION_SHELL",
            cards=cards,
        )
        initial_labels = {"header": 0, "thickness": 1, "volume": 2}
        kwd_data.label_registry = LabelRegistry.from_cards(
            cards, keyword="SECTION.SHELL", initial_labels=initial_labels
        )
        return kwd_data

    @pytest.fixture
    def handler(self):
        """Create a ReplaceCardHandler instance."""
        return ReplaceCardHandler()

    @pytest.fixture
    def mock_replacement_card(self):
        """Create a mock replacement card."""
        return Card(
            index=999,  # Will be overwritten by handler
            fields=[
                Field(name="title", type="str", position=0, width=80),
            ],
        )

    def test_handle_basic_replacement(self, handler, sample_kwd_data, mock_replacement_card):
        """Test basic card replacement."""
        with patch("keyword_generation.handlers.replace_card.get_card", return_value=mock_replacement_card):
            settings = [{
                "ref": "thickness",
                "card": {"source": "additional-cards", "card-name": "TITLE"}
            }]

            handler.handle(sample_kwd_data, settings)

            # Card at index 1 should now be the replacement card
            replaced_card = sample_kwd_data.cards[1]
            assert replaced_card is mock_replacement_card
            assert replaced_card["index"] == 1  # Index should be updated
            assert len(replaced_card["fields"]) == 1
            assert replaced_card["fields"][0]["name"] == "title"

    def test_handle_replacement_updates_registry(self, handler, sample_kwd_data, mock_replacement_card):
        """Test that replacement updates the label registry reference."""
        with patch("keyword_generation.handlers.replace_card.get_card", return_value=mock_replacement_card):
            settings = [{"ref": "volume", "card": {"source": "additional-cards", "card-name": "BLANK"}}]

            original_card = sample_kwd_data.cards[2]
            handler.handle(sample_kwd_data, settings)

            # Registry should now point to the new card object
            resolved_card = sample_kwd_data.label_registry.resolve("volume")
            assert resolved_card is mock_replacement_card
            assert resolved_card is not original_card

    def test_handle_multiple_replacements(self, handler, sample_kwd_data):
        """Test replacing multiple cards."""
        replacement1 = Card(index=999, fields=[Field(name="f1", type="int", position=0, width=10)])
        replacement2 = Card(index=999, fields=[Field(name="f2", type="float", position=0, width=10)])

        def mock_get_card(card_spec):
            if card_spec["card-name"] == "CARD1":
                return replacement1
            elif card_spec["card-name"] == "CARD2":
                return replacement2

        with patch("keyword_generation.handlers.replace_card.get_card", side_effect=mock_get_card):
            settings = [
                {"ref": "header", "card": {"source": "additional-cards", "card-name": "CARD1"}},
                {"ref": "volume", "card": {"source": "additional-cards", "card-name": "CARD2"}}
            ]

            handler.handle(sample_kwd_data, settings)

            assert sample_kwd_data.cards[0] is replacement1
            assert sample_kwd_data.cards[0]["index"] == 0
            assert sample_kwd_data.cards[2] is replacement2
            assert sample_kwd_data.cards[2]["index"] == 2

    def test_handle_preserves_other_cards(self, handler, sample_kwd_data, mock_replacement_card):
        """Test that replacement doesn't affect other cards."""
        with patch("keyword_generation.handlers.replace_card.get_card", return_value=mock_replacement_card):
            original_card0 = sample_kwd_data.cards[0]
            original_card2 = sample_kwd_data.cards[2]

            settings = [{"ref": "thickness", "card": {"source": "additional-cards", "card-name": "BLANK"}}]
            handler.handle(sample_kwd_data, settings)

            # Other cards should remain unchanged
            assert sample_kwd_data.cards[0] is original_card0
            assert sample_kwd_data.cards[2] is original_card2

    def test_handle_passes_card_spec_to_get_card(self, handler, sample_kwd_data):
        """Test that the card specification is correctly passed to get_card."""
        mock_card = Card(index=999, fields=[])

        with patch("keyword_generation.handlers.replace_card.get_card", return_value=mock_card) as mock_get_card:
            card_spec = {"source": "additional-cards", "card-name": "CUSTOM_CARD"}
            settings = [{"ref": "header", "card": card_spec}]

            handler.handle(sample_kwd_data, settings)

            # Verify get_card was called with the correct card spec
            mock_get_card.assert_called_once_with(card_spec)

    def test_handle_without_registry_raises(self, handler, sample_kwd_data, mock_replacement_card):
        """Test that handler raises ValueError if label_registry is missing."""
        sample_kwd_data.label_registry = None

        with patch("keyword_generation.handlers.replace_card.get_card", return_value=mock_replacement_card):
            settings = [{"ref": "header", "card": {"source": "additional-cards", "card-name": "BLANK"}}]

            with pytest.raises(ValueError, match="requires LabelRegistry"):
                handler.handle(sample_kwd_data, settings)

    def test_handle_undefined_ref_raises(self, handler, sample_kwd_data, mock_replacement_card):
        """Test that handler raises UndefinedLabelError for unknown label."""
        with patch("keyword_generation.handlers.replace_card.get_card", return_value=mock_replacement_card):
            settings = [{"ref": "nonexistent", "card": {"source": "additional-cards", "card-name": "BLANK"}}]

            with pytest.raises(UndefinedLabelError):
                handler.handle(sample_kwd_data, settings)

    def test_post_process_noop(self, handler, sample_kwd_data):
        """Test that post_process does nothing."""
        original_cards = sample_kwd_data.cards.copy()
        handler.post_process(sample_kwd_data)
        assert sample_kwd_data.cards == original_cards
