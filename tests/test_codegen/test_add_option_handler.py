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

"""Tests for the add-option handler."""

from unittest.mock import patch

import pytest

from keyword_generation.data_model.keyword_data import Card, Field, KeywordData
from keyword_generation.handlers.add_option import AddOptionHandler, AddOptionSettings


class TestAddOptionSettings:
    """Test AddOptionSettings dataclass functionality."""

    def test_settings_creation(self):
        """Test creating settings with all required fields."""
        cards = [{"name": "card1"}]
        settings = AddOptionSettings(
            name="ID", card_order=1, title_order=1, cards=cards, func=None
        )
        assert settings.name == "ID"
        assert settings.card_order == 1
        assert settings.title_order == 1
        assert settings.cards == cards
        assert settings.func is None

    def test_settings_with_func(self):
        """Test creating settings with optional func."""
        cards = [{"name": "card1"}]
        settings = AddOptionSettings(
            name="MPP", card_order=2, title_order=2, cards=cards, func="self.use_mpp"
        )
        assert settings.func == "self.use_mpp"

    def test_from_dict(self):
        """Test from_dict parsing."""
        data = {
            "name": "ID",
            "card-order": 1,
            "title-order": 2,
            "cards": [{"source": "cards", "card-name": "ID_CARD"}],
        }
        settings = AddOptionSettings.from_dict(data)
        assert settings.name == "ID"
        assert settings.card_order == 1
        assert settings.title_order == 2
        assert len(settings.cards) == 1
        assert settings.func is None

    def test_from_dict_with_func(self):
        """Test from_dict with optional func field."""
        data = {
            "name": "TITLE",
            "card-order": 3,
            "title-order": 1,
            "cards": [],
            "func": "self.has_title",
        }
        settings = AddOptionSettings.from_dict(data)
        assert settings.func == "self.has_title"


class TestAddOptionHandler:
    """Test AddOptionHandler functionality."""

    @pytest.fixture
    def sample_kwd_data(self):
        """Create sample KeywordData."""
        return KeywordData(
            keyword="CONTACT",
            subkeyword="",
            title="*CONTACT",
            cards=[
                Card(
                    index=0,
                    fields=[Field(name="ssid", type="int", position=0, width=10)],
                )
            ],
        )

    @pytest.fixture
    def handler(self):
        """Create an AddOptionHandler instance."""
        return AddOptionHandler()

    @pytest.fixture
    def mock_option_card(self):
        """Create a mock option card."""
        return Card(
            index=999,
            fields=[Field(name="cid", type="int", position=0, width=10)],
        )

    def test_handle_single_option(self, handler, sample_kwd_data, mock_option_card):
        """Test adding a single option group."""
        with patch(
            "keyword_generation.handlers.add_option.get_card",
            return_value=mock_option_card,
        ):
            settings = [
                {
                    "option-name": "ID",
                    "card-order": 1,
                    "title-order": 1,
                    "cards": [{"source": "cards", "card-name": "ID_CARD"}],
                }
            ]

            handler.handle(sample_kwd_data, settings)

            assert len(sample_kwd_data.options) == 1
            option = sample_kwd_data.options[0]
            assert option["name"] == "ID"
            assert option["card_order"] == 1
            assert option["title_order"] == 1
            assert len(option["cards"]) == 1
            assert option["cards"][0] is mock_option_card

    def test_handle_multiple_options(self, handler, sample_kwd_data):
        """Test adding multiple option groups."""
        card1 = Card(index=1, fields=[Field(name="id", type="int", position=0, width=10)])
        card2 = Card(index=2, fields=[Field(name="mpp", type="int", position=0, width=10)])

        def mock_get_card(spec):
            if spec.get("card-name") == "ID_CARD":
                return card1
            elif spec.get("card-name") == "MPP_CARD":
                return card2
            return Card(index=999, fields=[])

        with patch(
            "keyword_generation.handlers.add_option.get_card", side_effect=mock_get_card
        ):
            settings = [
                {
                    "option-name": "ID",
                    "card-order": 1,
                    "title-order": 1,
                    "cards": [{"source": "cards", "card-name": "ID_CARD"}],
                },
                {
                    "option-name": "MPP",
                    "card-order": 2,
                    "title-order": 2,
                    "cards": [{"source": "cards", "card-name": "MPP_CARD"}],
                },
            ]

            handler.handle(sample_kwd_data, settings)

            assert len(sample_kwd_data.options) == 2
            assert sample_kwd_data.options[0]["name"] == "ID"
            assert sample_kwd_data.options[1]["name"] == "MPP"
            assert sample_kwd_data.options[0]["cards"][0] is card1
            assert sample_kwd_data.options[1]["cards"][0] is card2

    def test_handle_option_with_active_func(self, handler, sample_kwd_data):
        """Test adding option with active function on card."""
        card_with_active = Card(
            index=1,
            fields=[Field(name="id", type="int", position=0, width=10)],
            active="self.has_id",
        )

        with patch(
            "keyword_generation.handlers.add_option.get_card",
            return_value=card_with_active,
        ):
            settings = [
                {
                    "option-name": "ID",
                    "card-order": 1,
                    "title-order": 1,
                    "cards": [{"source": "cards", "card-name": "ID_CARD"}],
                }
            ]

            handler.handle(sample_kwd_data, settings)

            # Verify that card's 'active' was expanded to 'func'
            option_card = sample_kwd_data.options[0]["cards"][0]
            assert option_card["func"] == "self.has_id"

    def test_handle_preserves_card_order(self, handler, sample_kwd_data):
        """Test that card_order and title_order are preserved."""
        mock_card = Card(index=1, fields=[])

        with patch(
            "keyword_generation.handlers.add_option.get_card", return_value=mock_card
        ):
            settings = [
                {
                    "option-name": "OPTION3",
                    "card-order": 5,
                    "title-order": 3,
                    "cards": [{"source": "cards", "card-name": "CARD"}],
                }
            ]

            handler.handle(sample_kwd_data, settings)

            option = sample_kwd_data.options[0]
            assert option["card_order"] == 5
            assert option["title_order"] == 3

    def test_handle_empty_cards_list(self, handler, sample_kwd_data):
        """Test adding option with empty cards list."""
        settings = [
            {
                "option-name": "EMPTY",
                "card-order": 1,
                "title-order": 1,
                "cards": [],
            }
        ]

        handler.handle(sample_kwd_data, settings)

        assert len(sample_kwd_data.options) == 1
        assert len(sample_kwd_data.options[0]["cards"]) == 0

    def test_handle_option_with_multiple_cards(self, handler, sample_kwd_data):
        """Test adding option with multiple cards in the group."""
        card1 = Card(index=1, fields=[Field(name="f1", type="int", position=0, width=10)])
        card2 = Card(index=2, fields=[Field(name="f2", type="int", position=0, width=10)])
        card3 = Card(index=3, fields=[Field(name="f3", type="int", position=0, width=10)])

        cards = [card1, card2, card3]
        card_idx = [0]

        def mock_get_card(spec):
            result = cards[card_idx[0]]
            card_idx[0] += 1
            return result

        with patch(
            "keyword_generation.handlers.add_option.get_card", side_effect=mock_get_card
        ):
            settings = [
                {
                    "option-name": "COMPLEX",
                    "card-order": 1,
                    "title-order": 1,
                    "cards": [
                        {"source": "cards", "card-name": "CARD1"},
                        {"source": "cards", "card-name": "CARD2"},
                        {"source": "cards", "card-name": "CARD3"},
                    ],
                }
            ]

            handler.handle(sample_kwd_data, settings)

            assert len(sample_kwd_data.options[0]["cards"]) == 3
            assert sample_kwd_data.options[0]["cards"][0] is card1
            assert sample_kwd_data.options[0]["cards"][1] is card2
            assert sample_kwd_data.options[0]["cards"][2] is card3

    def test_handle_replaces_existing_options(self, handler, sample_kwd_data):
        """Test that handle replaces any existing options."""
        sample_kwd_data.options = [{"name": "OLD", "cards": []}]

        mock_card = Card(index=1, fields=[])
        with patch(
            "keyword_generation.handlers.add_option.get_card", return_value=mock_card
        ):
            settings = [
                {
                    "option-name": "NEW",
                    "card-order": 1,
                    "title-order": 1,
                    "cards": [{"source": "cards", "card-name": "CARD"}],
                }
            ]

            handler.handle(sample_kwd_data, settings)

            assert len(sample_kwd_data.options) == 1
            assert sample_kwd_data.options[0]["name"] == "NEW"

    def test_post_process_noop(self, handler, sample_kwd_data):
        """Test that post_process does nothing."""
        original_options = sample_kwd_data.options.copy()
        handler.post_process(sample_kwd_data)
        assert sample_kwd_data.options == original_options
