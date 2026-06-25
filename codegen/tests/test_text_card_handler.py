# Copyright (C) 2023 - 2026 Synopsys, Inc. and ANSYS, Inc. All rights reserved.
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

"""Tests for TextCardHandler."""

import pytest

from keyword_generation.data_model.keyword_data import KeywordData
from keyword_generation.data_model.label_registry import LabelRegistry
from keyword_generation.data_model.metadata import TextCardMetadata
from keyword_generation.handlers.text_card import TextCardHandler


def _make_kwd_data(num_cards=1):
    """Create KeywordData with N plain cards and a LabelRegistry."""
    cards = [{"fields": [], "index": i} for i in range(num_cards)]
    kwd_data = KeywordData(
        keyword="COMMENT",
        subkeyword="COMMENT",
        title="Test",
        classname="Comment",
    )
    kwd_data.cards = cards
    registry = LabelRegistry()
    for i, card in enumerate(cards):
        registry.register(f"card_{i}", card)
    kwd_data.label_registry = registry
    return kwd_data


class TestTextCardHandler:
    """Tests for TextCardHandler."""

    def test_marks_card_as_text(self):
        """Handler sets card['text'] to a TextCardMetadata with the given name."""
        handler = TextCardHandler()
        kwd_data = _make_kwd_data(num_cards=1)

        handler.handle(kwd_data, [{"ref": "card_0", "name": "comment"}])

        card = kwd_data.cards[0]
        assert isinstance(card["text"], TextCardMetadata)
        assert card["text"].name == "comment"

    def test_sets_text_card_flag_on_kwd_data(self):
        """Handler sets kwd_data.text_card = True."""
        handler = TextCardHandler()
        kwd_data = _make_kwd_data(num_cards=1)

        assert kwd_data.text_card is False
        handler.handle(kwd_data, [{"ref": "card_0", "name": "comment"}])
        assert kwd_data.text_card is True

    def test_second_card_marked_as_text(self):
        """Handler can mark a card at index > 0 (e.g. DEFINE_FUNCTION card 1)."""
        handler = TextCardHandler()
        kwd_data = _make_kwd_data(num_cards=2)

        handler.handle(kwd_data, [{"ref": "card_1", "name": "function"}])

        assert kwd_data.cards[0].get("text") is None
        card = kwd_data.cards[1]
        assert isinstance(card["text"], TextCardMetadata)
        assert card["text"].name == "function"

    def test_raises_without_label_registry(self):
        """Handler raises ValueError when label_registry is not set."""
        handler = TextCardHandler()
        kwd_data = KeywordData(
            keyword="COMMENT",
            subkeyword="COMMENT",
            title="Test",
            classname="Comment",
        )
        kwd_data.cards = [{"fields": [], "index": 0}]
        # label_registry intentionally left as None

        with pytest.raises(ValueError, match="LabelRegistry"):
            handler.handle(kwd_data, [{"ref": "card_0", "name": "comment"}])

    def test_missing_required_key_raises(self):
        """Settings missing 'name' or 'ref' raises KeyError."""
        handler = TextCardHandler()
        kwd_data = _make_kwd_data(num_cards=1)

        with pytest.raises(KeyError):
            handler.handle(kwd_data, [{"ref": "card_0"}])  # missing 'name'
