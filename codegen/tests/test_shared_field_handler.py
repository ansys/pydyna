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

"""Tests for SharedFieldHandler — shared-field decoration and after_read plumbing."""

import pytest

from keyword_generation.data_model.keyword_data import Card, Field, KeywordData
from keyword_generation.handlers.shared_field import SharedFieldHandler


def _field(name: str) -> Field:
    return Field(name=name, type="float", position=0, width=10)


def _kwd_data(*card_field_names) -> KeywordData:
    """Build a KeywordData with one Field per card, named by card_field_names."""
    cards = [
        Card(index=i, fields=[_field(name)])
        for i, name in enumerate(card_field_names)
    ]
    kwd = KeywordData(keyword="TEST", subkeyword="KWD", title="Test", classname="TestKwd")
    kwd.cards = cards
    return kwd


class TestSharedFieldHandlerDirect:
    """handler.handle() with fields directly on the keyword (no card-set)."""

    def test_sets_card_indices_on_first_field(self):
        kwd = _kwd_data("ts1", "ts1", "ts1")
        SharedFieldHandler().handle(kwd, [{"name": "ts1", "cards": [1, 2, 3]}])
        assert kwd.cards[0].fields[0].card_indices == [1, 2, 3]

    def test_marks_duplicates_redundant(self):
        kwd = _kwd_data("ts1", "ts1", "ts1")
        SharedFieldHandler().handle(kwd, [{"name": "ts1", "cards": [1, 2, 3]}])
        assert not kwd.cards[0].fields[0].redundant
        assert kwd.cards[1].fields[0].redundant
        assert kwd.cards[2].fields[0].redundant

    def test_sets_has_shared_fields_flag(self):
        kwd = _kwd_data("ts1", "ts1")
        SharedFieldHandler().handle(kwd, [{"name": "ts1", "cards": [1, 2]}])
        assert kwd.has_shared_fields is True

    def test_populates_sync_shared_fields_on_kwd(self):
        kwd = _kwd_data("ts1", "ts1")
        SharedFieldHandler().handle(kwd, [{"name": "ts1", "cards": [1, 2]}])
        assert kwd.sync_shared_fields == [{"name": "ts1", "card_indices": [1, 2]}]

    def test_multiple_fields_accumulate(self):
        kwd = _kwd_data("ts1", "ts1", "ts2", "ts2")
        SharedFieldHandler().handle(kwd, [
            {"name": "ts1", "cards": [1, 2]},
            {"name": "ts2", "cards": [3, 4]},
        ])
        names = [sf["name"] for sf in kwd.sync_shared_fields]
        assert names == ["ts1", "ts2"]

    def test_negative_indices_deferred(self):
        kwd = _kwd_data("ts1")
        SharedFieldHandler().handle(kwd, [{"name": "ts1", "cards": [-1, -2]}])
        assert kwd.has_shared_fields is False
        assert kwd.sync_shared_fields == []
        assert len(kwd.negative_shared_fields) == 1


class TestSharedFieldHandlerInCardSet:
    """handler.handle() when fields live inside a card-set's source_cards."""

    def _kwd_with_cardset(self, field_name: str, num_cards: int):
        """Build kwd_data where the same field appears in num_cards source_cards."""
        fields = [_field(field_name) for _ in range(num_cards)]
        source_cards = [
            Card(index=i, fields=[fields[i]])
            for i in range(num_cards)
        ]
        card_set = {"name": "MyCardSet", "source_cards": source_cards}
        kwd = KeywordData(keyword="TEST", subkeyword="KWD", title="Test", classname="TestKwd")
        # cards in kwd_data are the same objects as in source_cards (handler scans kwd_data.cards)
        kwd.cards = list(source_cards)
        kwd.card_sets = {"sets": [card_set]}
        return kwd, card_set

    def test_places_sync_info_on_card_set(self):
        kwd, card_set = self._kwd_with_cardset("ts1", 3)
        SharedFieldHandler().handle(kwd, [{"name": "ts1", "cards": [1, 2, 3]}])
        assert card_set.get("sync_shared_fields") == [{"name": "ts1", "card_indices": [1, 2, 3]}]

    def test_does_not_add_to_kwd_sync_shared_fields(self):
        kwd, _ = self._kwd_with_cardset("ts1", 2)
        SharedFieldHandler().handle(kwd, [{"name": "ts1", "cards": [1, 2]}])
        assert kwd.sync_shared_fields == []

    def test_multiple_shared_fields_on_card_set(self):
        fields_ts1 = [_field("ts1"), _field("ts1")]
        fields_ts2 = [_field("ts2"), _field("ts2")]
        source_cards = [
            Card(index=0, fields=[fields_ts1[0], fields_ts2[0]]),
            Card(index=1, fields=[fields_ts1[1], fields_ts2[1]]),
        ]
        card_set = {"name": "MyCardSet", "source_cards": source_cards}
        kwd = KeywordData(keyword="TEST", subkeyword="KWD", title="Test", classname="TestKwd")
        kwd.cards = list(source_cards)
        kwd.card_sets = {"sets": [card_set]}

        SharedFieldHandler().handle(kwd, [
            {"name": "ts1", "cards": [1, 2]},
            {"name": "ts2", "cards": [1, 2]},
        ])
        names = [sf["name"] for sf in card_set.get("sync_shared_fields", [])]
        assert names == ["ts1", "ts2"]
        assert kwd.sync_shared_fields == []
