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

"""Tests for the table-card-group handler."""

import pytest

from keyword_generation.data_model.keyword_data import Card, Field, KeywordData
from keyword_generation.data_model.label_registry import LabelRegistry, UndefinedLabelError
from keyword_generation.handlers.table_card_group import (
    TableCardGroupHandler,
    TableCardGroupSettings,
)


class TestTableCardGroupSettings:
    """Test TableCardGroupSettings dataclass functionality."""

    def test_settings_creation(self):
        """Test creating settings with required fields."""
        settings = TableCardGroupSettings(
            refs=["card1", "card2"], property_name="table_row"
        )
        assert settings.refs == ["card1", "card2"]
        assert settings.property_name == "table_row"
        assert settings.length_func is None
        assert settings.active_func is None

    def test_settings_with_functions(self):
        """Test creating settings with optional functions."""
        settings = TableCardGroupSettings(
            refs=["card1", "card2"],
            property_name="table_row",
            length_func="self.nrows",
            active_func="self.has_table",
        )
        assert settings.length_func == "self.nrows"
        assert settings.active_func == "self.has_table"

    def test_from_dict(self):
        """Test from_dict parsing."""
        data = {"refs": ["c1", "c2", "c3"], "overall-name": "row"}
        settings = TableCardGroupSettings.from_dict(data)
        assert settings.refs == ["c1", "c2", "c3"]
        assert settings.property_name == "row"

    def test_from_dict_with_functions(self):
        """Test from_dict with optional function fields."""
        data = {
            "refs": ["c1", "c2"],
            "overall-name": "row",
            "length-func": "self.count",
            "active-func": "self.active",
        }
        settings = TableCardGroupSettings.from_dict(data)
        assert settings.length_func == "self.count"
        assert settings.active_func == "self.active"

    def test_resolve_indices(self):
        """Test resolve_indices resolves all refs to indices."""
        cards = [
            Card(index=i, fields=[Field(name=f"f{i}", type="int", position=0, width=10)])
            for i in range(5)
        ]
        registry = LabelRegistry(_keyword="TEST.KW")
        registry.register("c1", cards[1])
        registry.register("c2", cards[3])
        registry.register("c3", cards[4])

        settings = TableCardGroupSettings(refs=["c1", "c2", "c3"], property_name="row")
        indices = settings.resolve_indices(registry, cards)
        assert indices == [1, 3, 4]


class TestTableCardGroupHandler:
    """Test TableCardGroupHandler functionality."""

    @pytest.fixture
    def sample_kwd_data(self):
        """Create sample KeywordData with labeled cards."""
        cards = [
            Card(
                index=0,
                fields=[Field(name="header", type="int", position=0, width=10)],
            ),
            Card(
                index=1,
                fields=[Field(name="row_a", type="float", position=0, width=10)],
            ),
            Card(
                index=2,
                fields=[Field(name="row_b", type="float", position=0, width=10)],
            ),
            Card(
                index=3,
                fields=[Field(name="row_c", type="float", position=0, width=10)],
            ),
            Card(
                index=4,
                fields=[Field(name="footer", type="int", position=0, width=10)],
            ),
        ]
        kwd_data = KeywordData(
            keyword="TABLE",
            subkeyword="DATA",
            title="*TABLE_DATA",
            cards=cards,
        )
        initial_labels = {
            "header": 0,
            "row_a": 1,
            "row_b": 2,
            "row_c": 3,
            "footer": 4,
        }
        kwd_data.label_registry = LabelRegistry.from_cards(
            cards, keyword="TABLE.DATA", initial_labels=initial_labels
        )
        return kwd_data

    @pytest.fixture
    def handler(self):
        """Create a TableCardGroupHandler instance."""
        return TableCardGroupHandler()

    def test_handle_basic_group(self, handler, sample_kwd_data):
        """Test creating a basic table card group."""
        settings = [{"refs": ["row_a", "row_b", "row_c"], "overall-name": "table_row"}]

        handler.handle(sample_kwd_data, settings)

        # Verify table_group flag set
        assert sample_kwd_data.table_group is True

        # Verify card insertions created
        assert len(sample_kwd_data.card_insertions) == 1
        insertion = sample_kwd_data.card_insertions[0]
        assert insertion.target_index == 1  # Minimum index of grouped cards

        # Verify group card structure
        group_card = insertion.card
        assert group_card["table_group"] is True
        assert group_card["overall_name"] == "table_row"
        assert len(group_card["sub_cards"]) == 3

        # Verify sub_cards are references to original cards
        assert group_card["sub_cards"][0] is sample_kwd_data.cards[1]
        assert group_card["sub_cards"][1] is sample_kwd_data.cards[2]
        assert group_card["sub_cards"][2] is sample_kwd_data.cards[3]

        # Verify source cards marked for removal
        assert sample_kwd_data.cards[1]["mark_for_removal"] == 1
        assert sample_kwd_data.cards[2]["mark_for_removal"] == 1
        assert sample_kwd_data.cards[3]["mark_for_removal"] == 1

    def test_handle_with_length_func(self, handler, sample_kwd_data):
        """Test creating group with length function."""
        settings = [
            {
                "refs": ["row_a", "row_b"],
                "overall-name": "row",
                "length-func": "self.nrows",
            }
        ]

        handler.handle(sample_kwd_data, settings)

        group_card = sample_kwd_data.card_insertions[0].card
        assert group_card["length_func"] == "self.nrows"

    def test_handle_with_active_func(self, handler, sample_kwd_data):
        """Test creating group with active function."""
        settings = [
            {
                "refs": ["row_a", "row_b"],
                "overall-name": "row",
                "active-func": "self.has_table",
            }
        ]

        handler.handle(sample_kwd_data, settings)

        group_card = sample_kwd_data.card_insertions[0].card
        assert group_card["active_func"] == "self.has_table"

    def test_handle_with_both_functions(self, handler, sample_kwd_data):
        """Test creating group with both length and active functions."""
        settings = [
            {
                "refs": ["row_a", "row_b"],
                "overall-name": "row",
                "length-func": "self.nrows",
                "active-func": "self.active",
            }
        ]

        handler.handle(sample_kwd_data, settings)

        group_card = sample_kwd_data.card_insertions[0].card
        assert group_card["length_func"] == "self.nrows"
        assert group_card["active_func"] == "self.active"

    def test_handle_preserves_non_grouped_cards(self, handler, sample_kwd_data):
        """Test that non-grouped cards are not affected."""
        settings = [{"refs": ["row_a", "row_b"], "overall-name": "row"}]

        handler.handle(sample_kwd_data, settings)

        # Header and footer should not be marked for removal
        assert sample_kwd_data.cards[0].get("mark_for_removal") != 1
        assert sample_kwd_data.cards[4].get("mark_for_removal") != 1

    def test_handle_multiple_groups(self, handler, sample_kwd_data):
        """Test creating multiple table card groups."""
        settings = [
            {"refs": ["row_a", "row_b"], "overall-name": "group1"},
            {"refs": ["row_c"], "overall-name": "group2"},
        ]

        handler.handle(sample_kwd_data, settings)

        assert len(sample_kwd_data.card_insertions) == 2
        assert sample_kwd_data.card_insertions[0].card["overall_name"] == "group1"
        assert sample_kwd_data.card_insertions[1].card["overall_name"] == "group2"

    def test_handle_group_inserted_at_minimum_index(self, handler, sample_kwd_data):
        """Test that group is inserted at the minimum card index."""
        # Group cards at indices 2, 1, 3 (unordered refs)
        settings = [{"refs": ["row_c", "row_a", "row_b"], "overall-name": "row"}]

        handler.handle(sample_kwd_data, settings)

        # Should insert at index 1 (minimum of 1, 2, 3)
        assert sample_kwd_data.card_insertions[0].target_index == 1

    def test_handle_reference_semantics(self, handler, sample_kwd_data):
        """Test that sub_cards use reference semantics, not deep copies."""
        settings = [{"refs": ["row_a", "row_b"], "overall-name": "row"}]

        handler.handle(sample_kwd_data, settings)

        # Get the group's sub_cards
        group_card = sample_kwd_data.card_insertions[0].card
        sub_card = group_card["sub_cards"][0]

        # Modify the original card
        sample_kwd_data.cards[1]["new_field"] = "test_value"

        # Verify the modification appears in sub_card (because it's a reference)
        assert sub_card["new_field"] == "test_value"

    def test_handle_without_registry_raises(self, handler, sample_kwd_data):
        """Test that handler raises ValueError if label_registry is missing."""
        sample_kwd_data.label_registry = None
        settings = [{"refs": ["row_a"], "overall-name": "row"}]

        with pytest.raises(ValueError, match="requires label_registry"):
            handler.handle(sample_kwd_data, settings)

    def test_handle_undefined_ref_raises(self, handler, sample_kwd_data):
        """Test that handler raises UndefinedLabelError for unknown label."""
        settings = [{"refs": ["nonexistent_card"], "overall-name": "row"}]

        with pytest.raises(UndefinedLabelError):
            handler.handle(sample_kwd_data, settings)

    def test_handle_partial_undefined_refs_raises(self, handler, sample_kwd_data):
        """Test that handler raises error if any ref is undefined."""
        settings = [{"refs": ["row_a", "nonexistent", "row_b"], "overall-name": "row"}]

        with pytest.raises(UndefinedLabelError):
            handler.handle(sample_kwd_data, settings)

    def test_handle_empty_refs_list(self, handler, sample_kwd_data):
        """Test creating group with empty refs list skips group creation."""
        settings = [{"refs": [], "overall-name": "empty_group"}]

        # Empty refs should be silently skipped (no insertions)
        handler.handle(sample_kwd_data, settings)

        # Should not create any insertions for empty refs
        assert len(sample_kwd_data.card_insertions) == 0

    def test_handle_single_card_group(self, handler, sample_kwd_data):
        """Test creating group with just one card."""
        settings = [{"refs": ["row_b"], "overall-name": "single"}]

        handler.handle(sample_kwd_data, settings)

        group_card = sample_kwd_data.card_insertions[0].card
        assert len(group_card["sub_cards"]) == 1
        assert group_card["sub_cards"][0] is sample_kwd_data.cards[2]

    def test_handle_functions_default_to_empty_string(self, handler, sample_kwd_data):
        """Test that missing functions default to empty string."""
        settings = [{"refs": ["row_a"], "overall-name": "row"}]

        handler.handle(sample_kwd_data, settings)

        group_card = sample_kwd_data.card_insertions[0].card
        assert group_card["length_func"] == ""
        assert group_card["active_func"] == ""

    def test_post_process_noop(self, handler, sample_kwd_data):
        """Test that post_process does nothing."""
        original_insertions = sample_kwd_data.card_insertions.copy()
        handler.post_process(sample_kwd_data)
        assert sample_kwd_data.card_insertions == original_insertions
