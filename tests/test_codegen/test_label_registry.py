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

"""Tests for LabelRegistry and CardAddress classes."""

import pytest

from keyword_generation.data_model.label_registry import (
    CardAddress,
    DuplicateIndexError,
    DuplicateLabelError,
    LabelRegistry,
    UndefinedLabelError,
    resolve_setting_index,
    resolve_setting_indices,
)
from keyword_generation.data_model.keyword_data import Card, Field


class TestCardAddress:
    """Test CardAddress dataclass functionality."""

    def test_card_address_creation(self):
        """Test basic CardAddress creation."""
        addr = CardAddress(path=[3], entity_type="card")
        assert addr.path == [3]
        assert addr.entity_type == "card"
        assert addr.root_index == 3
        assert not addr.is_nested

    def test_card_address_nested(self):
        """Test nested CardAddress with multiple path elements."""
        addr = CardAddress(path=[3, 0, 2], entity_type="card")
        assert addr.path == [3, 0, 2]
        assert addr.root_index == 3
        assert addr.is_nested

    def test_card_address_empty_path_raises(self):
        """Test that empty path raises ValueError."""
        with pytest.raises(ValueError, match="path cannot be empty"):
            CardAddress(path=[], entity_type="card")

    def test_card_address_shifted(self):
        """Test shifting a CardAddress root index."""
        addr = CardAddress(path=[3, 1], entity_type="card")
        shifted = addr.shifted(2)
        assert shifted.path == [5, 1]
        assert shifted.entity_type == "card"
        # Original unchanged
        assert addr.path == [3, 1]

    def test_card_address_label_assignment(self):
        """Test label assignment in __post_init__ or manual."""
        addr = CardAddress(path=[3], entity_type="card", label="stress_card")
        assert addr.label == "stress_card"


class TestLabelRegistry:
    """Test LabelRegistry functionality."""

    def test_registry_creation(self):
        """Test basic registry creation."""
        registry = LabelRegistry()
        assert len(registry.get_all_labels()) == 0

    def test_register_and_resolve(self):
        """Test registering and resolving a label."""
        registry = LabelRegistry(_keyword="TEST.KEYWORD")
        addr = CardAddress(path=[5], entity_type="card")
        registry.register("stress_card", addr)

        resolved = registry.resolve("stress_card")
        assert resolved.path == [5]
        assert resolved.label == "stress_card"

    def test_resolve_index_with_label(self):
        """Test resolve_index with a string label."""
        registry = LabelRegistry(_keyword="TEST.KEYWORD")
        registry.register("main_card", CardAddress(path=[2], entity_type="card"))

        index = registry.resolve_index("main_card")
        assert index == 2

    def test_resolve_index_with_int(self):
        """Test resolve_index passes through integer indices."""
        registry = LabelRegistry(_keyword="TEST.KEYWORD")
        index = registry.resolve_index(7)
        assert index == 7

    def test_resolve_index_nested_raises(self):
        """Test resolve_index raises for nested addresses."""
        registry = LabelRegistry(_keyword="TEST.KEYWORD")
        registry.register("nested_card", CardAddress(path=[3, 0, 2], entity_type="card"))

        with pytest.raises(ValueError, match="nested address"):
            registry.resolve_index("nested_card")

    def test_resolve_undefined_label_raises(self):
        """Test resolving undefined label raises UndefinedLabelError."""
        registry = LabelRegistry(_keyword="TEST.KEYWORD")

        with pytest.raises(UndefinedLabelError, match="Undefined label 'nonexistent'"):
            registry.resolve("nonexistent")

    def test_duplicate_label_raises(self):
        """Test registering duplicate label raises DuplicateLabelError."""
        registry = LabelRegistry(_keyword="TEST.KEYWORD")
        registry.register("my_card", CardAddress(path=[1], entity_type="card"))

        with pytest.raises(DuplicateLabelError, match="already registered"):
            registry.register("my_card", CardAddress(path=[2], entity_type="card"))

    def test_has_label(self):
        """Test has_label check."""
        registry = LabelRegistry(_keyword="TEST.KEYWORD")
        registry.register("exists", CardAddress(path=[0], entity_type="card"))

        assert registry.has_label("exists")
        assert not registry.has_label("does_not_exist")

    def test_get_all_labels(self):
        """Test retrieving all registered labels."""
        registry = LabelRegistry(_keyword="TEST.KEYWORD")
        registry.register("card_a", CardAddress(path=[0], entity_type="card"))
        registry.register("card_b", CardAddress(path=[1], entity_type="card"))

        labels = registry.get_all_labels()
        assert "card_a" in labels
        assert "card_b" in labels
        assert len(labels) == 2

    def test_label_format_validation(self):
        """Test label format validation."""
        registry = LabelRegistry(_keyword="TEST.KEYWORD")

        # Valid labels
        registry.register("simple_label", CardAddress(path=[0], entity_type="card"))
        registry.register("nested/path", CardAddress(path=[1], entity_type="card"))
        registry.register("with_numbers_123", CardAddress(path=[2], entity_type="card"))

        # Invalid labels should raise
        with pytest.raises(ValueError, match="cannot be empty"):
            registry.register("", CardAddress(path=[3], entity_type="card"))

        with pytest.raises(ValueError, match="Invalid label format"):
            registry.register("has spaces", CardAddress(path=[4], entity_type="card"))

    def test_update_after_insert(self):
        """Test updating addresses after card insertion."""
        registry = LabelRegistry(_keyword="TEST.KEYWORD")
        registry.register("card_a", CardAddress(path=[0], entity_type="card"))
        registry.register("card_b", CardAddress(path=[2], entity_type="card"))
        registry.register("card_c", CardAddress(path=[4], entity_type="card"))

        # Insert at position 1 - should shift card_b and card_c
        registry.update_after_insert(insert_position=1, count=1)

        assert registry.resolve_index("card_a") == 0  # Unchanged
        assert registry.resolve_index("card_b") == 3  # Shifted by 1
        assert registry.resolve_index("card_c") == 5  # Shifted by 1

    def test_update_after_insert_multiple(self):
        """Test updating addresses after inserting multiple cards."""
        registry = LabelRegistry(_keyword="TEST.KEYWORD")
        registry.register("card_a", CardAddress(path=[1], entity_type="card"))
        registry.register("card_b", CardAddress(path=[3], entity_type="card"))

        # Insert 2 cards at position 2
        registry.update_after_insert(insert_position=2, count=2)

        assert registry.resolve_index("card_a") == 1  # Unchanged (before insert)
        assert registry.resolve_index("card_b") == 5  # Shifted by 2


class TestLabelRegistryFromCards:
    """Test LabelRegistry.from_cards factory method."""

    @pytest.fixture
    def sample_cards(self):
        """Create sample cards for testing."""
        return [
            Card(index=0, fields=[Field(name="f0", type="int", position=0, width=10)]),
            Card(index=1, fields=[Field(name="f1", type="int", position=0, width=10)]),
            Card(index=2, fields=[Field(name="f2", type="int", position=0, width=10)]),
        ]

    def test_from_cards_auto_labels(self, sample_cards):
        """Test auto-generated labels from card list."""
        registry = LabelRegistry.from_cards(sample_cards, keyword="TEST.KW")

        assert registry.has_label("card_0")
        assert registry.has_label("card_1")
        assert registry.has_label("card_2")
        assert registry.resolve_index("card_0") == 0
        assert registry.resolve_index("card_1") == 1
        assert registry.resolve_index("card_2") == 2

    def test_from_cards_with_initial_labels(self, sample_cards):
        """Test explicit labels override auto-generated ones."""
        initial_labels = {
            "main_card": 0,
            "data_card": 2,
        }
        registry = LabelRegistry.from_cards(sample_cards, keyword="TEST.KW", initial_labels=initial_labels)

        # Explicit labels work
        assert registry.resolve_index("main_card") == 0
        assert registry.resolve_index("data_card") == 2

        # Auto-generated label for card without explicit label
        assert registry.has_label("card_1")
        assert registry.resolve_index("card_1") == 1

        # Auto-generated labels should NOT exist for explicitly labeled cards
        assert not registry.has_label("card_0")
        assert not registry.has_label("card_2")

    def test_from_cards_invalid_index_raises(self, sample_cards):
        """Test that invalid indices in initial_labels raise error."""
        initial_labels = {"bad_card": 999}

        with pytest.raises(ValueError, match="Invalid index 999"):
            LabelRegistry.from_cards(sample_cards, keyword="TEST.KW", initial_labels=initial_labels)

    def test_from_cards_duplicate_index_raises(self, sample_cards):
        """Test that multiple labels for same index raises DuplicateIndexError."""
        initial_labels = {
            "label_one": 1,
            "label_two": 1,  # Same index as label_one
        }

        with pytest.raises(DuplicateIndexError, match="Index 1 has multiple labels"):
            LabelRegistry.from_cards(sample_cards, keyword="TEST.KW", initial_labels=initial_labels)


class TestResolveSettingHelpers:
    """Test the resolve_setting_index and resolve_setting_indices helper functions."""

    @pytest.fixture
    def registry(self):
        """Create a registry with some labels."""
        reg = LabelRegistry(_keyword="TEST.KW")
        reg.register("card_a", CardAddress(path=[0], entity_type="card"))
        reg.register("card_b", CardAddress(path=[3], entity_type="card"))
        reg.register("card_c", CardAddress(path=[5], entity_type="card"))
        return reg

    def test_resolve_setting_index_with_ref(self, registry):
        """Test resolve_setting_index using ref key."""
        setting = {"ref": "card_b", "func": "some_func"}
        index = resolve_setting_index(setting, registry)
        assert index == 3

    def test_resolve_setting_index_with_index(self, registry):
        """Test resolve_setting_index using index key."""
        setting = {"index": 7, "func": "some_func"}
        index = resolve_setting_index(setting, registry)
        assert index == 7

    def test_resolve_setting_index_missing_both_raises(self, registry):
        """Test resolve_setting_index raises when neither ref nor index provided."""
        setting = {"func": "some_func"}
        with pytest.raises(ValueError, match="must contain either"):
            resolve_setting_index(setting, registry)

    def test_resolve_setting_indices_with_refs(self, registry):
        """Test resolve_setting_indices using refs key."""
        setting = {"refs": ["card_a", "card_c"]}
        indices = resolve_setting_indices(setting, registry)
        assert indices == [0, 5]

    def test_resolve_setting_indices_with_indices(self, registry):
        """Test resolve_setting_indices using indices key."""
        setting = {"indices": [1, 4, 7]}
        indices = resolve_setting_indices(setting, registry)
        assert indices == [1, 4, 7]

    def test_resolve_setting_indices_missing_both_raises(self, registry):
        """Test resolve_setting_indices raises when neither refs nor indices provided."""
        setting = {"other": "value"}
        with pytest.raises(ValueError, match="must contain either"):
            resolve_setting_indices(setting, registry)
