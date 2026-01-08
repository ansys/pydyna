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

"""Tests for the series-card handler."""

from typing import List

import pytest

from keyword_generation.data_model.keyword_data import Card, Field, KeywordData
from keyword_generation.data_model.label_registry import LabelRegistry, UndefinedLabelError
from keyword_generation.data_model.metadata import VariableCardMetadata
from keyword_generation.handlers.series_card import SeriesCardHandler, SeriesCardSettings


class TestSeriesCardSettings:
    """Test SeriesCardSettings dataclass functionality."""

    def test_settings_with_ref_basic(self):
        """Test creating settings with required fields only."""
        settings = SeriesCardSettings(
            ref="my_card",
            name="my_array",
            card_size=1,
            element_width=10,
            type="float",
            help="Test array",
        )
        assert settings.ref == "my_card"
        assert settings.name == "my_array"
        assert settings.card_size == 1
        assert settings.element_width == 10
        assert settings.type == "float"
        assert settings.help == "Test array"
        assert settings.length_func is None
        assert settings.active_func is None
        assert settings.struct_info is None

    def test_settings_with_all_fields(self):
        """Test creating settings with all optional fields."""
        settings = SeriesCardSettings(
            ref="card_ref",
            name="data_points",
            card_size=2,
            element_width=8,
            type="int",
            help="Data points array",
            length_func="self.count",
            active_func="self.enabled",
            struct_info={"name": "DataPoint", "fields": []},
        )
        assert settings.ref == "card_ref"
        assert settings.length_func == "self.count"
        assert settings.active_func == "self.enabled"
        assert settings.struct_info == {"name": "DataPoint", "fields": []}

    def test_from_dict_basic(self):
        """Test from_dict with only required fields."""
        settings = SeriesCardSettings.from_dict(
            {
                "ref": "data_card",
                "name": "data_array",
                "card-size": 1,
                "element-width": 10,
                "type": "float",
                "help": "Data values",
            }
        )
        assert settings.ref == "data_card"
        assert settings.name == "data_array"
        assert settings.card_size == 1
        assert settings.element_width == 10
        assert settings.type == "float"
        assert settings.help == "Data values"

    def test_from_dict_with_optional_fields(self):
        """Test from_dict with optional fields."""
        settings = SeriesCardSettings.from_dict(
            {
                "ref": "points_card",
                "name": "points",
                "card-size": 1,
                "element-width": 20,
                "type": "float",
                "help": "Point values",
                "length-func": "self.npoints",
                "active-func": "self.has_points",
            }
        )
        assert settings.length_func == "self.npoints"
        assert settings.active_func == "self.has_points"

    def test_from_dict_missing_ref_raises(self):
        """Test from_dict raises KeyError when ref is missing."""
        with pytest.raises(KeyError):
            SeriesCardSettings.from_dict(
                {"name": "array", "card-size": 1, "element-width": 10, "type": "float", "help": "test"}
            )

    def test_from_dict_missing_name_raises(self):
        """Test from_dict raises KeyError when name is missing."""
        with pytest.raises(KeyError):
            SeriesCardSettings.from_dict(
                {"ref": "card", "card-size": 1, "element-width": 10, "type": "float", "help": "test"}
            )

    def test_resolve_index_with_ref(self):
        """Test resolve_index resolves label to index via object reference."""
        cards: List[Card] = [
            Card(index=i, fields=[Field(name=f"f{i}", type="int", position=0, width=10)]) for i in range(7)
        ]
        registry = LabelRegistry(_keyword="TEST.KW")
        registry.register("target_card", cards[6])  # Object reference

        settings = SeriesCardSettings(
            ref="target_card", name="array", card_size=1, element_width=10, type="float", help="test"
        )
        assert settings.resolve_index(registry, cards) == 6

    def test_resolve_index_undefined_label_raises(self):
        """Test resolve_index raises UndefinedLabelError for unknown label."""
        cards: List[Card] = [
            Card(index=i, fields=[Field(name=f"f{i}", type="int", position=0, width=10)]) for i in range(3)
        ]
        registry = LabelRegistry(_keyword="TEST.KW")
        settings = SeriesCardSettings(
            ref="unknown_card", name="array", card_size=1, element_width=10, type="float", help="test"
        )

        with pytest.raises(UndefinedLabelError, match="unknown_card"):
            settings.resolve_index(registry, cards)


class TestSeriesCardHandler:
    """Test SeriesCardHandler functionality."""

    @pytest.fixture
    def sample_kwd_data(self):
        """Create sample KeywordData with multiple cards."""
        return KeywordData(
            keyword="DEFINE",
            subkeyword="TABLE",
            title="*DEFINE_TABLE",
            cards=[
                Card(index=0, fields=[Field(name="tbid", type="int", position=0, width=10)]),
                Card(index=1, fields=[Field(name="x", type="float", position=0, width=10)]),
                Card(index=2, fields=[Field(name="y", type="float", position=0, width=10)]),
            ],
        )

    @pytest.fixture
    def handler(self):
        """Create a SeriesCardHandler instance."""
        return SeriesCardHandler()

    def test_handle_with_ref(self, handler, sample_kwd_data):
        """Test marking cards as series using label reference."""
        cards = sample_kwd_data.cards
        registry = LabelRegistry(_keyword="DEFINE.TABLE")
        registry.register("points_card", cards[1])  # Object reference
        sample_kwd_data.label_registry = registry

        settings = [
            {
                "ref": "points_card",
                "name": "points",
                "card-size": 1,
                "element-width": 20,
                "type": "float",
                "help": "Data points",
            }
        ]

        handler.handle(sample_kwd_data, settings)

        assert sample_kwd_data.variable is True
        var_meta = cards[1]["variable"]
        assert isinstance(var_meta, VariableCardMetadata)
        assert var_meta.name == "points"
        assert var_meta.size == 1
        assert var_meta.width == 20
        assert var_meta.type == "float"
        assert var_meta.help == "Data points"

    def test_handle_with_length_func(self, handler, sample_kwd_data):
        """Test series card with length function."""
        cards = sample_kwd_data.cards
        registry = LabelRegistry(_keyword="DEFINE.TABLE")
        registry.register("data_card", cards[2])
        sample_kwd_data.label_registry = registry

        settings = [
            {
                "ref": "data_card",
                "name": "values",
                "card-size": 1,
                "element-width": 10,
                "type": "float",
                "help": "Values",
                "length-func": "self.count",
            }
        ]

        handler.handle(sample_kwd_data, settings)

        var_meta = cards[2]["variable"]
        assert var_meta.length_func == "self.count"
        assert var_meta.active_func == ""

    def test_handle_with_active_func(self, handler, sample_kwd_data):
        """Test series card with active function."""
        cards = sample_kwd_data.cards
        registry = LabelRegistry(_keyword="DEFINE.TABLE")
        registry.register("conditional_card", cards[1])
        sample_kwd_data.label_registry = registry

        settings = [
            {
                "ref": "conditional_card",
                "name": "data",
                "card-size": 2,
                "element-width": 8,
                "type": "int",
                "help": "Conditional data",
                "active-func": "self.enabled",
            }
        ]

        handler.handle(sample_kwd_data, settings)

        var_meta = cards[1]["variable"]
        assert var_meta.active_func == "self.enabled"

    def test_handle_multiple_series(self, handler, sample_kwd_data):
        """Test marking multiple cards as series."""
        cards = sample_kwd_data.cards
        registry = LabelRegistry(_keyword="DEFINE.TABLE")
        registry.register("first_series", cards[1])
        registry.register("second_series", cards[2])
        sample_kwd_data.label_registry = registry

        settings = [
            {
                "ref": "first_series",
                "name": "x_values",
                "card-size": 1,
                "element-width": 10,
                "type": "float",
                "help": "X",
            },
            {
                "ref": "second_series",
                "name": "y_values",
                "card-size": 1,
                "element-width": 10,
                "type": "float",
                "help": "Y",
            },
        ]

        handler.handle(sample_kwd_data, settings)

        assert sample_kwd_data.variable is True
        assert cards[1]["variable"].name == "x_values"
        assert cards[2]["variable"].name == "y_values"
        # Card 0 should not have variable metadata
        assert cards[0].get("variable") is None

    def test_handle_with_from_cards_registry(self, handler, sample_kwd_data):
        """Test with registry created via from_cards factory."""
        cards = sample_kwd_data.cards
        initial_labels = {"header_card": 0, "data_card": 1}

        registry = LabelRegistry.from_cards(
            cards, keyword="DEFINE.TABLE", initial_labels=initial_labels
        )
        sample_kwd_data.label_registry = registry

        settings = [
            {
                "ref": "data_card",
                "name": "data",
                "card-size": 1,
                "element-width": 10,
                "type": "float",
                "help": "Data",
            }
        ]

        handler.handle(sample_kwd_data, settings)

        assert cards[1]["variable"].name == "data"

    def test_handle_without_registry_raises(self, handler, sample_kwd_data):
        """Test that handle raises ValueError when no registry is set."""
        settings = [
            {
                "ref": "some_card",
                "name": "array",
                "card-size": 1,
                "element-width": 10,
                "type": "float",
                "help": "test",
            }
        ]

        with pytest.raises(ValueError, match="requires LabelRegistry"):
            handler.handle(sample_kwd_data, settings)

    def test_handle_undefined_label_raises(self, handler, sample_kwd_data):
        """Test that handle raises UndefinedLabelError for unknown labels."""
        registry = LabelRegistry(_keyword="DEFINE.TABLE")
        sample_kwd_data.label_registry = registry

        settings = [
            {
                "ref": "nonexistent_card",
                "name": "array",
                "card-size": 1,
                "element-width": 10,
                "type": "float",
                "help": "test",
            }
        ]

        with pytest.raises(UndefinedLabelError, match="nonexistent_card"):
            handler.handle(sample_kwd_data, settings)

    def test_post_process_is_noop(self, handler, sample_kwd_data):
        """Test that post_process does nothing."""
        original_cards = list(sample_kwd_data.cards)
        handler.post_process(sample_kwd_data)
        assert sample_kwd_data.cards == original_cards
