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

"""Tests for the table-card handler."""

from typing import List

import pytest

from keyword_generation.data_model.keyword_data import Card, Field, KeywordData
from keyword_generation.data_model.label_registry import LabelRegistry, UndefinedLabelError
from keyword_generation.data_model.metadata import TableCardMetadata
from keyword_generation.handlers.table_card import TableCardHandler, TableCardSettings


class TestTableCardSettings:
    """Test TableCardSettings dataclass functionality."""

    def test_settings_with_ref(self):
        """Test creating settings with ref and property-name."""
        settings = TableCardSettings(ref="my_card", property_name="my_table")
        assert settings.ref == "my_card"
        assert settings.property_name == "my_table"
        assert settings.length_func is None
        assert settings.active_func is None

    def test_settings_with_all_fields(self):
        """Test creating settings with all optional fields."""
        settings = TableCardSettings(
            ref="card_ref",
            property_name="data_points",
            length_func="self.count",
            active_func="self.enabled",
        )
        assert settings.ref == "card_ref"
        assert settings.property_name == "data_points"
        assert settings.length_func == "self.count"
        assert settings.active_func == "self.enabled"

    def test_from_dict_basic(self):
        """Test from_dict with only required fields."""
        settings = TableCardSettings.from_dict({"ref": "data_card", "property-name": "data_table"})
        assert settings.ref == "data_card"
        assert settings.property_name == "data_table"
        assert settings.length_func is None
        assert settings.active_func is None

    def test_from_dict_with_optional_fields(self):
        """Test from_dict with optional fields."""
        settings = TableCardSettings.from_dict(
            {
                "ref": "integration_card",
                "property-name": "integration_points",
                "length-func": "self.nip",
                "active-func": "self.elform in [1, 2]",
            }
        )
        assert settings.ref == "integration_card"
        assert settings.property_name == "integration_points"
        assert settings.length_func == "self.nip"
        assert settings.active_func == "self.elform in [1, 2]"

    def test_from_dict_missing_ref_raises(self):
        """Test from_dict raises KeyError when ref is missing."""
        with pytest.raises(KeyError):
            TableCardSettings.from_dict({"property-name": "my_table"})

    def test_from_dict_missing_property_name_raises(self):
        """Test from_dict raises KeyError when property-name is missing."""
        with pytest.raises(KeyError):
            TableCardSettings.from_dict({"ref": "card_ref"})

    def test_resolve_index_with_ref(self):
        """Test resolve_index resolves label to index via object reference."""
        cards: List[Card] = [
            Card(index=i, fields=[Field(name=f"f{i}", type="int", position=0, width=10)]) for i in range(7)
        ]
        registry = LabelRegistry(_keyword="TEST.KW")
        registry.register("target_card", cards[6])  # Object reference

        settings = TableCardSettings(ref="target_card", property_name="my_table")
        assert settings.resolve_index(registry, cards) == 6

    def test_resolve_index_undefined_label_raises(self):
        """Test resolve_index raises UndefinedLabelError for unknown label."""
        cards: List[Card] = [
            Card(index=i, fields=[Field(name=f"f{i}", type="int", position=0, width=10)]) for i in range(3)
        ]
        registry = LabelRegistry(_keyword="TEST.KW")
        settings = TableCardSettings(ref="unknown_card", property_name="my_table")

        with pytest.raises(UndefinedLabelError, match="unknown_card"):
            settings.resolve_index(registry, cards)


class TestTableCardHandler:
    """Test TableCardHandler functionality."""

    @pytest.fixture
    def sample_kwd_data(self):
        """Create sample KeywordData with multiple cards."""
        return KeywordData(
            keyword="SECTION",
            subkeyword="SHELL",
            title="*SECTION_SHELL",
            cards=[
                Card(index=0, fields=[Field(name="secid", type="int", position=0, width=10)]),
                Card(index=1, fields=[Field(name="elform", type="int", position=0, width=10)]),
                Card(index=2, fields=[Field(name="nip", type="int", position=0, width=10)]),
                Card(index=3, fields=[Field(name="user_int", type="int", position=0, width=10)]),
                Card(index=4, fields=[Field(name="xi", type="float", position=0, width=10)]),
            ],
        )

    @pytest.fixture
    def handler(self):
        """Create a TableCardHandler instance."""
        return TableCardHandler()

    def test_handle_with_ref(self, handler, sample_kwd_data):
        """Test marking cards as tables using label reference."""
        cards = sample_kwd_data.cards
        registry = LabelRegistry(_keyword="SECTION.SHELL")
        registry.register("integration_points_card", cards[4])  # Object reference
        sample_kwd_data.label_registry = registry

        settings = [{"ref": "integration_points_card", "property-name": "integration_points"}]

        handler.handle(sample_kwd_data, settings)

        assert sample_kwd_data.table is True
        assert cards[4]["table"] is not None
        table_meta = cards[4]["table"]
        assert isinstance(table_meta, TableCardMetadata)
        assert table_meta.name == "integration_points"

    def test_handle_with_length_func(self, handler, sample_kwd_data):
        """Test marking table card with length function."""
        cards = sample_kwd_data.cards
        registry = LabelRegistry(_keyword="SECTION.SHELL")
        registry.register("data_card", cards[3])
        sample_kwd_data.label_registry = registry

        settings = [{"ref": "data_card", "property-name": "data_rows", "length-func": "self.nip"}]

        handler.handle(sample_kwd_data, settings)

        table_meta = cards[3]["table"]
        assert table_meta.name == "data_rows"
        assert table_meta.length_func == "self.nip"
        assert table_meta.active_func == ""

    def test_handle_with_active_func(self, handler, sample_kwd_data):
        """Test marking table card with active function."""
        cards = sample_kwd_data.cards
        registry = LabelRegistry(_keyword="SECTION.SHELL")
        registry.register("conditional_card", cards[2])
        sample_kwd_data.label_registry = registry

        settings = [
            {
                "ref": "conditional_card",
                "property-name": "conditional_data",
                "length-func": "self.count",
                "active-func": "self.elform in [1, 2]",
            }
        ]

        handler.handle(sample_kwd_data, settings)

        table_meta = cards[2]["table"]
        assert table_meta.name == "conditional_data"
        assert table_meta.length_func == "self.count"
        assert table_meta.active_func == "self.elform in [1, 2]"

    def test_handle_multiple_table_cards(self, handler, sample_kwd_data):
        """Test marking multiple cards as tables."""
        cards = sample_kwd_data.cards
        registry = LabelRegistry(_keyword="SECTION.SHELL")
        registry.register("first_table", cards[1])
        registry.register("second_table", cards[3])
        sample_kwd_data.label_registry = registry

        settings = [
            {"ref": "first_table", "property-name": "first_data"},
            {"ref": "second_table", "property-name": "second_data"},
        ]

        handler.handle(sample_kwd_data, settings)

        assert sample_kwd_data.table is True
        assert cards[1]["table"].name == "first_data"
        assert cards[3]["table"].name == "second_data"
        # Cards 0, 2, 4 should not have table metadata
        assert cards[0].get("table") is None
        assert cards[2].get("table") is None
        assert cards[4].get("table") is None

    def test_handle_with_from_cards_registry(self, handler, sample_kwd_data):
        """Test with registry created via from_cards factory."""
        cards = sample_kwd_data.cards
        initial_labels = {"main_card": 0, "table_card": 2}

        registry = LabelRegistry.from_cards(
            cards, keyword="SECTION.SHELL", initial_labels=initial_labels
        )
        sample_kwd_data.label_registry = registry

        settings = [{"ref": "table_card", "property-name": "table_data"}]

        handler.handle(sample_kwd_data, settings)

        assert cards[2]["table"].name == "table_data"

    def test_handle_without_registry_raises(self, handler, sample_kwd_data):
        """Test that handle raises ValueError when no registry is set."""
        settings = [{"ref": "some_card", "property-name": "some_table"}]

        with pytest.raises(ValueError, match="requires LabelRegistry"):
            handler.handle(sample_kwd_data, settings)

    def test_handle_undefined_label_raises(self, handler, sample_kwd_data):
        """Test that handle raises UndefinedLabelError for unknown labels."""
        registry = LabelRegistry(_keyword="SECTION.SHELL")
        sample_kwd_data.label_registry = registry

        settings = [{"ref": "nonexistent_card", "property-name": "my_table"}]

        with pytest.raises(UndefinedLabelError, match="nonexistent_card"):
            handler.handle(sample_kwd_data, settings)

    def test_post_process_is_noop(self, handler, sample_kwd_data):
        """Test that post_process does nothing."""
        original_cards = list(sample_kwd_data.cards)
        handler.post_process(sample_kwd_data)
        assert sample_kwd_data.cards == original_cards
