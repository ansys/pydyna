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

"""Tests for the override-field handler."""

import pytest

from keyword_generation.data_model.keyword_data import Card, Field, KeywordData
from keyword_generation.data_model.label_registry import LabelRegistry, UndefinedLabelError
from keyword_generation.handlers.override_field import OverrideFieldHandler, OverrideFieldSettings


class TestOverrideFieldSettings:
    """Test OverrideFieldSettings dataclass functionality."""

    def test_settings_creation(self):
        """Test creating settings with ref, field_name, and properties."""
        settings = OverrideFieldSettings(
            ref="header_card",
            field_name="secid",
            properties={"type": "int", "readonly": True}
        )
        assert settings.ref == "header_card"
        assert settings.field_name == "secid"
        assert settings.properties == {"type": "int", "readonly": True}

    def test_from_dict(self):
        """Test from_dict parsing extracts properties correctly."""
        data = {
            "ref": "main_card",
            "name": "PID",
            "type": "int",
            "readonly": True,
            "default": 0
        }
        settings = OverrideFieldSettings.from_dict(data)
        assert settings.ref == "main_card"
        assert settings.field_name == "PID"
        assert settings.properties == {"type": "int", "readonly": True, "default": 0}

    def test_from_dict_missing_ref_raises(self):
        """Test from_dict raises KeyError if ref is missing."""
        with pytest.raises(KeyError):
            OverrideFieldSettings.from_dict({"name": "PID", "type": "int"})

    def test_from_dict_missing_name_raises(self):
        """Test from_dict raises KeyError if name is missing."""
        with pytest.raises(KeyError):
            OverrideFieldSettings.from_dict({"ref": "card1", "type": "int"})

    def test_resolve_index_with_ref(self):
        """Test resolve_index resolves label to index via registry."""
        cards = [
            Card(index=i, fields=[Field(name=f"f{i}", type="int", position=0, width=10)])
            for i in range(3)
        ]
        registry = LabelRegistry(_keyword="TEST.KW")
        registry.register("target", cards[1])

        settings = OverrideFieldSettings(ref="target", field_name="field1", properties={})
        assert settings.resolve_index(registry, cards) == 1


class TestOverrideFieldHandler:
    """Test OverrideFieldHandler functionality."""

    @pytest.fixture
    def sample_kwd_data(self):
        """Create sample KeywordData with labeled cards."""
        cards = [
            Card(
                index=0,
                fields=[
                    Field(name="secid", type="int", position=0, width=10, default=None),
                    Field(name="elform", type="int", position=10, width=10, default=2),
                ],
            ),
            Card(
                index=1,
                fields=[
                    Field(name="shrf", type="float", position=0, width=10, default=1.0),
                    Field(name="nip", type="int", position=10, width=10, default=2),
                ],
            ),
        ]
        kwd_data = KeywordData(
            keyword="SECTION",
            subkeyword="SHELL",
            title="*SECTION_SHELL",
            cards=cards,
        )
        initial_labels = {"header": 0, "integration": 1}
        kwd_data.label_registry = LabelRegistry.from_cards(
            cards, keyword="SECTION.SHELL", initial_labels=initial_labels
        )
        return kwd_data

    @pytest.fixture
    def handler(self):
        """Create an OverrideFieldHandler instance."""
        return OverrideFieldHandler()

    def test_handle_override_type(self, handler, sample_kwd_data):
        """Test overriding field type."""
        settings = [{"ref": "header", "name": "secid", "type": "float"}]

        handler.handle(sample_kwd_data, settings)

        assert sample_kwd_data.cards[0]["fields"][0]["type"] == "float"

    def test_handle_override_readonly(self, handler, sample_kwd_data):
        """Test overriding field readonly status."""
        settings = [{"ref": "integration", "name": "shrf", "readonly": True}]

        handler.handle(sample_kwd_data, settings)

        assert sample_kwd_data.cards[1]["fields"][0]["readonly"] is True

    def test_handle_override_default(self, handler, sample_kwd_data):
        """Test overriding field default value."""
        settings = [{"ref": "header", "name": "elform", "default": 16}]

        handler.handle(sample_kwd_data, settings)

        assert sample_kwd_data.cards[0]["fields"][1]["default"] == 16

    def test_handle_override_position_and_width(self, handler, sample_kwd_data):
        """Test overriding field position and width."""
        settings = [{"ref": "integration", "name": "nip", "position": 20, "width": 5}]

        handler.handle(sample_kwd_data, settings)

        field = sample_kwd_data.cards[1]["fields"][1]
        assert field["position"] == 20
        assert field["width"] == 5

    def test_handle_override_options(self, handler, sample_kwd_data):
        """Test overriding field valid options."""
        settings = [{"ref": "header", "name": "elform", "options": [1, 2, 16, 25]}]

        handler.handle(sample_kwd_data, settings)

        assert sample_kwd_data.cards[0]["fields"][1]["options"] == [1, 2, 16, 25]

    def test_handle_override_field_name(self, handler, sample_kwd_data):
        """Test overriding field name using new-name property."""
        settings = [{"ref": "header", "name": "secid", "new-name": "section_id"}]

        handler.handle(sample_kwd_data, settings)

        assert sample_kwd_data.cards[0]["fields"][0]["name"] == "section_id"

    def test_handle_override_multiple_properties(self, handler, sample_kwd_data):
        """Test overriding multiple properties at once."""
        settings = [{
            "ref": "integration",
            "name": "shrf",
            "type": "int",
            "readonly": True,
            "default": 5,
            "position": 5
        }]

        handler.handle(sample_kwd_data, settings)

        field = sample_kwd_data.cards[1]["fields"][0]
        assert field["type"] == "int"
        assert field["readonly"] is True
        assert field["default"] == 5
        assert field["position"] == 5

    def test_handle_multiple_fields(self, handler, sample_kwd_data):
        """Test overriding multiple fields across different cards."""
        settings = [
            {"ref": "header", "name": "secid", "type": "float"},
            {"ref": "integration", "name": "nip", "default": 10}
        ]

        handler.handle(sample_kwd_data, settings)

        assert sample_kwd_data.cards[0]["fields"][0]["type"] == "float"
        assert sample_kwd_data.cards[1]["fields"][1]["default"] == 10

    def test_handle_case_insensitive_field_name(self, handler, sample_kwd_data):
        """Test that field name matching is case-insensitive."""
        settings = [{"ref": "header", "name": "SECID", "type": "float"}]

        handler.handle(sample_kwd_data, settings)

        # Field name is "secid" but we searched for "SECID"
        assert sample_kwd_data.cards[0]["fields"][0]["type"] == "float"

    def test_handle_without_registry_raises(self, handler, sample_kwd_data):
        """Test that handler raises ValueError if label_registry is missing."""
        sample_kwd_data.label_registry = None
        settings = [{"ref": "header", "name": "secid", "type": "float"}]

        with pytest.raises(ValueError, match="requires LabelRegistry"):
            handler.handle(sample_kwd_data, settings)

    def test_handle_undefined_ref_raises(self, handler, sample_kwd_data):
        """Test that handler raises UndefinedLabelError for unknown label."""
        settings = [{"ref": "nonexistent_card", "name": "secid", "type": "float"}]

        with pytest.raises(UndefinedLabelError):
            handler.handle(sample_kwd_data, settings)

    def test_post_process_noop(self, handler, sample_kwd_data):
        """Test that post_process does nothing."""
        original_card_count = len(sample_kwd_data.cards)
        original_field_counts = [len(card["fields"]) for card in sample_kwd_data.cards]
        handler.post_process(sample_kwd_data)
        # Verify no changes
        assert len(sample_kwd_data.cards) == original_card_count
        for i, card in enumerate(sample_kwd_data.cards):
            assert len(card["fields"]) == original_field_counts[i]
