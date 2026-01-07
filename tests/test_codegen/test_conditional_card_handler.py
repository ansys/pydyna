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

"""Tests for the conditional-card handler with ref-only interface."""

import pytest

from keyword_generation.handlers.conditional_card import (
    ConditionalCardHandler,
    ConditionalCardSettings,
)
from keyword_generation.data_model.keyword_data import Card, Field, KeywordData
from keyword_generation.data_model.label_registry import CardAddress, LabelRegistry, UndefinedLabelError


class TestConditionalCardSettings:
    """Tests for ConditionalCardSettings dataclass."""

    def test_settings_with_ref(self):
        """Test creating settings with ref (required)."""
        settings = ConditionalCardSettings(ref="my_card", func="self.value == 1")
        assert settings.ref == "my_card"
        assert settings.func == "self.value == 1"

    def test_from_dict_with_ref(self):
        """Test creating settings from dict with ref."""
        data = {"ref": "my_card", "func": "self.elform == 3"}
        settings = ConditionalCardSettings.from_dict(data)
        assert settings.ref == "my_card"
        assert settings.func == "self.elform == 3"

    def test_from_dict_missing_ref_raises(self):
        """Test that missing ref raises KeyError."""
        data = {"func": "self.value == 1"}
        with pytest.raises(KeyError):
            ConditionalCardSettings.from_dict(data)

    def test_from_dict_missing_func_raises(self):
        """Test that missing func raises KeyError."""
        data = {"ref": "my_card"}
        with pytest.raises(KeyError):
            ConditionalCardSettings.from_dict(data)

    def test_resolve_index_with_ref(self):
        """Test resolving ref to index via registry."""
        registry = LabelRegistry(_keyword="TEST.KW")
        registry.register("my_card", CardAddress(path=[2], entity_type="card"))
        settings = ConditionalCardSettings(ref="my_card", func="self.value == 1")
        assert settings.resolve_index(registry) == 2


class TestConditionalCardHandler:
    """Tests for ConditionalCardHandler."""

    @pytest.fixture
    def sample_kwd_data(self):
        """Create sample KeywordData with multiple cards."""
        return KeywordData(
            keyword="CONTROL_IMPLICIT",
            subkeyword="AUTO",
            title="*CONTROL_IMPLICIT_AUTO",
            cards=[
                Card(index=0, fields=[Field(name="iauto", type="int", position=0, width=10)]),
                Card(index=1, fields=[Field(name="hcmin", type="float", position=0, width=10)]),
                Card(index=2, fields=[Field(name="extra", type="float", position=0, width=10)]),
            ],
        )

    @pytest.fixture
    def handler(self):
        """Create a ConditionalCardHandler instance."""
        return ConditionalCardHandler()

    def test_handle_with_ref(self, handler, sample_kwd_data):
        """Test handler adds func to card using ref."""
        registry = LabelRegistry(_keyword="CONTROL_IMPLICIT.AUTO")
        registry.register("target_card", CardAddress(path=[1], entity_type="card"))
        sample_kwd_data.label_registry = registry

        handler.handle(sample_kwd_data, [{"ref": "target_card", "func": "self.iauto == 3"}])

        assert sample_kwd_data.cards[1]["func"] == "self.iauto == 3"
        assert sample_kwd_data.cards[0].func is None
        assert sample_kwd_data.cards[2].func is None

    def test_handle_multiple_refs(self, handler, sample_kwd_data):
        """Test handler with multiple ref-based settings."""
        registry = LabelRegistry(_keyword="CONTROL_IMPLICIT.AUTO")
        registry.register("card_a", CardAddress(path=[0], entity_type="card"))
        registry.register("card_b", CardAddress(path=[2], entity_type="card"))
        sample_kwd_data.label_registry = registry

        handler.handle(
            sample_kwd_data,
            [
                {"ref": "card_a", "func": "self.type == 0"},
                {"ref": "card_b", "func": "self.type == 2"},
            ],
        )

        assert sample_kwd_data.cards[0]["func"] == "self.type == 0"
        assert sample_kwd_data.cards[1].func is None
        assert sample_kwd_data.cards[2]["func"] == "self.type == 2"

    def test_handle_with_from_cards_registry(self, handler):
        """Test handler with registry created from LabelRegistry.from_cards."""
        cards = [
            Card(index=0, fields=[Field(name="main", type="int", position=0, width=10)]),
            Card(index=1, fields=[Field(name="conditional", type="float", position=0, width=10)]),
        ]
        initial_labels = {"my_target": 1}
        registry = LabelRegistry.from_cards("TEST.KW", cards, initial_labels=initial_labels)
        kwd_data = KeywordData(
            keyword="TEST",
            subkeyword="KW",
            title="*TEST_KW",
            cards=cards,
            label_registry=registry,
        )

        handler.handle(kwd_data, [{"ref": "my_target", "func": "self.value > 0"}])

        assert kwd_data.cards[1]["func"] == "self.value > 0"

    def test_handle_undefined_ref_raises(self, handler, sample_kwd_data):
        """Test that undefined ref raises UndefinedLabelError."""
        registry = LabelRegistry(_keyword="CONTROL_IMPLICIT.AUTO")
        registry.register("other_card", CardAddress(path=[0], entity_type="card"))
        sample_kwd_data.label_registry = registry

        with pytest.raises(UndefinedLabelError):
            handler.handle(sample_kwd_data, [{"ref": "nonexistent_card", "func": "self.x == 1"}])

    def test_handle_without_registry_raises(self, handler):
        """Test that missing registry raises ValueError."""
        kwd_data = KeywordData(
            keyword="TEST",
            subkeyword="KW",
            title="*TEST_KW",
            cards=[Card(index=0, fields=[Field(name="x", type="int", position=0, width=10)])],
            label_registry=None,
        )

        with pytest.raises(ValueError, match="requires LabelRegistry"):
            handler.handle(kwd_data, [{"ref": "any_card", "func": "self.x == 1"}])

    def test_post_process_noop(self, handler, sample_kwd_data):
        """Test that post_process does nothing."""
        registry = LabelRegistry(_keyword="CONTROL_IMPLICIT.AUTO")
        sample_kwd_data.label_registry = registry

        handler.post_process(sample_kwd_data)
        # No assertion needed - just verifying it doesn't raise


class TestConditionalCardIntegration:
    """Integration tests for conditional-card handler with labels."""

    def test_conditional_card_with_labels(self):
        """Test full workflow with labels section."""
        cards = [
            Card(index=0, fields=[Field(name="elform", type="int", position=0, width=10)]),
            Card(index=1, fields=[Field(name="ts1", type="float", position=0, width=10)]),
            Card(index=2, fields=[Field(name="a", type="float", position=0, width=10)]),
            Card(index=3, fields=[Field(name="rampt", type="float", position=0, width=10)]),
        ]
        labels = {
            "elform_1_11_dimensions": 1,
            "elform_2_13_properties": 2,
            "elform_3_cable": 3,
        }
        registry = LabelRegistry.from_cards("SECTION.BEAM", cards, initial_labels=labels)
        kwd_data = KeywordData(
            keyword="SECTION",
            subkeyword="BEAM",
            title="*SECTION_BEAM",
            cards=cards,
            label_registry=registry,
        )

        handler = ConditionalCardHandler()
        handler.handle(
            kwd_data,
            [
                {"ref": "elform_1_11_dimensions", "func": "self.elform in [1,11]"},
                {"ref": "elform_2_13_properties", "func": "self.elform in [2,12,13]"},
                {"ref": "elform_3_cable", "func": "self.elform == 3"},
            ],
        )

        assert kwd_data.cards[1]["func"] == "self.elform in [1,11]"
        assert kwd_data.cards[2]["func"] == "self.elform in [2,12,13]"
        assert kwd_data.cards[3]["func"] == "self.elform == 3"
        assert kwd_data.cards[0].func is None
