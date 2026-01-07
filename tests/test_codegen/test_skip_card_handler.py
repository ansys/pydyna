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

"""Tests for the skip-card handler."""

import pytest

from keyword_generation.data_model.keyword_data import Card, Field, KeywordData
from keyword_generation.data_model.label_registry import CardAddress, LabelRegistry
from keyword_generation.handlers.skip_card import SkipCardHandler, SkipCardSettings


class TestSkipCardSettings:
    """Test SkipCardSettings dataclass functionality."""

    def test_settings_with_ref(self):
        """Test creating settings with ref."""
        settings = SkipCardSettings(ref="my_card")
        assert settings.ref == "my_card"

    def test_from_dict_with_ref(self):
        """Test from_dict with ref key."""
        settings = SkipCardSettings.from_dict({"ref": "data_card"})
        assert settings.ref == "data_card"

    def test_resolve_index_with_ref(self):
        """Test resolve_index resolves label to index."""
        registry = LabelRegistry(_keyword="TEST.KW")
        registry.register("target_card", CardAddress(path=[6], entity_type="card"))

        settings = SkipCardSettings(ref="target_card")
        assert settings.resolve_index(registry) == 6


class TestSkipCardHandler:
    """Test SkipCardHandler functionality."""

    @pytest.fixture
    def sample_kwd_data(self):
        """Create sample KeywordData with multiple cards."""
        return KeywordData(
            keyword="SECTION",
            subkeyword="BEAM",
            title="*SECTION_BEAM",
            cards=[
                Card(index=0, fields=[Field(name="secid", type="int", position=0, width=10)]),
                Card(index=1, fields=[Field(name="ts1", type="float", position=0, width=10)]),
                Card(index=2, fields=[Field(name="a", type="float", position=0, width=10)]),
                Card(index=3, fields=[Field(name="vol", type="float", position=0, width=10)]),
                Card(index=4, fields=[Field(name="ts1", type="float", position=0, width=10)]),
            ],
        )

    @pytest.fixture
    def handler(self):
        """Create a SkipCardHandler instance."""
        return SkipCardHandler()

    def test_handle_with_ref(self, handler, sample_kwd_data):
        """Test marking cards for removal using label reference."""
        # Set up label registry
        registry = LabelRegistry(_keyword="SECTION.BEAM")
        registry.register("dimensions_card", CardAddress(path=[1], entity_type="card"))
        registry.register("discrete_beam_card", CardAddress(path=[3], entity_type="card"))
        sample_kwd_data.label_registry = registry

        settings = [{"ref": "dimensions_card"}, {"ref": "discrete_beam_card"}]

        handler.handle(sample_kwd_data, settings)

        assert sample_kwd_data.cards[0].get("mark_for_removal") != 1
        assert sample_kwd_data.cards[1]["mark_for_removal"] == 1
        assert sample_kwd_data.cards[2].get("mark_for_removal") != 1
        assert sample_kwd_data.cards[3]["mark_for_removal"] == 1
        assert sample_kwd_data.cards[4].get("mark_for_removal") != 1

    def test_handle_with_from_cards_registry(self, handler, sample_kwd_data):
        """Test with registry created via from_cards factory."""
        initial_labels = {
            "main_card": 0,
            "skip_me": 2,
            "also_skip": 4,
        }
        registry = LabelRegistry.from_cards(
            sample_kwd_data.cards, keyword="SECTION.BEAM", initial_labels=initial_labels
        )
        sample_kwd_data.label_registry = registry

        settings = [{"ref": "skip_me"}, {"ref": "also_skip"}]

        handler.handle(sample_kwd_data, settings)

        assert sample_kwd_data.cards[0].get("mark_for_removal") != 1
        assert sample_kwd_data.cards[1].get("mark_for_removal") != 1
        assert sample_kwd_data.cards[2]["mark_for_removal"] == 1
        assert sample_kwd_data.cards[3].get("mark_for_removal") != 1
        assert sample_kwd_data.cards[4]["mark_for_removal"] == 1

    def test_handle_undefined_ref_raises(self, handler, sample_kwd_data):
        """Test that undefined label reference raises error."""
        registry = LabelRegistry(_keyword="SECTION.BEAM")
        sample_kwd_data.label_registry = registry

        settings = [{"ref": "nonexistent_card"}]

        with pytest.raises(Exception):  # UndefinedLabelError
            handler.handle(sample_kwd_data, settings)

    def test_handle_without_registry_raises(self, handler, sample_kwd_data):
        """Test that handler raises if no registry is set."""
        settings = [{"ref": "some_card"}]

        with pytest.raises(ValueError, match="requires label_registry"):
            handler.handle(sample_kwd_data, settings)

    def test_post_process_noop(self, handler, sample_kwd_data):
        """Test that post_process is a no-op."""
        # Should not raise or change anything
        handler.post_process(sample_kwd_data)


class TestSkipCardIntegration:
    """Integration tests for skip-card with the full handler pipeline."""

    def test_skip_card_with_labels(self):
        """Test skip-card with label-based references."""
        kwd_data = KeywordData(
            keyword="TEST",
            subkeyword="KW",
            title="*TEST_KW",
            cards=[
                Card(index=i, fields=[Field(name=f"f{i}", type="int", position=0, width=10)]) for i in range(5)
            ],
        )

        # Set up registry
        registry = LabelRegistry.from_cards(
            kwd_data.cards,
            keyword="TEST.KW",
            initial_labels={"skip_a": 1, "skip_b": 3},
        )
        kwd_data.label_registry = registry

        handler = SkipCardHandler()
        settings = [{"ref": "skip_a"}, {"ref": "skip_b"}]
        handler.handle(kwd_data, settings)

        # Verify the expected cards are marked
        marked_indices = [i for i, c in enumerate(kwd_data.cards) if c.get("mark_for_removal") == 1]
        assert marked_indices == [1, 3]
