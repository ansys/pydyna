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

"""Tests for the external-card handler."""

import pytest

from keyword_generation.data_model.keyword_data import Card, Field, KeywordData
from keyword_generation.data_model.label_registry import LabelRegistry, UndefinedLabelError
from keyword_generation.data_model.metadata import ExternalCardMetadata, MixinImport
from keyword_generation.handlers.external_card import ExternalCardHandler, ExternalCardSettings


class TestExternalCardSettings:
    """Test ExternalCardSettings dataclass functionality."""

    def test_settings_creation(self):
        """Test creating settings with all required fields."""
        settings = ExternalCardSettings(
            ref="include_card",
            card_source="include_card_module",
            card_name="IncludeCard",
            mixin_name="IncludeCardMixin"
        )
        assert settings.ref == "include_card"
        assert settings.card_source == "include_card_module"
        assert settings.card_name == "IncludeCard"
        assert settings.mixin_name == "IncludeCardMixin"

    def test_from_dict(self):
        """Test from_dict parsing."""
        data = {
            "ref": "title_card",
            "card": {
                "source": "common_cards",
                "card-name": "TitleCard"
            },
            "mixin": "TitleCardMixin"
        }
        settings = ExternalCardSettings.from_dict(data)
        assert settings.ref == "title_card"
        assert settings.card_source == "common_cards"
        assert settings.card_name == "TitleCard"
        assert settings.mixin_name == "TitleCardMixin"

    def test_resolve_index(self):
        """Test resolve_index resolves label to card index."""
        cards = [
            Card(index=i, fields=[Field(name=f"f{i}", type="int", position=0, width=10)])
            for i in range(3)
        ]
        registry = LabelRegistry(_keyword="TEST.KW")
        registry.register("ext_card", cards[1])

        settings = ExternalCardSettings(
            ref="ext_card",
            card_source="module",
            card_name="ExtCard",
            mixin_name="ExtMixin"
        )
        assert settings.resolve_index(registry, cards) == 1


class TestExternalCardHandler:
    """Test ExternalCardHandler functionality."""

    @pytest.fixture
    def sample_kwd_data(self):
        """Create sample KeywordData with labeled cards."""
        cards = [
            Card(
                index=0,
                fields=[Field(name="pid", type="int", position=0, width=10)],
            ),
            Card(
                index=1,
                fields=[Field(name="title", type="str", position=0, width=80)],
            ),
            Card(
                index=2,
                fields=[Field(name="data", type="float", position=0, width=10)],
            ),
        ]
        kwd_data = KeywordData(
            keyword="SECTION",
            subkeyword="SHELL",
            title="*SECTION_SHELL",
            cards=cards,
        )
        initial_labels = {"header": 0, "title_card": 1, "data_card": 2}
        kwd_data.label_registry = LabelRegistry.from_cards(
            cards, keyword="SECTION.SHELL", initial_labels=initial_labels
        )
        return kwd_data

    @pytest.fixture
    def handler(self):
        """Create an ExternalCardHandler instance."""
        return ExternalCardHandler()

    def test_handle_single_external_card(self, handler, sample_kwd_data):
        """Test integrating a single external card."""
        settings = [{
            "ref": "title_card",
            "card": {"source": "common_cards", "card-name": "TitleCard"},
            "mixin": "TitleCardMixin"
        }]

        handler.handle(sample_kwd_data, settings)

        # Verify mixins list created
        assert len(sample_kwd_data.mixins) == 1
        assert sample_kwd_data.mixins[0] == "TitleCardMixin"

        # Verify mixin_imports list created
        assert len(sample_kwd_data.mixin_imports) == 1
        mixin_import = sample_kwd_data.mixin_imports[0]
        assert isinstance(mixin_import, MixinImport)
        assert mixin_import.source == "ansys.dyna.core.lib.cards_.special.common_cards"
        assert "TitleCard" in mixin_import.names
        assert "TitleCardMixin" in mixin_import.names

        # Verify card marked as external
        card = sample_kwd_data.cards[1]
        assert card["external"] is not None
        assert isinstance(card["external"], ExternalCardMetadata)
        assert card["external"].name == "TitleCard"

    def test_handle_multiple_external_cards(self, handler, sample_kwd_data):
        """Test integrating multiple external cards."""
        settings = [
            {
                "ref": "title_card",
                "card": {"source": "common_cards", "card-name": "TitleCard"},
                "mixin": "TitleCardMixin"
            },
            {
                "ref": "data_card",
                "card": {"source": "data_cards", "card-name": "DataCard"},
                "mixin": "DataCardMixin"
            }
        ]

        handler.handle(sample_kwd_data, settings)

        # Verify both mixins added
        assert len(sample_kwd_data.mixins) == 2
        assert "TitleCardMixin" in sample_kwd_data.mixins
        assert "DataCardMixin" in sample_kwd_data.mixins

        # Verify both imports added
        assert len(sample_kwd_data.mixin_imports) == 2
        sources = [mi.source for mi in sample_kwd_data.mixin_imports]
        assert "ansys.dyna.core.lib.cards_.special.common_cards" in sources
        assert "ansys.dyna.core.lib.cards_.special.data_cards" in sources

        # Verify both cards marked as external
        assert sample_kwd_data.cards[1]["external"] is not None
        assert sample_kwd_data.cards[1]["external"].name == "TitleCard"
        assert sample_kwd_data.cards[2]["external"] is not None
        assert sample_kwd_data.cards[2]["external"].name == "DataCard"

    def test_handle_preserves_non_external_cards(self, handler, sample_kwd_data):
        """Test that non-external cards are not affected."""
        settings = [{
            "ref": "title_card",
            "card": {"source": "common_cards", "card-name": "TitleCard"},
            "mixin": "TitleCardMixin"
        }]

        handler.handle(sample_kwd_data, settings)

        # Non-external cards should not have 'external' set
        assert sample_kwd_data.cards[0].get("external") is None
        assert sample_kwd_data.cards[2].get("external") is None

    def test_handle_mixin_import_structure(self, handler, sample_kwd_data):
        """Test that mixin imports have correct structure for Jinja templates."""
        settings = [{
            "ref": "title_card",
            "card": {"source": "common_cards", "card-name": "TitleCard"},
            "mixin": "TitleCardMixin"
        }]

        handler.handle(sample_kwd_data, settings)

        mixin_import = sample_kwd_data.mixin_imports[0]
        # This structure is what Jinja templates expect
        assert hasattr(mixin_import, 'source')
        assert hasattr(mixin_import, 'names')
        assert isinstance(mixin_import.names, list)
        assert len(mixin_import.names) == 2  # Both card and mixin

    def test_handle_without_registry_raises(self, handler, sample_kwd_data):
        """Test that handler raises ValueError if label_registry is missing."""
        sample_kwd_data.label_registry = None
        settings = [{
            "ref": "title_card",
            "card": {"source": "common_cards", "card-name": "TitleCard"},
            "mixin": "TitleCardMixin"
        }]

        with pytest.raises(ValueError, match="requires LabelRegistry"):
            handler.handle(sample_kwd_data, settings)

    def test_handle_undefined_ref_raises(self, handler, sample_kwd_data):
        """Test that handler raises UndefinedLabelError for unknown label."""
        settings = [{
            "ref": "nonexistent_card",
            "card": {"source": "common_cards", "card-name": "TitleCard"},
            "mixin": "TitleCardMixin"
        }]

        with pytest.raises(UndefinedLabelError):
            handler.handle(sample_kwd_data, settings)

    def test_handle_external_card_metadata_for_templates(self, handler, sample_kwd_data):
        """Test that external card metadata is structured correctly for templates."""
        settings = [{
            "ref": "title_card",
            "card": {"source": "common_cards", "card-name": "TitleCard"},
            "mixin": "TitleCardMixin"
        }]

        handler.handle(sample_kwd_data, settings)

        external_meta = sample_kwd_data.cards[1]["external"]
        # Template needs to access external.name
        assert external_meta.name == "TitleCard"
        assert isinstance(external_meta, ExternalCardMetadata)

    def test_post_process_noop(self, handler, sample_kwd_data):
        """Test that post_process does nothing."""
        original_mixins = sample_kwd_data.mixins.copy() if sample_kwd_data.mixins else []
        handler.post_process(sample_kwd_data)
        assert sample_kwd_data.mixins == original_mixins
