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

"""Tests for the rename-property handler."""

import pytest

from keyword_generation.data_model.keyword_data import Card, Field, KeywordData, RenamedProperty
from keyword_generation.data_model.label_registry import LabelRegistry, UndefinedLabelError
from keyword_generation.handlers.rename_property import RenamePropertyHandler, RenamePropertySettings


class TestRenamePropertySettings:
    """Test RenamePropertySettings dataclass functionality."""

    def test_settings_creation(self):
        """Test creating settings with required fields."""
        settings = RenamePropertySettings(
            name="PID",
            property_name="part_id",
            ref="header_card",
            description="Part identifier"
        )
        assert settings.name == "PID"
        assert settings.property_name == "part_id"
        assert settings.ref == "header_card"
        assert settings.description == "Part identifier"

    def test_settings_creation_without_description(self):
        """Test creating settings without optional description."""
        settings = RenamePropertySettings(
            name="SECID",
            property_name="section_id",
            ref="main_card"
        )
        assert settings.name == "SECID"
        assert settings.property_name == "section_id"
        assert settings.description is None

    def test_from_dict(self):
        """Test from_dict parsing."""
        data = {
            "ref": "card1",
            "name": "PID",
            "property-name": "part_id",
            "description": "Part ID"
        }
        settings = RenamePropertySettings.from_dict(data)
        assert settings.name == "PID"
        assert settings.property_name == "part_id"
        assert settings.ref == "card1"
        assert settings.description == "Part ID"

    def test_from_dict_without_description(self):
        """Test from_dict without optional description."""
        data = {"ref": "card1", "name": "SECID", "property-name": "section_id"}
        settings = RenamePropertySettings.from_dict(data)
        assert settings.description is None

    def test_from_dict_missing_required_raises(self):
        """Test from_dict raises KeyError if required fields are missing."""
        with pytest.raises(KeyError):
            RenamePropertySettings.from_dict({"name": "PID", "property-name": "part_id"})
        with pytest.raises(KeyError):
            RenamePropertySettings.from_dict({"ref": "card1", "property-name": "part_id"})
        with pytest.raises(KeyError):
            RenamePropertySettings.from_dict({"ref": "card1", "name": "PID"})

    def test_resolve_index(self):
        """Test resolve_index resolves label to card index."""
        cards = [
            Card(index=i, fields=[Field(name=f"f{i}", type="int", position=0, width=10)])
            for i in range(3)
        ]
        registry = LabelRegistry(_keyword="TEST.KW")
        registry.register("target", cards[2])

        settings = RenamePropertySettings(name="field", property_name="prop", ref="target")
        assert settings.resolve_index(registry, cards) == 2


class TestRenamePropertyHandler:
    """Test RenamePropertyHandler functionality."""

    @pytest.fixture
    def sample_kwd_data(self):
        """Create sample KeywordData with labeled cards."""
        cards = [
            Card(
                index=0,
                fields=[
                    Field(name="pid", type="int", position=0, width=10),
                    Field(name="secid", type="int", position=10, width=10),
                ],
            ),
            Card(
                index=1,
                fields=[
                    Field(name="elform", type="int", position=0, width=10),
                    Field(name="nip", type="int", position=10, width=10),
                ],
            ),
        ]
        kwd_data = KeywordData(
            keyword="SECTION",
            subkeyword="SHELL",
            title="*SECTION_SHELL",
            cards=cards,
        )
        initial_labels = {"header": 0, "params": 1}
        kwd_data.label_registry = LabelRegistry.from_cards(
            cards, keyword="SECTION.SHELL", initial_labels=initial_labels
        )
        return kwd_data

    @pytest.fixture
    def handler(self):
        """Create a RenamePropertyHandler instance."""
        return RenamePropertyHandler()

    def test_handle_basic_rename(self, handler, sample_kwd_data):
        """Test basic property renaming."""
        settings = [{"ref": "header", "name": "pid", "property-name": "part_id"}]

        handler.handle(sample_kwd_data, settings)

        field = sample_kwd_data.cards[0]["fields"][0]
        assert field["property_name"] == "part_id"
        assert field["name"] == "pid"  # Original name unchanged

    def test_handle_adds_to_renamed_properties_list(self, handler, sample_kwd_data):
        """Test that renamed properties are tracked in renamed_properties list."""
        settings = [{"ref": "header", "name": "secid", "property-name": "section_id"}]

        assert len(sample_kwd_data.renamed_properties) == 0
        handler.handle(sample_kwd_data, settings)

        assert len(sample_kwd_data.renamed_properties) == 1
        rp = sample_kwd_data.renamed_properties[0]
        assert isinstance(rp, RenamedProperty)
        assert rp.field_name == "SECID"
        assert rp.property_name == "section_id"
        assert rp.card_index == 0

    def test_handle_with_description(self, handler, sample_kwd_data):
        """Test renaming with description."""
        settings = [{
            "ref": "params",
            "name": "nip",
            "property-name": "num_integration_points",
            "description": "number of integration points"
        }]

        handler.handle(sample_kwd_data, settings)

        assert len(sample_kwd_data.renamed_properties) == 1
        rp = sample_kwd_data.renamed_properties[0]
        assert rp.description == "number of integration points"

    def test_handle_multiple_renames(self, handler, sample_kwd_data):
        """Test renaming multiple properties."""
        settings = [
            {"ref": "header", "name": "pid", "property-name": "part_id"},
            {"ref": "header", "name": "secid", "property-name": "section_id"},
            {"ref": "params", "name": "elform", "property-name": "element_formulation"}
        ]

        handler.handle(sample_kwd_data, settings)

        assert sample_kwd_data.cards[0]["fields"][0]["property_name"] == "part_id"
        assert sample_kwd_data.cards[0]["fields"][1]["property_name"] == "section_id"
        assert sample_kwd_data.cards[1]["fields"][0]["property_name"] == "element_formulation"
        assert len(sample_kwd_data.renamed_properties) == 3

    def test_handle_case_insensitive_field_name(self, handler, sample_kwd_data):
        """Test that field name matching is case-insensitive."""
        settings = [{"ref": "header", "name": "PID", "property-name": "part_id"}]

        handler.handle(sample_kwd_data, settings)

        # Field name is "pid" but we searched for "PID"
        assert sample_kwd_data.cards[0]["fields"][0]["property_name"] == "part_id"

    def test_handle_without_registry_raises(self, handler, sample_kwd_data):
        """Test that handler raises ValueError if label_registry is missing."""
        sample_kwd_data.label_registry = None
        settings = [{"ref": "header", "name": "pid", "property-name": "part_id"}]

        with pytest.raises(ValueError, match="requires a label registry"):
            handler.handle(sample_kwd_data, settings)

    def test_handle_undefined_ref_raises(self, handler, sample_kwd_data):
        """Test that handler raises UndefinedLabelError for unknown label."""
        settings = [{"ref": "nonexistent", "name": "pid", "property-name": "part_id"}]

        with pytest.raises(UndefinedLabelError):
            handler.handle(sample_kwd_data, settings)

    def test_post_process_detects_collisions(self, handler, sample_kwd_data):
        """Test post_process detects field name collisions with renamed properties."""
        # Add a third card with a field named "pid" (same as renamed field in card 0)
        card2 = Card(
            index=2,
            fields=[Field(name="pid", type="int", position=0, width=10)]
        )
        sample_kwd_data.cards.append(card2)
        sample_kwd_data.label_registry.register("extra", card2)

        # Rename pid in card 0 to part_id
        settings = [{
            "ref": "header",
            "name": "pid",
            "property-name": "part_id",
            "description": "Part identifier"
        }]
        handler.handle(sample_kwd_data, settings)

        # Post-process should detect collision (card 2 has "pid" but it wasn't renamed)
        handler.post_process(sample_kwd_data)

        # The collision note should be added for the property "pid" (card 2's property name)
        assert "pid" in sample_kwd_data.property_collisions
        collision_note = sample_kwd_data.property_collisions["pid"]
        assert "part_id" in collision_note
        assert "Part identifier" in collision_note
        assert "Card 1" in collision_note  # Card 0 displayed as Card 1

    def test_post_process_collision_without_description(self, handler, sample_kwd_data):
        """Test post_process collision note when no description provided."""
        card2 = Card(
            index=2,
            fields=[Field(name="secid", type="int", position=0, width=10)]
        )
        sample_kwd_data.cards.append(card2)
        sample_kwd_data.label_registry.register("extra", card2)

        settings = [{"ref": "header", "name": "secid", "property-name": "section_id"}]
        handler.handle(sample_kwd_data, settings)
        handler.post_process(sample_kwd_data)

        assert "secid" in sample_kwd_data.property_collisions
        collision_note = sample_kwd_data.property_collisions["secid"]
        assert "SECID" in collision_note
        assert "section_id" in collision_note

    def test_post_process_no_collisions(self, handler, sample_kwd_data):
        """Test post_process with no collisions."""
        settings = [{"ref": "header", "name": "pid", "property-name": "part_id"}]
        handler.handle(sample_kwd_data, settings)
        handler.post_process(sample_kwd_data)

        # No collision should be detected
        assert len(sample_kwd_data.property_collisions) == 0

    def test_post_process_empty_renamed_properties(self, handler, sample_kwd_data):
        """Test post_process with no renamed properties."""
        handler.post_process(sample_kwd_data)
        assert len(sample_kwd_data.property_collisions) == 0
