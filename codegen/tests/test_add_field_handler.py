# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#

"""Tests for AddFieldHandler."""

import pytest

from keyword_generation.data_model.keyword_data import KeywordData
from keyword_generation.data_model.label_registry import LabelRegistry
from keyword_generation.handlers.add_field import AddFieldHandler


def _make_kwd_data_with_card(fields):
    """Create a minimal KeywordData with one card for testing."""
    card = {"fields": list(fields), "index": 0}
    kwd_data = KeywordData(
        keyword="ELEMENT",
        subkeyword="MASS_PART",
        title="Test",
        classname="ElementMassPart",
    )
    kwd_data.cards = [card]
    registry = LabelRegistry()
    registry.register("elements_card", card)
    kwd_data.label_registry = registry
    return kwd_data, card


class TestAddFieldHandler:
    """Test suite for AddFieldHandler."""

    def test_adds_single_field(self):
        """New field is appended to the card's field list."""
        handler = AddFieldHandler()
        existing = [{"name": "PID", "type": "integer", "position": 0, "width": 8, "default": None}]
        kwd_data, card = _make_kwd_data_with_card(existing)

        settings = [
            {
                "ref": "elements_card",
                "fields": [
                    {"name": "MWD", "type": "integer", "position": 48, "width": 8, "default": None,
                     "help": "Mass-weighted distribution flag."}
                ],
            }
        ]
        handler.handle(kwd_data, settings)

        assert len(card["fields"]) == 2
        assert card["fields"][-1]["name"] == "MWD"
        assert card["fields"][-1]["position"] == 48
        assert card["fields"][-1]["type"] == "integer"

    def test_adds_multiple_fields(self):
        """Multiple fields are all appended in order."""
        handler = AddFieldHandler()
        existing = [{"name": "PSID", "type": "integer", "position": 0, "width": 8, "default": None}]
        kwd_data, card = _make_kwd_data_with_card(existing)

        settings = [
            {
                "ref": "elements_card",
                "fields": [
                    {"name": "LCID", "type": "integer", "position": 40, "width": 8, "default": None, "link": 19},
                    {"name": "MWD", "type": "integer", "position": 48, "width": 8, "default": None},
                ],
            }
        ]
        handler.handle(kwd_data, settings)

        names = [f["name"] for f in card["fields"]]
        assert names == ["PSID", "LCID", "MWD"]

    def test_skips_duplicate_field(self):
        """A field whose name already exists is skipped with a warning."""
        handler = AddFieldHandler()
        existing = [{"name": "MWD", "type": "integer", "position": 48, "width": 8, "default": None}]
        kwd_data, card = _make_kwd_data_with_card(existing)

        settings = [
            {
                "ref": "elements_card",
                "fields": [
                    {"name": "MWD", "type": "integer", "position": 48, "width": 8, "default": None}
                ],
            }
        ]
        handler.handle(kwd_data, settings)

        # Still only one field — duplicate was skipped
        assert len(card["fields"]) == 1

    def test_duplicate_check_is_case_insensitive(self):
        """Duplicate detection is case-insensitive."""
        handler = AddFieldHandler()
        existing = [{"name": "mwd", "type": "integer", "position": 48, "width": 8, "default": None}]
        kwd_data, card = _make_kwd_data_with_card(existing)

        settings = [
            {
                "ref": "elements_card",
                "fields": [
                    {"name": "MWD", "type": "integer", "position": 48, "width": 8, "default": None}
                ],
            }
        ]
        handler.handle(kwd_data, settings)

        assert len(card["fields"]) == 1

    def test_empty_fields_list(self):
        """An empty fields list leaves the card unchanged."""
        handler = AddFieldHandler()
        existing = [{"name": "PID", "type": "integer", "position": 0, "width": 8, "default": None}]
        kwd_data, card = _make_kwd_data_with_card(existing)

        settings = [{"ref": "elements_card", "fields": []}]
        handler.handle(kwd_data, settings)

        assert len(card["fields"]) == 1

    def test_requires_label_registry(self):
        """Raises ValueError when label_registry is None."""
        handler = AddFieldHandler()
        kwd_data = KeywordData(
            keyword="ELEMENT",
            subkeyword="MASS_PART",
            title="Test",
            classname="ElementMassPart",
        )
        kwd_data.cards = [{"fields": [], "index": 0}]
        kwd_data.label_registry = None

        with pytest.raises(ValueError, match="LabelRegistry"):
            handler.handle(kwd_data, [{"ref": "elements_card", "fields": []}])
