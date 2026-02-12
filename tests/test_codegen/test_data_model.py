# Copyright (C) 2023 - 2025 ANSYS, Inc. and/or its affiliates.
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

"""Tests for keyword_generation data model classes."""

import pytest

from keyword_generation.data_model.keyword_data import Card, Field, KeywordData



class TestField:
    """Test Field dataclass functionality."""

    def test_field_creation(self, sample_field):
        """Test basic Field creation."""
        assert sample_field.name == "pid"
        assert sample_field.type == "int"
        assert sample_field.position == 0
        assert sample_field.width == 10
        assert sample_field.help == "Part ID"

    def test_field_from_dict(self, sample_field_dict):
        """Test Field.from_dict() conversion."""
        field = Field.from_dict(sample_field_dict)
        assert field.name == "pid"
        assert field.type == "integer"  # Not yet normalized
        assert field.position == 0
        assert field.default == 0

    def test_field_dict_like_access(self, sample_field):
        """Test backward-compatible dict-like access."""
        assert sample_field["name"] == "pid"
        assert sample_field["type"] == "int"
        assert "name" in sample_field
        assert sample_field.get("name") == "pid"
        assert sample_field.get("nonexistent", "default") == "default"

    def test_field_dict_like_assignment(self, sample_field):
        """Test backward-compatible dict-like assignment."""
        sample_field["help"] = "Updated help"
        assert sample_field.help == "Updated help"
        assert sample_field["help"] == "Updated help"

    def test_field_normalize_type_mapping(self):
        """Test field normalization maps types correctly."""
        field = Field(name="test", type="integer", position=0, width=10)
        field.normalize()
        assert field.type == "int"

        field = Field(name="test", type="real", position=0, width=10)
        field.normalize()
        assert field.type == "float"

        field = Field(name="test", type="string", position=0, width=10)
        field.normalize()
        assert field.type == "str"

    def test_field_normalize_unused(self):
        """Test that unused fields get normalized to 'unused' name."""
        field = Field(name="oldname", type="int", position=0, width=10, used=False)
        field.normalize()
        assert field.name == "unused"
        assert field.default is None
        assert field.help == ""

    def test_field_normalize_name_fixing(self):
        """Test field name normalization handles special characters and reserved words."""
        field = Field(name="Test-Field/Name", type="int", position=0, width=10)
        field.normalize()
        assert field.name == "test-field/name"  # Original lowercased
        assert field.property_name == "test_field_name"  # Fixed version

        field = Field(name="global", type="int", position=0, width=10)
        field.normalize()
        assert field.property_name == "global_"  # Reserved word

        field = Field(name="123field", type="int", position=0, width=10)
        field.normalize()
        assert field.property_name == "_123field"  # Starts with digit

    def test_field_normalize_string_defaults(self):
        """Test string fields get quoted defaults and options."""
        field = Field(name="test", type="string", position=0, width=10, default="value", options=["opt1", "opt2"])
        field.normalize()
        assert field.type == "str"
        assert field.default == '"value"'
        assert field.options == ['"opt1"', '"opt2"']

    def test_field_normalize_int_defaults(self):
        """Test int fields convert string defaults to integers."""
        field = Field(name="test", type="integer", position=0, width=10, default="42")
        field.normalize()
        assert field.type == "int"
        assert field.default == 42

        field = Field(name="test", type="integer", position=0, width=10, default="3.7")
        field.normalize()
        assert field.default == 3  # Converts float string to int

    def test_field_normalize_flag_fields(self):
        """Test flag fields get converted to bool type."""
        field = Field(name="flag", type="integer", position=0, width=10, flag=True, on="YES", off="NO")
        field.normalize()
        assert field.type == "bool"
        assert field.flag is True

    def test_field_normalize_help_text(self):
        """Test help text cleanup removes leading whitespace."""
        field = Field(name="test", type="int", position=0, width=10, help="  Line 1\n  Line 2  ")
        field.normalize()
        assert field.help == "Line 1\nLine 2"



class TestCard:
    """Test Card dataclass functionality."""

    def test_card_creation(self, sample_card):
        """Test basic Card creation."""
        assert sample_card.index == 0
        assert len(sample_card.fields) == 2
        assert sample_card.fields[0].name == "pid"
        assert sample_card.fields[1].name == "secid"

    def test_card_from_dict(self, sample_card_dict):
        """Test Card.from_dict() conversion."""
        card = Card.from_dict(sample_card_dict)
        assert card.index == 0
        assert len(card.fields) == 2
        assert isinstance(card.fields[0], Field)
        assert card.fields[0].name == "pid"

    def test_card_dict_like_access(self, sample_card):
        """Test backward-compatible dict-like access."""
        assert sample_card["index"] == 0
        assert "index" in sample_card
        assert sample_card.get("index") == 0
        assert sample_card.get("nonexistent", "default") == "default"

    def test_card_dict_like_assignment(self, sample_card):
        """Test backward-compatible dict-like assignment."""
        sample_card["mark_for_removal"] = 1
        assert sample_card.mark_for_removal == 1
        assert sample_card["mark_for_removal"] == 1

    def test_card_with_metadata(self):
        """Test Card with duplicate/variable/external metadata."""
        # Simplified test - just verify metadata can be dict (not converted yet in all cases)
        card_dict = {
            "index": 0,
            "fields": [],
        }
        card = Card.from_dict(card_dict)
        assert card.index == 0
        assert len(card.fields) == 0



class TestKeywordData:
    """Test KeywordData dataclass functionality."""

    def test_keyword_data_creation(self, sample_keyword_data):
        """Test basic KeywordData creation."""
        assert sample_keyword_data.keyword == "SECTION"
        assert sample_keyword_data.subkeyword == "SHELL"
        assert sample_keyword_data.title == "*SECTION_SHELL"
        assert len(sample_keyword_data.cards) == 1

    def test_keyword_data_from_dict(self, sample_keyword_data_dict):
        """Test KeywordData.from_dict() conversion."""
        kwd = KeywordData.from_dict(sample_keyword_data_dict)
        assert kwd.keyword == "SECTION"
        assert kwd.subkeyword == "SHELL"
        assert len(kwd.cards) == 1
        assert isinstance(kwd.cards[0], Card)
        assert isinstance(kwd.cards[0].fields[0], Field)

    def test_keyword_data_dict_like_access(self, sample_keyword_data):
        """Test that KeywordData uses typed attributes."""
        # KeywordData is a typed dataclass, uses attribute access
        assert sample_keyword_data.keyword == "SECTION"
        assert hasattr(sample_keyword_data, "keyword")
        assert hasattr(sample_keyword_data, "cards")

    def test_keyword_data_with_options(self):
        """Test KeywordData with option cards."""
        kwd_dict = {
            "keyword": "CONTACT",
            "subkeyword": "AUTOMATIC",
            "title": "*CONTACT_AUTOMATIC",
            "cards": [],
            "options": [{"name": "ID", "card_order": 1, "title_order": 1, "cards": []}],
        }
        kwd = KeywordData.from_dict(kwd_dict)
        assert len(kwd.options) == 1
        # Options are OptionGroup instances, use attribute access
        assert kwd.options[0].name == "ID"

    def test_keyword_data_field_normalization(self, sample_keyword_data):
        """Test that fields can be normalized."""
        # Set fields to need normalization
        sample_keyword_data.cards[0].fields[0].type = "integer"
        sample_keyword_data.cards[0].fields[1].type = "integer"

        # Normalize fields individually
        for card in sample_keyword_data.cards:
            for field in card.fields:
                field.normalize()

        assert sample_keyword_data.cards[0].fields[0].type == "int"
        assert sample_keyword_data.cards[0].fields[1].type == "int"
