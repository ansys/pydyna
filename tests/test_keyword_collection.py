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

"""Tests for the new Pythonic keyword lookup API (KeywordCollection, split, etc.)."""

import pytest
from ansys.dyna.core.lib.deck import Deck
from ansys.dyna.core.lib.domain_mapper import by_domain, get_all_domains
from ansys.dyna.core.lib.keyword_collection import KeywordCollection
import ansys.dyna.core.keywords as kwd


@pytest.fixture
def populated_deck():
    """Create a deck with various keyword types for testing."""
    deck = Deck(title="Test Deck")
    
    # Add sections
    section_shell = kwd.SectionShell(secid=1)
    section_solid = kwd.SectionSolid(secid=2)
    section_beam = kwd.SectionBeam(secid=3)
    
    # Add materials
    mat_elastic = kwd.MatElastic(mid=10)
    mat_plastic = kwd.MatPlasticKinematic(mid=20)
    
    # Add elements
    element_shell = kwd.ElementShell()
    element_solid = kwd.ElementSolid()
    
    # Add other keywords
    node = kwd.Node()
    part = kwd.Part()
    define_curve = kwd.DefineCurve()
    define_table = kwd.DefineTable()
    control_timestep = kwd.ControlTimestep()
    
    # Add string keywords
    string_keyword = "*ALE_SMOOTHING\\n$#    dnid      nid1      nid2      ipre       xco       yco       zco\\n                             3         0       0.0       0.0       0.0"
    
    deck.extend([
        section_shell, section_solid, section_beam,
        mat_elastic, mat_plastic,
        element_shell, element_solid,
        node, part,
        define_curve, define_table,
        control_timestep,
        string_keyword
    ])
    
    return deck


class TestKeywordCollection:
    """Tests for KeywordCollection class."""
    
    def test_collection_creation(self, populated_deck):
        """Test creating a KeywordCollection."""
        collection = KeywordCollection(populated_deck.keywords)
        assert isinstance(collection, KeywordCollection)
    
    def test_collection_by_subtype(self, populated_deck):
        """Test filtering by subtype."""
        sections = populated_deck.sections
        shells = sections.by_subtype("SHELL")
        
        shell_list = shells.to_list()
        assert len(shell_list) == 1
        assert shell_list[0].subkeyword == "SHELL"
        assert shell_list[0].secid == 1
    
    def test_collection_where(self, populated_deck):
        """Test filtering with custom predicate."""
        sections = populated_deck.sections
        filtered = sections.where(lambda k: k.secid > 1)
        
        result = filtered.to_list()
        assert len(result) == 2
        assert all(k.secid > 1 for k in result)
    
    def test_collection_first(self, populated_deck):
        """Test getting first keyword."""
        first_section = populated_deck.sections.first()
        assert first_section is not None
        assert first_section.keyword == "SECTION"
        assert first_section.secid == 1
    
    def test_collection_first_empty(self):
        """Test first() on empty collection."""
        empty_deck = Deck()
        first = empty_deck.sections.first()
        assert first is None
    
    def test_collection_to_list(self, populated_deck):
        """Test materializing collection to list."""
        sections = populated_deck.sections.to_list()
        assert isinstance(sections, list)
        assert len(sections) == 3
    
    def test_collection_iter(self, populated_deck):
        """Test iterating over collection."""
        count = 0
        for section in populated_deck.sections:
            count += 1
            assert section.keyword == "SECTION"
        assert count == 3
    
    def test_collection_len(self, populated_deck):
        """Test getting length of collection."""
        assert len(populated_deck.sections) == 3
        assert len(populated_deck.materials) == 2
    
    def test_collection_getitem_index(self, populated_deck):
        """Test accessing collection by index."""
        first_section = populated_deck.sections[0]
        assert first_section.secid == 1
    
    def test_collection_getitem_slice(self, populated_deck):
        """Test accessing collection by slice."""
        first_two = populated_deck.sections[:2]
        assert isinstance(first_two, list)
        assert len(first_two) == 2
    
    def test_collection_chaining(self, populated_deck):
        """Test chaining multiple filters."""
        sections = populated_deck.sections.where(lambda k: k.secid >= 2)
        solids = sections.by_subtype("SOLID")
        result = solids.to_list()
        
        assert len(result) == 1
        assert result[0].secid == 2
        assert result[0].subkeyword == "SOLID"


class TestDeckDictAccess:
    """Tests for dict-like access on Deck."""
    
    def test_getitem_single_key(self, populated_deck):
        """Test accessing keywords by type."""
        sections = populated_deck["SECTION"]
        assert isinstance(sections, KeywordCollection)
        assert len(sections) == 3
    
    def test_getitem_tuple_key(self, populated_deck):
        """Test accessing keywords by type and subtype."""
        shells = populated_deck["SECTION", "SHELL"]
        assert isinstance(shells, KeywordCollection)
        assert len(shells) == 1
        assert shells.first().subkeyword == "SHELL"
    
    def test_getitem_invalid_tuple(self, populated_deck):
        """Test that invalid tuple raises error."""
        with pytest.raises(ValueError, match="exactly 2 elements"):
            _ = populated_deck["SECTION", "SHELL", "EXTRA"]


class TestDeckConvenienceProperties:
    """Tests for convenience properties on Deck."""
    
    def test_sections_property(self, populated_deck):
        """Test sections property."""
        sections = populated_deck.sections
        assert isinstance(sections, KeywordCollection)
        assert len(sections) == 3
    
    def test_materials_property(self, populated_deck):
        """Test materials property."""
        materials = populated_deck.materials
        assert isinstance(materials, KeywordCollection)
        assert len(materials) == 2
    
    def test_elements_property(self, populated_deck):
        """Test elements property."""
        elements = populated_deck.elements
        assert isinstance(elements, KeywordCollection)
        assert len(elements) == 2
    
    def test_nodes_property(self, populated_deck):
        """Test nodes property."""
        nodes = populated_deck.nodes
        assert isinstance(nodes, KeywordCollection)
        assert len(nodes) == 1
    
    def test_parts_property(self, populated_deck):
        """Test parts property."""
        parts = populated_deck.parts
        assert isinstance(parts, KeywordCollection)
        assert len(parts) == 1
    
    def test_sets_property(self, populated_deck):
        """Test sets property."""
        sets = populated_deck.sets
        assert isinstance(sets, KeywordCollection)
        assert len(sets) == 0  # No sets in the fixture
    
    def test_defines_property(self, populated_deck):
        """Test defines property."""
        defines = populated_deck.defines
        assert isinstance(defines, KeywordCollection)
        assert len(defines) == 2  # DefineCurve and DefineTable


class TestDeckIteration:
    """Tests for iteration protocol on Deck."""
    
    def test_deck_iter(self, populated_deck):
        """Test iterating over deck."""
        count = 0
        for kwd in populated_deck:
            count += 1
        assert count == len(populated_deck._keywords)
    
    def test_deck_len(self, populated_deck):
        """Test getting deck length."""
        assert len(populated_deck) == 13  # 12 KeywordBase + 1 string


class TestDeckSplit:
    """Tests for split() method on Deck."""
    
    def test_split_by_keyword_type(self, populated_deck):
        """Test splitting by keyword type."""
        def get_keyword_type(kwd):
            if isinstance(kwd, kwd.__class__.__bases__[0]):  # KeywordBase
                return kwd.keyword
            return "string"
        
        decks = populated_deck.split(lambda k: k.keyword if hasattr(k, 'keyword') else "string")
        
        assert "SECTION" in decks
        assert "MAT" in decks
        assert "string" in decks
        
        assert len(decks["SECTION"]) == 3
        assert len(decks["MAT"]) == 2
    
    def test_split_by_custom_predicate(self, populated_deck):
        """Test splitting with custom logic."""
        def by_id_range(kwd):
            if hasattr(kwd, 'secid'):
                return "low" if kwd.secid < 2 else "high"
            elif hasattr(kwd, 'mid'):
                return "low" if kwd.mid < 15 else "high"
            return "other"
        
        decks = populated_deck.split(by_id_range)
        
        assert "low" in decks
        assert "high" in decks
        assert "other" in decks
    
    def test_split_preserves_deck_properties(self, populated_deck):
        """Test that split decks inherit format and parameters."""
        decks = populated_deck.split(lambda k: "test")
        split_deck = decks["test"]
        
        assert split_deck.format == populated_deck.format
        # Parameters are copied, so they're different objects but have same content
        assert isinstance(split_deck.parameters, type(populated_deck.parameters))


class TestDeckSplitByDomain:
    """Tests for split_by_domain() method."""
    
    def test_split_by_domain(self, populated_deck):
        """Test splitting by domain."""
        decks = populated_deck.split_by_domain()
        
        assert "section" in decks
        assert "mat" in decks
        assert "element" in decks
        assert "control" in decks
        assert "define" in decks
        
        assert len(decks["section"]) == 3
        assert len(decks["mat"]) == 2
        assert len(decks["element"]) == 2
    
    def test_split_by_domain_includes_strings(self, populated_deck):
        """Test that string keywords are categorized by domain."""
        decks = populated_deck.split_by_domain()
        
        # String keyword starts with *ALE_SMOOTHING
        assert "ale" in decks
        assert len(decks["ale"]) == 1


class TestDomainMapper:
    """Tests for domain mapper functionality."""
    
    def test_by_domain_keywordbase(self):
        """Test domain mapping for KeywordBase objects."""
        section = kwd.SectionShell(secid=1)
        assert by_domain(section) == "section"
        
        mat = kwd.MatElastic(mid=1)
        assert by_domain(mat) == "mat"
        
        control = kwd.ControlTimestep()
        assert by_domain(control) == "control"
    
    def test_by_domain_string(self):
        """Test domain mapping for string keywords."""
        string_section = "*SECTION_SHELL\\n$# comment\\n1.0"
        assert by_domain(string_section) == "section"
        
        string_mat = "*MAT_ELASTIC\\n$# comment"
        assert by_domain(string_mat) == "mat"
        
        string_ale = "*ALE_SMOOTHING\\n$# comment"
        assert by_domain(string_ale) == "ale"
    
    def test_by_domain_string_with_comments(self):
        """Test domain mapping for string keywords with leading comments."""
        string_with_comment = "$# This is a comment\\n*CONTROL_TIMESTEP\\n$# another comment"
        assert by_domain(string_with_comment) == "control"
    
    def test_by_domain_unknown(self):
        """Test domain mapping for unknown keywords."""
        string_unknown = "*UNKNOWN_KEYWORD\\n$# comment"
        assert by_domain(string_unknown) == "other"
    
    def test_get_all_domains(self):
        """Test getting all domains."""
        domains = get_all_domains()
        assert isinstance(domains, list)
        assert "section" in domains
        assert "mat" in domains
        assert "control" in domains
        assert len(domains) > 0


class TestEndToEnd:
    """End-to-end integration tests."""
    
    def test_fluent_api_example(self, populated_deck):
        """Test a realistic fluent API usage."""
        # Get all section shells with secid > 1
        result = populated_deck.sections.by_subtype("SHELL").where(lambda k: k.secid > 1)
        
        # Should be empty since only SHELL has secid=1
        assert len(result.to_list()) == 0
        
        # Get all sections with secid > 1
        result2 = populated_deck.sections.where(lambda k: k.secid > 1)
        assert len(result2.to_list()) == 2
    
    def test_split_and_recombine(self, populated_deck):
        """Test splitting and combining decks."""
        # Split by domain
        decks = populated_deck.split_by_domain()
        
        # Keywords in split decks are still associated with those decks
        # To recombine, we need to use the write/loads cycle or clear associations
        assert len(decks["mat"]) == 2
        assert len(decks["section"]) == 3
        
        # Verify we can iterate and access keywords from split decks
        mat_ids = [m.mid for m in decks["mat"].keywords]
        assert mat_ids == [10, 20]
    
    def test_filtering_and_modification(self, populated_deck):
        """Test filtering and modifying keywords."""
        # Get all sections and modify their IDs
        sections = populated_deck.sections.to_list()
        for section in sections:
            section.secid += 100
        
        # Verify modifications
        assert populated_deck.sections.first().secid == 101
        assert populated_deck.sections.where(lambda k: k.secid == 102).first() is not None
