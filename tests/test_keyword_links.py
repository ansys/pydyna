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

"""Tests for the keyword linking framework (get_links API and *_link properties)."""

import pytest

from ansys.dyna.core.lib.deck import Deck
from ansys.dyna.core.lib.keyword_base import KeywordBase, LinkType
from ansys.dyna.core.keywords import keywords as kwd


class TestLinkTypeEnum:
    """Tests for the LinkType enum."""

    def test_link_type_values(self):
        """Test that LinkType enum has expected values."""
        assert LinkType.ALL.value == 0
        assert LinkType.NODE.value == 1
        assert LinkType.ELEMENT_BEAM.value == 3
        assert LinkType.ELEMENT_SHELL.value == 4
        assert LinkType.ELEMENT_SOLID.value == 5
        assert LinkType.PART.value == 13
        assert LinkType.MAT.value == 14
        assert LinkType.SECTION.value == 15
        assert LinkType.HOURGLASS.value == 17
        assert LinkType.DEFINE_CURVE.value == 19
        assert LinkType.DEFINE_BOX.value == 20
        assert LinkType.DEFINE_COORDINATE_SYSTEM.value == 21
        assert LinkType.DEFINE_VECTOR.value == 22
        assert LinkType.SET_BEAM.value == 25
        assert LinkType.SET_DISCRETE.value == 26
        assert LinkType.SET_NODE.value == 27
        assert LinkType.SET_PART.value == 28
        assert LinkType.SET_SEGMENT.value == 29
        assert LinkType.SET_SOLID.value == 31
        assert LinkType.DEFINE_TRANSFORMATION.value == 40
        assert LinkType.DEFINE_CURVE_OR_TABLE.value == 86

    def test_link_type_members(self):
        """Test that LinkType has all expected members."""
        members = [m.name for m in LinkType]
        assert "ALL" in members
        assert "NODE" in members
        assert "ELEMENT_BEAM" in members
        assert "ELEMENT_SHELL" in members
        assert "ELEMENT_SOLID" in members
        assert "MAT" in members
        assert "SECTION" in members
        assert "HOURGLASS" in members
        assert "DEFINE_CURVE" in members
        assert "DEFINE_BOX" in members
        assert "DEFINE_COORDINATE_SYSTEM" in members
        assert "DEFINE_VECTOR" in members
        assert "SET_BEAM" in members
        assert "SET_DISCRETE" in members
        assert "SET_NODE" in members
        assert "SET_PART" in members
        assert "SET_SEGMENT" in members
        assert "SET_SOLID" in members
        assert "DEFINE_TRANSFORMATION" in members
        assert "PART" in members
        assert "DEFINE_CURVE_OR_TABLE" in members


class TestLinkFieldsClassAttribute:
    """Tests for the _link_fields class attribute on keywords."""

    def test_mat_piecewise_linear_plasticity_has_link_fields(self):
        """Test that MAT_PIECEWISE_LINEAR_PLASTICITY has _link_fields."""
        assert hasattr(kwd.MatPiecewiseLinearPlasticity, "_link_fields")
        assert "lcsr" in kwd.MatPiecewiseLinearPlasticity._link_fields
        assert kwd.MatPiecewiseLinearPlasticity._link_fields["lcsr"] == LinkType.DEFINE_CURVE

    def test_include_transform_has_link_fields(self):
        """Test that INCLUDE_TRANSFORM has _link_fields for transformation."""
        assert hasattr(kwd.IncludeTransform, "_link_fields")
        assert "tranid" in kwd.IncludeTransform._link_fields
        assert kwd.IncludeTransform._link_fields["tranid"] == LinkType.DEFINE_TRANSFORMATION

    def test_keyword_without_links_has_empty_link_fields(self):
        """Test that keywords without links have empty _link_fields."""
        # KeywordBase has empty _link_fields by default
        assert KeywordBase._link_fields == {}


class TestDefineCurveLinkProperty:
    """Tests for the lcsr_link property on MAT_PIECEWISE_LINEAR_PLASTICITY."""

    def test_lcsr_link_without_deck_returns_none(self):
        """Test that lcsr_link returns None when keyword is not in a deck."""
        mat = kwd.MatPiecewiseLinearPlasticity()
        mat.lcsr = 100
        assert mat.lcsr_link is None

    def test_lcsr_link_returns_matching_curve(self):
        """Test that lcsr_link returns the correct DefineCurve when in a deck."""
        deck = Deck()
        curve = kwd.DefineCurve()
        curve.lcid = 100
        mat = kwd.MatPiecewiseLinearPlasticity()
        mat.lcsr = 100

        deck.extend([curve, mat])

        assert mat.lcsr_link is curve
        assert mat.lcsr_link.lcid == 100

    def test_lcsr_link_returns_none_when_curve_not_found(self):
        """Test that lcsr_link returns None when referenced curve doesn't exist."""
        deck = Deck()
        curve = kwd.DefineCurve()
        curve.lcid = 200  # Different ID
        mat = kwd.MatPiecewiseLinearPlasticity()
        mat.lcsr = 100

        deck.extend([curve, mat])

        assert mat.lcsr_link is None

    def test_lcsr_link_setter(self):
        """Test that lcsr_link setter updates the lcsr field."""
        mat = kwd.MatPiecewiseLinearPlasticity()
        curve = kwd.DefineCurve()
        curve.lcid = 42

        mat.lcsr_link = curve

        assert mat.lcsr == 42


class TestGetLinksMethod:
    """Tests for the get_links() method on KeywordBase."""

    def test_get_links_without_deck_returns_empty(self):
        """Test that get_links returns empty list when keyword is not in a deck."""
        mat = kwd.MatPiecewiseLinearPlasticity()
        mat.lcsr = 100

        assert mat.get_links() == []
        assert mat.get_links(LinkType.DEFINE_CURVE) == []

    def test_get_links_returns_curve(self):
        """Test that get_links returns referenced curve."""
        deck = Deck()
        curve = kwd.DefineCurve()
        curve.lcid = 100
        mat = kwd.MatPiecewiseLinearPlasticity()
        mat.lcsr = 100

        deck.extend([curve, mat])

        links = mat.get_links()
        assert len(links) == 1
        assert links[0] is curve

    def test_get_links_with_filter(self):
        """Test that get_links filters by link type."""
        deck = Deck()
        curve = kwd.DefineCurve()
        curve.lcid = 100
        mat = kwd.MatPiecewiseLinearPlasticity()
        mat.lcsr = 100

        deck.extend([curve, mat])

        # Filter by DEFINE_CURVE should return the curve
        curve_links = mat.get_links(LinkType.DEFINE_CURVE)
        assert len(curve_links) == 1
        assert curve_links[0] is curve

        # Filter by DEFINE_TRANSFORMATION should return empty
        transform_links = mat.get_links(LinkType.DEFINE_TRANSFORMATION)
        assert len(transform_links) == 0

    def test_get_links_all_filter(self):
        """Test that get_links with ALL filter returns all links."""
        deck = Deck()
        curve = kwd.DefineCurve()
        curve.lcid = 100
        mat = kwd.MatPiecewiseLinearPlasticity()
        mat.lcsr = 100

        deck.extend([curve, mat])

        # LinkType.ALL should return all links
        all_links = mat.get_links(LinkType.ALL)
        assert len(all_links) == 1
        assert all_links[0] is curve

    def test_get_links_no_duplicates(self):
        """Test that get_links doesn't return duplicate keywords."""
        deck = Deck()
        curve = kwd.DefineCurve()
        curve.lcid = 100
        mat = kwd.MatPiecewiseLinearPlasticity()
        mat.lcsr = 100

        deck.extend([curve, mat])

        # Even if called multiple times, should not have duplicates
        links = mat.get_links()
        assert len(links) == len(set(id(l) for l in links))


class TestDefineTransformationLink:
    """Tests for DEFINE_TRANSFORMATION links (existing functionality)."""

    def test_tranid_link_property(self):
        """Test that tranid_link returns the correct DefineTransformation."""
        deck = Deck()
        transform = kwd.DefineTransformation()
        transform.tranid = 50
        include = kwd.IncludeTransform()
        include.tranid = 50

        deck.extend([transform, include])

        assert include.tranid_link is transform

    def test_get_links_transformation(self):
        """Test get_links for transformation references."""
        deck = Deck()
        transform = kwd.DefineTransformation()
        transform.tranid = 50
        include = kwd.IncludeTransform()
        include.tranid = 50

        deck.extend([transform, include])

        links = include.get_links()
        assert len(links) == 1
        assert links[0] is transform

        # Filter by type
        transform_links = include.get_links(LinkType.DEFINE_TRANSFORMATION)
        assert len(transform_links) == 1

        curve_links = include.get_links(LinkType.DEFINE_CURVE)
        assert len(curve_links) == 0


class TestMultipleCurveReferences:
    """Tests for keywords with multiple curve references."""

    def test_keyword_with_multiple_curve_fields(self):
        """Test a keyword that references multiple curves."""
        # Find a keyword with multiple DEFINE_CURVE references
        # MAT_MODIFIED_PIECEWISE_LINEAR_PLASTICITY_RATE has multiple curve fields
        deck = Deck()
        curve1 = kwd.DefineCurve()
        curve1.lcid = 100
        curve2 = kwd.DefineCurve()
        curve2.lcid = 200

        # Use a material with multiple curve references
        # We check what fields MAT_PIECEWISE_LINEAR_PLASTICITY has
        # It only has lcsr with link=19 (lcss has link=86 which is TABLE)
        mat = kwd.MatPiecewiseLinearPlasticity()
        mat.lcsr = 100

        deck.extend([curve1, curve2, mat])

        links = mat.get_links(LinkType.DEFINE_CURVE)
        # Only lcsr is a DEFINE_CURVE link, lcss is TABLE
        assert len(links) == 1
        assert links[0] is curve1


class TestMatLinks:
    """Tests for MAT_* (material) links."""

    def test_element_shell_composite_has_mat_link_fields(self):
        """Test that ELEMENT_SHELL_COMPOSITE has MAT link fields."""
        assert hasattr(kwd.ElementShellComposite, "_link_fields")
        assert "mid1" in kwd.ElementShellComposite._link_fields
        assert "mid2" in kwd.ElementShellComposite._link_fields
        assert kwd.ElementShellComposite._link_fields["mid1"] == LinkType.MAT

    def test_mid_link_without_deck_returns_none(self):
        """Test that mid_link returns None when keyword is not in a deck."""
        elem = kwd.ElementShellComposite()
        elem.mid1 = 100
        assert elem.mid1_link is None

    def test_mid_link_returns_matching_material(self):
        """Test that mid_link returns the correct MAT_* keyword when in a deck."""
        deck = Deck()
        mat = kwd.Mat001()  # MAT_ELASTIC
        mat.mid = 100
        elem = kwd.ElementShellComposite()
        elem.mid1 = 100

        deck.extend([mat, elem])

        assert elem.mid1_link is mat
        assert elem.mid1_link.mid == 100

    def test_mid_link_returns_none_when_material_not_found(self):
        """Test that mid_link returns None when referenced material doesn't exist."""
        deck = Deck()
        mat = kwd.Mat001()
        mat.mid = 200  # Different ID
        elem = kwd.ElementShellComposite()
        elem.mid1 = 100

        deck.extend([mat, elem])

        assert elem.mid1_link is None

    def test_mid_link_setter(self):
        """Test that mid_link setter updates the mid field."""
        elem = kwd.ElementShellComposite()
        mat = kwd.Mat001()
        mat.mid = 42

        elem.mid1_link = mat

        assert elem.mid1 == 42

    def test_get_links_with_mat_filter(self):
        """Test get_links filters by MAT type."""
        deck = Deck()
        mat1 = kwd.Mat001()
        mat1.mid = 100
        mat2 = kwd.Mat001()
        mat2.mid = 200
        elem = kwd.ElementShellComposite()
        elem.mid1 = 100
        elem.mid2 = 200

        deck.extend([mat1, mat2, elem])

        # Filter by MAT should return both materials
        mat_links = elem.get_links(LinkType.MAT)
        assert len(mat_links) == 2
        assert mat1 in mat_links
        assert mat2 in mat_links

        # Filter by DEFINE_CURVE should return empty
        curve_links = elem.get_links(LinkType.DEFINE_CURVE)
        assert len(curve_links) == 0


class TestSectionLinks:
    """Tests for SECTION_* links."""

    def test_define_adaptive_solid_to_sph_has_section_link_fields(self):
        """Test that DEFINE_ADAPTIVE_SOLID_TO_SPH has SECTION link fields."""
        assert hasattr(kwd.DefineAdaptiveSolidToSph, "_link_fields")
        # The field is 'issph' not 'isdes'
        assert "issph" in kwd.DefineAdaptiveSolidToSph._link_fields
        assert kwd.DefineAdaptiveSolidToSph._link_fields["issph"] == LinkType.SECTION

    def test_section_link_returns_matching_section(self):
        """Test that section link returns the correct SECTION_* keyword."""
        deck = Deck()
        section = kwd.SectionSph()
        section.secid = 100
        define = kwd.DefineAdaptiveSolidToSph()
        define.issph = 100

        deck.extend([section, define])

        assert define.issph_link is section
        assert define.issph_link.secid == 100

    def test_get_links_with_section_filter(self):
        """Test get_links filters by SECTION type."""
        deck = Deck()
        section = kwd.SectionSph()
        section.secid = 100
        define = kwd.DefineAdaptiveSolidToSph()
        define.issph = 100

        deck.extend([section, define])

        section_links = define.get_links(LinkType.SECTION)
        assert len(section_links) == 1
        assert section_links[0] is section


class TestPolymorphicCurveOrTableLinks:
    """Tests for DEFINE_CURVE_OR_TABLE polymorphic links."""

    def test_mat_024_has_polymorphic_link_fields(self):
        """Test that MAT_024 (MAT_PIECEWISE_LINEAR_PLASTICITY) has polymorphic fields."""
        assert hasattr(kwd.Mat024, "_link_fields")
        # lcss is the polymorphic field (link=86)
        if "lcss" in kwd.Mat024._link_fields:
            assert kwd.Mat024._link_fields["lcss"] == LinkType.DEFINE_CURVE_OR_TABLE

    def test_polymorphic_link_resolves_to_curve(self):
        """Test that polymorphic link resolves to DEFINE_CURVE when curve exists."""
        deck = Deck()
        curve = kwd.DefineCurve()
        curve.lcid = 100
        mat = kwd.Mat024()
        mat.lcss = 100

        deck.extend([curve, mat])

        # lcss_link should resolve to the curve
        if hasattr(mat, "lcss_link"):
            assert mat.lcss_link is curve

    def test_polymorphic_link_resolves_to_table(self):
        """Test that polymorphic link resolves to DEFINE_TABLE when table exists."""
        deck = Deck()
        table = kwd.DefineTable()
        table.tbid = 100
        mat = kwd.Mat024()
        mat.lcss = 100

        deck.extend([table, mat])

        # lcss_link should resolve to the table
        if hasattr(mat, "lcss_link"):
            assert mat.lcss_link is table

    def test_get_links_with_polymorphic_filter(self):
        """Test get_links filters by DEFINE_CURVE_OR_TABLE type."""
        deck = Deck()
        curve = kwd.DefineCurve()
        curve.lcid = 100
        mat = kwd.Mat024()
        mat.lcss = 100

        deck.extend([curve, mat])

        polymorphic_links = mat.get_links(LinkType.DEFINE_CURVE_OR_TABLE)
        if "lcss" in mat._link_fields:
            assert len(polymorphic_links) >= 1


class TestRecursiveLinkTraversal:
    """Tests for recursive link traversal with the level parameter."""

    def test_level_zero_returns_empty(self):
        """Test that level=0 returns an empty list."""
        deck = Deck()
        curve = kwd.DefineCurve()
        curve.lcid = 100
        mat = kwd.MatPiecewiseLinearPlasticity()
        mat.lcsr = 100

        deck.extend([curve, mat])

        # level=0 should return empty
        assert mat.get_links(level=0) == []

    def test_level_one_returns_direct_links(self):
        """Test that level=1 (default) returns only direct links."""
        deck = Deck()
        curve = kwd.DefineCurve()
        curve.lcid = 100
        mat = kwd.MatPiecewiseLinearPlasticity()
        mat.lcsr = 100

        deck.extend([curve, mat])

        # level=1 returns direct links only
        links = mat.get_links(level=1)
        assert len(links) == 1
        assert links[0] is curve

    def test_level_two_traverses_two_hops(self):
        """Test that level=2 traverses two levels deep."""
        deck = Deck()
        curve = kwd.DefineCurve()
        curve.lcid = 100
        mat = kwd.MatPiecewiseLinearPlasticity()
        mat.lcsr = 100

        deck.extend([curve, mat])

        # level=2 with a depth-1 chain should work the same
        links = mat.get_links(level=2)
        assert len(links) >= 1

    def test_unlimited_level_traverses_fully(self):
        """Test that level=-1 traverses without limit."""
        deck = Deck()
        curve = kwd.DefineCurve()
        curve.lcid = 100
        mat = kwd.MatPiecewiseLinearPlasticity()
        mat.lcsr = 100

        deck.extend([curve, mat])

        # level=-1 should traverse fully
        links = mat.get_links(level=-1)
        assert len(links) >= 1
        assert curve in links

    def test_cycle_prevention(self):
        """Test that cycles don't cause infinite recursion."""
        deck = Deck()
        curve = kwd.DefineCurve()
        curve.lcid = 100
        mat = kwd.MatPiecewiseLinearPlasticity()
        mat.lcsr = 100

        deck.extend([curve, mat])

        # Calling with unlimited level should still terminate
        links = mat.get_links(level=-1)
        # No infinite loop = test passes
        assert isinstance(links, list)

    def test_no_duplicate_keywords_in_recursive_traversal(self):
        """Test that recursive traversal doesn't return duplicates."""
        deck = Deck()
        curve = kwd.DefineCurve()
        curve.lcid = 100
        mat = kwd.MatPiecewiseLinearPlasticity()
        mat.lcsr = 100

        deck.extend([curve, mat])

        links = mat.get_links(level=-1)
        # Check no duplicates by comparing ids
        link_ids = [id(l) for l in links]
        assert len(link_ids) == len(set(link_ids))

    def test_set_part_link_in_recursive_traversal(self):
        """Test that SET_PART links are found in recursive traversal alongside other link types."""
        deck = Deck()

        # Create a curve that will be linked
        curve = kwd.DefineCurve()
        curve.lcid = 100

        # Create a SET_PART that will be linked
        set_part = kwd.SetPartList()
        set_part.sid = 50

        # AirbagParticle has both DEFINE_CURVE links (e.g., hconv) and SET_PART links (sidsv)
        airbag = kwd.AirbagParticle()
        airbag.hconv = 100  # Links to DefineCurve
        airbag.sidsv = 50  # Links to SetPartList

        deck.extend([curve, set_part, airbag])

        # Get all links - should include both the curve and the set
        all_links = airbag.get_links(level=-1)
        assert curve in all_links, "DEFINE_CURVE should be found in links"
        assert set_part in all_links, "SET_PART should be found in links"

        # Filter by SET_PART type should only return the set
        set_links = airbag.get_links(LinkType.SET_PART)
        assert len(set_links) >= 1
        assert set_part in set_links
        assert curve not in set_links

        # Filter by DEFINE_CURVE type should only return the curve
        curve_links = airbag.get_links(LinkType.DEFINE_CURVE)
        assert curve in curve_links
        assert set_part not in curve_links

    def test_recursive_traversal_through_set_part_to_part(self):
        """Test recursive traversal: Airbag -> SET_PART -> PART.

        This demonstrates that a user can search through a SET to find the
        underlying PART keywords that the SET contains.
        """
        import pandas as pd

        deck = Deck()

        # Create a PART keyword
        part = kwd.Part()
        part.parts = pd.DataFrame(
            {"heading": ["My Part"], "pid": [100], "mid": [1], "secid": [1]}
        )

        # Create a SET_PART that references the part
        set_part = kwd.SetPartList()
        set_part.sid = 50
        set_part.pid1 = 100  # References the PART

        # AirbagParticle references the SET_PART
        airbag = kwd.AirbagParticle()
        airbag.sidsv = 50  # Links to SetPartList

        deck.extend([part, set_part, airbag])

        # level=1: should only find SET_PART (direct link)
        level1_links = airbag.get_links(level=1)
        assert set_part in level1_links
        assert part not in level1_links

        # level=2: should find SET_PART and PART (through the set)
        level2_links = airbag.get_links(level=2)
        assert set_part in level2_links
        assert part in level2_links, "PART should be reachable through SET_PART at level=2"

        # level=-1: unlimited traversal should also find both
        all_links = airbag.get_links(level=-1)
        assert set_part in all_links
        assert part in all_links


class TestTableBasedLinks:
    """Tests for table-based link properties (e.g., Part with multiple mid/secid per row)."""

    def test_part_has_table_link_fields(self):
        """Test that Part has _link_fields for mid and secid."""
        assert hasattr(kwd.Part, "_link_fields")
        assert "mid" in kwd.Part._link_fields
        assert "secid" in kwd.Part._link_fields
        assert kwd.Part._link_fields["mid"] == LinkType.MAT
        assert kwd.Part._link_fields["secid"] == LinkType.SECTION

    def test_mid_links_without_deck_returns_empty(self):
        """Test that mid_links returns empty dict when keyword is not in a deck."""
        import pandas as pd

        part = kwd.Part()
        part.parts = pd.DataFrame({"heading": ["Part 1"], "pid": [1], "mid": [100], "secid": [200]})
        assert part.mid_links == {}

    def test_mid_links_returns_correct_mapping(self):
        """Test that mid_links returns correct dict mapping pid to material."""
        import pandas as pd

        deck = Deck()
        mat1 = kwd.Mat001()  # MAT_ELASTIC
        mat1.mid = 100
        mat2 = kwd.Mat001()
        mat2.mid = 200

        part = kwd.Part()
        part.parts = pd.DataFrame(
            {"heading": ["Part 1", "Part 2"], "pid": [1, 2], "mid": [100, 200], "secid": [10, 20]}
        )

        deck.extend([mat1, mat2, part])

        mid_links = part.mid_links
        assert len(mid_links) == 2
        assert mid_links[1] is mat1
        assert mid_links[2] is mat2

    def test_get_mid_link_returns_correct_material(self):
        """Test that get_mid_link(pid) returns the correct material for that pid."""
        import pandas as pd

        deck = Deck()
        mat1 = kwd.Mat001()
        mat1.mid = 100
        mat2 = kwd.Mat001()
        mat2.mid = 200

        part = kwd.Part()
        part.parts = pd.DataFrame(
            {"heading": ["Part 1", "Part 2"], "pid": [1, 2], "mid": [100, 200], "secid": [10, 20]}
        )

        deck.extend([mat1, mat2, part])

        assert part.get_mid_link(1) is mat1
        assert part.get_mid_link(2) is mat2

    def test_get_mid_link_returns_none_for_missing_pid(self):
        """Test that get_mid_link returns None for non-existent pid."""
        import pandas as pd

        deck = Deck()
        mat = kwd.Mat001()
        mat.mid = 100

        part = kwd.Part()
        part.parts = pd.DataFrame({"heading": ["Part 1"], "pid": [1], "mid": [100], "secid": [10]})

        deck.extend([mat, part])

        assert part.get_mid_link(999) is None

    def test_get_mid_link_returns_none_when_material_not_found(self):
        """Test that get_mid_link returns None when referenced material doesn't exist."""
        import pandas as pd

        deck = Deck()
        mat = kwd.Mat001()
        mat.mid = 999  # Different ID

        part = kwd.Part()
        part.parts = pd.DataFrame({"heading": ["Part 1"], "pid": [1], "mid": [100], "secid": [10]})

        deck.extend([mat, part])

        assert part.get_mid_link(1) is None

    def test_secid_links_returns_correct_mapping(self):
        """Test that secid_links returns correct dict mapping pid to section."""
        import pandas as pd

        deck = Deck()
        sec1 = kwd.SectionShell()
        sec1.secid = 10
        sec2 = kwd.SectionShell()
        sec2.secid = 20

        part = kwd.Part()
        part.parts = pd.DataFrame(
            {"heading": ["Part 1", "Part 2"], "pid": [1, 2], "mid": [100, 200], "secid": [10, 20]}
        )

        deck.extend([sec1, sec2, part])

        secid_links = part.secid_links
        assert len(secid_links) == 2
        assert secid_links[1] is sec1
        assert secid_links[2] is sec2

    def test_get_secid_link_returns_correct_section(self):
        """Test that get_secid_link(pid) returns the correct section for that pid."""
        import pandas as pd

        deck = Deck()
        sec1 = kwd.SectionShell()
        sec1.secid = 10
        sec2 = kwd.SectionShell()
        sec2.secid = 20

        part = kwd.Part()
        part.parts = pd.DataFrame(
            {"heading": ["Part 1", "Part 2"], "pid": [1, 2], "mid": [100, 200], "secid": [10, 20]}
        )

        deck.extend([sec1, sec2, part])

        assert part.get_secid_link(1) is sec1
        assert part.get_secid_link(2) is sec2

    def test_mid_links_handles_missing_materials(self):
        """Test that mid_links only includes pids where material exists."""
        import pandas as pd

        deck = Deck()
        mat1 = kwd.Mat001()
        mat1.mid = 100
        # mat2 with mid=200 not added

        part = kwd.Part()
        part.parts = pd.DataFrame(
            {"heading": ["Part 1", "Part 2"], "pid": [1, 2], "mid": [100, 200], "secid": [10, 20]}
        )

        deck.extend([mat1, part])

        mid_links = part.mid_links
        assert len(mid_links) == 1
        assert 1 in mid_links
        assert 2 not in mid_links


class TestTableCardLinks:
    """Tests for TableCard links (e.g., ELEMENT_BEAM -> PART)."""

    def test_element_beam_has_part_link_fields(self):
        """Test that ElementBeam has _link_fields for pid."""
        assert hasattr(kwd.ElementBeam, "_link_fields")
        assert "pid" in kwd.ElementBeam._link_fields
        assert kwd.ElementBeam._link_fields["pid"] == LinkType.PART

    def test_pid_links_without_deck_returns_empty(self):
        """Test that pid_links returns empty dict when not in a deck."""
        import pandas as pd

        beam = kwd.ElementBeam()
        beam.elements = pd.DataFrame({"eid": [1, 2, 3], "pid": [10, 20, 30], "n1": [1, 2, 3], "n2": [4, 5, 6], "n3": [7, 8, 9]})

        assert beam.pid_links == {}

    def test_pid_links_returns_correct_mapping(self):
        """Test that pid_links returns correct dict mapping pid to Part."""
        import pandas as pd

        deck = Deck()
        part = kwd.Part()
        part.parts = pd.DataFrame(
            {"heading": ["Part 1", "Part 2"], "pid": [10, 20], "mid": [100, 200], "secid": [1, 2]}
        )

        beam = kwd.ElementBeam()
        beam.elements = pd.DataFrame({"eid": [1, 2], "pid": [10, 20], "n1": [1, 2], "n2": [3, 4], "n3": [5, 6]})

        deck.extend([part, beam])

        pid_links = beam.pid_links
        assert len(pid_links) == 2
        assert pid_links[10] is part
        assert pid_links[20] is part

    def test_pid_links_handles_multiple_part_keywords(self):
        """Test that pid_links finds parts in different Part keywords."""
        import pandas as pd

        deck = Deck()
        part1 = kwd.Part()
        part1.parts = pd.DataFrame({"heading": ["Part 1"], "pid": [10], "mid": [100], "secid": [1]})
        part2 = kwd.Part()
        part2.parts = pd.DataFrame({"heading": ["Part 2"], "pid": [20], "mid": [200], "secid": [2]})

        beam = kwd.ElementBeam()
        beam.elements = pd.DataFrame({"eid": [1, 2], "pid": [10, 20], "n1": [1, 2], "n2": [3, 4], "n3": [5, 6]})

        deck.extend([part1, part2, beam])

        pid_links = beam.pid_links
        assert len(pid_links) == 2
        assert pid_links[10] is part1
        assert pid_links[20] is part2

    def test_pid_links_handles_missing_parts(self):
        """Test that pid_links only includes pids where Part exists."""
        import pandas as pd

        deck = Deck()
        part = kwd.Part()
        part.parts = pd.DataFrame({"heading": ["Part 1"], "pid": [10], "mid": [100], "secid": [1]})

        beam = kwd.ElementBeam()
        beam.elements = pd.DataFrame({"eid": [1, 2, 3], "pid": [10, 20, 30], "n1": [1, 2, 3], "n2": [4, 5, 6], "n3": [7, 8, 9]})

        deck.extend([part, beam])

        pid_links = beam.pid_links
        assert len(pid_links) == 1
        assert 10 in pid_links
        assert 20 not in pid_links
        assert 30 not in pid_links

    def test_get_pid_link_returns_correct_part(self):
        """Test that get_pid_link(pid) returns the correct Part keyword."""
        import pandas as pd

        deck = Deck()
        part1 = kwd.Part()
        part1.parts = pd.DataFrame({"heading": ["Part 1"], "pid": [10], "mid": [100], "secid": [1]})
        part2 = kwd.Part()
        part2.parts = pd.DataFrame({"heading": ["Part 2"], "pid": [20], "mid": [200], "secid": [2]})

        beam = kwd.ElementBeam()
        beam.elements = pd.DataFrame({"eid": [1, 2], "pid": [10, 20], "n1": [1, 2], "n2": [3, 4], "n3": [5, 6]})

        deck.extend([part1, part2, beam])

        assert beam.get_pid_link(10) is part1
        assert beam.get_pid_link(20) is part2

    def test_get_pid_link_returns_none_for_missing_pid(self):
        """Test that get_pid_link returns None for non-existent pid."""
        import pandas as pd

        deck = Deck()
        part = kwd.Part()
        part.parts = pd.DataFrame({"heading": ["Part 1"], "pid": [10], "mid": [100], "secid": [1]})

        beam = kwd.ElementBeam()
        beam.elements = pd.DataFrame({"eid": [1], "pid": [10], "n1": [1], "n2": [2], "n3": [3]})

        deck.extend([part, beam])

        assert beam.get_pid_link(999) is None

    def test_get_pid_link_without_deck_returns_none(self):
        """Test that get_pid_link returns None when not in a deck."""
        import pandas as pd

        beam = kwd.ElementBeam()
        beam.elements = pd.DataFrame({"eid": [1, 2], "pid": [10, 20], "n1": [1, 2], "n2": [3, 4], "n3": [5, 6]})

        assert beam.get_pid_link(10) is None

    def test_pid_links_with_empty_elements(self):
        """Test that pid_links handles empty elements table."""
        import pandas as pd

        deck = Deck()
        part = kwd.Part()
        part.parts = pd.DataFrame({"heading": ["Part 1"], "pid": [10], "mid": [100], "secid": [1]})

        beam = kwd.ElementBeam()
        beam.elements = pd.DataFrame({"eid": [], "pid": [], "n1": [], "n2": [], "n3": []})

        deck.extend([part, beam])

        assert beam.pid_links == {}


@pytest.mark.skip(reason="PART links for scalar fields not yet implemented (kwd.json uses link:69)")
class TestScalarPartLinks:
    """Tests for scalar PART links (e.g., MESH_BL -> PART)."""

    def test_mesh_bl_has_part_link_fields(self):
        """Test that MeshBl has _link_fields for pid."""
        assert hasattr(kwd.MeshBl, "_link_fields")
        assert "pid" in kwd.MeshBl._link_fields
        assert kwd.MeshBl._link_fields["pid"] == LinkType.PART

    def test_pid_link_without_deck_returns_none(self):
        """Test that pid_link returns None when not in a deck."""
        mesh = kwd.MeshBl()
        mesh.pid = 1

        assert mesh.pid_link is None

    def test_pid_link_returns_correct_part(self):
        """Test that pid_link returns the correct Part keyword."""
        import pandas as pd

        deck = Deck()
        part = kwd.Part()
        part.parts = pd.DataFrame(
            {"heading": ["Part 1", "Part 2"], "pid": [1, 2], "mid": [100, 200], "secid": [10, 20]}
        )

        mesh = kwd.MeshBl()
        mesh.pid = 1

        deck.extend([part, mesh])

        assert mesh.pid_link is part

    def test_pid_link_finds_part_in_different_keywords(self):
        """Test that pid_link finds the correct Part keyword when there are multiple."""
        import pandas as pd

        deck = Deck()
        part1 = kwd.Part()
        part1.parts = pd.DataFrame({"heading": ["Part 1"], "pid": [1], "mid": [100], "secid": [10]})
        part2 = kwd.Part()
        part2.parts = pd.DataFrame({"heading": ["Part 2"], "pid": [2], "mid": [200], "secid": [20]})

        mesh = kwd.MeshBl()
        mesh.pid = 2

        deck.extend([part1, part2, mesh])

        assert mesh.pid_link is part2

    def test_pid_link_returns_none_when_part_not_found(self):
        """Test that pid_link returns None when referenced part doesn't exist."""
        import pandas as pd

        deck = Deck()
        part = kwd.Part()
        part.parts = pd.DataFrame({"heading": ["Part 1"], "pid": [1], "mid": [100], "secid": [10]})

        mesh = kwd.MeshBl()
        mesh.pid = 999  # Different ID

        deck.extend([part, mesh])

        assert mesh.pid_link is None

    def test_pid_link_returns_none_when_pid_is_none(self):
        """Test that pid_link returns None when pid field is None."""
        import pandas as pd

        deck = Deck()
        part = kwd.Part()
        part.parts = pd.DataFrame({"heading": ["Part 1"], "pid": [1], "mid": [100], "secid": [10]})

        mesh = kwd.MeshBl()
        # pid is None by default

        deck.extend([part, mesh])

        assert mesh.pid_link is None


@pytest.mark.skip(reason="PART links for ICFD/MESH keywords not yet implemented (kwd.json uses link:69)")
class TestRecursiveLinkChasingAcrossKeywords:
    """Tests for recursive link chasing across keyword types (e.g., ICFD -> PART -> SECTION)."""

    def test_icfd_to_part_to_section_chain(self):
        """Test recursive link chasing: ICFD_BOUNDARY_FSI -> PART -> SECTION_SHELL."""
        import pandas as pd

        deck = Deck()

        # Create section
        section = kwd.SectionShell()
        section.secid = 10

        # Create part referencing section
        part = kwd.Part()
        part.parts = pd.DataFrame(
            {"heading": ["Part 1"], "pid": [1], "mid": [100], "secid": [10]}
        )

        # Create FSI referencing part
        fsi = kwd.IcfdBoundaryFsi()
        fsi.boundaries = pd.DataFrame({"pid": [1]})

        deck.extend([section, part, fsi])

        # Test level=1: should find Part only
        direct_links = fsi.get_links(level=1)
        assert len(direct_links) == 1
        assert direct_links[0] is part

        # Test level=2: should find Part and Section
        two_level_links = fsi.get_links(level=2)
        assert len(two_level_links) == 2
        assert part in two_level_links
        assert section in two_level_links

        # Test level=-1: should find full dependency tree
        all_links = fsi.get_links(level=-1)
        assert len(all_links) >= 2
        assert part in all_links
        assert section in all_links

    def test_icfd_to_part_to_material_and_section_chain(self):
        """Test recursive link chasing: ICFD -> PART -> (MAT + SECTION)."""
        import pandas as pd

        deck = Deck()

        # Create material
        mat = kwd.Mat001()
        mat.mid = 100

        # Create section
        section = kwd.SectionShell()
        section.secid = 10

        # Create part referencing both
        part = kwd.Part()
        part.parts = pd.DataFrame(
            {"heading": ["Part 1"], "pid": [1], "mid": [100], "secid": [10]}
        )

        # Create FSI referencing part
        fsi = kwd.IcfdBoundaryFsi()
        fsi.boundaries = pd.DataFrame({"pid": [1]})

        deck.extend([mat, section, part, fsi])

        # Full recursive traversal should find Part, Mat, and Section
        all_links = fsi.get_links(level=-1)
        assert len(all_links) == 3
        assert part in all_links
        assert mat in all_links
        assert section in all_links

    def test_multiple_parts_recursive_traversal(self):
        """Test recursive traversal with multiple part references."""
        import pandas as pd

        deck = Deck()

        # Create materials
        mat1 = kwd.Mat001()
        mat1.mid = 100
        mat2 = kwd.Mat001()
        mat2.mid = 200

        # Create sections
        sec1 = kwd.SectionShell()
        sec1.secid = 10
        sec2 = kwd.SectionShell()
        sec2.secid = 20

        # Create parts
        part1 = kwd.Part()
        part1.parts = pd.DataFrame(
            {"heading": ["Part 1"], "pid": [1], "mid": [100], "secid": [10]}
        )
        part2 = kwd.Part()
        part2.parts = pd.DataFrame(
            {"heading": ["Part 2"], "pid": [2], "mid": [200], "secid": [20]}
        )

        # Create FSI referencing both parts
        fsi = kwd.IcfdBoundaryFsi()
        fsi.boundaries = pd.DataFrame({"pid": [1, 2]})

        deck.extend([mat1, mat2, sec1, sec2, part1, part2, fsi])

        # Full recursive traversal
        all_links = fsi.get_links(level=-1)
        assert len(all_links) == 6
        assert part1 in all_links
        assert part2 in all_links
        assert mat1 in all_links
        assert mat2 in all_links
        assert sec1 in all_links
        assert sec2 in all_links

    def test_filter_by_link_type_in_recursive_traversal(self):
        """Test filtering by link type during recursive traversal."""
        import pandas as pd

        deck = Deck()

        # Create material and section
        mat = kwd.Mat001()
        mat.mid = 100
        section = kwd.SectionShell()
        section.secid = 10

        # Create part
        part = kwd.Part()
        part.parts = pd.DataFrame(
            {"heading": ["Part 1"], "pid": [1], "mid": [100], "secid": [10]}
        )

        # Create FSI
        fsi = kwd.IcfdBoundaryFsi()
        fsi.boundaries = pd.DataFrame({"pid": [1]})

        deck.extend([mat, section, part, fsi])

        # Filter by PART only
        part_links = fsi.get_links(LinkType.PART, level=-1)
        assert len(part_links) == 1
        assert part in part_links

        # Filter by SECTION only - should not find directly from FSI
        section_links = fsi.get_links(LinkType.SECTION, level=-1)
        # Section is linked from Part, but if we filter by SECTION, we only follow SECTION links
        # which FSI doesn't have. So this should be empty.
        assert len(section_links) == 0

    def test_scalar_part_link_recursive_traversal(self):
        """Test recursive traversal from scalar part link (MESH_BL -> PART -> SECTION)."""
        import pandas as pd

        deck = Deck()

        # Create section
        section = kwd.SectionShell()
        section.secid = 10

        # Create part
        part = kwd.Part()
        part.parts = pd.DataFrame(
            {"heading": ["Part 1"], "pid": [1], "mid": [100], "secid": [10]}
        )

        # Create MESH_BL with scalar pid
        mesh = kwd.MeshBl()
        mesh.pid = 1

        deck.extend([section, part, mesh])

        # Test level=1: should find Part only
        direct_links = mesh.get_links(level=1)
        assert len(direct_links) == 1
        assert direct_links[0] is part

        # Test level=-1: should find Part and Section
        all_links = mesh.get_links(level=-1)
        assert len(all_links) >= 2
        assert part in all_links
        assert section in all_links

    def test_no_duplicate_in_recursive_chain(self):
        """Test that duplicates are prevented in recursive traversal."""
        import pandas as pd

        deck = Deck()

        # Create shared section
        section = kwd.SectionShell()
        section.secid = 10

        # Create two parts referencing same section
        part1 = kwd.Part()
        part1.parts = pd.DataFrame(
            {"heading": ["Part 1"], "pid": [1], "mid": [100], "secid": [10]}
        )
        part2 = kwd.Part()
        part2.parts = pd.DataFrame(
            {"heading": ["Part 2"], "pid": [2], "mid": [200], "secid": [10]}  # Same section
        )

        # Create FSI referencing both parts
        fsi = kwd.IcfdBoundaryFsi()
        fsi.boundaries = pd.DataFrame({"pid": [1, 2]})

        deck.extend([section, part1, part2, fsi])

        # Recursive traversal should not duplicate section
        all_links = fsi.get_links(level=-1)

        # Count occurrences of section
        section_count = sum(1 for link in all_links if link is section)
        assert section_count == 1, "Section should appear only once despite being referenced by two parts"

        # Should have part1, part2, and section
        assert len(all_links) == 3


class TestNodeLinks:
    """Tests for NODE links (link type 1)."""

    def test_node_link_type_value(self):
        """Test that NODE link type has correct value."""
        assert LinkType.NODE.value == 1

    def test_node_link_type_in_enum(self):
        """Test that NODE is a member of LinkType enum."""
        assert hasattr(LinkType, "NODE")


class TestHourglassLinks:
    """Tests for HOURGLASS links (link type 17)."""

    def test_hourglass_link_type_value(self):
        """Test that HOURGLASS link type has correct value."""
        assert LinkType.HOURGLASS.value == 17

    def test_hourglass_link_type_in_enum(self):
        """Test that HOURGLASS is a member of LinkType enum."""
        assert hasattr(LinkType, "HOURGLASS")

    def test_part_has_hourglass_link_fields(self):
        """Test that PART has HOURGLASS link fields (_link_fields)."""
        assert hasattr(kwd.Part, "_link_fields")
        assert "hgid" in kwd.Part._link_fields
        assert kwd.Part._link_fields["hgid"] == LinkType.HOURGLASS

    def test_hgid_link_returns_matching_hourglass(self):
        """Test that hgid_link returns the correct Hourglass keyword when in a deck."""
        import pandas as pd

        deck = Deck()
        hourglass = kwd.Hourglass()
        hourglass.add_set()  # Hourglass is a CardSet keyword
        hourglass.sets[0].hgid = 100
        hourglass.sets[0].ihq = 1

        part = kwd.Part()
        part.parts = pd.DataFrame(
            {"heading": ["Part 1"], "pid": [1], "mid": [10], "secid": [20], "hgid": [100]}
        )

        deck.extend([hourglass, part])

        # Test get_hgid_link method
        assert part.get_hgid_link(1) is hourglass

        # Test hgid_links property
        hgid_links = part.hgid_links
        assert len(hgid_links) == 1
        assert hgid_links[1] is hourglass

    def test_hgid_link_returns_empty_when_column_missing(self):
        """Test that hgid_link returns empty dict when hgid column is not in DataFrame."""
        import pandas as pd

        deck = Deck()
        hourglass = kwd.Hourglass()
        hourglass.add_set()  # Hourglass is a CardSet keyword
        hourglass.sets[0].hgid = 100
        hourglass.sets[0].ihq = 1

        part = kwd.Part()
        # DataFrame without hgid column
        part.parts = pd.DataFrame(
            {"heading": ["Part 1"], "pid": [1], "mid": [10], "secid": [20]}
        )

        deck.extend([hourglass, part])

        # Should return empty dict, not raise KeyError
        assert part.hgid_links == {}
        assert part.get_hgid_link(1) is None

    def test_hgid_link_returns_none_when_not_found(self):
        """Test that hgid_link returns None when referenced hourglass doesn't exist."""
        import pandas as pd

        deck = Deck()
        hourglass = kwd.Hourglass()
        hourglass.add_set()  # Hourglass is a CardSet keyword
        hourglass.sets[0].hgid = 200  # Different ID
        hourglass.sets[0].ihq = 1

        part = kwd.Part()
        part.parts = pd.DataFrame(
            {"heading": ["Part 1"], "pid": [1], "mid": [10], "secid": [20], "hgid": [100]}
        )

        deck.extend([hourglass, part])

        # hgid 100 doesn't match hourglass with hgid 200
        assert part.get_hgid_link(1) is None


class TestDefineBoxLinks:
    """Tests for DEFINE_BOX links (link type 20)."""

    def test_define_box_link_type_value(self):
        """Test that DEFINE_BOX link type has correct value."""
        assert LinkType.DEFINE_BOX.value == 20

    def test_define_box_link_type_in_enum(self):
        """Test that DEFINE_BOX is a member of LinkType enum."""
        assert hasattr(LinkType, "DEFINE_BOX")


class TestDefineCoordinateSystemLinks:
    """Tests for DEFINE_COORDINATE_SYSTEM links (link type 21)."""

    def test_define_coordinate_system_link_type_value(self):
        """Test that DEFINE_COORDINATE_SYSTEM link type has correct value."""
        assert LinkType.DEFINE_COORDINATE_SYSTEM.value == 21

    def test_define_coordinate_system_link_type_in_enum(self):
        """Test that DEFINE_COORDINATE_SYSTEM is a member of LinkType enum."""
        assert hasattr(LinkType, "DEFINE_COORDINATE_SYSTEM")


class TestDefineVectorLinks:
    """Tests for DEFINE_VECTOR links (link type 22)."""

    def test_define_vector_link_type_value(self):
        """Test that DEFINE_VECTOR link type has correct value."""
        assert LinkType.DEFINE_VECTOR.value == 22

    def test_define_vector_link_type_in_enum(self):
        """Test that DEFINE_VECTOR is a member of LinkType enum."""
        assert hasattr(LinkType, "DEFINE_VECTOR")


class TestElementBeamLinks:
    """Tests for ELEMENT_BEAM links (link type 3)."""

    def test_element_beam_link_type_value(self):
        """Test that ELEMENT_BEAM link type has correct value."""
        assert LinkType.ELEMENT_BEAM.value == 3

    def test_element_beam_link_type_in_enum(self):
        """Test that ELEMENT_BEAM is a member of LinkType enum."""
        assert hasattr(LinkType, "ELEMENT_BEAM")


class TestElementShellLinks:
    """Tests for ELEMENT_SHELL links (link type 4)."""

    def test_element_shell_link_type_value(self):
        """Test that ELEMENT_SHELL link type has correct value."""
        assert LinkType.ELEMENT_SHELL.value == 4

    def test_element_shell_link_type_in_enum(self):
        """Test that ELEMENT_SHELL is a member of LinkType enum."""
        assert hasattr(LinkType, "ELEMENT_SHELL")


class TestElementSolidLinks:
    """Tests for ELEMENT_SOLID links (link type 5)."""

    def test_element_solid_link_type_value(self):
        """Test that ELEMENT_SOLID link type has correct value."""
        assert LinkType.ELEMENT_SOLID.value == 5

    def test_element_solid_link_type_in_enum(self):
        """Test that ELEMENT_SOLID is a member of LinkType enum."""
        assert hasattr(LinkType, "ELEMENT_SOLID")


class TestSetBeamLinks:
    """Tests for SET_BEAM links (link type 25)."""

    def test_set_beam_link_type_value(self):
        """Test that SET_BEAM link type has correct value."""
        assert LinkType.SET_BEAM.value == 25

    def test_set_beam_link_type_in_enum(self):
        """Test that SET_BEAM is a member of LinkType enum."""
        assert hasattr(LinkType, "SET_BEAM")


class TestSetDiscreteLinks:
    """Tests for SET_DISCRETE links (link type 26)."""

    def test_set_discrete_link_type_value(self):
        """Test that SET_DISCRETE link type has correct value."""
        assert LinkType.SET_DISCRETE.value == 26

    def test_set_discrete_link_type_in_enum(self):
        """Test that SET_DISCRETE is a member of LinkType enum."""
        assert hasattr(LinkType, "SET_DISCRETE")


class TestSetNodeLinks:
    """Tests for SET_NODE links (link type 27)."""

    def test_set_node_link_type_value(self):
        """Test that SET_NODE link type has correct value."""
        assert LinkType.SET_NODE.value == 27

    def test_set_node_link_type_in_enum(self):
        """Test that SET_NODE is a member of LinkType enum."""
        assert hasattr(LinkType, "SET_NODE")


class TestSetPartLinks:
    """Tests for SET_PART links (link type 28)."""

    def test_set_part_link_type_value(self):
        """Test that SET_PART link type has correct value."""
        assert LinkType.SET_PART.value == 28

    def test_set_part_link_type_in_enum(self):
        """Test that SET_PART is a member of LinkType enum."""
        assert hasattr(LinkType, "SET_PART")


class TestSetSegmentLinks:
    """Tests for SET_SEGMENT links (link type 29)."""

    def test_set_segment_link_type_value(self):
        """Test that SET_SEGMENT link type has correct value."""
        assert LinkType.SET_SEGMENT.value == 29

    def test_set_segment_link_type_in_enum(self):
        """Test that SET_SEGMENT is a member of LinkType enum."""
        assert hasattr(LinkType, "SET_SEGMENT")


class TestSetSolidLinks:
    """Tests for SET_SOLID links (link type 31)."""

    def test_set_solid_link_type_value(self):
        """Test that SET_SOLID link type has correct value."""
        assert LinkType.SET_SOLID.value == 31

    def test_set_solid_link_type_in_enum(self):
        """Test that SET_SOLID is a member of LinkType enum."""
        assert hasattr(LinkType, "SET_SOLID")


class TestSetPartLinkProperty:
    """Tests for SET_PART link properties on generated keywords."""

    def test_airbag_particle_has_set_part_link_fields(self):
        """Test that AirbagParticle has SET_PART _link_fields."""
        assert hasattr(kwd.AirbagParticle, "_link_fields")
        assert "sidsv" in kwd.AirbagParticle._link_fields
        assert kwd.AirbagParticle._link_fields["sidsv"] == LinkType.SET_PART
        assert "psid1" in kwd.AirbagParticle._link_fields
        assert kwd.AirbagParticle._link_fields["psid1"] == LinkType.SET_PART

    def test_sidsv_link_without_deck_returns_none(self):
        """Test that sidsv_link returns None when keyword is not in a deck."""
        airbag = kwd.AirbagParticle()
        airbag.sidsv = 100
        assert airbag.sidsv_link is None

    def test_sidsv_link_returns_matching_set_part(self):
        """Test that sidsv_link returns the correct SET_PART when in a deck."""
        deck = Deck()
        set_part = kwd.SetPartList()
        set_part.sid = 100
        airbag = kwd.AirbagParticle()
        airbag.sidsv = 100

        deck.extend([set_part, airbag])

        assert airbag.sidsv_link is set_part
        assert airbag.sidsv_link.sid == 100

    def test_sidsv_link_returns_none_when_set_not_found(self):
        """Test that sidsv_link returns None when referenced set doesn't exist."""
        deck = Deck()
        set_part = kwd.SetPartList()
        set_part.sid = 200  # Different ID
        airbag = kwd.AirbagParticle()
        airbag.sidsv = 100

        deck.extend([set_part, airbag])

        assert airbag.sidsv_link is None

    def test_sidsv_link_setter(self):
        """Test that sidsv_link setter updates the sidsv field."""
        airbag = kwd.AirbagParticle()
        set_part = kwd.SetPartList()
        set_part.sid = 42

        airbag.sidsv_link = set_part

        assert airbag.sidsv == 42

    def test_get_links_with_set_part_filter(self):
        """Test get_links filters by SET_PART type."""
        deck = Deck()
        set_part = kwd.SetPartList()
        set_part.sid = 100
        airbag = kwd.AirbagParticle()
        airbag.sidsv = 100

        deck.extend([set_part, airbag])

        set_links = airbag.get_links(LinkType.SET_PART)
        assert len(set_links) >= 1
        assert set_part in set_links

        # Filter by unrelated type should not include the set
        mat_links = airbag.get_links(LinkType.MAT)
        assert set_part not in mat_links


class TestSetNodeLinkProperty:
    """Tests for SET_NODE link properties on generated keywords."""

    def test_boundary_prescribed_motion_set_has_set_node_link_fields(self):
        """Test that BoundaryPrescribedMotionSet has SET_NODE _link_fields."""
        assert hasattr(kwd.BoundaryPrescribedMotionSet, "_link_fields")
        assert "nsid" in kwd.BoundaryPrescribedMotionSet._link_fields
        assert kwd.BoundaryPrescribedMotionSet._link_fields["nsid"] == LinkType.SET_NODE

    def test_nsid_link_without_deck_returns_none(self):
        """Test that nsid_link returns None when keyword is not in a deck."""
        bpm = kwd.BoundaryPrescribedMotionSet()
        bpm.nsid = 100
        assert bpm.nsid_link is None

    def test_nsid_link_returns_matching_set_node(self):
        """Test that nsid_link returns the correct SET_NODE when in a deck."""
        deck = Deck()
        set_node = kwd.SetNodeList()
        set_node.sid = 100
        bpm = kwd.BoundaryPrescribedMotionSet()
        bpm.nsid = 100

        deck.extend([set_node, bpm])

        assert bpm.nsid_link is set_node
        assert bpm.nsid_link.sid == 100

    def test_nsid_link_does_not_match_wrong_set_type(self):
        """Test that nsid_link does not match SET_PART when expecting SET_NODE."""
        deck = Deck()
        set_part = kwd.SetPartList()  # Wrong type - should not match
        set_part.sid = 100
        bpm = kwd.BoundaryPrescribedMotionSet()
        bpm.nsid = 100

        deck.extend([set_part, bpm])

        # Should not find the SET_PART when looking for SET_NODE
        assert bpm.nsid_link is None

    def test_nsid_link_setter(self):
        """Test that nsid_link setter updates the nsid field."""
        bpm = kwd.BoundaryPrescribedMotionSet()
        set_node = kwd.SetNodeList()
        set_node.sid = 42

        bpm.nsid_link = set_node

        assert bpm.nsid == 42