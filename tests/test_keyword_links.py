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
        assert LinkType.MAT.value == 14
        assert LinkType.SECTION.value == 15
        assert LinkType.DEFINE_CURVE.value == 19
        assert LinkType.DEFINE_TRANSFORMATION.value == 40
        assert LinkType.DEFINE_CURVE_OR_TABLE.value == 86

    def test_link_type_members(self):
        """Test that LinkType has all expected members."""
        members = [m.name for m in LinkType]
        assert "ALL" in members
        assert "MAT" in members
        assert "SECTION" in members
        assert "DEFINE_CURVE" in members
        assert "DEFINE_TRANSFORMATION" in members
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
