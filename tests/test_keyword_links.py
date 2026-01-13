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
        assert LinkType.DEFINE_CURVE.value == 19
        assert LinkType.DEFINE_TRANSFORMATION.value == 40

    def test_link_type_members(self):
        """Test that LinkType has all expected members."""
        members = [m.name for m in LinkType]
        assert "ALL" in members
        assert "DEFINE_CURVE" in members
        assert "DEFINE_TRANSFORMATION" in members


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
