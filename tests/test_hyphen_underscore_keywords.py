# Copyright (C) 2021 - 2025 ANSYS, Inc. and/or its affiliates.
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

"""Tests for hyphen/underscore keyword variant handling.

Some LS-DYNA keywords exist in both hyphenated and underscored forms, e.g.:
- *MAT_RAMBERG-OSGOOD vs *MAT_RAMBERG_OSGOOD
- *SECTION_SPRING-DAMPER vs *SECTION_SPRING_DAMPER

These tests verify that:
1. Both variants are recognized and map to distinct classes
2. Each variant preserves its identity through deck loading/writing
3. Identical definitions use alias classes (underscore inherits from hyphen)
4. Non-identical definitions generate independent classes
"""

import pytest

from ansys.dyna.core import Deck
from ansys.dyna.core.keywords.keyword_classes.type_mapping import TypeMapping


class TestTypeMapping:
    """Test that TypeMapping correctly handles hyphen/underscore variants."""

    def test_ramberg_osgood_hyphen_maps_to_hyphen_class(self):
        """Hyphenated keyword maps to hyphenated class name."""
        assert TypeMapping.get("*MAT_RAMBERG-OSGOOD") == "MatRamberg_Osgood"

    def test_ramberg_osgood_underscore_maps_to_underscore_class(self):
        """Underscored keyword maps to underscored class name."""
        assert TypeMapping.get("*MAT_RAMBERG_OSGOOD") == "MatRambergOsgood"

    def test_ramberg_osgood_variants_map_to_different_classes(self):
        """Hyphen and underscore variants map to distinct class names."""
        hyphen_class = TypeMapping.get("*MAT_RAMBERG-OSGOOD")
        underscore_class = TypeMapping.get("*MAT_RAMBERG_OSGOOD")
        assert hyphen_class != underscore_class

    def test_section_spring_damper_hyphen_maps_to_hyphen_class(self):
        """SECTION_SPRING-DAMPER hyphenated keyword maps correctly."""
        assert TypeMapping.get("*SECTION_SPRING-DAMPER") == "SectionSpring_Damper"

    def test_section_spring_damper_underscore_maps_to_underscore_class(self):
        """SECTION_SPRING_DAMPER underscored keyword maps correctly."""
        assert TypeMapping.get("*SECTION_SPRING_DAMPER") == "SectionSpringDamper"

    def test_section_spring_damper_variants_map_to_different_classes(self):
        """SECTION_SPRING variants map to distinct class names."""
        hyphen_class = TypeMapping.get("*SECTION_SPRING-DAMPER")
        underscore_class = TypeMapping.get("*SECTION_SPRING_DAMPER")
        assert hyphen_class != underscore_class

    @pytest.mark.parametrize(
        "hyphen_keyword,underscore_keyword",
        [
            ("*MAT_RAMBERG-OSGOOD", "*MAT_RAMBERG_OSGOOD"),
            ("*SECTION_SPRING-DAMPER", "*SECTION_SPRING_DAMPER"),
            ("*SENSOR_DEFINE_CALC-MATH", "*SENSOR_DEFINE_CALC_MATH"),
            ("*ALE_STRUCTURED_MULTI-MATERIAL_GROUP", "*ALE_STRUCTURED_MULTI_MATERIAL_GROUP"),
        ],
    )
    def test_both_variants_exist_in_type_mapping(self, hyphen_keyword, underscore_keyword):
        """Both hyphen and underscore variants should exist in TypeMapping."""
        assert TypeMapping.get(hyphen_keyword) is not None, f"Missing: {hyphen_keyword}"
        assert TypeMapping.get(underscore_keyword) is not None, f"Missing: {underscore_keyword}"


class TestKeywordClassInheritance:
    """Test inheritance relationships between hyphen/underscore variant classes."""

    def test_ramberg_osgood_underscore_inherits_from_hyphen(self):
        """MatRambergOsgood (underscore) should inherit from MatRamberg_Osgood (hyphen)."""
        from ansys.dyna.core.keywords.keyword_classes.auto.mat import (
            MatRamberg_Osgood,
            MatRambergOsgood,
        )

        assert issubclass(MatRambergOsgood, MatRamberg_Osgood)

    def test_ramberg_osgood_hyphen_subkeyword(self):
        """MatRamberg_Osgood should have hyphenated subkeyword."""
        from ansys.dyna.core.keywords.keyword_classes.auto.mat import MatRamberg_Osgood

        instance = MatRamberg_Osgood()
        assert instance.subkeyword == "RAMBERG-OSGOOD"

    def test_ramberg_osgood_underscore_subkeyword(self):
        """MatRambergOsgood should have underscored subkeyword."""
        from ansys.dyna.core.keywords.keyword_classes.auto.mat import MatRambergOsgood

        instance = MatRambergOsgood()
        assert instance.subkeyword == "RAMBERG_OSGOOD"

    def test_section_spring_damper_not_inherited(self):
        """SECTION_SPRING variants have different definitions, so no inheritance."""
        from ansys.dyna.core.keywords.keyword_classes.auto.section import (
            SectionSpring_Damper,
            SectionSpringDamper,
        )

        # These are independent classes because their definitions differ
        assert not issubclass(SectionSpringDamper, SectionSpring_Damper)

    def test_section_spring_damper_hyphen_subkeyword(self):
        """SectionSpring_Damper should have hyphenated subkeyword."""
        from ansys.dyna.core.keywords.keyword_classes.auto.section import SectionSpring_Damper

        instance = SectionSpring_Damper()
        assert instance.subkeyword == "SPRING-DAMPER"

    def test_section_spring_damper_underscore_subkeyword(self):
        """SectionSpringDamper should have underscored subkeyword."""
        from ansys.dyna.core.keywords.keyword_classes.auto.section import SectionSpringDamper

        instance = SectionSpringDamper()
        assert instance.subkeyword == "SPRING_DAMPER"


class TestDeckLoading:
    """Test that deck loading correctly identifies hyphen/underscore variants."""

    def test_load_hyphen_keyword_gets_hyphen_class(self):
        """Loading a deck with hyphenated keyword gets the hyphenated class."""
        deck_content = """\
*KEYWORD
*MAT_RAMBERG-OSGOOD
         1       0.0       0.0       0.0       0.0       0.0       0.0       0.0
*END
"""
        deck = Deck()
        deck.loads(deck_content)

        # Find the MAT keyword
        mat_keywords = [kw for kw in deck.keywords if kw.keyword == "MAT"]
        assert len(mat_keywords) == 1

        mat_kw = mat_keywords[0]
        assert mat_kw.__class__.__name__ == "MatRamberg_Osgood"
        assert mat_kw.subkeyword == "RAMBERG-OSGOOD"

    def test_load_underscore_keyword_gets_underscore_class(self):
        """Loading a deck with underscored keyword gets the underscored class."""
        deck_content = """\
*KEYWORD
*MAT_RAMBERG_OSGOOD
         1       0.0       0.0       0.0       0.0       0.0       0.0       0.0
*END
"""
        deck = Deck()
        deck.loads(deck_content)

        # Find the MAT keyword
        mat_keywords = [kw for kw in deck.keywords if kw.keyword == "MAT"]
        assert len(mat_keywords) == 1

        mat_kw = mat_keywords[0]
        assert mat_kw.__class__.__name__ == "MatRambergOsgood"
        assert mat_kw.subkeyword == "RAMBERG_OSGOOD"


class TestRoundTrip:
    """Test that keywords preserve their identity through round-trip."""

    def test_hyphen_keyword_round_trip(self):
        """Hyphenated keyword should round-trip with hyphen preserved."""
        deck_content = """\
*KEYWORD
*MAT_RAMBERG-OSGOOD
         1       0.0       0.0       0.0       0.0       0.0       0.0       0.0
*END
"""
        deck = Deck()
        deck.loads(deck_content)

        # Write it back out
        output = deck.write()

        # Should contain the hyphenated form
        assert "*MAT_RAMBERG-OSGOOD" in output
        assert "*MAT_RAMBERG_OSGOOD" not in output

    def test_underscore_keyword_round_trip(self):
        """Underscored keyword should round-trip with underscore preserved."""
        deck_content = """\
*KEYWORD
*MAT_RAMBERG_OSGOOD
         1       0.0       0.0       0.0       0.0       0.0       0.0       0.0
*END
"""
        deck = Deck()
        deck.loads(deck_content)

        # Write it back out
        output = deck.write()

        # Should contain the underscored form
        assert "*MAT_RAMBERG_OSGOOD" in output
        assert "*MAT_RAMBERG-OSGOOD" not in output

    def test_mixed_variants_preserved(self):
        """Deck with both variants should preserve each one's form."""
        deck_content = """\
*KEYWORD
*MAT_RAMBERG-OSGOOD
         1       0.0       0.0       0.0       0.0       0.0       0.0       0.0
*MAT_RAMBERG_OSGOOD
         2       0.0       0.0       0.0       0.0       0.0       0.0       0.0
*END
"""
        deck = Deck()
        deck.loads(deck_content)

        # Write it back out
        output = deck.write()

        # Both forms should be present
        assert "*MAT_RAMBERG-OSGOOD" in output
        assert "*MAT_RAMBERG_OSGOOD" in output


class TestKeywordTitle:
    """Test that keyword titles are correctly generated."""

    def test_hyphen_class_title(self):
        """Hyphenated class should generate hyphenated title."""
        from ansys.dyna.core.keywords.keyword_classes.auto.mat import MatRamberg_Osgood

        instance = MatRamberg_Osgood()
        assert instance.get_title() == "*MAT_RAMBERG-OSGOOD"

    def test_underscore_class_title(self):
        """Underscored class should generate underscored title."""
        from ansys.dyna.core.keywords.keyword_classes.auto.mat import MatRambergOsgood

        instance = MatRambergOsgood()
        assert instance.get_title() == "*MAT_RAMBERG_OSGOOD"

    def test_section_spring_damper_hyphen_title(self):
        """SectionSpring_Damper should generate hyphenated title."""
        from ansys.dyna.core.keywords.keyword_classes.auto.section import SectionSpring_Damper

        instance = SectionSpring_Damper()
        assert instance.get_title() == "*SECTION_SPRING-DAMPER"

    def test_section_spring_damper_underscore_title(self):
        """SectionSpringDamper should generate underscored title."""
        from ansys.dyna.core.keywords.keyword_classes.auto.section import SectionSpringDamper

        instance = SectionSpringDamper()
        assert instance.get_title() == "*SECTION_SPRING_DAMPER"
