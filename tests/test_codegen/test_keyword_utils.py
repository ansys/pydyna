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
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE OF ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

"""Tests for keyword_generation.utils.keyword_utils."""

import pytest
from keyword_generation.utils.keyword_utils import KeywordNames, filter_keywords_by_domain


class TestKeywordNames:
    def test_from_keyword_basic(self):
        names = KeywordNames.from_keyword("SECTION_SHELL")
        assert names.keyword == "SECTION_SHELL"
        assert names.classname == "SectionShell"
        assert names.filename == "section_shell"
        assert names.domain == "section"

    def test_from_keyword_custom_classname(self):
        """The keyword_options dict can override the generated classname."""
        names = KeywordNames.from_keyword("SECTION_SHELL", keyword_options={"classname": "MyCustomClass"})
        assert names.classname == "MyCustomClass"
        # Other fields still derived normally
        assert names.keyword == "SECTION_SHELL"
        assert names.domain == "section"

    def test_from_keyword_no_options(self):
        names = KeywordNames.from_keyword("MAT_NULL", keyword_options=None)
        assert names.classname == "MatNull"
        assert names.domain == "mat"

    def test_from_keyword_options_without_classname(self):
        """Options dict without 'classname' key falls back to generated name."""
        names = KeywordNames.from_keyword("MAT_NULL", keyword_options={"some_other_key": 1})
        assert names.classname == "MatNull"


class TestFilterKeywordsByDomain:
    KEYWORDS = [
        "CONTACT_AUTOMATIC_SINGLE_SURFACE",
        "SECTION_SHELL",
        "MAT_NULL",
        "CONTACT_TIED_NODES_TO_SURFACE",
    ]

    def test_no_filter_returns_all(self):
        result = filter_keywords_by_domain(self.KEYWORDS)
        assert result == self.KEYWORDS

    def test_filter_by_single_domain(self):
        result = filter_keywords_by_domain(self.KEYWORDS, subset_domains=["contact"])
        assert result == ["CONTACT_AUTOMATIC_SINGLE_SURFACE", "CONTACT_TIED_NODES_TO_SURFACE"]

    def test_filter_by_multiple_domains(self):
        result = filter_keywords_by_domain(self.KEYWORDS, subset_domains=["contact", "mat"])
        assert len(result) == 3
        assert "SECTION_SHELL" not in result

    def test_filter_no_match(self):
        result = filter_keywords_by_domain(self.KEYWORDS, subset_domains=["element"])
        assert result == []

    def test_empty_keywords(self):
        result = filter_keywords_by_domain([], subset_domains=["contact"])
        assert result == []

    def test_single_item_list_matching(self):
        """Matches the exact call pattern used in generate.py."""
        assert filter_keywords_by_domain(["CONTACT_AUTOMATIC"], ["contact"]) == ["CONTACT_AUTOMATIC"]
        assert filter_keywords_by_domain(["SECTION_SHELL"], ["contact"]) == []

    def test_single_item_list_no_filter(self):
        """None subset_domains returns the list as-is (fast path)."""
        assert filter_keywords_by_domain(["MAT_NULL"], None) == ["MAT_NULL"]
