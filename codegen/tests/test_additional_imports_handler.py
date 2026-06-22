# Copyright (C) 2023 - 2026 Synopsys, Inc. and ANSYS, Inc. All rights reserved.
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

"""Tests for AdditionalImportsHandler."""

import pytest

from keyword_generation.data_model.keyword_data import KeywordData
from keyword_generation.handlers.additional_imports import AdditionalImportSettings, AdditionalImportsHandler


class TestAdditionalImportsHandler:
    """Test suite for AdditionalImportsHandler."""

    def test_single_import(self):
        """Test adding a single additional import."""
        handler = AdditionalImportsHandler()
        kwd_data = KeywordData(
            keyword="IGA",
            subkeyword="1D_NURBS_XYZ",
            title="IGA_1D_NURBS_XYZ",
            classname="Iga1DNurbsXyz",
        )

        settings = [{"name": "math"}]

        handler.handle(kwd_data, settings)

        assert kwd_data.additional_imports == ["math"]

    def test_multiple_imports(self):
        """Test adding multiple additional imports."""
        handler = AdditionalImportsHandler()
        kwd_data = KeywordData(
            keyword="TEST",
            subkeyword="KEYWORD",
            title="TEST_KEYWORD",
            classname="TestKeyword",
        )

        settings = [{"name": "math"}, {"name": "functools"}, {"name": "itertools"}]

        handler.handle(kwd_data, settings)

        assert kwd_data.additional_imports == ["math", "functools", "itertools"]

    def test_empty_settings(self):
        """Test that empty settings list leaves additional_imports empty."""
        handler = AdditionalImportsHandler()
        kwd_data = KeywordData(
            keyword="TEST",
            subkeyword="KEYWORD",
            title="TEST_KEYWORD",
            classname="TestKeyword",
        )

        handler.handle(kwd_data, [])

        assert kwd_data.additional_imports == []

    def test_duplicate_import_is_ignored(self):
        """Test that adding the same module twice produces only one entry."""
        handler = AdditionalImportsHandler()
        kwd_data = KeywordData(
            keyword="TEST",
            subkeyword="KEYWORD",
            title="TEST_KEYWORD",
            classname="TestKeyword",
        )

        settings = [{"name": "math"}, {"name": "math"}]

        handler.handle(kwd_data, settings)

        assert kwd_data.additional_imports == ["math"]

    def test_preserves_existing_imports(self):
        """Test that handler appends to pre-populated additional_imports."""
        handler = AdditionalImportsHandler()
        kwd_data = KeywordData(
            keyword="TEST",
            subkeyword="KEYWORD",
            title="TEST_KEYWORD",
            classname="TestKeyword",
        )
        kwd_data.additional_imports = ["os"]

        settings = [{"name": "math"}]

        handler.handle(kwd_data, settings)

        assert kwd_data.additional_imports == ["os", "math"]

    def test_duplicate_of_existing_import_is_ignored(self):
        """Test that a module already present is not added again."""
        handler = AdditionalImportsHandler()
        kwd_data = KeywordData(
            keyword="TEST",
            subkeyword="KEYWORD",
            title="TEST_KEYWORD",
            classname="TestKeyword",
        )
        kwd_data.additional_imports = ["math"]

        settings = [{"name": "math"}]

        handler.handle(kwd_data, settings)

        assert kwd_data.additional_imports == ["math"]


class TestAdditionalImportSettings:
    """Test suite for AdditionalImportSettings dataclass."""

    def test_from_dict(self):
        """Test creating settings from a manifest-style dict."""
        settings = AdditionalImportSettings.from_dict({"name": "math"})

        assert settings.name == "math"

    def test_missing_name_raises(self):
        """Test that a missing 'name' key raises KeyError."""
        with pytest.raises(KeyError):
            AdditionalImportSettings.from_dict({})
