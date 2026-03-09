# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#

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
