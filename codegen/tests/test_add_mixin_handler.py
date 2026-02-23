# Copyright (C) 2023 - 2026 ANSYS, Inc. and/or its affiliates.
# SPDX-License-Identifier: MIT
#

"""Tests for AddMixinHandler."""

import pytest

from keyword_generation.data_model.keyword_data import KeywordData
from keyword_generation.data_model.metadata import MixinImport
from keyword_generation.handlers.add_mixin import AddMixinHandler


class TestAddMixinHandler:
    """Test suite for AddMixinHandler."""

    def test_basic_mixin_addition(self):
        """Test adding a single mixin to a keyword."""
        handler = AddMixinHandler()
        kwd_data = KeywordData(
            keyword="DEFINE",
            subkeyword="CURVE",
            title="Test Curve",
            classname="DefineCurve",
        )

        settings = [{"source": "ansys.dyna.core.lib.mixins.curve_plotting", "name": "CurvePlottingMixin"}]

        handler.handle(kwd_data, settings)

        assert len(kwd_data.mixins) == 1
        assert kwd_data.mixins[0] == "CurvePlottingMixin"
        assert len(kwd_data.mixin_imports) == 1
        assert isinstance(kwd_data.mixin_imports[0], MixinImport)
        assert kwd_data.mixin_imports[0].source == "ansys.dyna.core.lib.mixins.curve_plotting"
        assert kwd_data.mixin_imports[0].names == ["CurvePlottingMixin"]

    def test_multiple_mixins(self):
        """Test adding multiple mixins to a keyword."""
        handler = AddMixinHandler()
        kwd_data = KeywordData(
            keyword="TEST",
            subkeyword="KEYWORD",
            title="Test Keyword",
            classname="TestKeyword",
        )

        settings = [
            {"source": "module.path.one", "name": "MixinOne"},
            {"source": "module.path.two", "name": "MixinTwo"},
            {"source": "module.path.three", "name": "MixinThree"},
        ]

        handler.handle(kwd_data, settings)

        assert len(kwd_data.mixins) == 3
        assert kwd_data.mixins == ["MixinOne", "MixinTwo", "MixinThree"]
        assert len(kwd_data.mixin_imports) == 3

        # Verify all imports
        for i, setting in enumerate(settings):
            assert kwd_data.mixin_imports[i].source == setting["source"]
            assert kwd_data.mixin_imports[i].names == [setting["name"]]

    def test_empty_settings(self):
        """Test that empty settings list initializes empty lists."""
        handler = AddMixinHandler()
        kwd_data = KeywordData(
            keyword="TEST",
            subkeyword="KEYWORD",
            title="Test Keyword",
            classname="TestKeyword",
        )

        settings = []

        handler.handle(kwd_data, settings)

        assert kwd_data.mixins == []
        assert kwd_data.mixin_imports == []

    def test_preserves_existing_mixins(self):
        """Test that handler appends to existing mixins (if initialized)."""
        handler = AddMixinHandler()
        kwd_data = KeywordData(
            keyword="TEST",
            subkeyword="KEYWORD",
            title="Test Keyword",
            classname="TestKeyword",
        )

        # Pre-populate with existing mixin
        kwd_data.mixins = ["ExistingMixin"]
        kwd_data.mixin_imports = [MixinImport(source="existing.module", names=["ExistingMixin"])]

        settings = [{"source": "new.module", "name": "NewMixin"}]

        handler.handle(kwd_data, settings)

        # Should have both old and new
        assert len(kwd_data.mixins) == 2
        assert kwd_data.mixins == ["ExistingMixin", "NewMixin"]
        assert len(kwd_data.mixin_imports) == 2

    def test_full_module_path(self):
        """Test that full module paths with dots are preserved."""
        handler = AddMixinHandler()
        kwd_data = KeywordData(
            keyword="TEST",
            subkeyword="KEYWORD",
            title="Test Keyword",
            classname="TestKeyword",
        )

        settings = [{"source": "ansys.dyna.core.lib.mixins.curve_plotting", "name": "CurvePlottingMixin"}]

        handler.handle(kwd_data, settings)

        # Verify the full path is preserved
        assert kwd_data.mixin_imports[0].source == "ansys.dyna.core.lib.mixins.curve_plotting"

    def test_short_module_path(self):
        """Test that short module paths (no dots) work correctly."""
        handler = AddMixinHandler()
        kwd_data = KeywordData(
            keyword="TEST",
            subkeyword="KEYWORD",
            title="Test Keyword",
            classname="TestKeyword",
        )

        settings = [{"source": "include_card", "name": "IncludeCardMixin"}]

        handler.handle(kwd_data, settings)

        # Should work just fine - templates will handle the import path
        assert kwd_data.mixin_imports[0].source == "include_card"


class TestAddMixinSettings:
    """Test suite for AddMixinSettings dataclass."""

    def test_from_dict(self):
        """Test creating settings from dictionary."""
        from keyword_generation.handlers.add_mixin import AddMixinSettings

        data = {"source": "test.module.path", "name": "TestMixin"}

        settings = AddMixinSettings.from_dict(data)

        assert settings.source == "test.module.path"
        assert settings.name == "TestMixin"

    def test_missing_required_fields(self):
        """Test that missing required fields raise appropriate errors."""
        from keyword_generation.handlers.add_mixin import AddMixinSettings

        # Missing 'name'
        with pytest.raises(KeyError):
            AddMixinSettings.from_dict({"source": "test.module"})

        # Missing 'source'
        with pytest.raises(KeyError):
            AddMixinSettings.from_dict({"name": "TestMixin"})
