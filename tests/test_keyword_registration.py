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

"""Tests for keyword overrides via ImportContext and legacy keyword support."""

import warnings

from ansys.dyna.core.lib.deck import Deck
from ansys.dyna.core.lib.import_handler import ImportContext


class TestImportContextKeywordOverrides:
    """Tests for ImportContext.keyword_overrides functionality."""

    def test_import_context_default_empty_overrides(self):
        """Test that ImportContext has empty keyword_overrides by default."""
        context = ImportContext()
        assert context.keyword_overrides == {}

    def test_import_context_with_overrides(self):
        """Test creating ImportContext with keyword overrides."""
        from ansys.dyna.core.keywords.keyword_classes.manual.mat_295_version_0_9_1 import Mat295Legacy

        context = ImportContext(keyword_overrides={"*MAT_295": Mat295Legacy})
        assert context.keyword_overrides["*MAT_295"] is Mat295Legacy

    def test_import_context_multiple_overrides(self):
        """Test ImportContext with multiple keyword overrides."""
        from ansys.dyna.core.keywords.keyword_classes.manual.mat_295_version_0_9_1 import (
            Mat295Legacy,
            MatAnisotropicHyperelasticLegacy,
        )

        context = ImportContext(
            keyword_overrides={
                "*MAT_295": Mat295Legacy,
                "*MAT_ANISOTROPIC_HYPERELASTIC": MatAnisotropicHyperelasticLegacy,
            }
        )
        assert context.keyword_overrides["*MAT_295"] is Mat295Legacy
        assert context.keyword_overrides["*MAT_ANISOTROPIC_HYPERELASTIC"] is MatAnisotropicHyperelasticLegacy


class TestLegacyMat295:
    """Tests for legacy Mat295 implementation."""

    def test_legacy_class_emits_deprecation_warning(self):
        """Test that constructing Mat295Legacy emits a DeprecationWarning."""
        from ansys.dyna.core.keywords.keyword_classes.manual.mat_295_version_0_9_1 import Mat295Legacy

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            _ = Mat295Legacy()
            assert len(w) == 1
            assert issubclass(w[0].category, DeprecationWarning)
            assert "Mat295Legacy is deprecated" in str(w[0].message)
            assert "fiber_families" in str(w[0].message)

    def test_legacy_subclass_emits_deprecation_warning(self):
        """Test that MatAnisotropicHyperelasticLegacy also emits warning."""
        from ansys.dyna.core.keywords.keyword_classes.manual.mat_295_version_0_9_1 import (
            MatAnisotropicHyperelasticLegacy,
        )

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            mat = MatAnisotropicHyperelasticLegacy()
            assert len(w) == 1
            assert issubclass(w[0].category, DeprecationWarning)
            # Check subkeyword is set correctly
            assert mat.subkeyword == "ANISOTROPIC_HYPERELASTIC"

    def test_legacy_class_has_ftype_property(self):
        """Test that legacy class maintains the ftype property."""
        from ansys.dyna.core.keywords.keyword_classes.manual.mat_295_version_0_9_1 import Mat295Legacy

        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            mat = Mat295Legacy()
            assert mat.ftype is None
            mat.ftype = 1
            assert mat.ftype == 1


class TestKeywordOverridesLoading:
    """Tests for loading keywords with ImportContext overrides."""

    MAT_295_DATA = """*MAT_295
$#     mid       rho      aopt
         1    1.0E-9       2.0
$#   title     itype      beta        nu
ISO             1       0.0      0.495
$#     mu1       mu2       mu3       mu4       mu5       mu6       mu7       mu8
      20.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
$#  alpha1    alpha2    alpha3    alpha4    alpha5    alpha6    alpha7    alpha8
       2.0       0.0       0.0       0.0       0.0       0.0       0.0       0.0
"""

    def test_load_with_keyword_override(self):
        """Test that keyword override in ImportContext is used during loading."""
        from ansys.dyna.core.keywords.keyword_classes.manual.mat_295_version_0_9_1 import Mat295Legacy

        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)

            deck = Deck()
            context = ImportContext(
                deck=deck,
                keyword_overrides={"*MAT_295": Mat295Legacy},
            )
            deck.loads(self.MAT_295_DATA, context=context)

            assert len(deck.keywords) == 1
            kwd = deck.keywords[0]
            assert isinstance(kwd, Mat295Legacy)
            assert kwd.mid == 1

    def test_load_without_override_uses_default(self):
        """Test that default keyword class is used when no override specified."""
        from ansys.dyna.core.keywords.keyword_classes.manual.mat_295 import Mat295

        deck = Deck()
        deck.loads(self.MAT_295_DATA)

        assert len(deck.keywords) == 1
        kwd = deck.keywords[0]
        # Should be the current (non-legacy) Mat295
        assert type(kwd).__name__ == "Mat295"
        # And it should be the manual subclass, not legacy
        assert isinstance(kwd, Mat295)

    def test_override_does_not_affect_other_contexts(self):
        """Test that overrides are context-specific."""
        from ansys.dyna.core.keywords.keyword_classes.manual.mat_295 import Mat295
        from ansys.dyna.core.keywords.keyword_classes.manual.mat_295_version_0_9_1 import Mat295Legacy

        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)

            # Load with override
            deck1 = Deck()
            context1 = ImportContext(
                deck=deck1,
                keyword_overrides={"*MAT_295": Mat295Legacy},
            )
            deck1.loads(self.MAT_295_DATA, context=context1)

            # Load without override
            deck2 = Deck()
            deck2.loads(self.MAT_295_DATA)

            # deck1 should have legacy class
            assert isinstance(deck1.keywords[0], Mat295Legacy)
            # deck2 should have current class
            assert isinstance(deck2.keywords[0], Mat295)
            assert not isinstance(deck2.keywords[0], Mat295Legacy)
