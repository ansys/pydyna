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


class TestLegacyInitialStrainShell:
    """Tests for legacy InitialStrainShell implementation."""

    def test_legacy_class_emits_deprecation_warning(self):
        """Test that constructing InitialStrainShellLegacy emits a DeprecationWarning."""
        from ansys.dyna.core.keywords.keyword_classes.manual.initial_strain_shell_version_0_9_1 import (
            InitialStrainShellLegacy,
        )

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            _ = InitialStrainShellLegacy()
            assert len(w) == 1
            assert issubclass(w[0].category, DeprecationWarning)
            assert "InitialStrainShellLegacy is deprecated" in str(w[0].message)
            assert "CardSet" in str(w[0].message)

    def test_legacy_class_has_table_card_api(self):
        """Test that legacy class maintains the TableCard/DataFrame API with add_set()."""
        from ansys.dyna.core.keywords.keyword_classes.manual.initial_strain_shell_version_0_9_1 import (
            InitialStrainShellLegacy,
        )

        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            kwd = InitialStrainShellLegacy()
            # Sets should initially be empty (unbounded length)
            assert len(kwd.sets) == 0
            
            # Use add_set() to add a CardSet
            kwd.add_set(eid=1, nplane=1, nthick=5, large=0)
            assert len(kwd.sets) == 1
            
            # The CardSet should be InitialStrainShellLegacyCardSet
            assert kwd.sets[0].__class__.__name__ == "InitialStrainShellLegacyCardSet"
            # The CardSet should have the TableCard API with strains property
            assert hasattr(kwd.sets[0], "strains")
            # strains should be a pandas DataFrame
            import pandas as pd
            assert isinstance(kwd.sets[0].strains, pd.DataFrame)


class TestLegacyInitialStressShell:
    """Tests for legacy InitialStressShell implementation."""

    def test_legacy_class_emits_deprecation_warning(self):
        """Test that constructing InitialStressShellLegacy emits a DeprecationWarning."""
        from ansys.dyna.core.keywords.keyword_classes.manual.initial_stress_shell_version_0_9_1 import (
            InitialStressShellLegacy,
        )

        with warnings.catch_warnings(record=True) as w:
            warnings.simplefilter("always")
            _ = InitialStressShellLegacy()
            assert len(w) == 1
            assert issubclass(w[0].category, DeprecationWarning)
            assert "InitialStressShellLegacy is deprecated" in str(w[0].message)
            assert "CardSet" in str(w[0].message)

    def test_legacy_class_has_table_card_api(self):
        """Test that legacy class maintains the TableCard/DataFrame API with add_set()."""
        from ansys.dyna.core.keywords.keyword_classes.manual.initial_stress_shell_version_0_9_1 import (
            InitialStressShellLegacy,
        )

        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            kwd = InitialStressShellLegacy()
            # Sets should initially be empty (unbounded length)
            assert len(kwd.sets) == 0
            
            # Use add_set() to add a CardSet
            kwd.add_set(eid=1, nplane=1, nthick=1, nhisv=19, large=0)
            assert len(kwd.sets) == 1
            
            # The CardSet should be InitialStressShellLegacyCardSet
            assert kwd.sets[0].__class__.__name__ == "InitialStressShellLegacyCardSet"
            # The CardSet should have nested sets (stress layers)
            assert hasattr(kwd.sets[0], "sets")
            # The nested sets should be InitialStressShellLegacyThicknessLargeCardSet
            assert len(kwd.sets[0].sets) == 1  # nplane * nthick = 1 * 1
            assert kwd.sets[0].sets[0].__class__.__name__ == "InitialStressShellLegacyThicknessLargeCardSet"

    def test_legacy_thickness_large_cardset_instantiation(self):
        """Test that InitialStressShellLegacyThicknessLargeCardSet can be instantiated properly.
        
        This tests card_set.py functionality using a real keyword CardSet class.
        """
        from ansys.dyna.core.keywords.keyword_classes.manual.initial_stress_shell_version_0_9_1 import (
            InitialStressShellLegacy,
            InitialStressShellLegacyThicknessLargeCardSet,
        )

        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)
            # Create a parent keyword to pass as context
            parent_kwd = InitialStressShellLegacy()
            
            # Properly instantiate the CardSet with required kwargs
            cardset = InitialStressShellLegacyThicknessLargeCardSet(
                keyword=parent_kwd,
                parent=parent_kwd
            )
            
            # Verify it has the expected structure
            assert hasattr(cardset, "t")
            assert hasattr(cardset, "sigxx")
            assert hasattr(cardset, "sigyy")
            assert hasattr(cardset, "sigzz")
            assert hasattr(cardset, "sigxy")
            assert hasattr(cardset, "sigyz")
            assert hasattr(cardset, "sigzx")
            assert hasattr(cardset, "eps")
            assert hasattr(cardset, "hisv")


class TestInitialStrainStressLegacyLoading:
    """Tests for loading INITIAL_STRAIN_SHELL and INITIAL_STRESS_SHELL with legacy overrides."""

    # Use actual test data from test_keywords.py
    INITIAL_STRAIN_DATA = """*INITIAL_STRAIN_SHELL
         1         1         5
 1.96E-003 7.65E-003-9.61E-003-2.29E-004-5.79E-004 1.41E-004-1.00E+000
 2.30E-003 7.56E-003-9.86E-003-6.10E-004-5.89E-004 1.61E-004-5.00E-001
 2.63E-003 7.47E-003-1.01E-002-9.91E-004-5.98E-004 1.80E-004 0.00E+000
 2.97E-003 7.36E-003-1.03E-002-1.34E-003-6.07E-004 1.98E-004 5.00E-001
 3.32E-003 7.26E-003-1.06E-002-1.68E-003-6.15E-004 2.17E-004 1.00E+000
         2         1         5
 1.96E-003 7.27E-003-9.23E-003-3.36E-004-3.96E-004-1.32E-005-1.00E+000
 2.11E-003 7.05E-003-9.16E-003-5.73E-004-3.88E-004-7.72E-006-5.00E-001
 2.27E-003 6.82E-003-9.09E-003-8.10E-004-3.81E-004-2.20E-006 0.00E+000
 2.45E-003 6.58E-003-9.03E-003-1.04E-003-3.73E-004 3.18E-006 5.00E-001
 2.63E-003 6.34E-003-8.96E-003-1.28E-003-3.65E-004 8.55E-006 1.00E+000
         3         1         5
 2.30E-003 7.28E-003-9.58E-003 1.63E-004-3.67E-004-2.03E-005-1.00E+000
 2.35E-003 7.14E-003-9.48E-003 1.57E-004-3.62E-004-2.01E-005-5.00E-001
 2.39E-003 7.00E-003-9.39E-003 1.51E-004-3.57E-004-1.99E-005 0.00E+000
 2.43E-003 6.88E-003-9.31E-003 1.73E-004-3.52E-004-2.03E-005 5.00E-001
 2.47E-003 6.75E-003-9.22E-003 1.96E-004-3.48E-004-2.07E-005 1.00E+000"""

    def test_load_initial_strain_with_keyword_override(self):
        """Test that keyword override is used when loading INITIAL_STRAIN_SHELL."""
        from ansys.dyna.core.keywords.keyword_classes.manual.initial_strain_shell_version_0_9_1 import (
            InitialStrainShellLegacy,
        )

        with warnings.catch_warnings():
            warnings.simplefilter("ignore", DeprecationWarning)

            deck = Deck()
            context = ImportContext(
                deck=deck,
                keyword_overrides={"*INITIAL_STRAIN_SHELL": InitialStrainShellLegacy},
            )
            deck.loads(self.INITIAL_STRAIN_DATA, context=context)

            assert len(deck.keywords) == 1
            kwd = deck.keywords[0]
            assert isinstance(kwd, InitialStrainShellLegacy)
            
            # Test that legacy API works - same expectations as test_initial_strain_shell
            assert len(kwd.sets) == 3
            assert kwd.sets[1].eid == 2
            # The legacy version uses TableCard with DataFrame API
            assert hasattr(kwd.sets[0], "strains")
            import pandas as pd
            assert isinstance(kwd.sets[2].strains, pd.DataFrame)
            # Access strains via DataFrame (legacy API)
            assert abs(kwd.sets[2].strains.iloc[0]["epszz"] - (-9.58e-003)) < 1e-6

    def test_load_without_override_uses_default(self):
        """Test that default keyword class is used when no override specified."""
        from ansys.dyna.core.keywords.keyword_classes.auto.boundary.initial_strain_shell import (
            InitialStrainShell,
        )

        deck = Deck()
        deck.loads(self.INITIAL_STRAIN_DATA)

        assert len(deck.keywords) == 1
        kwd = deck.keywords[0]
        # Should be the current (non-legacy) InitialStrainShell
        assert isinstance(kwd, InitialStrainShell)
        
        # Test that new API works - same expectations as test_initial_strain_shell
        assert len(kwd.sets) == 3
        assert kwd.sets[1].eid == 2
        # The new version uses CardSet with list of structured objects (not DataFrame)
        assert hasattr(kwd.sets[0], "strains")
        # Access strains via list indexing (new API)
        assert abs(kwd.sets[2].strains[0].epszz - (-9.58e-003)) < 1e-6
