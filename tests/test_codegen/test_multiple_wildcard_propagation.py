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

"""Tests for multiple-type wildcard propagation in codegen.

After manifest normalization, multiple-type entries (e.g. MAT_NULL, RIGIDWALL_PLANAR)
are flattened into first-class manifest keys. Wildcards (e.g. MAT prefix adding TITLE)
must propagate correctly to all generations.
"""

import pytest

import generate as codegen_generate
import keyword_generation.data_model as data_model
from keyword_generation.utils import get_this_folder


@pytest.fixture(scope="module")
def loaded_config():
    """Load codegen config once for the module."""
    codegen_dir = get_this_folder()
    data_model.load(
        str(codegen_dir),
        kwd_file="",
        manifest="",
        additional_cards="",
    )
    yield
    import keyword_generation.data_model as dm

    dm._MODEL = None  # noqa: SLF001


@pytest.mark.codegen
class TestMultipleWildcardPropagation:
    """Test that wildcards propagate to normalized multiple-type entries."""

    def test_mat_null_has_title_option(self, loaded_config):
        """MAT_NULL (from multiple-type) inherits MAT wildcard TITLE add-option."""
        opts = codegen_generate.get_keyword_options("MAT_NULL")
        add_opts = opts.get("generation-options", {}).get("add-option", [])
        titles = [a for a in add_opts if a.get("option-name") == "TITLE"]
        assert len(titles) >= 1, "MAT_NULL should have TITLE from MAT wildcard"

    def test_mat_009_has_title_option(self, loaded_config):
        """MAT_009 (from multiple-type, source-keyword MAT_NULL) inherits MAT wildcard TITLE."""
        opts = codegen_generate.get_keyword_options("MAT_009")
        add_opts = opts.get("generation-options", {}).get("add-option", [])
        titles = [a for a in add_opts if a.get("option-name") == "TITLE"]
        assert len(titles) >= 1, "MAT_009 should have TITLE from MAT wildcard"

    def test_rigidwall_planar_gen1_has_skip_card(self, loaded_config):
        """RIGIDWALL_PLANAR gen-1 preserves skip-card for id_title_card."""
        opts = codegen_generate.get_keyword_options("RIGIDWALL_PLANAR")
        skip_cards = opts.get("generation-options", {}).get("skip-card", [])
        id_title_skips = [s for s in skip_cards if s.get("ref") == "id_title_card"]
        assert len(id_title_skips) >= 1, "RIGIDWALL_PLANAR should skip id_title_card"

    def test_generated_mat_009_has_title_property(self):
        """Generated Mat009 class has title property (integration test)."""
        from ansys.dyna.core.keywords.keyword_classes.auto.mat.mat_009 import Mat009

        obj = Mat009()
        assert hasattr(obj, "title"), "Mat009 should have title property from MAT wildcard"

    def test_generated_mat_null_has_title_property(self):
        """Generated MatNull class has title property (integration test)."""
        from ansys.dyna.core.keywords.keyword_classes.auto.mat.mat_null import MatNull

        obj = MatNull()
        assert hasattr(obj, "title"), "MatNull should have title property from MAT wildcard"
