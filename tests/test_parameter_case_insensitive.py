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

"""Tests for case-insensitive parameter handling in PyDyna.

LS-DYNA parameter names are case-insensitive. This test suite verifies that
PyDyna handles parameters in a case-insensitive manner while preserving the
original casing for logging and display purposes.
"""

import pytest

from ansys.dyna.core.lib.parameters import ParameterSet


class TestParameterCaseInsensitivity:
    """Test case-insensitive parameter lookup and storage."""

    def test_add_and_get_same_case(self):
        """Test adding and retrieving a parameter with same casing."""
        params = ParameterSet()
        params.add("density", 7850.0)
        assert params.get("density") == 7850.0

    def test_add_lowercase_get_uppercase(self):
        """Test adding lowercase parameter and retrieving with uppercase."""
        params = ParameterSet()
        params.add("density", 7850.0)
        assert params.get("DENSITY") == 7850.0

    def test_add_uppercase_get_lowercase(self):
        """Test adding uppercase parameter and retrieving with lowercase."""
        params = ParameterSet()
        params.add("DENSITY", 7850.0)
        assert params.get("density") == 7850.0

    def test_add_mixedcase_get_different_case(self):
        """Test adding mixed-case parameter and retrieving with different case."""
        params = ParameterSet()
        params.add("Density", 7850.0)
        assert params.get("density") == 7850.0
        assert params.get("DENSITY") == 7850.0
        assert params.get("DENSity") == 7850.0

    def test_add_local_and_get_different_case(self):
        """Test adding local parameter and retrieving with different case."""
        params = ParameterSet()
        params.add_local("localParam", 999.0)
        assert params.get("localparam") == 999.0
        assert params.get("LOCALPARAM") == 999.0
        assert params.get("LocalParam") == 999.0

    def test_multiple_parameters_different_cases(self):
        """Test multiple parameters with case insensitivity."""
        params = ParameterSet()
        params.add("Density", 7850.0)
        params.add("THICKNESS", 0.1)
        params.add("youngs_modulus", 210000.0)

        # All should be retrievable with any casing
        assert params.get("density") == 7850.0
        assert params.get("DENSITY") == 7850.0
        assert params.get("thickness") == 0.1
        assert params.get("THICKNESS") == 0.1
        assert params.get("YOUNGS_MODULUS") == 210000.0
        assert params.get("youngs_modulus") == 210000.0

    def test_parent_scope_case_insensitive(self):
        """Test case-insensitive lookup across parent/child scopes."""
        parent = ParameterSet()
        parent.add("GlobalParam", 42)

        child = parent.copy_with_child_scope()
        assert child.get("globalparam") == 42
        assert child.get("GLOBALPARAM") == 42
        assert child.get("GlobalParam") == 42

    def test_child_override_parent_case_insensitive(self):
        """Test child parameter overrides parent even with different casing."""
        parent = ParameterSet()
        parent.add("Param", 100)

        child = parent.copy_with_child_scope()
        child.add("param", 200)  # Different case, should override parent

        # Child sees its own value
        assert child.get("param") == 200
        assert child.get("PARAM") == 200

        # Parent still has original value
        assert parent.get("Param") == 100

    def test_local_param_not_visible_to_parent_case_insensitive(self):
        """Test local parameter isolation even with different casing."""
        parent = ParameterSet()
        child = parent.copy_with_child_scope()
        child.add_local("ChildLocal", 555)

        # Child can get it
        assert child.get("childlocal") == 555

        # Parent cannot, even with different casing
        with pytest.raises(KeyError):
            parent.get("CHILDLOCAL")

    def test_parameter_not_found_with_different_case(self):
        """Test KeyError is raised for undefined parameter regardless of casing."""
        params = ParameterSet()
        with pytest.raises(KeyError):
            params.get("undefined")
        with pytest.raises(KeyError):
            params.get("UNDEFINED")
        with pytest.raises(KeyError):
            params.get("UnDeFiNeD")

    def test_get_global_params_preserves_original_casing(self):
        """Test get_global_params() returns parameters with original casing."""
        params = ParameterSet()
        params.add("Density", 7850.0)
        params.add("THICKNESS", 0.1)
        params.add("youngs_modulus", 210000.0)

        global_params = params.get_global_params()

        # Original casing should be preserved in the dict
        assert "Density" in global_params
        assert "THICKNESS" in global_params
        assert "youngs_modulus" in global_params

        # Verify values are correct
        assert global_params["Density"] == 7850.0
        assert global_params["THICKNESS"] == 0.1
        assert global_params["youngs_modulus"] == 210000.0

    def test_duplicate_parameter_different_case_overwrites(self):
        """Test that adding parameter with different case overwrites the original."""
        params = ParameterSet()
        params.add("Param", 100)
        params.add("param", 200)  # Different case, should overwrite

        # Both lookups should return the new value and get the new casing
        assert params.get("param") == 200
        assert params.get("PARAM") == 200
        assert params.get("Param") == 200

        # Original casing should be replaced in the dict
        global_params = params.get_global_params()
        assert "param" in global_params
        assert "Param" not in global_params

    def test_mixed_global_and_local_case_insensitive(self):
        """Test case insensitivity with both global and local parameters."""
        params = ParameterSet()
        params.add("Global", 111)
        params.add_local("Local", 222)

        # Both should be accessible case-insensitively
        assert params.get("global") == 111
        assert params.get("GLOBAL") == 111
        assert params.get("local") == 222
        assert params.get("LOCAL") == 222


class TestParameterRefStrings:
    """Test that parameter reference strings preserve original casing."""

    def test_ref_string_preserves_casing(self):
        """Test that recorded ref strings preserve their original casing."""
        params = ParameterSet()
        with params.scope("keyword1"):
            with params.scope("card0"):
                params.record_ref("field1", "&Density")

        # The ref string should be preserved exactly
        ref = params.get_ref("keyword1", "card0", "field1")
        assert ref == "&Density"

    def test_ref_string_negative_casing(self):
        """Test that negative ref strings preserve casing."""
        params = ParameterSet()
        with params.scope("keyword1"):
            params.record_ref("field1", "-&Density")

        ref = params.get_ref("keyword1", "field1")
        assert ref == "-&Density"
