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

"""Test parameter loading behavior and error location.

Undefined parameter references are now handled gracefully: loading succeeds with
type-appropriate defaults (None/NaN), and refs are recorded for write-back with
retain_parameters=True. No warning is emitted for undefined params.

For other error types (e.g. type mismatch in PARAMETER_EXPRESSION), the deck loader
tracks the 1-based line number and file path on ImportContext for warning messages.

Each test validates both load and write: decks are written to string with
retain_parameters=True. Output is checked for expected structure and preserved
parameter refs (e.g. &undef, &x_coord), including in SeriesCard (SET_NODE_LIST)
where undefined params resolve to None.
"""

import pytest

from ansys.dyna.core import Deck


class TestParameterErrorLocation:
    """Test parameter loading and error location for non-undefined-param errors."""

    def test_undefined_param_loads_successfully(self, file_utils):
        """Test that undefined parameter references load successfully with defaults."""
        deck_file = file_utils.get_asset_file_path("parameter_error_location/test_undefined_param.k")
        deck = Deck()
        result = deck.import_file(deck_file)
        # Should load without raising; undefined params get type-appropriate defaults
        assert len(deck.keywords) > 0
        assert len(result.warnings) == 0
        output = deck.write(retain_parameters=True)
        assert "*SET_NODE_LIST" in output
        assert "&undef" in output

    def test_undefined_param_multiple_keywords_loads_successfully(self, file_utils):
        """Test that multiple keywords with undefined params all load successfully."""
        deck_file = file_utils.get_asset_file_path("parameter_error_location/test_multiple.k")
        deck = Deck()
        result = deck.import_file(deck_file)
        assert len(deck.keywords) > 0
        output = deck.write(retain_parameters=True)
        assert "*SET_NODE_LIST" in output
        assert "*PARAMETER" in output
        assert "&undef" in output

    def test_undefined_param_loads_from_string_succeeds(self, file_utils):
        """Test that loading from string with undefined param succeeds."""
        deck_content = (file_utils.assets_folder / "parameter_error_location" / "test_undefined_param.k").read_text()
        deck = Deck()
        result = deck.loads(deck_content)
        assert len(deck.keywords) > 0
        output = deck.write(retain_parameters=True)
        assert "*SET_NODE_LIST" in output
        assert "&undef" in output

    def test_error_warning_backwards_compatible_no_context(self):
        """Test that on_error does not raise when called without a context."""
        from ansys.dyna.core.lib.parameters import ParameterHandler

        handler = ParameterHandler()
        # No context: should emit a plain warning without location suffix.
        with pytest.warns(UserWarning, match=r"Error processing parameter"):
            handler.on_error(KeyError("test_param"))

    def test_param_format_error_warning_includes_file_and_line(self, file_utils):
        """Test that parameter format errors include file path and line number.

        In test_param_format_error.k, the *NODE header is on line 4.
        """
        deck_file = file_utils.get_asset_file_path("parameter_error_location/test_param_format_error.k")
        deck = Deck()
        with pytest.warns(UserWarning, match=r"Error processing parameter"):
            result = deck.import_file(deck_file)
        assert len(result.warnings) >= 1
        warning = result.warnings[0]
        assert "Error processing parameter" in warning
        assert "test_param_format_error.k" in warning
        assert "line 4" in warning

    def test_param_format_error_loads_no_file_path(self, file_utils):
        """Test that loads() warnings include line number but not file path.

        When loading from a string, ImportContext has path=None, so the file
        path is omitted from warnings, but the line number is still included.
        """
        deck_content = (file_utils.assets_folder / "parameter_error_location" / "test_param_format_error.k").read_text()
        deck = Deck()
        with pytest.warns(UserWarning, match=r"Error processing parameter"):
            result = deck.loads(deck_content)
        assert len(result.warnings) >= 1
        warning = result.warnings[0]
        assert "Error processing parameter" in warning
        assert "in file '" not in warning
        assert "line 4" in warning

    def test_param_format_error_line_number_with_comments(self):
        """Test that comment lines are counted correctly so line numbers stay accurate.

        The *NODE header is on line 6 (after comment lines). Comments should be
        counted in line numbers since they're part of the file.
        """
        deck_content = """*KEYWORD
$ This is a comment
*TITLE
Test Deck
$ Another comment line
*NODE
         1  &x_coord       0.0       0.0
*END"""
        deck = Deck()
        with pytest.warns(UserWarning, match=r"Error processing parameter"):
            result = deck.loads(deck_content)
        assert len(result.warnings) >= 1
        warning = result.warnings[0]
        assert "Error processing parameter" in warning
        assert "line 6" in warning

    def test_undefined_param_with_comments_loads_successfully(self, file_utils):
        """Test that loading with undefined param and comment lines succeeds."""
        deck_file = file_utils.get_asset_file_path("parameter_error_location/test_comments.k")
        deck = Deck()
        result = deck.import_file(deck_file)
        assert len(deck.keywords) > 0
        output = deck.write(retain_parameters=True)
        assert "*SET_NODE_LIST" in output
        assert "&undef" in output

    def test_undefined_param_node_keyword_import_succeeds(self, file_utils):
        """Test that NODE keyword with undefined param can be imported and written."""
        deck_file = file_utils.get_asset_file_path("parameter_error_location/test_node_param.k")
        deck = Deck()
        result = deck.import_file(deck_file)
        assert len(deck.all_keywords) > 0
        output = deck.write(retain_parameters=True)
        assert "&x_coord" in output

    def test_write_retain_parameters_false_raises_on_undefined_param(self, file_utils):
        """Test that write(retain_parameters=False) raises when undefined params exist.

        The error should include the parameter name, file path, line number,
        and a hint to use retain_parameters=True.
        """
        deck_file = file_utils.get_asset_file_path("parameter_error_location/test_undefined_param.k")
        deck = Deck()
        deck.import_file(deck_file)
        with pytest.raises(ValueError, match=r"Cannot write undefined parameter"):
            deck.write(retain_parameters=False)
        with pytest.raises(ValueError, match=r"&undef"):
            deck.write(retain_parameters=False)
        with pytest.raises(ValueError, match=r"test_undefined_param\.k"):
            deck.write(retain_parameters=False)
        with pytest.raises(ValueError, match=r"line \d+"):
            deck.write(retain_parameters=False)
        with pytest.raises(ValueError, match=r"retain_parameters=True"):
            deck.write(retain_parameters=False)

    def test_write_retain_parameters_false_raises_with_line_number_no_file(self):
        """Test that write error includes line number even when loaded from string.

        When loading from a string, there's no file path, but the line number
        should still be included in the error message.
        """
        deck_content = """*KEYWORD
*TITLE
Test
*SET_NODE_LIST
         1       0.0       0.0       0.0       0.0    MECH         1
  &undef
*END"""
        deck = Deck()
        deck.loads(deck_content)
        with pytest.raises(ValueError, match=r"Cannot write undefined parameter"):
            deck.write(retain_parameters=False)
        with pytest.raises(ValueError, match=r"&undef"):
            deck.write(retain_parameters=False)
        with pytest.raises(ValueError, match=r"line \d+"):
            deck.write(retain_parameters=False)
        # Should NOT have file path when loaded from string
        try:
            deck.write(retain_parameters=False)
        except ValueError as e:
            assert "in '" not in str(e) or "in 'None'" not in str(e)

    def test_write_retain_parameters_false_succeeds_when_all_params_defined(self, file_utils):
        """Test that write(retain_parameters=False) succeeds when all params are defined."""
        deck_file = file_utils.get_asset_file_path("parameter_error_location/test_undefined_param.k")
        deck = Deck()
        deck.import_file(deck_file)
        deck.parameters.add("undef", 1)  # Define the param so write can substitute
        output = deck.write(retain_parameters=False)
        assert "*SET_NODE_LIST" in output
        assert "1" in output  # substituted value, not &undef

    def test_expand_warns_on_unresolved_parameters(self, tmp_path):
        """Test that expand() warns about unresolved parameters with location info."""
        include_content = """*SET_NODE_LIST
         1       0.0       0.0       0.0       0.0    MECH         1
  &undef"""
        include_path = tmp_path / "include_undef.k"
        include_path.write_text(include_content)

        deck_string = f"""*INCLUDE
{include_path}"""

        deck = Deck()
        deck.loads(deck_string)

        with pytest.warns(UserWarning, match=r"Unresolved parameter.*&undef"):
            deck.expand(cwd=str(tmp_path))
