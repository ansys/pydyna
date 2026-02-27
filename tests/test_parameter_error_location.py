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

"""Test that parameter error warnings include file path and line number information.

The deck loader tracks the 1-based line number of the start of each keyword block
and stores it on the ImportContext.  When a keyword fails to parse due to an
undefined parameter reference, ParameterHandler.on_error builds a warning message
that includes the file path (when available) and the keyword-block start line.

Line numbers reported are always the line on which the *KEYWORD_NAME header appears,
not the individual field line inside the keyword block.
"""

import pytest

from ansys.dyna.core import Deck


class TestParameterErrorLocation:
    """Test that parameter processing errors include location information."""

    def test_error_warning_includes_file_path(self, file_utils):
        """Test that error warnings include the source file path."""
        deck_file = file_utils.get_asset_file_path("parameter_error_location/test_undefined_param.k")
        deck = Deck()
        with pytest.warns(UserWarning, match=r"Error processing parameter.*in file '.*test_undefined_param\.k'"):
            deck.import_file(deck_file)

    def test_error_warning_includes_line_number(self, file_utils):
        """Test that error warnings include the keyword-block start line number.

        In test_undefined_param.k the *SET_NODE_LIST header is on line 4.
        """
        deck_file = file_utils.get_asset_file_path("parameter_error_location/test_undefined_param.k")
        deck = Deck()
        with pytest.warns(UserWarning, match=r"Error processing parameter.*on line 4"):
            deck.import_file(deck_file)

    def test_error_warning_includes_both_file_and_line(self, file_utils):
        """Test that error warnings include both file path and line number."""
        deck_file = file_utils.get_asset_file_path("parameter_error_location/test_undefined_param.k")
        deck = Deck()
        with pytest.warns(
            UserWarning,
            match=r"Error processing parameter.*in file '.*test_undefined_param\.k'.*on line 4",
        ):
            deck.import_file(deck_file)

    def test_error_warning_with_multiple_keywords_tracks_correct_line(self, file_utils):
        """Test that line numbers are updated correctly as multiple keywords are read.

        In test_multiple.k the first *SET_NODE_LIST (line 6) parses cleanly because
        it has no parameter references.  The second *SET_NODE_LIST header is on line 8
        and references the undefined parameter &undef, so the warning should cite line 8.
        """
        deck_file = file_utils.get_asset_file_path("parameter_error_location/test_multiple.k")
        deck = Deck()
        with pytest.warns(UserWarning, match=r"Error processing parameter.*on line 8"):
            deck.import_file(deck_file)

    def test_error_warning_with_loads_no_file_path(self, file_utils):
        """Test warnings when loading from a string: line number present, no file path."""
        deck_content = (file_utils.assets_folder / "parameter_error_location" / "test_undefined_param.k").read_text()

        deck = Deck()
        # loads() creates an ImportContext with path=None, so file path is omitted.
        with pytest.warns(UserWarning) as warning_list:
            deck.loads(deck_content)

        assert len(warning_list) > 0
        warning_message = str(warning_list[0].message)
        assert "in file '" not in warning_message
        assert "at line 4" in warning_message
        assert "Error processing parameter" in warning_message

    def test_error_warning_backwards_compatible_no_context(self):
        """Test that on_error does not raise when called without a context."""
        from ansys.dyna.core.lib.parameters import ParameterHandler

        handler = ParameterHandler()
        # No context: should emit a plain warning without location suffix.
        with pytest.warns(UserWarning, match=r"Error processing parameter"):
            handler.on_error(KeyError("test_param"))

    def test_error_warning_line_number_with_comments(self, file_utils):
        """Test that comment lines are counted correctly so line numbers stay accurate.

        In test_comments.k there are comment lines ($...) between *KEYWORD and
        *SET_NODE_LIST.  The *SET_NODE_LIST header is on line 6.
        """
        deck_file = file_utils.get_asset_file_path("parameter_error_location/test_comments.k")
        deck = Deck()
        with pytest.warns(UserWarning, match=r"Error processing parameter.*on line 6"):
            deck.import_file(deck_file)

    def test_error_warning_with_node_keyword(self, file_utils):
        """Test that location info is emitted for keywords other than *SET_NODE_LIST.

        In test_node_param.k the *NODE header is on line 4 and references the
        undefined parameter &x_coord.
        """
        deck_file = file_utils.get_asset_file_path("parameter_error_location/test_node_param.k")
        deck = Deck()
        with pytest.warns(
            UserWarning,
            match=r"Error processing parameter.*in file '.*test_node_param\.k'.*on line 4",
        ):
            deck.import_file(deck_file)
