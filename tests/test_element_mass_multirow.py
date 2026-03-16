# Copyright (C) 2021 - 2024 ANSYS, Inc. and/or its affiliates.
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

"""Tests for multi-row support in ELEMENT_MASS_PART and ELEMENT_MASS_PART_SET.

Regression tests for GitHub issue #1120: multiple entries listed under a single
keyword were silently dropped and an "out of bound card characters" warning was
raised because the keywords used a single Card instead of a TableCard.
"""

import warnings

import pandas as pd
import pytest

from ansys.dyna.core import Deck


# pid(8) + addmass(16) + finmass(16) + lcid(8) = 48 chars
_ELEMENT_MASS_PART_TWO_ROWS = """\
*KEYWORD
*ELEMENT_MASS_PART
     101         500.000           0.000       1
     102         200.000           0.000       2
*END"""

# psid(8) + addmass(16) + finmass(16) = 40 chars
_ELEMENT_MASS_PART_SET_TWO_ROWS = """\
*KEYWORD
*ELEMENT_MASS_PART_SET
   10501       69.248175           0.000
   10502       46.582768           0.000
*END"""

# Deck from the original bug report (issue #1120)
_ELEMENT_MASS_PART_SET_WITH_LCID = """\
*KEYWORD
*ELEMENT_MASS_PART_SET
   10501          69.248175                                       1
   10502          46.582768                                       1
*END"""


def _get_element_keyword(deck: Deck):
    """Return the first ELEMENT keyword found in the deck."""
    return next(kw for kw in deck.keywords if kw.keyword == "ELEMENT")


class TestElementMassPartMultiRow:
    """ELEMENT_MASS_PART reads all rows into a DataFrame without warnings."""

    def test_all_rows_are_loaded(self):
        deck = Deck()
        deck.loads(_ELEMENT_MASS_PART_TWO_ROWS)
        kw = _get_element_keyword(deck)
        assert len(kw.elements) == 2

    def test_row_values_are_correct(self):
        deck = Deck()
        deck.loads(_ELEMENT_MASS_PART_TWO_ROWS)
        kw = _get_element_keyword(deck)
        assert kw.elements["pid"].tolist() == [101, 102]
        assert kw.elements["addmass"].tolist() == pytest.approx([500.0, 200.0])
        assert kw.elements["lcid"].tolist() == [1, 2]

    def test_no_out_of_bound_warning(self):
        deck = Deck()
        with warnings.catch_warnings(record=True) as caught:
            warnings.simplefilter("always")
            deck.loads(_ELEMENT_MASS_PART_TWO_ROWS)
        oob = [w for w in caught if "out of bound" in str(w.message).lower()]
        assert oob == [], f"Unexpected 'out of bound' warning(s): {oob}"

    def test_parts_property_returns_dataframe(self):
        deck = Deck()
        deck.loads(_ELEMENT_MASS_PART_TWO_ROWS)
        kw = _get_element_keyword(deck)
        assert isinstance(kw.elements, pd.DataFrame)

    def test_roundtrip_write_contains_all_rows(self):
        deck = Deck()
        deck.loads(_ELEMENT_MASS_PART_TWO_ROWS)
        output = deck.write()
        assert output.count("101") == 1
        assert output.count("102") == 1


class TestElementMassPartSetMultiRow:
    """ELEMENT_MASS_PART_SET reads all rows into a DataFrame without warnings."""

    def test_all_rows_are_loaded(self):
        deck = Deck()
        deck.loads(_ELEMENT_MASS_PART_SET_TWO_ROWS)
        kw = _get_element_keyword(deck)
        assert len(kw.elements) == 2

    def test_row_values_are_correct(self):
        deck = Deck()
        deck.loads(_ELEMENT_MASS_PART_SET_TWO_ROWS)
        kw = _get_element_keyword(deck)
        assert kw.elements["psid"].tolist() == [10501, 10502]
        assert kw.elements["addmass"].tolist() == pytest.approx([69.248175, 46.582768])

    def test_no_out_of_bound_warning(self):
        """Regression: second row used to trigger 'out of bound card characters'."""
        deck = Deck()
        with warnings.catch_warnings(record=True) as caught:
            warnings.simplefilter("always")
            deck.loads(_ELEMENT_MASS_PART_SET_WITH_LCID)
        oob = [w for w in caught if "out of bound" in str(w.message).lower()]
        assert oob == [], f"Unexpected 'out of bound' warning(s): {oob}"

    def test_parts_property_returns_dataframe(self):
        deck = Deck()
        deck.loads(_ELEMENT_MASS_PART_SET_TWO_ROWS)
        kw = _get_element_keyword(deck)
        assert isinstance(kw.elements, pd.DataFrame)

    def test_roundtrip_write_contains_all_rows(self):
        deck = Deck()
        deck.loads(_ELEMENT_MASS_PART_SET_TWO_ROWS)
        output = deck.write()
        assert output.count("10501") == 1
        assert output.count("10502") == 1
