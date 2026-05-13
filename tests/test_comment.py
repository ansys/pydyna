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
"""Tests for *COMMENT multiline support."""

import pathlib

from ansys.dyna.core import Deck
from ansys.dyna.core import keywords as kwd

_ASSETS = pathlib.Path(__file__).parent / "testfiles" / "keywords"


def test_comment_import_multiline_all_lines_present():
    """Importing a deck with a 3-line *COMMENT preserves all three lines."""
    deck_string = "*KEYWORD\n*COMMENT\nLine1\nLine2\nLine3\n*END"
    deck = Deck()
    deck.loads(deck_string)

    comments = [k for k in deck.keywords if isinstance(k, kwd.Comment)]
    assert len(comments) == 1
    assert comments[0].comment == "Line1\nLine2\nLine3"


def test_comment_import_from_file():
    """Importing a *COMMENT from a .k file preserves all lines."""
    deck = Deck()
    deck.import_file(str(_ASSETS / "test_multiline_comment.k"))

    comments = [kw for kw in deck.keywords if isinstance(kw, kwd.Comment)]
    assert len(comments) == 1
    assert comments[0].comment == "Line1\nLine2\nLine3"


def test_comment_import_multiline_round_trip(ref_string):
    """A round-trip (load -> write) reproduces all comment lines."""
    deck_string = "*KEYWORD\n*COMMENT\nLine1\nLine2\nLine3\n*END"
    deck = Deck()
    deck.loads(deck_string)

    output = deck.write()
    assert output.strip() == ref_string.test_comment_deck_roundtrip.strip()


def test_comment_write_multiline_not_truncated(ref_string):
    """Writing a 20-line comment produces all 20 lines without truncation."""
    multiline = "\n".join(["0123456789"] * 20)
    k = kwd.Comment(comment=multiline)
    output = k.write()

    assert output.strip() == ref_string.test_comment_multiline_20_write.strip()


def test_comment_write_three_lines(ref_string):
    """Write output for a 3-line comment matches the reference string."""
    k = kwd.Comment(comment="Line1\nLine2\nLine3")
    output = k.write()

    assert output.strip() == ref_string.test_comment_multiline_write.strip()


def test_comment_property_round_trip():
    """The comment property getter returns exactly what was set."""
    text = "First line\nSecond line\nThird line"
    k = kwd.Comment(comment=text)
    assert k.comment == text


def test_comment_single_line_unchanged():
    """A single-line comment writes correctly."""
    k = kwd.Comment(comment="This is a single line comment")
    output = k.write()
    assert "This is a single line comment" in output
