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

import pytest

from ansys.dyna.core import Deck
from ansys.dyna.core import keywords as kwd


# -- import tests -----------------------------------------------------------

def test_comment_import_multiline_all_lines_present():
    """Importing a deck with a 3-line *COMMENT must preserve all three lines.

    Bug: only Line1 was stored; Line2 and Line3 were silently dropped.
    """
    deck_string = "*KEYWORD\n*COMMENT\nLine1\nLine2\nLine3\n*END"
    deck = Deck()
    deck.loads(deck_string)

    comments = [k for k in deck.keywords if isinstance(k, kwd.Comment)]
    assert len(comments) == 1, "Expected exactly one *COMMENT keyword"

    text = comments[0].comment
    assert text == "Line1\nLine2\nLine3", (
        f"Expected all three lines, got: {text!r}"
    )


def test_comment_import_multiline_round_trip(ref_string):
    """A round-trip (load -> write) must reproduce all comment lines."""
    deck_string = "*KEYWORD\n*COMMENT\nLine1\nLine2\nLine3\n*END"
    deck = Deck()
    deck.loads(deck_string)

    output = deck.write()
    assert output.strip() == ref_string.test_comment_deck_roundtrip.strip(), (
        f"Round-trip deck output does not match reference.\nGot:\n{output}"
    )


# -- write / property tests -------------------------------------------------

def test_comment_write_multiline_not_truncated(ref_string):
    """Setting a multiline comment must not truncate at 170 characters.

    Bug: the 20-line string "\\n".join(["0123456789"]*20) was cropped to ~170 chars
    (8 full lines + a partial "012" line).
    """
    multiline = "\n".join(["0123456789"] * 20)
    k = kwd.Comment(comment=multiline)
    output = k.write()

    assert output.strip() == ref_string.test_comment_multiline_20_write.strip(), (
        f"Write output does not match reference.\nGot:\n{output}"
    )


def test_comment_write_three_lines(ref_string):
    """Write output for a 3-line comment must match the reference string."""
    k = kwd.Comment(comment="Line1\nLine2\nLine3")
    output = k.write()

    assert output.strip() == ref_string.test_comment_multiline_write.strip(), (
        f"Write output does not match reference.\nGot:\n{output}"
    )


def test_comment_property_round_trip():
    """The comment property getter must return exactly what was set."""
    text = "First line\nSecond line\nThird line"
    k = kwd.Comment(comment=text)
    assert k.comment == text, "comment property did not round-trip correctly"


def test_comment_single_line_unchanged():
    """A single-line comment must still work as before."""
    k = kwd.Comment(comment="This is a single line comment")
    output = k.write()
    assert "This is a single line comment" in output
