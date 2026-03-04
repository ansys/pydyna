"""Tests for documents.py — document text management and keyword block extraction.

Run with the bundled Python (via ``npm run test-bundled``) or the dev environment:

    pytest vscode-extension/extension/server/tests/test_documents.py -v
"""

import sys
import os

# Allow importing documents.py from the server directory without installing it.
sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

import pytest
from documents import DocumentStore, KeywordBlock, extract_keyword_block


# ---------------------------------------------------------------------------
# Fixtures / shared data
# ---------------------------------------------------------------------------

SIMPLE = """\
*KEYWORD
*DEFINE_CURVE
$#    lcid      sidr       sfa       sfo      offa      offo    dattyp     lcint
         1         0       1.0       1.0       0.0       0.0         0         0
$#                a1                  o1
                 0.0                 0.0
                 1.0                 1.0
*MAT_ELASTIC
$#     mid        ro         e        pr        da        db  not used
         7  7.85E-04  2.10E+05      0.30       0.0       0.0       0.0
*PART
$# title
My part title
$#   pid    secid      mid
       1        1        7
*END"""

# Line index map for SIMPLE (0-based):
#  0  *KEYWORD
#  1  *DEFINE_CURVE
#  2  $#    lcid …
#  3           1 …
#  4  $#  a1 …
#  5           0.0 0.0
#  6           1.0 1.0
#  7  *MAT_ELASTIC
#  8  $#   mid …
#  9           7 …
# 10  *PART
# 11  $# title
# 12  My part title
# 13  $#   pid …
# 14         1 …
# 15  *END


# ---------------------------------------------------------------------------
# extract_keyword_block — happy path
# ---------------------------------------------------------------------------


class TestExtractKeywordBlockBasic:
    def test_returns_keyword_block(self):
        block = extract_keyword_block(SIMPLE, 3)
        assert isinstance(block, KeywordBlock)

    def test_define_curve_start_line(self):
        """Hovering on data line inside DEFINE_CURVE → block starts at line 1."""
        block = extract_keyword_block(SIMPLE, 3)
        assert block.start_line == 1

    def test_define_curve_text_starts_with_keyword(self):
        block = extract_keyword_block(SIMPLE, 3)
        assert block.text.startswith("*DEFINE_CURVE")

    def test_define_curve_text_does_not_include_next_keyword(self):
        block = extract_keyword_block(SIMPLE, 3)
        assert "*MAT_ELASTIC" not in block.text

    def test_define_curve_end_line(self):
        """DEFINE_CURVE block ends at line 6 (last data line before *MAT_ELASTIC)."""
        block = extract_keyword_block(SIMPLE, 3)
        assert block.end_line == 6

    def test_hover_on_keyword_line_itself(self):
        """Hovering directly on *DEFINE_CURVE → that block."""
        block = extract_keyword_block(SIMPLE, 1)
        assert block.start_line == 1
        assert block.text.startswith("*DEFINE_CURVE")

    def test_hover_on_comment_line_inside_block(self):
        """Hovering on a $# comment line returns the enclosing block."""
        block = extract_keyword_block(SIMPLE, 2)
        assert block.start_line == 1

    def test_mat_elastic_block(self):
        block = extract_keyword_block(SIMPLE, 9)
        assert block.start_line == 7
        assert block.text.startswith("*MAT_ELASTIC")
        assert "*PART" not in block.text

    def test_part_block_start(self):
        block = extract_keyword_block(SIMPLE, 12)
        assert block.start_line == 10
        assert block.text.startswith("*PART")

    def test_part_block_excludes_end(self):
        """*END must not appear inside any block's text."""
        block = extract_keyword_block(SIMPLE, 14)
        assert "*END" not in block.text

    def test_last_data_line_of_part(self):
        """Line 14 (last data line of *PART, just before *END) → PART block."""
        block = extract_keyword_block(SIMPLE, 14)
        assert block.start_line == 10


# ---------------------------------------------------------------------------
# extract_keyword_block — block text content
# ---------------------------------------------------------------------------


class TestExtractKeywordBlockContent:
    def test_define_curve_full_text(self):
        block = extract_keyword_block(SIMPLE, 5)
        expected_lines = SIMPLE.splitlines()[1:7]  # lines 1–6
        assert block.text == "\n".join(expected_lines)

    def test_mat_elastic_full_text(self):
        block = extract_keyword_block(SIMPLE, 9)
        expected_lines = SIMPLE.splitlines()[7:10]  # lines 7–9
        assert block.text == "\n".join(expected_lines)

    def test_block_wrappable_by_deck(self):
        """The extracted block must be parseable by Deck.loads() when wrapped."""
        from ansys.dyna.core.lib.deck import Deck

        block = extract_keyword_block(SIMPLE, 3)
        wrapped = f"*KEYWORD\n{block.text}\n*END\n"
        deck = Deck()
        deck.loads(wrapped)
        kwds = list(deck)
        assert len(kwds) == 1
        assert type(kwds[0]).__name__ == "DefineCurve"

    def test_mat_elastic_block_parseable(self):
        from ansys.dyna.core.lib.deck import Deck

        block = extract_keyword_block(SIMPLE, 9)
        wrapped = f"*KEYWORD\n{block.text}\n*END\n"
        deck = Deck()
        deck.loads(wrapped)
        kwds = list(deck)
        assert len(kwds) == 1
        assert type(kwds[0]).__name__ == "MatElastic"


# ---------------------------------------------------------------------------
# extract_keyword_block — edge cases that return None
# ---------------------------------------------------------------------------


class TestExtractKeywordBlockNone:
    def test_header_line_returns_none(self):
        """Line 0 is *KEYWORD — not a data keyword block."""
        assert extract_keyword_block(SIMPLE, 0) is None

    def test_end_line_returns_none(self):
        """*END is a terminator, not a data block."""
        assert extract_keyword_block(SIMPLE, 15) is None

    def test_negative_line_returns_none(self):
        assert extract_keyword_block(SIMPLE, -1) is None

    def test_out_of_range_line_returns_none(self):
        assert extract_keyword_block(SIMPLE, 999) is None

    def test_empty_text_returns_none(self):
        assert extract_keyword_block("", 0) is None

    def test_no_keywords_returns_none(self):
        text = "just a plain text file\nwith no keywords"
        assert extract_keyword_block(text, 0) is None

    def test_only_keyword_header_returns_none(self):
        text = "*KEYWORD\n*END\n"
        assert extract_keyword_block(text, 0) is None


# ---------------------------------------------------------------------------
# extract_keyword_block — single-keyword file
# ---------------------------------------------------------------------------


class TestExtractKeywordBlockSingleKeyword:
    SINGLE = "*KEYWORD\n*DEFINE_CURVE\n$# lcid\n     1\n*END\n"

    # line 0: *KEYWORD
    # line 1: *DEFINE_CURVE
    # line 2: $# lcid
    # line 3:      1
    # line 4: *END

    def test_data_line(self):
        block = extract_keyword_block(self.SINGLE, 3)
        assert block is not None
        assert block.start_line == 1

    def test_keyword_line(self):
        block = extract_keyword_block(self.SINGLE, 1)
        assert block is not None
        assert block.start_line == 1
        assert block.end_line == 3

    def test_end_line_returns_none(self):
        assert extract_keyword_block(self.SINGLE, 4) is None


# ---------------------------------------------------------------------------
# extract_keyword_block — file without *KEYWORD header
# ---------------------------------------------------------------------------


class TestExtractKeywordBlockNoHeader:
    """Some LS-DYNA files omit *KEYWORD and start directly with a keyword."""

    NO_HDR = "*DEFINE_CURVE\n$# lcid\n     1\n*MAT_NULL\n$# mid\n   3\n*END\n"

    def test_first_keyword_line_0(self):
        block = extract_keyword_block(self.NO_HDR, 2)
        assert block is not None
        assert block.start_line == 0

    def test_second_keyword(self):
        block = extract_keyword_block(self.NO_HDR, 5)
        assert block is not None
        assert block.start_line == 3

    def test_end_returns_none(self):
        assert extract_keyword_block(self.NO_HDR, 6) is None


# ---------------------------------------------------------------------------
# extract_keyword_block — CRLF line endings
# ---------------------------------------------------------------------------


class TestExtractKeywordBlockCRLF:
    def test_crlf_endings(self):
        text = SIMPLE.replace("\n", "\r\n")
        block = extract_keyword_block(text, 3)
        assert block is not None
        assert block.start_line == 1
        assert block.text.startswith("*DEFINE_CURVE")


# ---------------------------------------------------------------------------
# DocumentStore — lifecycle
# ---------------------------------------------------------------------------


class TestDocumentStore:
    URI = "file:///work/model.k"
    TEXT = SIMPLE

    def _fresh(self):
        return DocumentStore()

    def test_empty_store(self):
        store = self._fresh()
        assert len(store) == 0
        assert self.URI not in store

    def test_open_stores_text(self):
        store = self._fresh()
        store.open(self.URI, self.TEXT)
        assert self.URI in store
        assert store.get_text(self.URI) == self.TEXT

    def test_change_updates_text(self):
        store = self._fresh()
        store.open(self.URI, self.TEXT)
        new_text = self.TEXT + "\n$# changed"
        store.change(self.URI, new_text)
        assert store.get_text(self.URI) == new_text

    def test_close_removes_text(self):
        store = self._fresh()
        store.open(self.URI, self.TEXT)
        store.close(self.URI)
        assert self.URI not in store
        assert store.get_text(self.URI) is None

    def test_close_nonexistent_is_safe(self):
        store = self._fresh()
        store.close("file:///nonexistent.k")  # must not raise

    def test_get_text_unknown_uri_returns_none(self):
        store = self._fresh()
        assert store.get_text("file:///unknown.k") is None

    def test_multiple_documents(self):
        store = self._fresh()
        store.open("file:///a.k", "text a")
        store.open("file:///b.k", "text b")
        assert len(store) == 2
        assert store.get_text("file:///a.k") == "text a"
        assert store.get_text("file:///b.k") == "text b"


# ---------------------------------------------------------------------------
# DocumentStore — get_keyword_block integration
# ---------------------------------------------------------------------------


class TestDocumentStoreGetKeywordBlock:
    URI = "file:///work/model.k"

    def test_returns_block(self):
        store = DocumentStore()
        store.open(self.URI, SIMPLE)
        block = store.get_keyword_block(self.URI, 3)
        assert block is not None
        assert block.start_line == 1

    def test_unknown_uri_returns_none(self):
        store = DocumentStore()
        assert store.get_keyword_block("file:///nope.k", 3) is None

    def test_header_line_returns_none(self):
        store = DocumentStore()
        store.open(self.URI, SIMPLE)
        assert store.get_keyword_block(self.URI, 0) is None

    def test_after_close_returns_none(self):
        store = DocumentStore()
        store.open(self.URI, SIMPLE)
        store.close(self.URI)
        assert store.get_keyword_block(self.URI, 3) is None
