"""Tests for hover.py — narrow hover field resolution.

Run with the bundled Python::

    python/python/python.exe -m pytest server/tests/test_hover.py -v

Or with the dev environment::

    pytest vscode-extension/extension/server/tests/test_hover.py -v
"""

from __future__ import annotations

import sys
import os

# Allow importing server modules without installing them.
sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

import pytest
from hover import _parse_block, _resolve_field, _format_markdown, narrow_hover
from documents import DocumentStore


# ---------------------------------------------------------------------------
# Test data
# ---------------------------------------------------------------------------

MAT_ELASTIC_BLOCK = """\
*MAT_ELASTIC
$#     mid        ro         e        pr        da        db  not used
         7  7.85E-04  2.10E+05      0.30       0.0       0.0       0.0"""

DEFINE_CURVE_BLOCK = """\
*DEFINE_CURVE
$#    lcid      sidr       sfa       sfo      offa      offo    dattyp     lcint
         1         0       1.0       1.0       0.0       0.0         0         0"""

FULL_DOC = """\
*KEYWORD
*MAT_ELASTIC
$#     mid        ro         e        pr        da        db  not used
         7  7.85E-04  2.10E+05      0.30       0.0       0.0       0.0
*DEFINE_CURVE
$#    lcid      sidr       sfa       sfo      offa      offo    dattyp     lcint
         1         0       1.0       1.0       0.0       0.0         0         0
*END"""

# Line index map for FULL_DOC (0-based):
#  0  *KEYWORD
#  1  *MAT_ELASTIC
#  2  $#     mid …
#  3           7 …      <- MAT_ELASTIC data; col 0-9="mid", 10-19="ro", 20-29="e", 30-39="pr"
#  4  *DEFINE_CURVE
#  5  $#    lcid …
#  6           1 …      <- DEFINE_CURVE data; col 0-9="lcid", 20-29="sfa"
#  7  *END


# ---------------------------------------------------------------------------
# _parse_block
# ---------------------------------------------------------------------------


class TestParseBlock:
    def test_returns_keyword_instance_for_mat_elastic(self):
        kw = _parse_block(MAT_ELASTIC_BLOCK)
        assert kw is not None
        assert type(kw).__name__ == "MatElastic"

    def test_returns_keyword_instance_for_define_curve(self):
        kw = _parse_block(DEFINE_CURVE_BLOCK)
        assert kw is not None
        assert type(kw).__name__ == "DefineCurve"

    def test_returns_none_for_unknown_keyword(self):
        result = _parse_block("*COMPLETELY_UNKNOWN_KEYWORD_XYZ\n   1   2   3")
        assert result is None

    def test_returns_none_for_empty_string(self):
        result = _parse_block("")
        assert result is None

    def test_returns_none_for_garbage(self):
        result = _parse_block("this is not a keyword")
        assert result is None

    def test_returns_keyword_for_block_with_param_references(self):
        """Keywords with &param references that can't be resolved must still parse."""
        block = """\
*MAT_RIGID
$#     mid        ro         e        pr         n    couple         m     alias
         1  &rho_mat   &E_mat  &nu_mat     0.000     0.000     0.000
$#     cmo      con1      con2
  1.000000         7         7"""
        kw = _parse_block(block)
        assert kw is not None
        assert type(kw).__name__ == "MatRigid"


# ---------------------------------------------------------------------------
# _resolve_field
# ---------------------------------------------------------------------------


class TestResolveField:
    @pytest.fixture
    def mat_elastic(self):
        return _parse_block(MAT_ELASTIC_BLOCK)

    @pytest.fixture
    def define_curve(self):
        return _parse_block(DEFINE_CURVE_BLOCK)

    def test_name_line_returns_none(self, mat_elastic):
        """block_relative_line == 0 (*NAME line) -> None."""
        assert _resolve_field(mat_elastic, 0, 5) is None

    def test_comment_line_returns_none(self, mat_elastic):
        """block_relative_line == 1 ($# line) -> None."""
        assert _resolve_field(mat_elastic, 1, 5) is None

    def test_first_field_mid(self, mat_elastic):
        """col 5 on data line (rel=2) -> 'mid' (offset 0, width 10)."""
        result = _resolve_field(mat_elastic, 2, 5)
        assert result is not None
        assert result[1].name == "mid"

    def test_second_field_ro(self, mat_elastic):
        """col 15 -> 'ro' (offset 10, width 10)."""
        result = _resolve_field(mat_elastic, 2, 15)
        assert result is not None
        assert result[1].name == "ro"

    def test_third_field_e(self, mat_elastic):
        """col 25 -> 'e' (offset 20, width 10)."""
        result = _resolve_field(mat_elastic, 2, 25)
        assert result is not None
        assert result[1].name == "e"

    def test_fourth_field_pr(self, mat_elastic):
        """col 35 -> 'pr' (offset 30, width 10)."""
        result = _resolve_field(mat_elastic, 2, 35)
        assert result is not None
        assert result[1].name == "pr"

    def test_boundary_start_offset_0(self, mat_elastic):
        """col 0 (exact start of 'mid') -> 'mid'."""
        result = _resolve_field(mat_elastic, 2, 0)
        assert result is not None
        assert result[1].name == "mid"

    def test_boundary_end_offset_9(self, mat_elastic):
        """col 9 (last col of 'mid') -> 'mid'."""
        result = _resolve_field(mat_elastic, 2, 9)
        assert result is not None
        assert result[1].name == "mid"

    def test_boundary_next_start_offset_10(self, mat_elastic):
        """col 10 (first col of 'ro') -> 'ro'."""
        result = _resolve_field(mat_elastic, 2, 10)
        assert result is not None
        assert result[1].name == "ro"

    def test_past_all_fields_returns_none(self, mat_elastic):
        """col 85 (beyond all fields) -> None."""
        assert _resolve_field(mat_elastic, 2, 85) is None

    def test_line_far_beyond_cards_returns_none(self, mat_elastic):
        """block_relative_line far beyond any card -> None."""
        assert _resolve_field(mat_elastic, 99, 5) is None

    def test_define_curve_lcid_field(self, define_curve):
        """col 5, rel=2 -> 'lcid'."""
        result = _resolve_field(define_curve, 2, 5)
        assert result is not None
        assert result[1].name == "lcid"

    def test_define_curve_sfa_field(self, define_curve):
        """col 25, rel=2 -> 'sfa' (offset 20, width 10)."""
        result = _resolve_field(define_curve, 2, 25)
        assert result is not None
        assert result[1].name == "sfa"


# ---------------------------------------------------------------------------
# _format_markdown
# ---------------------------------------------------------------------------


class TestFormatMarkdown:
    @pytest.fixture
    def mat_elastic(self):
        return _parse_block(MAT_ELASTIC_BLOCK)

    def _get_fs(self, kw, field_name: str):
        """Return the FieldSchema for the named field on the first active card."""
        for card in kw._get_all_cards():
            if not card.active:
                continue
            schema = getattr(card, "_schema", None)
            if schema:
                for fs in schema.fields:
                    if fs.name == field_name:
                        return fs
        raise KeyError(f"{field_name!r} not found")

    def test_contains_field_name_bold(self, mat_elastic):
        fs = self._get_fs(mat_elastic, "mid")
        md = _format_markdown(mat_elastic, fs)
        assert "**`mid`**" in md

    def test_contains_keyword_class_name(self, mat_elastic):
        fs = self._get_fs(mat_elastic, "mid")
        md = _format_markdown(mat_elastic, fs)
        assert "MatElastic" in md

    def test_contains_type_int(self, mat_elastic):
        fs = self._get_fs(mat_elastic, "mid")
        md = _format_markdown(mat_elastic, fs)
        assert "int" in md

    def test_contains_width(self, mat_elastic):
        fs = self._get_fs(mat_elastic, "mid")
        md = _format_markdown(mat_elastic, fs)
        assert "10" in md

    def test_pr_field_has_docstring(self, mat_elastic):
        """Poisson's ratio field ('pr') should carry a docstring."""
        fs = self._get_fs(mat_elastic, "pr")
        md = _format_markdown(mat_elastic, fs)
        lines = md.splitlines()
        assert len(lines) >= 2


# ---------------------------------------------------------------------------
# narrow_hover integration
# ---------------------------------------------------------------------------


class TestNarrowHover:
    URI = "file:///test/test.k"

    @pytest.fixture
    def store(self):
        s = DocumentStore()
        s.open(self.URI, FULL_DOC)
        return s

    def _params(self, line: int, col: int):
        from lsprotocol import types

        return types.HoverParams(
            text_document=types.TextDocumentIdentifier(uri=self.URI),
            position=types.Position(line=line, character=col),
        )

    def test_hover_returns_hover_object(self, store):
        from lsprotocol import types

        result = narrow_hover(store, self._params(3, 5))
        assert isinstance(result, types.Hover)

    def test_hover_mid_field(self, store):
        result = narrow_hover(store, self._params(3, 5))
        assert result is not None
        assert "mid" in result.contents.value

    def test_hover_ro_field(self, store):
        result = narrow_hover(store, self._params(3, 15))
        assert result is not None
        assert "ro" in result.contents.value

    def test_hover_name_line_none(self, store):
        """*MAT_ELASTIC line -> None."""
        assert narrow_hover(store, self._params(1, 5)) is None

    def test_hover_comment_line_none(self, store):
        """$# header line -> None."""
        assert narrow_hover(store, self._params(2, 5)) is None

    def test_hover_past_fields_none(self, store):
        """col > 80 -> None."""
        assert narrow_hover(store, self._params(3, 85)) is None

    def test_hover_keyword_header_line_none(self, store):
        """*KEYWORD line (line 0) -> None."""
        assert narrow_hover(store, self._params(0, 0)) is None

    def test_hover_end_line_none(self, store):
        """*END line -> None."""
        assert narrow_hover(store, self._params(7, 0)) is None

    def test_hover_define_curve_lcid(self, store):
        """Hover on DEFINE_CURVE data line -> 'lcid'."""
        result = narrow_hover(store, self._params(6, 5))
        assert result is not None
        assert "lcid" in result.contents.value

    def test_hover_content_kind_markdown(self, store):
        from lsprotocol import types

        result = narrow_hover(store, self._params(3, 5))
        assert result.contents.kind == types.MarkupKind.Markdown

    def test_hover_keyword_name_in_content(self, store):
        result = narrow_hover(store, self._params(3, 5))
        assert "MatElastic" in result.contents.value

    def test_hover_range_start(self, store):
        """Range start col = field offset (0 for 'mid')."""
        result = narrow_hover(store, self._params(3, 5))
        assert result.range.start.character == 0

    def test_hover_range_end(self, store):
        """Range end col = field offset + width (10 for 'mid')."""
        result = narrow_hover(store, self._params(3, 5))
        assert result.range.end.character == 10

    def test_hover_range_line(self, store):
        """Range line matches the hover line."""
        result = narrow_hover(store, self._params(3, 5))
        assert result.range.start.line == 3
        assert result.range.end.line == 3
