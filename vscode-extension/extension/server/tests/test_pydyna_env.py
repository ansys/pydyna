"""Tests for the minimal bundled-Python environment used by the LSP server.

These tests verify that:
1. The ``ansys.dyna.core`` keyword classes can be imported.
2. ``Deck.loads()`` correctly parses keyword blocks.
3. ``FieldSchema`` attributes (``name``, ``offset``, ``width``, ``type``) are
   accessible on parsed cards.
4. Property docstrings are accessible for hover text extraction.

Run with the bundled Python::

    python/python/python.exe -m pytest server/tests/test_pydyna_env.py -v

Or with the normal dev environment::

    pytest vscode-extension/extension/server/tests/test_pydyna_env.py -v
"""

import pytest


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

DEFINE_CURVE_BLOCK = """\
*KEYWORD
*DEFINE_CURVE
$#    lcid      sidr       sfa       sfo      offa      offo    dattyp     lcint
         1         0       1.0       1.0       0.0       0.0         0         0
$#                a1                  o1
                 0.0                 0.0
                 1.0                 1.0
*END
"""

MAT_ELASTIC_BLOCK = """\
*KEYWORD
*MAT_ELASTIC
$#     mid        ro         e        pr        da        db  not used
         7  7.85E-04  2.10E+05      0.30       0.0       0.0       0.0
*END
"""

MULTI_KEYWORD_BLOCK = """\
*KEYWORD
*DEFINE_CURVE
$#    lcid      sidr       sfa       sfo      offa      offo    dattyp     lcint
         1         0       1.0       1.0       0.0       0.0         0         0
$#                a1                  o1
                 0.0                 0.0
                 1.0                 1.0
*MAT_NULL
$#     mid        ro        pc        mu    terod    cerod        ym        pr
         3     1.3e-3       0.0       0.0       0.0       0.0       0.0       0.0
*END
"""


# ---------------------------------------------------------------------------
# Import tests
# ---------------------------------------------------------------------------


class TestImports:
    """Verify that all required packages can be imported in this environment."""

    def test_ansys_dyna_core_imports(self):
        """Top-level ansys.dyna.core package must import without error."""
        import ansys.dyna.core  # noqa: F401

    def test_deck_imports(self):
        """Deck class must be importable from the lib module."""
        from ansys.dyna.core.lib.deck import Deck  # noqa: F401

    def test_keyword_base_imports(self):
        """KeywordBase must be importable."""
        from ansys.dyna.core.lib.keyword_base import KeywordBase  # noqa: F401

    def test_field_schema_imports(self):
        """FieldSchema / CardSchema must be importable."""
        from ansys.dyna.core.lib.field_schema import CardSchema, FieldSchema  # noqa: F401

    def test_define_curve_class_imports(self):
        """Auto-generated DefineCurve keyword class must be importable."""
        from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve  # noqa: F401

    def test_mat_elastic_class_imports(self):
        """Auto-generated MatElastic keyword class must be importable."""
        from ansys.dyna.core.keywords.keyword_classes.auto.mat.mat_elastic import MatElastic  # noqa: F401

    def test_transitive_deps_available(self):
        """All cherry-picked transitive dependencies must be importable."""
        import appdirs  # noqa: F401
        import charset_normalizer  # noqa: F401
        import hollerith  # noqa: F401
        import numpy  # noqa: F401
        import pandas  # noqa: F401
        import transformations  # noqa: F401


# ---------------------------------------------------------------------------
# Deck.loads() parsing tests
# ---------------------------------------------------------------------------


class TestDeckLoads:
    """Verify that Deck.loads() correctly parses keyword blocks."""

    def test_loads_returns_keyword_instance(self):
        """Parsing a DEFINE_CURVE block must yield a DefineCurve instance."""
        from ansys.dyna.core.lib.deck import Deck
        from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

        deck = Deck()
        deck.loads(DEFINE_CURVE_BLOCK)
        keywords = list(deck)
        assert len(keywords) == 1
        assert isinstance(keywords[0], DefineCurve)

    def test_loads_parses_field_values(self):
        """Field values must be correctly read from the keyword block."""
        from ansys.dyna.core.lib.deck import Deck

        deck = Deck()
        deck.loads(DEFINE_CURVE_BLOCK)
        kw = list(deck)[0]
        assert kw.lcid == 1
        assert kw.sfa == pytest.approx(1.0)
        assert kw.sfo == pytest.approx(1.0)

    def test_loads_mat_elastic(self):
        """MAT_ELASTIC block must parse to a MatElastic instance with correct values."""
        from ansys.dyna.core.lib.deck import Deck
        from ansys.dyna.core.keywords.keyword_classes.auto.mat.mat_elastic import MatElastic

        deck = Deck()
        deck.loads(MAT_ELASTIC_BLOCK)
        keywords = list(deck)
        assert len(keywords) == 1
        assert isinstance(keywords[0], MatElastic)
        kw = keywords[0]
        assert kw.mid == 7
        assert kw.e == pytest.approx(2.1e5)
        assert kw.pr == pytest.approx(0.30)

    def test_loads_multiple_keywords(self):
        """Multiple keyword blocks in one string must each be parsed."""
        from ansys.dyna.core.lib.deck import Deck

        deck = Deck()
        deck.loads(MULTI_KEYWORD_BLOCK)
        keywords = list(deck)
        assert len(keywords) == 2
        names = [type(k).__name__ for k in keywords]
        assert "DefineCurve" in names


# ---------------------------------------------------------------------------
# FieldSchema metadata tests
# ---------------------------------------------------------------------------


class TestFieldSchema:
    """Verify that FieldSchema attributes are accessible on parsed keywords."""

    def _get_define_curve(self):
        from ansys.dyna.core.lib.deck import Deck

        deck = Deck()
        deck.loads(DEFINE_CURVE_BLOCK)
        return list(deck)[0]

    def test_card_has_schema(self):
        """The first card of a parsed keyword must have a _schema attribute."""
        kw = self._get_define_curve()
        assert hasattr(kw._cards[0], "_schema")

    def test_schema_has_fields(self):
        """CardSchema must expose a non-empty list of FieldSchema objects."""
        kw = self._get_define_curve()
        fields = kw._cards[0]._schema.fields
        assert len(fields) > 0

    def test_field_schema_name(self):
        """Every FieldSchema must have a non-empty string name."""
        kw = self._get_define_curve()
        for field in kw._cards[0]._schema.fields:
            assert isinstance(field.name, str) and field.name

    def test_field_schema_offset_and_width(self):
        """Every FieldSchema must have non-negative integer offset and width."""
        kw = self._get_define_curve()
        for field in kw._cards[0]._schema.fields:
            assert isinstance(field.offset, int)
            assert isinstance(field.width, int)
            assert field.offset >= 0
            assert field.width > 0

    def test_field_schema_lcid_offset(self):
        """LCID must be the first field (offset=0, width=10) in DEFINE_CURVE."""
        kw = self._get_define_curve()
        lcid_field = kw._cards[0]._schema.fields[0]
        assert lcid_field.name == "lcid"
        assert lcid_field.offset == 0
        assert lcid_field.width == 10


# ---------------------------------------------------------------------------
# Property docstring extraction tests
# ---------------------------------------------------------------------------


class TestPropertyDocstrings:
    """Verify that keyword property docstrings are accessible for hover text."""

    def test_define_curve_lcid_docstring(self):
        """DefineCurve.lcid property must have a non-empty docstring."""
        from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

        doc = type(DefineCurve()).lcid.fget.__doc__
        assert doc is not None
        assert len(doc.strip()) > 0

    def test_mat_elastic_mid_docstring(self):
        """MatElastic.mid property must have a non-empty docstring."""
        from ansys.dyna.core.keywords.keyword_classes.auto.mat.mat_elastic import MatElastic

        doc = type(MatElastic()).mid.fget.__doc__
        assert doc is not None
        assert len(doc.strip()) > 0

    def test_mat_elastic_e_docstring_mentions_modulus(self):
        """MatElastic.e docstring should describe Young's modulus."""
        from ansys.dyna.core.keywords.keyword_classes.auto.mat.mat_elastic import MatElastic

        doc = (type(MatElastic()).e.fget.__doc__ or "").lower()
        # The docstring should mention modulus or elastic in some form
        assert any(word in doc for word in ("modulus", "elastic", "young", " e,"))

    def test_docstring_accessible_via_parsed_deck(self):
        """Property docstrings must be accessible from a Deck-parsed instance."""
        from ansys.dyna.core.lib.deck import Deck

        deck = Deck()
        deck.loads(DEFINE_CURVE_BLOCK)
        kw = list(deck)[0]
        doc = type(kw).lcid.fget.__doc__
        assert doc is not None
        assert len(doc.strip()) > 0


# ---------------------------------------------------------------------------
# Standalone smoke test (run directly with: python test_pydyna_env.py)
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    import sys

    result = pytest.main([__file__, "-v"])
    sys.exit(result)
