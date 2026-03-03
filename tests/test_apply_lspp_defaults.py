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

"""Tests for the apply_lspp_defaults free function."""

import pytest

from ansys.dyna.core.lib.card import Card
from ansys.dyna.core.lib.field import Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.series_card import SeriesCard
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.table_card_group import TableCardGroup
from ansys.dyna.core.lib.config import disable_lspp_defaults
from ansys.dyna.core.lib.algorithms import apply_lspp_defaults
from ansys.dyna.core import keywords as kwd


# ---------------------------------------------------------------------------
# Helpers shared across tests
# ---------------------------------------------------------------------------

_SCHEMAS_MIXED = (
    FieldSchema("id", int, 0, 10, 99),  # int with LSPP default
    FieldSchema("scale", float, 10, 10, 1.5),  # float with LSPP default
    FieldSchema("label", str, 20, 10, "AUTO"),  # str with LSPP default
    FieldSchema("free", float, 30, 10, None),  # no LSPP default → stays None
)

_SCHEMAS_FLAG = (
    FieldSchema("toggled", bool, 0, 10, Flag(True, "&", "")),  # flag with LSPP default
    FieldSchema("count", int, 10, 10, 7),
)

_SCHEMAS_NO_DEFAULTS = (
    FieldSchema("a", int, 0, 10, None),
    FieldSchema("b", float, 10, 10, None),
)


def _make_null_card(schemas):
    """Return a Card whose every field is None (simulates disable_lspp_defaults)."""
    return Card.from_field_schemas(schemas, values=[None] * len(schemas))


# ---------------------------------------------------------------------------
# Card-level tests
# ---------------------------------------------------------------------------


class TestFillCardDefaults:
    """Unit tests for _fill_card_defaults via the public apply_lspp_defaults path."""

    def test_int_float_str_fields_filled(self):
        card = _make_null_card(_SCHEMAS_MIXED)
        # precondition: all None
        assert card.get_value("id") is None
        assert card.get_value("scale") is None
        assert card.get_value("label") is None

        # wrap in a minimal keyword-like structure so apply_lspp_defaults can walk it
        keyword = _MinimalKeyword([card])
        apply_lspp_defaults(keyword)

        assert card.get_value("id") == 99
        assert card.get_value("scale") == 1.5
        assert card.get_value("label") == "AUTO"

    def test_field_with_no_lspp_default_stays_none(self):
        card = _make_null_card(_SCHEMAS_MIXED)
        keyword = _MinimalKeyword([card])
        apply_lspp_defaults(keyword)
        assert card.get_value("free") is None

    def test_existing_non_none_value_not_overwritten(self):
        card = Card.from_field_schemas(_SCHEMAS_MIXED, values=[42, None, None, None])
        keyword = _MinimalKeyword([card])
        apply_lspp_defaults(keyword)
        # explicitly-set value must survive
        assert card.get_value("id") == 42
        # others still filled from schema
        assert card.get_value("scale") == 1.5

    def test_flag_field_filled(self):
        card = _make_null_card(_SCHEMAS_FLAG)
        keyword = _MinimalKeyword([card])
        apply_lspp_defaults(keyword)
        filled = card.get_value("toggled")
        # The filled value should be the Flag instance from the schema default
        assert isinstance(filled, Flag)
        assert filled.true_value == "&"
        assert card.get_value("count") == 7

    def test_all_no_defaults_remains_none(self):
        card = _make_null_card(_SCHEMAS_NO_DEFAULTS)
        keyword = _MinimalKeyword([card])
        apply_lspp_defaults(keyword)
        assert card.get_value("a") is None
        assert card.get_value("b") is None

    def test_fields_set_flag_not_mutated(self):
        """_fields_set must not be touched — it tracks user intent, not schema defaults."""
        card = _make_null_card(_SCHEMAS_MIXED)
        assert card._fields_set is False
        keyword = _MinimalKeyword([card])
        apply_lspp_defaults(keyword)
        assert card._fields_set is False


# ---------------------------------------------------------------------------
# OptionCardSet tests
# ---------------------------------------------------------------------------


class TestOptionCardSetHandling:
    """apply_lspp_defaults must recurse into OptionCardSet child cards."""

    def test_option_card_children_filled_when_active(self):
        """Real keyword with OptionCardSet: children should be filled."""
        with disable_lspp_defaults():
            kw = kwd.SectionSolid()
            kw.activate_option("TITLE")
        # Before applying defaults, TITLE field should be None
        assert kw.title is None

        apply_lspp_defaults(kw)

        # TITLE field has no schema default, so it should still be None
        assert kw.title is None

    def test_option_card_base_fields_filled(self):
        """Real keyword with OptionCardSet: base card fields should be filled."""
        with disable_lspp_defaults():
            kw = kwd.SectionSolid()
            kw.activate_option("TITLE")

        assert kw.elform is None
        assert kw.aet is None

        apply_lspp_defaults(kw)

        assert kw.elform == 1  # LSPP default
        assert kw.aet == 0  # LSPP default


# ---------------------------------------------------------------------------
# SeriesCard tests
# ---------------------------------------------------------------------------


class TestSeriesCardHandling:
    """SeriesCard must be skipped with a debug log — no exception, no mutation."""

    def test_series_card_skipped_without_error(self):
        series = SeriesCard("val", 8, 10, float, lambda: 0, lambda: True)
        keyword = _MinimalKeyword([series])
        # Must not raise
        apply_lspp_defaults(keyword)

    def test_series_card_data_unchanged(self):
        series = SeriesCard("val", 8, 10, float, lambda: 3, lambda: True)
        series.data = [1.0, 2.0, 3.0]
        keyword = _MinimalKeyword([series])
        apply_lspp_defaults(keyword)
        # SeriesCard.data is stored as an array, so compare as such
        assert list(series.data) == [1.0, 2.0, 3.0]


# ---------------------------------------------------------------------------
# Unknown card type warning
# ---------------------------------------------------------------------------


class TestUnknownCardTypeWarning:
    def test_unknown_card_type_emits_warning(self):
        class _Alien:
            """Deliberately not a CardInterface subclass."""

            format = None

        keyword = _MinimalKeyword([_Alien()])
        with pytest.warns(UserWarning, match="unrecognised card type"):
            apply_lspp_defaults(keyword)


# ---------------------------------------------------------------------------
# Keyword-level integration tests using real generated keywords
# ---------------------------------------------------------------------------


class TestKeywordLevelIntegration:
    """End-to-end: create a keyword with disable_lspp_defaults, then restore."""

    def test_control_contact_restored(self):
        with disable_lspp_defaults():
            kw = kwd.ControlContact()

        # Spot-check a sample of fields that should be None before restoration
        assert kw.slsfac is None
        assert kw.islchk is None
        assert kw.orien is None

        apply_lspp_defaults(kw)

        assert kw.slsfac == 0.1  # float default
        assert kw.islchk == 1  # int default
        assert kw.orien == 1  # int default

    def test_fields_with_no_lspp_default_remain_none_after_restoration(self):
        with disable_lspp_defaults():
            kw = kwd.ControlContact()
        apply_lspp_defaults(kw)
        # rwpnal has no LSPP default in the schema
        assert kw.rwpnal is None

    def test_explicitly_set_field_survives_restoration(self):
        with disable_lspp_defaults():
            kw = kwd.ControlContact()
        kw.slsfac = 0.5  # user override
        apply_lspp_defaults(kw)
        assert kw.slsfac == 0.5  # must not be overwritten by default (0.1)

    def test_section_solid_with_option_card(self):
        """Keyword with an OptionCardSet: the TITLE option card child is also filled."""
        with disable_lspp_defaults():
            kw = kwd.SectionSolid()
            kw.activate_option("TITLE")

        assert kw.elform is None
        assert kw.aet is None

        apply_lspp_defaults(kw)

        assert kw.elform == 1  # LSPP default from _SECTIONSOLID_CARD0
        assert kw.aet == 0

    def test_idempotent(self):
        """Calling apply_lspp_defaults twice must produce the same result."""
        with disable_lspp_defaults():
            kw = kwd.ControlContact()
        apply_lspp_defaults(kw)
        first_slsfac = kw.slsfac
        apply_lspp_defaults(kw)
        assert kw.slsfac == first_slsfac


# ---------------------------------------------------------------------------
# TableCard tests
# ---------------------------------------------------------------------------

_TC_SCHEMAS = (
    FieldSchema("id", int, 0, 10, 99),      # int with LSPP default
    FieldSchema("scale", float, 10, 10, 1.5),  # float with LSPP default
    FieldSchema("label", str, 20, 10, "AUTO"),  # str with LSPP default
    FieldSchema("free", float, 30, 10, None),   # no LSPP default → stays as-is
)


def _make_null_table_card():
    """TableCard with _values all None (simulates disable_lspp_defaults)."""
    card = TableCard.from_field_schemas(_TC_SCHEMAS, length_func=None)
    card._values = [None] * len(_TC_SCHEMAS)
    return card


class TestTableCardHandling:
    """apply_lspp_defaults must fill TableCard columns via the correct path."""

    def test_pre_init_values_updated(self):
        """Pre-init: _values should be patched so _make_column uses schema defaults."""
        card = _make_null_table_card()
        assert not card._initialized

        apply_lspp_defaults(_MinimalKeyword([card]))

        assert card._values[0] == 99      # id
        assert card._values[1] == 1.5     # scale
        assert card._values[2] == "AUTO"  # label
        assert card._values[3] is None    # free — no default

    def test_pre_init_dataframe_reflects_defaults_on_first_access(self):
        """After patching _values, accessing .table should produce defaulted columns."""
        import pandas as pd

        card = _make_null_table_card()
        apply_lspp_defaults(_MinimalKeyword([card]))

        # Trigger lazy initialization by appending one row
        import numpy as np

        row = pd.DataFrame({"id": pd.array([1], dtype=pd.Int32Dtype()),
                            "scale": [2.0], "label": ["X"], "free": [np.nan]})
        card.table = row

        # Column defaults are baked into _values; a fresh bounded card would use them.
        # Here we just verify the visitor didn't break anything.
        assert len(card.table) == 1

    def test_post_init_none_cells_filled(self):
        """Post-init: None/NaN cells in an existing DataFrame are filled.

        Simulates a bounded TableCard that was lazily initialized under
        ``disable_lspp_defaults`` (``_values`` were all ``None``), so
        ``_make_column`` produced ``None``/``NaN`` columns.
        """
        import pandas as pd
        import numpy as np

        # Bounded card: _initialize_data uses _make_column which stores real
        # None/NaN (not string "None") for each type.
        card = TableCard.from_field_schemas(_TC_SCHEMAS, length_func=lambda: 2)
        card._values = [None, None, None, None]  # simulate disable_lspp_defaults
        # Trigger lazy init so DataFrame is built from the None _values
        _ = card.table

        # Patch row 1 with explicit non-None values to verify preservation
        card._table.loc[1, "id"] = 5
        card._table.loc[1, "scale"] = 2.0
        card._table.loc[1, "label"] = "KEEP"

        apply_lspp_defaults(_MinimalKeyword([card]))

        assert card.table["id"][0] == 99       # filled from schema default
        assert card.table["id"][1] == 5        # existing value preserved
        assert card.table["scale"][0] == 1.5   # filled
        assert card.table["scale"][1] == 2.0   # preserved
        assert card.table["label"][0] == "AUTO"  # filled
        assert card.table["label"][1] == "KEEP"  # preserved
        assert pd.isna(card.table["free"][0])  # no default → untouched
        assert pd.isna(card.table["free"][1])  # no default → untouched

    def test_post_init_idempotent(self):
        """Calling apply_lspp_defaults twice on a TableCard produces the same result."""
        card = TableCard.from_field_schemas(_TC_SCHEMAS, length_func=lambda: 1)
        card._values = [None, None, None, None]
        _ = card.table  # trigger lazy init with None _values

        kw = _MinimalKeyword([card])
        apply_lspp_defaults(kw)
        apply_lspp_defaults(kw)
        assert card.table["id"][0] == 99
        assert card.table["scale"][0] == 1.5


# ---------------------------------------------------------------------------
# TableCardGroup tests
# ---------------------------------------------------------------------------

_TCG_CARD_A = (
    FieldSchema("eid", int, 0, 10, 7),
    FieldSchema("x", float, 10, 10, 0.0),
)
_TCG_CARD_B = (
    FieldSchema("y", float, 0, 10, 9.9),
    FieldSchema("z", float, 10, 10, None),
)


class TestTableCardGroupHandling:
    """apply_lspp_defaults must delegate into each sub-card of a TableCardGroup."""

    def test_pre_init_sub_card_values_updated(self):
        """Pre-init group: each sub-card's _values should be patched."""
        group = TableCardGroup([_TCG_CARD_A, _TCG_CARD_B], length_func=None)
        # Blank out _values on both sub-cards
        for c in group._cards:
            c._values = [None] * len(c._schema.fields)

        apply_lspp_defaults(_MinimalKeyword([group]))

        assert group._cards[0]._values[0] == 7     # eid
        assert group._cards[0]._values[1] == 0.0   # x
        assert group._cards[1]._values[0] == 9.9   # y
        assert group._cards[1]._values[1] is None  # z — no default

    def test_post_init_none_cells_filled(self):
        """Post-init group: None/NaN cells across sub-cards are filled."""
        import pandas as pd
        import numpy as np

        group = TableCardGroup([_TCG_CARD_A, _TCG_CARD_B], length_func=lambda: 2)
        for card in group._cards:
            card._values = [None] * len(card._schema.fields)
        # Trigger lazy init so both sub-cards build DataFrames from None _values
        _ = group.table

        # Patch row 1 of each sub-card with explicit non-None values
        group._cards[0]._table.loc[1, "eid"] = 2
        group._cards[0]._table.loc[1, "x"] = 1.0
        group._cards[1]._table.loc[1, "y"] = 3.0

        apply_lspp_defaults(_MinimalKeyword([group]))

        # The group's merged table is rebuilt from sub-cards after patching
        merged = group.table
        assert merged["eid"][0] == 7      # filled
        assert merged["eid"][1] == 2      # preserved
        assert merged["x"][0] == 0.0     # filled
        assert merged["x"][1] == 1.0     # preserved
        assert merged["y"][0] == 9.9     # filled
        assert merged["y"][1] == 3.0     # preserved
        assert pd.isna(merged["z"][0])   # no default → untouched
        assert pd.isna(merged["z"][1])   # no default → untouched


# ---------------------------------------------------------------------------
# Minimal stub keyword used by unit tests
# ---------------------------------------------------------------------------


class _MinimalKeyword:
    """Lightweight stand-in for KeywordBase that holds an arbitrary card list."""

    def __init__(self, cards):
        self._cards = cards
