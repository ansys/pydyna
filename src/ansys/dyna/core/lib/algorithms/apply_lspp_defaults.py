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

"""Apply LSPP defaults to keyword fields."""

import typing

from ansys.dyna.core.lib.card import Card
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.table_card_group import TableCardGroup
from ansys.dyna.core.lib.visitors import KeywordCardVisitor


class _LsppDefaultFiller(KeywordCardVisitor):
    """Visitor that fills None-valued fields with their LSPP schema defaults.

    Subclasses can override :meth:`_get_fill_value` to change the fill policy
    without rewriting the traversal or DataFrame logic.
    """

    def _get_fill_value(self, field_schema: FieldSchema) -> typing.Any:
        """Return the value to fill for a ``None`` field, or ``None`` to skip.

        The default implementation returns the schema's LSPP default, or
        ``None`` (skip) if the schema has no default.

        Override this in a subclass to customise fill behaviour — for example,
        falling back to ``0.0`` for float fields with no schema default, or
        excluding specific fields based on keyword type (available via
        ``self.keyword``).

        Parameters
        ----------
        field_schema : FieldSchema
            The schema of the field about to be filled.

        Returns
        -------
        Any
            The value to write, or ``None`` to leave the field untouched.
        """
        return field_schema.default

    def visit_card(self, card: Card) -> None:
        """Fill each None-valued field using :meth:`_get_fill_value`."""
        for index, fs in enumerate(card._schema.fields):
            if card._values[index] is not None:
                continue
            fill = self._get_fill_value(fs)
            if fill is not None:
                card._values[index] = fill

    def visit_table_card(self, card: TableCard) -> None:
        """Fill a TableCard using :meth:`_get_fill_value`.

        Two cases:
        - Not yet initialized (lazy): update ``_values`` so that ``_make_column``
          uses the fill value when the DataFrame is built on first access.
        - Already initialized: fill ``None``/``NaN`` cells in each DataFrame
          column with the fill value.
        """
        for index, fs in enumerate(card._schema.fields):
            fill = self._get_fill_value(fs)
            if fill is None:
                continue
            if not card._initialized:
                if card._values[index] is None:
                    card._values[index] = fill
            else:
                col_name = fs.name
                if col_name not in card._table.columns:
                    continue
                col = card._table[col_name]
                if fs.type == float:
                    mask = col.isna()
                else:
                    mask = col.isna() | (col == None)  # noqa: E711
                if mask.any():
                    card._table[col_name] = col.where(~mask, other=fill)

    def visit_table_card_group(self, group: TableCardGroup) -> None:
        """Fill each sub-card of a TableCardGroup using :meth:`_get_fill_value`."""
        for card in group._cards:
            self.visit_table_card(card)
        if group._initialized:
            import pandas as pd

            group._table = pd.concat([card.table for card in group._cards], axis=1)


def apply_lspp_defaults(keyword: "KeywordBase") -> None:
    """Retroactively apply LSPP defaults to all ``None``-valued fields in a keyword.

    When a keyword is created inside a :func:`~ansys.dyna.core.lib.config.disable_lspp_defaults`
    context, fields that have no LSPP default are left as ``None``.  This function
    walks every card in *keyword* and fills in any ``None`` value whose
    :class:`~ansys.dyna.core.lib.field_schema.FieldSchema` carries a non-``None``
    LSPP default.  Fields that were already explicitly set (including to ``None``
    intentionally) are indistinguishable from unset ones, so only the schema default
    is used as the fill criterion — existing non-``None`` values are never overwritten.

    Supported card types and their behaviour:

    * :class:`~ansys.dyna.core.lib.card.Card` — each ``None`` value is replaced
      with its schema default.
    * :class:`~ansys.dyna.core.lib.table_card.TableCard` — if the DataFrame has
      not yet been lazily initialized, ``_values`` is updated so that
      ``_make_column`` uses the schema defaults when the DataFrame is first
      built.  If the DataFrame already exists, ``None``/``NaN`` cells in each
      column are filled with the corresponding schema default.
    * :class:`~ansys.dyna.core.lib.table_card_group.TableCardGroup` — delegates
      to each sub-card's ``TableCard`` handling, then rebuilds the merged
      DataFrame if the group had already been initialized.
    * :class:`~ansys.dyna.core.lib.option_card.OptionCardSet` — recurses into
      its child cards.
    * :class:`~ansys.dyna.core.lib.card_set.CardSet` — recurses into items.
    * :class:`~ansys.dyna.core.lib.series_card.SeriesCard` — skipped; series cards
      hold a variable-length array whose element defaults are not meaningful in this
      context.  A debug message is emitted.
    * Any other :class:`~ansys.dyna.core.lib.card_interface.CardInterface` subclass —
      skipped with a warning.

    Parameters
    ----------
    keyword : KeywordBase
        The keyword whose cards should have LSPP defaults applied.

    Examples
    --------
    >>> from ansys.dyna.core.lib.config import disable_lspp_defaults
    >>> from ansys.dyna.core.lib.algorithms import apply_lspp_defaults
    >>> from ansys.dyna.core import keywords as kwd
    >>> with disable_lspp_defaults():
    ...     k = kwd.SectionSolid()
    >>> k.elform  # None — no LSPP default applied yet
    >>> apply_lspp_defaults(k)
    >>> k.elform  # 1 — LSPP default restored
    """
    _LsppDefaultFiller().visit_keyword(keyword)
