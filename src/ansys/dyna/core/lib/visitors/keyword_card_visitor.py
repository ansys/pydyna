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

"""Base visitor class for traversing the cards of a keyword.

This module provides a template-method pattern where the traversal skeleton
(dispatch on card type, recursion, special handling) is shared by all subclasses,
and only the leaf actions vary per subclass depending on the card type visited.

Card-type dispatch notes
------------------------
* :class:`~ansys.dyna.core.lib.card.Card` — fixed schema; field values stored in
  ``card._values``.  Routed to :meth:`visit_card` (default: no-op).
* :class:`~ansys.dyna.core.lib.table_card.TableCard` — subclasses ``Card`` but
  stores all data in a pandas DataFrame (``card.table``); ``card._values`` is
  stale scratch space and must not be used.  Routed to :meth:`visit_table_card`.
* :class:`~ansys.dyna.core.lib.table_card_group.TableCardGroup` — composite of
  ``TableCard`` instances sharing a single DataFrame.  Routed to
  :meth:`visit_table_card_group`.
* :class:`~ansys.dyna.core.lib.option_card.OptionCardSet` — recurses into child
  cards via :meth:`_visit_card`.
* :class:`~ansys.dyna.core.lib.card_set.CardSet` — recurses into items.
* :class:`~ansys.dyna.core.lib.series_card.SeriesCard` — variable-length array;
  routed to :meth:`visit_series_card` (default: debug-log and skip).
* Anything else — routed to :meth:`visit_unknown_card` (default: warning).
"""

import abc
import logging
import warnings

from ansys.dyna.core.lib.card import Card
from ansys.dyna.core.lib.card_interface import CardInterface
from ansys.dyna.core.lib.card_set import CardSet
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.option_card import OptionCardSet
from ansys.dyna.core.lib.series_card import SeriesCard
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.table_card_group import TableCardGroup

logger = logging.getLogger(__name__)


class KeywordCardVisitor(abc.ABC):
    """Base class for visiting the cards of a keyword.

    Provides a traversal skeleton (dispatch on card type, recursion into
    containers) shared by all subclasses.  Each card type is routed to a
    dedicated ``visit_*`` hook; all hooks default to no-ops so subclasses
    only override what they need.

    The visitor never touches :class:`~ansys.dyna.core.lib.card_interface.CardInterface`,
    allowing algorithms to be added without modifying the card hierarchy.

    Examples
    --------
    >>> class CardCounter(KeywordCardVisitor):
    ...     def __init__(self):
    ...         self.count = 0
    ...     def visit_card(self, card):
    ...         self.count += 1
    >>> counter = CardCounter()
    >>> counter.visit_keyword(my_keyword)
    >>> print(f"Plain cards: {counter.count}")
    """

    def visit_keyword(self, keyword: "KeywordBase") -> None:
        """Traverse all cards in the keyword.

        The keyword being visited is available to all hooks via
        ``self.keyword`` for the duration of this call.

        Parameters
        ----------
        keyword : KeywordBase
            The keyword to traverse.
        """
        self.keyword = keyword
        for card in keyword._cards:
            self._visit_card(card)

    def _visit_card(self, card: CardInterface) -> None:
        """Dispatch on card type and recurse. Called internally; do not override."""
        if isinstance(card, TableCard):
            # Must be checked before Card since TableCard subclasses Card.
            # TableCard stores data in a DataFrame (card.table), not in _values.
            self.visit_table_card(card)
        elif isinstance(card, Card):
            # Plain Card: fixed schema, values stored in card._values.
            self.visit_card(card)
        elif isinstance(card, TableCardGroup):
            # Composite of TableCards sharing a DataFrame.
            self.visit_table_card_group(card)
        elif isinstance(card, OptionCardSet):
            # Recurse into child cards.
            for child in card.cards:
                self._visit_card(child)
        elif isinstance(card, CardSet):
            # Recurse into items; each item is a Cards object with ._cards list.
            for item in card.items():
                for child_card in item._cards:
                    self._visit_card(child_card)
        elif isinstance(card, SeriesCard):
            # SeriesCard holds variable-length data; delegate to hook.
            self.visit_series_card(card)
        else:
            # Unknown card type; delegate to hook.
            self.visit_unknown_card(card)

    def visit_card(self, card: Card) -> None:
        """Called for every plain :class:`~ansys.dyna.core.lib.card.Card`.

        Never called for :class:`~ansys.dyna.core.lib.table_card.TableCard` or
        :class:`~ansys.dyna.core.lib.table_card_group.TableCardGroup` — those
        are routed to :meth:`visit_table_card` and
        :meth:`visit_table_card_group` respectively.

        The default implementation does nothing.

        Parameters
        ----------
        card : Card
            The plain card to visit.
        """

    def visit_table_card(self, card: TableCard) -> None:
        """Called when a TableCard is encountered. Override to handle custom logic.

        :class:`~ansys.dyna.core.lib.table_card.TableCard` stores all data in a
        pandas DataFrame (``card.table``).  ``card._values`` is stale and must not
        be read or written.  The default implementation does nothing.

        Parameters
        ----------
        card : TableCard
            The table card.
        """

    def visit_table_card_group(self, card: TableCardGroup) -> None:
        """Called when a TableCardGroup is encountered. Override to handle custom logic.

        :class:`~ansys.dyna.core.lib.table_card_group.TableCardGroup` stores data in
        a shared pandas DataFrame (``card.table``).  The default implementation does
        nothing.

        Parameters
        ----------
        card : TableCardGroup
            The table card group.
        """

    def visit_series_card(self, card: SeriesCard) -> None:
        """Called when a SeriesCard is encountered. Override to handle custom logic.

        The default implementation emits a debug log and does nothing.

        Parameters
        ----------
        card : SeriesCard
            The series card.
        """
        logger.debug(
            "KeywordCardVisitor: skipping SeriesCard %r (variable-length data).",
            card,
        )

    def visit_unknown_card(self, card: CardInterface) -> None:
        """Called when an unknown CardInterface subclass is encountered.

        The default implementation emits a warning and does nothing.

        Parameters
        ----------
        card : CardInterface
            The unknown card.
        """
        warnings.warn(
            f"KeywordCardVisitor: unrecognised card type {type(card).__name__!r} — skipped.",
            stacklevel=3,
        )
