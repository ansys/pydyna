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

"""Base class for cards and I/O"""

import typing

from ansys.dyna.core.lib.card_interface import CardInterface
from ansys.dyna.core.lib.card_writer import write_cards
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.kwd_line_formatter import read_line
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionsAPI


class Cards:
    def __init__(self, keyword):
        self._cards = []
        self._kw = keyword
        self._options_api = OptionsAPI(keyword)

    @property
    def options(self) -> OptionsAPI:
        """Gets the options_api of this keyword, if any"""
        return self._options_api

    @property
    def _cards(self) -> typing.List[CardInterface]:
        return self._base_cards

    @_cards.setter
    def _cards(self, value: typing.List[CardInterface]) -> None:
        self._base_cards = value

    def _get_non_option_cards(self) -> typing.List[CardInterface]:
        return [card for card in self._cards if type(card) != OptionCardSet]

    def _get_sorted_option_cards(self) -> typing.List[OptionCardSet]:
        option_cards = [card for card in self._cards if type(card) == OptionCardSet]
        option_cards.sort()
        return option_cards

    def _get_post_options_with_no_title_order(self):
        option_cards = [card for card in self._get_sorted_option_cards() if card.title_order == 0]
        for option_card in option_cards:
            assert option_card.card_order > 0, "Cards with a title order of 0 must have a positive card order"
        return option_cards

    def _get_active_options(self) -> typing.List[OptionCardSet]:
        """Return all active option card sets, sorted by card order."""
        option_cards = self._get_sorted_option_cards()
        active_option_cards = [o for o in option_cards if self._kw.is_option_active(o.name)]
        return active_option_cards

    def _flatten_2d_card_list(self, card_list: typing.List[typing.List[CardInterface]]) -> typing.List[CardInterface]:
        """Given a list of lists of cards, flatten into a single list of cards."""
        flattened = sum(card_list, [])
        return flattened

    def _unwrap_option_sets(
        self, option_sets: typing.List[OptionCardSet], fn_filter: typing.Callable
    ) -> typing.List[typing.List[CardInterface]]:
        """Given a list of card sets, turn it into a list of lists of cards.

        Apply the filter of fn_filter
        """
        return [option_set.cards for option_set in option_sets if fn_filter(option_set)]

    def _get_pre_option_cards(self) -> typing.List[CardInterface]:
        """Get the option cards that go before the non-optional cards."""
        active_option_sets = self._get_active_options()
        pre_option_cards = self._unwrap_option_sets(active_option_sets, lambda o: o.card_order < 0)
        return self._flatten_2d_card_list(pre_option_cards)

    def _get_post_option_cards(self) -> typing.List[CardInterface]:
        """Get the option cards that go after the non-optional cards."""
        active_option_sets = self._get_active_options()
        post_option_cards = self._unwrap_option_sets(active_option_sets, lambda o: o.card_order > 0)
        return self._flatten_2d_card_list(post_option_cards)

    def _get_all_cards(self) -> typing.List[CardInterface]:
        cards = self._get_pre_option_cards()
        cards.extend(self._get_non_option_cards())
        cards.extend(self._get_post_option_cards())
        return cards

    def write(
        self,
        buf: typing.TextIO,
        format: format_type,
        comment: typing.Optional[bool] = True,
    ) -> bool:
        """Writes the cards to `buf` using `format`.
        Returns whether a superfluous newline is added
        """
        superfluous_newline = write_cards(self._get_all_cards(), buf, format, comment)
        return superfluous_newline

    def _try_read_options_with_no_title(self, buf: typing.TextIO) -> None:
        # some cards are not active until we read.. how to handle?
        # if there are monotonically increasing options with a title order of 0
        # *AND* all active cards have been read with the maximum title order less
        # than the monotonically increasing options, then
        # the solution is to read more lines, activating one option at a time
        # they are assumed not to be activated here. when writing, they will
        # assumed to have been activated either manually or from being read.
        pos = buf.tell()
        any_options_read = False
        cards = self._get_post_options_with_no_title_order()
        exit = False
        while True:
            if len(cards) == 0:
                break
            linepos = buf.tell()
            _, exit = read_line(buf)
            if exit:
                break
            buf.seek(linepos)
            card = cards.pop(0)
            self._kw.activate_option(card.name)
            any_options_read = True
            card.read(buf)
        if not any_options_read:
            buf.seek(pos)

    def _read_data(self, buf: typing.TextIO) -> None:
        for card in self._get_pre_option_cards():
            card.read(buf)

        for card in self._get_non_option_cards():
            card.read(buf)

        for card in self._get_post_option_cards():
            card.read(buf)

        self._try_read_options_with_no_title(buf)
