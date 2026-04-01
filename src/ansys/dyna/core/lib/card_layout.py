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

"""Pure functions for assembling a keyword's card sequence.

Given a flat list of ``CardInterface`` objects (which may include
``OptionCardSet`` entries) and an ``OptionsInterface`` that knows which
options are active, these functions compute the ordered card list used
for reading and writing keyword data.
"""

import typing

from ansys.dyna.core.lib.card_interface import CardInterface
from ansys.dyna.core.lib.card_position import CardPlacement
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionsInterface


def get_non_option_cards(cards: typing.List[CardInterface]) -> typing.List[CardInterface]:
    """Return cards that are not ``OptionCardSet`` entries."""
    return [card for card in cards if type(card) != OptionCardSet]


def get_sorted_option_cards(cards: typing.List[CardInterface]) -> typing.List[OptionCardSet]:
    """Return ``OptionCardSet`` entries sorted by position."""
    option_cards = [card for card in cards if type(card) == OptionCardSet]
    option_cards.sort()
    return option_cards


def get_post_options_with_no_title_order(cards: typing.List[CardInterface]) -> typing.List[OptionCardSet]:
    """Return POST-placed option cards that have ``title_order == 0``.

    These represent options whose presence is discovered at read time
    (they don't appear in the keyword title line).
    """
    option_cards = [card for card in get_sorted_option_cards(cards) if card.title_order == 0]
    for option_card in option_cards:
        if option_card.position.placement == CardPlacement.PRE:
            raise ValueError("Cards with a title order of 0 must not have 'pre' placement")
    return [o for o in option_cards if o.position.placement == CardPlacement.POST]


def get_active_options(
    cards: typing.List[CardInterface], options_api: OptionsInterface
) -> typing.List[OptionCardSet]:
    """Return active option card sets, sorted by position."""
    option_cards = get_sorted_option_cards(cards)
    return [o for o in option_cards if options_api.is_option_active(o.name)]


def get_pre_option_cards(
    cards: typing.List[CardInterface], options_api: OptionsInterface
) -> typing.List[CardInterface]:
    """Return the inner cards of active PRE-placed option sets."""
    active = get_active_options(cards, options_api)
    nested = [o.cards for o in active if o.position.placement == CardPlacement.PRE]
    return [card for group in nested for card in group]


def get_post_option_cards(
    cards: typing.List[CardInterface], options_api: OptionsInterface
) -> typing.List[CardInterface]:
    """Return the inner cards of active POST-placed option sets."""
    active = get_active_options(cards, options_api)
    nested = [o.cards for o in active if o.position.placement == CardPlacement.POST]
    return [card for group in nested for card in group]


def get_main_option_cards_by_index(
    cards: typing.List[CardInterface], options_api: OptionsInterface
) -> typing.Dict[int, typing.List[CardInterface]]:
    """Return active MAIN option cards grouped by their insertion index.

    The key *N* is the 0-based index of the non-option card **after
    which** the option cards are inserted.
    """
    active = get_active_options(cards, options_api)
    by_index: typing.Dict[int, typing.List[CardInterface]] = {}
    for option_set in active:
        if option_set.position.placement == CardPlacement.MAIN:
            idx = option_set.position.index
            by_index.setdefault(idx, []).extend(option_set.cards)
    return by_index


def get_all_cards(
    cards: typing.List[CardInterface], options_api: OptionsInterface
) -> typing.List[CardInterface]:
    """Assemble the full card sequence with active option cards interleaved.

    Layout order: PRE options, then non-option cards with MAIN options
    inserted at their designated indices, then POST options.
    """
    main_by_index = get_main_option_cards_by_index(cards, options_api)
    result = get_pre_option_cards(cards, options_api)
    non_option = get_non_option_cards(cards)
    for i, card in enumerate(non_option):
        result.append(card)
        result.extend(main_by_index.get(i, []))
    result.extend(get_post_option_cards(cards, options_api))
    return result
