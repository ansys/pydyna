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

"""Set of cards that act as one card.

It is a generic card, so it needs to be given a type as an argument.
That type is used for each card, and behaves like a keyword.
"""

import typing

from ansys.dyna.core.lib.card_interface import CardInterface
from ansys.dyna.core.lib.cards import Cards
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.io_utils import write_or_return
from ansys.dyna.core.lib.kwd_line_formatter import at_end_of_keyword
from ansys.dyna.core.lib.option_card import OptionSpec
from ansys.dyna.core.lib.parameters import ParameterSet


class CardSet(CardInterface):
    def __init__(
        self,
        set_type: type,
        length_func: typing.Callable = None,
        active_func: typing.Callable = None,
        option_specs: typing.List[OptionSpec] = None,
        **kwargs,
    ):
        self._set_type = set_type
        self._items: typing.List[Cards] = list()
        self._format_type: format_type = kwargs.get("format", format_type.default)
        self._length_func = length_func
        self._active_func = active_func
        self._bounded = length_func != None
        self._parent = kwargs.pop("parent", None)
        self._keyword = kwargs.pop("keyword", None)
        if option_specs == None:
            option_specs = []
        self._option_specs = option_specs
        self._initialized: bool = False
        if len(kwargs) > 0 and not self._bounded:
            # implicit unbounded initializer!
            self._initialize(**kwargs)
        kwargs["parent"] = self._parent
        kwargs["keyword"] = self._keyword

    @property
    def _items(self) -> typing.List[Cards]:
        return self._base_items

    @_items.setter
    def _items(self, value: typing.List[Cards]) -> None:
        self._base_items = value

    def __len__(self) -> int:
        if not self.active:
            return 0
        if self._bounded:
            return self._length_func()
        else:
            return len(self._base_items)

    def _initialize(self, **kwargs) -> None:
        if self._initialized:
            return
        if self._bounded:
            self._initialize_data(self._length_func())
        elif len(kwargs) > 0:
            self.add_item(**kwargs)
        self._initialized = True

    @property
    def option_specs(self) -> typing.List[OptionSpec]:
        return self._option_specs

    @property
    def active(self) -> bool:
        if self._active_func == None:
            return True
        return self._active_func()

    def _initialize_data(self, num_items: int) -> None:
        for _ in range(num_items):
            self.add_item()

    def _add_item_simple(self) -> int:
        self._items.append(self._set_type(parent=self._parent, keyword=self._keyword))

    def add_item(self, **kwargs) -> int:
        """Add a card to the set. Return the index of the added card."""
        self._items.append(self._set_type(**kwargs, parent=self._parent, keyword=self._keyword))
        return len(self._items) - 1

    def items(self) -> typing.List[Cards]:
        if not self._initialized:
            self._initialize()
        return self._items

    @property
    def bounded(self) -> bool:
        return self._bounded

    @property
    def format(self) -> format_type:
        """Get the card format type."""
        return self._format_type

    @format.setter
    def format(self, value: format_type) -> None:
        """Set the card format type."""
        self._format_type = value

    def write(
        self,
        format: typing.Optional[format_type] = None,
        buf: typing.Optional[typing.TextIO] = None,
        comment: typing.Optional[bool] = True,
        **kwargs,
    ) -> typing.Union[str, None]:
        """Renders the card in the dyna keyword format.
        :param buf: Buffer to write to. If None, the output is returned as a string
        :param format: format_type to use. Default to standard.
        """

        def _write(buf: typing.TextIO):
            # write each item in the card set
            for item_index, item in enumerate([item for item in self._items]):
                write_comment = comment and item_index == 0
                if item_index != 0:
                    buf.write("\n")
                item.write(buf, format, write_comment, **kwargs)

        return write_or_return(buf, _write)

    def _read_item_cards(self, buf: typing.TextIO, index: int, parameter_set: ParameterSet) -> bool:
        """Read cards for a single item in the set.

        If the item has a custom `_read_data` method, use it for reading.
        This allows CardSet items to implement custom read logic for cases
        where card activation depends on values within those cards (e.g.,
        MAT_295 fiber families where ftype determines which card format to use).

        Args:
            buf: The text buffer to read from.
            index: The index of the item to read.
            parameter_set: Optional parameter set for substitution.

        Returns:
            True if the reader hit the end of the keyword early, False otherwise.
            Custom `_read_data` methods should also return a boolean with this meaning.
        """
        item = self._items[index]

        # Check if item has custom read logic
        if hasattr(item, "_read_data"):
            return item._read_data(buf, parameter_set)

        # Default: iterate through all cards
        for card in item._get_all_cards():
            ret = card.read(buf, parameter_set)
            if ret:
                # according to the card, we are at the end of the keyword, so
                # we can break out of the card reading loop.
                return True
        return False

    def _load_bounded_from_buffer(self, buf: typing.TextIO, parameter_set: ParameterSet) -> None:
        length = self._length_func()
        for index in range(length):
            if self._read_item_cards(buf, index, parameter_set):
                break

    def _load_unbounded_from_buffer(self, buf: typing.TextIO, parameter_set: ParameterSet) -> None:
        index = -1
        while True:
            self._add_item_simple()
            index += 1
            self._read_item_cards(buf, index, parameter_set)
            if at_end_of_keyword(buf):
                return

    def read(self, buf: typing.TextIO, parameter_set: ParameterSet = None) -> bool:
        if not self.active:
            return False
        self._initialize()
        if self.bounded:
            self._load_bounded_from_buffer(buf, parameter_set)
            return False
        else:
            self._load_unbounded_from_buffer(buf, parameter_set)
            return True


def read_cards_with_discriminator(
    cards: typing.List,
    buf: typing.TextIO,
    parameters,
    discriminator: "Field",
    cards_with_field: typing.List[int],
) -> bool:
    """Read cards where a discriminator field determines which card variant to use.

    This handles the "chicken-and-egg" problem where card conditionals depend on a
    field value that is IN those very cards. The solution:
    1. Read non-discriminator cards first (cards before the first discriminator card)
    2. Peek at next line to extract the discriminator field value
    3. Set the field on all cards that have it (so conditionals evaluate correctly)
    4. Read remaining cards (only active ones will consume data)

    Args:
        cards: List of Card objects in the CardSet item
        buf: Text buffer to read from
        parameters: Parameter set for substitution
        discriminator: Field instance with name, offset, width, and default value
        cards_with_field: List of card indices that contain the discriminator field

    Returns:
        True if the reader hit end of keyword early, False otherwise.
    """
    from ansys.dyna.core.lib.kwd_line_formatter import read_line

    first_discriminator_card = min(cards_with_field)

    # Read non-discriminator cards first
    for i, card in enumerate(cards):
        if i >= first_discriminator_card:
            break
        card.read(buf, parameters)

    # Peek at next line to determine discriminator value
    pos = buf.tell()
    line, _ = read_line(buf)
    buf.seek(pos)

    # Parse discriminator field value using Field's offset and width
    default_val = discriminator.value if discriminator.value is not None else 1
    try:
        end_pos = discriminator.offset + discriminator.width
        field_str = line[discriminator.offset : end_pos].strip()
        field_val = int(field_str) if field_str else default_val
    except (ValueError, IndexError):
        field_val = default_val

    # Set field on all cards that have it so conditionals work
    for card_idx in cards_with_field:
        cards[card_idx].set_value(discriminator.name, field_val)

    # Read remaining cards (only active ones will consume data)
    for i, card in enumerate(cards):
        if i >= first_discriminator_card:
            card.read(buf, parameters)

    return False


def ensure_card_set_properties(kwd, for_setter: bool) -> None:
    """Help with handling card sets.

    For convenience the first card set can be manipulated by the keyword
    if it is currently empty. Getters, on the other hand, only work if
    a card set has been added."""
    num_sets = len(kwd.sets)
    if num_sets == 0:
        if for_setter:
            kwd.add_set()
            return
        else:
            raise LookupError("Cannot get property, there are no sets. Use `add_set()` to add a set!")
    if num_sets != 1:
        raise LookupError("Cannot get property, there is not exactly one card set!")
