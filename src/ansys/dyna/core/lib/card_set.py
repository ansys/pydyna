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
        self._parent = kwargs.get("parent", None)
        self._keyword = kwargs.get("keyword", None)
        if option_specs == None:
            option_specs = []
        self._option_specs = option_specs
        self._initialized: bool = False

    @property
    def _items(self) -> typing.List[Cards]:
        return self._base_items

    @_items.setter
    def _items(self, value: typing.List[Cards]) -> None:
        self._base_items = value

    def _initialize(self):
        if self._initialized:
            return
        if self._bounded and self._active:
            self._initialize_data(self._length_func())
        self._initialized = True

    def initialize(self):
        self._initialize()

    @property
    def option_specs(self):
        return self._option_specs

    @property
    def _active(self) -> bool:
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
    ) -> typing.Union[str, None]:
        """Renders the card in the dyna keyword format.
        :param buf: Buffer to write to. If None, the output is returned as a string
        :param format: format_type to use. Default to standard.
        """

        def _write(buf: typing.TextIO):
            for item_index, item in enumerate([item for item in self._items]):
                write_comment = comment and item_index == 0
                if item_index != 0:
                    buf.write("\n")
                item.write(buf, format, write_comment)

        return write_or_return(buf, _write)

    def _read_item_cards(self, buf: typing.TextIO, index: int) -> bool:
        item = self._items[index]
        for card in item._get_all_cards():
            ret = card.read(buf)
            if ret:
                # according to the card, we are at the end of the keyword, so
                # we can break out of the card reading loop.
                return True
        return False

    def _load_bounded_from_buffer(self, buf: typing.TextIO) -> None:
        length = self._length_func()
        for index in range(length):
            if self._read_item_cards(buf, index):
                break

    def _load_unbounded_from_buffer(self, buf: typing.TextIO) -> None:
        index = -1
        while True:
            self._add_item_simple()
            index += 1
            self._read_item_cards(buf, index)
            if at_end_of_keyword(buf):
                # the buffer is at the end of the keyword, exit
                return

    def read(self, buf: typing.TextIO) -> bool:
        self._initialize()
        if self.bounded:
            self._load_bounded_from_buffer(buf)
            return False
        else:
            self._load_unbounded_from_buffer(buf)
            return True
