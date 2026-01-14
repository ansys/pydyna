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
"""Module for handling groups of table cards."""

import io
import math
import typing

import pandas as pd

from ansys.dyna.core.lib.card import Card
from ansys.dyna.core.lib.card_interface import CardInterface
from ansys.dyna.core.lib.format_type import format_type
from ansys.dyna.core.lib.io_utils import write_or_return
from ansys.dyna.core.lib.kwd_line_formatter import buffer_to_lines
from ansys.dyna.core.lib.parameters import ParameterSet
from ansys.dyna.core.lib.table_card import TableCard, get_first_row, try_initialize_table


def _to_table_card(card: Card, length_func: typing.Callable) -> TableCard:
    return TableCard(card._fields, length_func, card._active_func)


class TableCardGroup(CardInterface):
    def __init__(
        self,
        cards: typing.List[Card],
        length_func: typing.Callable,
        active_func: typing.Callable = None,
        name: str = None,
        format: format_type = format_type.default,
        **kwargs,
    ):
        self._cards = [_to_table_card(card, length_func) for card in cards]
        self._length_func = length_func
        self._active_func = active_func
        self.format = format
        if length_func == None:
            self._bounded = False
            self._length_func = self._get_unbounded_length
        else:
            self._bounded = True
            self._length_func = length_func
        self._initialized = try_initialize_table(self, name, **kwargs)
        if not self._initialized:
            first_row = get_first_row(self._get_fields(), **kwargs)
            for card in self._cards:
                card._first_row = first_row

    def _get_fields(self):
        fields = []
        for card in self._cards:
            fields.extend(card._fields)
        return fields

    def _initialize(self) -> None:
        if self._initialized:
            return
        [card._initialize(True) for card in self._cards]
        data = pd.concat([card.table for card in self._cards], axis=1)
        self._table = data
        self._initialized = True

    @property
    def table(self) -> pd.DataFrame:
        self._initialize()
        return self._table

    @table.setter
    def table(self, value: pd.DataFrame):
        # remove duplicate columns
        value = value.loc[:, ~value.columns.duplicated()]
        # store the table
        self._table = value
        # propagate table to child duplicate card objects
        self._propagate()
        self._initialized = True

    @property
    def format(self) -> format_type:
        return self._format

    def _propagate(self) -> None:
        """Propagate view of data frame to all child cards."""
        for card in self._cards:
            card.table = self._table

    @format.setter
    def format(self, value: format_type) -> None:
        self._format = value
        for card in self._cards:
            card.format = value

    def _load_unbounded_from_buffer(self, buf: typing.TextIO, parameter_set: ParameterSet) -> None:
        data_lines = buffer_to_lines(buf)
        self._load_lines(data_lines, parameter_set)

    def _load_bounded_from_buffer(self, buf: typing.TextIO, parameter_set: ParameterSet) -> None:
        data_lines = buffer_to_lines(buf, self._num_rows())
        self._load_lines(data_lines, parameter_set)

    def read(self, buf: typing.TextIO, parameter_set: ParameterSet = None) -> None:
        if self.bounded:
            self._load_bounded_from_buffer(buf, parameter_set)
        else:
            self._load_unbounded_from_buffer(buf, parameter_set)

    def _load_lines(self, data_lines: typing.List[str], parameter_set: ParameterSet) -> None:
        """Load the card data from a list of strings."""
        card_lines = self._divide_data_lines(data_lines)
        for index, lines in enumerate(card_lines):
            self._cards[index]._load_lines(lines, parameter_set)
        self.table = pd.concat([card.table for card in self._cards], axis=1)

    def write(
        self,
        format: typing.Optional[format_type] = None,
        buf: typing.Optional[typing.TextIO] = None,
        comment: typing.Optional[bool] = True,
    ) -> str:
        if self.active:
            self._initialize()
            self._propagate()

        if format == None:
            format = self.format

        def _as_buffer(card: TableCard, add_newline: bool) -> io.StringIO:
            card_buf = io.StringIO()
            card.write(format, card_buf, True)
            if add_newline:
                card_buf.write("\n")
            card_buf.seek(0)
            return card_buf

        def _write(buf: typing.TextIO):
            if self._num_rows() > 0:
                card_buffers = []
                active_cards = self._get_active_cards()
                for idx, card in enumerate(active_cards):
                    card_buffer = _as_buffer(card, idx != len(active_cards) - 1)
                    card_buffers.append(card_buffer)

                iter = zip(*card_buffers)
                comment_lines = next(iter)
                first_lines = next(iter)
                for comment_line, first_line in zip(comment_lines, first_lines):
                    if comment:
                        buf.write(comment_line)
                    buf.write(first_line)
                for lines in zip(*card_buffers):
                    for line in lines:
                        buf.write(line)

        return write_or_return(buf, _write)

    def _divide_data_lines(self, data_lines: typing.List[str]) -> typing.List:
        """Divides the data lines into a set of lines, one for each sub-card"""
        card_lines = [[] for i in range(len(self._cards))]
        for index, line in enumerate(data_lines):
            card_index = self._get_index_of_which_card(index)
            card_lines[card_index].append(line)
        return card_lines

    def _get_index_of_which_card(self, overall_index: int) -> int:
        """Given the overall index, returns the index into self._cards
        to identify which sub-card the overall index indexes into
        """
        return overall_index % len(self._get_active_cards())

    def _get_index_of_given_card(self, overall_index: int) -> int:
        """Given the overall index, returns the index to be used to
        index into the card given by _get_index_of_which_card
        """
        return math.floor(overall_index / len(self._get_active_cards()))

    @property
    def active(self) -> bool:
        if self._active_func == None:
            return True
        return self._active_func()

    def _is_card_active(self, card) -> bool:
        if card._active_func != None:
            return card._active_func()
        return True

    def _get_active_cards(self) -> typing.List[TableCard]:
        return [card for card in self._cards if self._is_card_active(card)]

    def _get_unbounded_length(self) -> int:
        """The unbounded length is the minimum of all sub-card's unbounded length"""
        self._initialize()  # Need to initialize first, so that the sub card can calculate num_rows
        lens = [card._num_rows() for card in self._get_active_cards()]
        return min(lens)

    @property
    def bounded(self) -> bool:
        return self._bounded

    def _num_rows(self) -> int:
        if not self.active:
            return 0
        num_active_cards = len(self._get_active_cards())
        return self._length_func() * num_active_cards

    def __repr__(self) -> str:
        """Returns a console-friendly representation of the desired parameters for the card"""
        self._propagate()
        content_lines = []
        for card in self._get_active_cards():
            content_lines.append(card._get_comment(self._format))
        output = "\n".join(content_lines)
        return "TableCardGroup: \n" + output
