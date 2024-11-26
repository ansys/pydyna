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
"""Module to define function."""

import typing

from ansys.dyna.core.lib.card import Card, Field
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.text_card import TextCard


class DefineFunction(KeywordBase):
    """DYNA DEFINE_FUNCTION keyword."""

    keyword = "DEFINE"
    subkeyword = "FUNCTION"

    def __init__(self, **kwargs):
        """Initialize DefineFunction."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field("fid", int, 0, 10, kwargs.get("fid")),
                    Field("heading", str, 10, 70, kwargs.get("heading")),
                ],
            ),
            TextCard("function", kwargs.get("function")),
        ]

    @property
    def fid(self) -> typing.Optional[int]:
        """Get or set the Function ID.

        Functions, tables (see `*DEFINE_TABLE`), and load curves may not share common ID's.
        A unique number has to be defined.
        """
        return self._cards[0].get_value("fid")

    @fid.setter
    def fid(self, value: int) -> None:
        self._cards[0].set_value("fid", value)

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the An optional descriptive heading."""
        return self._cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        self._cards[0].set_value("heading", value)

    @property
    def function(self) -> typing.Optional[str]:
        """Get or set the Arithmetic expression involving a combination
        of independent variables and other functions.

        ,i.e., `f(a,b,c)=a*2+b*c+sqrt(a*c)`
        where a, b, and c are the independent variables.  The function name, f(a,b,c), must be
        unique since other functions can then use and reference this function.
        For example, `g(a,b,c,d)=f(a,b,c)**2+d`.  In this example, two `*DEFINE_ FUNCTION` definitions
        are needed to define functions f and g.
        """
        return self._cards[1].value

    @function.setter
    def function(self, value: str) -> None:
        """Set the function."""
        self._cards[1].value = value
