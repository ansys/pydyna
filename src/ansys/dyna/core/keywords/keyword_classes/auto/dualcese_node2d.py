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

"""Module providing the DualceseNode2D class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DualceseNode2D(KeywordBase):
    """DYNA DUALCESE_NODE2D keyword"""

    keyword = "DUALCESE"
    subkeyword = "NODE2D"

    def __init__(self, **kwargs):
        """Initialize the DualceseNode2D class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nid",
                        int,
                        0,
                        8,
                        **kwargs,
                    ),
                    Field(
                        "x",
                        float,
                        8,
                        16,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "y",
                        float,
                        24,
                        16,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node number
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[0].set_value("nid", value)

    @property
    def x(self) -> float:
        """Get or set the x coordinate
        """ # nopep8
        return self._cards[0].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        """Set the x property."""
        self._cards[0].set_value("x", value)

    @property
    def y(self) -> float:
        """Get or set the y coordinate
        """ # nopep8
        return self._cards[0].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[0].set_value("y", value)

