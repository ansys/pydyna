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

"""Module providing the InterfaceSsiAux class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class InterfaceSsiAux(KeywordBase):
    """DYNA INTERFACE_SSI_AUX keyword"""

    keyword = "INTERFACE"
    subkeyword = "SSI_AUX"

    def __init__(self, **kwargs):
        """Initialize the InterfaceSsiAux class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "gmset",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "setid",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def gmset(self) -> typing.Optional[int]:
        """Get or set the Identifier for this set of recorded motions to be referred to in *INTERFACE_SSI. Must be unique.
        """ # nopep8
        return self._cards[0].get_value("gmset")

    @gmset.setter
    def gmset(self, value: int) -> None:
        """Set the gmset property."""
        self._cards[0].set_value("gmset", value)

    @property
    def setid(self) -> typing.Optional[int]:
        """Get or set the Segment set or node set ID where motions are to be recorded.
        """ # nopep8
        return self._cards[0].get_value("setid")

    @setid.setter
    def setid(self, value: int) -> None:
        """Set the setid property."""
        self._cards[0].set_value("setid", value)

