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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ConstrainedAdaptivity(KeywordBase):
    """DYNA CONSTRAINED_ADAPTIVITY keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "ADAPTIVITY"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "dnid",
                        int,
                        0,
                        10,
                        kwargs.get("dnid")
                    ),
                    Field(
                        "nid1",
                        int,
                        10,
                        10,
                        kwargs.get("nid1")
                    ),
                    Field(
                        "nid2",
                        int,
                        20,
                        10,
                        kwargs.get("nid2")
                    ),
                ],
            ),
        ]

    @property
    def dnid(self) -> typing.Optional[int]:
        """Get or set the Dependent node. This is the node constrained at the midpoint of an edge of an element.
        """ # nopep8
        return self._cards[0].get_value("dnid")

    @dnid.setter
    def dnid(self, value: int) -> None:
        self._cards[0].set_value("dnid", value)

    @property
    def nid1(self) -> typing.Optional[int]:
        """Get or set the Node at one end of an element edge
        """ # nopep8
        return self._cards[0].get_value("nid1")

    @nid1.setter
    def nid1(self, value: int) -> None:
        self._cards[0].set_value("nid1", value)

    @property
    def nid2(self) -> typing.Optional[int]:
        """Get or set the Node at the other end of that same element edge.
        """ # nopep8
        return self._cards[0].get_value("nid2")

    @nid2.setter
    def nid2(self, value: int) -> None:
        self._cards[0].set_value("nid2", value)

