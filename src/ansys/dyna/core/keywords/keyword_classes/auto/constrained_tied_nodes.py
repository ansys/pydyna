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

class ConstrainedTiedNodes(KeywordBase):
    """DYNA CONSTRAINED_TIED_NODES keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "TIED_NODES"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nsid",
                        int,
                        0,
                        10,
                        kwargs.get("nsid")
                    ),
                    Field(
                        "eppf",
                        float,
                        10,
                        10,
                        kwargs.get("eppf", 0.0)
                    ),
                    Field(
                        "etype",
                        int,
                        20,
                        10,
                        kwargs.get("etype", 0)
                    ),
                ],
            ),
        ]

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID, see *SET_NODE.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        self._cards[0].set_value("nsid", value)

    @property
    def eppf(self) -> float:
        """Get or set the Plastic strain, volumetric strain, or damage (MAT_107, MAT_110, MAT_224, or GISSMO) at failure.
        """ # nopep8
        return self._cards[0].get_value("eppf")

    @eppf.setter
    def eppf(self, value: float) -> None:
        self._cards[0].set_value("eppf", value)

    @property
    def etype(self) -> int:
        """Get or set the Element type for nodal group:
        EQ:0: shell,
        EQ.1: solid element.
        """ # nopep8
        return self._cards[0].get_value("etype")

    @etype.setter
    def etype(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""etype must be one of {0,1}""")
        self._cards[0].set_value("etype", value)

