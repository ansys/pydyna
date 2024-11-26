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

class IcfdControlEmbedshell(KeywordBase):
    """DYNA ICFD_CONTROL_EMBEDSHELL keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_EMBEDSHELL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "gtype",
                        int,
                        0,
                        10,
                        kwargs.get("gtype", 0)
                    ),
                    Field(
                        "dist",
                        float,
                        10,
                        10,
                        kwargs.get("dist", 0.1)
                    ),
                    Field(
                        "tps",
                        int,
                        20,
                        10,
                        kwargs.get("tps", 0)
                    ),
                ],
            ),
        ]

    @property
    def gtype(self) -> int:
        """Get or set the Gap type. Defines the criteria for selecting a distance to build the gap between the embedded nodes and the newly generated :
        EQ.0:	Automatic and based on the surface mesh size multiplied by a scale factor given by DIST.Default method.
        EQ.1 : Specific gap size given by the user and defined by DIST
        """ # nopep8
        return self._cards[0].get_value("gtype")

    @gtype.setter
    def gtype(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""gtype must be one of {0,1}""")
        self._cards[0].set_value("gtype", value)

    @property
    def dist(self) -> float:
        """Get or set the Distance value if GTYPE=1 or scale factor value if GTYPE=0
        """ # nopep8
        return self._cards[0].get_value("dist")

    @dist.setter
    def dist(self, value: float) -> None:
        self._cards[0].set_value("dist", value)

    @property
    def tps(self) -> int:
        """Get or set the Triple Point Seal. Allows to control the fluid escape through triple points
        EQ.0:	Off
        EQ.1 : On.The triple points of embedded shells in contact to walls or among each other are sealed and no flow goes through them
        """ # nopep8
        return self._cards[0].get_value("tps")

    @tps.setter
    def tps(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""tps must be one of {0,1}""")
        self._cards[0].set_value("tps", value)

