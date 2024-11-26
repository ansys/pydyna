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

class InterfaceEnsight(KeywordBase):
    """DYNA INTERFACE_ENSIGHT keyword"""

    keyword = "INTERFACE"
    subkeyword = "ENSIGHT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nset",
                        int,
                        0,
                        10,
                        kwargs.get("nset")
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "nlfile",
                        int,
                        20,
                        10,
                        kwargs.get("nlfile")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "gfile",
                        str,
                        0,
                        80,
                        kwargs.get("gfile")
                    ),
                ],
            ),
        ]

    @property
    def nset(self) -> typing.Optional[int]:
        """Get or set the Set of node in ls-dyna input deck, which contains all nodes to be subject to transient loading mapped from the following Ensight-formatted geometry file and loading files.
        """ # nopep8
        return self._cards[0].get_value("nset")

    @nset.setter
    def nset(self, value: int) -> None:
        self._cards[0].set_value("nset", value)

    @property
    def nlfile(self) -> typing.Optional[int]:
        """Get or set the number of transient loading files, which contain loading info. For each node defined in the geometry file at various time steps defined in the last card.
        """ # nopep8
        return self._cards[0].get_value("nlfile")

    @nlfile.setter
    def nlfile(self, value: int) -> None:
        self._cards[0].set_value("nlfile", value)

    @property
    def gfile(self) -> typing.Optional[str]:
        """Get or set the EnSightGold-formatted geometry file describing part of the mesh, nodal 	coordinates and element connectivity, used by other solver to yield the loading described by the following loading files.  Even the geometry file can contain the whole mesh or just part of the mesh subject to transient loading defined in the following transient loading, the latter is recommended �C including only mesh subject to loading defined in 	loading files �C to  reduce memory requirement and speed up analysis.
        """ # nopep8
        return self._cards[1].get_value("gfile")

    @gfile.setter
    def gfile(self, value: str) -> None:
        self._cards[1].set_value("gfile", value)

