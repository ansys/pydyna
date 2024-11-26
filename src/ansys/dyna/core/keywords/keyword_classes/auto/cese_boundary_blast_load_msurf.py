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

class CeseBoundaryBlastLoadMsurf(KeywordBase):
    """DYNA CESE_BOUNDARY_BLAST_LOAD_MSURF keyword"""

    keyword = "CESE"
    subkeyword = "BOUNDARY_BLAST_LOAD_MSURF"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "bid",
                        int,
                        0,
                        10,
                        kwargs.get("bid")
                    ),
                    Field(
                        "msurfid",
                        int,
                        10,
                        10,
                        kwargs.get("msurfid")
                    ),
                ],
            ),
        ]

    @property
    def bid(self) -> typing.Optional[int]:
        """Get or set the Blast source ID.
        """ # nopep8
        return self._cards[0].get_value("bid")

    @bid.setter
    def bid(self, value: int) -> None:
        self._cards[0].set_value("bid", value)

    @property
    def msurfid(self) -> typing.Optional[int]:
        """Get or set the A mesh surface part ID referenced in *MESH_SURFACE_ELEMENT cards
        """ # nopep8
        return self._cards[0].get_value("msurfid")

    @msurfid.setter
    def msurfid(self, value: int) -> None:
        self._cards[0].set_value("msurfid", value)

