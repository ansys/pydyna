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

"""Module providing the CeseBoundaryBlastLoadMsurfSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class CeseBoundaryBlastLoadMsurfSet(KeywordBase):
    """DYNA CESE_BOUNDARY_BLAST_LOAD_MSURF_SET keyword"""

    keyword = "CESE"
    subkeyword = "BOUNDARY_BLAST_LOAD_MSURF_SET"

    def __init__(self, **kwargs):
        """Initialize the CeseBoundaryBlastLoadMsurfSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "bid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "msurf_s",
                        int,
                        10,
                        10,
                        **kwargs,
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
        """Set the bid property."""
        self._cards[0].set_value("bid", value)

    @property
    def msurf_s(self) -> typing.Optional[int]:
        """Get or set the Identifier of a set of mesh surface part IDs created with a *LSO_ID_SET card.
        """ # nopep8
        return self._cards[0].get_value("msurf_s")

    @msurf_s.setter
    def msurf_s(self, value: int) -> None:
        """Set the msurf_s property."""
        self._cards[0].set_value("msurf_s", value)

