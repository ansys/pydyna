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

class EmEpCellmodelTomek(KeywordBase):
    """DYNA EM_EP_CELLMODEL_TOMEK keyword"""

    keyword = "EM"
    subkeyword = "EP_CELLMODEL_TOMEK"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "matid",
                        int,
                        0,
                        10,
                        kwargs.get("matid")
                    ),
                    Field(
                        "phiendmid",
                        float,
                        10,
                        10,
                        kwargs.get("phiendmid")
                    ),
                    Field(
                        "phimidepl",
                        float,
                        20,
                        10,
                        kwargs.get("phimidepl")
                    ),
                ],
            ),
        ]

    @property
    def matid(self) -> typing.Optional[int]:
        """Get or set the Material ID: refers to MID in the *PART card
        """ # nopep8
        return self._cards[0].get_value("matid")

    @matid.setter
    def matid(self, value: int) -> None:
        self._cards[0].set_value("matid", value)

    @property
    def phiendmid(self) -> typing.Optional[float]:
        """Get or set the Value between 0 and 1 that indicates the ratio of the cardiac tissue to be considered as the endocardial version of the ToR-Ord cell model
        """ # nopep8
        return self._cards[0].get_value("phiendmid")

    @phiendmid.setter
    def phiendmid(self, value: float) -> None:
        self._cards[0].set_value("phiendmid", value)

    @property
    def phimidepl(self) -> typing.Optional[float]:
        """Get or set the Value between 0 and 1 that indicates the ratio of the cardiac tissue to be considered as the myocardial version of the ToR-Ord cell model
        """ # nopep8
        return self._cards[0].get_value("phimidepl")

    @phimidepl.setter
    def phimidepl(self, value: float) -> None:
        self._cards[0].set_value("phimidepl", value)

