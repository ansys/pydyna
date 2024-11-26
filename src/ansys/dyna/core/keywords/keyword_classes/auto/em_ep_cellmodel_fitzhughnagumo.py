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

class EmEpCellmodelFitzhughnagumo(KeywordBase):
    """DYNA EM_EP_CELLMODEL_FITZHUGHNAGUMO keyword"""

    keyword = "EM"
    subkeyword = "EP_CELLMODEL_FITZHUGHNAGUMO"

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
                ],
            ),
            Card(
                [
                    Field(
                        "alpha",
                        float,
                        0,
                        10,
                        kwargs.get("alpha")
                    ),
                    Field(
                        "beta",
                        float,
                        10,
                        10,
                        kwargs.get("beta")
                    ),
                    Field(
                        "gamma",
                        float,
                        20,
                        10,
                        kwargs.get("gamma")
                    ),
                    Field(
                        "c",
                        float,
                        30,
                        10,
                        kwargs.get("c")
                    ),
                    Field(
                        "mu1",
                        float,
                        40,
                        10,
                        kwargs.get("mu1")
                    ),
                    Field(
                        "mu2",
                        float,
                        50,
                        10,
                        kwargs.get("mu2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "v",
                        float,
                        0,
                        10,
                        kwargs.get("v")
                    ),
                    Field(
                        "r",
                        float,
                        10,
                        10,
                        kwargs.get("r")
                    ),
                ],
            ),
        ]

    @property
    def matid(self) -> typing.Optional[int]:
        """Get or set the Material ID defined in *MAT_.

        """ # nopep8
        return self._cards[0].get_value("matid")

    @matid.setter
    def matid(self, value: int) -> None:
        self._cards[0].set_value("matid", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Excitation constant alpha described in Equation(1).
        """ # nopep8
        return self._cards[1].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        self._cards[1].set_value("alpha", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Excitation constant beta described in Equation(2).
        """ # nopep8
        return self._cards[1].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[1].set_value("beta", value)

    @property
    def gamma(self) -> typing.Optional[float]:
        """Get or set the Excitation constant gamma described in Equation(2).
        """ # nopep8
        return self._cards[1].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        self._cards[1].set_value("gamma", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Excitation constant c described in Equation(1).
        """ # nopep8
        return self._cards[1].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        self._cards[1].set_value("c", value)

    @property
    def mu1(self) -> typing.Optional[float]:
        """Get or set the Excitation constant mu1 described in Equation(2).
        """ # nopep8
        return self._cards[1].get_value("mu1")

    @mu1.setter
    def mu1(self, value: float) -> None:
        self._cards[1].set_value("mu1", value)

    @property
    def mu2(self) -> typing.Optional[float]:
        """Get or set the Excitation constant mu2 described in Equation (2).
        """ # nopep8
        return self._cards[1].get_value("mu2")

    @mu2.setter
    def mu2(self, value: float) -> None:
        self._cards[1].set_value("mu2", value)

    @property
    def v(self) -> typing.Optional[float]:
        """Get or set the Initial value of V.
        """ # nopep8
        return self._cards[2].get_value("v")

    @v.setter
    def v(self, value: float) -> None:
        self._cards[2].set_value("v", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Initial value of r.
        """ # nopep8
        return self._cards[2].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        self._cards[2].set_value("r", value)

