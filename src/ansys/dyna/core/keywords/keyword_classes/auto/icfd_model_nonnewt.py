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

class IcfdModelNonnewt(KeywordBase):
    """DYNA ICFD_MODEL_NONNEWT keyword"""

    keyword = "ICFD"
    subkeyword = "MODEL_NONNEWT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nnmoid",
                        int,
                        0,
                        10,
                        kwargs.get("nnmoid")
                    ),
                    Field(
                        "nnid",
                        int,
                        10,
                        10,
                        kwargs.get("nnid", 1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "k",
                        float,
                        0,
                        10,
                        kwargs.get("k", 0.0)
                    ),
                    Field(
                        "n",
                        float,
                        10,
                        10,
                        kwargs.get("n", 0.0)
                    ),
                    Field(
                        "mumin",
                        float,
                        20,
                        10,
                        kwargs.get("mumin", 0.0)
                    ),
                    Field(
                        "lambda",
                        float,
                        30,
                        10,
                        kwargs.get("lambda", 1e30)
                    ),
                    Field(
                        "alpha",
                        float,
                        40,
                        10,
                        kwargs.get("alpha", 0)
                    ),
                    Field(
                        "talpha",
                        float,
                        50,
                        10,
                        kwargs.get("talpha", 0)
                    ),
                ],
            ),
        ]

    @property
    def nnmoid(self) -> typing.Optional[int]:
        """Get or set the Non-Newtonian Model ID.
        """ # nopep8
        return self._cards[0].get_value("nnmoid")

    @nnmoid.setter
    def nnmoid(self, value: int) -> None:
        self._cards[0].set_value("nnmoid", value)

    @property
    def nnid(self) -> int:
        """Get or set the Non-Newtonian fluid model type:
        EQ.1 : Power-Law model.
        EQ.2 : Carreau model.
        EQ.3 : Cross model.
        EQ.4 : Herschel-Bulkley model.
        EQ.5 : Cross II model.
        EQ.6 : Sutherland formula for temperature dependent viscosity.
        EQ.7 : Power-Law for temperature dependent viscosity.
        EQ.8 : Viscosity defined by Load Curve ID or Function ID.
        """ # nopep8
        return self._cards[0].get_value("nnid")

    @nnid.setter
    def nnid(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 6, 7, 8]:
            raise Exception("""nnid must be one of {1,2,3,4,5,6,7,8}""")
        self._cards[0].set_value("nnid", value)

    @property
    def k(self) -> float:
        """Get or set the Consistency index if NNID = 1 and 4. Zero shear Viscosity if NNID = 2,3 and 5.Reference viscosity if NNID = 6 and NNID = 7. Load curve ID or function ID if NNID = 8.
        """ # nopep8
        return self._cards[1].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[1].set_value("k", value)

    @property
    def n(self) -> float:
        """Get or set the Measure of the deviation of the fluid from Newtonian (Power Law index) for NNID = 1,2,3,4,5,7. Not used for NNID = 6 and 8.
        """ # nopep8
        return self._cards[1].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[1].set_value("n", value)

    @property
    def mumin(self) -> float:
        """Get or set the Minimum acceptable viscosity value if NNID = 1. Infinite Shear Viscosity if NNID = 2,5.Yielding viscosity if NNID = 4.Not used if NNID = 3,6,7,8.
        """ # nopep8
        return self._cards[1].get_value("mumin")

    @mumin.setter
    def mumin(self, value: float) -> None:
        self._cards[1].set_value("mumin", value)

    @property
    def lambda_(self) -> float:
        """Get or set the Maximum acceptable viscosity value if NNID = 1. Time constant if NNID = 2, 3, 5. Yield Stress Threshold if NNID = 4.Sutherland constant if NNID = 6. Not used if NNID = 7,8.
        """ # nopep8
        return self._cards[1].get_value("lambda")

    @lambda_.setter
    def lambda_(self, value: float) -> None:
        self._cards[1].set_value("lambda", value)

    @property
    def alpha(self) -> float:
        """Get or set the Activation energy if NNID = 1, 2. Not used if NNID = 3,4,5,6,7,8
        """ # nopep8
        return self._cards[1].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        self._cards[1].set_value("alpha", value)

    @property
    def talpha(self) -> float:
        """Get or set the Reference temperature if NNID = 2. Not used if NNID = 1,3,4,5,6,7,8
        """ # nopep8
        return self._cards[1].get_value("talpha")

    @talpha.setter
    def talpha(self, value: float) -> None:
        self._cards[1].set_value("talpha", value)

