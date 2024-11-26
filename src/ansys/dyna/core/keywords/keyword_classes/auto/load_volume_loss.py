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

class LoadVolumeLoss(KeywordBase):
    """DYNA LOAD_VOLUME_LOSS keyword"""

    keyword = "LOAD"
    subkeyword = "VOLUME_LOSS"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "psid",
                        int,
                        0,
                        10,
                        kwargs.get("psid")
                    ),
                    Field(
                        "coord",
                        int,
                        10,
                        10,
                        kwargs.get("coord")
                    ),
                    Field(
                        "lcur",
                        int,
                        20,
                        10,
                        kwargs.get("lcur", 0)
                    ),
                    Field(
                        "fx",
                        float,
                        30,
                        10,
                        kwargs.get("fx", 1.0)
                    ),
                    Field(
                        "fy",
                        float,
                        40,
                        10,
                        kwargs.get("fy", 1.0)
                    ),
                    Field(
                        "fz",
                        float,
                        50,
                        10,
                        kwargs.get("fz", 1.0)
                    ),
                    Field(
                        "pmin",
                        float,
                        60,
                        10,
                        kwargs.get("pmin", -10E20)
                    ),
                    Field(
                        "factor",
                        float,
                        70,
                        10,
                        kwargs.get("factor", 0.01)
                    ),
                ],
            ),
        ]

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part Set ID.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[0].set_value("psid", value)

    @property
    def coord(self) -> typing.Optional[int]:
        """Get or set the Coordinate System ID (default - global coordinate system).
        """ # nopep8
        return self._cards[0].get_value("coord")

    @coord.setter
    def coord(self, value: int) -> None:
        self._cards[0].set_value("coord", value)

    @property
    def lcur(self) -> int:
        """Get or set the Curve ID containing volume fraction lost as a function of time.
        """ # nopep8
        return self._cards[0].get_value("lcur")

    @lcur.setter
    def lcur(self, value: int) -> None:
        self._cards[0].set_value("lcur", value)

    @property
    def fx(self) -> float:
        """Get or set the Fraction of strain occurring in x-direction.
        """ # nopep8
        return self._cards[0].get_value("fx")

    @fx.setter
    def fx(self, value: float) -> None:
        self._cards[0].set_value("fx", value)

    @property
    def fy(self) -> float:
        """Get or set the Fraction of strain occurring in y-direction.
        """ # nopep8
        return self._cards[0].get_value("fy")

    @fy.setter
    def fy(self, value: float) -> None:
        self._cards[0].set_value("fy", value)

    @property
    def fz(self) -> float:
        """Get or set the Fraction of strain occurring in z-direction.
        """ # nopep8
        return self._cards[0].get_value("fz")

    @fz.setter
    def fz(self, value: float) -> None:
        self._cards[0].set_value("fz", value)

    @property
    def pmin(self) -> float:
        """Get or set the (Leave blank).
        """ # nopep8
        return self._cards[0].get_value("pmin")

    @pmin.setter
    def pmin(self, value: float) -> None:
        self._cards[0].set_value("pmin", value)

    @property
    def factor(self) -> float:
        """Get or set the Feedback factor.
        """ # nopep8
        return self._cards[0].get_value("factor")

    @factor.setter
    def factor(self, value: float) -> None:
        self._cards[0].set_value("factor", value)

