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

class ControlRefineMppDistribution(KeywordBase):
    """DYNA CONTROL_REFINE_MPP_DISTRIBUTION keyword"""

    keyword = "CONTROL"
    subkeyword = "REFINE_MPP_DISTRIBUTION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id", 0)
                    ),
                    Field(
                        "dx",
                        float,
                        10,
                        10,
                        kwargs.get("dx", 0.0)
                    ),
                    Field(
                        "dy",
                        float,
                        20,
                        10,
                        kwargs.get("dy", 0.0)
                    ),
                    Field(
                        "dz",
                        float,
                        30,
                        10,
                        kwargs.get("dz", 0.0)
                    ),
                    Field(
                        "ex",
                        float,
                        40,
                        10,
                        kwargs.get("ex", 1.0)
                    ),
                    Field(
                        "ey",
                        float,
                        50,
                        10,
                        kwargs.get("ey", 1.0)
                    ),
                    Field(
                        "ez",
                        float,
                        60,
                        10,
                        kwargs.get("ez", 1.0)
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> int:
        """Get or set the ID = -NTOTRF in *CONTROL_REFINE_ALE.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def dx(self) -> float:
        """Get or set the Dimensionless x-displacement of the box. (see Remark 1).
        """ # nopep8
        return self._cards[0].get_value("dx")

    @dx.setter
    def dx(self, value: float) -> None:
        self._cards[0].set_value("dx", value)

    @property
    def dy(self) -> float:
        """Get or set the Dimensionless y-displacement of the box. (see Remark 1).
        """ # nopep8
        return self._cards[0].get_value("dy")

    @dy.setter
    def dy(self, value: float) -> None:
        self._cards[0].set_value("dy", value)

    @property
    def dz(self) -> float:
        """Get or set the Dimensionless z-displacement of the box. (see Remark 1).
        """ # nopep8
        return self._cards[0].get_value("dz")

    @dz.setter
    def dz(self, value: float) -> None:
        self._cards[0].set_value("dz", value)

    @property
    def ex(self) -> float:
        """Get or set the Dimensionless x-expansion of the box. (see Remark 2).
        """ # nopep8
        return self._cards[0].get_value("ex")

    @ex.setter
    def ex(self, value: float) -> None:
        self._cards[0].set_value("ex", value)

    @property
    def ey(self) -> float:
        """Get or set the Dimensionless y-expansion of the box. (see Remark 2).
        """ # nopep8
        return self._cards[0].get_value("ey")

    @ey.setter
    def ey(self, value: float) -> None:
        self._cards[0].set_value("ey", value)

    @property
    def ez(self) -> float:
        """Get or set the Dimensionless z-expansion of the box. (see Remark 2).
        """ # nopep8
        return self._cards[0].get_value("ez")

    @ez.setter
    def ez(self, value: float) -> None:
        self._cards[0].set_value("ez", value)

