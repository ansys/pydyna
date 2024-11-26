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

class FrequencyDomainAcousticFringePlotPlate(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_ACOUSTIC_FRINGE_PLOT_PLATE keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_ACOUSTIC_FRINGE_PLOT_PLATE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "norm",
                        int,
                        0,
                        10,
                        kwargs.get("norm", 1)
                    ),
                    Field(
                        "len_x",
                        float,
                        10,
                        10,
                        kwargs.get("len_x")
                    ),
                    Field(
                        "len_y",
                        float,
                        20,
                        10,
                        kwargs.get("len_y")
                    ),
                    Field(
                        "x",
                        float,
                        30,
                        10,
                        kwargs.get("x")
                    ),
                    Field(
                        "y",
                        float,
                        40,
                        10,
                        kwargs.get("y")
                    ),
                    Field(
                        "z",
                        float,
                        50,
                        10,
                        kwargs.get("z")
                    ),
                    Field(
                        "nelm_x",
                        int,
                        60,
                        10,
                        kwargs.get("nelm_x", 10)
                    ),
                    Field(
                        "nelm_y",
                        int,
                        70,
                        10,
                        kwargs.get("nelm_y", 10)
                    ),
                ],
            ),
        ]

    @property
    def norm(self) -> int:
        """Get or set the Norm direction of the plate.
        EQ.1: x-direction
        EQ.2: y-direction
        EQ.3: z-direction
        """ # nopep8
        return self._cards[0].get_value("norm")

    @norm.setter
    def norm(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""norm must be one of {1,2,3}""")
        self._cards[0].set_value("norm", value)

    @property
    def len_x(self) -> typing.Optional[float]:
        """Get or set the Length of longer side of the plate.
        """ # nopep8
        return self._cards[0].get_value("len_x")

    @len_x.setter
    def len_x(self, value: float) -> None:
        self._cards[0].set_value("len_x", value)

    @property
    def len_y(self) -> typing.Optional[float]:
        """Get or set the Length of shorter side of the plate.
        """ # nopep8
        return self._cards[0].get_value("len_y")

    @len_y.setter
    def len_y(self, value: float) -> None:
        self._cards[0].set_value("len_y", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the x-coordinate of the center.
        """ # nopep8
        return self._cards[0].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        self._cards[0].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of the center.
        """ # nopep8
        return self._cards[0].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        self._cards[0].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the z-coordinate of the center.
        """ # nopep8
        return self._cards[0].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        self._cards[0].set_value("z", value)

    @property
    def nelm_x(self) -> int:
        """Get or set the Number of elements on longer side of the plate.
        """ # nopep8
        return self._cards[0].get_value("nelm_x")

    @nelm_x.setter
    def nelm_x(self, value: int) -> None:
        self._cards[0].set_value("nelm_x", value)

    @property
    def nelm_y(self) -> int:
        """Get or set the Number of elements on shorter side of the plate.
        """ # nopep8
        return self._cards[0].get_value("nelm_y")

    @nelm_y.setter
    def nelm_y(self, value: int) -> None:
        self._cards[0].set_value("nelm_y", value)

