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

class FrequencyDomainAcousticFringePlotSphere(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_ACOUSTIC_FRINGE_PLOT_SPHERE keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_ACOUSTIC_FRINGE_PLOT_SPHERE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "center",
                        int,
                        0,
                        10,
                        kwargs.get("center", 1)
                    ),
                    Field(
                        "r",
                        float,
                        10,
                        10,
                        kwargs.get("r")
                    ),
                    Field(
                        "density",
                        int,
                        20,
                        10,
                        kwargs.get("density")
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
                        "half1",
                        int,
                        60,
                        10,
                        kwargs.get("half1", 0)
                    ),
                    Field(
                        "half2",
                        int,
                        70,
                        10,
                        kwargs.get("half2", 0)
                    ),
                ],
            ),
        ]

    @property
    def center(self) -> int:
        """Get or set the Flag for defining the center point for the sphere.
        EQ.1: mass center of the original structure.
        EQ.2: geometry center of the original structure.
        EQ.3: defined by (x, y, z)..
        """ # nopep8
        return self._cards[0].get_value("center")

    @center.setter
    def center(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""center must be one of {1,2,3}""")
        self._cards[0].set_value("center", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Radius of the sphere..
        """ # nopep8
        return self._cards[0].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        self._cards[0].set_value("r", value)

    @property
    def density(self) -> typing.Optional[int]:
        """Get or set the Parameter to define how coarse or dense the created sphere mesh is. It is a
        number between 3 and 39, where "3" gives you 24 elements while "39"
        gives you 8664 elements.
        """ # nopep8
        return self._cards[0].get_value("density")

    @density.setter
    def density(self, value: int) -> None:
        self._cards[0].set_value("density", value)

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
    def half1(self) -> int:
        """Get or set the Create a half sphere by trimming the defined sphere (see Remark 3 and Figure 0-1). Note that (x_0,y_0,z_0 ) below is the center of the sphere.
        EQ.0:	A full sphere is created, no trimming
        EQ.1 : Keep x≥x_0
        EQ. - 1 : Keep x≤x_0
        EQ.2 : Keep y≥y_0
        EQ. - 2 : Keep y≤y_0
        EQ.3 : Keep z≥z_0
        EQ. - 3 : Keep z≤z_0
        """ # nopep8
        return self._cards[0].get_value("half1")

    @half1.setter
    def half1(self, value: int) -> None:
        if value not in [0, 1, -1, 2, -2, 3, -3]:
            raise Exception("""half1 must be one of {0,1,-1,2,-2,3,-3}""")
        self._cards[0].set_value("half1", value)

    @property
    def half2(self) -> int:
        """Get or set the Create a quarter sphere by trimming the half sphere defined with HALF1 (see Remark 3 and Figure 0-1):
        EQ.0:	No second trimming
        EQ.1 : Keep x≥x_0
        EQ. - 1 : Keep x≤x_0
        EQ.2 : Keep y≥y_0
        EQ. - 2 : Keep y≤y_0
        EQ.3 : Keep z≥z_0
        EQ. - 3 : Keep z≤z_0
        """ # nopep8
        return self._cards[0].get_value("half2")

    @half2.setter
    def half2(self, value: int) -> None:
        if value not in [0, 1, -1, 2, -2, 3, -3]:
            raise Exception("""half2 must be one of {0,1,-1,2,-2,3,-3}""")
        self._cards[0].set_value("half2", value)

