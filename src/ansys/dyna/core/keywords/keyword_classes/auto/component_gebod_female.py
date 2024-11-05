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

class ComponentGebodFemale(KeywordBase):
    """DYNA COMPONENT_GEBOD_FEMALE keyword"""

    keyword = "COMPONENT"
    subkeyword = "GEBOD_FEMALE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "did",
                        int,
                        0,
                        10,
                        kwargs.get("did")
                    ),
                    Field(
                        "units",
                        int,
                        10,
                        10,
                        kwargs.get("units", 1)
                    ),
                    Field(
                        "size",
                        float,
                        20,
                        10,
                        kwargs.get("size")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "vx",
                        float,
                        0,
                        10,
                        kwargs.get("vx", 0.0)
                    ),
                    Field(
                        "vy",
                        float,
                        10,
                        10,
                        kwargs.get("vy", 0.0)
                    ),
                    Field(
                        "vz",
                        float,
                        20,
                        10,
                        kwargs.get("vz", 0.0)
                    ),
                    Field(
                        "gx",
                        float,
                        30,
                        10,
                        kwargs.get("gx", 0.0)
                    ),
                    Field(
                        "gy",
                        float,
                        40,
                        10,
                        kwargs.get("gy", 0.0)
                    ),
                    Field(
                        "gz",
                        float,
                        50,
                        10,
                        kwargs.get("gz", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def did(self) -> typing.Optional[int]:
        """Get or set the Dummy ID. A unique number must be specified.
        """ # nopep8
        return self._cards[0].get_value("did")

    @did.setter
    def did(self, value: int) -> None:
        self._cards[0].set_value("did", value)

    @property
    def units(self) -> int:
        """Get or set the System of units used in the finite element model.
        EQ.1: lbf*sec^2/in-inch-sec,
        EQ.2: kg-meter-sec,
        EQ.3: kgf*sec^2/mm-mm-sec,
        EQ.4: metric ton-mm-sec,
        EQ.5: kg-mm-msec.
        """ # nopep8
        return self._cards[0].get_value("units")

    @units.setter
    def units(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5]:
            raise Exception("""units must be one of {1,2,3,4,5}""")
        self._cards[0].set_value("units", value)

    @property
    def size(self) -> typing.Optional[float]:
        """Get or set the Size of the dummy. This represents a combined height and weight percentile ranging from 0 to 100.
        """ # nopep8
        return self._cards[0].get_value("size")

    @size.setter
    def size(self, value: float) -> None:
        self._cards[0].set_value("size", value)

    @property
    def vx(self) -> float:
        """Get or set the Initial velocity of the dummy in the global x-direction.
        """ # nopep8
        return self._cards[1].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        self._cards[1].set_value("vx", value)

    @property
    def vy(self) -> float:
        """Get or set the Initial velocity of the dummy in the global y-direction.
        """ # nopep8
        return self._cards[1].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        self._cards[1].set_value("vy", value)

    @property
    def vz(self) -> float:
        """Get or set the Initial velocity of the dummy in the global z-direction.
        """ # nopep8
        return self._cards[1].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        self._cards[1].set_value("vz", value)

    @property
    def gx(self) -> float:
        """Get or set the Global x-component of gravitational acceleration applied to the dummy.
        """ # nopep8
        return self._cards[1].get_value("gx")

    @gx.setter
    def gx(self, value: float) -> None:
        self._cards[1].set_value("gx", value)

    @property
    def gy(self) -> float:
        """Get or set the Global y-component of gravitational acceleration applied to the dummy.
        """ # nopep8
        return self._cards[1].get_value("gy")

    @gy.setter
    def gy(self, value: float) -> None:
        self._cards[1].set_value("gy", value)

    @property
    def gz(self) -> float:
        """Get or set the Global z-component of gravitational acceleration applied to the dummy.
        """ # nopep8
        return self._cards[1].get_value("gz")

    @gz.setter
    def gz(self, value: float) -> None:
        self._cards[1].set_value("gz", value)

