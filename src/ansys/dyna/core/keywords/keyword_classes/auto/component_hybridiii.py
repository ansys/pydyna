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

class ComponentHybridiii(KeywordBase):
    """DYNA COMPONENT_HYBRIDIII keyword"""

    keyword = "COMPONENT"
    subkeyword = "HYBRIDIII"

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
                        "size",
                        int,
                        10,
                        10,
                        kwargs.get("size", 1)
                    ),
                    Field(
                        "units",
                        int,
                        20,
                        10,
                        kwargs.get("units", 1)
                    ),
                    Field(
                        "defrm",
                        int,
                        30,
                        10,
                        kwargs.get("defrm", 1)
                    ),
                    Field(
                        "vx",
                        float,
                        40,
                        10,
                        kwargs.get("vx", 0.0)
                    ),
                    Field(
                        "vy",
                        float,
                        50,
                        10,
                        kwargs.get("vy", 0.0)
                    ),
                    Field(
                        "vz",
                        float,
                        60,
                        10,
                        kwargs.get("vz", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "hx",
                        float,
                        0,
                        10,
                        kwargs.get("hx", 0.0)
                    ),
                    Field(
                        "hy",
                        float,
                        10,
                        10,
                        kwargs.get("hy", 0.0)
                    ),
                    Field(
                        "hz",
                        float,
                        20,
                        10,
                        kwargs.get("hz", 0.0)
                    ),
                    Field(
                        "rx",
                        float,
                        30,
                        10,
                        kwargs.get("rx", 0.0)
                    ),
                    Field(
                        "ry",
                        float,
                        40,
                        10,
                        kwargs.get("ry", 0.0)
                    ),
                    Field(
                        "rz",
                        float,
                        50,
                        10,
                        kwargs.get("rz", 0.0)
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
    def size(self) -> int:
        """Get or set the Size of dummy:
        EQ.1: 5th percentile adult (default),
        EQ.2: 50th percentile adult,
        EQ.3: 95th percentile adult.
        """ # nopep8
        return self._cards[0].get_value("size")

    @size.setter
    def size(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""size must be one of {1,2,3}""")
        self._cards[0].set_value("size", value)

    @property
    def units(self) -> int:
        """Get or set the System of units used in the finite element model:
        EQ.1: lbf*sec^2/in-inch-sec (default),
        EQ.2: kg-meter-sec,
        EQ.3:kgf*sec^2/mm-mm-sec,
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
    def defrm(self) -> int:
        """Get or set the Deformability type:
        EQ.1: all dummy segments entirely rigid (default),
        EQ.2: deformable abdomen (low density foam, mat #57),
        EQ.3: deformable jacket (low density foam, mat #57),
        EQ.4: deformable headskin (viscoelastic, mat #6),
        EQ.5: deformable abdomen/jacket,
        EQ.6: deformable jacket/headskin,
        EQ.7: deformable abdomen/headskin,
        EQ.8: deformable abdomen/jacket/headskin.
        """ # nopep8
        return self._cards[0].get_value("defrm")

    @defrm.setter
    def defrm(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 6, 7, 8]:
            raise Exception("""defrm must be one of {1,2,3,4,5,6,7,8}""")
        self._cards[0].set_value("defrm", value)

    @property
    def vx(self) -> float:
        """Get or set the Initial velocity of the dummy in the global x-direction.
        """ # nopep8
        return self._cards[0].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        self._cards[0].set_value("vx", value)

    @property
    def vy(self) -> float:
        """Get or set the Initial velocity of the dummy in the global y-direction.
        """ # nopep8
        return self._cards[0].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        self._cards[0].set_value("vy", value)

    @property
    def vz(self) -> float:
        """Get or set the Initial velocity of the dummy in the global z-direction.
        """ # nopep8
        return self._cards[0].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        self._cards[0].set_value("vz", value)

    @property
    def hx(self) -> float:
        """Get or set the Initial global x-coordinate value of the H-point.
        """ # nopep8
        return self._cards[1].get_value("hx")

    @hx.setter
    def hx(self, value: float) -> None:
        self._cards[1].set_value("hx", value)

    @property
    def hy(self) -> float:
        """Get or set the Initial global y-coordinate value of the H-point.
        """ # nopep8
        return self._cards[1].get_value("hy")

    @hy.setter
    def hy(self, value: float) -> None:
        self._cards[1].set_value("hy", value)

    @property
    def hz(self) -> float:
        """Get or set the Initial global z-coordinate value of the H-point.
        """ # nopep8
        return self._cards[1].get_value("hz")

    @hz.setter
    def hz(self, value: float) -> None:
        self._cards[1].set_value("hz", value)

    @property
    def rx(self) -> float:
        """Get or set the Initial rotation of dummy about the H-point with respect to the global x-axis (degrees).
        """ # nopep8
        return self._cards[1].get_value("rx")

    @rx.setter
    def rx(self, value: float) -> None:
        self._cards[1].set_value("rx", value)

    @property
    def ry(self) -> float:
        """Get or set the Initial rotation of dummy about the H-point with respect to the global y-axis (degrees).
        """ # nopep8
        return self._cards[1].get_value("ry")

    @ry.setter
    def ry(self, value: float) -> None:
        self._cards[1].set_value("ry", value)

    @property
    def rz(self) -> float:
        """Get or set the Initial rotation of dummy about the H-point with respect to the global z-axis (degrees).
        """ # nopep8
        return self._cards[1].get_value("rz")

    @rz.setter
    def rz(self, value: float) -> None:
        self._cards[1].set_value("rz", value)

