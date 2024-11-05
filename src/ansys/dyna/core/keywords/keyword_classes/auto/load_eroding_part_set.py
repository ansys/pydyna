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

class LoadErodingPartSet(KeywordBase):
    """DYNA LOAD_ERODING_PART_SET keyword"""

    keyword = "LOAD"
    subkeyword = "ERODING_PART_SET"

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
                        kwargs.get("id")
                    ),
                    Field(
                        "lcid",
                        int,
                        10,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "sf",
                        float,
                        20,
                        10,
                        kwargs.get("sf", 1)
                    ),
                    Field(
                        "at",
                        float,
                        30,
                        10,
                        kwargs.get("at", 0.0)
                    ),
                    Field(
                        "psid",
                        int,
                        40,
                        10,
                        kwargs.get("psid")
                    ),
                    Field(
                        "boxid",
                        int,
                        50,
                        10,
                        kwargs.get("boxid", 0)
                    ),
                    Field(
                        "mem",
                        int,
                        60,
                        10,
                        kwargs.get("mem", 50)
                    ),
                    Field(
                        "alpha",
                        float,
                        70,
                        10,
                        kwargs.get("alpha", 80.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "iflag",
                        int,
                        0,
                        10,
                        kwargs.get("iflag", 0)
                    ),
                    Field(
                        "x",
                        float,
                        10,
                        10,
                        kwargs.get("x", 0.0)
                    ),
                    Field(
                        "y",
                        float,
                        20,
                        10,
                        kwargs.get("y", 0.0)
                    ),
                    Field(
                        "z",
                        float,
                        30,
                        10,
                        kwargs.get("z", 0.0)
                    ),
                    Field(
                        "beta",
                        float,
                        40,
                        10,
                        kwargs.get("beta", 90.0)
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the ID number.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining pressure as a function of time, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Scale factor.
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        self._cards[0].set_value("sf", value)

    @property
    def at(self) -> float:
        """Get or set the Arrival time.
        """ # nopep8
        return self._cards[0].get_value("at")

    @at.setter
    def at(self, value: float) -> None:
        self._cards[0].set_value("at", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part set ID, see *SET_PART..
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[0].set_value("psid", value)

    @property
    def boxid(self) -> int:
        """Get or set the Box ID, see *DEFINE_BOX.
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        self._cards[0].set_value("boxid", value)

    @property
    def mem(self) -> int:
        """Get or set the Extra memory, in percent, to be allocated above the initial memory for storing the new load segments exposed by the erosion.
        """ # nopep8
        return self._cards[0].get_value("mem")

    @mem.setter
    def mem(self, value: int) -> None:
        self._cards[0].set_value("mem", value)

    @property
    def alpha(self) -> float:
        """Get or set the The maximum angle (in degrees) permitted between the normal of a segment at its centroid and the average normal at its nodes. This angle is used to eliminate interior segments.
        """ # nopep8
        return self._cards[0].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        self._cards[0].set_value("alpha", value)

    @property
    def iflag(self) -> int:
        """Get or set the Flag for choosing a subset of the exposed surface that is oriented towards a blast or other loading source. The vector from the center of the element to the source location must be within an angle of BETA of the surface normal. If IFLAG>0, then the subset is chosen, otherwise if  IFLAG=0, the entire surface is loaded.
        """ # nopep8
        return self._cards[1].get_value("iflag")

    @iflag.setter
    def iflag(self, value: int) -> None:
        self._cards[1].set_value("iflag", value)

    @property
    def x(self) -> float:
        """Get or set the Optional source location.
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        self._cards[1].set_value("x", value)

    @property
    def y(self) -> float:
        """Get or set the Optional source location.
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        self._cards[1].set_value("y", value)

    @property
    def z(self) -> float:
        """Get or set the Optional source location.
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        self._cards[1].set_value("z", value)

    @property
    def beta(self) -> float:
        """Get or set the Maximum permitted angle (in degrees) between the surface normal and the vector to the source. The exposed segment is not loaded if the calculated angle is greater than BETA.
        """ # nopep8
        return self._cards[1].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[1].set_value("beta", value)

