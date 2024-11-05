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

class InitialVehicleKinematics(KeywordBase):
    """DYNA INITIAL_VEHICLE_KINEMATICS keyword"""

    keyword = "INITIAL"
    subkeyword = "VEHICLE_KINEMATICS"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "grav",
                        int,
                        0,
                        10,
                        kwargs.get("grav", 1)
                    ),
                    Field(
                        "psid",
                        int,
                        10,
                        10,
                        kwargs.get("psid")
                    ),
                    Field(
                        "xo",
                        float,
                        20,
                        10,
                        kwargs.get("xo", 0.0)
                    ),
                    Field(
                        "yo",
                        float,
                        30,
                        10,
                        kwargs.get("yo", 0.0)
                    ),
                    Field(
                        "zo",
                        float,
                        40,
                        10,
                        kwargs.get("zo", 0.0)
                    ),
                    Field(
                        "xf",
                        float,
                        50,
                        10,
                        kwargs.get("xf", 0.0)
                    ),
                    Field(
                        "yf",
                        float,
                        60,
                        10,
                        kwargs.get("yf", 0.0)
                    ),
                    Field(
                        "zf",
                        float,
                        70,
                        10,
                        kwargs.get("zf", 0.0)
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
                        "aaxis",
                        int,
                        30,
                        10,
                        kwargs.get("aaxis", 1)
                    ),
                    Field(
                        "baxis",
                        int,
                        40,
                        10,
                        kwargs.get("baxis", 1)
                    ),
                    Field(
                        "caxis",
                        int,
                        50,
                        10,
                        kwargs.get("caxis", 1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "aang",
                        float,
                        0,
                        10,
                        kwargs.get("aang", 0.0)
                    ),
                    Field(
                        "bang",
                        float,
                        10,
                        10,
                        kwargs.get("bang", 0.0)
                    ),
                    Field(
                        "cang",
                        float,
                        20,
                        10,
                        kwargs.get("cang", 0.0)
                    ),
                    Field(
                        "wa",
                        float,
                        30,
                        10,
                        kwargs.get("wa", 0.0)
                    ),
                    Field(
                        "wb",
                        float,
                        40,
                        10,
                        kwargs.get("wb", 0.0)
                    ),
                    Field(
                        "wc",
                        float,
                        50,
                        10,
                        kwargs.get("wc", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def grav(self) -> int:
        """Get or set the Gravity direction code:
        EQ.1: Global +x direction,
        EQ.-1: Global -x direction,
        EQ.2: Global +y direction,
        EQ.-2: Global -y direction,
        EQ.3 Global +z direction,
        EQ.-3: Global -z direction.
        Note: This must be the same for all vehicles present in the model.
        """ # nopep8
        return self._cards[0].get_value("grav")

    @grav.setter
    def grav(self, value: int) -> None:
        if value not in [1, -1, 2, -2, 3, -3]:
            raise Exception("""grav must be one of {1,-1,2,-2,3,-3}""")
        self._cards[0].set_value("grav", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part set ID, see also *SET_PART.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[0].set_value("psid", value)

    @property
    def xo(self) -> float:
        """Get or set the x-coordinate of initial position of mass center.
        """ # nopep8
        return self._cards[0].get_value("xo")

    @xo.setter
    def xo(self, value: float) -> None:
        self._cards[0].set_value("xo", value)

    @property
    def yo(self) -> float:
        """Get or set the y-coordinate of initial position of mass center.
        """ # nopep8
        return self._cards[0].get_value("yo")

    @yo.setter
    def yo(self, value: float) -> None:
        self._cards[0].set_value("yo", value)

    @property
    def zo(self) -> float:
        """Get or set the z-coordinate of initial position of mass center.
        """ # nopep8
        return self._cards[0].get_value("zo")

    @zo.setter
    def zo(self, value: float) -> None:
        self._cards[0].set_value("zo", value)

    @property
    def xf(self) -> float:
        """Get or set the x-coordinate of final position of mass center.
        """ # nopep8
        return self._cards[0].get_value("xf")

    @xf.setter
    def xf(self, value: float) -> None:
        self._cards[0].set_value("xf", value)

    @property
    def yf(self) -> float:
        """Get or set the y-coordinate of final position of mass center.
        """ # nopep8
        return self._cards[0].get_value("yf")

    @yf.setter
    def yf(self, value: float) -> None:
        self._cards[0].set_value("yf", value)

    @property
    def zf(self) -> float:
        """Get or set the z-coordinate of final position of mass center.
        """ # nopep8
        return self._cards[0].get_value("zf")

    @zf.setter
    def zf(self, value: float) -> None:
        self._cards[0].set_value("zf", value)

    @property
    def vx(self) -> float:
        """Get or set the x-component of mass center velocity.
        """ # nopep8
        return self._cards[1].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        self._cards[1].set_value("vx", value)

    @property
    def vy(self) -> float:
        """Get or set the y-component of mass center velocity.
        """ # nopep8
        return self._cards[1].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        self._cards[1].set_value("vy", value)

    @property
    def vz(self) -> float:
        """Get or set the z-component of mass center velocity.
        """ # nopep8
        return self._cards[1].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        self._cards[1].set_value("vz", value)

    @property
    def aaxis(self) -> int:
        """Get or set the First rotation axis code:
        EQ.1: Initially aligned with global x-axis,
        EQ.2: Initially aligned with global y-axis,
        EQ.3: Initially aligned with global z-axis.
        """ # nopep8
        return self._cards[1].get_value("aaxis")

    @aaxis.setter
    def aaxis(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""aaxis must be one of {1,2,3}""")
        self._cards[1].set_value("aaxis", value)

    @property
    def baxis(self) -> int:
        """Get or set the Second rotation axis code:
        EQ.1: Initially aligned with global x-axis,
        EQ.2: Initially aligned with global y-axis,
        EQ.3: Initially aligned with global z-axis.
        """ # nopep8
        return self._cards[1].get_value("baxis")

    @baxis.setter
    def baxis(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""baxis must be one of {1,2,3}""")
        self._cards[1].set_value("baxis", value)

    @property
    def caxis(self) -> int:
        """Get or set the Third rotation axis code:
        EQ.1: Initially aligned with global x-axis,
        EQ.2: Initially aligned with global y-axis,
        EQ.3: Initially aligned with global z-axis.
        """ # nopep8
        return self._cards[1].get_value("caxis")

    @caxis.setter
    def caxis(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""caxis must be one of {1,2,3}""")
        self._cards[1].set_value("caxis", value)

    @property
    def aang(self) -> float:
        """Get or set the Rotation angle about the first rotation axis (degrees).
        """ # nopep8
        return self._cards[2].get_value("aang")

    @aang.setter
    def aang(self, value: float) -> None:
        self._cards[2].set_value("aang", value)

    @property
    def bang(self) -> float:
        """Get or set the Rotation angle about the second rotation axis (degrees).
        """ # nopep8
        return self._cards[2].get_value("bang")

    @bang.setter
    def bang(self, value: float) -> None:
        self._cards[2].set_value("bang", value)

    @property
    def cang(self) -> float:
        """Get or set the Rotation angle about the third rotation axis (degrees).
        """ # nopep8
        return self._cards[2].get_value("cang")

    @cang.setter
    def cang(self, value: float) -> None:
        self._cards[2].set_value("cang", value)

    @property
    def wa(self) -> float:
        """Get or set the Angular velocity component for the first axis (radian/second).
        """ # nopep8
        return self._cards[2].get_value("wa")

    @wa.setter
    def wa(self, value: float) -> None:
        self._cards[2].set_value("wa", value)

    @property
    def wb(self) -> float:
        """Get or set the Angular velocity component for the second axis (radian/second).
        """ # nopep8
        return self._cards[2].get_value("wb")

    @wb.setter
    def wb(self, value: float) -> None:
        self._cards[2].set_value("wb", value)

    @property
    def wc(self) -> float:
        """Get or set the Angular velocity component for the third axis (radian/second).
        """ # nopep8
        return self._cards[2].get_value("wc")

    @wc.setter
    def wc(self, value: float) -> None:
        self._cards[2].set_value("wc", value)

