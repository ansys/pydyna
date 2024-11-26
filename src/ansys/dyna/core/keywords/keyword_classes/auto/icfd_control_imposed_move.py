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

class IcfdControlImposedMove(KeywordBase):
    """DYNA ICFD_CONTROL_IMPOSED_MOVE keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_IMPOSED_MOVE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "lcvx",
                        int,
                        10,
                        10,
                        kwargs.get("lcvx")
                    ),
                    Field(
                        "lcvy",
                        int,
                        20,
                        10,
                        kwargs.get("lcvy")
                    ),
                    Field(
                        "lcvz",
                        int,
                        30,
                        10,
                        kwargs.get("lcvz")
                    ),
                    Field(
                        "vadt",
                        int,
                        40,
                        10,
                        kwargs.get("vadt", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "alphal",
                        int,
                        0,
                        10,
                        kwargs.get("alphal")
                    ),
                    Field(
                        "betal",
                        int,
                        10,
                        10,
                        kwargs.get("betal")
                    ),
                    Field(
                        "gammal",
                        int,
                        20,
                        10,
                        kwargs.get("gammal")
                    ),
                    Field(
                        "alphag",
                        int,
                        30,
                        10,
                        kwargs.get("alphag")
                    ),
                    Field(
                        "betag",
                        int,
                        40,
                        10,
                        kwargs.get("betag")
                    ),
                    Field(
                        "gammag",
                        int,
                        50,
                        10,
                        kwargs.get("gammag")
                    ),
                    Field(
                        "vadr",
                        int,
                        60,
                        10,
                        kwargs.get("vadr", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ptid",
                        int,
                        0,
                        10,
                        kwargs.get("ptid", 0)
                    ),
                    Field(
                        "x1",
                        float,
                        10,
                        10,
                        kwargs.get("x1", 1.0)
                    ),
                    Field(
                        "y1",
                        float,
                        20,
                        10,
                        kwargs.get("y1", 0.0)
                    ),
                    Field(
                        "z1",
                        float,
                        30,
                        10,
                        kwargs.get("z1", 0.0)
                    ),
                    Field(
                        "x2",
                        float,
                        40,
                        10,
                        kwargs.get("x2", 0.0)
                    ),
                    Field(
                        "y2",
                        float,
                        50,
                        10,
                        kwargs.get("y2", 1.0)
                    ),
                    Field(
                        "z2",
                        float,
                        60,
                        10,
                        kwargs.get("z2", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ptido",
                        int,
                        0,
                        10,
                        kwargs.get("ptido", 0)
                    ),
                    Field(
                        "axe",
                        int,
                        10,
                        10,
                        kwargs.get("axe", 0)
                    ),
                    Field(
                        "ptidv",
                        int,
                        20,
                        10,
                        kwargs.get("ptidv", 0)
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the This can be any part ID referenced in *ICFD_PART or *ICFD_PART_VOL. If PID = 0,then the whole volume mesh will be used.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def lcvx(self) -> typing.Optional[int]:
        """Get or set the LCID for the velocity in the three directions (X,Y,Z).
        """ # nopep8
        return self._cards[0].get_value("lcvx")

    @lcvx.setter
    def lcvx(self, value: int) -> None:
        self._cards[0].set_value("lcvx", value)

    @property
    def lcvy(self) -> typing.Optional[int]:
        """Get or set the LCID for the velocity in the three directions (X,Y,Z).
        """ # nopep8
        return self._cards[0].get_value("lcvy")

    @lcvy.setter
    def lcvy(self, value: int) -> None:
        self._cards[0].set_value("lcvy", value)

    @property
    def lcvz(self) -> typing.Optional[int]:
        """Get or set the LCID for the velocity in the three directions (X,Y,Z).
        """ # nopep8
        return self._cards[0].get_value("lcvz")

    @lcvz.setter
    def lcvz(self, value: int) -> None:
        self._cards[0].set_value("lcvz", value)

    @property
    def vadt(self) -> int:
        """Get or set the Velocity/Displacements flag for translation components
        EQ.0:Prescribe Velocity
        EQ.1:Prescribe Displacements
        """ # nopep8
        return self._cards[0].get_value("vadt")

    @vadt.setter
    def vadt(self, value: int) -> None:
        self._cards[0].set_value("vadt", value)

    @property
    def alphal(self) -> typing.Optional[int]:
        """Get or set the LCID for the three Euler angle rotational velocities in the local reference frame.
        """ # nopep8
        return self._cards[1].get_value("alphal")

    @alphal.setter
    def alphal(self, value: int) -> None:
        self._cards[1].set_value("alphal", value)

    @property
    def betal(self) -> typing.Optional[int]:
        """Get or set the LCID for the three Euler angle rotational velocities in the local reference frame.
        """ # nopep8
        return self._cards[1].get_value("betal")

    @betal.setter
    def betal(self, value: int) -> None:
        self._cards[1].set_value("betal", value)

    @property
    def gammal(self) -> typing.Optional[int]:
        """Get or set the LCID for the three Euler angle rotational velocities in the local reference frame.
        """ # nopep8
        return self._cards[1].get_value("gammal")

    @gammal.setter
    def gammal(self, value: int) -> None:
        self._cards[1].set_value("gammal", value)

    @property
    def alphag(self) -> typing.Optional[int]:
        """Get or set the LCID for the three Euler angle rotational velocities in the global reference frame.
        """ # nopep8
        return self._cards[1].get_value("alphag")

    @alphag.setter
    def alphag(self, value: int) -> None:
        self._cards[1].set_value("alphag", value)

    @property
    def betag(self) -> typing.Optional[int]:
        """Get or set the LCID for the three Euler angle rotational velocities in the global reference frame.
        """ # nopep8
        return self._cards[1].get_value("betag")

    @betag.setter
    def betag(self, value: int) -> None:
        self._cards[1].set_value("betag", value)

    @property
    def gammag(self) -> typing.Optional[int]:
        """Get or set the LCID for the three Euler angle rotational velocities in the global reference frame.
        """ # nopep8
        return self._cards[1].get_value("gammag")

    @gammag.setter
    def gammag(self, value: int) -> None:
        self._cards[1].set_value("gammag", value)

    @property
    def vadr(self) -> int:
        """Get or set the Velocity/Displacements flag for rotation components
        EQ.0:Prescribe Velocity
        EQ.1:Prescribe Displacements
        """ # nopep8
        return self._cards[1].get_value("vadr")

    @vadr.setter
    def vadr(self, value: int) -> None:
        self._cards[1].set_value("vadr", value)

    @property
    def ptid(self) -> int:
        """Get or set the Point ID for the origin of the local reference frame.If not defined, the barycenter of the volume mesh will be used.
        """ # nopep8
        return self._cards[2].get_value("ptid")

    @ptid.setter
    def ptid(self, value: int) -> None:
        self._cards[2].set_value("ptid", value)

    @property
    def x1(self) -> float:
        """Get or set the Three components of the local reference X1 axis./n If not defined, the global x axis will be used.
        """ # nopep8
        return self._cards[2].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        self._cards[2].set_value("x1", value)

    @property
    def y1(self) -> float:
        """Get or set the Three components of the local reference X1 axis./n If not defined, the global x axis will be used.
        """ # nopep8
        return self._cards[2].get_value("y1")

    @y1.setter
    def y1(self, value: float) -> None:
        self._cards[2].set_value("y1", value)

    @property
    def z1(self) -> float:
        """Get or set the Three components of the local reference X1 axis./n If not defined, the global x axis will be used.
        """ # nopep8
        return self._cards[2].get_value("z1")

    @z1.setter
    def z1(self, value: float) -> None:
        self._cards[2].set_value("z1", value)

    @property
    def x2(self) -> float:
        """Get or set the Three components of the local reference X2 axis./n If not defined, the global y axis will be used.
        """ # nopep8
        return self._cards[2].get_value("x2")

    @x2.setter
    def x2(self, value: float) -> None:
        self._cards[2].set_value("x2", value)

    @property
    def y2(self) -> float:
        """Get or set the Three components of the local reference X2 axis./n If not defined, the global y axis will be used.
        """ # nopep8
        return self._cards[2].get_value("y2")

    @y2.setter
    def y2(self, value: float) -> None:
        self._cards[2].set_value("y2", value)

    @property
    def z2(self) -> float:
        """Get or set the Three components of the local reference X2 axis./n If not defined, the global y axis will be used.
        """ # nopep8
        return self._cards[2].get_value("z2")

    @z2.setter
    def z2(self, value: float) -> None:
        self._cards[2].set_value("z2", value)

    @property
    def ptido(self) -> int:
        """Get or set the Point ID (See ICFD_DEFINE_POINT) for the center of rotation.
        """ # nopep8
        return self._cards[3].get_value("ptido")

    @ptido.setter
    def ptido(self, value: int) -> None:
        self._cards[3].set_value("ptido", value)

    @property
    def axe(self) -> int:
        """Get or set the Rotation axis (X=1, Y=2, Z=3).
        """ # nopep8
        return self._cards[3].get_value("axe")

    @axe.setter
    def axe(self, value: int) -> None:
        self._cards[3].set_value("axe", value)

    @property
    def ptidv(self) -> int:
        """Get or set the Point ID (See ICFD_DEFINE_POINT) for the rotation velocity. If point is static, no rotation will occur.
        """ # nopep8
        return self._cards[3].get_value("ptidv")

    @ptidv.setter
    def ptidv(self, value: int) -> None:
        self._cards[3].set_value("ptidv", value)

