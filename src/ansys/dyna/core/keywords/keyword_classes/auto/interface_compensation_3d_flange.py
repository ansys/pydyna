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

class InterfaceCompensation3DFlange(KeywordBase):
    """DYNA INTERFACE_COMPENSATION_3D_FLANGE keyword"""

    keyword = "INTERFACE"
    subkeyword = "COMPENSATION_3D_FLANGE"

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
                        "vx",
                        float,
                        0,
                        10,
                        kwargs.get("vx")
                    ),
                    Field(
                        "vy",
                        float,
                        0,
                        10,
                        kwargs.get("vy")
                    ),
                    Field(
                        "vz",
                        float,
                        0,
                        10,
                        kwargs.get("vz")
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the The part ID of the flanging steel to be compensated
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def vx(self) -> typing.Optional[float]:
        """Get or set the The vector components of the flanging steel�s (PID) moving direction.
        """ # nopep8
        return self._cards[0].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        self._cards[0].set_value("vx", value)

    @property
    def vy(self) -> typing.Optional[float]:
        """Get or set the The vector components of the flanging steel�s (PID) moving direction.
        """ # nopep8
        return self._cards[0].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        self._cards[0].set_value("vy", value)

    @property
    def vz(self) -> typing.Optional[float]:
        """Get or set the The vector components of the flanging steel�s (PID) moving direction.
        """ # nopep8
        return self._cards[0].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        self._cards[0].set_value("vz", value)

