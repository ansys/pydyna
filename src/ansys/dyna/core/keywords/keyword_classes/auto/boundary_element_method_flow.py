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

class BoundaryElementMethodFlow(KeywordBase):
    """DYNA BOUNDARY_ELEMENT_METHOD_FLOW keyword"""

    keyword = "BOUNDARY"
    subkeyword = "ELEMENT_METHOD_FLOW"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ssid",
                        int,
                        0,
                        10,
                        kwargs.get("ssid")
                    ),
                    Field(
                        "vx",
                        float,
                        10,
                        10,
                        kwargs.get("vx")
                    ),
                    Field(
                        "vy",
                        float,
                        20,
                        10,
                        kwargs.get("vy")
                    ),
                    Field(
                        "vz",
                        float,
                        30,
                        10,
                        kwargs.get("vz")
                    ),
                    Field(
                        "ro",
                        float,
                        40,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "pstatic",
                        float,
                        50,
                        10,
                        kwargs.get("pstatic", 0.0)
                    ),
                    Field(
                        "mach",
                        float,
                        60,
                        10,
                        kwargs.get("mach", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Shell set ID for the set of shell elements which define the surface of the bodies of interest (see *SET_SHELL). The nodes of these shells should be ordered so that the shell normals point into the fluid.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        self._cards[0].set_value("ssid", value)

    @property
    def vx(self) -> typing.Optional[float]:
        """Get or set the x-component of the free-stream fluid velocity.
        """ # nopep8
        return self._cards[0].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        self._cards[0].set_value("vx", value)

    @property
    def vy(self) -> typing.Optional[float]:
        """Get or set the y-component of the free-stream fluid velocity.
        """ # nopep8
        return self._cards[0].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        self._cards[0].set_value("vy", value)

    @property
    def vz(self) -> typing.Optional[float]:
        """Get or set the z-component of the free-stream fluid velocity.
        """ # nopep8
        return self._cards[0].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        self._cards[0].set_value("vz", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Fluid density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def pstatic(self) -> float:
        """Get or set the Fluid static pressure.
        """ # nopep8
        return self._cards[0].get_value("pstatic")

    @pstatic.setter
    def pstatic(self, value: float) -> None:
        self._cards[0].set_value("pstatic", value)

    @property
    def mach(self) -> float:
        """Get or set the Free-stream Mach number.
        """ # nopep8
        return self._cards[0].get_value("mach")

    @mach.setter
    def mach(self, value: float) -> None:
        self._cards[0].set_value("mach", value)

