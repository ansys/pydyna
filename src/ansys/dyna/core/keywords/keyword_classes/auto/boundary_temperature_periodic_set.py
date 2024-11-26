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

class BoundaryTemperaturePeriodicSet(KeywordBase):
    """DYNA BOUNDARY_TEMPERATURE_PERIODIC_SET keyword"""

    keyword = "BOUNDARY"
    subkeyword = "TEMPERATURE_PERIODIC_SET"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ssid1",
                        int,
                        0,
                        10,
                        kwargs.get("ssid1")
                    ),
                    Field(
                        "ptype",
                        int,
                        10,
                        10,
                        kwargs.get("ptype")
                    ),
                    Field(
                        "ssid2",
                        int,
                        20,
                        10,
                        kwargs.get("ssid2")
                    ),
                    Field(
                        "tdlcid",
                        int,
                        30,
                        10,
                        kwargs.get("tdlcid")
                    ),
                    Field(
                        "axe",
                        int,
                        40,
                        10,
                        kwargs.get("axe")
                    ),
                    Field(
                        "nid",
                        int,
                        50,
                        10,
                        kwargs.get("nid")
                    ),
                    Field(
                        "angle",
                        float,
                        60,
                        10,
                        kwargs.get("angle")
                    ),
                ],
            ),
        ]

    @property
    def ssid1(self) -> typing.Optional[int]:
        """Get or set the First Segment set on which the periodic temperature boundary condition will be applied.
        """ # nopep8
        return self._cards[0].get_value("ssid1")

    @ssid1.setter
    def ssid1(self, value: int) -> None:
        self._cards[0].set_value("ssid1", value)

    @property
    def ptype(self) -> typing.Optional[int]:
        """Get or set the Type of periodic boundary condition:
        EQ.1:	Rotation boundary condition defined by an axis, an origin pointand a rotation angle.
        EQ.2 : Reflective boundary condition defined by an axis and origin point.
        EQ.3 : Sliding boundary condition.
        """ # nopep8
        return self._cards[0].get_value("ptype")

    @ptype.setter
    def ptype(self, value: int) -> None:
        self._cards[0].set_value("ptype", value)

    @property
    def ssid2(self) -> typing.Optional[int]:
        """Get or set the Second Segment set on which the periodic temperature boundary condition will be applied.
        """ # nopep8
        return self._cards[0].get_value("ssid2")

    @ssid2.setter
    def ssid2(self, value: int) -> None:
        self._cards[0].set_value("ssid2", value)

    @property
    def tdlcid(self) -> typing.Optional[int]:
        """Get or set the Optional load curve specifying the temperature drop, T_drop, between the two surfaces in the periodic boundary condition as a function of time. Note that T_drop =T_1-T_2 where T_1 is the temperature of the surface specified with SSID1 and T_2 is the temperature of the surface specified with SSID2.
        EQ.0:	No temperature drop between that surfacs, that is, T_drop = 0.0
        """ # nopep8
        return self._cards[0].get_value("tdlcid")

    @tdlcid.setter
    def tdlcid(self, value: int) -> None:
        self._cards[0].set_value("tdlcid", value)

    @property
    def axe(self) -> typing.Optional[int]:
        """Get or set the Axis for Ptype=1 or 2 EQ.1:	X-axis
        EQ.2:	Y - axis
        EQ.3 : Z - axis.
        Flag for meaning of ANGLE for PTYPE = 3. Setting AXE = 1 means that ANGLE is the contact distance. Otherwise, it is a scale factor on the contact distance search
        """ # nopep8
        return self._cards[0].get_value("axe")

    @axe.setter
    def axe(self, value: int) -> None:
        self._cards[0].set_value("axe", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID giving the origin point coordinates
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[0].set_value("nid", value)

    @property
    def angle(self) -> typing.Optional[float]:
        """Get or set the Rotation angle if PTYPE=1. Scaling factor on contact distance search if PTYPE=3 (default applies a
        scale factor of 0.3 on local element size). If AXE=1 and PTYPE=3, then ANGLE becomes the contact distance
        """ # nopep8
        return self._cards[0].get_value("angle")

    @angle.setter
    def angle(self, value: float) -> None:
        self._cards[0].set_value("angle", value)

