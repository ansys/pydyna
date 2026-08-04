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

"""Module providing the DualceseBoundaryPrescribedPhaseChangeSegmentSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEBOUNDARYPRESCRIBEDPHASECHANGESEGMENTSET_CARD0 = (
    FieldSchema("ssid", int, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("dirx", float, 20, 10, None),
    FieldSchema("diry", float, 30, 10, None),
    FieldSchema("dirz", float, 40, 10, None),
)

_DUALCESEBOUNDARYPRESCRIBEDPHASECHANGESEGMENTSET_CARD1 = (
    FieldSchema("lcidu", int, 0, 10, None),
    FieldSchema("lcidv", int, 10, 10, None),
    FieldSchema("lcidw", int, 20, 10, None),
    FieldSchema("lcidrho", int, 30, 10, None),
    FieldSchema("lcidp", int, 40, 10, None),
    FieldSchema("lcidt", int, 50, 10, None),
    FieldSchema("lcidy1", int, 60, 10, None),
    FieldSchema("lcidy2", int, 70, 10, None),
)

_DUALCESEBOUNDARYPRESCRIBEDPHASECHANGESEGMENTSET_CARD2 = (
    FieldSchema("lcidy3", int, 0, 10, None),
)

class DualceseBoundaryPrescribedPhaseChangeSegmentSet(KeywordBase):
    """DYNA DUALCESE_BOUNDARY_PRESCRIBED_PHASE_CHANGE_SEGMENT_SET keyword"""

    keyword = "DUALCESE"
    subkeyword = "BOUNDARY_PRESCRIBED_PHASE_CHANGE_SEGMENT_SET"

    def __init__(self, **kwargs):
        """Initialize the DualceseBoundaryPrescribedPhaseChangeSegmentSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEBOUNDARYPRESCRIBEDPHASECHANGESEGMENTSET_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEBOUNDARYPRESCRIBEDPHASECHANGESEGMENTSET_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEBOUNDARYPRESCRIBEDPHASECHANGESEGMENTSET_CARD2,
                **kwargs,
            ),
        ]
    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID for the segment set created with *DUALCESE_SEGMENTSET or *DUALCESE_BLOCKMESH
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[0].set_value("ssid", value)

    @property
    def dirx(self) -> typing.Optional[float]:
        """Get or set the If nonzero, vector giving the prescribed flow direction
        """ # nopep8
        return self._cards[0].get_value("dirx")

    @dirx.setter
    def dirx(self, value: float) -> None:
        """Set the dirx property."""
        self._cards[0].set_value("dirx", value)

    @property
    def diry(self) -> typing.Optional[float]:
        """Get or set the If nonzero, vector giving the prescribed flow direction
        """ # nopep8
        return self._cards[0].get_value("diry")

    @diry.setter
    def diry(self, value: float) -> None:
        """Set the diry property."""
        self._cards[0].set_value("diry", value)

    @property
    def dirz(self) -> typing.Optional[float]:
        """Get or set the If nonzero, vector giving the prescribed flow direction
        """ # nopep8
        return self._cards[0].get_value("dirz")

    @dirz.setter
    def dirz(self, value: float) -> None:
        """Set the dirz property."""
        self._cards[0].set_value("dirz", value)

    @property
    def lcidu(self) -> typing.Optional[int]:
        """Get or set the Load curve or defined function ID to describe the xx-component of the velocity as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
        EQ.0: The x - component of velocity is a constant with value SFU.
        EQ. - 1: The x - component of velocity is computed by the solver.
        """ # nopep8
        return self._cards[1].get_value("lcidu")

    @lcidu.setter
    def lcidu(self, value: int) -> None:
        """Set the lcidu property."""
        self._cards[1].set_value("lcidu", value)

    @property
    def lcidv(self) -> typing.Optional[int]:
        """Get or set the Load curve or defined function ID to describe the yy-component of the velocity as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
        EQ.0: The y - component of velocity is a constant with value SFV.
        EQ. - 1: The y - component of velocity is computed by the solver.
        """ # nopep8
        return self._cards[1].get_value("lcidv")

    @lcidv.setter
    def lcidv(self, value: int) -> None:
        """Set the lcidv property."""
        self._cards[1].set_value("lcidv", value)

    @property
    def lcidw(self) -> typing.Optional[int]:
        """Get or set the Load curve or defined function ID to describe the zz-component of the velocity as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
        EQ.0: The z - component of velocity is a constant with value SFW.
        EQ. - 1: The z - component of velocity is computed by the solver.
        """ # nopep8
        return self._cards[1].get_value("lcidw")

    @lcidw.setter
    def lcidw(self, value: int) -> None:
        """Set the lcidw property."""
        self._cards[1].set_value("lcidw", value)

    @property
    def lcidrho(self) -> typing.Optional[int]:
        """Get or set the Load curve or defined function ID to describe the mixture density as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
        EQ.0: The density is a constant with value SFRHO.
        EQ. - 1: The density is computed by the solver.
        """ # nopep8
        return self._cards[1].get_value("lcidrho")

    @lcidrho.setter
    def lcidrho(self, value: int) -> None:
        """Set the lcidrho property."""
        self._cards[1].set_value("lcidrho", value)

    @property
    def lcidp(self) -> typing.Optional[int]:
        """Get or set the Load curve or defined function ID to describe the pressure as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
        EQ.0: The pressure is a constant with value SFP.
        EQ. - 1: The pressure is computed by the solver.
        """ # nopep8
        return self._cards[1].get_value("lcidp")

    @lcidp.setter
    def lcidp(self, value: int) -> None:
        """Set the lcidp property."""
        self._cards[1].set_value("lcidp", value)

    @property
    def lcidt(self) -> typing.Optional[int]:
        """Get or set the Load curve or defined function ID to describe the temperature as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
        EQ.0: The temperature is a constant with value SFT.
        EQ. - 1: The temperature is computed by the solver.
        """ # nopep8
        return self._cards[1].get_value("lcidt")

    @lcidt.setter
    def lcidt(self, value: int) -> None:
        """Set the lcidt property."""
        self._cards[1].set_value("lcidt", value)

    @property
    def lcidy1(self) -> typing.Optional[int]:
        """Get or set the Load curve or defined function ID to describe the mass fraction of material 1 as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
        EQ.0: The mass fraction is a constant with value SFY1.
        EQ. - 1: The mass fraction is computed by the solver.
        """ # nopep8
        return self._cards[1].get_value("lcidy1")

    @lcidy1.setter
    def lcidy1(self, value: int) -> None:
        """Set the lcidy1 property."""
        self._cards[1].set_value("lcidy1", value)

    @property
    def lcidy2(self) -> typing.Optional[int]:
        """Get or set the Load curve or defined function ID to describe the mass fraction of material 2 as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
        EQ.0: The mass fraction is a constant with value SFY2.
        EQ. - 1: The mass fraction is computed by the solver.
        """ # nopep8
        return self._cards[1].get_value("lcidy2")

    @lcidy2.setter
    def lcidy2(self, value: int) -> None:
        """Set the lcidy2 property."""
        self._cards[1].set_value("lcidy2", value)

    @property
    def lcidy3(self) -> typing.Optional[int]:
        """Get or set the Load curve or defined function ID to describe the mass fraction of material 3 as a function of time or a function of position, velocity, temperature, pressure, and time, f(x, y, z, vx, vy, vz, temp, pres, time), respectively.
        EQ.0: The mass fraction is a constant with value SFY3.
        EQ. - 1: The mass fraction is computed by the solver.
        """ # nopep8
        return self._cards[2].get_value("lcidy3")

    @lcidy3.setter
    def lcidy3(self, value: int) -> None:
        """Set the lcidy3 property."""
        self._cards[2].set_value("lcidy3", value)

