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

"""Module providing the IcfdInitial class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDINITIAL_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("vx", float, 10, 10, None),
    FieldSchema("vy", float, 20, 10, None),
    FieldSchema("vz", float, 30, 10, None),
    FieldSchema("t", float, 40, 10, None),
    FieldSchema("p", float, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("dfunc", int, 70, 10, 0),
)

class IcfdInitial(KeywordBase):
    """DYNA ICFD_INITIAL keyword"""

    keyword = "ICFD"
    subkeyword = "INITIAL"

    def __init__(self, **kwargs):
        """Initialize the IcfdInitial class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDINITIAL_CARD0,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID for the volume elements or the surface elements where the values are initialized (see *ICFD_PART_VOL and *ICFD_PART).PID = 0 to assign the initial condition to all nodes at once.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def vx(self) -> typing.Optional[float]:
        """Get or set the x coordinate for the velocity.
        """ # nopep8
        return self._cards[0].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[0].set_value("vx", value)

    @property
    def vy(self) -> typing.Optional[float]:
        """Get or set the y coordinate for the velocity.
        """ # nopep8
        return self._cards[0].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[0].set_value("vy", value)

    @property
    def vz(self) -> typing.Optional[float]:
        """Get or set the z coordinate for the velocity.
        """ # nopep8
        return self._cards[0].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[0].set_value("vz", value)

    @property
    def t(self) -> typing.Optional[float]:
        """Get or set the Initial temperature.
        """ # nopep8
        return self._cards[0].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        """Set the t property."""
        self._cards[0].set_value("t", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the Initial Pressure.
        """ # nopep8
        return self._cards[0].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        """Set the p property."""
        self._cards[0].set_value("p", value)

    @property
    def dfunc(self) -> int:
        """Get or set the Option to define initial conditions using *DEFINE_FUNCTION
        EQ.0:	Turned off.
        EQ.1:	Turned on. All previous flags for initial velocity, pressure and temperature now refer to *DEFINE_FUNCTION IDs. The following parameters are allowed : f(x,y,z), allowing to define initial profiles function of coordinates.
        """ # nopep8
        return self._cards[0].get_value("dfunc")

    @dfunc.setter
    def dfunc(self, value: int) -> None:
        """Set the dfunc property."""
        if value not in [0, 1, None]:
            raise Exception("""dfunc must be `None` or one of {0,1}.""")
        self._cards[0].set_value("dfunc", value)

