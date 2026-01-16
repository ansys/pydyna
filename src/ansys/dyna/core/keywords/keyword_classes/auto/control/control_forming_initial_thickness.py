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

"""Module providing the ControlFormingInitialThickness class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_CONTROLFORMINGINITIALTHICKNESS_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("lcid", int, 10, 10, None),
    FieldSchema("x0", float, 20, 10, None),
    FieldSchema("y0", float, 30, 10, None),
    FieldSchema("z0x", float, 40, 10, None),
    FieldSchema("vx", float, 50, 10, None),
    FieldSchema("vy", float, 60, 10, None),
    FieldSchema("vz", float, 70, 10, None),
)

class ControlFormingInitialThickness(KeywordBase):
    """DYNA CONTROL_FORMING_INITIAL_THICKNESS keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_INITIAL_THICKNESS"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ControlFormingInitialThickness class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLFORMINGINITIALTHICKNESS_CARD0,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the sheet blank to be defined with varying thickness, as in *PART.  Currently only 1 PID is allowed.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining thickness (Y-values) vs. distance (X-values) starting from position coordinates (X0, Y0, Z0) and in the direction of a vector [VX, VY, VZ], as in *DEFINE_CURVE.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def x0(self) -> typing.Optional[float]:
        """Get or set the Starting position coordinates.
        """ # nopep8
        return self._cards[0].get_value("x0")

    @x0.setter
    def x0(self, value: float) -> None:
        """Set the x0 property."""
        self._cards[0].set_value("x0", value)

    @property
    def y0(self) -> typing.Optional[float]:
        """Get or set the Starting position coordinates.
        """ # nopep8
        return self._cards[0].get_value("y0")

    @y0.setter
    def y0(self, value: float) -> None:
        """Set the y0 property."""
        self._cards[0].set_value("y0", value)

    @property
    def z0x(self) -> typing.Optional[float]:
        """Get or set the Starting position coordinates.
        """ # nopep8
        return self._cards[0].get_value("z0x")

    @z0x.setter
    def z0x(self, value: float) -> None:
        """Set the z0x property."""
        self._cards[0].set_value("z0x", value)

    @property
    def vx(self) -> typing.Optional[float]:
        """Get or set the Vector components defining the direction of the distance in the load curve
        """ # nopep8
        return self._cards[0].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[0].set_value("vx", value)

    @property
    def vy(self) -> typing.Optional[float]:
        """Get or set the Vector components defining the direction of the distance in the load curve
        """ # nopep8
        return self._cards[0].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[0].set_value("vy", value)

    @property
    def vz(self) -> typing.Optional[float]:
        """Get or set the Vector components defining the direction of the distance in the load curve
        """ # nopep8
        return self._cards[0].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[0].set_value("vz", value)

    @property
    def lcid_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid:
                return kwd
        return None

    @lcid_link.setter
    def lcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid."""
        self.lcid = value.lcid

    @property
    def pid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

