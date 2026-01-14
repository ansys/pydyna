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

"""Module providing the IcfdBoundaryNavierslip class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_ICFDBOUNDARYNAVIERSLIP_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("fric", float, 0, 10, None),
)

class IcfdBoundaryNavierslip(KeywordBase):
    """DYNA ICFD_BOUNDARY_NAVIERSLIP keyword"""

    keyword = "ICFD"
    subkeyword = "BOUNDARY_NAVIERSLIP"
    _link_fields = {
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the IcfdBoundaryNavierslip class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDBOUNDARYNAVIERSLIP_CARD0,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the PID of the fluid surface where non-slip boundary condition is applied.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def fric(self) -> typing.Optional[float]:
        """Get or set the Friction coefficient. If a negative value is entered, it will refer to a Load curve ID used to describe the friction coefficient  value versus time, see *DEFINE_CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION.  If a DEFINE_FUNCTION is used, the following parameters are allowed: f(x,y,z,vx,vy,vz,temp,pres,time).
        """ # nopep8
        return self._cards[0].get_value("fric")

    @fric.setter
    def fric(self, value: float) -> None:
        """Set the fric property."""
        self._cards[0].set_value("fric", value)

    @property
    def pid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

