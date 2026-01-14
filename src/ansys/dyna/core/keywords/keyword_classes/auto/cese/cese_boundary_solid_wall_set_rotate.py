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

"""Module providing the CeseBoundarySolidWallSetRotate class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CESEBOUNDARYSOLIDWALLSETROTATE_CARD0 = (
    FieldSchema("ssid", int, 0, 10, None),
    FieldSchema("lcid", int, 10, 10, 0),
    FieldSchema("vx", float, 20, 10, 0.0),
    FieldSchema("vy", float, 30, 10, 0.0),
    FieldSchema("vz", float, 40, 10, 0.0),
    FieldSchema("nx", float, 50, 10, 0.0),
    FieldSchema("ny", float, 60, 10, 0.0),
    FieldSchema("nz", float, 70, 10, 0.0),
)

class CeseBoundarySolidWallSetRotate(KeywordBase):
    """DYNA CESE_BOUNDARY_SOLID_WALL_SET_ROTATE keyword"""

    keyword = "CESE"
    subkeyword = "BOUNDARY_SOLID_WALL_SET_ROTATE"

    def __init__(self, **kwargs):
        """Initialize the CeseBoundarySolidWallSetRotate class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CESEBOUNDARYSOLIDWALLSETROTATE_CARD0,
                **kwargs,
            ),        ]
    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set  ID.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[0].set_value("ssid", value)

    @property
    def lcid(self) -> int:
        """Get or set the Load curve ID to define this solid wall boundary movement.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def vx(self) -> float:
        """Get or set the x-,y- & z-coordinates of a point in the rotating axis.
        """ # nopep8
        return self._cards[0].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[0].set_value("vx", value)

    @property
    def vy(self) -> float:
        """Get or set the x-,y- & z-coordinates of a point in the rotating axis.
        """ # nopep8
        return self._cards[0].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[0].set_value("vy", value)

    @property
    def vz(self) -> float:
        """Get or set the x-,y- & z-coordinates of a point in the rotating axis.
        """ # nopep8
        return self._cards[0].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[0].set_value("vz", value)

    @property
    def nx(self) -> float:
        """Get or set the Unit vector of the rotating axis (for 2D case, this is not used) The rotating frequency (Hz) is given by the load curve.
        """ # nopep8
        return self._cards[0].get_value("nx")

    @nx.setter
    def nx(self, value: float) -> None:
        """Set the nx property."""
        self._cards[0].set_value("nx", value)

    @property
    def ny(self) -> float:
        """Get or set the Unit vector of the rotating axis (for 2D case, this is not used) The rotating frequency (Hz) is given by the load curve.
        """ # nopep8
        return self._cards[0].get_value("ny")

    @ny.setter
    def ny(self, value: float) -> None:
        """Set the ny property."""
        self._cards[0].set_value("ny", value)

    @property
    def nz(self) -> float:
        """Get or set the Unit vector of the rotating axis (for 2D case, this is not used) The rotating frequency (Hz) is given by the load curve.
        """ # nopep8
        return self._cards[0].get_value("nz")

    @nz.setter
    def nz(self, value: float) -> None:
        """Set the nz property."""
        self._cards[0].set_value("nz", value)

