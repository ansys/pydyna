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

"""Module providing the DualceseBoundarySolidWallMsurf class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEBOUNDARYSOLIDWALLMSURF_CARD0 = (
    FieldSchema("mspid", int, 0, 10, None),
    FieldSchema("lcid", int, 10, 10, None),
    FieldSchema("vx", float, 20, 10, None),
    FieldSchema("vy", float, 30, 10, None),
    FieldSchema("vz", float, 40, 10, None),
)

class DualceseBoundarySolidWallMsurf(KeywordBase):
    """DYNA DUALCESE_BOUNDARY_SOLID_WALL_MSURF keyword"""

    keyword = "DUALCESE"
    subkeyword = "BOUNDARY_SOLID_WALL_MSURF"

    def __init__(self, **kwargs):
        """Initialize the DualceseBoundarySolidWallMsurf class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEBOUNDARYSOLIDWALLMSURF_CARD0,
                **kwargs,
            ),        ]
    @property
    def mspid(self) -> typing.Optional[int]:
        """Get or set the Mesh surface part ID that is referenced by *MESH_SURFACE_ELEMENT cards
        """ # nopep8
        return self._cards[0].get_value("mspid")

    @mspid.setter
    def mspid(self, value: int) -> None:
        """Set the mspid property."""
        self._cards[0].set_value("mspid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID scales the velocity vector specified with   to give the solid wall boundary movement. If not defined, the solid wall boundary moves with a constant velocity vector specified by VX,VY,VZ
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def vx(self) -> typing.Optional[float]:
        """Get or set the Velocity vector of the solid wall boundary condition:
        LCID.EQ.0:	Constant velocity vector specified with VX, VY,and VZ.
        LCID.NE.0 : VX, VY,and VZ give the velocity vector that is scaled by LCID.
        """ # nopep8
        return self._cards[0].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[0].set_value("vx", value)

    @property
    def vy(self) -> typing.Optional[float]:
        """Get or set the Velocity vector of the solid wall boundary condition:
        LCID.EQ.0:	Constant velocity vector specified with VX, VY,and VZ.
        LCID.NE.0 : VX, VY,and VZ give the velocity vector that is scaled by LCID.
        """ # nopep8
        return self._cards[0].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[0].set_value("vy", value)

    @property
    def vz(self) -> typing.Optional[float]:
        """Get or set the Velocity vector of the solid wall boundary condition:
        LCID.EQ.0:	Constant velocity vector specified with VX, VY,and VZ.
        LCID.NE.0 : VX, VY,and VZ give the velocity vector that is scaled by LCID.
        """ # nopep8
        return self._cards[0].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[0].set_value("vz", value)

