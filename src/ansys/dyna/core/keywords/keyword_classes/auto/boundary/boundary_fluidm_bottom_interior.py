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

"""Module providing the BoundaryFluidmBottomInterior class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BOUNDARYFLUIDMBOTTOMINTERIOR_CARD0 = (
    FieldSchema("tname", str, 0, 80, None),
)

_BOUNDARYFLUIDMBOTTOMINTERIOR_CARD1 = (
    FieldSchema("fstype", int, 0, 10, None),
    FieldSchema("ieigen", int, 10, 10, 0),
    FieldSchema("ssid", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("ssidbf", int, 40, 10, 0),
    FieldSchema("besimf", int, 50, 10, 0),
)

_BOUNDARYFLUIDMBOTTOMINTERIOR_CARD2 = (
    FieldSchema("rhof", float, 0, 10, None),
    FieldSchema("unused", float, 10, 10, None),
    FieldSchema("distfs", float, 20, 10, None),
    FieldSchema("cxfs", float, 30, 10, 0.0),
    FieldSchema("cyfs", float, 40, 10, 0.0),
    FieldSchema("czfs", float, 50, 10, 0.0),
)

class BoundaryFluidmBottomInterior(KeywordBase):
    """DYNA BOUNDARY_FLUIDM_BOTTOM_INTERIOR keyword"""

    keyword = "BOUNDARY"
    subkeyword = "FLUIDM_BOTTOM_INTERIOR"

    def __init__(self, **kwargs):
        """Initialize the BoundaryFluidmBottomInterior class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYFLUIDMBOTTOMINTERIOR_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BOUNDARYFLUIDMBOTTOMINTERIOR_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _BOUNDARYFLUIDMBOTTOMINTERIOR_CARD2,
                **kwargs,
            ),
        ]
    @property
    def tname(self) -> typing.Optional[str]:
        """Get or set the Name of the tank or volume. If forming a fluid mass superelement, this name is the file name.
        """ # nopep8
        return self._cards[0].get_value("tname")

    @tname.setter
    def tname(self, value: str) -> None:
        """Set the tname property."""
        self._cards[0].set_value("tname", value)

    @property
    def fstype(self) -> typing.Optional[int]:
        """Get or set the Free surface type:
        EQ.2: A quiescent zero pressure surface located in space by DISTFS, CXFS, CYFS,and CZFS.
        """ # nopep8
        return self._cards[1].get_value("fstype")

    @fstype.setter
    def fstype(self, value: int) -> None:
        """Set the fstype property."""
        self._cards[1].set_value("fstype", value)

    @property
    def ieigen(self) -> int:
        """Get or set the Eigenvalue solution flag:
        EQ.0: Use the fluid mass directly to calculate the in - fluid structural modes of vibration.
        EQ.1: Write a superelement for the fluid - added mass matrix to file[TNAME].DMIG,and then stop the solution.This is only advisable for very small matrices.
        """ # nopep8
        return self._cards[1].get_value("ieigen")

    @ieigen.setter
    def ieigen(self, value: int) -> None:
        """Set the ieigen property."""
        if value not in [0, 1, None]:
            raise Exception("""ieigen must be `None` or one of {0,1}.""")
        self._cards[1].set_value("ieigen", value)

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID for the structural faces for which the fluid boundary mass is calculated, excluding the free surface
        """ # nopep8
        return self._cards[1].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[1].set_value("ssid", value)

    @property
    def ssidbf(self) -> int:
        """Get or set the Segment set ID for internal baffle faces having fluid on both sides of plate elements.
        """ # nopep8
        return self._cards[1].get_value("ssidbf")

    @ssidbf.setter
    def ssidbf(self, value: int) -> None:
        """Set the ssidbf property."""
        self._cards[1].set_value("ssidbf", value)

    @property
    def besimf(self) -> int:
        """Get or set the Boundary element solution method for internal fluid mass:
        EQ.0: LAPACK direct matrix solution(default)
        EQ.1: Block low rank(BLR) direct solution without pivoting
        EQ.2: Pseudo - block GMRES iterative solution
        """ # nopep8
        return self._cards[1].get_value("besimf")

    @besimf.setter
    def besimf(self, value: int) -> None:
        """Set the besimf property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""besimf must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("besimf", value)

    @property
    def rhof(self) -> typing.Optional[float]:
        """Get or set the Fluid mass density in the same units as the structural model
        """ # nopep8
        return self._cards[2].get_value("rhof")

    @rhof.setter
    def rhof(self, value: float) -> None:
        """Set the rhof property."""
        self._cards[2].set_value("rhof", value)

    @property
    def distfs(self) -> typing.Optional[float]:
        """Get or set the Perpendicular distance from the plane of the free surface to the origin of the global coordinate system
        """ # nopep8
        return self._cards[2].get_value("distfs")

    @distfs.setter
    def distfs(self, value: float) -> None:
        """Set the distfs property."""
        self._cards[2].set_value("distfs", value)

    @property
    def cxfs(self) -> float:
        """Get or set the x-direction cosine for the normal vector of the free surface pointing into the air
        """ # nopep8
        return self._cards[2].get_value("cxfs")

    @cxfs.setter
    def cxfs(self, value: float) -> None:
        """Set the cxfs property."""
        self._cards[2].set_value("cxfs", value)

    @property
    def cyfs(self) -> float:
        """Get or set the y-direction cosine for the normal vector of the free surface pointing into the air
        """ # nopep8
        return self._cards[2].get_value("cyfs")

    @cyfs.setter
    def cyfs(self, value: float) -> None:
        """Set the cyfs property."""
        self._cards[2].set_value("cyfs", value)

    @property
    def czfs(self) -> float:
        """Get or set the z-direction cosine for the normal vector of the free surface pointing into the air
        """ # nopep8
        return self._cards[2].get_value("czfs")

    @czfs.setter
    def czfs(self, value: float) -> None:
        """Set the czfs property."""
        self._cards[2].set_value("czfs", value)

