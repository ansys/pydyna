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

"""Module providing the BoundarySpcSymmetryPlane class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_BOUNDARYSPCSYMMETRYPLANE_CARD0 = (
    FieldSchema("idsp", int, 0, 10, None),
    FieldSchema("pid", int, 10, 10, None),
    FieldSchema("x", float, 20, 10, 0.0),
    FieldSchema("y", float, 30, 10, 0.0),
    FieldSchema("z", float, 40, 10, 0.0),
    FieldSchema("vx", float, 50, 10, 0.0),
    FieldSchema("vy", float, 60, 10, 0.0),
    FieldSchema("vz", float, 70, 10, 0.0),
)

_BOUNDARYSPCSYMMETRYPLANE_CARD1 = (
    FieldSchema("tol", float, 0, 10, 0.0),
)

class BoundarySpcSymmetryPlane(KeywordBase):
    """DYNA BOUNDARY_SPC_SYMMETRY_PLANE keyword"""

    keyword = "BOUNDARY"
    subkeyword = "SPC_SYMMETRY_PLANE"
    _link_fields = {
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the BoundarySpcSymmetryPlane class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYSPCSYMMETRYPLANE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _BOUNDARYSPCSYMMETRYPLANE_CARD1,
                **kwargs,
            ),        ]
    @property
    def idsp(self) -> typing.Optional[int]:
        """Get or set the Identification number of the constraint. Must be unique.
        """ # nopep8
        return self._cards[0].get_value("idsp")

    @idsp.setter
    def idsp(self, value: int) -> None:
        """Set the idsp property."""
        self._cards[0].set_value("idsp", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the deformable part (sheet metal blank, for example) on which the constraints will be imposed.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def x(self) -> float:
        """Get or set the Position coordinates on the symmetry plane.
        """ # nopep8
        return self._cards[0].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        """Set the x property."""
        self._cards[0].set_value("x", value)

    @property
    def y(self) -> float:
        """Get or set the Position coordinates on the symmetry plane.
        """ # nopep8
        return self._cards[0].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[0].set_value("y", value)

    @property
    def z(self) -> float:
        """Get or set the Position coordinates on the symmetry plane.
        """ # nopep8
        return self._cards[0].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        """Set the z property."""
        self._cards[0].set_value("z", value)

    @property
    def vx(self) -> float:
        """Get or set the Vector components of the symmetry plane normal.
        """ # nopep8
        return self._cards[0].get_value("vx")

    @vx.setter
    def vx(self, value: float) -> None:
        """Set the vx property."""
        self._cards[0].set_value("vx", value)

    @property
    def vy(self) -> float:
        """Get or set the Vector components of the symmetry plane normal.
        """ # nopep8
        return self._cards[0].get_value("vy")

    @vy.setter
    def vy(self, value: float) -> None:
        """Set the vy property."""
        self._cards[0].set_value("vy", value)

    @property
    def vz(self) -> float:
        """Get or set the Vector components of the symmetry plane normal.
        """ # nopep8
        return self._cards[0].get_value("vz")

    @vz.setter
    def vz(self, value: float) -> None:
        """Set the vz property."""
        self._cards[0].set_value("vz", value)

    @property
    def tol(self) -> float:
        """Get or set the A distance tolerance value within which the nodes on the deformable part will be constrained.For shell elements, the default tolerance is 0.2.
        """ # nopep8
        return self._cards[1].get_value("tol")

    @tol.setter
    def tol(self, value: float) -> None:
        """Set the tol property."""
        self._cards[1].set_value("tol", value)

    @property
    def pid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

