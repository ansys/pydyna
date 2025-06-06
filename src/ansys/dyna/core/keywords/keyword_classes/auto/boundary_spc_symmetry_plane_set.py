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

"""Module providing the BoundarySpcSymmetryPlaneSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class BoundarySpcSymmetryPlaneSet(KeywordBase):
    """DYNA BOUNDARY_SPC_SYMMETRY_PLANE_SET keyword"""

    keyword = "BOUNDARY"
    subkeyword = "SPC_SYMMETRY_PLANE_SET"

    def __init__(self, **kwargs):
        """Initialize the BoundarySpcSymmetryPlaneSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "idsp",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "psid",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "x",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "y",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "z",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "vx",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "vy",
                        float,
                        60,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "vz",
                        float,
                        70,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "tol",
                        float,
                        0,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
        ]

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
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part set ID of the deformable part set (sheet metal blank, for example) on which the constraints will be imposed.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

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

