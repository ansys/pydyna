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

"""Module providing the IcfdInitialLevelset class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class IcfdInitialLevelset(KeywordBase):
    """DYNA ICFD_INITIAL_LEVELSET keyword"""

    keyword = "ICFD"
    subkeyword = "INITIAL_LEVELSET"

    def __init__(self, **kwargs):
        """Initialize the IcfdInitialLevelset class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "stype",
                        int,
                        0,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "nx",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "ny",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "nz",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "x",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "y",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "z",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "invert",
                        int,
                        70,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def stype(self) -> int:
        """Get or set the Initial surface type :
        EQ.0/1: :	Generated by a section plane
        EQ.2:	Generated by a box. See Remark 1.
        EQ.3: Generated by a sphere.
        EQ.4: Generated by cylinder
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        """Set the stype property."""
        if value not in [0, 1, 2, 3, 4, None]:
            raise Exception("""stype must be `None` or one of {0,1,2,3,4}.""")
        self._cards[0].set_value("stype", value)

    @property
    def nx(self) -> typing.Optional[float]:
        """Get or set the X components of the section plane normal if STYPE=1. PMIN coordinates if STYPE=2. NX is the sphere/cylinder radius if STYPE=3.and STYPE = 4. NY is the cylinder length if STYPE = 4. NZ becomes the global axis if STYPE = 4 (X=1, Y=2, Z=3).
        """ # nopep8
        return self._cards[0].get_value("nx")

    @nx.setter
    def nx(self, value: float) -> None:
        """Set the nx property."""
        self._cards[0].set_value("nx", value)

    @property
    def ny(self) -> typing.Optional[float]:
        """Get or set the Y components of the section plane normal if STYPE=1. PMIN coordinates if STYPE=2. NX is the sphere radius if STYPE=3.
        """ # nopep8
        return self._cards[0].get_value("ny")

    @ny.setter
    def ny(self, value: float) -> None:
        """Set the ny property."""
        self._cards[0].set_value("ny", value)

    @property
    def nz(self) -> typing.Optional[float]:
        """Get or set the Z components of the section plane normal if STYPE=1. PMIN coordinates if STYPE=2. NX is the sphere radius if STYPE=3.
        """ # nopep8
        return self._cards[0].get_value("nz")

    @nz.setter
    def nz(self, value: float) -> None:
        """Set the nz property."""
        self._cards[0].set_value("nz", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the X components of the section plane origin point if STYPE=1. PMAX coordinates if STYPE=2. Sphere origin point coordinates if STYPE=3..
        """ # nopep8
        return self._cards[0].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        """Set the x property."""
        self._cards[0].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the Y components of the section plane origin point if STYPE=1. PMAX coordinates if STYPE=2. Sphere origin point coordinates if STYPE=3.
        """ # nopep8
        return self._cards[0].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[0].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the Z components of the section plane origin point if STYPE=1. PMAX coordinates if STYPE=2. Sphere origin point coordinates if STYPE=3.
        """ # nopep8
        return self._cards[0].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        """Set the z property."""
        self._cards[0].set_value("z", value)

    @property
    def invert(self) -> int:
        """Get or set the Inversion of initial levelset :
        EQ.0:	No inversion. Positive levelset values are assigned to nodes contained within the volume defined by STYPE.
        EQ.1:	The sign of the initial levelset values is reversed.
        """ # nopep8
        return self._cards[0].get_value("invert")

    @invert.setter
    def invert(self, value: int) -> None:
        """Set the invert property."""
        if value not in [0, 1, None]:
            raise Exception("""invert must be `None` or one of {0,1}.""")
        self._cards[0].set_value("invert", value)

