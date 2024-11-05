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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class InitialLagMappingWrite3Daxi(KeywordBase):
    """DYNA INITIAL_LAG_MAPPING_WRITE3DAXI keyword"""

    keyword = "INITIAL"
    subkeyword = "LAG_MAPPING_WRITE3DAXI"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "setid",
                        int,
                        0,
                        10,
                        kwargs.get("setid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xp",
                        float,
                        0,
                        10,
                        kwargs.get("xp", 0.0)
                    ),
                    Field(
                        "yp",
                        float,
                        10,
                        10,
                        kwargs.get("yp", 0.0)
                    ),
                    Field(
                        "zp",
                        float,
                        20,
                        10,
                        kwargs.get("zp", 0.0)
                    ),
                    Field(
                        "vecid",
                        int,
                        30,
                        10,
                        kwargs.get("vecid")
                    ),
                    Field(
                        "angle",
                        float,
                        40,
                        10,
                        kwargs.get("angle")
                    ),
                    Field(
                        "nelangl",
                        int,
                        50,
                        10,
                        kwargs.get("nelangl")
                    ),
                ],
            ),
        ]

    @property
    def setid(self) -> typing.Optional[int]:
        """Get or set the part set ID
        """ # nopep8
        return self._cards[0].get_value("setid")

    @setid.setter
    def setid(self, value: int) -> None:
        self._cards[0].set_value("setid", value)

    @property
    def xp(self) -> float:
        """Get or set the x-position of a point belonging to the plane from which the 3D mesh is generated
        """ # nopep8
        return self._cards[1].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[1].set_value("xp", value)

    @property
    def yp(self) -> float:
        """Get or set the y-position of a point belonging to the plane from which the 3D mesh is generated
        """ # nopep8
        return self._cards[1].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[1].set_value("yp", value)

    @property
    def zp(self) -> float:
        """Get or set the z-position of a point belonging to the plane from which the 3D mesh is generated
        """ # nopep8
        return self._cards[1].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[1].set_value("zp", value)

    @property
    def vecid(self) -> typing.Optional[int]:
        """Get or set the ID of the symmetric axis defined by *DEFINE_VECTOR
        """ # nopep8
        return self._cards[1].get_value("vecid")

    @vecid.setter
    def vecid(self, value: int) -> None:
        self._cards[1].set_value("vecid", value)

    @property
    def angle(self) -> typing.Optional[float]:
        """Get or set the Angle of rotation around an axis defined by *DEFINE_VECTOR
        """ # nopep8
        return self._cards[1].get_value("angle")

    @angle.setter
    def angle(self, value: float) -> None:
        self._cards[1].set_value("angle", value)

    @property
    def nelangl(self) -> typing.Optional[int]:
        """Get or set the Mapping parameter.  See Remark 5.
        GT. 0:	For a 2D to 3D mapping, number of elements to create in the azimuthal direction for ANGLE
        EQ.-1:	No mesh is generated or projected.
        EQ.-2:	For a 3D to 3D mapping, ANGLE only rotates the data from the mapping file (not the current mesh).
        EQ.-3:	No mesh is generated or projected except that the boundary nodes of the current mesh are projected on the boundary faces of the previous mesh
        """ # nopep8
        return self._cards[1].get_value("nelangl")

    @nelangl.setter
    def nelangl(self, value: int) -> None:
        self._cards[1].set_value("nelangl", value)

