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

class InitialDetonationGeometry(KeywordBase):
    """DYNA INITIAL_DETONATION_GEOMETRY keyword"""

    keyword = "INITIAL"
    subkeyword = "DETONATION_GEOMETRY"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "heid",
                        int,
                        0,
                        10,
                        kwargs.get("heid")
                    ),
                    Field(
                        "hetyp",
                        float,
                        10,
                        10,
                        kwargs.get("hetyp", 0)
                    ),
                    Field(
                        "mmgse",
                        float,
                        20,
                        10,
                        kwargs.get("mmgse", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "geotyp",
                        int,
                        0,
                        10,
                        kwargs.get("geotyp", 0)
                    ),
                    Field(
                        "lt",
                        float,
                        10,
                        10,
                        kwargs.get("lt", 0.0)
                    ),
                    Field(
                        "dgeo",
                        float,
                        20,
                        10,
                        kwargs.get("dgeo", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "v1",
                        int,
                        0,
                        10,
                        kwargs.get("v1")
                    ),
                    Field(
                        "v2",
                        int,
                        10,
                        10,
                        kwargs.get("v2")
                    ),
                    Field(
                        "v3",
                        int,
                        20,
                        10,
                        kwargs.get("v3")
                    ),
                    Field(
                        "v4",
                        int,
                        30,
                        10,
                        kwargs.get("v4")
                    ),
                ],
            ),
        ]

    @property
    def heid(self) -> typing.Optional[int]:
        """Get or set the ID specifying the high explosives to be lit
        """ # nopep8
        return self._cards[0].get_value("heid")

    @heid.setter
    def heid(self, value: int) -> None:
        self._cards[0].set_value("heid", value)

    @property
    def hetyp(self) -> float:
        """Get or set the Type of HEID :
        EQ.0:	Part set(*SET_PART)
        """ # nopep8
        return self._cards[0].get_value("hetyp")

    @hetyp.setter
    def hetyp(self, value: float) -> None:
        self._cards[0].set_value("hetyp", value)

    @property
    def mmgse(self) -> float:
        """Get or set the ID of *SET_‌MULTI-MATERIAL_‌GROUP_LIST selecting the explosive ALE groups to be lit
        """ # nopep8
        return self._cards[0].get_value("mmgse")

    @mmgse.setter
    def mmgse(self, value: float) -> None:
        self._cards[0].set_value("mmgse", value)

    @property
    def geotyp(self) -> int:
        """Get or set the Type of geometry formed by the detonation points:
        EQ.1: Plane
        """ # nopep8
        return self._cards[1].get_value("geotyp")

    @geotyp.setter
    def geotyp(self, value: int) -> None:
        self._cards[1].set_value("geotyp", value)

    @property
    def lt(self) -> float:
        """Get or set the Lighting time for detonation point
        """ # nopep8
        return self._cards[1].get_value("lt")

    @lt.setter
    def lt(self, value: float) -> None:
        self._cards[1].set_value("lt", value)

    @property
    def dgeo(self) -> float:
        """Get or set the Maximum distance from the detonation geometry for determining which HE elements become detonation points. If the element center for the specified HE is less than this distance away from the detonation geometry, then the element center becomes detonation point. If zero or undefined, DGEO becomes the length of the largest specified HE element.
        """ # nopep8
        return self._cards[1].get_value("dgeo")

    @dgeo.setter
    def dgeo(self, value: float) -> None:
        self._cards[1].set_value("dgeo", value)

    @property
    def v1(self) -> typing.Optional[int]:
        """Get or set the IDs of vectors (*DEFINE_VECTOR) for specifying orientation and location of the geometry selected with GEOTYP.
        GEOTYP.EQ.1:	V1 is the plane’s normal vector.The tail of V1 is a point in this plane.V2, V3,and V4 are unused
        """ # nopep8
        return self._cards[2].get_value("v1")

    @v1.setter
    def v1(self, value: int) -> None:
        self._cards[2].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[int]:
        """Get or set the IDs of vectors (*DEFINE_VECTOR) for specifying orientation and location of the geometry selected with GEOTYP.
        GEOTYP.EQ.1:	V1 is the plane’s normal vector.The tail of V1 is a point in this plane.V2, V3,and V4 are unused
        """ # nopep8
        return self._cards[2].get_value("v2")

    @v2.setter
    def v2(self, value: int) -> None:
        self._cards[2].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[int]:
        """Get or set the IDs of vectors (*DEFINE_VECTOR) for specifying orientation and location of the geometry selected with GEOTYP.
        GEOTYP.EQ.1:	V1 is the plane’s normal vector.The tail of V1 is a point in this plane.V2, V3,and V4 are unused
        """ # nopep8
        return self._cards[2].get_value("v3")

    @v3.setter
    def v3(self, value: int) -> None:
        self._cards[2].set_value("v3", value)

    @property
    def v4(self) -> typing.Optional[int]:
        """Get or set the IDs of vectors (*DEFINE_VECTOR) for specifying orientation and location of the geometry selected with GEOTYP.
        GEOTYP.EQ.1:	V1 is the plane’s normal vector.The tail of V1 is a point in this plane.V2, V3,and V4 are unused
        """ # nopep8
        return self._cards[2].get_value("v4")

    @v4.setter
    def v4(self, value: int) -> None:
        self._cards[2].set_value("v4", value)

