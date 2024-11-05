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

class BoundaryAleMapping(KeywordBase):
    """DYNA BOUNDARY_ALE_MAPPING keyword"""

    keyword = "BOUNDARY"
    subkeyword = "ALE_MAPPING"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id")
                    ),
                    Field(
                        "typ",
                        int,
                        10,
                        10,
                        kwargs.get("typ", 0)
                    ),
                    Field(
                        "ammsid",
                        int,
                        20,
                        10,
                        kwargs.get("ammsid")
                    ),
                    Field(
                        "ivoltyp",
                        int,
                        30,
                        10,
                        kwargs.get("ivoltyp")
                    ),
                    Field(
                        "birth",
                        float,
                        40,
                        10,
                        kwargs.get("birth", 0.0)
                    ),
                    Field(
                        "death",
                        float,
                        50,
                        10,
                        kwargs.get("death", 1e20)
                    ),
                    Field(
                        "dtout",
                        float,
                        60,
                        10,
                        kwargs.get("dtout")
                    ),
                    Field(
                        "ini",
                        int,
                        70,
                        10,
                        kwargs.get("ini", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "thick",
                        float,
                        0,
                        10,
                        kwargs.get("thick", 0.0)
                    ),
                    Field(
                        "radius",
                        float,
                        10,
                        10,
                        kwargs.get("radius", 0.0)
                    ),
                    Field(
                        "x1",
                        float,
                        20,
                        10,
                        kwargs.get("x1", 0.0)
                    ),
                    Field(
                        "y1",
                        float,
                        30,
                        10,
                        kwargs.get("y1", 0.0)
                    ),
                    Field(
                        "z1",
                        float,
                        40,
                        10,
                        kwargs.get("z1", 0.0)
                    ),
                    Field(
                        "x2",
                        float,
                        50,
                        10,
                        kwargs.get("x2", 0.0)
                    ),
                    Field(
                        "y2",
                        float,
                        60,
                        10,
                        kwargs.get("y2", 0.0)
                    ),
                    Field(
                        "z2",
                        float,
                        70,
                        10,
                        kwargs.get("z2", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x0",
                        float,
                        0,
                        10,
                        kwargs.get("x0", 0.0)
                    ),
                    Field(
                        "y0",
                        float,
                        10,
                        10,
                        kwargs.get("y0", 0.0)
                    ),
                    Field(
                        "z0",
                        float,
                        20,
                        10,
                        kwargs.get("z0", 0.0)
                    ),
                    Field(
                        "vecid",
                        int,
                        30,
                        10,
                        kwargs.get("vecid")
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Part ID or part set ID or element set ID
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def typ(self) -> int:
        """Get or set the Type of "ID" (see remark 1):
        EQ.0: part set ID.
        EQ.1: part ID.
        EQ.2: shell set ID.
        EQ.3: solid set ID.
        """ # nopep8
        return self._cards[0].get_value("typ")

    @typ.setter
    def typ(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""typ must be one of {0,1,2,3}""")
        self._cards[0].set_value("typ", value)

    @property
    def ammsid(self) -> typing.Optional[int]:
        """Get or set the Set ID of ALE multi-material groups defined in *SET_MULTI-MA-TERIAL_GROUP
        """ # nopep8
        return self._cards[0].get_value("ammsid")

    @ammsid.setter
    def ammsid(self, value: int) -> None:
        self._cards[0].set_value("ammsid", value)

    @property
    def ivoltyp(self) -> typing.Optional[int]:
        """Get or set the Type of volume containing the selected elements for the mapping.
        The absolute value of IVOLTYPE indicates the type of volume and
        the sign indicates whether the data is being read of written. Volume Type
        |IVOLTYP|.EQ.1: Spherical surface with thickness (THICK).
        |IVOLTYP|.EQ.2: Box.
        |IVOLTYP|.EQ.3: Cylindrical surface with thickness (THICK)
        |IVOLTYP|.EQ.4: All the elements defined by ID. Read/Write
        IVOLTYP.LT.0: data from the mapping file are read for the elements of this volume.
        IVOLTYP.GT.0: data from the elements of this volume are written in the mapping file.
        """ # nopep8
        return self._cards[0].get_value("ivoltyp")

    @ivoltyp.setter
    def ivoltyp(self, value: int) -> None:
        self._cards[0].set_value("ivoltyp", value)

    @property
    def birth(self) -> float:
        """Get or set the Birth time to write or read the mapping file. If a mapping file is
        written, the next run reading this file will begin at time BIRTH if this parameter for this next run is not larger.
        """ # nopep8
        return self._cards[0].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        self._cards[0].set_value("birth", value)

    @property
    def death(self) -> float:
        """Get or set the Death time to write or read the mapping file. If a mapping file is
        written, the next run will stop to read this file at time DEATH if this parameter for this next run is not smaller.
        """ # nopep8
        return self._cards[0].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        self._cards[0].set_value("death", value)

    @property
    def dtout(self) -> typing.Optional[float]:
        """Get or set the Time interval between outputs in the mapping file. This parameter
        is only used to write in the mapping file
        """ # nopep8
        return self._cards[0].get_value("dtout")

    @dtout.setter
    def dtout(self, value: float) -> None:
        self._cards[0].set_value("dtout", value)

    @property
    def ini(self) -> int:
        """Get or set the Flag to initialize all the ALE domain of the next run:
        EQ.0: No initialization
        EQ.1: Initialization. *INITIAL_ALE_MAPPING will have to be in
        the input deck of the next run to read the data from the
        mapping file. The initial time of the next run will be BIRTH
        """ # nopep8
        return self._cards[0].get_value("ini")

    @ini.setter
    def ini(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ini must be one of {0,1}""")
        self._cards[0].set_value("ini", value)

    @property
    def thick(self) -> float:
        """Get or set the Thickness for the element selection using surfaces.
        """ # nopep8
        return self._cards[1].get_value("thick")

    @thick.setter
    def thick(self, value: float) -> None:
        self._cards[1].set_value("thick", value)

    @property
    def radius(self) -> float:
        """Get or set the Radius for abs(IVOLTYP) = 1 and abs(IVOLTYP) = 2.
        """ # nopep8
        return self._cards[1].get_value("radius")

    @radius.setter
    def radius(self, value: float) -> None:
        self._cards[1].set_value("radius", value)

    @property
    def x1(self) -> float:
        """Get or set the If abs(IVOLTYP).EQ.1: X1 is the X-coordinate of the sphere center
        If abs(IVOLTYP).EQ.2: X1 is the X-coordinate of the boxs minimum point.
        If abs(IVOLTYP).EQ.3: X1 is the X-coordinate of a point on the cylinders axis.
        If abs(IVOLTYP).EQ.4: ignored
        """ # nopep8
        return self._cards[1].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        self._cards[1].set_value("x1", value)

    @property
    def y1(self) -> float:
        """Get or set the If abs(IVOLTYP).EQ.1: Y1 is the Y-coordinate of the sphere center
        If abs(IVOLTYP).EQ.2: Y1 is the Y-coordinate of the boxs minimum point.
        If abs(IVOLTYP).EQ.3: Y1 is the Y-coordinate of a point on the cylinders axis.
        If abs(IVOLTYP).EQ.4: ignored
        """ # nopep8
        return self._cards[1].get_value("y1")

    @y1.setter
    def y1(self, value: float) -> None:
        self._cards[1].set_value("y1", value)

    @property
    def z1(self) -> float:
        """Get or set the If abs(IVOLTYP).EQ.1: Z1 is the Z-coordinate of the sphere center
        If abs(IVOLTYP).EQ.2: Z1 is the Z-coordinate of the boxs minimum point.
        If abs(IVOLTYP).EQ.3: Z1 is the Z-coordinate of a point on the cylinders axis.
        If abs(IVOLTYP).EQ.4: ignored
        """ # nopep8
        return self._cards[1].get_value("z1")

    @z1.setter
    def z1(self, value: float) -> None:
        self._cards[1].set_value("z1", value)

    @property
    def x2(self) -> float:
        """Get or set the If abs(IVOLTYP).EQ.1: ignored
        If abs(IVOLTYP).EQ.2: X2 is the X-coordinate of the boxs maximum point.
        If abs(IVOLTYP).EQ.3: X2 is the X-coordinate of a vector parallel to the cylinders axis..
        If abs(IVOLTYP).EQ.4: ignored
        """ # nopep8
        return self._cards[1].get_value("x2")

    @x2.setter
    def x2(self, value: float) -> None:
        self._cards[1].set_value("x2", value)

    @property
    def y2(self) -> float:
        """Get or set the If abs(IVOLTYP).EQ.1: ignored
        If abs(IVOLTYP).EQ.2: Y2 is the Y-coordinate of the boxs maximum point.
        If abs(IVOLTYP).EQ.3: Y2 is the Y-coordinate of a vector parallel to the cylinders axis..
        If abs(IVOLTYP).EQ.4: ignored
        """ # nopep8
        return self._cards[1].get_value("y2")

    @y2.setter
    def y2(self, value: float) -> None:
        self._cards[1].set_value("y2", value)

    @property
    def z2(self) -> float:
        """Get or set the If abs(IVOLTYP).EQ.1: ignored
        If abs(IVOLTYP).EQ.2: Z2 is the X-coordinate of the boxs maximum point.
        If abs(IVOLTYP).EQ.3: Z2 is the Z-coordinate of a vector parallel to the cylinders axis..
        If abs(IVOLTYP).EQ.4: ignored
        """ # nopep8
        return self._cards[1].get_value("z2")

    @z2.setter
    def z2(self, value: float) -> None:
        self._cards[1].set_value("z2", value)

    @property
    def x0(self) -> float:
        """Get or set the Origin position in global X-direction. See remark 2.
        """ # nopep8
        return self._cards[2].get_value("x0")

    @x0.setter
    def x0(self, value: float) -> None:
        self._cards[2].set_value("x0", value)

    @property
    def y0(self) -> float:
        """Get or set the Origin position in global Y-direction. See remark 2.
        """ # nopep8
        return self._cards[2].get_value("y0")

    @y0.setter
    def y0(self, value: float) -> None:
        self._cards[2].set_value("y0", value)

    @property
    def z0(self) -> float:
        """Get or set the Origin position in global Z-direction. See remark 2.
        """ # nopep8
        return self._cards[2].get_value("z0")

    @z0.setter
    def z0(self, value: float) -> None:
        self._cards[2].set_value("z0", value)

    @property
    def vecid(self) -> typing.Optional[int]:
        """Get or set the ID of the symmetric axis defined by *DEFINE_VECTOR. See remark 3.
        """ # nopep8
        return self._cards[2].get_value("vecid")

    @vecid.setter
    def vecid(self, value: int) -> None:
        self._cards[2].set_value("vecid", value)

