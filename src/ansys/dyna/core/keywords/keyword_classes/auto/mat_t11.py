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
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class MatT11(KeywordBase):
    """DYNA MAT_T11 keyword"""

    keyword = "MAT"
    subkeyword = "T11"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "tmid",
                        int,
                        0,
                        10,
                        kwargs.get("tmid")
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "mt",
                        int,
                        20,
                        10,
                        kwargs.get("mt", 11)
                    ),
                    Field(
                        "lmc",
                        int,
                        30,
                        10,
                        kwargs.get("lmc")
                    ),
                    Field(
                        "nhv",
                        int,
                        40,
                        10,
                        kwargs.get("nhv")
                    ),
                    Field(
                        "aopt",
                        float,
                        50,
                        10,
                        kwargs.get("aopt")
                    ),
                    Field(
                        "iortho",
                        int,
                        60,
                        10,
                        kwargs.get("iortho", 0)
                    ),
                    Field(
                        "ihve",
                        int,
                        70,
                        10,
                        kwargs.get("ihve")
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
                        kwargs.get("xp")
                    ),
                    Field(
                        "yp",
                        float,
                        10,
                        10,
                        kwargs.get("yp")
                    ),
                    Field(
                        "zp",
                        float,
                        20,
                        10,
                        kwargs.get("zp")
                    ),
                    Field(
                        "a1",
                        float,
                        30,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "a2",
                        float,
                        40,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "a3",
                        float,
                        50,
                        10,
                        kwargs.get("a3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "d1",
                        float,
                        0,
                        10,
                        kwargs.get("d1")
                    ),
                    Field(
                        "d2",
                        float,
                        10,
                        10,
                        kwargs.get("d2")
                    ),
                    Field(
                        "d3",
                        float,
                        20,
                        10,
                        kwargs.get("d3")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatT11.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def tmid(self) -> typing.Optional[int]:
        """Get or set the Thermal material identification. A unique number has to be specified.
        """ # nopep8
        return self._cards[0].get_value("tmid")

    @tmid.setter
    def tmid(self, value: int) -> None:
        self._cards[0].set_value("tmid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def mt(self) -> int:
        """Get or set the User material type (11-15 inclusive). A number between 11 and 15 has to be chosen.
        """ # nopep8
        return self._cards[0].get_value("mt")

    @mt.setter
    def mt(self, value: int) -> None:
        if value not in [11, 12, 13, 14, 15]:
            raise Exception("""mt must be one of {11,12,13,14,15}""")
        self._cards[0].set_value("mt", value)

    @property
    def lmc(self) -> typing.Optional[int]:
        """Get or set the Length of material constant array which is equal to the number of material constants to be input.
        """ # nopep8
        return self._cards[0].get_value("lmc")

    @lmc.setter
    def lmc(self, value: int) -> None:
        self._cards[0].set_value("lmc", value)

    @property
    def nhv(self) -> typing.Optional[int]:
        """Get or set the Number of history variables to be stored, see also Appendix A of users manual.
        """ # nopep8
        return self._cards[0].get_value("nhv")

    @nhv.setter
    def nhv(self, value: int) -> None:
        self._cards[0].set_value("nhv", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option of orthotropic materials (see MAT_OPTIONTROPIC_ELASTIC for more details). Set if IORTHO = 1.0.
        EQ.0.0:	Locally orthotropic with material axes by element nodes N1, N2and N4
        EQ.1.0 : Locally orthotropic with material axes determined by a point, Image, in spaceand global location of element center
        EQ.2.0 : Globally orthotropic with material axes determined by vectors
        EQ.3.0 : Locally orthotropic with first material axis orthogonal to element normal(defined by element nodes N1, N2 and N4) and to a vector d - Third material direction corresponds to element normal.
        EQ.4.0 : Local orthogonal in cylindrical coordinates with the material axes determined by a vector Image,and an originating point, Image, which define the centerline axis.
        LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_NODES, *DEFINE_COORDINATE_SYSTEM or *DEFINE_COORDINATE_VECTOR)
        """ # nopep8
        return self._cards[0].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[0].set_value("aopt", value)

    @property
    def iortho(self) -> int:
        """Get or set the Orthtropic flag:
        EQ.0: non orthotropic material (default),
        EQ.1: orthotropic material.
        """ # nopep8
        return self._cards[0].get_value("iortho")

    @iortho.setter
    def iortho(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""iortho must be one of {0,1}""")
        self._cards[0].set_value("iortho", value)

    @property
    def ihve(self) -> typing.Optional[int]:
        """Get or set the Address of bulk modulus in material constants array, see also Appendix A of users manual.
        """ # nopep8
        return self._cards[0].get_value("ihve")

    @ihve.setter
    def ihve(self, value: int) -> None:
        self._cards[0].set_value("ihve", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1 and 4
        """ # nopep8
        return self._cards[1].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[1].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1 and 4
        """ # nopep8
        return self._cards[1].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[1].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1 and 4
        """ # nopep8
        return self._cards[1].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[1].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[1].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[1].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[1].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[1].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[1].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[1].set_value("a3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2, 3 and 4.
        """ # nopep8
        return self._cards[2].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[2].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2, 3 and 4.
        """ # nopep8
        return self._cards[2].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[2].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2, 3 and 4
        """ # nopep8
        return self._cards[2].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[2].set_value("d3", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

