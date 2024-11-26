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

class Mat249Udfiber(KeywordBase):
    """DYNA MAT_249_UDfiber keyword"""

    keyword = "MAT"
    subkeyword = "249_UDfiber"
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
                        "mid",
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "em",
                        float,
                        20,
                        10,
                        kwargs.get("em")
                    ),
                    Field(
                        "prm",
                        float,
                        30,
                        10,
                        kwargs.get("prm")
                    ),
                    Field(
                        "g",
                        float,
                        40,
                        10,
                        kwargs.get("g")
                    ),
                    Field(
                        "ezdef",
                        float,
                        50,
                        10,
                        kwargs.get("ezdef")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nfib",
                        int,
                        0,
                        10,
                        kwargs.get("nfib")
                    ),
                    Field(
                        "aopt",
                        float,
                        10,
                        10,
                        kwargs.get("aopt")
                    ),
                    Field(
                        "xp",
                        float,
                        20,
                        10,
                        kwargs.get("xp")
                    ),
                    Field(
                        "yp",
                        float,
                        30,
                        10,
                        kwargs.get("yp")
                    ),
                    Field(
                        "zp",
                        float,
                        40,
                        10,
                        kwargs.get("zp")
                    ),
                    Field(
                        "a1",
                        float,
                        50,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "a2",
                        float,
                        60,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "a3",
                        float,
                        70,
                        10,
                        kwargs.get("a3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "v1",
                        float,
                        0,
                        10,
                        kwargs.get("v1")
                    ),
                    Field(
                        "v2",
                        float,
                        10,
                        10,
                        kwargs.get("v2")
                    ),
                    Field(
                        "v3",
                        float,
                        20,
                        10,
                        kwargs.get("v3")
                    ),
                    Field(
                        "d1",
                        float,
                        30,
                        10,
                        kwargs.get("d1")
                    ),
                    Field(
                        "d2",
                        float,
                        40,
                        10,
                        kwargs.get("d2")
                    ),
                    Field(
                        "d3",
                        float,
                        50,
                        10,
                        kwargs.get("d3")
                    ),
                    Field(
                        "mangl",
                        float,
                        60,
                        10,
                        kwargs.get("mangl")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "idf1",
                        int,
                        0,
                        10,
                        kwargs.get("idf1")
                    ),
                    Field(
                        "alph1",
                        float,
                        10,
                        10,
                        kwargs.get("alph1")
                    ),
                    Field(
                        "ef1",
                        float,
                        20,
                        10,
                        kwargs.get("ef1")
                    ),
                    Field(
                        "kap1",
                        float,
                        30,
                        10,
                        kwargs.get("kap1")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "idf2",
                        int,
                        0,
                        10,
                        kwargs.get("idf2")
                    ),
                    Field(
                        "alph2",
                        float,
                        10,
                        10,
                        kwargs.get("alph2")
                    ),
                    Field(
                        "ef2",
                        float,
                        20,
                        10,
                        kwargs.get("ef2")
                    ),
                    Field(
                        "kap2",
                        float,
                        30,
                        10,
                        kwargs.get("kap2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "idf3",
                        int,
                        0,
                        10,
                        kwargs.get("idf3")
                    ),
                    Field(
                        "alph3",
                        float,
                        10,
                        10,
                        kwargs.get("alph3")
                    ),
                    Field(
                        "ef3",
                        float,
                        20,
                        10,
                        kwargs.get("ef3")
                    ),
                    Field(
                        "kap3",
                        float,
                        30,
                        10,
                        kwargs.get("kap3")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat249Udfiber.option_specs[0],
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
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Density
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def em(self) -> typing.Optional[float]:
        """Get or set the Isotropic young's modulus
        """ # nopep8
        return self._cards[0].get_value("em")

    @em.setter
    def em(self, value: float) -> None:
        self._cards[0].set_value("em", value)

    @property
    def prm(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio for matrix material
        """ # nopep8
        return self._cards[0].get_value("prm")

    @prm.setter
    def prm(self, value: float) -> None:
        self._cards[0].set_value("prm", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Linear shear modulus
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        self._cards[0].set_value("g", value)

    @property
    def ezdef(self) -> typing.Optional[float]:
        """Get or set the Algorithmic parameter. If set to 1, last row of deformation gradient is not updated during the calculation
        """ # nopep8
        return self._cards[0].get_value("ezdef")

    @ezdef.setter
    def ezdef(self, value: float) -> None:
        self._cards[0].set_value("ezdef", value)

    @property
    def nfib(self) -> typing.Optional[int]:
        """Get or set the Number of fiber families to be considered
        """ # nopep8
        return self._cards[1].get_value("nfib")

    @nfib.setter
    def nfib(self, value: int) -> None:
        self._cards[1].set_value("nfib", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC for a more complete description):
        EQ.0.0: locally orthotropic with material axes determined by
        element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES, and then rotated about the shell element normal by the angle MANGL.
        EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
        EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
        BETA, from a line in the plane of the element defined by	the cross product of the vector v with the element normal.
        LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
        *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
        """ # nopep8
        return self._cards[1].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[1].set_value("aopt", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the X-coordinate of point p for AOPT = 1
        """ # nopep8
        return self._cards[1].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[1].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Y-coordinate of point p for AOPT = 1
        """ # nopep8
        return self._cards[1].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[1].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Z-coordinate of point p for AOPT = 1
        """ # nopep8
        return self._cards[1].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[1].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the X-component of vector a for AOPT = 2
        """ # nopep8
        return self._cards[1].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[1].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Y-component of vector a for AOPT = 2
        """ # nopep8
        return self._cards[1].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[1].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Z-component of vector a for AOPT = 2
        """ # nopep8
        return self._cards[1].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[1].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the X-component of vector v for AOPT = 3
        """ # nopep8
        return self._cards[2].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[2].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Y-component of vector v for AOPT = 3
        """ # nopep8
        return self._cards[2].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[2].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Z-component of vector v for AOPT = 3
        """ # nopep8
        return self._cards[2].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[2].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the X-component of vector d for AOPT = 2
        """ # nopep8
        return self._cards[2].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[2].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Y-component of vector d for AOPT = 2
        """ # nopep8
        return self._cards[2].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[2].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Z-component of vector d for AOPT = 2
        """ # nopep8
        return self._cards[2].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[2].set_value("d3", value)

    @property
    def mangl(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 0 and 3, may be overwritten on the element card, see *ELEMENT_SHELL_BETA
        """ # nopep8
        return self._cards[2].get_value("mangl")

    @mangl.setter
    def mangl(self, value: float) -> None:
        self._cards[2].set_value("mangl", value)

    @property
    def idf1(self) -> typing.Optional[int]:
        """Get or set the ID for i-th fiber family for post-processing
        """ # nopep8
        return self._cards[3].get_value("idf1")

    @idf1.setter
    def idf1(self, value: int) -> None:
        self._cards[3].set_value("idf1", value)

    @property
    def alph1(self) -> typing.Optional[float]:
        """Get or set the Orientation angle ALPHA for i-th fiber with respect to overall material direction
        """ # nopep8
        return self._cards[3].get_value("alph1")

    @alph1.setter
    def alph1(self, value: float) -> None:
        self._cards[3].set_value("alph1", value)

    @property
    def ef1(self) -> typing.Optional[float]:
        """Get or set the Young's modulus for i-th fiber family
        """ # nopep8
        return self._cards[3].get_value("ef1")

    @ef1.setter
    def ef1(self, value: float) -> None:
        self._cards[3].set_value("ef1", value)

    @property
    def kap1(self) -> typing.Optional[float]:
        """Get or set the Fiber volume ratio for i-th fiber family
        """ # nopep8
        return self._cards[3].get_value("kap1")

    @kap1.setter
    def kap1(self, value: float) -> None:
        self._cards[3].set_value("kap1", value)

    @property
    def idf2(self) -> typing.Optional[int]:
        """Get or set the ID for i-th fiber family for post-processing
        """ # nopep8
        return self._cards[4].get_value("idf2")

    @idf2.setter
    def idf2(self, value: int) -> None:
        self._cards[4].set_value("idf2", value)

    @property
    def alph2(self) -> typing.Optional[float]:
        """Get or set the Orientation angle ALPHA for i-th fiber with respect to overall material direction
        """ # nopep8
        return self._cards[4].get_value("alph2")

    @alph2.setter
    def alph2(self, value: float) -> None:
        self._cards[4].set_value("alph2", value)

    @property
    def ef2(self) -> typing.Optional[float]:
        """Get or set the Young's modulus for i-th fiber family
        """ # nopep8
        return self._cards[4].get_value("ef2")

    @ef2.setter
    def ef2(self, value: float) -> None:
        self._cards[4].set_value("ef2", value)

    @property
    def kap2(self) -> typing.Optional[float]:
        """Get or set the Fiber volume ratio for i-th fiber family
        """ # nopep8
        return self._cards[4].get_value("kap2")

    @kap2.setter
    def kap2(self, value: float) -> None:
        self._cards[4].set_value("kap2", value)

    @property
    def idf3(self) -> typing.Optional[int]:
        """Get or set the ID for i-th fiber family for post-processing
        """ # nopep8
        return self._cards[5].get_value("idf3")

    @idf3.setter
    def idf3(self, value: int) -> None:
        self._cards[5].set_value("idf3", value)

    @property
    def alph3(self) -> typing.Optional[float]:
        """Get or set the Orientation angle ALPHA for i-th fiber with respect to overall material direction
        """ # nopep8
        return self._cards[5].get_value("alph3")

    @alph3.setter
    def alph3(self, value: float) -> None:
        self._cards[5].set_value("alph3", value)

    @property
    def ef3(self) -> typing.Optional[float]:
        """Get or set the Young's modulus for i-th fiber family
        """ # nopep8
        return self._cards[5].get_value("ef3")

    @ef3.setter
    def ef3(self, value: float) -> None:
        self._cards[5].set_value("ef3", value)

    @property
    def kap3(self) -> typing.Optional[float]:
        """Get or set the Fiber volume ratio for i-th fiber family
        """ # nopep8
        return self._cards[5].get_value("kap3")

    @kap3.setter
    def kap3(self, value: float) -> None:
        self._cards[5].set_value("kap3", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[6].cards[0].set_value("title", value)

