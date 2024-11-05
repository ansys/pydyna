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

class MatOrthoElasticPlastic(KeywordBase):
    """DYNA MAT_ORTHO_ELASTIC_PLASTIC keyword"""

    keyword = "MAT"
    subkeyword = "ORTHO_ELASTIC_PLASTIC"
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
                        "e11",
                        float,
                        20,
                        10,
                        kwargs.get("e11")
                    ),
                    Field(
                        "e22",
                        float,
                        30,
                        10,
                        kwargs.get("e22")
                    ),
                    Field(
                        "g12",
                        float,
                        40,
                        10,
                        kwargs.get("g12")
                    ),
                    Field(
                        "pr12",
                        float,
                        50,
                        10,
                        kwargs.get("pr12")
                    ),
                    Field(
                        "pr23",
                        float,
                        60,
                        10,
                        kwargs.get("pr23")
                    ),
                    Field(
                        "pr31",
                        float,
                        70,
                        10,
                        kwargs.get("pr31")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sigma0",
                        float,
                        0,
                        10,
                        kwargs.get("sigma0")
                    ),
                    Field(
                        "lc",
                        int,
                        10,
                        10,
                        kwargs.get("lc")
                    ),
                    Field(
                        "qr1",
                        float,
                        20,
                        10,
                        kwargs.get("qr1")
                    ),
                    Field(
                        "cr1",
                        float,
                        30,
                        10,
                        kwargs.get("cr1")
                    ),
                    Field(
                        "qr2",
                        float,
                        40,
                        10,
                        kwargs.get("qr2")
                    ),
                    Field(
                        "cr2",
                        float,
                        50,
                        10,
                        kwargs.get("cr2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "r11",
                        float,
                        0,
                        10,
                        kwargs.get("r11")
                    ),
                    Field(
                        "r22",
                        float,
                        10,
                        10,
                        kwargs.get("r22")
                    ),
                    Field(
                        "r33",
                        float,
                        20,
                        10,
                        kwargs.get("r33")
                    ),
                    Field(
                        "r12",
                        float,
                        30,
                        10,
                        kwargs.get("r12")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "aopt",
                        float,
                        0,
                        10,
                        kwargs.get("aopt")
                    ),
                    Field(
                        "beta",
                        float,
                        10,
                        10,
                        kwargs.get("beta")
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
                ],
            ),
            OptionCardSet(
                option_spec = MatOrthoElasticPlastic.option_specs[0],
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
        """Get or set the Material identification. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def e11(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in 11-direction.
        """ # nopep8
        return self._cards[0].get_value("e11")

    @e11.setter
    def e11(self, value: float) -> None:
        self._cards[0].set_value("e11", value)

    @property
    def e22(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in 22-direction.
        """ # nopep8
        return self._cards[0].get_value("e22")

    @e22.setter
    def e22(self, value: float) -> None:
        self._cards[0].set_value("e22", value)

    @property
    def g12(self) -> typing.Optional[float]:
        """Get or set the Shear modulus in 12-direction.
        """ # nopep8
        return self._cards[0].get_value("g12")

    @g12.setter
    def g12(self, value: float) -> None:
        self._cards[0].set_value("g12", value)

    @property
    def pr12(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio 12.
        """ # nopep8
        return self._cards[0].get_value("pr12")

    @pr12.setter
    def pr12(self, value: float) -> None:
        self._cards[0].set_value("pr12", value)

    @property
    def pr23(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio 23.
        """ # nopep8
        return self._cards[0].get_value("pr23")

    @pr23.setter
    def pr23(self, value: float) -> None:
        self._cards[0].set_value("pr23", value)

    @property
    def pr31(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio 31.
        """ # nopep8
        return self._cards[0].get_value("pr31")

    @pr31.setter
    def pr31(self, value: float) -> None:
        self._cards[0].set_value("pr31", value)

    @property
    def sigma0(self) -> typing.Optional[float]:
        """Get or set the Initial yield stress.
        """ # nopep8
        return self._cards[1].get_value("sigma0")

    @sigma0.setter
    def sigma0(self, value: float) -> None:
        self._cards[1].set_value("sigma0", value)

    @property
    def lc(self) -> typing.Optional[int]:
        """Get or set the Load curve ID or Table ID. The load curve ID defines effective stress versus effective plastic strain.
        """ # nopep8
        return self._cards[1].get_value("lc")

    @lc.setter
    def lc(self, value: int) -> None:
        self._cards[1].set_value("lc", value)

    @property
    def qr1(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter QR1.
        """ # nopep8
        return self._cards[1].get_value("qr1")

    @qr1.setter
    def qr1(self, value: float) -> None:
        self._cards[1].set_value("qr1", value)

    @property
    def cr1(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter CR1.
        """ # nopep8
        return self._cards[1].get_value("cr1")

    @cr1.setter
    def cr1(self, value: float) -> None:
        self._cards[1].set_value("cr1", value)

    @property
    def qr2(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter QR2.
        """ # nopep8
        return self._cards[1].get_value("qr2")

    @qr2.setter
    def qr2(self, value: float) -> None:
        self._cards[1].set_value("qr2", value)

    @property
    def cr2(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter CR2.
        """ # nopep8
        return self._cards[1].get_value("cr2")

    @cr2.setter
    def cr2(self, value: float) -> None:
        self._cards[1].set_value("cr2", value)

    @property
    def r11(self) -> typing.Optional[float]:
        """Get or set the Yield criteria parameter R11
        """ # nopep8
        return self._cards[2].get_value("r11")

    @r11.setter
    def r11(self, value: float) -> None:
        self._cards[2].set_value("r11", value)

    @property
    def r22(self) -> typing.Optional[float]:
        """Get or set the Yield criteria parameter R22
        """ # nopep8
        return self._cards[2].get_value("r22")

    @r22.setter
    def r22(self, value: float) -> None:
        self._cards[2].set_value("r22", value)

    @property
    def r33(self) -> typing.Optional[float]:
        """Get or set the Yield criteria parameter R33
        """ # nopep8
        return self._cards[2].get_value("r33")

    @r33.setter
    def r33(self, value: float) -> None:
        self._cards[2].set_value("r33", value)

    @property
    def r12(self) -> typing.Optional[float]:
        """Get or set the Yield criteria parameter R12
        """ # nopep8
        return self._cards[2].get_value("r12")

    @r12.setter
    def r12(self, value: float) -> None:
        self._cards[2].set_value("r12", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option:
        EQ.0.0: locally orthotropic with material axes determined by
        element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES, and then rotated about the shell element normal by the angle BETA.
        EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
        EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
        BETA, from a line in the plane of the element defined by	the cross product of the vector v with the normal to the plane of the element.
        LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
        *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
        """ # nopep8
        return self._cards[3].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[3].set_value("aopt", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT=3, may be overwritten on the element card, see *ELEMENT_SHELL_BEAT or *ELEMENT_SOLID_ORTHO.
        """ # nopep8
        return self._cards[3].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[3].set_value("beta", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the x-coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[4].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[4].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the y-coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[4].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[4].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the z-coordinates of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[4].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[4].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[4].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[4].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[4].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the component of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[5].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[5].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the component of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[5].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[5].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the component of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[5].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[5].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[5].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[5].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[5].set_value("d3", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[6].cards[0].set_value("title", value)

