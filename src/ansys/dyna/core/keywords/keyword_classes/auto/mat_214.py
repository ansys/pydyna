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

class Mat214(KeywordBase):
    """DYNA MAT_214 keyword"""

    keyword = "MAT"
    subkeyword = "214"
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
                        "ea",
                        float,
                        20,
                        10,
                        kwargs.get("ea")
                    ),
                    Field(
                        "eb",
                        float,
                        30,
                        10,
                        kwargs.get("eb")
                    ),
                    Field(
                        "gab1",
                        float,
                        40,
                        10,
                        kwargs.get("gab1")
                    ),
                    Field(
                        "gab2",
                        float,
                        50,
                        10,
                        kwargs.get("gab2")
                    ),
                    Field(
                        "gab3",
                        float,
                        60,
                        10,
                        kwargs.get("gab3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "gbc",
                        float,
                        0,
                        10,
                        kwargs.get("gbc")
                    ),
                    Field(
                        "gca",
                        float,
                        10,
                        10,
                        kwargs.get("gca")
                    ),
                    Field(
                        "gamab1",
                        float,
                        20,
                        10,
                        kwargs.get("gamab1")
                    ),
                    Field(
                        "gamab2",
                        float,
                        30,
                        10,
                        kwargs.get("gamab2")
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
                        "unused",
                        float,
                        10,
                        10,
                        kwargs.get("unused")
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
                        "beta",
                        float,
                        60,
                        10,
                        kwargs.get("beta")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "eacrf",
                        float,
                        0,
                        10,
                        kwargs.get("eacrf")
                    ),
                    Field(
                        "ebcrf",
                        float,
                        10,
                        10,
                        kwargs.get("ebcrf")
                    ),
                    Field(
                        "eacrp",
                        float,
                        20,
                        10,
                        kwargs.get("eacrp")
                    ),
                    Field(
                        "ebcrp",
                        float,
                        30,
                        10,
                        kwargs.get("ebcrp")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "easf",
                        float,
                        0,
                        10,
                        kwargs.get("easf")
                    ),
                    Field(
                        "ebsf",
                        float,
                        10,
                        10,
                        kwargs.get("ebsf")
                    ),
                    Field(
                        "eunlf",
                        float,
                        20,
                        10,
                        kwargs.get("eunlf")
                    ),
                    Field(
                        "ecomf",
                        float,
                        30,
                        10,
                        kwargs.get("ecomf")
                    ),
                    Field(
                        "eamax",
                        float,
                        40,
                        10,
                        kwargs.get("eamax")
                    ),
                    Field(
                        "ebmax",
                        float,
                        50,
                        10,
                        kwargs.get("ebmax")
                    ),
                    Field(
                        "sigpost",
                        float,
                        60,
                        10,
                        kwargs.get("sigpost")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cce",
                        float,
                        0,
                        10,
                        kwargs.get("cce")
                    ),
                    Field(
                        "pce",
                        float,
                        10,
                        10,
                        kwargs.get("pce")
                    ),
                    Field(
                        "cse",
                        float,
                        20,
                        10,
                        kwargs.get("cse")
                    ),
                    Field(
                        "pse",
                        float,
                        30,
                        10,
                        kwargs.get("pse")
                    ),
                    Field(
                        "dfac",
                        float,
                        40,
                        10,
                        kwargs.get("dfac")
                    ),
                    Field(
                        "emax",
                        float,
                        50,
                        10,
                        kwargs.get("emax")
                    ),
                    Field(
                        "eafail",
                        float,
                        60,
                        10,
                        kwargs.get("eafail")
                    ),
                    Field(
                        "ebfail",
                        float,
                        70,
                        10,
                        kwargs.get("ebfail")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat214.option_specs[0],
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
        """Get or set the Continuum equivalent Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def ea(self) -> typing.Optional[float]:
        """Get or set the Modulus of elasticity in the longitudinal (warp) direction, which corresponds to the slope of segment AB .
        """ # nopep8
        return self._cards[0].get_value("ea")

    @ea.setter
    def ea(self, value: float) -> None:
        self._cards[0].set_value("ea", value)

    @property
    def eb(self) -> typing.Optional[float]:
        """Get or set the Modulus of elasticity in the transverse (fill) direction, which corre-sponds to the slope of segment of AB.
        """ # nopep8
        return self._cards[0].get_value("eb")

    @eb.setter
    def eb(self, value: float) -> None:
        self._cards[0].set_value("eb", value)

    @property
    def gab1(self) -> typing.Optional[float]:
        """Get or set the Shear stress-strain behavior is modeled as piecewise linear in three segments.  See the figure to the right.  The shear moduli GABi corre-spond to the slope of the ith segment.  The start and end points for the segments are specified in the GAMAB[1-2] fields.
        """ # nopep8
        return self._cards[0].get_value("gab1")

    @gab1.setter
    def gab1(self, value: float) -> None:
        self._cards[0].set_value("gab1", value)

    @property
    def gab2(self) -> typing.Optional[float]:
        """Get or set the Shear stress-strain behavior is modeled as piecewise linear in three segments.  See the figure to the right.  The shear moduli GABi corre-spond to the slope of the ith segment.  The start and end points for the segments are specified in the GAMAB[1-2] fields.
        """ # nopep8
        return self._cards[0].get_value("gab2")

    @gab2.setter
    def gab2(self, value: float) -> None:
        self._cards[0].set_value("gab2", value)

    @property
    def gab3(self) -> typing.Optional[float]:
        """Get or set the Shear stress-strain behavior is modeled as piecewise linear in three segments.  See the figure to the right.  The shear moduli GABi corre-spond to the slope of the ith segment.  The start and end points for the segments are specified in the GAMAB[1-2] fields.
        """ # nopep8
        return self._cards[0].get_value("gab3")

    @gab3.setter
    def gab3(self, value: float) -> None:
        self._cards[0].set_value("gab3", value)

    @property
    def gbc(self) -> typing.Optional[float]:
        """Get or set the Shear modulus in bc direction.
        """ # nopep8
        return self._cards[1].get_value("gbc")

    @gbc.setter
    def gbc(self, value: float) -> None:
        self._cards[1].set_value("gbc", value)

    @property
    def gca(self) -> typing.Optional[float]:
        """Get or set the Shear modulus in ca direction.
        """ # nopep8
        return self._cards[1].get_value("gca")

    @gca.setter
    def gca(self, value: float) -> None:
        self._cards[1].set_value("gca", value)

    @property
    def gamab1(self) -> typing.Optional[float]:
        """Get or set the Shear stress-strain behavior is modeled as piecewise linear in three segments.  See the figure to the right.  The shear moduli GABi corre-spond to the slope of the ith segment.  The start and end points for the segments are specified in the GAMAB[1-2] fields.
        """ # nopep8
        return self._cards[1].get_value("gamab1")

    @gamab1.setter
    def gamab1(self, value: float) -> None:
        self._cards[1].set_value("gamab1", value)

    @property
    def gamab2(self) -> typing.Optional[float]:
        """Get or set the Shear stress-strain behavior is modeled as piecewise linear in three segments.  See the figure to the right.  The shear moduli GABi corre-spond to the slope of the ith segment.  The start and end points for the segments are specified in the GAMAB[1-2] fields.
        """ # nopep8
        return self._cards[1].get_value("gamab2")

    @gamab2.setter
    def gamab2(self, value: float) -> None:
        self._cards[1].set_value("gamab2", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option.  See *MAT_OPTIONTROPIC_ELAS¬TIC for a more complete description:
        EQ.0.0:	locally orthotropic with material axes determined by ele-ment nodes 1, 2, and 4, as with *DEFINECOORDINATE_NODES, and then rotated about the element normal by an angle BETA.
        EQ.2.0:	globally orthotropic with material axes determined by vec-tors defined below, as with *DEFINE_COORDINATE_VECTOR.
        EQ.3.0:	locally orthotropic material axes determined by rotating the material axes about the element normal by an angle, BETA, from a line in the plane of the element defined by the cross product of the vector v with the element normal.
        LT.0.0:	the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES, *DEFINE_COORDINATE_SYSTEM or *DEFINE_COORDINATE_VECTOR).  Available in R3 version of 971 and later.
        """ # nopep8
        return self._cards[2].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        self._cards[2].set_value("aopt", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Components of vector x.
        """ # nopep8
        return self._cards[2].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        self._cards[2].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Components of vector y.
        """ # nopep8
        return self._cards[2].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        self._cards[2].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Components of vector z
        """ # nopep8
        return self._cards[2].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        self._cards[2].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[2].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[2].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2..
        """ # nopep8
        return self._cards[2].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[2].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[3].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[3].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3..
        """ # nopep8
        return self._cards[3].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[3].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3..
        """ # nopep8
        return self._cards[3].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[3].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2..
        """ # nopep8
        return self._cards[3].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        self._cards[3].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        self._cards[3].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        self._cards[3].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 0 and 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA.
        """ # nopep8
        return self._cards[3].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[3].set_value("beta", value)

    @property
    def eacrf(self) -> typing.Optional[float]:
        """Get or set the Factor for crimp region modulus of elasticity in longitudinal direction
        """ # nopep8
        return self._cards[4].get_value("eacrf")

    @eacrf.setter
    def eacrf(self, value: float) -> None:
        self._cards[4].set_value("eacrf", value)

    @property
    def ebcrf(self) -> typing.Optional[float]:
        """Get or set the Factor for crimp region modulus of elasticity in transverse direction .
        """ # nopep8
        return self._cards[4].get_value("ebcrf")

    @ebcrf.setter
    def ebcrf(self, value: float) -> None:
        self._cards[4].set_value("ebcrf", value)

    @property
    def eacrp(self) -> typing.Optional[float]:
        """Get or set the Crimp strain in longitudinal direction .
        """ # nopep8
        return self._cards[4].get_value("eacrp")

    @eacrp.setter
    def eacrp(self, value: float) -> None:
        self._cards[4].set_value("eacrp", value)

    @property
    def ebcrp(self) -> typing.Optional[float]:
        """Get or set the Crimp strain in transverse direction .
        """ # nopep8
        return self._cards[4].get_value("ebcrp")

    @ebcrp.setter
    def ebcrp(self, value: float) -> None:
        self._cards[4].set_value("ebcrp", value)

    @property
    def easf(self) -> typing.Optional[float]:
        """Get or set the Factor for post-peak region modulus of elasticity in longitudinal direction
        """ # nopep8
        return self._cards[5].get_value("easf")

    @easf.setter
    def easf(self, value: float) -> None:
        self._cards[5].set_value("easf", value)

    @property
    def ebsf(self) -> typing.Optional[float]:
        """Get or set the Factor for post-peak region modulus of elasticity in transverse direc-tion.
        """ # nopep8
        return self._cards[5].get_value("ebsf")

    @ebsf.setter
    def ebsf(self, value: float) -> None:
        self._cards[5].set_value("ebsf", value)

    @property
    def eunlf(self) -> typing.Optional[float]:
        """Get or set the Factor for unloading modulus of elasticity .
        """ # nopep8
        return self._cards[5].get_value("eunlf")

    @eunlf.setter
    def eunlf(self, value: float) -> None:
        self._cards[5].set_value("eunlf", value)

    @property
    def ecomf(self) -> typing.Optional[float]:
        """Get or set the Factor for compression zone modulus of elasticity .
        """ # nopep8
        return self._cards[5].get_value("ecomf")

    @ecomf.setter
    def ecomf(self, value: float) -> None:
        self._cards[5].set_value("ecomf", value)

    @property
    def eamax(self) -> typing.Optional[float]:
        """Get or set the Strain at peak stress in longitudinal direction
        """ # nopep8
        return self._cards[5].get_value("eamax")

    @eamax.setter
    def eamax(self, value: float) -> None:
        self._cards[5].set_value("eamax", value)

    @property
    def ebmax(self) -> typing.Optional[float]:
        """Get or set the Strain at peak stress in transverse direction
        """ # nopep8
        return self._cards[5].get_value("ebmax")

    @ebmax.setter
    def ebmax(self, value: float) -> None:
        self._cards[5].set_value("ebmax", value)

    @property
    def sigpost(self) -> typing.Optional[float]:
        """Get or set the Stress value in post-peak region at which nonlinear behavior begins
        """ # nopep8
        return self._cards[5].get_value("sigpost")

    @sigpost.setter
    def sigpost(self, value: float) -> None:
        self._cards[5].set_value("sigpost", value)

    @property
    def cce(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter C, Cowper-Symonds factor for modulus. If zero,  rate effects are not considered
        """ # nopep8
        return self._cards[6].get_value("cce")

    @cce.setter
    def cce(self, value: float) -> None:
        self._cards[6].set_value("cce", value)

    @property
    def pce(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter P, Cowper-Symonds factor for modulus. If zero,  rate effects are not considered.
        """ # nopep8
        return self._cards[6].get_value("pce")

    @pce.setter
    def pce(self, value: float) -> None:
        self._cards[6].set_value("pce", value)

    @property
    def cse(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter C, Cowper-Symonds factor for stress to peak / failure. If zero, rate effects are not considered.
        """ # nopep8
        return self._cards[6].get_value("cse")

    @cse.setter
    def cse(self, value: float) -> None:
        self._cards[6].set_value("cse", value)

    @property
    def pse(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter P, Cowper-Symonds factor for stress to peak / failure. If zero, rate effects are not considered.
        """ # nopep8
        return self._cards[6].get_value("pse")

    @pse.setter
    def pse(self, value: float) -> None:
        self._cards[6].set_value("pse", value)

    @property
    def dfac(self) -> typing.Optional[float]:
        """Get or set the Damage factor
        """ # nopep8
        return self._cards[6].get_value("dfac")

    @dfac.setter
    def dfac(self, value: float) -> None:
        self._cards[6].set_value("dfac", value)

    @property
    def emax(self) -> typing.Optional[float]:
        """Get or set the Erosion strain of element
        """ # nopep8
        return self._cards[6].get_value("emax")

    @emax.setter
    def emax(self, value: float) -> None:
        self._cards[6].set_value("emax", value)

    @property
    def eafail(self) -> typing.Optional[float]:
        """Get or set the Erosion strain in longitudinal direction
        """ # nopep8
        return self._cards[6].get_value("eafail")

    @eafail.setter
    def eafail(self, value: float) -> None:
        self._cards[6].set_value("eafail", value)

    @property
    def ebfail(self) -> typing.Optional[float]:
        """Get or set the Erosion strain in transverse direction .
        """ # nopep8
        return self._cards[6].get_value("ebfail")

    @ebfail.setter
    def ebfail(self, value: float) -> None:
        self._cards[6].set_value("ebfail", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[7].cards[0].set_value("title", value)

