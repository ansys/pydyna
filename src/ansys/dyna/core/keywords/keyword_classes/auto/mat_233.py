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

"""Module providing the Mat233 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class Mat233(KeywordBase):
    """DYNA MAT_233 keyword"""

    keyword = "MAT"
    subkeyword = "233"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat233 class."""
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
                        **kwargs,
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "e",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pr",
                        float,
                        30,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "hr",
                        float,
                        40,
                        10,
                        1.0,
                        **kwargs,
                    ),
                    Field(
                        "p1",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "p2",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "iter",
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
                        "a",
                        float,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c11",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c22",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c33",
                        float,
                        30,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "lcid",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "e0",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "k",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "p3",
                        float,
                        70,
                        10,
                        **kwargs,
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
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "unused",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c12",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c13",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c23",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "c44",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xp ",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "yp ",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "zp ",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a1",
                        float,
                        30,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "a2",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "a3",
                        float,
                        50,
                        10,
                        **kwargs,
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
                        **kwargs,
                    ),
                    Field(
                        "v2",
                        float,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "v3",
                        float,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "d1",
                        float,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "d2",
                        float,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "d3",
                        float,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "beta",
                        float,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "fit",
                        int,
                        70,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lc1id",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lc2id",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "numint",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lccid",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "icflag",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "idflag",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lc3id",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "epsfg",
                        float,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat233.option_specs[0],
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
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Constant Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus
        E.GT.0.0: constant value
        E.LT.0.0: load curve ID (-E) which defines the Young's modulus as a function of plastic strain.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> float:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def hr(self) -> float:
        """Get or set the Hardening rules:
        EQ.1.0: linear hardening (default)
        EQ.2.0: exponential hardening (Swift)
        EQ.3.0: load curve
        EQ.4.0:exponential hardening (Voce)
        EQ.5.0:exponential hardening (Gosh)
        EQ.6.0:exponential hardening (Hocken-Sherby)
        """ # nopep8
        return self._cards[0].get_value("hr")

    @hr.setter
    def hr(self, value: float) -> None:
        """Set the hr property."""
        if value not in [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, None]:
            raise Exception("""hr must be `None` or one of {1.0,2.0,3.0,4.0,5.0,6.0}.""")
        self._cards[0].set_value("hr", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the Material parameter:
        HR.EQ.1.0: tangent modulus
        HR.EQ.2.0: q, coefficient for exponential hardening law (Swift)
        HR.EQ.4.0: a, coefficient for exponential hardening law (Voce)
        HR.EQ.5.0: q, coefficient for exponential hardening law (Gosh)
        HR.EQ.6.0: a, coefficient for exponential hardening law (Hocket-Sherby)
        """ # nopep8
        return self._cards[0].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        """Set the p1 property."""
        self._cards[0].set_value("p1", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the Material parameter:
        HR.EQ.1.0: yield stress for the linear hardening law
        HR.EQ.2.0: n, coefficient for the exponential hardening law (Swift)
        HR.EQ.4.0: c, coefficient for exponential hardening law (Voce)
        HR.EQ.5.0: n, coefficient for exponential hardening law (Gosh)
        HR.EQ.6.0: c, coefficient for exponential hardening law (Hocket-Sherby)
        """ # nopep8
        return self._cards[0].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        """Set the p2 property."""
        self._cards[0].set_value("p2", value)

    @property
    def iter(self) -> float:
        """Get or set the Iteration flag for speed:
        EQ.0.0: fully iterative
        EQ.1.0: fixed at three iterations Generally, ITER=0.0 is recommended. However, ITER=1.0 is faster and may give acceptable results in most problems.
        """ # nopep8
        return self._cards[0].get_value("iter")

    @iter.setter
    def iter(self, value: float) -> None:
        """Set the iter property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""iter must be `None` or one of {0.0,1.0}.""")
        self._cards[0].set_value("iter", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Exponent in Cazacu-Barlat's orthotropic yield surface ( A>1)
        """ # nopep8
        return self._cards[1].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[1].set_value("a", value)

    @property
    def c11(self) -> typing.Optional[float]:
        """Get or set the Material parameter (see card 5 pos. 8):FIT.EQ.1.0 or EQ.2.0: yield stress for tension in the 00 direction FIT.EQ.0.0: material parameter c11
        """ # nopep8
        return self._cards[1].get_value("c11")

    @c11.setter
    def c11(self, value: float) -> None:
        """Set the c11 property."""
        self._cards[1].set_value("c11", value)

    @property
    def c22(self) -> typing.Optional[float]:
        """Get or set the Material parameter (see card 5 pos.8) FIT.EQ.1.0 or EQ.2.0: yield stress for tension in the 45 direction FIT.EQ.0.0: material parameter c22
        """ # nopep8
        return self._cards[1].get_value("c22")

    @c22.setter
    def c22(self, value: float) -> None:
        """Set the c22 property."""
        self._cards[1].set_value("c22", value)

    @property
    def c33(self) -> float:
        """Get or set the Material parameter (see card 5 pos.8)FIT.EQ.1.0 or EQ.2.0: yield stress for tension in the 90 direction FIT.EQ.0.0: material parameter c33
        """ # nopep8
        return self._cards[1].get_value("c33")

    @c33.setter
    def c33(self, value: float) -> None:
        """Set the c33 property."""
        self._cards[1].set_value("c33", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for the hardening law (HR.EQ.3.0)
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[1].set_value("lcid", value)

    @property
    def e0(self) -> typing.Optional[float]:
        """Get or set the Material parameter:HR.EQ.2.0: initial yield stress for exponential hardening law (Swift)(default =0.0) HR.EQ.4.0: b, coefficient for exponential hardening (Voce)HR.EQ.5.0: initial yield stress for exponential hardening (Gosh),Default=0.0HR.EQ.6.0: b, coefficient for exponential hardening law (Hocket-Sherby)
        """ # nopep8
        return self._cards[1].get_value("e0")

    @e0.setter
    def e0(self, value: float) -> None:
        """Set the e0 property."""
        self._cards[1].set_value("e0", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Material parameter (see card 5 pos.8)FIT.EQ.1.0 or EQ.2.0: yield stress for compression in the 00 directionFIT.EQ.0.0: material parameter (-1<k<1)
        """ # nopep8
        return self._cards[1].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        """Set the k property."""
        self._cards[1].set_value("k", value)

    @property
    def p3(self) -> typing.Optional[float]:
        """Get or set the Material parameter:HR.EQ.5.0: p, coefficient for exponential hardening (Gosh)HR.EQ.6.0: n, exponent for exponential hardening law (Hocket-Sherby)
        """ # nopep8
        return self._cards[1].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        """Set the p3 property."""
        self._cards[1].set_value("p3", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option (see MAT_OPTION TROPIC_ELASTIC for more complete description). AOPT.EQ.0.0 locally orthotropic with material axes determined by element nodes 1, 2 and 4, as with *DEFINE_COORDINATE_NODES. AOPT.EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINED_COORDINATE_VECTOR. AOPT.EQ.3.0: locally orthotropic material axes determined by rotating the material axes abut the element normal by an angle BETA, from a line in the plane of the element defined by the cross product of the vector V with the element normal. AOPT.LT.0.0: the absolute value of AOPT is coordinate system ID (CID on *DEFINE_COORDINATE_NODES, *DEFINE_COORDINATE_SYSTEM or *DEFINE_COORDINATE_VECTOR). Available with the R3 release of 971 and later.
        """ # nopep8
        return self._cards[2].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        """Set the aopt property."""
        self._cards[2].set_value("aopt", value)

    @property
    def c12(self) -> typing.Optional[float]:
        """Get or set the Material parameter. If parameter identification (FIT=1.0) is turned on C12 is not used.
        """ # nopep8
        return self._cards[2].get_value("c12")

    @c12.setter
    def c12(self, value: float) -> None:
        """Set the c12 property."""
        self._cards[2].set_value("c12", value)

    @property
    def c13(self) -> typing.Optional[float]:
        """Get or set the Material parameter. If parameter identification (FIT=1.0) is turned on C13=0.0
        """ # nopep8
        return self._cards[2].get_value("c13")

    @c13.setter
    def c13(self, value: float) -> None:
        """Set the c13 property."""
        self._cards[2].set_value("c13", value)

    @property
    def c23(self) -> typing.Optional[float]:
        """Get or set the Material parameter. If parameter identification (FIT=1.0) is turned on C23=0.0
        """ # nopep8
        return self._cards[2].get_value("c23")

    @c23.setter
    def c23(self, value: float) -> None:
        """Set the c23 property."""
        self._cards[2].set_value("c23", value)

    @property
    def c44(self) -> typing.Optional[float]:
        """Get or set the Material parameter (see card 5 pos.8)
        FIT.EQ.1.0 or EQ.2.0: yield stress for the balanced biaxial tension test
        FIT.EQ.0.0: material parameter c44
        """ # nopep8
        return self._cards[2].get_value("c44")

    @c44.setter
    def c44(self, value: float) -> None:
        """Set the c44 property."""
        self._cards[2].set_value("c44", value)

    @property
    def xp_(self) -> typing.Optional[int]:
        """Get or set the Coordinates of point p for AOPT = 1 and 4
        """ # nopep8
        return self._cards[3].get_value("xp ")

    @xp_.setter
    def xp_(self, value: int) -> None:
        """Set the xp_ property."""
        self._cards[3].set_value("xp ", value)

    @property
    def yp_(self) -> typing.Optional[int]:
        """Get or set the Coordinates of point p for AOPT = 1 and 4
        """ # nopep8
        return self._cards[3].get_value("yp ")

    @yp_.setter
    def yp_(self, value: int) -> None:
        """Set the yp_ property."""
        self._cards[3].set_value("yp ", value)

    @property
    def zp_(self) -> typing.Optional[int]:
        """Get or set the Coordinates of point p for AOPT = 1 and 4
        """ # nopep8
        return self._cards[3].get_value("zp ")

    @zp_.setter
    def zp_(self, value: int) -> None:
        """Set the zp_ property."""
        self._cards[3].set_value("zp ", value)

    @property
    def a1(self) -> float:
        """Get or set the Components of vector a for AOPT=2.0
        """ # nopep8
        return self._cards[3].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[3].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT=2.0
        """ # nopep8
        return self._cards[3].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[3].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT=2.0
        """ # nopep8
        return self._cards[3].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[3].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT=3.0
        """ # nopep8
        return self._cards[4].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[4].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT=3.0
        """ # nopep8
        return self._cards[4].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[4].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT=3.0
        """ # nopep8
        return self._cards[4].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[4].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT=2.0
        """ # nopep8
        return self._cards[4].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[4].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT=2.0
        """ # nopep8
        return self._cards[4].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[4].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT=2.0
        """ # nopep8
        return self._cards[4].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[4].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT=3.0. NOTE, may be overridden on the element card, see *ELEMENT_SHELL_BETA
        """ # nopep8
        return self._cards[4].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[4].set_value("beta", value)

    @property
    def fit(self) -> int:
        """Get or set the Flag for parameter identification algorithm:
        EQ.0.0: No parameter identification routine is used. The variables K, C11, C22, C33, C44, C12, C13 and C23 are interpreted as material parameters.
        EQ.1.0: Parameter fit is used. The variables C11, C22, C33, C44 and K are interpreted as yield stresses in the 00, 45, 90 degree directions, the balanced biaxial tension and the 00 degree compression, respectively. NOTE: it is recommended to always check the d3hsp file to see the fitted parameters before complex jobs are submitted.
        EQ.2.0: Same as EQ.1.0 but also produce contour plots of the yield surface. For each material three LS-PrePost ready xy-datafiles are created; Contour1_x, Contour2_x and Contour3_x where xequal the material numbers.
        """ # nopep8
        return self._cards[4].get_value("fit")

    @fit.setter
    def fit(self, value: int) -> None:
        """Set the fit property."""
        if value not in [0.1, 1.0, 2.0, None]:
            raise Exception("""fit must be `None` or one of {0.1,1.0,2.0}.""")
        self._cards[4].set_value("fit", value)

    @property
    def lc1id(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving equivalent plastic strain to failure as a
        function of stress triaxiality or a table ID giving plastic strain to
        failure as a function of Lode parameter and stress triaxiality (solids)
        """ # nopep8
        return self._cards[5].get_value("lc1id")

    @lc1id.setter
    def lc1id(self, value: int) -> None:
        """Set the lc1id property."""
        self._cards[5].set_value("lc1id", value)

    @property
    def lc2id(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving equivalent plastic strain to failure as a
        function of equivalent plastic strain rate, the failure strain will be
        computed as the product of the values on LC1ID and LC2ID
        """ # nopep8
        return self._cards[5].get_value("lc2id")

    @lc2id.setter
    def lc2id(self, value: int) -> None:
        """Set the lc2id property."""
        self._cards[5].set_value("lc2id", value)

    @property
    def numint(self) -> typing.Optional[int]:
        """Get or set the Number of through thickness integration points which must fail
        before the element is deleted (inactive for solid elements
        """ # nopep8
        return self._cards[5].get_value("numint")

    @numint.setter
    def numint(self, value: int) -> None:
        """Set the numint property."""
        self._cards[5].set_value("numint", value)

    @property
    def lccid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving effective stress in function of plastic strain
        obtained from a compression stress, input of this load curve will
        activate distortional hardening and is NOT compatible with the use of strain rate effects
        """ # nopep8
        return self._cards[5].get_value("lccid")

    @lccid.setter
    def lccid(self, value: int) -> None:
        """Set the lccid property."""
        self._cards[5].set_value("lccid", value)

    @property
    def icflag(self) -> typing.Optional[int]:
        """Get or set the Automated input conversion flag.
        EQ.0 then the load
        curves provided under LCID and LCCID contain Cazacu-Barlat
        effective stress as a function of energy conjugate plastic strain. If
        EQ.1 then both load curves are given in terms of von
        Mises stress versus equivalent plastic strain
        """ # nopep8
        return self._cards[5].get_value("icflag")

    @icflag.setter
    def icflag(self, value: int) -> None:
        """Set the icflag property."""
        self._cards[5].set_value("icflag", value)

    @property
    def idflag(self) -> typing.Optional[int]:
        """Get or set the Damage flag.
        EQ.0 the failure model is of the Johnson
        Cook type and requires LC1ID and LC2ID as additional input. If
        EQ.1 the failure model is of the Gurson type and requires
        LC3ID and EPSFG as additional input
        """ # nopep8
        return self._cards[5].get_value("idflag")

    @idflag.setter
    def idflag(self, value: int) -> None:
        """Set the idflag property."""
        self._cards[5].set_value("idflag", value)

    @property
    def lc3id(self) -> typing.Optional[int]:
        """Get or set the Load curve giving the critical void fraction of the Gurson model
        as a function of the plastic strain to failure measured in the uniaxial tensile test
        """ # nopep8
        return self._cards[5].get_value("lc3id")

    @lc3id.setter
    def lc3id(self, value: int) -> None:
        """Set the lc3id property."""
        self._cards[5].set_value("lc3id", value)

    @property
    def epsfg(self) -> typing.Optional[float]:
        """Get or set the Plastic strain to failure measured in the uniaxial tensile test, this
        value is used by the Gurson type failure model only.
        """ # nopep8
        return self._cards[5].get_value("epsfg")

    @epsfg.setter
    def epsfg(self, value: float) -> None:
        """Set the epsfg property."""
        self._cards[5].set_value("epsfg", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[6].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[6].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

