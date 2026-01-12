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

"""Module providing the MatCazacuBarlat class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATCAZACUBARLAT_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, 0.0),
    FieldSchema("hr", float, 40, 10, 1.0),
    FieldSchema("p1", float, 50, 10, None),
    FieldSchema("p2", float, 60, 10, None),
    FieldSchema("iter", float, 70, 10, 0.0),
)

_MATCAZACUBARLAT_CARD1 = (
    FieldSchema("a", float, 0, 10, None),
    FieldSchema("c11", float, 10, 10, None),
    FieldSchema("c22", float, 20, 10, None),
    FieldSchema("c33", float, 30, 10, 0.0),
    FieldSchema("lcid", int, 40, 10, None),
    FieldSchema("e0", float, 50, 10, None),
    FieldSchema("k", float, 60, 10, None),
    FieldSchema("p3", float, 70, 10, None),
)

_MATCAZACUBARLAT_CARD2 = (
    FieldSchema("aopt", float, 0, 10, None),
    FieldSchema("unused", float, 10, 10, None),
    FieldSchema("unused", float, 20, 10, None),
    FieldSchema("unused", float, 30, 10, None),
    FieldSchema("c12", float, 40, 10, None),
    FieldSchema("c13", float, 50, 10, None),
    FieldSchema("c23", float, 60, 10, None),
    FieldSchema("c44", float, 70, 10, None),
)

_MATCAZACUBARLAT_CARD3 = (
    FieldSchema("xp ", int, 0, 10, None),
    FieldSchema("yp ", int, 10, 10, None),
    FieldSchema("zp ", int, 20, 10, None),
    FieldSchema("a1", float, 30, 10, 0.0),
    FieldSchema("a2", float, 40, 10, None),
    FieldSchema("a3", float, 50, 10, None),
)

_MATCAZACUBARLAT_CARD4 = (
    FieldSchema("v1", float, 0, 10, None),
    FieldSchema("v2", float, 10, 10, None),
    FieldSchema("v3", float, 20, 10, None),
    FieldSchema("d1", float, 30, 10, None),
    FieldSchema("d2", float, 40, 10, None),
    FieldSchema("d3", float, 50, 10, None),
    FieldSchema("beta", float, 60, 10, None),
    FieldSchema("fit", int, 70, 10, 0),
)

_MATCAZACUBARLAT_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatCazacuBarlat(KeywordBase):
    """DYNA MAT_CAZACU_BARLAT keyword"""

    keyword = "MAT"
    subkeyword = "CAZACU_BARLAT"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatCazacuBarlat class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATCAZACUBARLAT_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATCAZACUBARLAT_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATCAZACUBARLAT_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATCAZACUBARLAT_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATCAZACUBARLAT_CARD4,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatCazacuBarlat.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATCAZACUBARLAT_OPTION0_CARD0,
                        **kwargs,
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
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[5].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

