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

"""Module providing the Mat133 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MAT133_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("fit", float, 40, 10, 0.0),
    FieldSchema("beta", float, 50, 10, None),
    FieldSchema("iter", float, 60, 10, 0.0),
    FieldSchema("iscale", float, 70, 10, 0.0),
)

_MAT133_CARD1 = (
    FieldSchema("k", float, 0, 10, None),
    FieldSchema("e0", float, 10, 10, None),
    FieldSchema("n", float, 20, 10, None),
    FieldSchema("c", float, 30, 10, None),
    FieldSchema("p", float, 40, 10, None),
    FieldSchema("hard", float, 50, 10, 1.0),
    FieldSchema("a", float, 60, 10, None),
)

_MAT133_CARD2 = (
    FieldSchema("crc1", float, 0, 10, None),
    FieldSchema("cra1", float, 10, 10, None),
    FieldSchema("crc2", float, 20, 10, None),
    FieldSchema("cra2", float, 30, 10, None),
    FieldSchema("crc3", float, 40, 10, None),
    FieldSchema("cra3", float, 50, 10, None),
    FieldSchema("crc4", float, 60, 10, None),
    FieldSchema("cra4", float, 70, 10, None),
)

_MAT133_CARD3 = (
    FieldSchema("alpha1", float, 0, 10, None),
    FieldSchema("alpha2", float, 10, 10, None),
    FieldSchema("alpha3", float, 20, 10, None),
    FieldSchema("alpha4", float, 30, 10, None),
    FieldSchema("alpha5", float, 40, 10, None),
    FieldSchema("alpha6", float, 50, 10, None),
    FieldSchema("alpha7", float, 60, 10, None),
    FieldSchema("alpha8", float, 70, 10, None),
)

_MAT133_CARD4 = (
    FieldSchema("sig00", float, 0, 10, None),
    FieldSchema("sig45", float, 10, 10, None),
    FieldSchema("sig90", float, 20, 10, None),
    FieldSchema("r00", float, 30, 10, None),
    FieldSchema("r45", float, 40, 10, None),
    FieldSchema("r90", float, 50, 10, None),
)

_MAT133_CARD5 = (
    FieldSchema("sigxx", float, 0, 10, None),
    FieldSchema("sigyy", float, 10, 10, None),
    FieldSchema("sigxy", float, 20, 10, None),
    FieldSchema("dxx", float, 30, 10, None),
    FieldSchema("dyy", float, 40, 10, None),
    FieldSchema("dxy", float, 50, 10, None),
)

_MAT133_CARD6 = (
    FieldSchema("aopt", float, 0, 10, None),
    FieldSchema("offang", float, 10, 10, None),
    FieldSchema("p4", float, 20, 10, None),
    FieldSchema("htflag", int, 30, 10, 0),
    FieldSchema("hta", int, 40, 10, None),
    FieldSchema("htb", int, 50, 10, None),
    FieldSchema("htc", int, 60, 10, None),
    FieldSchema("htd", int, 70, 10, None),
)

_MAT133_CARD7 = (
    FieldSchema("unused", float, 0, 10, None),
    FieldSchema("unused", float, 10, 10, None),
    FieldSchema("unused", float, 20, 10, None),
    FieldSchema("a1", float, 30, 10, None),
    FieldSchema("a2", float, 40, 10, None),
    FieldSchema("a3", float, 50, 10, None),
)

_MAT133_CARD8 = (
    FieldSchema("v1", float, 0, 10, None),
    FieldSchema("v2", float, 10, 10, None),
    FieldSchema("v3", float, 20, 10, None),
    FieldSchema("d1", float, 30, 10, None),
    FieldSchema("d2", float, 40, 10, None),
    FieldSchema("d3", float, 50, 10, None),
    FieldSchema("usrfail", int, 60, 10, 0),
)

_MAT133_CARD9 = (
    FieldSchema("cp", float, 0, 10, None),
    FieldSchema("t0", float, 10, 10, None),
    FieldSchema("tref", float, 20, 10, None),
    FieldSchema("ta0", float, 30, 10, None),
)

_MAT133_CARD10 = (
    FieldSchema("a", float, 0, 10, None),
    FieldSchema("b", float, 10, 10, None),
    FieldSchema("c", float, 20, 10, None),
    FieldSchema("d", float, 30, 10, None),
    FieldSchema("p", float, 40, 10, None),
    FieldSchema("q", float, 50, 10, None),
    FieldSchema("e0mart", float, 60, 10, None),
    FieldSchema("vm0", float, 70, 10, None),
)

_MAT133_CARD11 = (
    FieldSchema("ahs", float, 0, 10, None),
    FieldSchema("bhs", float, 10, 10, None),
    FieldSchema("m", float, 20, 10, None),
    FieldSchema("n", float, 30, 10, None),
    FieldSchema("eps0", float, 40, 10, None),
    FieldSchema("hmart", float, 50, 10, None),
    FieldSchema("k1", float, 60, 10, None),
    FieldSchema("k2", float, 70, 10, None),
)

class Mat133(KeywordBase):
    """DYNA MAT_133 keyword"""

    keyword = "MAT"
    subkeyword = "133"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat133 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT133_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT133_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT133_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT133_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT133_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT133_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT133_CARD6,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT133_CARD7,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT133_CARD8,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT133_CARD9,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT133_CARD10,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT133_CARD11,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat133.option_specs[0],
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
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def fit(self) -> float:
        """Get or set the Material parameter fit flag:
        EQ.0.0: Material parameters are used directly on card3.
        EQ.1.0:Material parameter are determined from test data on card3 and 4
        """ # nopep8
        return self._cards[0].get_value("fit")

    @fit.setter
    def fit(self, value: float) -> None:
        """Set the fit property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""fit must be `None` or one of {0.0,1.0}.""")
        self._cards[0].set_value("fit", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Hardening parameter, 0<b<1.
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[0].set_value("beta", value)

    @property
    def iter(self) -> float:
        """Get or set the Plastic iteration flag:
        EQ.0.0:Plane stress algorithm for stress return.
        EQ.1.0:Secant iteration algorithm for stress return.
        """ # nopep8
        return self._cards[0].get_value("iter")

    @iter.setter
    def iter(self, value: float) -> None:
        """Set the iter property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""iter must be `None` or one of {0.0,1.0}.""")
        self._cards[0].set_value("iter", value)

    @property
    def iscale(self) -> float:
        """Get or set the Yield locus scaling flag:
        EQ.0.0: Scaling on - reference direction=rolling direction (default)
        EQ.1.0: Scaling off - reference direction arbitrary
        """ # nopep8
        return self._cards[0].get_value("iscale")

    @iscale.setter
    def iscale(self, value: float) -> None:
        """Set the iscale property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""iscale must be `None` or one of {0.0,1.0}.""")
        self._cards[0].set_value("iscale", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the material parameter.
        HARD.EQ.1.0:k strength coefficient for exponential hardening.
        EQ.2.0: a in voce hardening law
        """ # nopep8
        return self._cards[1].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        """Set the k property."""
        self._cards[1].set_value("k", value)

    @property
    def e0(self) -> typing.Optional[float]:
        """Get or set the epsilon-0, strain corresponding to the initial yield or b in Voce.
        """ # nopep8
        return self._cards[1].get_value("e0")

    @e0.setter
    def e0(self, value: float) -> None:
        """Set the e0 property."""
        self._cards[1].set_value("e0", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the n, hardening exponent for yield strength or c in Voce.
        """ # nopep8
        return self._cards[1].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[1].set_value("n", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the epsilon-SR0, in powerlaw rate sensitivity.
        """ # nopep8
        return self._cards[1].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[1].set_value("c", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the m, exponent for strain rate effects.
        """ # nopep8
        return self._cards[1].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        """Set the p property."""
        self._cards[1].set_value("p", value)

    @property
    def hard(self) -> float:
        """Get or set the Hardening law:
        EQ.1.0: Exponential hardening
        EQ.2.0: Voce hardening
        EQ.3.0: Hansel hardening
        EQ.4.0: Gosh hardening
        EQ.5.0: Hocket-Sherby hardening
        LT.0.0: absolute value defines load curve ID or table ID. If it is a load curve, then yield stress is a function of plastic strain.
        If it is a table, then yield stress is a function of either plastic strain and plastic strain rate in case of a 2D table,
        or, a function of plastic strain, plastic strain rate, and temperature in case of a 3D table
        """ # nopep8
        return self._cards[1].get_value("hard")

    @hard.setter
    def hard(self, value: float) -> None:
        """Set the hard property."""
        self._cards[1].set_value("hard", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Flow potential exponent.
        """ # nopep8
        return self._cards[1].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[1].set_value("a", value)

    @property
    def crc1(self) -> typing.Optional[float]:
        """Get or set the Chaboche-Roussilier kinematic hardening parameter1
        """ # nopep8
        return self._cards[2].get_value("crc1")

    @crc1.setter
    def crc1(self, value: float) -> None:
        """Set the crc1 property."""
        self._cards[2].set_value("crc1", value)

    @property
    def cra1(self) -> typing.Optional[float]:
        """Get or set the Chaboche-Roussilier kinematic hardening parameter
        """ # nopep8
        return self._cards[2].get_value("cra1")

    @cra1.setter
    def cra1(self, value: float) -> None:
        """Set the cra1 property."""
        self._cards[2].set_value("cra1", value)

    @property
    def crc2(self) -> typing.Optional[float]:
        """Get or set the Chaboche-Roussilier kinematic hardening parameter
        """ # nopep8
        return self._cards[2].get_value("crc2")

    @crc2.setter
    def crc2(self, value: float) -> None:
        """Set the crc2 property."""
        self._cards[2].set_value("crc2", value)

    @property
    def cra2(self) -> typing.Optional[float]:
        """Get or set the Chaboche-Roussilier kinematic hardening parameter
        """ # nopep8
        return self._cards[2].get_value("cra2")

    @cra2.setter
    def cra2(self, value: float) -> None:
        """Set the cra2 property."""
        self._cards[2].set_value("cra2", value)

    @property
    def crc3(self) -> typing.Optional[float]:
        """Get or set the Chaboche-Roussilier kinematic hardening parameter
        """ # nopep8
        return self._cards[2].get_value("crc3")

    @crc3.setter
    def crc3(self, value: float) -> None:
        """Set the crc3 property."""
        self._cards[2].set_value("crc3", value)

    @property
    def cra3(self) -> typing.Optional[float]:
        """Get or set the Chaboche-Roussilier kinematic hardening parameter
        """ # nopep8
        return self._cards[2].get_value("cra3")

    @cra3.setter
    def cra3(self, value: float) -> None:
        """Set the cra3 property."""
        self._cards[2].set_value("cra3", value)

    @property
    def crc4(self) -> typing.Optional[float]:
        """Get or set the Chaboche-Roussilier kinematic hardening parameter
        """ # nopep8
        return self._cards[2].get_value("crc4")

    @crc4.setter
    def crc4(self, value: float) -> None:
        """Set the crc4 property."""
        self._cards[2].set_value("crc4", value)

    @property
    def cra4(self) -> typing.Optional[float]:
        """Get or set the Chaboche-Roussilier kinematic hardening parameter
        """ # nopep8
        return self._cards[2].get_value("cra4")

    @cra4.setter
    def cra4(self, value: float) -> None:
        """Set the cra4 property."""
        self._cards[2].set_value("cra4", value)

    @property
    def alpha1(self) -> typing.Optional[float]:
        """Get or set the Alpha1
        """ # nopep8
        return self._cards[3].get_value("alpha1")

    @alpha1.setter
    def alpha1(self, value: float) -> None:
        """Set the alpha1 property."""
        self._cards[3].set_value("alpha1", value)

    @property
    def alpha2(self) -> typing.Optional[float]:
        """Get or set the Alpha2
        """ # nopep8
        return self._cards[3].get_value("alpha2")

    @alpha2.setter
    def alpha2(self, value: float) -> None:
        """Set the alpha2 property."""
        self._cards[3].set_value("alpha2", value)

    @property
    def alpha3(self) -> typing.Optional[float]:
        """Get or set the Alpha3
        """ # nopep8
        return self._cards[3].get_value("alpha3")

    @alpha3.setter
    def alpha3(self, value: float) -> None:
        """Set the alpha3 property."""
        self._cards[3].set_value("alpha3", value)

    @property
    def alpha4(self) -> typing.Optional[float]:
        """Get or set the Alpha4
        """ # nopep8
        return self._cards[3].get_value("alpha4")

    @alpha4.setter
    def alpha4(self, value: float) -> None:
        """Set the alpha4 property."""
        self._cards[3].set_value("alpha4", value)

    @property
    def alpha5(self) -> typing.Optional[float]:
        """Get or set the Alpha5
        """ # nopep8
        return self._cards[3].get_value("alpha5")

    @alpha5.setter
    def alpha5(self, value: float) -> None:
        """Set the alpha5 property."""
        self._cards[3].set_value("alpha5", value)

    @property
    def alpha6(self) -> typing.Optional[float]:
        """Get or set the Alpha6
        """ # nopep8
        return self._cards[3].get_value("alpha6")

    @alpha6.setter
    def alpha6(self, value: float) -> None:
        """Set the alpha6 property."""
        self._cards[3].set_value("alpha6", value)

    @property
    def alpha7(self) -> typing.Optional[float]:
        """Get or set the Alpha7
        """ # nopep8
        return self._cards[3].get_value("alpha7")

    @alpha7.setter
    def alpha7(self, value: float) -> None:
        """Set the alpha7 property."""
        self._cards[3].set_value("alpha7", value)

    @property
    def alpha8(self) -> typing.Optional[float]:
        """Get or set the Alpha8
        """ # nopep8
        return self._cards[3].get_value("alpha8")

    @alpha8.setter
    def alpha8(self, value: float) -> None:
        """Set the alpha8 property."""
        self._cards[3].set_value("alpha8", value)

    @property
    def sig00(self) -> typing.Optional[float]:
        """Get or set the Yield stress in 00 direction
        LT.0.0:	-SIG00 is load curve ID, defining this stress as a function of temperature
        """ # nopep8
        return self._cards[4].get_value("sig00")

    @sig00.setter
    def sig00(self, value: float) -> None:
        """Set the sig00 property."""
        self._cards[4].set_value("sig00", value)

    @property
    def sig45(self) -> typing.Optional[float]:
        """Get or set the Yield stress in 45 direction
        LT.0.0:	-SIG45 is load curve ID, defining this stress as a function of temperature
        """ # nopep8
        return self._cards[4].get_value("sig45")

    @sig45.setter
    def sig45(self, value: float) -> None:
        """Set the sig45 property."""
        self._cards[4].set_value("sig45", value)

    @property
    def sig90(self) -> typing.Optional[float]:
        """Get or set the Yield stress in 90 direction
        LT.0.0:	-SIG90 is load curve ID, defining this stress as a function of temperature
        """ # nopep8
        return self._cards[4].get_value("sig90")

    @sig90.setter
    def sig90(self, value: float) -> None:
        """Set the sig90 property."""
        self._cards[4].set_value("sig90", value)

    @property
    def r00(self) -> typing.Optional[float]:
        """Get or set the R-value in 00 direction
        LT.0.0:	-R00 is load curve ID, defining this value as a function of temperature
        """ # nopep8
        return self._cards[4].get_value("r00")

    @r00.setter
    def r00(self, value: float) -> None:
        """Set the r00 property."""
        self._cards[4].set_value("r00", value)

    @property
    def r45(self) -> typing.Optional[float]:
        """Get or set the R-value in 45 direction
        LT.0.0:	-R45 is load curve ID, defining this value as a function of temperature
        """ # nopep8
        return self._cards[4].get_value("r45")

    @r45.setter
    def r45(self, value: float) -> None:
        """Set the r45 property."""
        self._cards[4].set_value("r45", value)

    @property
    def r90(self) -> typing.Optional[float]:
        """Get or set the R-value in 90 direction
        LT.0.0:	-R90 is load curve ID, defining this value as a function of temperature.
        """ # nopep8
        return self._cards[4].get_value("r90")

    @r90.setter
    def r90(self, value: float) -> None:
        """Set the r90 property."""
        self._cards[4].set_value("r90", value)

    @property
    def sigxx(self) -> typing.Optional[float]:
        """Get or set the xx-component of stress on yield surface (see Remark 2).
        """ # nopep8
        return self._cards[5].get_value("sigxx")

    @sigxx.setter
    def sigxx(self, value: float) -> None:
        """Set the sigxx property."""
        self._cards[5].set_value("sigxx", value)

    @property
    def sigyy(self) -> typing.Optional[float]:
        """Get or set the yy-component of stress on yield surface (see Remark 2).
        """ # nopep8
        return self._cards[5].get_value("sigyy")

    @sigyy.setter
    def sigyy(self, value: float) -> None:
        """Set the sigyy property."""
        self._cards[5].set_value("sigyy", value)

    @property
    def sigxy(self) -> typing.Optional[float]:
        """Get or set the xy-component of stress on yield surface (see Remark 2).
        """ # nopep8
        return self._cards[5].get_value("sigxy")

    @sigxy.setter
    def sigxy(self, value: float) -> None:
        """Set the sigxy property."""
        self._cards[5].set_value("sigxy", value)

    @property
    def dxx(self) -> typing.Optional[float]:
        """Get or set the xx-component of tangent to yield surface (see Remark 2)
        """ # nopep8
        return self._cards[5].get_value("dxx")

    @dxx.setter
    def dxx(self, value: float) -> None:
        """Set the dxx property."""
        self._cards[5].set_value("dxx", value)

    @property
    def dyy(self) -> typing.Optional[float]:
        """Get or set the yy-component of tangent to yield surface (see Remark 2)
        """ # nopep8
        return self._cards[5].get_value("dyy")

    @dyy.setter
    def dyy(self, value: float) -> None:
        """Set the dyy property."""
        self._cards[5].set_value("dyy", value)

    @property
    def dxy(self) -> typing.Optional[float]:
        """Get or set the xy-component of tangent to yield surface (see Remark 2)
        """ # nopep8
        return self._cards[5].get_value("dxy")

    @dxy.setter
    def dxy(self, value: float) -> None:
        """Set the dxy property."""
        self._cards[5].set_value("dxy", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option:
        EQ.0.0: locally orthotropic with material axes determined by
        element nodes 1, 2, and 4, as with *DEFINE_COORDINATE_NODES, and then rotated about the shell element normal by the angle BETA.
        EQ.2.0: globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
        EQ.3.0: locally orthotropic material axes determined by rotating the material axes about the element normal by an angle,
        BETA, from a line in the plane of the element defined by	the cross product of the vector v with the element normal.
        LT.0.0: the absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
        *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR). Available with the R3 release of Version 971 and later.
        """ # nopep8
        return self._cards[6].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        """Set the aopt property."""
        self._cards[6].set_value("aopt", value)

    @property
    def offang(self) -> typing.Optional[float]:
        """Get or set the Offset angle for AOPT = 3.
        """ # nopep8
        return self._cards[6].get_value("offang")

    @offang.setter
    def offang(self, value: float) -> None:
        """Set the offang property."""
        self._cards[6].set_value("offang", value)

    @property
    def p4(self) -> typing.Optional[float]:
        """Get or set the Material parameter:
        HARD.EQ.4.0: p in Gosh hardening law
        HARD.EQ:5.0: q in Hocket-Sherby hardening law.
        """ # nopep8
        return self._cards[6].get_value("p4")

    @p4.setter
    def p4(self, value: float) -> None:
        """Set the p4 property."""
        self._cards[6].set_value("p4", value)

    @property
    def htflag(self) -> int:
        """Get or set the Heat treatment flag (see remarks):
        HTFLAG.EQ.0: Preforming stage
        HTFLAG.EQ.1: Heat treatment stage
        HTFLAG.EQ.2: Postforming stage.
        """ # nopep8
        return self._cards[6].get_value("htflag")

    @htflag.setter
    def htflag(self, value: int) -> None:
        """Set the htflag property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""htflag must be `None` or one of {0,1,2}.""")
        self._cards[6].set_value("htflag", value)

    @property
    def hta(self) -> typing.Optional[int]:
        """Get or set the Load curve/Table ID for postforming parameter A.
        """ # nopep8
        return self._cards[6].get_value("hta")

    @hta.setter
    def hta(self, value: int) -> None:
        """Set the hta property."""
        self._cards[6].set_value("hta", value)

    @property
    def htb(self) -> typing.Optional[int]:
        """Get or set the Load curve/Table ID for postforming parameter B.
        """ # nopep8
        return self._cards[6].get_value("htb")

    @htb.setter
    def htb(self, value: int) -> None:
        """Set the htb property."""
        self._cards[6].set_value("htb", value)

    @property
    def htc(self) -> typing.Optional[int]:
        """Get or set the Load curve/Table ID for postforming parameter C.
        """ # nopep8
        return self._cards[6].get_value("htc")

    @htc.setter
    def htc(self, value: int) -> None:
        """Set the htc property."""
        self._cards[6].set_value("htc", value)

    @property
    def htd(self) -> typing.Optional[int]:
        """Get or set the Load curve/Table ID for postforming parameter D.
        """ # nopep8
        return self._cards[6].get_value("htd")

    @htd.setter
    def htd(self, value: int) -> None:
        """Set the htd property."""
        self._cards[6].set_value("htd", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[7].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[7].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[7].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[7].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[7].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[7].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[8].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[8].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[8].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[8].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[8].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[8].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[8].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[8].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[8].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[8].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[8].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[8].set_value("d3", value)

    @property
    def usrfail(self) -> int:
        """Get or set the User defined failure flag
        EQ.0: no user subroutine is called
        EQ.1: user subroutine matusr_24 in dyn21.f is called.
        """ # nopep8
        return self._cards[8].get_value("usrfail")

    @usrfail.setter
    def usrfail(self, value: int) -> None:
        """Set the usrfail property."""
        if value not in [0, 1, None]:
            raise Exception("""usrfail must be `None` or one of {0,1}.""")
        self._cards[8].set_value("usrfail", value)

    @property
    def cp(self) -> typing.Optional[float]:
        """Get or set the Adiabatic temperature calculation option:
        EQ.0.0: Adiabatic temperature calculation is disabled.
        GT.0.0: CP is the specific heat Cp. Adiabatic temperature calculation is enabled.
        """ # nopep8
        return self._cards[9].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        """Set the cp property."""
        self._cards[9].set_value("cp", value)

    @property
    def t0(self) -> typing.Optional[float]:
        """Get or set the Initial temperature T0 of the material if adiabatic temperature calculation is enabled.
        """ # nopep8
        return self._cards[9].get_value("t0")

    @t0.setter
    def t0(self, value: float) -> None:
        """Set the t0 property."""
        self._cards[9].set_value("t0", value)

    @property
    def tref(self) -> typing.Optional[float]:
        """Get or set the Reference temperature for output of the yield stress as history variable.
        """ # nopep8
        return self._cards[9].get_value("tref")

    @tref.setter
    def tref(self, value: float) -> None:
        """Set the tref property."""
        self._cards[9].set_value("tref", value)

    @property
    def ta0(self) -> typing.Optional[float]:
        """Get or set the Reference temperature TA0, the absolute zero for the used temperature scale, e.g. -273.15 if the Celsius scale is used and 0.0 if the Kelvin scale is used.
        """ # nopep8
        return self._cards[9].get_value("ta0")

    @ta0.setter
    def ta0(self, value: float) -> None:
        """Set the ta0 property."""
        self._cards[9].set_value("ta0", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Martensite rate equation parameter A.
        """ # nopep8
        return self._cards[10].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[10].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Martensite rate equation parameter B.
        """ # nopep8
        return self._cards[10].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[10].set_value("b", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Martensite rate equation parameter C.
        """ # nopep8
        return self._cards[10].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[10].set_value("c", value)

    @property
    def d(self) -> typing.Optional[float]:
        """Get or set the Martensite rate equation parameter D.
        """ # nopep8
        return self._cards[10].get_value("d")

    @d.setter
    def d(self, value: float) -> None:
        """Set the d property."""
        self._cards[10].set_value("d", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the Martensite rate equation parameter P.
        """ # nopep8
        return self._cards[10].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        """Set the p property."""
        self._cards[10].set_value("p", value)

    @property
    def q(self) -> typing.Optional[float]:
        """Get or set the Martensite rate equation parameter Q.
        """ # nopep8
        return self._cards[10].get_value("q")

    @q.setter
    def q(self, value: float) -> None:
        """Set the q property."""
        self._cards[10].set_value("q", value)

    @property
    def e0mart(self) -> typing.Optional[float]:
        """Get or set the Martensite rate equation parameter E0(mart).
        """ # nopep8
        return self._cards[10].get_value("e0mart")

    @e0mart.setter
    def e0mart(self, value: float) -> None:
        """Set the e0mart property."""
        self._cards[10].set_value("e0mart", value)

    @property
    def vm0(self) -> typing.Optional[float]:
        """Get or set the The initial volume fraction of martensite 0.0<Vm0<1.0 may be initialised using two different methods:
        GT.0.0: Vm0 is set to VM0.
        LT.0.0: Can be used only when there are initial plastic strains Ep
        present, e.g. when using *INITIAL_STRESS_SHELL. The absolute
        value of VM0 is then the load curve ID for a function f that sets
        Vm0 = f(Ep). The function f must be a monotonically nondecreasing function of Ep.
        """ # nopep8
        return self._cards[10].get_value("vm0")

    @vm0.setter
    def vm0(self, value: float) -> None:
        """Set the vm0 property."""
        self._cards[10].set_value("vm0", value)

    @property
    def ahs(self) -> typing.Optional[float]:
        """Get or set the Hardening law parameter AHS.
        """ # nopep8
        return self._cards[11].get_value("ahs")

    @ahs.setter
    def ahs(self, value: float) -> None:
        """Set the ahs property."""
        self._cards[11].set_value("ahs", value)

    @property
    def bhs(self) -> typing.Optional[float]:
        """Get or set the Hardening law parameter BHS.
        """ # nopep8
        return self._cards[11].get_value("bhs")

    @bhs.setter
    def bhs(self, value: float) -> None:
        """Set the bhs property."""
        self._cards[11].set_value("bhs", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Hardening law parameter m.
        """ # nopep8
        return self._cards[11].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[11].set_value("m", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Hardening law parameter n.
        """ # nopep8
        return self._cards[11].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[11].set_value("n", value)

    @property
    def eps0(self) -> typing.Optional[float]:
        """Get or set the Hardening law parameter E0.
        """ # nopep8
        return self._cards[11].get_value("eps0")

    @eps0.setter
    def eps0(self, value: float) -> None:
        """Set the eps0 property."""
        self._cards[11].set_value("eps0", value)

    @property
    def hmart(self) -> typing.Optional[float]:
        """Get or set the Hardening law parameter.
        """ # nopep8
        return self._cards[11].get_value("hmart")

    @hmart.setter
    def hmart(self, value: float) -> None:
        """Set the hmart property."""
        self._cards[11].set_value("hmart", value)

    @property
    def k1(self) -> typing.Optional[float]:
        """Get or set the Hardening law parameter K1.
        """ # nopep8
        return self._cards[11].get_value("k1")

    @k1.setter
    def k1(self, value: float) -> None:
        """Set the k1 property."""
        self._cards[11].set_value("k1", value)

    @property
    def k2(self) -> typing.Optional[float]:
        """Get or set the Hardening law parameter K2.
        """ # nopep8
        return self._cards[11].get_value("k2")

    @k2.setter
    def k2(self, value: float) -> None:
        """Set the k2 property."""
        self._cards[11].set_value("k2", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[12].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[12].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

