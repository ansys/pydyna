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

"""Module providing the Mat295 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.card_set import CardSet, ensure_card_set_properties
from ansys.dyna.core.lib.cards import Cards
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_FIBERFAMILY_CARD0 = (
    FieldSchema("theta", float, 0, 10, None),
    FieldSchema("a", float, 10, 10, None),
    FieldSchema("b", float, 20, 10, None),
)

_FIBERFAMILY_CARD1 = (
    FieldSchema("ftype", int, 0, 10, 1),
    FieldSchema("fcid", int, 10, 10, None),
    FieldSchema("k1", float, 20, 10, None),
    FieldSchema("k2", float, 30, 10, None),
)

_FIBERFAMILY_CARD2 = (
    FieldSchema("ftype", int, 0, 10, 1),
    FieldSchema("flcid", int, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("r0norm", float, 30, 10, None),
    FieldSchema("h0norm", float, 40, 10, None),
)

class FiberFamily(Cards):
    """ CardSet."""

    def __init__(self, **kwargs):
        """Initialize the FiberFamily CardSet."""
        super().__init__(kwargs["keyword"])
        self._parent = kwargs["parent"]
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _FIBERFAMILY_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _FIBERFAMILY_CARD1,
                active_func=lambda: self.ftype == 1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _FIBERFAMILY_CARD2,
                active_func=lambda: self.ftype == 2,
                **kwargs,
            ),        ]

    def _read_data(self, buf: typing.TextIO, parameters) -> bool:
        """Custom read logic to handle ftype-dependent cards."""
        from ansys.dyna.core.lib.card_set import read_cards_with_discriminator

        return read_cards_with_discriminator(
            self._cards,
            buf,
            parameters,
            discriminator=self._cards[1]._fields[0],
            cards_with_field=[1, 2],
        )

    @property
    def theta(self) -> typing.Optional[float]:
        """Get or set the Mean fiber family orientation angle with respect to the a material axis in the a-b material plane in degrees.
        """ # nopep8
        return self._cards[0].get_value("theta")

    @theta.setter
    def theta(self, value: float) -> None:
        """Set the theta property."""
        self._cards[0].set_value("theta", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the First structure tensor parameter.
        """ # nopep8
        return self._cards[0].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[0].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Second structure tensor parameter.
        """ # nopep8
        return self._cards[0].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[0].set_value("b", value)

    @property
    def fcid(self) -> typing.Optional[int]:
        """Get or set the Curve ID defining the fiber stress versus fiber stretch relation, default if nonzero.
        """ # nopep8
        return self._cards[1].get_value("fcid")

    @fcid.setter
    def fcid(self, value: int) -> None:
        """Set the fcid property."""
        self._cards[1].set_value("fcid", value)

    @property
    def k1(self) -> typing.Optional[float]:
        """Get or set the Holzapfel-Gasser-Ogden modulus.

        Note: For the K1 (Card 11), use ``coupling_k1``.
        """ # nopep8
        return self._cards[1].get_value("k1")

    @k1.setter
    def k1(self, value: float) -> None:
        """Set the k1 property."""
        self._cards[1].set_value("k1", value)

    @property
    def k2(self) -> typing.Optional[float]:
        """Get or set the Holzapfel-Gasser-Ogden constant.

        Note: For the K2 (Card 11), use ``coupling_k2``.
        """ # nopep8
        return self._cards[1].get_value("k2")

    @k2.setter
    def k2(self, value: float) -> None:
        """Set the k2 property."""
        self._cards[1].set_value("k2", value)

    @property
    def flcid(self) -> typing.Optional[int]:
        """Get or set the Curve ID defining the fiber stress versus fiber stretch relation, default if nonzero.
        """ # nopep8
        return self._cards[2].get_value("flcid")

    @flcid.setter
    def flcid(self, value: int) -> None:
        """Set the flcid property."""
        self._cards[2].set_value("flcid", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Fiber modulus.
        """ # nopep8
        return self._cards[2].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[2].set_value("e", value)

    @property
    def r0norm(self) -> typing.Optional[float]:
        """Get or set the Initial crimp/coil amplitude normalized with respect to the initial fiber radius (R0/r0).
        """ # nopep8
        return self._cards[2].get_value("r0norm")

    @r0norm.setter
    def r0norm(self, value: float) -> None:
        """Set the r0norm property."""
        self._cards[2].set_value("r0norm", value)

    @property
    def h0norm(self) -> typing.Optional[float]:
        """Get or set the Initial crimp/coil wavelength normalized with respect to the initial fiber radius (H0/r0).
        """ # nopep8
        return self._cards[2].get_value("h0norm")

    @h0norm.setter
    def h0norm(self, value: float) -> None:
        """Set the h0norm property."""
        self._cards[2].set_value("h0norm", value)

    @property
    def ftype(self) -> typing.Optional[int]:
        """Get or set the Type of fiber model:
EQ.1:	Holzapfel-Gasser-Ogden [6]
EQ.2:	Freed-Doehring [2].
        """ # nopep8
        return self._cards[1].get_value("ftype")

    @ftype.setter
    def ftype(self, value: int) -> None:
        self._cards[1].set_value("ftype", value)
        self._cards[2].set_value("ftype", value)

    @property
    def parent(self) -> KeywordBase:
        """Get the parent keyword."""
        return self._parent

_MAT295_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("rho", float, 10, 10, None),
    FieldSchema("aopt", float, 20, 10, None),
)

_MAT295_CARD1 = (
    FieldSchema("title", str, 0, 10, "ISO"),
    FieldSchema("itype", int, 10, 10, None),
    FieldSchema("beta", float, 20, 10, None),
    FieldSchema("nu", float, 30, 10, None),
)

_MAT295_CARD2 = (
    FieldSchema("mu1", float, 0, 10, None),
    FieldSchema("mu2", float, 10, 10, None),
    FieldSchema("mu3", float, 20, 10, None),
    FieldSchema("mu4", float, 30, 10, None),
    FieldSchema("mu5", float, 40, 10, None),
    FieldSchema("mu6", float, 50, 10, None),
    FieldSchema("mu7", float, 60, 10, None),
    FieldSchema("mu8", float, 70, 10, None),
)

_MAT295_CARD3 = (
    FieldSchema("alpha1", float, 0, 10, None),
    FieldSchema("alpha2", float, 10, 10, None),
    FieldSchema("alpha3", float, 20, 10, None),
    FieldSchema("alpha4", float, 30, 10, None),
    FieldSchema("alpha5", float, 40, 10, None),
    FieldSchema("alpha6", float, 50, 10, None),
    FieldSchema("alpha7", float, 60, 10, None),
    FieldSchema("alpha8", float, 70, 10, None),
)

_MAT295_CARD4 = (
    FieldSchema("c1", float, 0, 10, None),
    FieldSchema("c2", float, 10, 10, None),
    FieldSchema("c3", float, 20, 10, None),
)

_MAT295_CARD5 = (
    FieldSchema("k1", float, 0, 10, None),
    FieldSchema("k2", float, 10, 10, None),
)

_MAT295_CARD6 = (
    FieldSchema("title", str, 0, 10, "ANISO"),
    FieldSchema("atype", int, 10, 10, None),
    FieldSchema("intype", int, 20, 10, None),
    FieldSchema("nf", int, 30, 10, None),
)

_MAT295_CARD8 = (
    FieldSchema("k1", float, 0, 10, None),
    FieldSchema("k2", float, 10, 10, None),
)

_MAT295_CARD9 = (
    FieldSchema("title", str, 0, 10, "ACTIVE"),
    FieldSchema("actype", int, 10, 10, None),
    FieldSchema("acdir", int, 20, 10, 0),
    FieldSchema("acid", int, 30, 10, None),
    FieldSchema("acthr", float, 40, 10, 0.0),
    FieldSchema("sf", float, 50, 10, 1.0),
    FieldSchema("ss", float, 60, 10, 0.0),
    FieldSchema("sn", float, 70, 10, 0.0),
)

_MAT295_CARD10 = (
    FieldSchema("t0", float, 0, 10, None),
    FieldSchema("ca2ion", float, 10, 10, None),
    FieldSchema("ca2ionm", float, 20, 10, None),
    FieldSchema("n", float, 30, 10, None),
    FieldSchema("taumax", float, 40, 10, None),
    FieldSchema("stf", float, 50, 10, None),
    FieldSchema("b", float, 60, 10, None),
    FieldSchema("l0", float, 70, 10, None),
)

_MAT295_CARD11 = (
    FieldSchema("l", float, 0, 10, None),
    FieldSchema("dtmax", float, 10, 10, None),
    FieldSchema("mr", float, 20, 10, None),
    FieldSchema("tr", float, 30, 10, None),
)

_MAT295_CARD12 = (
    FieldSchema("l", float, 0, 10, None),
    FieldSchema("eta", float, 10, 10, None),
)

_MAT295_CARD13 = (
    FieldSchema("t0", float, 0, 10, None),
    FieldSchema("ca2ion", float, 10, 10, None),
    FieldSchema("ca2ion50", float, 20, 10, None),
    FieldSchema("n", float, 30, 10, None),
    FieldSchema("sigmax", float, 40, 10, None),
    FieldSchema("f", float, 50, 10, None),
    FieldSchema("l", float, 60, 10, None),
    FieldSchema("eta", float, 70, 10, None),
)

_MAT295_CARD14 = (
    FieldSchema("t0", float, 0, 10, None),
    FieldSchema("ca2ion50", float, 10, 10, None),
    FieldSchema("ca2ionmax", float, 20, 10, None),
    FieldSchema("n", float, 30, 10, None),
    FieldSchema("sigmax", float, 40, 10, None),
    FieldSchema("f", float, 50, 10, None),
    FieldSchema("ca2ion0", float, 60, 10, None),
    FieldSchema("tca", float, 70, 10, None),
)

_MAT295_CARD15 = (
    FieldSchema("l", float, 0, 10, None),
    FieldSchema("eta", float, 10, 10, None),
)

_MAT295_CARD16 = (
    FieldSchema("fseid", int, 0, 10, None),
    FieldSchema("flid", int, 10, 10, None),
    FieldSchema("fvid", int, 20, 10, None),
    FieldSchema("alphaid", int, 30, 10, None),
)

_MAT295_CARD17 = (
    FieldSchema("xp", float, 0, 10, None),
    FieldSchema("yp", float, 10, 10, None),
    FieldSchema("zp", float, 20, 10, None),
    FieldSchema("a1", float, 30, 10, None),
    FieldSchema("a2", float, 40, 10, None),
    FieldSchema("a3", float, 50, 10, None),
    FieldSchema("macf", int, 60, 10, 1),
    FieldSchema("unused", int, 70, 10, None),
)

_MAT295_CARD18 = (
    FieldSchema("v1", float, 0, 10, None),
    FieldSchema("v2", float, 10, 10, None),
    FieldSchema("v3", float, 20, 10, None),
    FieldSchema("d1", float, 30, 10, None),
    FieldSchema("d2", float, 40, 10, None),
    FieldSchema("d3", float, 50, 10, None),
    FieldSchema("beta", float, 60, 10, None),
    FieldSchema("ref", float, 70, 10, None),
)

_MAT295_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat295(KeywordBase):
    """DYNA MAT_295 keyword

    Fields with renamed properties:
        TITLE (Card 2) -> isotropic_title
        TITLE (Card 7) -> anisotropic_title
        TITLE (Card 12) -> active_title
        K1 (Card 11) -> coupling_k1
        K2 (Card 11) -> coupling_k2
        BETA (Card 21) -> material_angle_beta
    """

    keyword = "MAT"
    subkeyword = "295"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat295 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        kwargs["keyword"] = self
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT295_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT295_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT295_CARD2,
                active_func=lambda: self.itype and abs(self.itype) == 1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT295_CARD3,
                active_func=lambda: self.itype and abs(self.itype) == 1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT295_CARD4,
                active_func=lambda: self.itype and abs(self.itype) == 2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT295_CARD5,
                active_func=lambda: self.itype and abs(self.itype) == 3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT295_CARD6,
                active_func=lambda: self.atype and abs(self.atype) == 1,
                **kwargs,
            ),            CardSet(
                FiberFamily,
                length_func = lambda: self.nf or 0,
                active_func = lambda: self.atype and abs(self.atype) == 1,
                **kwargs
            ),            Card.from_field_schemas_with_defaults(
                _MAT295_CARD8,
                active_func=lambda: self.atype and abs(self.atype) == 1 and self.intype == 1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT295_CARD9,
                active_func=lambda: self.atype and abs(self.atype) == 1 and self.actype in [1,2,3,4,5],
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT295_CARD10,
                active_func=lambda: self.atype and abs(self.atype) == 1 and self.actype in [1,2],
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT295_CARD11,
                active_func=lambda: self.atype and abs(self.atype) == 1 and self.actype == 1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT295_CARD12,
                active_func=lambda: self.atype and abs(self.atype) == 1 and self.actype == 2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT295_CARD13,
                active_func=lambda: self.atype and abs(self.atype) == 1 and self.actype == 3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT295_CARD14,
                active_func=lambda: self.atype and abs(self.atype) == 1 and self.actype == 4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT295_CARD15,
                active_func=lambda: self.atype and abs(self.atype) == 1 and self.actype == 4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT295_CARD16,
                active_func=lambda: self.atype and abs(self.atype) == 1 and self.actype == 5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT295_CARD17,
                active_func=lambda: self.atype and abs(self.atype) == 1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT295_CARD18,
                active_func=lambda: self.atype and abs(self.atype) == 1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat295.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT295_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def theta(self) -> typing.Optional[float]:
        """Get or set the theta
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].theta

    @theta.setter
    def theta(self, value: float) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].theta = value

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the a
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].a

    @a.setter
    def a(self, value: float) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].a = value

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the b
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].b

    @b.setter
    def b(self, value: float) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].b = value

    @property
    def fcid(self) -> typing.Optional[int]:
        """Get or set the fcid
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].fcid

    @fcid.setter
    def fcid(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].fcid = value

    @property
    def k1(self) -> typing.Optional[float]:
        """Get or set the k1
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].k1

    @k1.setter
    def k1(self, value: float) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].k1 = value

    @property
    def k2(self) -> typing.Optional[float]:
        """Get or set the k2
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].k2

    @k2.setter
    def k2(self, value: float) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].k2 = value

    @property
    def flcid(self) -> typing.Optional[int]:
        """Get or set the flcid
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].flcid

    @flcid.setter
    def flcid(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].flcid = value

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the e
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].e

    @e.setter
    def e(self, value: float) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].e = value

    @property
    def r0norm(self) -> typing.Optional[float]:
        """Get or set the r0norm
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].r0norm

    @r0norm.setter
    def r0norm(self, value: float) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].r0norm = value

    @property
    def h0norm(self) -> typing.Optional[float]:
        """Get or set the h0norm
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].h0norm

    @h0norm.setter
    def h0norm(self, value: float) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].h0norm = value

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification.  A unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def rho(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("rho")

    @rho.setter
    def rho(self, value: float) -> None:
        """Set the rho property."""
        self._cards[0].set_value("rho", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option (see *MAT_002 for a more complete description):
        EQ.0.0:	Locally orthotropic with material axes determined by element nodes.The a - direction is from node 1 to node 2 of the element.The b - direction is orthogonal to the a - direction and is in the plane formed by nodes 1, 2,and 4. For shells only, the material axes are then rotated about the normal vector to the surface of the shell by the angle BETA.
        EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center; this is the a - direction.This option is for solid elements only.
        EQ.2.0:	Globally orthotropic with material axes determined by vectors a and d input below, as with* DEFINE_COORDINATE_VECTOR
        EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.Note that for solids, the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying BETA depending on MACF.
        EQ.4.0 : Locally orthotropic in a cylindrical coordinate system with the material axes determined by a vector v,and an originating point, P, which define the centerline axis.This option is for solid elements only.
        LT.0.0 : | AOPT | is a coordinate system ID(see * DEFINE_COORDINATE_OPTION).
        """ # nopep8
        return self._cards[0].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        """Set the aopt property."""
        self._cards[0].set_value("aopt", value)

    @property
    def isotropic_title(self) -> str:
        """Get or set the Module title.

        Note: Location in manual: card:2, field: TITLE
        """ # nopep8
        return self._cards[1].get_value("title")

    @property
    def itype(self) -> typing.Optional[int]:
        """Get or set the Type of isotropic model (see remarks 1 and 2):
        EQ.-1/+1:	compressible/nearly-incompressible Ogden [12] (see notes 1-3)
        EQ.-2:	Yeoh [13]
        EQ.-3/+3:	compressible/nearly-incompressible Holzapfel-Ogden [1], [7].
        """ # nopep8
        return self._cards[1].get_value("itype")

    @itype.setter
    def itype(self, value: int) -> None:
        """Set the itype property."""
        self._cards[1].set_value("itype", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Volumetric response function coefficient.

        Note: For the BETA (Card 21), use ``material_angle_beta``.
        """ # nopep8
        return self._cards[1].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[1].set_value("beta", value)

    @property
    def nu(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio (see remark 3).
        """ # nopep8
        return self._cards[1].get_value("nu")

    @nu.setter
    def nu(self, value: float) -> None:
        """Set the nu property."""
        self._cards[1].set_value("nu", value)

    @property
    def mu1(self) -> typing.Optional[float]:
        """Get or set the Ogden moduli, with i = 1,…,8.
        """ # nopep8
        return self._cards[2].get_value("mu1")

    @mu1.setter
    def mu1(self, value: float) -> None:
        """Set the mu1 property."""
        self._cards[2].set_value("mu1", value)

    @property
    def mu2(self) -> typing.Optional[float]:
        """Get or set the Ogden moduli, with i = 1,…,8.
        """ # nopep8
        return self._cards[2].get_value("mu2")

    @mu2.setter
    def mu2(self, value: float) -> None:
        """Set the mu2 property."""
        self._cards[2].set_value("mu2", value)

    @property
    def mu3(self) -> typing.Optional[float]:
        """Get or set the Ogden moduli, with i = 1,…,8.
        """ # nopep8
        return self._cards[2].get_value("mu3")

    @mu3.setter
    def mu3(self, value: float) -> None:
        """Set the mu3 property."""
        self._cards[2].set_value("mu3", value)

    @property
    def mu4(self) -> typing.Optional[float]:
        """Get or set the Ogden moduli, with i = 1,…,8.
        """ # nopep8
        return self._cards[2].get_value("mu4")

    @mu4.setter
    def mu4(self, value: float) -> None:
        """Set the mu4 property."""
        self._cards[2].set_value("mu4", value)

    @property
    def mu5(self) -> typing.Optional[float]:
        """Get or set the Ogden moduli, with i = 1,…,8.
        """ # nopep8
        return self._cards[2].get_value("mu5")

    @mu5.setter
    def mu5(self, value: float) -> None:
        """Set the mu5 property."""
        self._cards[2].set_value("mu5", value)

    @property
    def mu6(self) -> typing.Optional[float]:
        """Get or set the Ogden moduli, with i = 1,…,8.
        """ # nopep8
        return self._cards[2].get_value("mu6")

    @mu6.setter
    def mu6(self, value: float) -> None:
        """Set the mu6 property."""
        self._cards[2].set_value("mu6", value)

    @property
    def mu7(self) -> typing.Optional[float]:
        """Get or set the Ogden moduli, with i = 1,…,8.
        """ # nopep8
        return self._cards[2].get_value("mu7")

    @mu7.setter
    def mu7(self, value: float) -> None:
        """Set the mu7 property."""
        self._cards[2].set_value("mu7", value)

    @property
    def mu8(self) -> typing.Optional[float]:
        """Get or set the Ogden moduli, with i = 1,…,8.
        """ # nopep8
        return self._cards[2].get_value("mu8")

    @mu8.setter
    def mu8(self, value: float) -> None:
        """Set the mu8 property."""
        self._cards[2].set_value("mu8", value)

    @property
    def alpha1(self) -> typing.Optional[float]:
        """Get or set the Ogden constants, with i = 1,…,8.
        """ # nopep8
        return self._cards[3].get_value("alpha1")

    @alpha1.setter
    def alpha1(self, value: float) -> None:
        """Set the alpha1 property."""
        self._cards[3].set_value("alpha1", value)

    @property
    def alpha2(self) -> typing.Optional[float]:
        """Get or set the Ogden constants, with i = 1,…,8.
        """ # nopep8
        return self._cards[3].get_value("alpha2")

    @alpha2.setter
    def alpha2(self, value: float) -> None:
        """Set the alpha2 property."""
        self._cards[3].set_value("alpha2", value)

    @property
    def alpha3(self) -> typing.Optional[float]:
        """Get or set the Ogden constants, with i = 1,…,8.
        """ # nopep8
        return self._cards[3].get_value("alpha3")

    @alpha3.setter
    def alpha3(self, value: float) -> None:
        """Set the alpha3 property."""
        self._cards[3].set_value("alpha3", value)

    @property
    def alpha4(self) -> typing.Optional[float]:
        """Get or set the Ogden constants, with i = 1,…,8.
        """ # nopep8
        return self._cards[3].get_value("alpha4")

    @alpha4.setter
    def alpha4(self, value: float) -> None:
        """Set the alpha4 property."""
        self._cards[3].set_value("alpha4", value)

    @property
    def alpha5(self) -> typing.Optional[float]:
        """Get or set the Ogden constants, with i = 1,…,8.
        """ # nopep8
        return self._cards[3].get_value("alpha5")

    @alpha5.setter
    def alpha5(self, value: float) -> None:
        """Set the alpha5 property."""
        self._cards[3].set_value("alpha5", value)

    @property
    def alpha6(self) -> typing.Optional[float]:
        """Get or set the Ogden constants, with i = 1,…,8.
        """ # nopep8
        return self._cards[3].get_value("alpha6")

    @alpha6.setter
    def alpha6(self, value: float) -> None:
        """Set the alpha6 property."""
        self._cards[3].set_value("alpha6", value)

    @property
    def alpha7(self) -> typing.Optional[float]:
        """Get or set the Ogden constants, with i = 1,…,8.
        """ # nopep8
        return self._cards[3].get_value("alpha7")

    @alpha7.setter
    def alpha7(self, value: float) -> None:
        """Set the alpha7 property."""
        self._cards[3].set_value("alpha7", value)

    @property
    def alpha8(self) -> typing.Optional[float]:
        """Get or set the Ogden constants, with i = 1,…,8.
        """ # nopep8
        return self._cards[3].get_value("alpha8")

    @alpha8.setter
    def alpha8(self, value: float) -> None:
        """Set the alpha8 property."""
        self._cards[3].set_value("alpha8", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Yeoh moduli, with i = 1,2,3.
        """ # nopep8
        return self._cards[4].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        """Set the c1 property."""
        self._cards[4].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Yeoh moduli, with i = 1,2,3.
        """ # nopep8
        return self._cards[4].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        """Set the c2 property."""
        self._cards[4].set_value("c2", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the Yeoh moduli, with i = 1,2,3.
        """ # nopep8
        return self._cards[4].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        """Set the c3 property."""
        self._cards[4].set_value("c3", value)

    @property
    def k1(self) -> typing.Optional[float]:
        """Get or set the Holzapfel-Ogden modulus.

        Note: For the K1 (Card 11), use ``coupling_k1``.
        """ # nopep8
        return self._cards[5].get_value("k1")

    @k1.setter
    def k1(self, value: float) -> None:
        """Set the k1 property."""
        self._cards[5].set_value("k1", value)

    @property
    def k2(self) -> typing.Optional[float]:
        """Get or set the Holzapfel-Ogden constant.

        Note: For the K2 (Card 11), use ``coupling_k2``.
        """ # nopep8
        return self._cards[5].get_value("k2")

    @k2.setter
    def k2(self, value: float) -> None:
        """Set the k2 property."""
        self._cards[5].set_value("k2", value)

    @property
    def anisotropic_title(self) -> str:
        """Get or set the Module title.

        Note: Location in manual: card:7, field: TITLE
        """ # nopep8
        return self._cards[6].get_value("title")

    @property
    def atype(self) -> typing.Optional[int]:
        """Get or set the Type of anisotropic model:
        EQ.-1/+1:	general structure tensor-based, see Holzapfel et al. [8] (see remark 4 and note 4)
        """ # nopep8
        return self._cards[6].get_value("atype")

    @atype.setter
    def atype(self, value: int) -> None:
        """Set the atype property."""
        self._cards[6].set_value("atype", value)

    @property
    def intype(self) -> typing.Optional[int]:
        """Get or set the Type of interaction between the fiber families (see remarks 5 and 6):
        EQ.0:	none
        EQ.1:	Holzapfel-Ogden [1], [5].
        """ # nopep8
        return self._cards[6].get_value("intype")

    @intype.setter
    def intype(self, value: int) -> None:
        """Set the intype property."""
        self._cards[6].set_value("intype", value)

    @property
    def nf(self) -> typing.Optional[int]:
        """Get or set the Number of fiber families (see remark 4).
        """ # nopep8
        return self._cards[6].get_value("nf")

    @nf.setter
    def nf(self, value: int) -> None:
        """Set the nf property."""
        self._cards[6].set_value("nf", value)

    @property
    def fiber_families(self) -> typing.List[FiberFamily]:
        """Gets the list of fiber_families."""
        return self._cards[7].items()

    @property
    def coupling_k1(self) -> typing.Optional[float]:
        """Get or set the Coupling modulus between the fiber and sheet directions

        Note: Location in manual: card:11, field: K1
        """ # nopep8
        return self._cards[8].get_value("k1")

    @coupling_k1.setter
    def coupling_k1(self, value: float) -> None:
        """Set the coupling_k1 property."""
        self._cards[8].set_value("k1", value)

    @property
    def coupling_k2(self) -> typing.Optional[float]:
        """Get or set the Coupling constant between the fiber and sheet directions

        Note: Location in manual: card:11, field: K2
        """ # nopep8
        return self._cards[8].get_value("k2")

    @coupling_k2.setter
    def coupling_k2(self, value: float) -> None:
        """Set the coupling_k2 property."""
        self._cards[8].set_value("k2", value)

    @property
    def active_title(self) -> str:
        """Get or set the Module title.

        Note: Location in manual: card:12, field: TITLE
        """ # nopep8
        return self._cards[9].get_value("title")

    @property
    def actype(self) -> typing.Optional[int]:
        """Get or set the Type of active model:
        EQ.1:	Guccione-Waldman-McCulloch [4]
        EQ.2:	Guccione-Waldman-McCulloch [4] and Hunter-Nash-Sands [9]
        EQ.3:	Hunter-Nash-Sands	[9]
        EQ.4:	Hunter-Nash-Sands [9] and Hunter-McCulloch-ter Keurs [10].
        EQ.5: Martins-Pato-Pires [14]
        """ # nopep8
        return self._cards[9].get_value("actype")

    @actype.setter
    def actype(self, value: int) -> None:
        """Set the actype property."""
        self._cards[9].set_value("actype", value)

    @property
    def acdir(self) -> int:
        """Get or set the Direction of active tension: GT.0:	Active tension develops along the mean orientation of the ACDIRth fiber family.
        """ # nopep8
        return self._cards[9].get_value("acdir")

    @acdir.setter
    def acdir(self, value: int) -> None:
        """Set the acdir property."""
        self._cards[9].set_value("acdir", value)

    @property
    def acid(self) -> typing.Optional[int]:
        """Get or set the Activation curve ID (takes priority over T0 for ACTYPE = 1, 2, 3, or 4 when defined, see Remark 8
        """ # nopep8
        return self._cards[9].get_value("acid")

    @acid.setter
    def acid(self, value: int) -> None:
        """Set the acid property."""
        self._cards[9].set_value("acid", value)

    @property
    def acthr(self) -> float:
        """Get or set the (De/re)activation threshold (see Remark 8)
        """ # nopep8
        return self._cards[9].get_value("acthr")

    @acthr.setter
    def acthr(self, value: float) -> None:
        """Set the acthr property."""
        self._cards[9].set_value("acthr", value)

    @property
    def sf(self) -> float:
        """Get or set the Active stress scaling factor in the fiber direction (see Remark 9)
        """ # nopep8
        return self._cards[9].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[9].set_value("sf", value)

    @property
    def ss(self) -> float:
        """Get or set the Active stress scaling factor in the transverse sheet direction (see Remark 9)
        """ # nopep8
        return self._cards[9].get_value("ss")

    @ss.setter
    def ss(self, value: float) -> None:
        """Set the ss property."""
        self._cards[9].set_value("ss", value)

    @property
    def sn(self) -> float:
        """Get or set the Active stress scaling factor in the transverse normal direction (see Remark 9)
        """ # nopep8
        return self._cards[9].get_value("sn")

    @sn.setter
    def sn(self, value: float) -> None:
        """Set the sn property."""
        self._cards[9].set_value("sn", value)

    @property
    def t0(self) -> typing.Optional[float]:
        """Get or set the Starting time of active stress development.
        """ # nopep8
        return self._cards[10].get_value("t0")

    @t0.setter
    def t0(self, value: float) -> None:
        """Set the t0 property."""
        self._cards[10].set_value("t0", value)
        self._cards[13].set_value("t0", value)
        self._cards[14].set_value("t0", value)

    @property
    def ca2ion(self) -> typing.Optional[float]:
        """Get or set the Intercellular calcium ion concentration
        """ # nopep8
        return self._cards[10].get_value("ca2ion")

    @ca2ion.setter
    def ca2ion(self, value: float) -> None:
        """Set the ca2ion property."""
        self._cards[10].set_value("ca2ion", value)
        self._cards[13].set_value("ca2ion", value)

    @property
    def ca2ionm(self) -> typing.Optional[float]:
        """Get or set the Maximum intercellular calcium ion concentration.
        """ # nopep8
        return self._cards[10].get_value("ca2ionm")

    @ca2ionm.setter
    def ca2ionm(self, value: float) -> None:
        """Set the ca2ionm property."""
        self._cards[10].set_value("ca2ionm", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Hill coefficient.
        """ # nopep8
        return self._cards[10].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[10].set_value("n", value)
        self._cards[13].set_value("n", value)
        self._cards[14].set_value("n", value)

    @property
    def taumax(self) -> typing.Optional[float]:
        """Get or set the Peak isometric tension under maximum activation.
        """ # nopep8
        return self._cards[10].get_value("taumax")

    @taumax.setter
    def taumax(self, value: float) -> None:
        """Set the taumax property."""
        self._cards[10].set_value("taumax", value)

    @property
    def stf(self) -> typing.Optional[float]:
        """Get or set the Transverse fiber stress scaling factor.
        """ # nopep8
        return self._cards[10].get_value("stf")

    @stf.setter
    def stf(self, value: float) -> None:
        """Set the stf property."""
        self._cards[10].set_value("stf", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Shape coefficient.
        """ # nopep8
        return self._cards[10].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[10].set_value("b", value)

    @property
    def l0(self) -> typing.Optional[float]:
        """Get or set the Sarcomere length with no active tension.
        """ # nopep8
        return self._cards[10].get_value("l0")

    @l0.setter
    def l0(self, value: float) -> None:
        """Set the l0 property."""
        self._cards[10].set_value("l0", value)

    @property
    def l(self) -> typing.Optional[float]:
        """Get or set the Reference (stress-free) sarcomere length.
        """ # nopep8
        return self._cards[11].get_value("l")

    @l.setter
    def l(self, value: float) -> None:
        """Set the l property."""
        self._cards[11].set_value("l", value)
        self._cards[12].set_value("l", value)
        self._cards[13].set_value("l", value)
        self._cards[15].set_value("l", value)

    @property
    def dtmax(self) -> typing.Optional[float]:
        """Get or set the Time to peak tension.
        """ # nopep8
        return self._cards[11].get_value("dtmax")

    @dtmax.setter
    def dtmax(self, value: float) -> None:
        """Set the dtmax property."""
        self._cards[11].set_value("dtmax", value)

    @property
    def mr(self) -> typing.Optional[float]:
        """Get or set the Slope of linear relaxation versus sarcomere length relation.
        """ # nopep8
        return self._cards[11].get_value("mr")

    @mr.setter
    def mr(self, value: float) -> None:
        """Set the mr property."""
        self._cards[11].set_value("mr", value)

    @property
    def tr(self) -> typing.Optional[float]:
        """Get or set the Time intercept of linear relaxation versus sarcomere length relation.
        """ # nopep8
        return self._cards[11].get_value("tr")

    @tr.setter
    def tr(self, value: float) -> None:
        """Set the tr property."""
        self._cards[11].set_value("tr", value)

    @property
    def eta(self) -> typing.Optional[float]:
        """Get or set the Scaling parameter.
        """ # nopep8
        return self._cards[12].get_value("eta")

    @eta.setter
    def eta(self, value: float) -> None:
        """Set the eta property."""
        self._cards[12].set_value("eta", value)
        self._cards[13].set_value("eta", value)
        self._cards[15].set_value("eta", value)

    @property
    def ca2ion50(self) -> typing.Optional[float]:
        """Get or set the Intercellular calcium ion concentration at half of peak isometric tension.
        """ # nopep8
        return self._cards[13].get_value("ca2ion50")

    @ca2ion50.setter
    def ca2ion50(self, value: float) -> None:
        """Set the ca2ion50 property."""
        self._cards[13].set_value("ca2ion50", value)
        self._cards[14].set_value("ca2ion50", value)

    @property
    def sigmax(self) -> typing.Optional[float]:
        """Get or set the Peak isometric tension under maximum activation.
        """ # nopep8
        return self._cards[13].get_value("sigmax")

    @sigmax.setter
    def sigmax(self, value: float) -> None:
        """Set the sigmax property."""
        self._cards[13].set_value("sigmax", value)
        self._cards[14].set_value("sigmax", value)

    @property
    def f(self) -> typing.Optional[float]:
        """Get or set the Transverse fiber stress scaling factor.
        """ # nopep8
        return self._cards[13].get_value("f")

    @f.setter
    def f(self, value: float) -> None:
        """Set the f property."""
        self._cards[13].set_value("f", value)
        self._cards[14].set_value("f", value)

    @property
    def ca2ionmax(self) -> typing.Optional[float]:
        """Get or set the Maximum intercellular calcium ion concentration.
        """ # nopep8
        return self._cards[14].get_value("ca2ionmax")

    @ca2ionmax.setter
    def ca2ionmax(self, value: float) -> None:
        """Set the ca2ionmax property."""
        self._cards[14].set_value("ca2ionmax", value)

    @property
    def ca2ion0(self) -> typing.Optional[float]:
        """Get or set the Intercellular calcium ion concentration at rest.
        """ # nopep8
        return self._cards[14].get_value("ca2ion0")

    @ca2ion0.setter
    def ca2ion0(self, value: float) -> None:
        """Set the ca2ion0 property."""
        self._cards[14].set_value("ca2ion0", value)

    @property
    def tca(self) -> typing.Optional[float]:
        """Get or set the Shape coefficient.
        """ # nopep8
        return self._cards[14].get_value("tca")

    @tca.setter
    def tca(self, value: float) -> None:
        """Set the tca property."""
        self._cards[14].set_value("tca", value)

    @property
    def fseid(self) -> typing.Optional[int]:
        """Get or set the Serial stress function ID
        """ # nopep8
        return self._cards[16].get_value("fseid")

    @fseid.setter
    def fseid(self, value: int) -> None:
        """Set the fseid property."""
        self._cards[16].set_value("fseid", value)

    @property
    def flid(self) -> typing.Optional[int]:
        """Get or set the Normalized force-contractile stretch curve ID
        """ # nopep8
        return self._cards[16].get_value("flid")

    @flid.setter
    def flid(self, value: int) -> None:
        """Set the flid property."""
        self._cards[16].set_value("flid", value)

    @property
    def fvid(self) -> typing.Optional[int]:
        """Get or set the Normalized force-contractile stretch rate curve ID
        """ # nopep8
        return self._cards[16].get_value("fvid")

    @fvid.setter
    def fvid(self, value: int) -> None:
        """Set the fvid property."""
        self._cards[16].set_value("fvid", value)

    @property
    def alphaid(self) -> typing.Optional[int]:
        """Get or set the Activation curve ID
        """ # nopep8
        return self._cards[16].get_value("alphaid")

    @alphaid.setter
    def alphaid(self, value: int) -> None:
        """Set the alphaid property."""
        self._cards[16].set_value("alphaid", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point  for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[17].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[17].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point  for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[17].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[17].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point  for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[17].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[17].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector  for AOPT = 2.
        """ # nopep8
        return self._cards[17].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[17].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector  for AOPT = 2.
        """ # nopep8
        return self._cards[17].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[17].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector  for AOPT = 2.
        """ # nopep8
        return self._cards[17].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[17].set_value("a3", value)

    @property
    def macf(self) -> int:
        """Get or set the Material axes change flag for brick elements:
        EQ.1:	no change (default)
        EQ.2:	switch material axes a and b
        EQ.3:	switch material axes a and c
        EQ.4:	switch material axes b and c.
        """ # nopep8
        return self._cards[17].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        """Set the macf property."""
        if value not in [1, 2, 3, 4, None]:
            raise Exception("""macf must be `None` or one of {1,2,3,4}.""")
        self._cards[17].set_value("macf", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Components of vector  for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[18].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[18].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Components of vector  for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[18].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[18].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Components of vector  for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[18].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[18].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector  for AOPT = 2.
        """ # nopep8
        return self._cards[18].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[18].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector  for AOPT = 2.
        """ # nopep8
        return self._cards[18].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[18].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector  for AOPT = 2.
        """ # nopep8
        return self._cards[18].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[18].set_value("d3", value)

    @property
    def material_angle_beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 0 (shells and thick shells only) and AOPT = 3 (all element types).
This angle may be overridden on the element card;
see *ELEMENT_SHELL_BETA, *ELEMENT_TSHELL_BETA, and *ELEMENT_SOLID_ORTHO.

        Note: Location in manual: card:21, field: BETA
        """ # nopep8
        return self._cards[18].get_value("beta")

    @material_angle_beta.setter
    def material_angle_beta(self, value: float) -> None:
        """Set the material_angle_beta property."""
        self._cards[18].set_value("beta", value)

    @property
    def ref(self) -> typing.Optional[float]:
        """Get or set the Use reference geometry to initialize the stress tensor. The reference geometry is defined by the keyword:
        *INITIAL_FOAM_REFERENCE_GEOMETRY.
        EQ.0.0:	off
        EQ.1.0:	on.
        """ # nopep8
        return self._cards[18].get_value("ref")

    @ref.setter
    def ref(self, value: float) -> None:
        """Set the ref property."""
        self._cards[18].set_value("ref", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[19].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[19].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")


class MatAnisotropicHyperelastic(Mat295):
    """Alias for MAT keyword."""
    subkeyword = "ANISOTROPIC_HYPERELASTIC"
