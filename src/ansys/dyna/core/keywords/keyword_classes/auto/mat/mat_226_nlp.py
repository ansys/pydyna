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

"""Module providing the Mat226Nlp class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MAT226NLP_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("m", float, 40, 10, None),
    FieldSchema("r00", float, 50, 10, None),
    FieldSchema("r45", float, 60, 10, None),
    FieldSchema("r90", float, 70, 10, None),
)

_MAT226NLP_CARD1 = (
    FieldSchema("cb", float, 0, 10, None),
    FieldSchema("y", float, 10, 10, None),
    FieldSchema("sc", float, 20, 10, None),
    FieldSchema("k", float, 30, 10, None),
    FieldSchema("rsat", float, 40, 10, None),
    FieldSchema("sb", float, 50, 10, None),
    FieldSchema("h", float, 60, 10, None),
    FieldSchema("hlcid", int, 70, 10, None),
)

_MAT226NLP_CARD2 = (
    FieldSchema("aopt", float, 0, 10, None),
    FieldSchema("iopt", int, 10, 10, 0),
    FieldSchema("c1", float, 20, 10, None),
    FieldSchema("c2", float, 30, 10, None),
    FieldSchema("ifld", int, 40, 10, None),
    FieldSchema("ea", float, 50, 10, None),
    FieldSchema("coe", float, 60, 10, None),
)

_MAT226NLP_CARD3 = (
    FieldSchema("xp", float, 0, 10, None),
    FieldSchema("yp", float, 10, 10, None),
    FieldSchema("zp", float, 20, 10, None),
    FieldSchema("a1", float, 30, 10, None),
    FieldSchema("a2", float, 40, 10, None),
    FieldSchema("a3", float, 50, 10, None),
)

_MAT226NLP_CARD4 = (
    FieldSchema("v1", float, 0, 10, None),
    FieldSchema("v2", float, 10, 10, None),
    FieldSchema("v3", float, 20, 10, None),
    FieldSchema("d1", float, 30, 10, None),
    FieldSchema("d2", float, 40, 10, None),
    FieldSchema("d3", float, 50, 10, None),
    FieldSchema("beta", float, 60, 10, None),
)

_MAT226NLP_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat226Nlp(KeywordBase):
    """DYNA MAT_226_NLP keyword"""

    keyword = "MAT"
    subkeyword = "226_NLP"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "hlcid": LinkType.DEFINE_CURVE,
        "ifld": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the Mat226Nlp class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT226NLP_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT226NLP_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT226NLP_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT226NLP_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT226NLP_CARD4,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat226Nlp.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT226NLP_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number must be specified.
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
        """Get or set the Young's Modulus.
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
    def m(self) -> typing.Optional[float]:
        """Get or set the m, exponent in Barlat's yield criterion.
        """ # nopep8
        return self._cards[0].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[0].set_value("m", value)

    @property
    def r00(self) -> typing.Optional[float]:
        """Get or set the R00, Lankford parameter in 0 degree direction.
        """ # nopep8
        return self._cards[0].get_value("r00")

    @r00.setter
    def r00(self, value: float) -> None:
        """Set the r00 property."""
        self._cards[0].set_value("r00", value)

    @property
    def r45(self) -> typing.Optional[float]:
        """Get or set the R45, Lankford parameter in 45 degree direction.
        """ # nopep8
        return self._cards[0].get_value("r45")

    @r45.setter
    def r45(self, value: float) -> None:
        """Set the r45 property."""
        self._cards[0].set_value("r45", value)

    @property
    def r90(self) -> typing.Optional[float]:
        """Get or set the R90, Lankford parameter in 90 degree direction.
        """ # nopep8
        return self._cards[0].get_value("r90")

    @r90.setter
    def r90(self, value: float) -> None:
        """Set the r90 property."""
        self._cards[0].set_value("r90", value)

    @property
    def cb(self) -> typing.Optional[float]:
        """Get or set the The uppercase B defined in the Yoshida's equations.
        """ # nopep8
        return self._cards[1].get_value("cb")

    @cb.setter
    def cb(self, value: float) -> None:
        """Set the cb property."""
        self._cards[1].set_value("cb", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the Hardening parameter as defined in the Yoshida's equations.
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[1].set_value("y", value)

    @property
    def sc(self) -> typing.Optional[float]:
        """Get or set the The lowercase c defined in the Yoshida's equations.
        """ # nopep8
        return self._cards[1].get_value("sc")

    @sc.setter
    def sc(self, value: float) -> None:
        """Set the sc property."""
        self._cards[1].set_value("sc", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Hardening parameter as defined in the Yoshida's equations.
        """ # nopep8
        return self._cards[1].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        """Set the k property."""
        self._cards[1].set_value("k", value)

    @property
    def rsat(self) -> typing.Optional[float]:
        """Get or set the Hardening parameter as defined in the Yoshida's equations.
        """ # nopep8
        return self._cards[1].get_value("rsat")

    @rsat.setter
    def rsat(self, value: float) -> None:
        """Set the rsat property."""
        self._cards[1].set_value("rsat", value)

    @property
    def sb(self) -> typing.Optional[float]:
        """Get or set the The lowercase b as defined in the Yoshida's equations
        """ # nopep8
        return self._cards[1].get_value("sb")

    @sb.setter
    def sb(self, value: float) -> None:
        """Set the sb property."""
        self._cards[1].set_value("sb", value)

    @property
    def h(self) -> typing.Optional[float]:
        """Get or set the Anisotropic parameter associated with work-hardening stagnation, defined in the Yoshida's equations.
        """ # nopep8
        return self._cards[1].get_value("h")

    @h.setter
    def h(self, value: float) -> None:
        """Set the h property."""
        self._cards[1].set_value("h", value)

    @property
    def hlcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID in keyword *DEFINE_CURVE, where true strain and true	stress relationship is characterized.
        """ # nopep8
        return self._cards[1].get_value("hlcid")

    @hlcid.setter
    def hlcid(self, value: int) -> None:
        """Set the hlcid property."""
        self._cards[1].set_value("hlcid", value)

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
        return self._cards[2].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        """Set the aopt property."""
        self._cards[2].set_value("aopt", value)

    @property
    def iopt(self) -> int:
        """Get or set the Kinematic hardening rule flag:
        EQ.0: Original Yoshida formulation,
        EQ.1: Modified formulation. Define C1, C2 below.
        """ # nopep8
        return self._cards[2].get_value("iopt")

    @iopt.setter
    def iopt(self, value: int) -> None:
        """Set the iopt property."""
        if value not in [0, 1, None]:
            raise Exception("""iopt must be `None` or one of {0,1}.""")
        self._cards[2].set_value("iopt", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Constants used to modify R:
        """ # nopep8
        return self._cards[2].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        """Set the c1 property."""
        self._cards[2].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Constants used to modify R:
        """ # nopep8
        return self._cards[2].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        """Set the c2 property."""
        self._cards[2].set_value("c2", value)

    @property
    def ifld(self) -> typing.Optional[int]:
        """Get or set the ID of a load curve of the traditional Forming Limit Diagram (FLD) for the linear strain paths.  In the load curve, abscissas represent minor strains while ordinates represent major strains.  Define only when the NLP option is used.
        """ # nopep8
        return self._cards[2].get_value("ifld")

    @ifld.setter
    def ifld(self, value: int) -> None:
        """Set the ifld property."""
        self._cards[2].set_value("ifld", value)

    @property
    def ea(self) -> typing.Optional[float]:
        """Get or set the Variable controlling the change of Young’s modulus, E^A
        """ # nopep8
        return self._cards[2].get_value("ea")

    @ea.setter
    def ea(self, value: float) -> None:
        """Set the ea property."""
        self._cards[2].set_value("ea", value)

    @property
    def coe(self) -> typing.Optional[float]:
        """Get or set the Variable controlling the change of Young’s modulus
        """ # nopep8
        return self._cards[2].get_value("coe")

    @coe.setter
    def coe(self, value: float) -> None:
        """Set the coe property."""
        self._cards[2].set_value("coe", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[3].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[3].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[3].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[3].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[3].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[3].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[3].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[3].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2
        """ # nopep8
        return self._cards[3].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[3].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[4].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[4].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[4].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[4].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[4].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[4].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[4].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[4].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2
        """ # nopep8
        return self._cards[4].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[4].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT=3, may be overridden on the element card, see *ELEMENT_SHELL_BETA
        """ # nopep8
        return self._cards[4].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[4].set_value("beta", value)

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

    @property
    def hlcid_link(self) -> DefineCurve:
        """Get the DefineCurve object for hlcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.hlcid:
                return kwd
        return None

    @hlcid_link.setter
    def hlcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for hlcid."""
        self.hlcid = value.lcid

    @property
    def ifld_link(self) -> DefineCurve:
        """Get the DefineCurve object for ifld."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.ifld:
                return kwd
        return None

    @ifld_link.setter
    def ifld_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for ifld."""
        self.ifld = value.lcid

