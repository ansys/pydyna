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

"""Module providing the Mat040 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MAT040_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("ea", float, 20, 10, None),
    FieldSchema("eb", float, 30, 10, None),
    FieldSchema("ec", float, 40, 10, None),
    FieldSchema("prba", float, 50, 10, None),
    FieldSchema("prca", float, 60, 10, None),
    FieldSchema("prcb", float, 70, 10, None),
)

_MAT040_CARD1 = (
    FieldSchema("gab", float, 0, 10, None),
    FieldSchema("gbc", float, 10, 10, None),
    FieldSchema("gca", float, 20, 10, None),
    FieldSchema("dt", float, 30, 10, None),
    FieldSchema("tramp", float, 40, 10, None),
    FieldSchema("alpha", float, 50, 10, None),
)

_MAT040_CARD2 = (
    FieldSchema("lcida", float, 0, 10, 0.0),
    FieldSchema("lcidb", float, 10, 10, 0.0),
    FieldSchema("efail", float, 20, 10, None),
    FieldSchema("dtfail", float, 30, 10, None),
    FieldSchema("cdamp", float, 40, 10, None),
    FieldSchema("aopt", float, 50, 10, None),
    FieldSchema("macf", int, 60, 10, 1),
    FieldSchema("atrack", int, 70, 10, 0),
)

_MAT040_CARD3 = (
    FieldSchema("xp", float, 0, 10, None),
    FieldSchema("yp", float, 10, 10, None),
    FieldSchema("zp", float, 20, 10, None),
    FieldSchema("a1", float, 30, 10, None),
    FieldSchema("a2", float, 40, 10, None),
    FieldSchema("a3", float, 50, 10, None),
)

_MAT040_CARD4 = (
    FieldSchema("v1", float, 0, 10, None),
    FieldSchema("v2", float, 10, 10, None),
    FieldSchema("v3", float, 20, 10, None),
    FieldSchema("d1", float, 30, 10, None),
    FieldSchema("d2", float, 40, 10, None),
    FieldSchema("d3", float, 50, 10, None),
    FieldSchema("beta", float, 60, 10, None),
)

_MAT040_CARD5 = (
    FieldSchema("lcidc", int, 0, 10, 0),
    FieldSchema("lcidab", int, 10, 10, 0),
    FieldSchema("lcidbc", int, 20, 10, 0),
    FieldSchema("lcidca", int, 30, 10, 0),
)

_MAT040_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat040(KeywordBase):
    """DYNA MAT_040 keyword"""

    keyword = "MAT"
    subkeyword = "040"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcida": LinkType.DEFINE_CURVE,
        "lcidb": LinkType.DEFINE_CURVE,
        "lcidc": LinkType.DEFINE_CURVE,
        "lcidab": LinkType.DEFINE_CURVE,
        "lcidbc": LinkType.DEFINE_CURVE,
        "lcidca": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the Mat040 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT040_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT040_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT040_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT040_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT040_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT040_CARD5,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat040.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT040_OPTION0_CARD0,
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
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def ea(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in a-direction.
        """ # nopep8
        return self._cards[0].get_value("ea")

    @ea.setter
    def ea(self, value: float) -> None:
        """Set the ea property."""
        self._cards[0].set_value("ea", value)

    @property
    def eb(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in b-direction.
        """ # nopep8
        return self._cards[0].get_value("eb")

    @eb.setter
    def eb(self, value: float) -> None:
        """Set the eb property."""
        self._cards[0].set_value("eb", value)

    @property
    def ec(self) -> typing.Optional[float]:
        """Get or set the Young's modulus in c-direction.
        """ # nopep8
        return self._cards[0].get_value("ec")

    @ec.setter
    def ec(self, value: float) -> None:
        """Set the ec property."""
        self._cards[0].set_value("ec", value)

    @property
    def prba(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio ba.
        """ # nopep8
        return self._cards[0].get_value("prba")

    @prba.setter
    def prba(self, value: float) -> None:
        """Set the prba property."""
        self._cards[0].set_value("prba", value)

    @property
    def prca(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio ca.
        """ # nopep8
        return self._cards[0].get_value("prca")

    @prca.setter
    def prca(self, value: float) -> None:
        """Set the prca property."""
        self._cards[0].set_value("prca", value)

    @property
    def prcb(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio cb.
        """ # nopep8
        return self._cards[0].get_value("prcb")

    @prcb.setter
    def prcb(self, value: float) -> None:
        """Set the prcb property."""
        self._cards[0].set_value("prcb", value)

    @property
    def gab(self) -> typing.Optional[float]:
        """Get or set the Shear modulus ab.
        """ # nopep8
        return self._cards[1].get_value("gab")

    @gab.setter
    def gab(self, value: float) -> None:
        """Set the gab property."""
        self._cards[1].set_value("gab", value)

    @property
    def gbc(self) -> typing.Optional[float]:
        """Get or set the Shear modulus bc.
        """ # nopep8
        return self._cards[1].get_value("gbc")

    @gbc.setter
    def gbc(self, value: float) -> None:
        """Set the gbc property."""
        self._cards[1].set_value("gbc", value)

    @property
    def gca(self) -> typing.Optional[float]:
        """Get or set the Shear modulus ca.
        """ # nopep8
        return self._cards[1].get_value("gca")

    @gca.setter
    def gca(self, value: float) -> None:
        """Set the gca property."""
        self._cards[1].set_value("gca", value)

    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the Temperature increment for isotropic stress initialization. This option can be used during dynamic relaxation.
        """ # nopep8
        return self._cards[1].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[1].set_value("dt", value)

    @property
    def tramp(self) -> typing.Optional[float]:
        """Get or set the Time to ramp up to the final temperature.
        """ # nopep8
        return self._cards[1].get_value("tramp")

    @tramp.setter
    def tramp(self, value: float) -> None:
        """Set the tramp property."""
        self._cards[1].set_value("tramp", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Thermal expansion coefficient.
        """ # nopep8
        return self._cards[1].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[1].set_value("alpha", value)

    @property
    def lcida(self) -> float:
        """Get or set the Optional load curve ID defining the nominal stress versus strain along a-axis.
        """ # nopep8
        return self._cards[2].get_value("lcida")

    @lcida.setter
    def lcida(self, value: float) -> None:
        """Set the lcida property."""
        self._cards[2].set_value("lcida", value)

    @property
    def lcidb(self) -> float:
        """Get or set the Optional load curve ID defining the nominal stress versus strain along b-axis.
        """ # nopep8
        return self._cards[2].get_value("lcidb")

    @lcidb.setter
    def lcidb(self, value: float) -> None:
        """Set the lcidb property."""
        self._cards[2].set_value("lcidb", value)

    @property
    def efail(self) -> typing.Optional[float]:
        """Get or set the Failure strain.
        """ # nopep8
        return self._cards[2].get_value("efail")

    @efail.setter
    def efail(self, value: float) -> None:
        """Set the efail property."""
        self._cards[2].set_value("efail", value)

    @property
    def dtfail(self) -> typing.Optional[float]:
        """Get or set the Time step for automatic element erosion
        """ # nopep8
        return self._cards[2].get_value("dtfail")

    @dtfail.setter
    def dtfail(self, value: float) -> None:
        """Set the dtfail property."""
        self._cards[2].set_value("dtfail", value)

    @property
    def cdamp(self) -> typing.Optional[float]:
        """Get or set the Damping coefficient.
        """ # nopep8
        return self._cards[2].get_value("cdamp")

    @cdamp.setter
    def cdamp(self, value: float) -> None:
        """Set the cdamp property."""
        self._cards[2].set_value("cdamp", value)

    @property
    def aopt(self) -> typing.Optional[float]:
        """Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
        EQ.0.0:	Locally orthotropic with material axes determined by element nodes 1, 2,and 4, as with* DEFINE_COORDINATE_NODES.For shells only, the material axes are then rotated about the normal vector to the surface of the shell by the angle BETA.
        EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center; this is the a - direction.This option is for solid elements only.
        EQ.2.0:	Globally orthotropic with material axes determined by vectors defined below, as with* DEFINE_COORDINATE_VECTOR
        EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, for solid elements, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.Note that for solids, the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying BETA depending on the value of MACF.
        EQ.4.0 : Locally orthotropic in a cylindrical coordinate system with the material axes determined by a vector v,and an originating point, P, which define the centerline axis.This option is for solid elements only.
        LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_OPTION).
        """ # nopep8
        return self._cards[2].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        """Set the aopt property."""
        self._cards[2].set_value("aopt", value)

    @property
    def macf(self) -> int:
        """Get or set the Material axes change flag for solid elements:
        EQ.1 : No change, default
        EQ.2 : Switch material axes a and b after BETA rotation
        EQ.3 : Switch material axes a and c after BETA rotation
        EQ.4 : Switch material axes b and c after BETA rotation
        EQ. - 4 : Switch material axes b and c before BETA rotation
        EQ. - 3 : Switch material axes a and c before BETA rotation
        EQ. - 2 : Switch material axes a and b before BETA rotation
        Figure Error!Reference source not found.indicates when LS - DYNA applies MACF during the process to obtain the final material axes.If BETA on * ELEMENT_SOLID_{OPTION} is defined, then that BETA is used for the rotation for all AOPT options.Otherwise, if AOPT = 3, the BETA input on Card 3 rotates the axes.For all other values of AOPT, the material axes will be switched as specified by MACF, but no BETA rotation will be performed.
        """ # nopep8
        return self._cards[2].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        """Set the macf property."""
        if value not in [1, 2, 3, 4, -4, -3, -2, None]:
            raise Exception("""macf must be `None` or one of {1,2,3,4,-4,-3,-2}.""")
        self._cards[2].set_value("macf", value)

    @property
    def atrack(self) -> int:
        """Get or set the Material a-axis tracking flag (shell elements only)
        EQ.0:	a - axis rotates with element(default)
        EQ.1 : a - axis also tracks deformation
        """ # nopep8
        return self._cards[2].get_value("atrack")

    @atrack.setter
    def atrack(self, value: int) -> None:
        """Set the atrack property."""
        self._cards[2].set_value("atrack", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the x-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[3].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[3].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the y-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[3].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[3].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the z-coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[3].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[3].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[3].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[3].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[3].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[4].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[4].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[4].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[4].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[4].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[4].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[4].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[4].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[4].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT=3, may be overridden on the element card, see *ELEMENT_SHELL_BETA.
        """ # nopep8
        return self._cards[4].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[4].set_value("beta", value)

    @property
    def lcidc(self) -> int:
        """Get or set the Load curve ID defining the nominal stress versus strain along c-axis. Only solid elements.
        """ # nopep8
        return self._cards[5].get_value("lcidc")

    @lcidc.setter
    def lcidc(self, value: int) -> None:
        """Set the lcidc property."""
        self._cards[5].set_value("lcidc", value)

    @property
    def lcidab(self) -> int:
        """Get or set the Load curve ID defining the nominal ab shear stress versus ab-strain in the ab-plane. Only solid elements.
        """ # nopep8
        return self._cards[5].get_value("lcidab")

    @lcidab.setter
    def lcidab(self, value: int) -> None:
        """Set the lcidab property."""
        self._cards[5].set_value("lcidab", value)

    @property
    def lcidbc(self) -> int:
        """Get or set the Load curve ID defining the nominal ab shear stress versus ab-strain in the bc-plane. Only solid elements.
        """ # nopep8
        return self._cards[5].get_value("lcidbc")

    @lcidbc.setter
    def lcidbc(self, value: int) -> None:
        """Set the lcidbc property."""
        self._cards[5].set_value("lcidbc", value)

    @property
    def lcidca(self) -> int:
        """Get or set the Load curve ID defining the nominal ab shear stress versus ab-strain in the ca-plane. Only solid elements.
        """ # nopep8
        return self._cards[5].get_value("lcidca")

    @lcidca.setter
    def lcidca(self, value: int) -> None:
        """Set the lcidca property."""
        self._cards[5].set_value("lcidca", value)

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

    @property
    def lcida_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcida."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcida:
                return kwd
        return None

    @lcida_link.setter
    def lcida_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcida."""
        self.lcida = value.lcid

    @property
    def lcidb_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidb."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidb:
                return kwd
        return None

    @lcidb_link.setter
    def lcidb_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidb."""
        self.lcidb = value.lcid

    @property
    def lcidc_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidc."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidc:
                return kwd
        return None

    @lcidc_link.setter
    def lcidc_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidc."""
        self.lcidc = value.lcid

    @property
    def lcidab_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidab."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidab:
                return kwd
        return None

    @lcidab_link.setter
    def lcidab_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidab."""
        self.lcidab = value.lcid

    @property
    def lcidbc_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidbc."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidbc:
                return kwd
        return None

    @lcidbc_link.setter
    def lcidbc_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidbc."""
        self.lcidbc = value.lcid

    @property
    def lcidca_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidca."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidca:
                return kwd
        return None

    @lcidca_link.setter
    def lcidca_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidca."""
        self.lcidca = value.lcid

