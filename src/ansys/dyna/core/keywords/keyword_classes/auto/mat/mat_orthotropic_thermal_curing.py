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

"""Module providing the MatOrthotropicThermalCuring class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATORTHOTROPICTHERMALCURING_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("ea", float, 20, 10, None),
    FieldSchema("eb", float, 30, 10, None),
    FieldSchema("ec", float, 40, 10, None),
    FieldSchema("prba", float, 50, 10, None),
    FieldSchema("prca", float, 60, 10, None),
    FieldSchema("prcb", float, 70, 10, None),
)

_MATORTHOTROPICTHERMALCURING_CARD1 = (
    FieldSchema("gab", float, 0, 10, None),
    FieldSchema("gbc", float, 10, 10, None),
    FieldSchema("gca", float, 20, 10, None),
    FieldSchema("aa", float, 30, 10, None),
    FieldSchema("ab", float, 40, 10, None),
    FieldSchema("ac", float, 50, 10, None),
    FieldSchema("aopt", float, 60, 10, None),
    FieldSchema("macf", int, 70, 10, 1),
)

_MATORTHOTROPICTHERMALCURING_CARD2 = (
    FieldSchema("xp", float, 0, 10, None),
    FieldSchema("yp", float, 10, 10, None),
    FieldSchema("zp", float, 20, 10, None),
    FieldSchema("a1", float, 30, 10, None),
    FieldSchema("a2", float, 40, 10, None),
    FieldSchema("a3", float, 50, 10, None),
)

_MATORTHOTROPICTHERMALCURING_CARD3 = (
    FieldSchema("v1", float, 0, 10, None),
    FieldSchema("v2", float, 10, 10, None),
    FieldSchema("v3", float, 20, 10, None),
    FieldSchema("d1", float, 30, 10, None),
    FieldSchema("d2", float, 40, 10, None),
    FieldSchema("d3", float, 50, 10, None),
    FieldSchema("beta", float, 60, 10, None),
    FieldSchema("ref", int, 70, 10, 0),
)

_MATORTHOTROPICTHERMALCURING_CARD4 = (
    FieldSchema("k1", float, 0, 10, None),
    FieldSchema("k2", float, 10, 10, None),
    FieldSchema("c1", float, 20, 10, None),
    FieldSchema("c2", float, 30, 10, None),
    FieldSchema("m", float, 40, 10, None),
    FieldSchema("n", float, 50, 10, None),
    FieldSchema("r", float, 60, 10, None),
)

_MATORTHOTROPICTHERMALCURING_CARD5 = (
    FieldSchema("lccha", int, 0, 10, None),
    FieldSchema("lcchb", int, 10, 10, None),
    FieldSchema("lcchc", int, 20, 10, None),
    FieldSchema("lcaa", int, 30, 10, None),
    FieldSchema("lcab", int, 40, 10, None),
    FieldSchema("lcac", int, 50, 10, None),
)

_MATORTHOTROPICTHERMALCURING_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatOrthotropicThermalCuring(KeywordBase):
    """DYNA MAT_ORTHOTROPIC_THERMAL_CURING keyword"""

    keyword = "MAT"
    subkeyword = "ORTHOTROPIC_THERMAL_CURING"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatOrthotropicThermalCuring class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATORTHOTROPICTHERMALCURING_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATORTHOTROPICTHERMALCURING_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATORTHOTROPICTHERMALCURING_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATORTHOTROPICTHERMALCURING_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATORTHOTROPICTHERMALCURING_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATORTHOTROPICTHERMALCURING_CARD5,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatOrthotropicThermalCuring.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATORTHOTROPICTHERMALCURING_OPTION0_CARD0,
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
        """Get or set the Ea , Young's modulus in a-direction.
        """ # nopep8
        return self._cards[0].get_value("ea")

    @ea.setter
    def ea(self, value: float) -> None:
        """Set the ea property."""
        self._cards[0].set_value("ea", value)

    @property
    def eb(self) -> typing.Optional[float]:
        """Get or set the Eb , Young's modulus in b-direction.
        """ # nopep8
        return self._cards[0].get_value("eb")

    @eb.setter
    def eb(self, value: float) -> None:
        """Set the eb property."""
        self._cards[0].set_value("eb", value)

    @property
    def ec(self) -> typing.Optional[float]:
        """Get or set the Ec , Young's modulus in c-direction.
        """ # nopep8
        return self._cards[0].get_value("ec")

    @ec.setter
    def ec(self, value: float) -> None:
        """Set the ec property."""
        self._cards[0].set_value("ec", value)

    @property
    def prba(self) -> typing.Optional[float]:
        """Get or set the PRbc , Poisson's ratio, ba.
        """ # nopep8
        return self._cards[0].get_value("prba")

    @prba.setter
    def prba(self, value: float) -> None:
        """Set the prba property."""
        self._cards[0].set_value("prba", value)

    @property
    def prca(self) -> typing.Optional[float]:
        """Get or set the PRca , Poisson's ratio, ca.
        """ # nopep8
        return self._cards[0].get_value("prca")

    @prca.setter
    def prca(self, value: float) -> None:
        """Set the prca property."""
        self._cards[0].set_value("prca", value)

    @property
    def prcb(self) -> typing.Optional[float]:
        """Get or set the PRcb , Poisson's ratio, cb.
        """ # nopep8
        return self._cards[0].get_value("prcb")

    @prcb.setter
    def prcb(self, value: float) -> None:
        """Set the prcb property."""
        self._cards[0].set_value("prcb", value)

    @property
    def gab(self) -> typing.Optional[float]:
        """Get or set the G ab , Shear modulus, ab.
        """ # nopep8
        return self._cards[1].get_value("gab")

    @gab.setter
    def gab(self, value: float) -> None:
        """Set the gab property."""
        self._cards[1].set_value("gab", value)

    @property
    def gbc(self) -> typing.Optional[float]:
        """Get or set the G bc , Shear modulus, bc.
        """ # nopep8
        return self._cards[1].get_value("gbc")

    @gbc.setter
    def gbc(self, value: float) -> None:
        """Set the gbc property."""
        self._cards[1].set_value("gbc", value)

    @property
    def gca(self) -> typing.Optional[float]:
        """Get or set the G ca , Shear modulus, ca.
        """ # nopep8
        return self._cards[1].get_value("gca")

    @gca.setter
    def gca(self, value: float) -> None:
        """Set the gca property."""
        self._cards[1].set_value("gca", value)

    @property
    def aa(self) -> typing.Optional[float]:
        """Get or set the Alpha a,coefficients of thermal expansion in the a-direction.
        """ # nopep8
        return self._cards[1].get_value("aa")

    @aa.setter
    def aa(self, value: float) -> None:
        """Set the aa property."""
        self._cards[1].set_value("aa", value)

    @property
    def ab(self) -> typing.Optional[float]:
        """Get or set the Alpha b,coefficients of thermal expansion in the b-direction.
        """ # nopep8
        return self._cards[1].get_value("ab")

    @ab.setter
    def ab(self, value: float) -> None:
        """Set the ab property."""
        self._cards[1].set_value("ab", value)

    @property
    def ac(self) -> typing.Optional[float]:
        """Get or set the Alpha c,coefficients of thermal expansion in the c-direction.
        """ # nopep8
        return self._cards[1].get_value("ac")

    @ac.setter
    def ac(self, value: float) -> None:
        """Set the ac property."""
        self._cards[1].set_value("ac", value)

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
        return self._cards[1].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        """Set the aopt property."""
        self._cards[1].set_value("aopt", value)

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
        return self._cards[1].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        """Set the macf property."""
        if value not in [1, 2, 3, 4, -4, -3, -2, None]:
            raise Exception("""macf must be `None` or one of {1,2,3,4,-4,-3,-2}.""")
        self._cards[1].set_value("macf", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the X-Coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[2].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[2].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Y-Coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[2].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[2].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Z-Coordinates of point p for AOPT = 1.
        """ # nopep8
        return self._cards[2].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[2].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the X-Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[2].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Y-Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[2].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Z-Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[2].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[2].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Component 1 of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[3].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[3].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Component 2 of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[3].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[3].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Component 3 of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[3].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[3].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Component 1 of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[3].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Component 2 of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[3].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Component 3 of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[3].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 0 (shells and tshells only) and AOPT = 3 (all element types).  It may be overridden on the element card; see *ELEMENT_‌SHELL_‌BETA, *ELEMENT_TSHELL_BETA, or *ELEMENT_‌SOLID_‌ORTHO
        """ # nopep8
        return self._cards[3].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[3].set_value("beta", value)

    @property
    def ref(self) -> int:
        """Get or set the Use reference geometry to initialize the stress tensor. The reference geometriy is defined by the keyword:*INITIAL_FOAM_REFERENCE_ GEOMETRY. This option is currently restricted to 8-noded solid elements with one point integration.
        EQ.0.0: off,
        EQ.1.0: on.
        """ # nopep8
        return self._cards[3].get_value("ref")

    @ref.setter
    def ref(self, value: int) -> None:
        """Set the ref property."""
        if value not in [0, 1, None]:
            raise Exception("""ref must be `None` or one of {0,1}.""")
        self._cards[3].set_value("ref", value)

    @property
    def k1(self) -> typing.Optional[float]:
        """Get or set the Parameter k_1 for Kamal model. For details see remarks below.
        """ # nopep8
        return self._cards[4].get_value("k1")

    @k1.setter
    def k1(self, value: float) -> None:
        """Set the k1 property."""
        self._cards[4].set_value("k1", value)

    @property
    def k2(self) -> typing.Optional[float]:
        """Get or set the Parameter k_2 for Kamal model.
        """ # nopep8
        return self._cards[4].get_value("k2")

    @k2.setter
    def k2(self, value: float) -> None:
        """Set the k2 property."""
        self._cards[4].set_value("k2", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Parameter c_1 for Kamal model.
        """ # nopep8
        return self._cards[4].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        """Set the c1 property."""
        self._cards[4].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Parameter c_2 for Kamal model.
        """ # nopep8
        return self._cards[4].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        """Set the c2 property."""
        self._cards[4].set_value("c2", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Exponent m for Kamal model.
        """ # nopep8
        return self._cards[4].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[4].set_value("m", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Exponent n for Kamal model.
        """ # nopep8
        return self._cards[4].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[4].set_value("n", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Gas constant for Kamal model.
        """ # nopep8
        return self._cards[4].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        """Set the r property."""
        self._cards[4].set_value("r", value)

    @property
    def lccha(self) -> typing.Optional[int]:
        """Get or set the Load curve for γ_a, coefficient of chemical shrinkage in the a-direction. Input γ_a as function of state of cure β.
        """ # nopep8
        return self._cards[5].get_value("lccha")

    @lccha.setter
    def lccha(self, value: int) -> None:
        """Set the lccha property."""
        self._cards[5].set_value("lccha", value)

    @property
    def lcchb(self) -> typing.Optional[int]:
        """Get or set the Load curve for γ_b, coefficient of chemical shrinkage in the b-direction. Input γ_b as function of state of cure β.
        """ # nopep8
        return self._cards[5].get_value("lcchb")

    @lcchb.setter
    def lcchb(self, value: int) -> None:
        """Set the lcchb property."""
        self._cards[5].set_value("lcchb", value)

    @property
    def lcchc(self) -> typing.Optional[int]:
        """Get or set the Load curve for γ_c, coefficient of chemical shrinkage in the c-direction. Input γ_c as function of state of cure β.
        """ # nopep8
        return self._cards[5].get_value("lcchc")

    @lcchc.setter
    def lcchc(self, value: int) -> None:
        """Set the lcchc property."""
        self._cards[5].set_value("lcchc", value)

    @property
    def lcaa(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID for α_a. If defined, parameter AA is ignored.  If a load curve, then α_a is a function of temperature. If a table ID, the α_a is a function of the state of cure (table values) and temperature (see*DEFINE_TABLE)..
        """ # nopep8
        return self._cards[5].get_value("lcaa")

    @lcaa.setter
    def lcaa(self, value: int) -> None:
        """Set the lcaa property."""
        self._cards[5].set_value("lcaa", value)

    @property
    def lcab(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for α_b. If defined parameter, AB is ignored. See LCAA for further details..
        """ # nopep8
        return self._cards[5].get_value("lcab")

    @lcab.setter
    def lcab(self, value: int) -> None:
        """Set the lcab property."""
        self._cards[5].set_value("lcab", value)

    @property
    def lcac(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for α_c. If defined parameter, AC is ignored. See LCAA for further details..
        """ # nopep8
        return self._cards[5].get_value("lcac")

    @lcac.setter
    def lcac(self, value: int) -> None:
        """Set the lcac property."""
        self._cards[5].set_value("lcac", value)

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

