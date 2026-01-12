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

"""Module providing the MatShapeMemoryAlloy class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATSHAPEMEMORYALLOY_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("rho", float, 10, 10, None),
    FieldSchema("em", float, 20, 10, None),
    FieldSchema("ea", float, 30, 10, None),
    FieldSchema("prm", float, 40, 10, None),
    FieldSchema("pra", float, 50, 10, None),
    FieldSchema("aopt", int, 60, 10, None),
    FieldSchema("stype", int, 70, 10, 0),
)

_MATSHAPEMEMORYALLOY_CARD1 = (
    FieldSchema("cpm", float, 0, 10, None),
    FieldSchema("cpa", float, 10, 10, None),
    FieldSchema("lh", float, 20, 10, None),
    FieldSchema("tc", float, 30, 10, None),
    FieldSchema("tmf", float, 40, 10, None),
    FieldSchema("tms", float, 50, 10, None),
    FieldSchema("tas", float, 60, 10, None),
    FieldSchema("taf", float, 70, 10, None),
)

_MATSHAPEMEMORYALLOY_CARD2 = (
    FieldSchema("a1i", float, 0, 10, None),
    FieldSchema("a2i", float, 10, 10, None),
    FieldSchema("bi", float, 20, 10, None),
    FieldSchema("ci", float, 30, 10, None),
    FieldSchema("ki", float, 40, 10, None),
    FieldSchema("mi", float, 50, 10, None),
    FieldSchema("kl", float, 60, 10, None),
    FieldSchema("ml", float, 70, 10, None),
)

_MATSHAPEMEMORYALLOY_CARD3 = (
    FieldSchema("a1s", float, 0, 10, None),
    FieldSchema("a2s", float, 10, 10, None),
    FieldSchema("bs", float, 20, 10, None),
    FieldSchema("cs", float, 30, 10, None),
    FieldSchema("ks", float, 40, 10, None),
    FieldSchema("ms", float, 50, 10, None),
)

_MATSHAPEMEMORYALLOY_CARD4 = (
    FieldSchema("d0l", float, 0, 10, None),
    FieldSchema("d0m", float, 10, 10, None),
)

_MATSHAPEMEMORYALLOY_CARD5 = (
    FieldSchema("xp", float, 0, 10, None),
    FieldSchema("yp", float, 10, 10, None),
    FieldSchema("zp", float, 20, 10, None),
    FieldSchema("a1", float, 30, 10, None),
    FieldSchema("a2", float, 40, 10, None),
    FieldSchema("a3", float, 50, 10, None),
    FieldSchema("macf", int, 60, 10, 1),
)

_MATSHAPEMEMORYALLOY_CARD6 = (
    FieldSchema("v1", float, 0, 10, None),
    FieldSchema("v2", float, 10, 10, None),
    FieldSchema("v3", float, 20, 10, None),
    FieldSchema("d1", float, 30, 10, None),
    FieldSchema("d2", float, 40, 10, None),
    FieldSchema("d3", float, 50, 10, None),
    FieldSchema("beta", float, 60, 10, None),
    FieldSchema("ref", float, 70, 10, None),
)

_MATSHAPEMEMORYALLOY_CARD7 = (
    FieldSchema("n11", float, 0, 10, None),
    FieldSchema("n22", float, 10, 10, None),
    FieldSchema("n33", float, 20, 10, None),
    FieldSchema("n44", float, 30, 10, None),
    FieldSchema("n55", float, 40, 10, None),
    FieldSchema("n66", float, 50, 10, None),
    FieldSchema("n12", float, 60, 10, None),
    FieldSchema("n23", float, 70, 10, None),
)

_MATSHAPEMEMORYALLOY_CARD8 = (
    FieldSchema("n34", float, 0, 10, None),
    FieldSchema("n45", float, 10, 10, None),
    FieldSchema("n56", float, 20, 10, None),
    FieldSchema("n13", float, 30, 10, None),
    FieldSchema("n24", float, 40, 10, None),
    FieldSchema("n35", float, 50, 10, None),
    FieldSchema("n46", float, 60, 10, None),
    FieldSchema("n14", float, 70, 10, None),
)

_MATSHAPEMEMORYALLOY_CARD9 = (
    FieldSchema("n25", float, 0, 10, None),
    FieldSchema("n36", float, 10, 10, None),
    FieldSchema("n15", float, 20, 10, None),
    FieldSchema("n26", float, 30, 10, None),
    FieldSchema("n16", float, 40, 10, None),
)

class MatShapeMemoryAlloy(KeywordBase):
    """DYNA MAT_SHAPE_MEMORY_ALLOY keyword"""

    keyword = "MAT"
    subkeyword = "SHAPE_MEMORY_ALLOY"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatShapeMemoryAlloy class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATSHAPEMEMORYALLOY_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATSHAPEMEMORYALLOY_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATSHAPEMEMORYALLOY_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATSHAPEMEMORYALLOY_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATSHAPEMEMORYALLOY_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATSHAPEMEMORYALLOY_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATSHAPEMEMORYALLOY_CARD6,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATSHAPEMEMORYALLOY_CARD7,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATSHAPEMEMORYALLOY_CARD8,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATSHAPEMEMORYALLOY_CARD9,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatShapeMemoryAlloy.option_specs[0],
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
    def em(self) -> typing.Optional[float]:
        """Get or set the Martensite Young's modulus.
        """ # nopep8
        return self._cards[0].get_value("em")

    @em.setter
    def em(self, value: float) -> None:
        """Set the em property."""
        self._cards[0].set_value("em", value)

    @property
    def ea(self) -> typing.Optional[float]:
        """Get or set the Austenite Young's modulus.
        """ # nopep8
        return self._cards[0].get_value("ea")

    @ea.setter
    def ea(self, value: float) -> None:
        """Set the ea property."""
        self._cards[0].set_value("ea", value)

    @property
    def prm(self) -> typing.Optional[float]:
        """Get or set the Martensite Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("prm")

    @prm.setter
    def prm(self, value: float) -> None:
        """Set the prm property."""
        self._cards[0].set_value("prm", value)

    @property
    def pra(self) -> typing.Optional[float]:
        """Get or set the Austenite Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pra")

    @pra.setter
    def pra(self, value: float) -> None:
        """Set the pra property."""
        self._cards[0].set_value("pra", value)

    @property
    def aopt(self) -> typing.Optional[int]:
        """Get or set the Material axes option (see MAT_OPTIONTROPIC_ELASTIC, particularly the Material Directions section, for details):
        EQ.0.0:	Locally orthotropic with material axes determined by element nodes 1, 2,and 4, as with* DEFINE_COORDINATE_NODES.
        EQ.1.0 : Locally orthotropic with material axes determined by a point, P, in spaceand the global location of the element center; this is the a - direction.
        EQ.2.0:	Globally orthotropic with material axes determined by vectors defined below, as with* DEFINE_COORDINATE_VECTOR
        EQ.3.0 : Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element.The plane of a solid element is the midsurface between the inner surface and outer surface defined by the first four nodes and the last four nodes of the connectivity of the element, respectively.Thus, AOPT = 3 is only available for hexahedrons.a is determined by taking the cross product of v with the normal vector, b is determined by taking the cross product of the normal vector with a,and c is the normal vector.Then aand b are rotated about c by an angle BETA.BETA may be set in the keyword input for the element or in the input for this keyword.Note that the material axes may be switched depending on the choice of MACF.The switch may occur before or after applying BETA depending on the value of MACF.
        EQ.4.0 : Locally orthotropic in a cylindrical coordinate system with the material axes determined by a vector v,and an originating point, P, which define the centerline axis.
        LT.0.0 : The absolute value of AOPT is a coordinate system ID number(CID on * DEFINE_COORDINATE_OPTION).
        """ # nopep8
        return self._cards[0].get_value("aopt")

    @aopt.setter
    def aopt(self, value: int) -> None:
        """Set the aopt property."""
        self._cards[0].set_value("aopt", value)

    @property
    def stype(self) -> int:
        """Get or set the Initiation/saturation surface type:
        EQ.0:	uses strain invariants(default)
        EQ.1 : uses principal strains.
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        """Set the stype property."""
        if value not in [0, 1, None]:
            raise Exception("""stype must be `None` or one of {0,1}.""")
        self._cards[0].set_value("stype", value)

    @property
    def cpm(self) -> typing.Optional[float]:
        """Get or set the Martensite volumetric heat capacity (density times specific heat capacity).
        """ # nopep8
        return self._cards[1].get_value("cpm")

    @cpm.setter
    def cpm(self, value: float) -> None:
        """Set the cpm property."""
        self._cards[1].set_value("cpm", value)

    @property
    def cpa(self) -> typing.Optional[float]:
        """Get or set the Austenite volumetric heat capacity (density times specific heat capacity).
        """ # nopep8
        return self._cards[1].get_value("cpa")

    @cpa.setter
    def cpa(self, value: float) -> None:
        """Set the cpa property."""
        self._cards[1].set_value("cpa", value)

    @property
    def lh(self) -> typing.Optional[float]:
        """Get or set the Volumetric latent heat of transformation (density times specific latent heat).
        """ # nopep8
        return self._cards[1].get_value("lh")

    @lh.setter
    def lh(self, value: float) -> None:
        """Set the lh property."""
        self._cards[1].set_value("lh", value)

    @property
    def tc(self) -> typing.Optional[float]:
        """Get or set the Thermodynamic temperature.
        """ # nopep8
        return self._cards[1].get_value("tc")

    @tc.setter
    def tc(self, value: float) -> None:
        """Set the tc property."""
        self._cards[1].set_value("tc", value)

    @property
    def tmf(self) -> typing.Optional[float]:
        """Get or set the Martensite finish temperature, optional;
        """ # nopep8
        return self._cards[1].get_value("tmf")

    @tmf.setter
    def tmf(self, value: float) -> None:
        """Set the tmf property."""
        self._cards[1].set_value("tmf", value)

    @property
    def tms(self) -> typing.Optional[float]:
        """Get or set the Martensite start temperature, optional;
        """ # nopep8
        return self._cards[1].get_value("tms")

    @tms.setter
    def tms(self, value: float) -> None:
        """Set the tms property."""
        self._cards[1].set_value("tms", value)

    @property
    def tas(self) -> typing.Optional[float]:
        """Get or set the Austenite start temperature, optional;
        """ # nopep8
        return self._cards[1].get_value("tas")

    @tas.setter
    def tas(self, value: float) -> None:
        """Set the tas property."""
        self._cards[1].set_value("tas", value)

    @property
    def taf(self) -> typing.Optional[float]:
        """Get or set the Austenite finish temperature, optional;
        """ # nopep8
        return self._cards[1].get_value("taf")

    @taf.setter
    def taf(self, value: float) -> None:
        """Set the taf property."""
        self._cards[1].set_value("taf", value)

    @property
    def a1i(self) -> typing.Optional[float]:
        """Get or set the Tension/compression asymmetry for initiation surface.
        """ # nopep8
        return self._cards[2].get_value("a1i")

    @a1i.setter
    def a1i(self, value: float) -> None:
        """Set the a1i property."""
        self._cards[2].set_value("a1i", value)

    @property
    def a2i(self) -> typing.Optional[float]:
        """Get or set the Tension/compression asymmetry for initiation surface.
        """ # nopep8
        return self._cards[2].get_value("a2i")

    @a2i.setter
    def a2i(self, value: float) -> None:
        """Set the a2i property."""
        self._cards[2].set_value("a2i", value)

    @property
    def bi(self) -> typing.Optional[float]:
        """Get or set the Radius for initiation surface.
        """ # nopep8
        return self._cards[2].get_value("bi")

    @bi.setter
    def bi(self, value: float) -> None:
        """Set the bi property."""
        self._cards[2].set_value("bi", value)

    @property
    def ci(self) -> typing.Optional[float]:
        """Get or set the Eccentricity of initiation surface with respect to material direction.
        """ # nopep8
        return self._cards[2].get_value("ci")

    @ci.setter
    def ci(self, value: float) -> None:
        """Set the ci property."""
        self._cards[2].set_value("ci", value)

    @property
    def ki(self) -> typing.Optional[float]:
        """Get or set the Coefficient in initiation energy.
        """ # nopep8
        return self._cards[2].get_value("ki")

    @ki.setter
    def ki(self, value: float) -> None:
        """Set the ki property."""
        self._cards[2].set_value("ki", value)

    @property
    def mi(self) -> typing.Optional[float]:
        """Get or set the Exponent in initiation energy.
        """ # nopep8
        return self._cards[2].get_value("mi")

    @mi.setter
    def mi(self, value: float) -> None:
        """Set the mi property."""
        self._cards[2].set_value("mi", value)

    @property
    def kl(self) -> typing.Optional[float]:
        """Get or set the Coefficient in volume fraction energy
        """ # nopep8
        return self._cards[2].get_value("kl")

    @kl.setter
    def kl(self, value: float) -> None:
        """Set the kl property."""
        self._cards[2].set_value("kl", value)

    @property
    def ml(self) -> typing.Optional[float]:
        """Get or set the Exponent in volume fraction energy.
        """ # nopep8
        return self._cards[2].get_value("ml")

    @ml.setter
    def ml(self, value: float) -> None:
        """Set the ml property."""
        self._cards[2].set_value("ml", value)

    @property
    def a1s(self) -> typing.Optional[float]:
        """Get or set the Tension/compression asymmetry for saturation surface.
        """ # nopep8
        return self._cards[3].get_value("a1s")

    @a1s.setter
    def a1s(self, value: float) -> None:
        """Set the a1s property."""
        self._cards[3].set_value("a1s", value)

    @property
    def a2s(self) -> typing.Optional[float]:
        """Get or set the Tension/compression asymmetry for saturation surface.
        """ # nopep8
        return self._cards[3].get_value("a2s")

    @a2s.setter
    def a2s(self, value: float) -> None:
        """Set the a2s property."""
        self._cards[3].set_value("a2s", value)

    @property
    def bs(self) -> typing.Optional[float]:
        """Get or set the Radius for saturation surface.
        """ # nopep8
        return self._cards[3].get_value("bs")

    @bs.setter
    def bs(self, value: float) -> None:
        """Set the bs property."""
        self._cards[3].set_value("bs", value)

    @property
    def cs(self) -> typing.Optional[float]:
        """Get or set the Eccentricity of saturation surface with respect to material direction.
        """ # nopep8
        return self._cards[3].get_value("cs")

    @cs.setter
    def cs(self, value: float) -> None:
        """Set the cs property."""
        self._cards[3].set_value("cs", value)

    @property
    def ks(self) -> typing.Optional[float]:
        """Get or set the Coefficient in saturation energy.
        """ # nopep8
        return self._cards[3].get_value("ks")

    @ks.setter
    def ks(self, value: float) -> None:
        """Set the ks property."""
        self._cards[3].set_value("ks", value)

    @property
    def ms(self) -> typing.Optional[float]:
        """Get or set the Exponent in saturation energy.
        """ # nopep8
        return self._cards[3].get_value("ms")

    @ms.setter
    def ms(self, value: float) -> None:
        """Set the ms property."""
        self._cards[3].set_value("ms", value)

    @property
    def d0l(self) -> typing.Optional[float]:
        """Get or set the Initial driving force for volume fraction transformation.
        """ # nopep8
        return self._cards[4].get_value("d0l")

    @d0l.setter
    def d0l(self, value: float) -> None:
        """Set the d0l property."""
        self._cards[4].set_value("d0l", value)

    @property
    def d0m(self) -> typing.Optional[float]:
        """Get or set the Initial driving force for martensite strain transformation.
        """ # nopep8
        return self._cards[4].get_value("d0m")

    @d0m.setter
    def d0m(self, value: float) -> None:
        """Set the d0m property."""
        self._cards[4].set_value("d0m", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point P for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[5].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[5].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point P for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[5].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[5].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Coordinates of point P for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[5].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[5].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[5].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[5].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[5].set_value("a3", value)

    @property
    def macf(self) -> int:
        """Get or set the Material axes change flag for solid elements:
        EQ. - 4:	Switch material axes b and c before BETA rotation
        EQ. - 3 : Switch material axes a and c before BETA rotation
        EQ. - 2 : Switch material axes a and b before BETA rotation
        EQ.1 : No change, default
        EQ.2 : Switch material axes a and b after BETA rotation
        EQ.3 : Switch material axes a and c after BETA rotation
        EQ.4 : Switch material axes b and c after BETA rotation
        Figure Error!Reference source not found.indicates when LS - DYNA applies MACF during the process to obtain the final material axes.If BETA on * ELEMENT_SOLID_{OPTION} is defined, then that BETA is used for the rotation for all AOPT options.Otherwise, for AOPT = 3, the BETA input on Card 7 rotates the axes.For all other values of AOPT, the material axes will be switched as specified by MACF, but no BETA rotation will be performed
        """ # nopep8
        return self._cards[5].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        """Set the macf property."""
        if value not in [1, 2, 3, 4, -2, -3, -4, None]:
            raise Exception("""macf must be `None` or one of {1,2,3,4,-2,-3,-4}.""")
        self._cards[5].set_value("macf", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[6].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[6].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[6].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[6].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[6].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[6].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[6].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[6].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[6].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 3.  This angle may be overridden on the element card; see *ELEMENT_SOLID_ORTHO.
        """ # nopep8
        return self._cards[6].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[6].set_value("beta", value)

    @property
    def ref(self) -> typing.Optional[float]:
        """Get or set the Use reference geometry to initialize the stress tensor.  The reference geometry is defined by the keyword:
        *INITIAL_FOAM_REFERENCE_GEOMETRY.EQ.0.0:	off
        EQ.1.0:	on
        """ # nopep8
        return self._cards[6].get_value("ref")

    @ref.setter
    def ref(self, value: float) -> None:
        """Set the ref property."""
        self._cards[6].set_value("ref", value)

    @property
    def n11(self) -> typing.Optional[float]:
        """Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
        """ # nopep8
        return self._cards[7].get_value("n11")

    @n11.setter
    def n11(self, value: float) -> None:
        """Set the n11 property."""
        self._cards[7].set_value("n11", value)

    @property
    def n22(self) -> typing.Optional[float]:
        """Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
        """ # nopep8
        return self._cards[7].get_value("n22")

    @n22.setter
    def n22(self, value: float) -> None:
        """Set the n22 property."""
        self._cards[7].set_value("n22", value)

    @property
    def n33(self) -> typing.Optional[float]:
        """Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
        """ # nopep8
        return self._cards[7].get_value("n33")

    @n33.setter
    def n33(self, value: float) -> None:
        """Set the n33 property."""
        self._cards[7].set_value("n33", value)

    @property
    def n44(self) -> typing.Optional[float]:
        """Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
        """ # nopep8
        return self._cards[7].get_value("n44")

    @n44.setter
    def n44(self, value: float) -> None:
        """Set the n44 property."""
        self._cards[7].set_value("n44", value)

    @property
    def n55(self) -> typing.Optional[float]:
        """Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
        """ # nopep8
        return self._cards[7].get_value("n55")

    @n55.setter
    def n55(self, value: float) -> None:
        """Set the n55 property."""
        self._cards[7].set_value("n55", value)

    @property
    def n66(self) -> typing.Optional[float]:
        """Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
        """ # nopep8
        return self._cards[7].get_value("n66")

    @n66.setter
    def n66(self, value: float) -> None:
        """Set the n66 property."""
        self._cards[7].set_value("n66", value)

    @property
    def n12(self) -> typing.Optional[float]:
        """Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
        """ # nopep8
        return self._cards[7].get_value("n12")

    @n12.setter
    def n12(self, value: float) -> None:
        """Set the n12 property."""
        self._cards[7].set_value("n12", value)

    @property
    def n23(self) -> typing.Optional[float]:
        """Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
        """ # nopep8
        return self._cards[7].get_value("n23")

    @n23.setter
    def n23(self, value: float) -> None:
        """Set the n23 property."""
        self._cards[7].set_value("n23", value)

    @property
    def n34(self) -> typing.Optional[float]:
        """Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
        """ # nopep8
        return self._cards[8].get_value("n34")

    @n34.setter
    def n34(self, value: float) -> None:
        """Set the n34 property."""
        self._cards[8].set_value("n34", value)

    @property
    def n45(self) -> typing.Optional[float]:
        """Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
        """ # nopep8
        return self._cards[8].get_value("n45")

    @n45.setter
    def n45(self, value: float) -> None:
        """Set the n45 property."""
        self._cards[8].set_value("n45", value)

    @property
    def n56(self) -> typing.Optional[float]:
        """Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
        """ # nopep8
        return self._cards[8].get_value("n56")

    @n56.setter
    def n56(self, value: float) -> None:
        """Set the n56 property."""
        self._cards[8].set_value("n56", value)

    @property
    def n13(self) -> typing.Optional[float]:
        """Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
        """ # nopep8
        return self._cards[8].get_value("n13")

    @n13.setter
    def n13(self, value: float) -> None:
        """Set the n13 property."""
        self._cards[8].set_value("n13", value)

    @property
    def n24(self) -> typing.Optional[float]:
        """Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
        """ # nopep8
        return self._cards[8].get_value("n24")

    @n24.setter
    def n24(self, value: float) -> None:
        """Set the n24 property."""
        self._cards[8].set_value("n24", value)

    @property
    def n35(self) -> typing.Optional[float]:
        """Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
        """ # nopep8
        return self._cards[8].get_value("n35")

    @n35.setter
    def n35(self, value: float) -> None:
        """Set the n35 property."""
        self._cards[8].set_value("n35", value)

    @property
    def n46(self) -> typing.Optional[float]:
        """Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
        """ # nopep8
        return self._cards[8].get_value("n46")

    @n46.setter
    def n46(self, value: float) -> None:
        """Set the n46 property."""
        self._cards[8].set_value("n46", value)

    @property
    def n14(self) -> typing.Optional[float]:
        """Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
        """ # nopep8
        return self._cards[8].get_value("n14")

    @n14.setter
    def n14(self, value: float) -> None:
        """Set the n14 property."""
        self._cards[8].set_value("n14", value)

    @property
    def n25(self) -> typing.Optional[float]:
        """Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
        """ # nopep8
        return self._cards[9].get_value("n25")

    @n25.setter
    def n25(self, value: float) -> None:
        """Set the n25 property."""
        self._cards[9].set_value("n25", value)

    @property
    def n36(self) -> typing.Optional[float]:
        """Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
        """ # nopep8
        return self._cards[9].get_value("n36")

    @n36.setter
    def n36(self, value: float) -> None:
        """Set the n36 property."""
        self._cards[9].set_value("n36", value)

    @property
    def n15(self) -> typing.Optional[float]:
        """Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
        """ # nopep8
        return self._cards[9].get_value("n15")

    @n15.setter
    def n15(self, value: float) -> None:
        """Set the n15 property."""
        self._cards[9].set_value("n15", value)

    @property
    def n26(self) -> typing.Optional[float]:
        """Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
        """ # nopep8
        return self._cards[9].get_value("n26")

    @n26.setter
    def n26(self, value: float) -> None:
        """Set the n26 property."""
        self._cards[9].set_value("n26", value)

    @property
    def n16(self) -> typing.Optional[float]:
        """Get or set the Additional anisotropy parameters for initiation/saturation surface, relative to material axis given by AOPT. Used for STYPE = 1.
        """ # nopep8
        return self._cards[9].get_value("n16")

    @n16.setter
    def n16(self, value: float) -> None:
        """Set the n16 property."""
        self._cards[9].set_value("n16", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[10].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[10].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

