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

"""Module providing the MatTransverselyIsotropicCrushableFoam class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATTRANSVERSELYISOTROPICCRUSHABLEFOAM_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e11", float, 20, 10, None),
    FieldSchema("e22", float, 30, 10, None),
    FieldSchema("e12", float, 40, 10, None),
    FieldSchema("e23", float, 50, 10, None),
    FieldSchema("g", float, 60, 10, None),
    FieldSchema("k", float, 70, 10, None),
)

_MATTRANSVERSELYISOTROPICCRUSHABLEFOAM_CARD1 = (
    FieldSchema("i11", int, 0, 10, None),
    FieldSchema("i22", int, 10, 10, None),
    FieldSchema("i12", int, 20, 10, None),
    FieldSchema("i23", int, 30, 10, None),
    FieldSchema("iaa", int, 40, 10, None),
    FieldSchema("nsym", int, 50, 10, None),
    FieldSchema("ang", float, 60, 10, None),
    FieldSchema("mu", float, 70, 10, None),
)

_MATTRANSVERSELYISOTROPICCRUSHABLEFOAM_CARD2 = (
    FieldSchema("aopt", float, 0, 10, None),
    FieldSchema("iscl", int, 10, 10, None),
    FieldSchema("beta", float, 20, 10, None),
    FieldSchema("macf", int, 30, 10, 1),
)

_MATTRANSVERSELYISOTROPICCRUSHABLEFOAM_CARD3 = (
    FieldSchema("xp", float, 0, 10, None),
    FieldSchema("yp", float, 10, 10, None),
    FieldSchema("zp", float, 20, 10, None),
    FieldSchema("a1", float, 30, 10, None),
    FieldSchema("a2", float, 40, 10, None),
    FieldSchema("a3", float, 50, 10, None),
)

_MATTRANSVERSELYISOTROPICCRUSHABLEFOAM_CARD4 = (
    FieldSchema("d1", float, 0, 10, None),
    FieldSchema("d2", float, 10, 10, None),
    FieldSchema("d3", float, 20, 10, None),
    FieldSchema("v1", float, 30, 10, None),
    FieldSchema("v2", float, 40, 10, None),
    FieldSchema("v3", float, 50, 10, None),
)

_MATTRANSVERSELYISOTROPICCRUSHABLEFOAM_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatTransverselyIsotropicCrushableFoam(KeywordBase):
    """DYNA MAT_TRANSVERSELY_ISOTROPIC_CRUSHABLE_FOAM keyword"""

    keyword = "MAT"
    subkeyword = "TRANSVERSELY_ISOTROPIC_CRUSHABLE_FOAM"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "i11": LinkType.DEFINE_CURVE,
        "i22": LinkType.DEFINE_CURVE,
        "i12": LinkType.DEFINE_CURVE,
        "i23": LinkType.DEFINE_CURVE,
        "iaa": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatTransverselyIsotropicCrushableFoam class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATTRANSVERSELYISOTROPICCRUSHABLEFOAM_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATTRANSVERSELYISOTROPICCRUSHABLEFOAM_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATTRANSVERSELYISOTROPICCRUSHABLEFOAM_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATTRANSVERSELYISOTROPICCRUSHABLEFOAM_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATTRANSVERSELYISOTROPICCRUSHABLEFOAM_CARD4,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatTransverselyIsotropicCrushableFoam.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATTRANSVERSELYISOTROPICCRUSHABLEFOAM_OPTION0_CARD0,
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
    def e11(self) -> typing.Optional[float]:
        """Get or set the Elastic modulus in axial direction.
        """ # nopep8
        return self._cards[0].get_value("e11")

    @e11.setter
    def e11(self, value: float) -> None:
        """Set the e11 property."""
        self._cards[0].set_value("e11", value)

    @property
    def e22(self) -> typing.Optional[float]:
        """Get or set the Elastic modulus in transverse direction (E22=E33).
        """ # nopep8
        return self._cards[0].get_value("e22")

    @e22.setter
    def e22(self, value: float) -> None:
        """Set the e22 property."""
        self._cards[0].set_value("e22", value)

    @property
    def e12(self) -> typing.Optional[float]:
        """Get or set the Elastic shear modulus (E12=E31).
        """ # nopep8
        return self._cards[0].get_value("e12")

    @e12.setter
    def e12(self, value: float) -> None:
        """Set the e12 property."""
        self._cards[0].set_value("e12", value)

    @property
    def e23(self) -> typing.Optional[float]:
        """Get or set the Elastic shear modulus in transverse plane.
        """ # nopep8
        return self._cards[0].get_value("e23")

    @e23.setter
    def e23(self, value: float) -> None:
        """Set the e23 property."""
        self._cards[0].set_value("e23", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus.
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[0].set_value("g", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Bulk modulus for contact stiffness.
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        """Set the k property."""
        self._cards[0].set_value("k", value)

    @property
    def i11(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for nominal axial stress versus volumetric strain.
        """ # nopep8
        return self._cards[1].get_value("i11")

    @i11.setter
    def i11(self, value: int) -> None:
        """Set the i11 property."""
        self._cards[1].set_value("i11", value)

    @property
    def i22(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for nominal transverse stresses versus volumetric strain (I22=I33).
        """ # nopep8
        return self._cards[1].get_value("i22")

    @i22.setter
    def i22(self, value: int) -> None:
        """Set the i22 property."""
        self._cards[1].set_value("i22", value)

    @property
    def i12(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for shear stress component 12 and 31 versus volumetric strain.(I12=I31).
        """ # nopep8
        return self._cards[1].get_value("i12")

    @i12.setter
    def i12(self, value: int) -> None:
        """Set the i12 property."""
        self._cards[1].set_value("i12", value)

    @property
    def i23(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for shear stress component 23 versus volumetric strain.
        """ # nopep8
        return self._cards[1].get_value("i23")

    @i23.setter
    def i23(self, value: int) -> None:
        """Set the i23 property."""
        self._cards[1].set_value("i23", value)

    @property
    def iaa(self) -> typing.Optional[int]:
        """Get or set the Load curve ID (optional) for nominal stress versus volumetric strain for load at angle, ANG, relative to the material axis.
        """ # nopep8
        return self._cards[1].get_value("iaa")

    @iaa.setter
    def iaa(self, value: int) -> None:
        """Set the iaa property."""
        self._cards[1].set_value("iaa", value)

    @property
    def nsym(self) -> typing.Optional[int]:
        """Get or set the Set to unity for a symmetric yield surface in volumetric compression and tension direction.
        """ # nopep8
        return self._cards[1].get_value("nsym")

    @nsym.setter
    def nsym(self, value: int) -> None:
        """Set the nsym property."""
        self._cards[1].set_value("nsym", value)

    @property
    def ang(self) -> typing.Optional[float]:
        """Get or set the Angle corresponding to load curve ID, IAA
        """ # nopep8
        return self._cards[1].get_value("ang")

    @ang.setter
    def ang(self, value: float) -> None:
        """Set the ang property."""
        self._cards[1].set_value("ang", value)

    @property
    def mu(self) -> typing.Optional[float]:
        """Get or set the Damping coefficient for tensor viscosity which acts in both tension and compression. Recommended values vary between 0.05 to 0.10. If zero, tensor viscosity is not used, but bulk viscosity is used instead. Bulk viscosity creates a pressure as the element compresses that is added to the normal stresses, which can have the effect of creating transverse deformations when none are expected.
        """ # nopep8
        return self._cards[1].get_value("mu")

    @mu.setter
    def mu(self, value: float) -> None:
        """Set the mu property."""
        self._cards[1].set_value("mu", value)

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
    def iscl(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for the strain rate scale factor versus the volumetric strain rate. The yield stress is scaled by the value.
        """ # nopep8
        return self._cards[2].get_value("iscl")

    @iscl.setter
    def iscl(self, value: int) -> None:
        """Set the iscl property."""
        self._cards[2].set_value("iscl", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 0 (shells and tshells only) and AOPT = 3 (all element types).
        This angle may be overridden on the element card, see *ELEMENT_SHELL_BETA,
        *ELEMENT_TSHELL_BETA, and *ELEMENT_SOLID_ORTHO
        """ # nopep8
        return self._cards[2].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[2].set_value("beta", value)

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
        """Get or set the Components of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[3].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[3].set_value("a3", value)

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
        """Get or set the Components of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[4].set_value("d3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[4].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[4].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[4].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[4].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Define components of vector v for AOPT = 3 and 4
        """ # nopep8
        return self._cards[4].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[4].set_value("v3", value)

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
    def i11_link(self) -> DefineCurve:
        """Get the DefineCurve object for i11."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.i11:
                return kwd
        return None

    @i11_link.setter
    def i11_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for i11."""
        self.i11 = value.lcid

    @property
    def i22_link(self) -> DefineCurve:
        """Get the DefineCurve object for i22."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.i22:
                return kwd
        return None

    @i22_link.setter
    def i22_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for i22."""
        self.i22 = value.lcid

    @property
    def i12_link(self) -> DefineCurve:
        """Get the DefineCurve object for i12."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.i12:
                return kwd
        return None

    @i12_link.setter
    def i12_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for i12."""
        self.i12 = value.lcid

    @property
    def i23_link(self) -> DefineCurve:
        """Get the DefineCurve object for i23."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.i23:
                return kwd
        return None

    @i23_link.setter
    def i23_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for i23."""
        self.i23 = value.lcid

    @property
    def iaa_link(self) -> DefineCurve:
        """Get the DefineCurve object for iaa."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.iaa:
                return kwd
        return None

    @iaa_link.setter
    def iaa_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for iaa."""
        self.iaa = value.lcid

