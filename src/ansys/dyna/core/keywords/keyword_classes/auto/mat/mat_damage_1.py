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

"""Module providing the MatDamage1 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATDAMAGE1_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("sigy", float, 40, 10, None),
    FieldSchema("lcss", int, 50, 10, None),
    FieldSchema("lcds", int, 60, 10, None),
)

_MATDAMAGE1_CARD1 = (
    FieldSchema("q1", float, 0, 10, None),
    FieldSchema("c1", float, 10, 10, None),
    FieldSchema("q2", float, 20, 10, None),
    FieldSchema("c2", float, 30, 10, None),
    FieldSchema("epsd", float, 40, 10, None),
    FieldSchema("espr/s", float, 50, 10, None),
    FieldSchema("dc", float, 60, 10, 0.5),
    FieldSchema("flag", int, 70, 10, 0),
)

_MATDAMAGE1_CARD2 = (
    FieldSchema("vk", float, 0, 10, None),
    FieldSchema("vm", float, 10, 10, None),
    FieldSchema("r00/f", float, 20, 10, None),
    FieldSchema("r45/g", float, 30, 10, None),
    FieldSchema("r90/h", float, 40, 10, None),
    FieldSchema("l", float, 50, 10, 1.5),
    FieldSchema("m", float, 60, 10, 1.5),
    FieldSchema("n", float, 70, 10, 1.5),
)

_MATDAMAGE1_CARD3 = (
    FieldSchema("aopt", float, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("macf", int, 30, 10, 1),
)

_MATDAMAGE1_CARD4 = (
    FieldSchema("xp", float, 0, 10, None),
    FieldSchema("yp", float, 10, 10, None),
    FieldSchema("zp", float, 20, 10, None),
    FieldSchema("a1", float, 30, 10, None),
    FieldSchema("a2", float, 40, 10, None),
    FieldSchema("a3", float, 50, 10, None),
)

_MATDAMAGE1_CARD5 = (
    FieldSchema("v1", float, 0, 10, None),
    FieldSchema("v2", float, 10, 10, None),
    FieldSchema("v3", float, 20, 10, None),
    FieldSchema("d1", float, 30, 10, None),
    FieldSchema("d2", float, 40, 10, None),
    FieldSchema("d3", float, 50, 10, None),
    FieldSchema("beta", float, 60, 10, None),
)

_MATDAMAGE1_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatDamage1(KeywordBase):
    """DYNA MAT_DAMAGE_1 keyword"""

    keyword = "MAT"
    subkeyword = "DAMAGE_1"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatDamage1 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATDAMAGE1_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATDAMAGE1_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATDAMAGE1_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATDAMAGE1_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATDAMAGE1_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATDAMAGE1_CARD5,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatDamage1.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATDAMAGE1_OPTION0_CARD0,
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
    def sigy(self) -> typing.Optional[float]:
        """Get or set the Initial yield stress, sigma-0.
        """ # nopep8
        return self._cards[0].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        """Set the sigy property."""
        self._cards[0].set_value("sigy", value)

    @property
    def lcss(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining efective stress versus effective plastic strain. For FLAG = -1.
        """ # nopep8
        return self._cards[0].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        """Set the lcss property."""
        self._cards[0].set_value("lcss", value)

    @property
    def lcds(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining nonlinear damage curve. For FLAG = -1.
        """ # nopep8
        return self._cards[0].get_value("lcds")

    @lcds.setter
    def lcds(self, value: int) -> None:
        """Set the lcds property."""
        self._cards[0].set_value("lcds", value)

    @property
    def q1(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter Q1.
        """ # nopep8
        return self._cards[1].get_value("q1")

    @q1.setter
    def q1(self, value: float) -> None:
        """Set the q1 property."""
        self._cards[1].set_value("q1", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter C1.
        """ # nopep8
        return self._cards[1].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        """Set the c1 property."""
        self._cards[1].set_value("c1", value)

    @property
    def q2(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter Q2.
        """ # nopep8
        return self._cards[1].get_value("q2")

    @q2.setter
    def q2(self, value: float) -> None:
        """Set the q2 property."""
        self._cards[1].set_value("q2", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Isotropic hardening parameter C2.
        """ # nopep8
        return self._cards[1].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        """Set the c2 property."""
        self._cards[1].set_value("c2", value)

    @property
    def epsd(self) -> typing.Optional[float]:
        """Get or set the Damage threshold rd. Damage effective plastic strain when material softening begin (default = 0.0).
        """ # nopep8
        return self._cards[1].get_value("epsd")

    @epsd.setter
    def epsd(self, value: float) -> None:
        """Set the epsd property."""
        self._cards[1].set_value("epsd", value)

    @property
    def espr_s(self) -> typing.Optional[float]:
        """Get or set the Damage material constant S (default = sigma0/200) For FLAG>=0.
        Or
        Plastic strain at which material ruptures (logarithmic).
        """ # nopep8
        return self._cards[1].get_value("espr/s")

    @espr_s.setter
    def espr_s(self, value: float) -> None:
        """Set the espr_s property."""
        self._cards[1].set_value("espr/s", value)

    @property
    def dc(self) -> float:
        """Get or set the Critical damage value Dc. When the damage value D reaches this value, the element is deleted from the calculation (default=0.5)For FLAG>=0.
        """ # nopep8
        return self._cards[1].get_value("dc")

    @dc.setter
    def dc(self, value: float) -> None:
        """Set the dc property."""
        self._cards[1].set_value("dc", value)

    @property
    def flag(self) -> int:
        """Get or set the Flag
        EQ.-1. Anisotropic damage
        EQ.0. No calculation of localization due to damage(default),
        EQ.1:The model flags element where strain localization occur.
        """ # nopep8
        return self._cards[1].get_value("flag")

    @flag.setter
    def flag(self, value: int) -> None:
        """Set the flag property."""
        if value not in [0, -1, 1, None]:
            raise Exception("""flag must be `None` or one of {0,-1,1}.""")
        self._cards[1].set_value("flag", value)

    @property
    def vk(self) -> typing.Optional[float]:
        """Get or set the Viscous material parameter Vk.
        """ # nopep8
        return self._cards[2].get_value("vk")

    @vk.setter
    def vk(self, value: float) -> None:
        """Set the vk property."""
        self._cards[2].set_value("vk", value)

    @property
    def vm(self) -> typing.Optional[float]:
        """Get or set the Viscous material parameter Vm.
        """ # nopep8
        return self._cards[2].get_value("vm")

    @vm.setter
    def vm(self, value: float) -> None:
        """Set the vm property."""
        self._cards[2].set_value("vm", value)

    @property
    def r00_f(self) -> typing.Optional[float]:
        """Get or set the R00 for shell (default = 1.0).
        F for brick (default = 1/2).
        """ # nopep8
        return self._cards[2].get_value("r00/f")

    @r00_f.setter
    def r00_f(self, value: float) -> None:
        """Set the r00_f property."""
        self._cards[2].set_value("r00/f", value)

    @property
    def r45_g(self) -> typing.Optional[float]:
        """Get or set the R45 for shell (default = 1.0).
        G for brick (default = 1/2).
        """ # nopep8
        return self._cards[2].get_value("r45/g")

    @r45_g.setter
    def r45_g(self, value: float) -> None:
        """Set the r45_g property."""
        self._cards[2].set_value("r45/g", value)

    @property
    def r90_h(self) -> typing.Optional[float]:
        """Get or set the R90 for shell (default = 1.0).
        H for brick (default = 1/2).
        """ # nopep8
        return self._cards[2].get_value("r90/h")

    @r90_h.setter
    def r90_h(self, value: float) -> None:
        """Set the r90_h property."""
        self._cards[2].set_value("r90/h", value)

    @property
    def l(self) -> float:
        """Get or set the L for brick (default = 3/2).
        """ # nopep8
        return self._cards[2].get_value("l")

    @l.setter
    def l(self, value: float) -> None:
        """Set the l property."""
        self._cards[2].set_value("l", value)

    @property
    def m(self) -> float:
        """Get or set the M for brick (default =3/2).
        """ # nopep8
        return self._cards[2].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[2].set_value("m", value)

    @property
    def n(self) -> float:
        """Get or set the N for brick (default =3/2).
        """ # nopep8
        return self._cards[2].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[2].set_value("n", value)

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
        return self._cards[3].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        """Set the aopt property."""
        self._cards[3].set_value("aopt", value)

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
        return self._cards[3].get_value("macf")

    @macf.setter
    def macf(self, value: int) -> None:
        """Set the macf property."""
        if value not in [1, 2, 3, 4, -4, -3, -2, None]:
            raise Exception("""macf must be `None` or one of {1,2,3,4,-4,-3,-2}.""")
        self._cards[3].set_value("macf", value)

    @property
    def xp(self) -> typing.Optional[float]:
        """Get or set the Define coordinate of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[4].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[4].set_value("xp", value)

    @property
    def yp(self) -> typing.Optional[float]:
        """Get or set the Define coordinate of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[4].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[4].set_value("yp", value)

    @property
    def zp(self) -> typing.Optional[float]:
        """Get or set the Define coordinate of point p for AOPT = 1 and 4.
        """ # nopep8
        return self._cards[4].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[4].set_value("zp", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the Define component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[4].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the Define component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[4].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the Define component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[4].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[4].set_value("a3", value)

    @property
    def v1(self) -> typing.Optional[float]:
        """Get or set the Define component of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[5].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[5].set_value("v1", value)

    @property
    def v2(self) -> typing.Optional[float]:
        """Get or set the Define component of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[5].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[5].set_value("v2", value)

    @property
    def v3(self) -> typing.Optional[float]:
        """Get or set the Define component of vector v for AOPT = 3 and 4.
        """ # nopep8
        return self._cards[5].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[5].set_value("v3", value)

    @property
    def d1(self) -> typing.Optional[float]:
        """Get or set the Define component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[5].set_value("d1", value)

    @property
    def d2(self) -> typing.Optional[float]:
        """Get or set the Define component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[5].set_value("d2", value)

    @property
    def d3(self) -> typing.Optional[float]:
        """Get or set the Define component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[5].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[5].set_value("d3", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Material angle in degrees for AOPT = 3, may be overridden on the element card, see *ELEMENT_SHELL_BETA or *ELEMENT_SOLID_ORTHO.
        """ # nopep8
        return self._cards[5].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[5].set_value("beta", value)

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

