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

"""Module providing the Mat194 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MAT194_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, 0.0),
    FieldSchema("tmax", float, 40, 10, 0.0),
)

_MAT194_CARD1 = (
    FieldSchema("fc_", float, 0, 10, 0.0, "fc'"),
    FieldSchema("pref", float, 10, 10, 0.0),
    FieldSchema("fyield", float, 20, 10, 0.0),
    FieldSchema("sig0", float, 30, 10, 0.0),
    FieldSchema("unconv", float, 40, 10, 0.0),
    FieldSchema("alpha", float, 50, 10, 0.0),
    FieldSchema("ft", float, 60, 10, 0.0),
    FieldSchema("erienf", float, 70, 10, 0.0),
)

_MAT194_CARD2 = (
    FieldSchema("a", float, 0, 10, 0.05),
    FieldSchema("b", float, 10, 10, 0.55),
    FieldSchema("c", float, 20, 10, 0.125),
    FieldSchema("d", float, 30, 10, 0.66),
    FieldSchema("e", float, 40, 10, 0.25),
    FieldSchema("f", float, 50, 10, 1.0),
)

_MAT194_CARD3 = (
    FieldSchema("y1", float, 0, 10, 0.0),
    FieldSchema("y2", float, 10, 10, 0.0),
    FieldSchema("y3", float, 20, 10, 0.0),
    FieldSchema("y4", float, 30, 10, 0.0),
    FieldSchema("y5", float, 40, 10, 0.0),
)

_MAT194_CARD4 = (
    FieldSchema("t1", float, 0, 10, 0.0),
    FieldSchema("t2", float, 10, 10, 0.0),
    FieldSchema("t3", float, 20, 10, 0.0),
    FieldSchema("t4", float, 30, 10, 0.0),
    FieldSchema("t5", float, 40, 10, 0.0),
)

_MAT194_CARD5 = (
    FieldSchema("aopt", float, 0, 10, 0.0),
)

_MAT194_CARD6 = (
    FieldSchema("xp", float, 0, 10, 0.0),
    FieldSchema("yp", float, 10, 10, 0.0),
    FieldSchema("zp", float, 20, 10, 0.0),
    FieldSchema("a1", float, 30, 10, 0.0),
    FieldSchema("a2", float, 40, 10, 0.0),
    FieldSchema("a3", float, 50, 10, 0.0),
)

_MAT194_CARD7 = (
    FieldSchema("v1", float, 0, 10, 0.0),
    FieldSchema("v2", float, 10, 10, 0.0),
    FieldSchema("v3", float, 20, 10, 0.0),
    FieldSchema("d1", float, 30, 10, 0.0),
    FieldSchema("d2", float, 40, 10, 0.0),
    FieldSchema("d3", float, 50, 10, 0.0),
    FieldSchema("beta", float, 60, 10, 0.0),
)

_MAT194_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat194(KeywordBase):
    """DYNA MAT_194 keyword"""

    keyword = "MAT"
    subkeyword = "194"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat194 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT194_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT194_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT194_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT194_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT194_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT194_CARD5,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT194_CARD6,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT194_CARD7,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = Mat194._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT194_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number or label not must be specified (see *PART).
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
    def pr(self) -> float:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def tmax(self) -> float:
        """Get or set the Ultimate in-plane shear stress. If set to zero, LS-DYNA calculates TMAX based on the formulae in the Uniform Building Code, using the data on Card 2. See Remark 3.
        """ # nopep8
        return self._cards[0].get_value("tmax")

    @tmax.setter
    def tmax(self, value: float) -> None:
        """Set the tmax property."""
        self._cards[0].set_value("tmax", value)

    @property
    def fc_(self) -> float:
        """Get or set the Unconfined Compressive Strength of concrete.
        Used in the calculation of ultimate shear stress; crushing behaviour is not modelled.
        """ # nopep8
        return self._cards[1].get_value("fc_")

    @fc_.setter
    def fc_(self, value: float) -> None:
        """Set the fc_ property."""
        self._cards[1].set_value("fc_", value)

    @property
    def pref(self) -> float:
        """Get or set the Percent reinforcement, For example, if 1.2% of the material is reinforcement, enter 1.2.
        """ # nopep8
        return self._cards[1].get_value("pref")

    @pref.setter
    def pref(self, value: float) -> None:
        """Set the pref property."""
        self._cards[1].set_value("pref", value)

    @property
    def fyield(self) -> float:
        """Get or set the Yield stress of reinforcement.
        """ # nopep8
        return self._cards[1].get_value("fyield")

    @fyield.setter
    def fyield(self, value: float) -> None:
        """Set the fyield property."""
        self._cards[1].set_value("fyield", value)

    @property
    def sig0(self) -> float:
        """Get or set the Overburden stress (in-plane compressive stress). It is used in the calculation of ultimate shear stress. Usually, SIG0 is left as zero.
        """ # nopep8
        return self._cards[1].get_value("sig0")

    @sig0.setter
    def sig0(self, value: float) -> None:
        """Set the sig0 property."""
        self._cards[1].set_value("sig0", value)

    @property
    def unconv(self) -> float:
        """Get or set the Unit conversion factor. UCONV = SQRT (1.0 psi in the model stress units).
        This factor is used to convert the ultimate tensile stress of concrete which is expressed as SQRT(FC) where FC is given in PSI.
        Therefore a unit conversion factor of sqrt(PSI/STRESS UNIT) is required.
        Examples:
        UCONV = 83.3 = sqrt(6894) if the stress unit is N/m2
        UCONV = 0.083 if the stress unit is MN/m2 or N/mm2
        """ # nopep8
        return self._cards[1].get_value("unconv")

    @unconv.setter
    def unconv(self, value: float) -> None:
        """Set the unconv property."""
        self._cards[1].set_value("unconv", value)

    @property
    def alpha(self) -> float:
        """Get or set the Shear span factor. See Remark 3.
        """ # nopep8
        return self._cards[1].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[1].set_value("alpha", value)

    @property
    def ft(self) -> float:
        """Get or set the Cracking stress in direct tension.
        See Remark 5. Default is 8% of the cylinder strength.
        """ # nopep8
        return self._cards[1].get_value("ft")

    @ft.setter
    def ft(self, value: float) -> None:
        """Set the ft property."""
        self._cards[1].set_value("ft", value)

    @property
    def erienf(self) -> float:
        """Get or set the Young's modulus of the reinforcement. It is used to calculate the post-cracked stiffness. See Remark 5.
        """ # nopep8
        return self._cards[1].get_value("erienf")

    @erienf.setter
    def erienf(self, value: float) -> None:
        """Set the erienf property."""
        self._cards[1].set_value("erienf", value)

    @property
    def a(self) -> float:
        """Get or set the Hysteresis constants determining the shape of the hysteresis loops.
        Default is set to 0.05.
        """ # nopep8
        return self._cards[2].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[2].set_value("a", value)

    @property
    def b(self) -> float:
        """Get or set the Hysteresis constants determining the shape of the hysteresis loops.
        Default is set to 0.55.
        """ # nopep8
        return self._cards[2].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[2].set_value("b", value)

    @property
    def c(self) -> float:
        """Get or set the Hysteresis constants determining the shape of the hysteresis loops.
        Default is set to 0.125.
        """ # nopep8
        return self._cards[2].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[2].set_value("c", value)

    @property
    def d(self) -> float:
        """Get or set the Hysteresis constants determining the shape of the hysteresis loops.
        Default is set to 0.66.
        """ # nopep8
        return self._cards[2].get_value("d")

    @d.setter
    def d(self, value: float) -> None:
        """Set the d property."""
        self._cards[2].set_value("d", value)

    @property
    def e(self) -> float:
        """Get or set the Hysteresis constants determining the shape of the hysteresis loops.
        Default is set to 0.25.
        """ # nopep8
        return self._cards[2].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[2].set_value("e", value)

    @property
    def f(self) -> float:
        """Get or set the Strength degradation factor. After the ultimate shear stress has been achieved, F multiplies the maximum shear stress from the curve for subsequent reloading.
        F=1.0 implies no strength degradation (default).
        F=0.5 implies that the strength is halved for subsequent reloading.
        """ # nopep8
        return self._cards[2].get_value("f")

    @f.setter
    def f(self, value: float) -> None:
        """Set the f property."""
        self._cards[2].set_value("f", value)

    @property
    def y1(self) -> float:
        """Get or set the Engineering shear strain points on stress-strain curve. By default, these are calculated from the values on Card 1. See Remark 3.
        """ # nopep8
        return self._cards[3].get_value("y1")

    @y1.setter
    def y1(self, value: float) -> None:
        """Set the y1 property."""
        self._cards[3].set_value("y1", value)

    @property
    def y2(self) -> float:
        """Get or set the Engineering shear strain points on stress-strain curve. By default, these are calculated from the values on Card 1. See Remark 3.
        """ # nopep8
        return self._cards[3].get_value("y2")

    @y2.setter
    def y2(self, value: float) -> None:
        """Set the y2 property."""
        self._cards[3].set_value("y2", value)

    @property
    def y3(self) -> float:
        """Get or set the Engineering shear strain points on stress-strain curve. By default, these are calculated from the values on Card 1. See Remark 3.
        """ # nopep8
        return self._cards[3].get_value("y3")

    @y3.setter
    def y3(self, value: float) -> None:
        """Set the y3 property."""
        self._cards[3].set_value("y3", value)

    @property
    def y4(self) -> float:
        """Get or set the Engineering shear strain points on stress-strain curve. By default, these are calculated from the values on Card 1. See Remark 3.
        """ # nopep8
        return self._cards[3].get_value("y4")

    @y4.setter
    def y4(self, value: float) -> None:
        """Set the y4 property."""
        self._cards[3].set_value("y4", value)

    @property
    def y5(self) -> float:
        """Get or set the Engineering shear strain points on stress-strain curve. By default, these are calculated from the values on Card 1. See Remark 3.
        """ # nopep8
        return self._cards[3].get_value("y5")

    @y5.setter
    def y5(self, value: float) -> None:
        """Set the y5 property."""
        self._cards[3].set_value("y5", value)

    @property
    def t1(self) -> float:
        """Get or set the Shear stress points on stress-strain curve. By default, these are calculated from the values on Card 1. See Remark 3.
        """ # nopep8
        return self._cards[4].get_value("t1")

    @t1.setter
    def t1(self, value: float) -> None:
        """Set the t1 property."""
        self._cards[4].set_value("t1", value)

    @property
    def t2(self) -> float:
        """Get or set the Shear stress points on stress-strain curve. By default, these are calculated from the values on Card 1. See Remark 3.
        """ # nopep8
        return self._cards[4].get_value("t2")

    @t2.setter
    def t2(self, value: float) -> None:
        """Set the t2 property."""
        self._cards[4].set_value("t2", value)

    @property
    def t3(self) -> float:
        """Get or set the Shear stress points on stress-strain curve. By default, these are calculated from the values on Card 1. See Remark 3.
        """ # nopep8
        return self._cards[4].get_value("t3")

    @t3.setter
    def t3(self, value: float) -> None:
        """Set the t3 property."""
        self._cards[4].set_value("t3", value)

    @property
    def t4(self) -> float:
        """Get or set the Shear stress points on stress-strain curve. By default, these are calculated from the values on Card 1. See Remark 3.
        """ # nopep8
        return self._cards[4].get_value("t4")

    @t4.setter
    def t4(self, value: float) -> None:
        """Set the t4 property."""
        self._cards[4].set_value("t4", value)

    @property
    def t5(self) -> float:
        """Get or set the Shear stress points on stress-strain curve. By default, these are calculated from the values on Card 1. See Remark 3.
        """ # nopep8
        return self._cards[4].get_value("t5")

    @t5.setter
    def t5(self, value: float) -> None:
        """Set the t5 property."""
        self._cards[4].set_value("t5", value)

    @property
    def aopt(self) -> float:
        """Get or set the Material axes option(see *MAT_OPTIONTROPIC_ELASTIC for more details):
        EQ.0.0: Locally orthotropic with material axes determined by
        element nodes as shown in Figure ERRORError! Reference source not found., and then rotated about the shell element normal by the angle BETA.
        EQ.2.0: Globally orthotropic with material axes determined by vectors defined below, as with *DEFINE_COORDI_NATE_VECTOR.
        EQ.3.0:Locally orthotropic material axes determined by a vector v and the normal vector to the plane of the element (see
        Figure Error! Reference source not found.). a is determined by taking the cross product of v with the normal vector, b is
        determined by taking the cross product of the normal vector with a, and c is the normal vector. Then a and b are rotated
        about c by an angle BETA. BETA may be set in the keyword input for the element or in the input for this keyword.
        LT.0.0: The absolute value of AOPT is a coordinate system ID number (CID on *DEFINE_COORDINATE_NODES,
        *DEFINE_COORDINATE_SYSTEM or *DEFINE_COOR_DINATE_VECTOR).
        """ # nopep8
        return self._cards[5].get_value("aopt")

    @aopt.setter
    def aopt(self, value: float) -> None:
        """Set the aopt property."""
        self._cards[5].set_value("aopt", value)

    @property
    def xp(self) -> float:
        """Get or set the x-coordinate of point p for AOPT = 1.
        """ # nopep8
        return self._cards[6].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[6].set_value("xp", value)

    @property
    def yp(self) -> float:
        """Get or set the y-coordinate of point p for AOPT = 1.
        """ # nopep8
        return self._cards[6].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[6].set_value("yp", value)

    @property
    def zp(self) -> float:
        """Get or set the z-coordinate of point p for AOPT = 1.
        """ # nopep8
        return self._cards[6].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[6].set_value("zp", value)

    @property
    def a1(self) -> float:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[6].set_value("a1", value)

    @property
    def a2(self) -> float:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[6].set_value("a2", value)

    @property
    def a3(self) -> float:
        """Get or set the Component of vector a for AOPT = 2.
        """ # nopep8
        return self._cards[6].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[6].set_value("a3", value)

    @property
    def v1(self) -> float:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[7].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        """Set the v1 property."""
        self._cards[7].set_value("v1", value)

    @property
    def v2(self) -> float:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[7].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        """Set the v2 property."""
        self._cards[7].set_value("v2", value)

    @property
    def v3(self) -> float:
        """Get or set the Component of vector v for AOPT = 3.
        """ # nopep8
        return self._cards[7].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        """Set the v3 property."""
        self._cards[7].set_value("v3", value)

    @property
    def d1(self) -> float:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[7].get_value("d1")

    @d1.setter
    def d1(self, value: float) -> None:
        """Set the d1 property."""
        self._cards[7].set_value("d1", value)

    @property
    def d2(self) -> float:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[7].get_value("d2")

    @d2.setter
    def d2(self, value: float) -> None:
        """Set the d2 property."""
        self._cards[7].set_value("d2", value)

    @property
    def d3(self) -> float:
        """Get or set the Component of vector d for AOPT = 2.
        """ # nopep8
        return self._cards[7].get_value("d3")

    @d3.setter
    def d3(self, value: float) -> None:
        """Set the d3 property."""
        self._cards[7].set_value("d3", value)

    @property
    def beta(self) -> float:
        """Get or set the Material angle in degrees for AOPT = 0 and 3. It may be overridden on the element card, see *ELEMENT_SHELL_BETA.
        """ # nopep8
        return self._cards[7].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[7].set_value("beta", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[8].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[8].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

