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

"""Module providing the MatWinfrithConcrete class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATWINFRITHCONCRETE_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("tm", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("ucs", float, 40, 10, None),
    FieldSchema("uts", float, 50, 10, None),
    FieldSchema("fe", float, 60, 10, None),
    FieldSchema("asize", float, 70, 10, None),
)

_MATWINFRITHCONCRETE_CARD1 = (
    FieldSchema("e", float, 0, 10, None),
    FieldSchema("ys", float, 10, 10, None),
    FieldSchema("eh", float, 20, 10, None),
    FieldSchema("uelong", float, 30, 10, None),
    FieldSchema("rate", float, 40, 10, 0.0),
    FieldSchema("conm", float, 50, 10, None),
    FieldSchema("conl", float, 60, 10, None),
    FieldSchema("cont", float, 70, 10, None),
)

_MATWINFRITHCONCRETE_CARD2 = (
    FieldSchema("eps1", float, 0, 10, None),
    FieldSchema("eps2", float, 10, 10, None),
    FieldSchema("eps3", float, 20, 10, None),
    FieldSchema("eps4", float, 30, 10, None),
    FieldSchema("eps5", float, 40, 10, None),
    FieldSchema("eps6", float, 50, 10, None),
    FieldSchema("eps7", float, 60, 10, None),
    FieldSchema("eps8", float, 70, 10, None),
)

_MATWINFRITHCONCRETE_CARD3 = (
    FieldSchema("p1", float, 0, 10, None),
    FieldSchema("p2", float, 10, 10, None),
    FieldSchema("p3", float, 20, 10, None),
    FieldSchema("p4", float, 30, 10, None),
    FieldSchema("p5", float, 40, 10, None),
    FieldSchema("p6", float, 50, 10, None),
    FieldSchema("p7", float, 60, 10, None),
    FieldSchema("p8", float, 70, 10, None),
)

_MATWINFRITHCONCRETE_CARD4 = (
    FieldSchema("maxshr", float, 0, 10, None),
    FieldSchema("lcymt", int, 10, 10, None),
    FieldSchema("lcftt", int, 20, 10, None),
    FieldSchema("lcfct", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("lctst", int, 60, 10, None),
    FieldSchema("lccmp", int, 70, 10, None),
)

_MATWINFRITHCONCRETE_CARD5 = (
    FieldSchema("crfac", float, 0, 10, 0.0),
    FieldSchema("cod1", float, 10, 10, 0.0),
    FieldSchema("tenpwr", float, 20, 10, 1.0),
    FieldSchema("tenrsd", float, 30, 10, 0.01),
    FieldSchema("lcfib", int, 40, 10, None),
    FieldSchema("ro_g", float, 50, 10, 0.0),
    FieldSchema("zsurf", float, 60, 10, 0.0),
    FieldSchema("lcftim", int, 70, 10, None),
)

_MATWINFRITHCONCRETE_CARD6 = (
    FieldSchema("otto", int, 0, 10, 1),
    FieldSchema("dilatd", float, 10, 10, 0.0),
    FieldSchema("dilrat", float, 20, 10, 0.0),
    FieldSchema("degrad", float, 30, 10, 0.0),
    FieldSchema("tfac8", float, 40, 10, 0.9),
    FieldSchema("tlossc", float, 50, 10, 1.0),
    FieldSchema("cdsf", float, 60, 10, 8.0),
    FieldSchema("lcrate", float, 70, 10, None),
)

_MATWINFRITHCONCRETE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatWinfrithConcrete(KeywordBase):
    """DYNA MAT_WINFRITH_CONCRETE keyword"""

    keyword = "MAT"
    subkeyword = "WINFRITH_CONCRETE"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "lcymt": LinkType.DEFINE_CURVE,
        "lcftt": LinkType.DEFINE_CURVE,
        "lcfct": LinkType.DEFINE_CURVE,
        "lctst": LinkType.DEFINE_CURVE,
        "lccmp": LinkType.DEFINE_CURVE,
        "lcfib": LinkType.DEFINE_CURVE,
        "lcftim": LinkType.DEFINE_CURVE,
        "lcrate": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatWinfrithConcrete class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATWINFRITHCONCRETE_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATWINFRITHCONCRETE_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATWINFRITHCONCRETE_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATWINFRITHCONCRETE_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATWINFRITHCONCRETE_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATWINFRITHCONCRETE_CARD5,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATWINFRITHCONCRETE_CARD6,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = MatWinfrithConcrete._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATWINFRITHCONCRETE_OPTION0_CARD0,
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
    def tm(self) -> typing.Optional[float]:
        """Get or set the Initial tangent (Young's) modulus of concrete.
        """ # nopep8
        return self._cards[0].get_value("tm")

    @tm.setter
    def tm(self, value: float) -> None:
        """Set the tm property."""
        self._cards[0].set_value("tm", value)

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
    def ucs(self) -> typing.Optional[float]:
        """Get or set the Uniaxial compressive strength.
        """ # nopep8
        return self._cards[0].get_value("ucs")

    @ucs.setter
    def ucs(self, value: float) -> None:
        """Set the ucs property."""
        self._cards[0].set_value("ucs", value)

    @property
    def uts(self) -> typing.Optional[float]:
        """Get or set the Uniaxial tensile strength.
        """ # nopep8
        return self._cards[0].get_value("uts")

    @uts.setter
    def uts(self, value: float) -> None:
        """Set the uts property."""
        self._cards[0].set_value("uts", value)

    @property
    def fe(self) -> typing.Optional[float]:
        """Get or set the The meaning of FE depends on the value of RATE (see Remark 8):
        RATE.EQ.0: Fracture energy(energy per unit area dissipated in opening the crack)
        RATE.GT.0: Crack width at which the crack-normal tensile stress goes to zero
        """ # nopep8
        return self._cards[0].get_value("fe")

    @fe.setter
    def fe(self, value: float) -> None:
        """Set the fe property."""
        self._cards[0].set_value("fe", value)

    @property
    def asize(self) -> typing.Optional[float]:
        """Get or set the Aggregate size, depending on the value of RATE.
        RATE.LE.1: Aggregate radius in model length units.
        RATE.GE.2: Aggregate diameter in meters.The formula for shear stress carried across cracks with aggregate interlock uses this field; see Remark 11.
        """ # nopep8
        return self._cards[0].get_value("asize")

    @asize.setter
    def asize(self, value: float) -> None:
        """Set the asize property."""
        self._cards[0].set_value("asize", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus of rebar.
        """ # nopep8
        return self._cards[1].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[1].set_value("e", value)

    @property
    def ys(self) -> typing.Optional[float]:
        """Get or set the Yield stress of rebar.
        """ # nopep8
        return self._cards[1].get_value("ys")

    @ys.setter
    def ys(self, value: float) -> None:
        """Set the ys property."""
        self._cards[1].set_value("ys", value)

    @property
    def eh(self) -> typing.Optional[float]:
        """Get or set the Hardening modulus of rebar.
        """ # nopep8
        return self._cards[1].get_value("eh")

    @eh.setter
    def eh(self, value: float) -> None:
        """Set the eh property."""
        self._cards[1].set_value("eh", value)

    @property
    def uelong(self) -> typing.Optional[float]:
        """Get or set the Ultimate elongation before rebar fails.
        """ # nopep8
        return self._cards[1].get_value("uelong")

    @uelong.setter
    def uelong(self, value: float) -> None:
        """Set the uelong property."""
        self._cards[1].set_value("uelong", value)

    @property
    def rate(self) -> float:
        """Get or set the Material model option (see Remarks 8 and 13):
        EQ.0.0: Original Broadhouse implementation with strain rate effects  included. WARNING: This option does not guarantee energy conservation.
        EQ.1.0: Original Broadhouse implementation with strain rate effects turned off.
        EQ.2.0: Like RATE = 1 but includes an improved crack algorithm.It is superseded by RATE = 8.
        EQ.8.0: Improved crack algorithm plus additional inputs on Cards 5 through 7 (recommended).
        """ # nopep8
        return self._cards[1].get_value("rate")

    @rate.setter
    def rate(self, value: float) -> None:
        """Set the rate property."""
        if value not in [0, 1, 2, 8, None]:
            raise Exception("""rate must be `None` or one of {0,1,2,8}.""")
        self._cards[1].set_value("rate", value)

    @property
    def conm(self) -> typing.Optional[float]:
        """Get or set the Units (conversion) flag:
        GT.0.0: Factor to convert model mass units to kg
        EQ. - 1.0: Mass, length,and time units in the model are lbf*sec2/in, inch,and sec.
        EQ. - 2.0: Mass, length,and time units in the model are g, cm,and microsec.
        EQ. - 3.0: Mass, length,and time units in the model are g, mm,and msec.
        EQ. - 4.0: Mass, length,and time units in the model are metric ton, mm,and sec.
        EQ. - 5.0: Mass, length,and time units in the model are kg, mm,and msec.
        """ # nopep8
        return self._cards[1].get_value("conm")

    @conm.setter
    def conm(self, value: float) -> None:
        """Set the conm property."""
        self._cards[1].set_value("conm", value)

    @property
    def conl(self) -> typing.Optional[float]:
        """Get or set the Length units conversion factor:
        CONM.GT.0: CONL is the conversion factor from model length units to meters(for instance, CONL = 0.001 for millimeters).
        CONM.LE.0: CONL is ignored.
        """ # nopep8
        return self._cards[1].get_value("conl")

    @conl.setter
    def conl(self, value: float) -> None:
        """Set the conl property."""
        self._cards[1].set_value("conl", value)

    @property
    def cont(self) -> typing.Optional[float]:
        """Get or set the Time units conversion factor:
        CONM.GT.0: CONT is the conversion factor from time units to seconds(for example, CONT = 0.001 for milliseconds).
        CONM.LE.0: CONT is ignored.
        """ # nopep8
        return self._cards[1].get_value("cont")

    @cont.setter
    def cont(self, value: float) -> None:
        """Set the cont property."""
        self._cards[1].set_value("cont", value)

    @property
    def eps1(self) -> typing.Optional[float]:
        """Get or set the Volumetric strain values (natural logarithmic values); see Remark 3.
        If this card is not left blank, a minimum of 2 values must be defined and a maximum of 8 values are allowed.
        """ # nopep8
        return self._cards[2].get_value("eps1")

    @eps1.setter
    def eps1(self, value: float) -> None:
        """Set the eps1 property."""
        self._cards[2].set_value("eps1", value)

    @property
    def eps2(self) -> typing.Optional[float]:
        """Get or set the Volumetric strain values (natural logarithmic values); see Remark 3.
        If this card is not left blank, a minimum of 2 values must be defined and a maximum of 8 values are allowed.
        """ # nopep8
        return self._cards[2].get_value("eps2")

    @eps2.setter
    def eps2(self, value: float) -> None:
        """Set the eps2 property."""
        self._cards[2].set_value("eps2", value)

    @property
    def eps3(self) -> typing.Optional[float]:
        """Get or set the Volumetric strain values (natural logarithmic values); see Remark 3.
        If this card is not left blank, a minimum of 2 values must be defined and a maximum of 8 values are allowed.
        """ # nopep8
        return self._cards[2].get_value("eps3")

    @eps3.setter
    def eps3(self, value: float) -> None:
        """Set the eps3 property."""
        self._cards[2].set_value("eps3", value)

    @property
    def eps4(self) -> typing.Optional[float]:
        """Get or set the Volumetric strain values (natural logarithmic values); see Remark 3.
        If this card is not left blank, a minimum of 2 values must be defined and a maximum of 8 values are allowed.
        """ # nopep8
        return self._cards[2].get_value("eps4")

    @eps4.setter
    def eps4(self, value: float) -> None:
        """Set the eps4 property."""
        self._cards[2].set_value("eps4", value)

    @property
    def eps5(self) -> typing.Optional[float]:
        """Get or set the Volumetric strain values (natural logarithmic values); see Remark 3.
        If this card is not left blank, a minimum of 2 values must be defined and a maximum of 8 values are allowed.
        """ # nopep8
        return self._cards[2].get_value("eps5")

    @eps5.setter
    def eps5(self, value: float) -> None:
        """Set the eps5 property."""
        self._cards[2].set_value("eps5", value)

    @property
    def eps6(self) -> typing.Optional[float]:
        """Get or set the Volumetric strain values (natural logarithmic values); see Remark 3.
        If this card is not left blank, a minimum of 2 values must be defined and a maximum of 8 values are allowed.
        """ # nopep8
        return self._cards[2].get_value("eps6")

    @eps6.setter
    def eps6(self, value: float) -> None:
        """Set the eps6 property."""
        self._cards[2].set_value("eps6", value)

    @property
    def eps7(self) -> typing.Optional[float]:
        """Get or set the Volumetric strain values (natural logarithmic values); see Remark 3.
        If this card is not left blank, a minimum of 2 values must be defined and a maximum of 8 values are allowed.
        """ # nopep8
        return self._cards[2].get_value("eps7")

    @eps7.setter
    def eps7(self, value: float) -> None:
        """Set the eps7 property."""
        self._cards[2].set_value("eps7", value)

    @property
    def eps8(self) -> typing.Optional[float]:
        """Get or set the Volumetric strain values (natural logarithmic values); see Remark 3.
        If this card is not left blank, a minimum of 2 values must be defined and a maximum of 8 values are allowed.
        """ # nopep8
        return self._cards[2].get_value("eps8")

    @eps8.setter
    def eps8(self, value: float) -> None:
        """Set the eps8 property."""
        self._cards[2].set_value("eps8", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the Pressures corresponding to first volumetric strain value.
        """ # nopep8
        return self._cards[3].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        """Set the p1 property."""
        self._cards[3].set_value("p1", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the Pressures corresponding to second volumetric strain value.
        """ # nopep8
        return self._cards[3].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        """Set the p2 property."""
        self._cards[3].set_value("p2", value)

    @property
    def p3(self) -> typing.Optional[float]:
        """Get or set the Pressures corresponding to third volumetric strain value.
        """ # nopep8
        return self._cards[3].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        """Set the p3 property."""
        self._cards[3].set_value("p3", value)

    @property
    def p4(self) -> typing.Optional[float]:
        """Get or set the Pressures corresponding to fourth volumetric strain value.
        """ # nopep8
        return self._cards[3].get_value("p4")

    @p4.setter
    def p4(self, value: float) -> None:
        """Set the p4 property."""
        self._cards[3].set_value("p4", value)

    @property
    def p5(self) -> typing.Optional[float]:
        """Get or set the Pressures corresponding to fifth volumetric strain value.
        """ # nopep8
        return self._cards[3].get_value("p5")

    @p5.setter
    def p5(self, value: float) -> None:
        """Set the p5 property."""
        self._cards[3].set_value("p5", value)

    @property
    def p6(self) -> typing.Optional[float]:
        """Get or set the Pressures corresponding to sixth volumetric strain value.
        """ # nopep8
        return self._cards[3].get_value("p6")

    @p6.setter
    def p6(self, value: float) -> None:
        """Set the p6 property."""
        self._cards[3].set_value("p6", value)

    @property
    def p7(self) -> typing.Optional[float]:
        """Get or set the Pressures corresponding to seventh volumetric strain value.
        """ # nopep8
        return self._cards[3].get_value("p7")

    @p7.setter
    def p7(self, value: float) -> None:
        """Set the p7 property."""
        self._cards[3].set_value("p7", value)

    @property
    def p8(self) -> typing.Optional[float]:
        """Get or set the Pressures corresponding to eight volumetric strain value.
        """ # nopep8
        return self._cards[3].get_value("p8")

    @p8.setter
    def p8(self, value: float) -> None:
        """Set the p8 property."""
        self._cards[3].set_value("p8", value)

    @property
    def maxshr(self) -> typing.Optional[float]:
        """Get or set the Maximum shear stress that can be carried across a crack under conditions
        of zero normal stress on the crack and zero crack opening displacement.
        The default value is 1.161 times UTS; see Remark 11.
        """ # nopep8
        return self._cards[4].get_value("maxshr")

    @maxshr.setter
    def maxshr(self, value: float) -> None:
        """Set the maxshr property."""
        self._cards[4].set_value("maxshr", value)

    @property
    def lcymt(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID governing the variation of elastic stiffness with temperature.
        The x-axis is temperature, and the y-axis is a nondimensional factor on elastic modulus TM.
        """ # nopep8
        return self._cards[4].get_value("lcymt")

    @lcymt.setter
    def lcymt(self, value: int) -> None:
        """Set the lcymt property."""
        self._cards[4].set_value("lcymt", value)

    @property
    def lcftt(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID governing the variation of tensile strength with temperature.
        The x-axis is temperature, and the y-axis is a nondimensional factor on tensile strength UTS. See Remark 9.
        """ # nopep8
        return self._cards[4].get_value("lcftt")

    @lcftt.setter
    def lcftt(self, value: int) -> None:
        """Set the lcftt property."""
        self._cards[4].set_value("lcftt", value)

    @property
    def lcfct(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID governing the variation of compressive strength with temperature.
        The x-axis is temperature, and the y-axis is a nondimensional factor on compressive strength UCS.
        """ # nopep8
        return self._cards[4].get_value("lcfct")

    @lcfct.setter
    def lcfct(self, value: int) -> None:
        """Set the lcfct property."""
        self._cards[4].set_value("lcfct", value)

    @property
    def lctst(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID governing the post-cracking tensile response. See Remark 8.
        The x-axis is crack-opening displacement (length units), and the y-axis is a
        nondimensional factor on tensile strength UTS. If LCTST is defined, it overrides FE on Card 1.
        The first point should be (0, 1).
        """ # nopep8
        return self._cards[4].get_value("lctst")

    @lctst.setter
    def lctst(self, value: int) -> None:
        """Set the lctst property."""
        self._cards[4].set_value("lctst", value)

    @property
    def lccmp(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID governing post-yield compression/shear response.
        The x-axis is plastic strain,and  the y-axis is a nondimensional factor that scales UCS. See Remark 14.
        """ # nopep8
        return self._cards[4].get_value("lccmp")

    @lccmp.setter
    def lccmp(self, value: int) -> None:
        """Set the lccmp property."""
        self._cards[4].set_value("lccmp", value)

    @property
    def crfac(self) -> float:
        """Get or set the Scale the tensile strength of uncracked elements by (1 -  CRFAC) if an adjacent element has cracked. See Remark 9.
        """ # nopep8
        return self._cards[5].get_value("crfac")

    @crfac.setter
    def crfac(self, value: float) -> None:
        """Set the crfac property."""
        self._cards[5].set_value("crfac", value)

    @property
    def cod1(self) -> float:
        """Get or set the Crack opening displacement (length units) of the adjacent element at which the full value of CRFAC applies.
        For crack opening displacements smaller than COD1, linear interpolation is applied. See Remark 9.
        """ # nopep8
        return self._cards[5].get_value("cod1")

    @cod1.setter
    def cod1(self, value: float) -> None:
        """Set the cod1 property."""
        self._cards[5].set_value("cod1", value)

    @property
    def tenpwr(self) -> float:
        """Get or set the Power law term governing tensile strength when at least one principal stress is compressive. See Remark 9.
        """ # nopep8
        return self._cards[5].get_value("tenpwr")

    @tenpwr.setter
    def tenpwr(self, value: float) -> None:
        """Set the tenpwr property."""
        self._cards[5].set_value("tenpwr", value)

    @property
    def tenrsd(self) -> float:
        """Get or set the Residual factor term governing tensile strength when at least one principal stress is compressive. See Remark 9.
        """ # nopep8
        return self._cards[5].get_value("tenrsd")

    @tenrsd.setter
    def tenrsd(self, value: float) -> None:
        """Set the tenrsd property."""
        self._cards[5].set_value("tenrsd", value)

    @property
    def lcfib(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID for fiber-reinforced concrete. The x-axis of the curve is crack-opening
        displacement (length units), and the y-axis is additional tensile stress due to the presence of the fibers. See Remark 10.
        """ # nopep8
        return self._cards[5].get_value("lcfib")

    @lcfib.setter
    def lcfib(self, value: int) -> None:
        """Set the lcfib property."""
        self._cards[5].set_value("lcfib", value)

    @property
    def ro_g(self) -> float:
        """Get or set the A nonzero RO_G invokes the option for water pressure to be applied within cracks.
        The value of RO_G is water density times acceleration due to gravity. See Remark 22.
        """ # nopep8
        return self._cards[5].get_value("ro_g")

    @ro_g.setter
    def ro_g(self, value: float) -> None:
        """Set the ro_g property."""
        self._cards[5].set_value("ro_g", value)

    @property
    def zsurf(self) -> float:
        """Get or set the Global z-coordinate of the water surface, used for calculating water pressure in cracks. See Remark 22.
        """ # nopep8
        return self._cards[5].get_value("zsurf")

    @zsurf.setter
    def zsurf(self, value: float) -> None:
        """Set the zsurf property."""
        self._cards[5].set_value("zsurf", value)

    @property
    def lcftim(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID giving scaling factor on tensile strength (UTS) as a function of time. See Remarks 9 and 21.
        """ # nopep8
        return self._cards[5].get_value("lcftim")

    @lcftim.setter
    def lcftim(self, value: int) -> None:
        """Set the lcftim property."""
        self._cards[5].set_value("lcftim", value)

    @property
    def otto(self) -> int:
        """Get or set the Option for automatic calculation of the Ottosen yield surface constants (see Remark 13):
        EQ.1: fib Model Code 2010, normal weight concrete
        EQ.2: fib Model Code 2010, lightweight concrete
        EQ.3: Same as RATE = 0, 1 or 2 (Broadhouse model).
        """ # nopep8
        return self._cards[6].get_value("otto")

    @otto.setter
    def otto(self, value: int) -> None:
        """Set the otto property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""otto must be `None` or one of {1,2,3}.""")
        self._cards[6].set_value("otto", value)

    @property
    def dilatd(self) -> float:
        """Get or set the Maximum dilation displacement (in model length units) due to crack sliding or yielding.
        """ # nopep8
        return self._cards[6].get_value("dilatd")

    @dilatd.setter
    def dilatd(self, value: float) -> None:
        """Set the dilatd property."""
        self._cards[6].set_value("dilatd", value)

    @property
    def dilrat(self) -> float:
        """Get or set the Initial dilation ratio.
        """ # nopep8
        return self._cards[6].get_value("dilrat")

    @dilrat.setter
    def dilrat(self, value: float) -> None:
        """Set the dilrat property."""
        self._cards[6].set_value("dilrat", value)

    @property
    def degrad(self) -> float:
        """Get or set the Lower limit on the factor by which the compressive strength of cracked elements is scaled (see Remark 15):
        EQ.0: No reduction of compressive strength
        GT.0: Equation from Eurocode 2 with lower limit = DEGRAD.
        """ # nopep8
        return self._cards[6].get_value("degrad")

    @degrad.setter
    def degrad(self, value: float) -> None:
        """Set the degrad property."""
        self._cards[6].set_value("degrad", value)

    @property
    def tfac8(self) -> float:
        """Get or set the Nondimensional modification factor applied to any tensile principal stresses when calculating the Ottosen yield function; see Remark 16.
        """ # nopep8
        return self._cards[6].get_value("tfac8")

    @tfac8.setter
    def tfac8(self, value: float) -> None:
        """Set the tfac8 property."""
        self._cards[6].set_value("tfac8", value)

    @property
    def tlossc(self) -> float:
        """Get or set the Nondimensional parameter controlling loss of tensile strength in crushed elements; see Remark 9.
        """ # nopep8
        return self._cards[6].get_value("tlossc")

    @tlossc.setter
    def tlossc(self, value: float) -> None:
        """Set the tlossc property."""
        self._cards[6].set_value("tlossc", value)

    @property
    def cdsf(self) -> float:
        """Get or set the Nondimensional ductility factor for confined concrete. CDSF controls scaling of the x-axis of LCCMP; see Remark 14.
        """ # nopep8
        return self._cards[6].get_value("cdsf")

    @cdsf.setter
    def cdsf(self, value: float) -> None:
        """Set the cdsf property."""
        self._cards[6].set_value("cdsf", value)

    @property
    def lcrate(self) -> typing.Optional[float]:
        """Get or set the Optional load curve ID, |LCRATE| giving the strength enhancement factor as a function of strain rate (see Remark 23):
        GT.0: Rate effects treated by a simple scaling of the strength
        LT.0 : Rate effects treated by a fully viscoplastic formulation
        """ # nopep8
        return self._cards[6].get_value("lcrate")

    @lcrate.setter
    def lcrate(self, value: float) -> None:
        """Set the lcrate property."""
        self._cards[6].set_value("lcrate", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[7].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def lcymt_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcymt."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcymt:
                return kwd
        return None

    @lcymt_link.setter
    def lcymt_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcymt."""
        self.lcymt = value.lcid

    @property
    def lcftt_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcftt."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcftt:
                return kwd
        return None

    @lcftt_link.setter
    def lcftt_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcftt."""
        self.lcftt = value.lcid

    @property
    def lcfct_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcfct."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcfct:
                return kwd
        return None

    @lcfct_link.setter
    def lcfct_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcfct."""
        self.lcfct = value.lcid

    @property
    def lctst_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lctst."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lctst:
                return kwd
        return None

    @lctst_link.setter
    def lctst_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lctst."""
        self.lctst = value.lcid

    @property
    def lccmp_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lccmp."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lccmp:
                return kwd
        return None

    @lccmp_link.setter
    def lccmp_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lccmp."""
        self.lccmp = value.lcid

    @property
    def lcfib_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcfib."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcfib:
                return kwd
        return None

    @lcfib_link.setter
    def lcfib_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcfib."""
        self.lcfib = value.lcid

    @property
    def lcftim_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcftim."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcftim:
                return kwd
        return None

    @lcftim_link.setter
    def lcftim_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcftim."""
        self.lcftim = value.lcid

    @property
    def lcrate_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcrate."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcrate:
                return kwd
        return None

    @lcrate_link.setter
    def lcrate_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcrate."""
        self.lcrate = value.lcid

