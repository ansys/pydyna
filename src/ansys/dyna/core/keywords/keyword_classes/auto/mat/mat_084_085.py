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

"""Module providing the Mat084085 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MAT084085_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("tm", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("ucs", float, 40, 10, None),
    FieldSchema("uts", float, 50, 10, None),
    FieldSchema("fe", float, 60, 10, None),
    FieldSchema("asize", float, 70, 10, None),
)

_MAT084085_CARD1 = (
    FieldSchema("e", float, 0, 10, None),
    FieldSchema("ys", float, 10, 10, None),
    FieldSchema("eh", float, 20, 10, None),
    FieldSchema("uelong", float, 30, 10, None),
    FieldSchema("rate", float, 40, 10, 0.0),
    FieldSchema("conm", float, 50, 10, None),
    FieldSchema("conl", float, 60, 10, None),
    FieldSchema("cont", float, 70, 10, None),
)

_MAT084085_CARD2 = (
    FieldSchema("eps1", float, 0, 10, None),
    FieldSchema("eps2", float, 10, 10, None),
    FieldSchema("eps3", float, 20, 10, None),
    FieldSchema("eps4", float, 30, 10, None),
    FieldSchema("eps5", float, 40, 10, None),
    FieldSchema("eps6", float, 50, 10, None),
    FieldSchema("eps7", float, 60, 10, None),
    FieldSchema("eps8", float, 70, 10, None),
)

_MAT084085_CARD3 = (
    FieldSchema("p1", float, 0, 10, None),
    FieldSchema("p2", float, 10, 10, None),
    FieldSchema("p3", float, 20, 10, None),
    FieldSchema("p4", float, 30, 10, None),
    FieldSchema("p5", float, 40, 10, None),
    FieldSchema("p6", float, 50, 10, None),
    FieldSchema("p7", float, 60, 10, None),
    FieldSchema("p8", float, 70, 10, None),
)

_MAT084085_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat084085(KeywordBase):
    """DYNA MAT_084/085 keyword"""

    keyword = "MAT"
    subkeyword = "084/085"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat084085 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT084085_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT084085_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT084085_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT084085_CARD3,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = Mat084085._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT084085_OPTION0_CARD0,
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
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[4].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

