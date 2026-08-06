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

"""Module providing the Eos043 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EOS043_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("a", float, 10, 10, None),
    FieldSchema("b", float, 20, 10, None),
    FieldSchema("r1", float, 30, 10, None),
    FieldSchema("r2", float, 40, 10, None),
    FieldSchema("w", float, 50, 10, None),
    FieldSchema("detv", float, 60, 10, None),
    FieldSchema("ezig", float, 70, 10, None),
)

_EOS043_CARD1 = (
    FieldSchema("pcj", float, 0, 10, None),
    FieldSchema("wreac", float, 10, 10, None),
    FieldSchema("dfmax", float, 20, 10, None),
    FieldSchema("rri", float, 30, 10, None),
    FieldSchema("rrb", float, 40, 10, None),
    FieldSchema("rra", float, 50, 10, None),
    FieldSchema("rrx", float, 60, 10, None),
)

_EOS043_CARD2 = (
    FieldSchema("rrg1", float, 0, 10, None),
    FieldSchema("rrc", float, 10, 10, None),
    FieldSchema("rrd", float, 20, 10, None),
    FieldSchema("rry", float, 30, 10, None),
    FieldSchema("rrg2", float, 40, 10, None),
    FieldSchema("rre", float, 50, 10, None),
    FieldSchema("rrg", float, 60, 10, None),
    FieldSchema("rrz", float, 70, 10, None),
)

_EOS043_CARD3 = (
    FieldSchema("figmax", float, 0, 10, None),
    FieldSchema("fg1max", float, 10, 10, None),
    FieldSchema("fg2min", float, 20, 10, None),
    FieldSchema("vumax", float, 30, 10, None),
)

_EOS043_CARD4 = (
    FieldSchema("eosunre", int, 0, 10, None),
    FieldSchema("shkgam", float, 10, 10, None),
    FieldSchema("shkc1", float, 20, 10, None),
    FieldSchema("shks1", float, 30, 10, None),
    FieldSchema("shkc2", float, 40, 10, None),
    FieldSchema("shks2", float, 50, 10, None),
    FieldSchema("shkve", float, 60, 10, None),
    FieldSchema("shkvb", float, 70, 10, None),
)

_EOS043_CARD5 = (
    FieldSchema("aur", float, 0, 10, None),
    FieldSchema("bur", float, 10, 10, None),
    FieldSchema("r1u", float, 20, 10, None),
    FieldSchema("r2u", float, 30, 10, None),
    FieldSchema("wu", float, 40, 10, None),
    FieldSchema("vvns", float, 50, 10, None),
    FieldSchema("eziu", float, 60, 10, None),
)

_EOS043_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Eos043(KeywordBase):
    """DYNA EOS_043 keyword"""

    keyword = "EOS"
    subkeyword = "043"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Eos043 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EOS043_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EOS043_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EOS043_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EOS043_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EOS043_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EOS043_CARD5,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = Eos043._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _EOS043_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state ID, a unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Product JWL constant A (Units of pressure)
        """ # nopep8
        return self._cards[0].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[0].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Product JWL constant B (Units of pressure)
        """ # nopep8
        return self._cards[0].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[0].set_value("b", value)

    @property
    def r1(self) -> typing.Optional[float]:
        """Get or set the Product JWL constant R1 (Unitless)
        """ # nopep8
        return self._cards[0].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        """Set the r1 property."""
        self._cards[0].set_value("r1", value)

    @property
    def r2(self) -> typing.Optional[float]:
        """Get or set the Product JWL constant R2 (Unitless)
        """ # nopep8
        return self._cards[0].get_value("r2")

    @r2.setter
    def r2(self, value: float) -> None:
        """Set the r2 property."""
        self._cards[0].set_value("r2", value)

    @property
    def w(self) -> typing.Optional[float]:
        """Get or set the Product JWL constant ? (Unitless)
        """ # nopep8
        return self._cards[0].get_value("w")

    @w.setter
    def w(self, value: float) -> None:
        """Set the w property."""
        self._cards[0].set_value("w", value)

    @property
    def detv(self) -> typing.Optional[float]:
        """Get or set the CJ Detonation velocity uD (Units of speed)
        """ # nopep8
        return self._cards[0].get_value("detv")

    @detv.setter
    def detv(self, value: float) -> None:
        """Set the detv property."""
        self._cards[0].set_value("detv", value)

    @property
    def ezig(self) -> typing.Optional[float]:
        """Get or set the CJ Energy/unit volume E0,g (Units of pressure)
        """ # nopep8
        return self._cards[0].get_value("ezig")

    @ezig.setter
    def ezig(self, value: float) -> None:
        """Set the ezig property."""
        self._cards[0].set_value("ezig", value)

    @property
    def pcj(self) -> typing.Optional[float]:
        """Get or set the CJ Pressure (Unitless of pressure)
        """ # nopep8
        return self._cards[1].get_value("pcj")

    @pcj.setter
    def pcj(self, value: float) -> None:
        """Set the pcj property."""
        self._cards[1].set_value("pcj", value)

    @property
    def wreac(self) -> typing.Optional[float]:
        """Get or set the Reaction zone width (Unitless)
        """ # nopep8
        return self._cards[1].get_value("wreac")

    @wreac.setter
    def wreac(self, value: float) -> None:
        """Set the wreac property."""
        self._cards[1].set_value("wreac", value)

    @property
    def dfmax(self) -> typing.Optional[float]:
        """Get or set the Maximum change in reaction ratio (Unitless)
        """ # nopep8
        return self._cards[1].get_value("dfmax")

    @dfmax.setter
    def dfmax(self, value: float) -> None:
        """Set the dfmax property."""
        self._cards[1].set_value("dfmax", value)

    @property
    def rri(self) -> typing.Optional[float]:
        """Get or set the Reaction rate parameter I (Units of time)
        """ # nopep8
        return self._cards[1].get_value("rri")

    @rri.setter
    def rri(self, value: float) -> None:
        """Set the rri property."""
        self._cards[1].set_value("rri", value)

    @property
    def rrb(self) -> typing.Optional[float]:
        """Get or set the Reaction rate parameter b (Unitless).
        """ # nopep8
        return self._cards[1].get_value("rrb")

    @rrb.setter
    def rrb(self, value: float) -> None:
        """Set the rrb property."""
        self._cards[1].set_value("rrb", value)

    @property
    def rra(self) -> typing.Optional[float]:
        """Get or set the Reaction rate parameter a (Unitless)
        """ # nopep8
        return self._cards[1].get_value("rra")

    @rra.setter
    def rra(self, value: float) -> None:
        """Set the rra property."""
        self._cards[1].set_value("rra", value)

    @property
    def rrx(self) -> typing.Optional[float]:
        """Get or set the Reaction rate parameter x (Unitless)
        """ # nopep8
        return self._cards[1].get_value("rrx")

    @rrx.setter
    def rrx(self, value: float) -> None:
        """Set the rrx property."""
        self._cards[1].set_value("rrx", value)

    @property
    def rrg1(self) -> typing.Optional[float]:
        """Get or set the Reaction rate parameter G1 (Unitless)
        """ # nopep8
        return self._cards[2].get_value("rrg1")

    @rrg1.setter
    def rrg1(self, value: float) -> None:
        """Set the rrg1 property."""
        self._cards[2].set_value("rrg1", value)

    @property
    def rrc(self) -> typing.Optional[float]:
        """Get or set the Reaction rate parameter c (Unitless)
        """ # nopep8
        return self._cards[2].get_value("rrc")

    @rrc.setter
    def rrc(self, value: float) -> None:
        """Set the rrc property."""
        self._cards[2].set_value("rrc", value)

    @property
    def rrd(self) -> typing.Optional[float]:
        """Get or set the Reaction rate parameter d (Unitless)
        """ # nopep8
        return self._cards[2].get_value("rrd")

    @rrd.setter
    def rrd(self, value: float) -> None:
        """Set the rrd property."""
        self._cards[2].set_value("rrd", value)

    @property
    def rry(self) -> typing.Optional[float]:
        """Get or set the Reaction rate parameter y (Unitless)
        """ # nopep8
        return self._cards[2].get_value("rry")

    @rry.setter
    def rry(self, value: float) -> None:
        """Set the rry property."""
        self._cards[2].set_value("rry", value)

    @property
    def rrg2(self) -> typing.Optional[float]:
        """Get or set the Reaction rate parameter G2 (Unitless).
        """ # nopep8
        return self._cards[2].get_value("rrg2")

    @rrg2.setter
    def rrg2(self, value: float) -> None:
        """Set the rrg2 property."""
        self._cards[2].set_value("rrg2", value)

    @property
    def rre(self) -> typing.Optional[float]:
        """Get or set the Reaction rate parameter e (Unitless)
        """ # nopep8
        return self._cards[2].get_value("rre")

    @rre.setter
    def rre(self, value: float) -> None:
        """Set the rre property."""
        self._cards[2].set_value("rre", value)

    @property
    def rrg(self) -> typing.Optional[float]:
        """Get or set the Reaction rate parameter g (Unitless)
        """ # nopep8
        return self._cards[2].get_value("rrg")

    @rrg.setter
    def rrg(self, value: float) -> None:
        """Set the rrg property."""
        self._cards[2].set_value("rrg", value)

    @property
    def rrz(self) -> typing.Optional[float]:
        """Get or set the Reaction rate parameter z (Unitless)
        """ # nopep8
        return self._cards[2].get_value("rrz")

    @rrz.setter
    def rrz(self, value: float) -> None:
        """Set the rrz property."""
        self._cards[2].set_value("rrz", value)

    @property
    def figmax(self) -> typing.Optional[float]:
        """Get or set the Maximum reaction ratio of ignition (Unitless)
        """ # nopep8
        return self._cards[3].get_value("figmax")

    @figmax.setter
    def figmax(self, value: float) -> None:
        """Set the figmax property."""
        self._cards[3].set_value("figmax", value)

    @property
    def fg1max(self) -> typing.Optional[float]:
        """Get or set the Maximum reaction ratio of G1 (Unitless)
        """ # nopep8
        return self._cards[3].get_value("fg1max")

    @fg1max.setter
    def fg1max(self, value: float) -> None:
        """Set the fg1max property."""
        self._cards[3].set_value("fg1max", value)

    @property
    def fg2min(self) -> typing.Optional[float]:
        """Get or set the Minimum reaction ratio of G2 (Unitless)
        """ # nopep8
        return self._cards[3].get_value("fg2min")

    @fg2min.setter
    def fg2min(self, value: float) -> None:
        """Set the fg2min property."""
        self._cards[3].set_value("fg2min", value)

    @property
    def vumax(self) -> typing.Optional[float]:
        """Get or set the Maximum relative volume in tension (Unitless)
        """ # nopep8
        return self._cards[3].get_value("vumax")

    @vumax.setter
    def vumax(self, value: float) -> None:
        """Set the vumax property."""
        self._cards[3].set_value("vumax", value)

    @property
    def eosunre(self) -> typing.Optional[int]:
        """Get or set the Flag to indicate which unreacted EOS is used for the unreacted explosive.  (1 = Shock; 2=JWL).
        """ # nopep8
        return self._cards[4].get_value("eosunre")

    @eosunre.setter
    def eosunre(self, value: int) -> None:
        """Set the eosunre property."""
        self._cards[4].set_value("eosunre", value)

    @property
    def shkgam(self) -> typing.Optional[float]:
        """Get or set the Unreacted EOS Shock Gr�neisen gamma (Unitless).
        """ # nopep8
        return self._cards[4].get_value("shkgam")

    @shkgam.setter
    def shkgam(self, value: float) -> None:
        """Set the shkgam property."""
        self._cards[4].set_value("shkgam", value)

    @property
    def shkc1(self) -> typing.Optional[float]:
        """Get or set the Unreacted EOS Shock s parameter (Unitless)
        """ # nopep8
        return self._cards[4].get_value("shkc1")

    @shkc1.setter
    def shkc1(self, value: float) -> None:
        """Set the shkc1 property."""
        self._cards[4].set_value("shkc1", value)

    @property
    def shks1(self) -> typing.Optional[float]:
        """Get or set the Unreacted EOS Shock s parameter (Unitless)
        """ # nopep8
        return self._cards[4].get_value("shks1")

    @shks1.setter
    def shks1(self, value: float) -> None:
        """Set the shks1 property."""
        self._cards[4].set_value("shks1", value)

    @property
    def shkc2(self) -> typing.Optional[float]:
        """Get or set the Unreacted EOS Shock c, for bilinear shock (Units of velocity)
        """ # nopep8
        return self._cards[4].get_value("shkc2")

    @shkc2.setter
    def shkc2(self, value: float) -> None:
        """Set the shkc2 property."""
        self._cards[4].set_value("shkc2", value)

    @property
    def shks2(self) -> typing.Optional[float]:
        """Get or set the Unreacted EOS Shock s, for bilinear shock (Unitless)
        """ # nopep8
        return self._cards[4].get_value("shks2")

    @shks2.setter
    def shks2(self, value: float) -> None:
        """Set the shks2 property."""
        self._cards[4].set_value("shks2", value)

    @property
    def shkve(self) -> typing.Optional[float]:
        """Get or set the Relative volume above which the C1 and S1 parameter used for unreacted Shock EOS (Unitless)
        """ # nopep8
        return self._cards[4].get_value("shkve")

    @shkve.setter
    def shkve(self, value: float) -> None:
        """Set the shkve property."""
        self._cards[4].set_value("shkve", value)

    @property
    def shkvb(self) -> typing.Optional[float]:
        """Get or set the Relative volume above which the C2 and S2 parameter used for unreacted Shock EOS (Unitless).
        """ # nopep8
        return self._cards[4].get_value("shkvb")

    @shkvb.setter
    def shkvb(self, value: float) -> None:
        """Set the shkvb property."""
        self._cards[4].set_value("shkvb", value)

    @property
    def aur(self) -> typing.Optional[float]:
        """Get or set the Unreacted EOS JWL A (Units of pressure).
        """ # nopep8
        return self._cards[5].get_value("aur")

    @aur.setter
    def aur(self, value: float) -> None:
        """Set the aur property."""
        self._cards[5].set_value("aur", value)

    @property
    def bur(self) -> typing.Optional[float]:
        """Get or set the Unreacted EOS JWL B (Units of pressure)e.
        """ # nopep8
        return self._cards[5].get_value("bur")

    @bur.setter
    def bur(self, value: float) -> None:
        """Set the bur property."""
        self._cards[5].set_value("bur", value)

    @property
    def r1u(self) -> typing.Optional[float]:
        """Get or set the Unreacted EOS JWL R1(Unitless)
        """ # nopep8
        return self._cards[5].get_value("r1u")

    @r1u.setter
    def r1u(self, value: float) -> None:
        """Set the r1u property."""
        self._cards[5].set_value("r1u", value)

    @property
    def r2u(self) -> typing.Optional[float]:
        """Get or set the Unreacted EOS JWL R2(Unitless)
        """ # nopep8
        return self._cards[5].get_value("r2u")

    @r2u.setter
    def r2u(self, value: float) -> None:
        """Set the r2u property."""
        self._cards[5].set_value("r2u", value)

    @property
    def wu(self) -> typing.Optional[float]:
        """Get or set the Unreacted EOS JWL W(Unitless)
        """ # nopep8
        return self._cards[5].get_value("wu")

    @wu.setter
    def wu(self, value: float) -> None:
        """Set the wu property."""
        self._cards[5].set_value("wu", value)

    @property
    def vvns(self) -> typing.Optional[float]:
        """Get or set the Von Neuman spike relative volume (Unitless)
        """ # nopep8
        return self._cards[5].get_value("vvns")

    @vvns.setter
    def vvns(self, value: float) -> None:
        """Set the vvns property."""
        self._cards[5].set_value("vvns", value)

    @property
    def eziu(self) -> typing.Optional[float]:
        """Get or set the CJ Energy / unit volume (Units of pressure)
        """ # nopep8
        return self._cards[5].get_value("eziu")

    @eziu.setter
    def eziu(self, value: float) -> None:
        """Set the eziu property."""
        self._cards[5].set_value("eziu", value)

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

