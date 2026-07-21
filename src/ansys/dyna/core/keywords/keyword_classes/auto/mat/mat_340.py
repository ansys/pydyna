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

"""Module providing the Mat340 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MAT340_CARD0 = (
    FieldSchema("matid", int, 0, 10, None),
    FieldSchema("eosur", int, 10, 10, 0),
    FieldSchema("ro", float, 20, 10, None),
)

_MAT340_CARD1 = (
    FieldSchema("a", float, 0, 10, None),
    FieldSchema("b", float, 10, 10, None),
    FieldSchema("r1", float, 20, 10, None),
    FieldSchema("r2", float, 30, 10, None),
    FieldSchema("omega", float, 40, 10, None),
    FieldSchema("dcj", float, 50, 10, None),
    FieldSchema("ecj", float, 60, 10, None),
    FieldSchema("pcj", float, 70, 10, None),
)

_MAT340_CARD2 = (
    FieldSchema("raw", float, 0, 10, None),
    FieldSchema("rrmax", float, 10, 10, None),
    FieldSchema("i", float, 20, 10, None),
    FieldSchema("irrexp", float, 30, 10, None),
    FieldSchema("icomp", float, 40, 10, None),
    FieldSchema("icompexp", float, 50, 10, None),
)

_MAT340_CARD3 = (
    FieldSchema("g1", float, 0, 10, None),
    FieldSchema("c", float, 10, 10, None),
    FieldSchema("d", float, 20, 10, None),
    FieldSchema("y", float, 30, 10, None),
    FieldSchema("g2", float, 40, 10, None),
    FieldSchema("e", float, 50, 10, None),
    FieldSchema("g", float, 60, 10, None),
    FieldSchema("z", float, 70, 10, None),
)

_MAT340_CARD4 = (
    FieldSchema("maxrri", float, 0, 10, None),
    FieldSchema("maxrrg", float, 10, 10, None),
    FieldSchema("minrrc", float, 20, 10, None),
    FieldSchema("maxrvol", float, 30, 10, None),
)

_MAT340_CARD5 = (
    FieldSchema("gammau", float, 0, 10, None),
    FieldSchema("q2u", float, 10, 10, None),
    FieldSchema("trefu", float, 20, 10, None),
    FieldSchema("cpu", float, 30, 10, None),
    FieldSchema("tcu", float, 40, 10, None),
)

_MAT340_CARD6 = (
    FieldSchema("c1u", float, 0, 10, None),
    FieldSchema("s1u", float, 10, 10, None),
    FieldSchema("c2u", float, 20, 10, None),
    FieldSchema("s2u", float, 30, 10, None),
    FieldSchema("veu", float, 40, 10, None),
    FieldSchema("vbu", float, 50, 10, None),
)

_MAT340_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat340(KeywordBase):
    """DYNA MAT_340 keyword"""

    keyword = "MAT"
    subkeyword = "340"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the Mat340 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT340_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT340_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT340_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT340_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT340_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT340_CARD5,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MAT340_CARD6,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = Mat340._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT340_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def matid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number or label must be specified (see *PART).
        """ # nopep8
        return self._cards[0].get_value("matid")

    @matid.setter
    def matid(self, value: int) -> None:
        """Set the matid property."""
        self._cards[0].set_value("matid", value)

    @property
    def eosur(self) -> int:
        """Get or set the EOS of state formulation used for the unreacted explosive (see Remark 1):
        EQ.0: Shock
        EQ.1 : JWL
        """ # nopep8
        return self._cards[0].get_value("eosur")

    @eosur.setter
    def eosur(self, value: int) -> None:
        """Set the eosur property."""
        if value not in [0, 1, None]:
            raise Exception("""eosur must be `None` or one of {0,1}.""")
        self._cards[0].set_value("eosur", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Product JWL parameter A. See Remark 1
        """ # nopep8
        return self._cards[1].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[1].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Product JWL parameter B. See Remark 1
        """ # nopep8
        return self._cards[1].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[1].set_value("b", value)

    @property
    def r1(self) -> typing.Optional[float]:
        """Get or set the Product JWL parameter R_1. See Remark 1
        """ # nopep8
        return self._cards[1].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        """Set the r1 property."""
        self._cards[1].set_value("r1", value)

    @property
    def r2(self) -> typing.Optional[float]:
        """Get or set the Product JWL parameter R_2. See Remark 1.
        """ # nopep8
        return self._cards[1].get_value("r2")

    @r2.setter
    def r2(self, value: float) -> None:
        """Set the r2 property."""
        self._cards[1].set_value("r2", value)

    @property
    def omega(self) -> typing.Optional[float]:
        """Get or set the Product JWL parameter ω. See Remark 1.
        """ # nopep8
        return self._cards[1].get_value("omega")

    @omega.setter
    def omega(self, value: float) -> None:
        """Set the omega property."""
        self._cards[1].set_value("omega", value)

    @property
    def dcj(self) -> typing.Optional[float]:
        """Get or set the Chapman-Jouget detonation velocity for the product, D_CJ
        """ # nopep8
        return self._cards[1].get_value("dcj")

    @dcj.setter
    def dcj(self, value: float) -> None:
        """Set the dcj property."""
        self._cards[1].set_value("dcj", value)

    @property
    def ecj(self) -> typing.Optional[float]:
        """Get or set the Chapman-Jouget energy per unit mass for the product
        """ # nopep8
        return self._cards[1].get_value("ecj")

    @ecj.setter
    def ecj(self, value: float) -> None:
        """Set the ecj property."""
        self._cards[1].set_value("ecj", value)

    @property
    def pcj(self) -> typing.Optional[float]:
        """Get or set the Chapman-Jouget pressure, p_CJ , for the product
        """ # nopep8
        return self._cards[1].get_value("pcj")

    @pcj.setter
    def pcj(self, value: float) -> None:
        """Set the pcj property."""
        self._cards[1].set_value("pcj", value)

    @property
    def raw(self) -> typing.Optional[float]:
        """Get or set the Reaction zone width
        """ # nopep8
        return self._cards[2].get_value("raw")

    @raw.setter
    def raw(self, value: float) -> None:
        """Set the raw property."""
        self._cards[2].set_value("raw", value)

    @property
    def rrmax(self) -> typing.Optional[float]:
        """Get or set the Maximum change in the reaction ratio
        """ # nopep8
        return self._cards[2].get_value("rrmax")

    @rrmax.setter
    def rrmax(self, value: float) -> None:
        """Set the rrmax property."""
        self._cards[2].set_value("rrmax", value)

    @property
    def i(self) -> typing.Optional[float]:
        """Get or set the Ignition parameter, I
        """ # nopep8
        return self._cards[2].get_value("i")

    @i.setter
    def i(self, value: float) -> None:
        """Set the i property."""
        self._cards[2].set_value("i", value)

    @property
    def irrexp(self) -> typing.Optional[float]:
        """Get or set the Ignition reaction ratio exponent
        """ # nopep8
        return self._cards[2].get_value("irrexp")

    @irrexp.setter
    def irrexp(self, value: float) -> None:
        """Set the irrexp property."""
        self._cards[2].set_value("irrexp", value)

    @property
    def icomp(self) -> typing.Optional[float]:
        """Get or set the Ignition critical compression
        """ # nopep8
        return self._cards[2].get_value("icomp")

    @icomp.setter
    def icomp(self, value: float) -> None:
        """Set the icomp property."""
        self._cards[2].set_value("icomp", value)

    @property
    def icompexp(self) -> typing.Optional[float]:
        """Get or set the Ignition compression exponent
        """ # nopep8
        return self._cards[2].get_value("icompexp")

    @icompexp.setter
    def icompexp(self, value: float) -> None:
        """Set the icompexp property."""
        self._cards[2].set_value("icompexp", value)

    @property
    def g1(self) -> typing.Optional[float]:
        """Get or set the Growth parameter G_1
        """ # nopep8
        return self._cards[3].get_value("g1")

    @g1.setter
    def g1(self, value: float) -> None:
        """Set the g1 property."""
        self._cards[3].set_value("g1", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Growth reaction ratio exponent c
        """ # nopep8
        return self._cards[3].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[3].set_value("c", value)

    @property
    def d(self) -> typing.Optional[float]:
        """Get or set the Growth reaction ratio exponent d
        """ # nopep8
        return self._cards[3].get_value("d")

    @d.setter
    def d(self, value: float) -> None:
        """Set the d property."""
        self._cards[3].set_value("d", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the Growth pressure exponent y
        """ # nopep8
        return self._cards[3].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[3].set_value("y", value)

    @property
    def g2(self) -> typing.Optional[float]:
        """Get or set the Completion parameter G_2
        """ # nopep8
        return self._cards[3].get_value("g2")

    @g2.setter
    def g2(self, value: float) -> None:
        """Set the g2 property."""
        self._cards[3].set_value("g2", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Completion reaction ratio exponent e
        """ # nopep8
        return self._cards[3].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[3].set_value("e", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Completion reaction ratio exponent, g
        """ # nopep8
        return self._cards[3].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[3].set_value("g", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the Completion pressure exponent, z
        """ # nopep8
        return self._cards[3].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        """Set the z property."""
        self._cards[3].set_value("z", value)

    @property
    def maxrri(self) -> typing.Optional[float]:
        """Get or set the Maximum reaction ratio for ignition
        """ # nopep8
        return self._cards[4].get_value("maxrri")

    @maxrri.setter
    def maxrri(self, value: float) -> None:
        """Set the maxrri property."""
        self._cards[4].set_value("maxrri", value)

    @property
    def maxrrg(self) -> typing.Optional[float]:
        """Get or set the Maximum reaction ratio for growth
        """ # nopep8
        return self._cards[4].get_value("maxrrg")

    @maxrrg.setter
    def maxrrg(self, value: float) -> None:
        """Set the maxrrg property."""
        self._cards[4].set_value("maxrrg", value)

    @property
    def minrrc(self) -> typing.Optional[float]:
        """Get or set the Minimum reaction ratio for completion
        """ # nopep8
        return self._cards[4].get_value("minrrc")

    @minrrc.setter
    def minrrc(self, value: float) -> None:
        """Set the minrrc property."""
        self._cards[4].set_value("minrrc", value)

    @property
    def maxrvol(self) -> typing.Optional[float]:
        """Get or set the Maximum relative volume in tension
        """ # nopep8
        return self._cards[4].get_value("maxrvol")

    @maxrvol.setter
    def maxrvol(self, value: float) -> None:
        """Set the maxrvol property."""
        self._cards[4].set_value("maxrvol", value)

    @property
    def gammau(self) -> typing.Optional[float]:
        """Get or set the Gruneisen coefficient, Γ_0
        """ # nopep8
        return self._cards[5].get_value("gammau")

    @gammau.setter
    def gammau(self, value: float) -> None:
        """Set the gammau property."""
        self._cards[5].set_value("gammau", value)

    @property
    def q2u(self) -> typing.Optional[float]:
        """Get or set the Constant for the quadratic curve
        """ # nopep8
        return self._cards[5].get_value("q2u")

    @q2u.setter
    def q2u(self, value: float) -> None:
        """Set the q2u property."""
        self._cards[5].set_value("q2u", value)

    @property
    def trefu(self) -> typing.Optional[float]:
        """Get or set the Reference temperature
        """ # nopep8
        return self._cards[5].get_value("trefu")

    @trefu.setter
    def trefu(self, value: float) -> None:
        """Set the trefu property."""
        self._cards[5].set_value("trefu", value)

    @property
    def cpu(self) -> typing.Optional[float]:
        """Get or set the Specific heat capacity
        """ # nopep8
        return self._cards[5].get_value("cpu")

    @cpu.setter
    def cpu(self, value: float) -> None:
        """Set the cpu property."""
        self._cards[5].set_value("cpu", value)

    @property
    def tcu(self) -> typing.Optional[float]:
        """Get or set the Thermal conductivity
        """ # nopep8
        return self._cards[5].get_value("tcu")

    @tcu.setter
    def tcu(self, value: float) -> None:
        """Set the tcu property."""
        self._cards[5].set_value("tcu", value)

    @property
    def c1u(self) -> typing.Optional[float]:
        """Get or set the Constant, c_1
        """ # nopep8
        return self._cards[6].get_value("c1u")

    @c1u.setter
    def c1u(self, value: float) -> None:
        """Set the c1u property."""
        self._cards[6].set_value("c1u", value)

    @property
    def s1u(self) -> typing.Optional[float]:
        """Get or set the Constant, s_1
        """ # nopep8
        return self._cards[6].get_value("s1u")

    @s1u.setter
    def s1u(self, value: float) -> None:
        """Set the s1u property."""
        self._cards[6].set_value("s1u", value)

    @property
    def c2u(self) -> typing.Optional[float]:
        """Get or set the Constant, c_2
        """ # nopep8
        return self._cards[6].get_value("c2u")

    @c2u.setter
    def c2u(self, value: float) -> None:
        """Set the c2u property."""
        self._cards[6].set_value("c2u", value)

    @property
    def s2u(self) -> typing.Optional[float]:
        """Get or set the Constant, s_2
        """ # nopep8
        return self._cards[6].get_value("s2u")

    @s2u.setter
    def s2u(self, value: float) -> None:
        """Set the s2u property."""
        self._cards[6].set_value("s2u", value)

    @property
    def veu(self) -> typing.Optional[float]:
        """Get or set the Relative volume, V_E/V_0
        """ # nopep8
        return self._cards[6].get_value("veu")

    @veu.setter
    def veu(self, value: float) -> None:
        """Set the veu property."""
        self._cards[6].set_value("veu", value)

    @property
    def vbu(self) -> typing.Optional[float]:
        """Get or set the Relative volume, V_B/V_0
        """ # nopep8
        return self._cards[6].get_value("vbu")

    @vbu.setter
    def vbu(self, value: float) -> None:
        """Set the vbu property."""
        self._cards[6].set_value("vbu", value)

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

