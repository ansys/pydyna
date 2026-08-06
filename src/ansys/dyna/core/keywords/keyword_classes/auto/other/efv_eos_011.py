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

"""Module providing the EfvEos011 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVEOS011_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("eosur", int, 10, 10, None),
    FieldSchema("eosr", int, 20, 10, 1),
    FieldSchema("g", float, 30, 10, None),
    FieldSchema("c", float, 30, 10, None),
)

_EFVEOS011_CARD1 = (
    FieldSchema("a", float, 0, 10, None),
    FieldSchema("b", float, 10, 10, None),
    FieldSchema("r1", float, 20, 10, None),
    FieldSchema("r2", float, 30, 10, None),
    FieldSchema("omega", float, 40, 10, None),
    FieldSchema("dcj", float, 50, 10, None),
    FieldSchema("ecj", float, 60, 10, None),
)

_EFVEOS011_CARD2 = (
    FieldSchema("c1", float, 0, 10, None),
    FieldSchema("c2", float, 10, 10, None),
    FieldSchema("d", float, 20, 10, None),
    FieldSchema("e", float, 30, 10, None),
)

_EFVEOS011_CARD3 = (
    FieldSchema("pg1", float, 0, 10, None),
    FieldSchema("pg2", float, 10, 10, None),
    FieldSchema("pg3", float, 20, 10, None),
    FieldSchema("pg4", float, 30, 10, None),
    FieldSchema("pg5", float, 40, 10, None),
    FieldSchema("pg6", float, 50, 10, None),
    FieldSchema("pg7", float, 60, 10, None),
    FieldSchema("pg8", float, 70, 10, None),
)

_EFVEOS011_CARD4 = (
    FieldSchema("pg9", float, 0, 10, None),
    FieldSchema("pg10", float, 10, 10, None),
)

_EFVEOS011_CARD5 = (
    FieldSchema("h1", float, 0, 10, None),
    FieldSchema("h2", float, 10, 10, None),
    FieldSchema("h3", float, 20, 10, None),
    FieldSchema("h4", float, 30, 10, None),
    FieldSchema("h5", float, 40, 10, None),
    FieldSchema("h6", float, 50, 10, None),
    FieldSchema("h7", float, 60, 10, None),
    FieldSchema("h8", float, 70, 10, None),
)

_EFVEOS011_CARD6 = (
    FieldSchema("h9", float, 0, 10, None),
    FieldSchema("h10", float, 10, 10, None),
)

_EFVEOS011_CARD7 = (
    FieldSchema("rhos1", float, 0, 10, None),
    FieldSchema("rhos2", float, 10, 10, None),
    FieldSchema("rhos3", float, 20, 10, None),
    FieldSchema("rhos4", float, 30, 10, None),
    FieldSchema("rhos5", float, 40, 10, None),
    FieldSchema("rhos6", float, 50, 10, None),
    FieldSchema("rhos7", float, 60, 10, None),
    FieldSchema("rhos8", float, 70, 10, None),
)

_EFVEOS011_CARD8 = (
    FieldSchema("rhos9", float, 0, 10, None),
    FieldSchema("rhos10", float, 10, 10, None),
)

_EFVEOS011_CARD9 = (
    FieldSchema("gamma1", float, 0, 10, None),
    FieldSchema("gamma2", float, 10, 10, None),
    FieldSchema("gamma3", float, 20, 10, None),
    FieldSchema("gamma4", float, 30, 10, None),
    FieldSchema("gamma5", float, 40, 10, None),
    FieldSchema("gamma6", float, 50, 10, None),
    FieldSchema("gamma7", float, 60, 10, None),
    FieldSchema("gamma8", float, 70, 10, None),
)

_EFVEOS011_CARD10 = (
    FieldSchema("gamma9", float, 0, 10, None),
    FieldSchema("gamma10", float, 10, 10, None),
)

class EfvEos011(KeywordBase):
    """DYNA EFV_EOS_011 keyword"""

    keyword = "EFV"
    subkeyword = "EOS_011"

    def __init__(self, **kwargs):
        """Initialize the EfvEos011 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVEOS011_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS011_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS011_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS011_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS011_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS011_CARD5,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS011_CARD6,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS011_CARD7,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS011_CARD8,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS011_CARD9,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS011_CARD10,
                **kwargs,
            ),
        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state identification. A unique number or label must be used.(see Remark Error! Reference source not found. in *EFV_MAT).
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def eosur(self) -> typing.Optional[int]:
        """Get or set the EOS of state identification that calls either *EFV_EOS_LINEAR or *EFV_EOS_COMPACTION for the unreacted (solid) material
        """ # nopep8
        return self._cards[0].get_value("eosur")

    @eosur.setter
    def eosur(self, value: int) -> None:
        """Set the eosur property."""
        self._cards[0].set_value("eosur", value)

    @property
    def eosr(self) -> int:
        """Get or set the EOS of state formulation used for the reacted (gaseous) material:
        EQ.1: JWL
        EQ.2: Exponential
        """ # nopep8
        return self._cards[0].get_value("eosr")

    @eosr.setter
    def eosr(self, value: int) -> None:
        """Set the eosr property."""
        if value not in [1, 2, None]:
            raise Exception("""eosr must be `None` or one of {1,2}.""")
        self._cards[0].set_value("eosr", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Growth parameter, G
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[0].set_value("g", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Growth reaction ratio exponent, c
        """ # nopep8
        return self._cards[0].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[0].set_value("c", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the JWL parameter A
        """ # nopep8
        return self._cards[1].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[1].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the JWL parameter B
        """ # nopep8
        return self._cards[1].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[1].set_value("b", value)

    @property
    def r1(self) -> typing.Optional[float]:
        """Get or set the JWL parameter R_1
        """ # nopep8
        return self._cards[1].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        """Set the r1 property."""
        self._cards[1].set_value("r1", value)

    @property
    def r2(self) -> typing.Optional[float]:
        """Get or set the JWL parameter R_2
        """ # nopep8
        return self._cards[1].get_value("r2")

    @r2.setter
    def r2(self, value: float) -> None:
        """Set the r2 property."""
        self._cards[1].set_value("r2", value)

    @property
    def omega(self) -> typing.Optional[float]:
        """Get or set the JWL parameter w
        """ # nopep8
        return self._cards[1].get_value("omega")

    @omega.setter
    def omega(self, value: float) -> None:
        """Set the omega property."""
        self._cards[1].set_value("omega", value)

    @property
    def dcj(self) -> typing.Optional[float]:
        """Get or set the Chapman-Jouget detonation velocity, D_CJ
        """ # nopep8
        return self._cards[1].get_value("dcj")

    @dcj.setter
    def dcj(self, value: float) -> None:
        """Set the dcj property."""
        self._cards[1].set_value("dcj", value)

    @property
    def ecj(self) -> typing.Optional[float]:
        """Get or set the Chapman-Jouget energy per unit volume
        """ # nopep8
        return self._cards[1].get_value("ecj")

    @ecj.setter
    def ecj(self, value: float) -> None:
        """Set the ecj property."""
        self._cards[1].set_value("ecj", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Constant C_1 used to determine burn velocity for the exponential reacted EOS
        """ # nopep8
        return self._cards[2].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        """Set the c1 property."""
        self._cards[2].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the Constant C_2 used to determine burn velocity for the exponential reacted EOS
        """ # nopep8
        return self._cards[2].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        """Set the c2 property."""
        self._cards[2].set_value("c2", value)

    @property
    def d(self) -> typing.Optional[float]:
        """Get or set the Constant D in the exponential reacted EOS
        """ # nopep8
        return self._cards[2].get_value("d")

    @d.setter
    def d(self, value: float) -> None:
        """Set the d property."""
        self._cards[2].set_value("d", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Energy per unit volume
        """ # nopep8
        return self._cards[2].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[2].set_value("e", value)

    @property
    def pg1(self) -> typing.Optional[float]:
        """Get or set the Gas pressure, P_g, values giving the burning velocity as a function of gas pressure curve
        """ # nopep8
        return self._cards[3].get_value("pg1")

    @pg1.setter
    def pg1(self, value: float) -> None:
        """Set the pg1 property."""
        self._cards[3].set_value("pg1", value)

    @property
    def pg2(self) -> typing.Optional[float]:
        """Get or set the Gas pressure, P_g, values giving the burning velocity as a function of gas pressure curve
        """ # nopep8
        return self._cards[3].get_value("pg2")

    @pg2.setter
    def pg2(self, value: float) -> None:
        """Set the pg2 property."""
        self._cards[3].set_value("pg2", value)

    @property
    def pg3(self) -> typing.Optional[float]:
        """Get or set the Gas pressure, P_g, values giving the burning velocity as a function of gas pressure curve
        """ # nopep8
        return self._cards[3].get_value("pg3")

    @pg3.setter
    def pg3(self, value: float) -> None:
        """Set the pg3 property."""
        self._cards[3].set_value("pg3", value)

    @property
    def pg4(self) -> typing.Optional[float]:
        """Get or set the Gas pressure, P_g, values giving the burning velocity as a function of gas pressure curve
        """ # nopep8
        return self._cards[3].get_value("pg4")

    @pg4.setter
    def pg4(self, value: float) -> None:
        """Set the pg4 property."""
        self._cards[3].set_value("pg4", value)

    @property
    def pg5(self) -> typing.Optional[float]:
        """Get or set the Gas pressure, P_g, values giving the burning velocity as a function of gas pressure curve
        """ # nopep8
        return self._cards[3].get_value("pg5")

    @pg5.setter
    def pg5(self, value: float) -> None:
        """Set the pg5 property."""
        self._cards[3].set_value("pg5", value)

    @property
    def pg6(self) -> typing.Optional[float]:
        """Get or set the Gas pressure, P_g, values giving the burning velocity as a function of gas pressure curve
        """ # nopep8
        return self._cards[3].get_value("pg6")

    @pg6.setter
    def pg6(self, value: float) -> None:
        """Set the pg6 property."""
        self._cards[3].set_value("pg6", value)

    @property
    def pg7(self) -> typing.Optional[float]:
        """Get or set the Gas pressure, P_g, values giving the burning velocity as a function of gas pressure curve
        """ # nopep8
        return self._cards[3].get_value("pg7")

    @pg7.setter
    def pg7(self, value: float) -> None:
        """Set the pg7 property."""
        self._cards[3].set_value("pg7", value)

    @property
    def pg8(self) -> typing.Optional[float]:
        """Get or set the Gas pressure, P_g, values giving the burning velocity as a function of gas pressure curve
        """ # nopep8
        return self._cards[3].get_value("pg8")

    @pg8.setter
    def pg8(self, value: float) -> None:
        """Set the pg8 property."""
        self._cards[3].set_value("pg8", value)

    @property
    def pg9(self) -> typing.Optional[float]:
        """Get or set the Gas pressure, P_g, values giving the burning velocity as a function of gas pressure curve
        """ # nopep8
        return self._cards[4].get_value("pg9")

    @pg9.setter
    def pg9(self, value: float) -> None:
        """Set the pg9 property."""
        self._cards[4].set_value("pg9", value)

    @property
    def pg10(self) -> typing.Optional[float]:
        """Get or set the Gas pressure, P_g, values giving the burning velocity as a function of gas pressure curve
        """ # nopep8
        return self._cards[4].get_value("pg10")

    @pg10.setter
    def pg10(self, value: float) -> None:
        """Set the pg10 property."""
        self._cards[4].set_value("pg10", value)

    @property
    def h1(self) -> typing.Optional[float]:
        """Get or set the Burning velocity, H(P_g ), values giving the burning velocity as a function of gas pressure curve
        """ # nopep8
        return self._cards[5].get_value("h1")

    @h1.setter
    def h1(self, value: float) -> None:
        """Set the h1 property."""
        self._cards[5].set_value("h1", value)

    @property
    def h2(self) -> typing.Optional[float]:
        """Get or set the Burning velocity, H(P_g ), values giving the burning velocity as a function of gas pressure curve
        """ # nopep8
        return self._cards[5].get_value("h2")

    @h2.setter
    def h2(self, value: float) -> None:
        """Set the h2 property."""
        self._cards[5].set_value("h2", value)

    @property
    def h3(self) -> typing.Optional[float]:
        """Get or set the Burning velocity, H(P_g ), values giving the burning velocity as a function of gas pressure curve
        """ # nopep8
        return self._cards[5].get_value("h3")

    @h3.setter
    def h3(self, value: float) -> None:
        """Set the h3 property."""
        self._cards[5].set_value("h3", value)

    @property
    def h4(self) -> typing.Optional[float]:
        """Get or set the Burning velocity, H(P_g ), values giving the burning velocity as a function of gas pressure curve
        """ # nopep8
        return self._cards[5].get_value("h4")

    @h4.setter
    def h4(self, value: float) -> None:
        """Set the h4 property."""
        self._cards[5].set_value("h4", value)

    @property
    def h5(self) -> typing.Optional[float]:
        """Get or set the Burning velocity, H(P_g ), values giving the burning velocity as a function of gas pressure curve
        """ # nopep8
        return self._cards[5].get_value("h5")

    @h5.setter
    def h5(self, value: float) -> None:
        """Set the h5 property."""
        self._cards[5].set_value("h5", value)

    @property
    def h6(self) -> typing.Optional[float]:
        """Get or set the Burning velocity, H(P_g ), values giving the burning velocity as a function of gas pressure curve
        """ # nopep8
        return self._cards[5].get_value("h6")

    @h6.setter
    def h6(self, value: float) -> None:
        """Set the h6 property."""
        self._cards[5].set_value("h6", value)

    @property
    def h7(self) -> typing.Optional[float]:
        """Get or set the Burning velocity, H(P_g ), values giving the burning velocity as a function of gas pressure curve
        """ # nopep8
        return self._cards[5].get_value("h7")

    @h7.setter
    def h7(self, value: float) -> None:
        """Set the h7 property."""
        self._cards[5].set_value("h7", value)

    @property
    def h8(self) -> typing.Optional[float]:
        """Get or set the Burning velocity, H(P_g ), values giving the burning velocity as a function of gas pressure curve
        """ # nopep8
        return self._cards[5].get_value("h8")

    @h8.setter
    def h8(self, value: float) -> None:
        """Set the h8 property."""
        self._cards[5].set_value("h8", value)

    @property
    def h9(self) -> typing.Optional[float]:
        """Get or set the Burning velocity, H(P_g ), values giving the burning velocity as a function of gas pressure curve
        """ # nopep8
        return self._cards[6].get_value("h9")

    @h9.setter
    def h9(self, value: float) -> None:
        """Set the h9 property."""
        self._cards[6].set_value("h9", value)

    @property
    def h10(self) -> typing.Optional[float]:
        """Get or set the Burning velocity, H(P_g ), values giving the burning velocity as a function of gas pressure curve
        """ # nopep8
        return self._cards[6].get_value("h10")

    @h10.setter
    def h10(self, value: float) -> None:
        """Set the h10 property."""
        self._cards[6].set_value("h10", value)

    @property
    def rhos1(self) -> typing.Optional[float]:
        """Get or set the Solid density, p_s, values giving the r as a function of p_s curve for determining the burn velocity
        """ # nopep8
        return self._cards[7].get_value("rhos1")

    @rhos1.setter
    def rhos1(self, value: float) -> None:
        """Set the rhos1 property."""
        self._cards[7].set_value("rhos1", value)

    @property
    def rhos2(self) -> typing.Optional[float]:
        """Get or set the Solid density, p_s, values giving the r as a function of p_s curve for determining the burn velocity
        """ # nopep8
        return self._cards[7].get_value("rhos2")

    @rhos2.setter
    def rhos2(self, value: float) -> None:
        """Set the rhos2 property."""
        self._cards[7].set_value("rhos2", value)

    @property
    def rhos3(self) -> typing.Optional[float]:
        """Get or set the Solid density, p_s, values giving the r as a function of p_s curve for determining the burn velocity
        """ # nopep8
        return self._cards[7].get_value("rhos3")

    @rhos3.setter
    def rhos3(self, value: float) -> None:
        """Set the rhos3 property."""
        self._cards[7].set_value("rhos3", value)

    @property
    def rhos4(self) -> typing.Optional[float]:
        """Get or set the Solid density, p_s, values giving the r as a function of p_s curve for determining the burn velocity
        """ # nopep8
        return self._cards[7].get_value("rhos4")

    @rhos4.setter
    def rhos4(self, value: float) -> None:
        """Set the rhos4 property."""
        self._cards[7].set_value("rhos4", value)

    @property
    def rhos5(self) -> typing.Optional[float]:
        """Get or set the Solid density, p_s, values giving the r as a function of p_s curve for determining the burn velocity
        """ # nopep8
        return self._cards[7].get_value("rhos5")

    @rhos5.setter
    def rhos5(self, value: float) -> None:
        """Set the rhos5 property."""
        self._cards[7].set_value("rhos5", value)

    @property
    def rhos6(self) -> typing.Optional[float]:
        """Get or set the Solid density, p_s, values giving the r as a function of p_s curve for determining the burn velocity
        """ # nopep8
        return self._cards[7].get_value("rhos6")

    @rhos6.setter
    def rhos6(self, value: float) -> None:
        """Set the rhos6 property."""
        self._cards[7].set_value("rhos6", value)

    @property
    def rhos7(self) -> typing.Optional[float]:
        """Get or set the Solid density, p_s, values giving the r as a function of p_s curve for determining the burn velocity
        """ # nopep8
        return self._cards[7].get_value("rhos7")

    @rhos7.setter
    def rhos7(self, value: float) -> None:
        """Set the rhos7 property."""
        self._cards[7].set_value("rhos7", value)

    @property
    def rhos8(self) -> typing.Optional[float]:
        """Get or set the Solid density, p_s, values giving the r as a function of p_s curve for determining the burn velocity
        """ # nopep8
        return self._cards[7].get_value("rhos8")

    @rhos8.setter
    def rhos8(self, value: float) -> None:
        """Set the rhos8 property."""
        self._cards[7].set_value("rhos8", value)

    @property
    def rhos9(self) -> typing.Optional[float]:
        """Get or set the Solid density, p_s, values giving the r as a function of p_s curve for determining the burn velocity
        """ # nopep8
        return self._cards[8].get_value("rhos9")

    @rhos9.setter
    def rhos9(self, value: float) -> None:
        """Set the rhos9 property."""
        self._cards[8].set_value("rhos9", value)

    @property
    def rhos10(self) -> typing.Optional[float]:
        """Get or set the Solid density, p_s, values giving the r as a function of p_s curve for determining the burn velocity
        """ # nopep8
        return self._cards[8].get_value("rhos10")

    @rhos10.setter
    def rhos10(self, value: float) -> None:
        """Set the rhos10 property."""
        self._cards[8].set_value("rhos10", value)

    @property
    def gamma1(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[9].get_value("gamma1")

    @gamma1.setter
    def gamma1(self, value: float) -> None:
        """Set the gamma1 property."""
        self._cards[9].set_value("gamma1", value)

    @property
    def gamma2(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[9].get_value("gamma2")

    @gamma2.setter
    def gamma2(self, value: float) -> None:
        """Set the gamma2 property."""
        self._cards[9].set_value("gamma2", value)

    @property
    def gamma3(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[9].get_value("gamma3")

    @gamma3.setter
    def gamma3(self, value: float) -> None:
        """Set the gamma3 property."""
        self._cards[9].set_value("gamma3", value)

    @property
    def gamma4(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[9].get_value("gamma4")

    @gamma4.setter
    def gamma4(self, value: float) -> None:
        """Set the gamma4 property."""
        self._cards[9].set_value("gamma4", value)

    @property
    def gamma5(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[9].get_value("gamma5")

    @gamma5.setter
    def gamma5(self, value: float) -> None:
        """Set the gamma5 property."""
        self._cards[9].set_value("gamma5", value)

    @property
    def gamma6(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[9].get_value("gamma6")

    @gamma6.setter
    def gamma6(self, value: float) -> None:
        """Set the gamma6 property."""
        self._cards[9].set_value("gamma6", value)

    @property
    def gamma7(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[9].get_value("gamma7")

    @gamma7.setter
    def gamma7(self, value: float) -> None:
        """Set the gamma7 property."""
        self._cards[9].set_value("gamma7", value)

    @property
    def gamma8(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[9].get_value("gamma8")

    @gamma8.setter
    def gamma8(self, value: float) -> None:
        """Set the gamma8 property."""
        self._cards[9].set_value("gamma8", value)

    @property
    def gamma9(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[10].get_value("gamma9")

    @gamma9.setter
    def gamma9(self, value: float) -> None:
        """Set the gamma9 property."""
        self._cards[10].set_value("gamma9", value)

    @property
    def gamma10(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[10].get_value("gamma10")

    @gamma10.setter
    def gamma10(self, value: float) -> None:
        """Set the gamma10 property."""
        self._cards[10].set_value("gamma10", value)

