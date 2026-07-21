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

"""Module providing the EfvEosLee_Tarver class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVEOSLEE_TARVER_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("eosur", int, 10, 10, None),
)

_EFVEOSLEE_TARVER_CARD1 = (
    FieldSchema("a", float, 0, 10, None),
    FieldSchema("b", float, 10, 10, None),
    FieldSchema("r1", float, 20, 10, None),
    FieldSchema("r2", float, 30, 10, None),
    FieldSchema("omega", float, 40, 10, None),
    FieldSchema("dcj", float, 50, 10, None),
    FieldSchema("ecj", float, 60, 10, None),
    FieldSchema("pcj", float, 70, 10, None),
)

_EFVEOSLEE_TARVER_CARD2 = (
    FieldSchema("rzw", float, 0, 10, None),
    FieldSchema("rrmax", float, 10, 10, None),
    FieldSchema("i", float, 20, 10, None),
    FieldSchema("irrexp", float, 30, 10, None),
    FieldSchema("icomp", float, 40, 10, None),
    FieldSchema("icompexp", float, 50, 10, None),
)

_EFVEOSLEE_TARVER_CARD3 = (
    FieldSchema("g1", float, 0, 10, None),
    FieldSchema("c", float, 10, 10, None),
    FieldSchema("d", float, 20, 10, None),
    FieldSchema("y", float, 30, 10, None),
    FieldSchema("g2", float, 40, 10, None),
    FieldSchema("e", float, 50, 10, None),
    FieldSchema("g", float, 60, 10, None),
    FieldSchema("z", float, 70, 10, None),
)

_EFVEOSLEE_TARVER_CARD4 = (
    FieldSchema("maxrr1", float, 0, 10, None),
    FieldSchema("maxrrg", float, 10, 10, None),
    FieldSchema("minrrc", float, 20, 10, None),
    FieldSchema("maxrvol", float, 30, 10, None),
)

_EFVEOSLEE_TARVER_CARD5 = (
    FieldSchema("gamma0", int, 0, 10, None),
    FieldSchema("q2u", float, 10, 10, None),
    FieldSchema("trefu", float, 20, 10, 293.0),
    FieldSchema("cpu", float, 30, 10, None),
    FieldSchema("tcu", float, 40, 10, None),
)

_EFVEOSLEE_TARVER_CARD6 = (
    FieldSchema("c1u", float, 0, 10, None),
    FieldSchema("s1u", float, 10, 10, None),
    FieldSchema("c2u", float, 20, 10, None),
    FieldSchema("s2u", float, 30, 10, None),
    FieldSchema("veu", float, 40, 10, None),
    FieldSchema("vbu", float, 50, 10, None),
)

_EFVEOSLEE_TARVER_CARD7 = (
    FieldSchema("au", float, 0, 10, None),
    FieldSchema("bu", float, 10, 10, None),
    FieldSchema("r1u", float, 20, 10, None),
    FieldSchema("r2u", float, 30, 10, None),
    FieldSchema("omegau", float, 40, 10, None),
    FieldSchema("vru", float, 50, 10, None),
    FieldSchema("ecju", float, 60, 10, None),
)

class EfvEosLee_Tarver(KeywordBase):
    """DYNA EFV_EOS_LEE-TARVER keyword"""

    keyword = "EFV"
    subkeyword = "EOS_LEE-TARVER"

    def __init__(self, **kwargs):
        """Initialize the EfvEosLee_Tarver class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVEOSLEE_TARVER_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOSLEE_TARVER_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOSLEE_TARVER_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOSLEE_TARVER_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOSLEE_TARVER_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOSLEE_TARVER_CARD5,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOSLEE_TARVER_CARD6,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOSLEE_TARVER_CARD7,
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
        """Get or set the EOS of state formulation used for the unreacted explosive (see Remark 1):
        EQ.0: Shock
        EQ.1: JWL
        """ # nopep8
        return self._cards[0].get_value("eosur")

    @eosur.setter
    def eosur(self, value: int) -> None:
        """Set the eosur property."""
        self._cards[0].set_value("eosur", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Product JWL parameter A_p, see Remarks
        """ # nopep8
        return self._cards[1].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[1].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Product JWL parameter B_p, see Remarks
        """ # nopep8
        return self._cards[1].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[1].set_value("b", value)

    @property
    def r1(self) -> typing.Optional[float]:
        """Get or set the Product JWL parameter R1_p, see Remarks
        """ # nopep8
        return self._cards[1].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        """Set the r1 property."""
        self._cards[1].set_value("r1", value)

    @property
    def r2(self) -> typing.Optional[float]:
        """Get or set the Product JWL parameter R2_p, see Remarks
        """ # nopep8
        return self._cards[1].get_value("r2")

    @r2.setter
    def r2(self, value: float) -> None:
        """Set the r2 property."""
        self._cards[1].set_value("r2", value)

    @property
    def omega(self) -> typing.Optional[float]:
        """Get or set the Product JWL parameter OMEGA_p, see Remarks
        """ # nopep8
        return self._cards[1].get_value("omega")

    @omega.setter
    def omega(self, value: float) -> None:
        """Set the omega property."""
        self._cards[1].set_value("omega", value)

    @property
    def dcj(self) -> typing.Optional[float]:
        """Get or set the Chapman-Jouget detonation velocity for the product, D_CJ,p
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
        """Get or set the Chapman-Jouget pressure, p_CJ,p , for the product
        """ # nopep8
        return self._cards[1].get_value("pcj")

    @pcj.setter
    def pcj(self, value: float) -> None:
        """Set the pcj property."""
        self._cards[1].set_value("pcj", value)

    @property
    def rzw(self) -> typing.Optional[float]:
        """Get or set the Reaction zone width
        """ # nopep8
        return self._cards[2].get_value("rzw")

    @rzw.setter
    def rzw(self, value: float) -> None:
        """Set the rzw property."""
        self._cards[2].set_value("rzw", value)

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
        """Get or set the Completion reaction ratio exponent g
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
    def maxrr1(self) -> typing.Optional[float]:
        """Get or set the Maximum reaction ratio for ignition
        """ # nopep8
        return self._cards[4].get_value("maxrr1")

    @maxrr1.setter
    def maxrr1(self, value: float) -> None:
        """Set the maxrr1 property."""
        self._cards[4].set_value("maxrr1", value)

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
    def gamma0(self) -> typing.Optional[int]:
        """Get or set the Gruneisen coefficient,
        """ # nopep8
        return self._cards[5].get_value("gamma0")

    @gamma0.setter
    def gamma0(self, value: int) -> None:
        """Set the gamma0 property."""
        self._cards[5].set_value("gamma0", value)

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
    def trefu(self) -> float:
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
    def au(self) -> typing.Optional[float]:
        """Get or set the Unreacted explosive JWL parameter A_c
        """ # nopep8
        return self._cards[7].get_value("au")

    @au.setter
    def au(self, value: float) -> None:
        """Set the au property."""
        self._cards[7].set_value("au", value)

    @property
    def bu(self) -> typing.Optional[float]:
        """Get or set the Unreacted explosive JWL parameter B_c
        """ # nopep8
        return self._cards[7].get_value("bu")

    @bu.setter
    def bu(self, value: float) -> None:
        """Set the bu property."""
        self._cards[7].set_value("bu", value)

    @property
    def r1u(self) -> typing.Optional[float]:
        """Get or set the Unreacted explosive JWL parameter R1_c
        """ # nopep8
        return self._cards[7].get_value("r1u")

    @r1u.setter
    def r1u(self, value: float) -> None:
        """Set the r1u property."""
        self._cards[7].set_value("r1u", value)

    @property
    def r2u(self) -> typing.Optional[float]:
        """Get or set the Unreacted explosive JWL parameter R2_c
        """ # nopep8
        return self._cards[7].get_value("r2u")

    @r2u.setter
    def r2u(self, value: float) -> None:
        """Set the r2u property."""
        self._cards[7].set_value("r2u", value)

    @property
    def omegau(self) -> typing.Optional[float]:
        """Get or set the Unreacted explosive JWL parameter OMEGAU_c
        """ # nopep8
        return self._cards[7].get_value("omegau")

    @omegau.setter
    def omegau(self, value: float) -> None:
        """Set the omegau property."""
        self._cards[7].set_value("omegau", value)

    @property
    def vru(self) -> typing.Optional[float]:
        """Get or set the von Neumann spike relative volume
        """ # nopep8
        return self._cards[7].get_value("vru")

    @vru.setter
    def vru(self, value: float) -> None:
        """Set the vru property."""
        self._cards[7].set_value("vru", value)

    @property
    def ecju(self) -> typing.Optional[float]:
        """Get or set the Chapman-Jouget energy per unit mass for the product
        """ # nopep8
        return self._cards[7].get_value("ecju")

    @ecju.setter
    def ecju(self, value: float) -> None:
        """Set the ecju property."""
        self._cards[7].set_value("ecju", value)

