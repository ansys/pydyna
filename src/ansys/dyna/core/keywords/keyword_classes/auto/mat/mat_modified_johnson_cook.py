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

"""Module providing the MatModifiedJohnsonCook class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATMODIFIEDJOHNSONCOOK_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("beta", float, 40, 10, None),
    FieldSchema("xsi", float, 50, 10, None),
    FieldSchema("cp", float, 60, 10, None),
    FieldSchema("alpha", float, 70, 10, None),
)

_MATMODIFIEDJOHNSONCOOK_CARD1 = (
    FieldSchema("e0dot", float, 0, 10, None),
    FieldSchema("tr", float, 10, 10, None),
    FieldSchema("tm", float, 20, 10, None),
    FieldSchema("t0", float, 30, 10, None),
    FieldSchema("flag1", float, 40, 10, None),
    FieldSchema("flag2", float, 50, 10, None),
)

_MATMODIFIEDJOHNSONCOOK_CARD2 = (
    FieldSchema("a/siga", float, 0, 10, None),
    FieldSchema("b/b", float, 10, 10, None),
    FieldSchema("n/beta0", float, 20, 10, None),
    FieldSchema("c/beta1", float, 30, 10, None),
    FieldSchema("m/na", float, 40, 10, None),
)

_MATMODIFIEDJOHNSONCOOK_CARD3 = (
    FieldSchema("q1/a", float, 0, 10, None),
    FieldSchema("c1/n", float, 10, 10, None),
    FieldSchema("q2/alpha0", float, 20, 10, None),
    FieldSchema("c2/alpha1", float, 30, 10, None),
)

_MATMODIFIEDJOHNSONCOOK_CARD4 = (
    FieldSchema("dc/dc", float, 0, 10, None),
    FieldSchema("pd/wc", float, 10, 10, None),
    FieldSchema("d1/na", float, 20, 10, None),
    FieldSchema("d2/na", float, 30, 10, None),
    FieldSchema("d3/na", float, 40, 10, None),
    FieldSchema("d4/na", float, 50, 10, None),
    FieldSchema("d5/na", float, 60, 10, None),
)

_MATMODIFIEDJOHNSONCOOK_CARD5 = (
    FieldSchema("tc", float, 0, 10, None),
    FieldSchema("tauc", float, 10, 10, None),
)

class MatModifiedJohnsonCook(KeywordBase):
    """DYNA MAT_MODIFIED_JOHNSON_COOK keyword"""

    keyword = "MAT"
    subkeyword = "MODIFIED_JOHNSON_COOK"

    def __init__(self, **kwargs):
        """Initialize the MatModifiedJohnsonCook class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATMODIFIEDJOHNSONCOOK_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATMODIFIEDJOHNSONCOOK_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATMODIFIEDJOHNSONCOOK_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATMODIFIEDJOHNSONCOOK_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATMODIFIEDJOHNSONCOOK_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATMODIFIEDJOHNSONCOOK_CARD5,
                **kwargs,
            ),        ]
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
    def beta(self) -> typing.Optional[float]:
        """Get or set the Damage coupling parameter.
        EQ.0.0 No coupling between ductile damage and the constitutive relation.
        EQ.1.0 Full coupling between ductile damage and the constitutive relation.
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[0].set_value("beta", value)

    @property
    def xsi(self) -> typing.Optional[float]:
        """Get or set the Taylor-Quinney coefficient. Gives the portion of plastic work converted into heat (normally taken to be 0.9).
        """ # nopep8
        return self._cards[0].get_value("xsi")

    @xsi.setter
    def xsi(self, value: float) -> None:
        """Set the xsi property."""
        self._cards[0].set_value("xsi", value)

    @property
    def cp(self) -> typing.Optional[float]:
        """Get or set the Specific heat.
        """ # nopep8
        return self._cards[0].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        """Set the cp property."""
        self._cards[0].set_value("cp", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Thermal expansion coefficient.
        """ # nopep8
        return self._cards[0].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[0].set_value("alpha", value)

    @property
    def e0dot(self) -> typing.Optional[float]:
        """Get or set the User-defined strain rate normalization factor
        """ # nopep8
        return self._cards[1].get_value("e0dot")

    @e0dot.setter
    def e0dot(self, value: float) -> None:
        """Set the e0dot property."""
        self._cards[1].set_value("e0dot", value)

    @property
    def tr(self) -> typing.Optional[float]:
        """Get or set the Room temperature
        """ # nopep8
        return self._cards[1].get_value("tr")

    @tr.setter
    def tr(self, value: float) -> None:
        """Set the tr property."""
        self._cards[1].set_value("tr", value)

    @property
    def tm(self) -> typing.Optional[float]:
        """Get or set the Melt temperature
        """ # nopep8
        return self._cards[1].get_value("tm")

    @tm.setter
    def tm(self, value: float) -> None:
        """Set the tm property."""
        self._cards[1].set_value("tm", value)

    @property
    def t0(self) -> typing.Optional[float]:
        """Get or set the Initial temperature
        """ # nopep8
        return self._cards[1].get_value("t0")

    @t0.setter
    def t0(self, value: float) -> None:
        """Set the t0 property."""
        self._cards[1].set_value("t0", value)

    @property
    def flag1(self) -> typing.Optional[float]:
        """Get or set the Constitutive relation flag used for parameters on card 3 and 4.
        EQ.0.0 Modified Johnson-Cook constitutive relation.
        EQ.1.0 Zerilli-Armstrong constitutive relation.
        """ # nopep8
        return self._cards[1].get_value("flag1")

    @flag1.setter
    def flag1(self, value: float) -> None:
        """Set the flag1 property."""
        self._cards[1].set_value("flag1", value)

    @property
    def flag2(self) -> typing.Optional[float]:
        """Get or set the Fracture criterion flag used for parameters on card 5
        EQ.0.0 Modified Johnson-Cook fracture criterion.
        EQ.1.0 Cockcroft-Latharn fracture criterion.
        """ # nopep8
        return self._cards[1].get_value("flag2")

    @flag2.setter
    def flag2(self, value: float) -> None:
        """Set the flag2 property."""
        self._cards[1].set_value("flag2", value)

    @property
    def a_siga(self) -> typing.Optional[float]:
        """Get or set the If FLAG1=0: Johnson-Cook yield stress A.
        If FLAG1=1 :Zerilli-Armstrong parameter alfa_a.
        """ # nopep8
        return self._cards[2].get_value("a/siga")

    @a_siga.setter
    def a_siga(self, value: float) -> None:
        """Set the a_siga property."""
        self._cards[2].set_value("a/siga", value)

    @property
    def b_b(self) -> typing.Optional[float]:
        """Get or set the If FLAG1=0: Johnson-Cook hardening parameter B.
        If FLAG1=1: Zerilli-Armstrong parameter B.
        """ # nopep8
        return self._cards[2].get_value("b/b")

    @b_b.setter
    def b_b(self, value: float) -> None:
        """Set the b_b property."""
        self._cards[2].set_value("b/b", value)

    @property
    def n_beta0(self) -> typing.Optional[float]:
        """Get or set the If FLAG1=0: Johnson-Cook hardening parameter
        If FLAG1=1: Zerilli-Armstrong parameter beta_0.
        """ # nopep8
        return self._cards[2].get_value("n/beta0")

    @n_beta0.setter
    def n_beta0(self, value: float) -> None:
        """Set the n_beta0 property."""
        self._cards[2].set_value("n/beta0", value)

    @property
    def c_beta1(self) -> typing.Optional[float]:
        """Get or set the If FLAG1=0: Johnson-Cook hardening parameter C
        If FLAG1=1: Zerilli-Armstrong parameter beta_1.
        """ # nopep8
        return self._cards[2].get_value("c/beta1")

    @c_beta1.setter
    def c_beta1(self, value: float) -> None:
        """Set the c_beta1 property."""
        self._cards[2].set_value("c/beta1", value)

    @property
    def m_na(self) -> typing.Optional[float]:
        """Get or set the Define only if FLAG1=0: Johnson-Cook thermal softening parameter m
        """ # nopep8
        return self._cards[2].get_value("m/na")

    @m_na.setter
    def m_na(self, value: float) -> None:
        """Set the m_na property."""
        self._cards[2].set_value("m/na", value)

    @property
    def q1_a(self) -> typing.Optional[float]:
        """Get or set the If FLAG1=0: Voce hardening parameter Q1.
        If FLAG1=1 :Zerilli-Armstrong parameter alfa_a.
        """ # nopep8
        return self._cards[3].get_value("q1/a")

    @q1_a.setter
    def q1_a(self, value: float) -> None:
        """Set the q1_a property."""
        self._cards[3].set_value("q1/a", value)

    @property
    def c1_n(self) -> typing.Optional[float]:
        """Get or set the If FLAG1=0: Voce hardening parameter C1.
        If FLAG1=1: Zerilli-Armstrong parameter B.
        """ # nopep8
        return self._cards[3].get_value("c1/n")

    @c1_n.setter
    def c1_n(self, value: float) -> None:
        """Set the c1_n property."""
        self._cards[3].set_value("c1/n", value)

    @property
    def q2_alpha0(self) -> typing.Optional[float]:
        """Get or set the If FLAG1=0: Voce hardening parameter Q2
        If FLAG1=1: Zerilli-Armstrong parameter beta_0.
        """ # nopep8
        return self._cards[3].get_value("q2/alpha0")

    @q2_alpha0.setter
    def q2_alpha0(self, value: float) -> None:
        """Set the q2_alpha0 property."""
        self._cards[3].set_value("q2/alpha0", value)

    @property
    def c2_alpha1(self) -> typing.Optional[float]:
        """Get or set the If FLAG1=0: Voce hardening parameter C2
        If FLAG1=1: Zerilli-Armstrong parameter beta_1.
        """ # nopep8
        return self._cards[3].get_value("c2/alpha1")

    @c2_alpha1.setter
    def c2_alpha1(self, value: float) -> None:
        """Set the c2_alpha1 property."""
        self._cards[3].set_value("c2/alpha1", value)

    @property
    def dc_dc(self) -> typing.Optional[float]:
        """Get or set the Critical damage parameter Dc. When the damage value D reaches this value, the element is eroded from the calculation.
        """ # nopep8
        return self._cards[4].get_value("dc/dc")

    @dc_dc.setter
    def dc_dc(self, value: float) -> None:
        """Set the dc_dc property."""
        self._cards[4].set_value("dc/dc", value)

    @property
    def pd_wc(self) -> typing.Optional[float]:
        """Get or set the If FLAG2=0: Damage threshold.
        If FLAG2=1: Critical Cockcroft-Latham parameter Wc. When the plastic work per volume reaches this value, the element is eroded from the simulation..
        """ # nopep8
        return self._cards[4].get_value("pd/wc")

    @pd_wc.setter
    def pd_wc(self, value: float) -> None:
        """Set the pd_wc property."""
        self._cards[4].set_value("pd/wc", value)

    @property
    def d1_na(self) -> typing.Optional[float]:
        """Get or set the Define only if FLAG2=0: Fracture parameters in the Johnson-Cook fracture criterion.
        """ # nopep8
        return self._cards[4].get_value("d1/na")

    @d1_na.setter
    def d1_na(self, value: float) -> None:
        """Set the d1_na property."""
        self._cards[4].set_value("d1/na", value)

    @property
    def d2_na(self) -> typing.Optional[float]:
        """Get or set the Define only if FLAG2=0: Fracture parameters in the Johnson-Cook fracture criterion.
        """ # nopep8
        return self._cards[4].get_value("d2/na")

    @d2_na.setter
    def d2_na(self, value: float) -> None:
        """Set the d2_na property."""
        self._cards[4].set_value("d2/na", value)

    @property
    def d3_na(self) -> typing.Optional[float]:
        """Get or set the Define only if FLAG2=0: Fracture parameters in the Johnson-Cook fracture criterion.
        """ # nopep8
        return self._cards[4].get_value("d3/na")

    @d3_na.setter
    def d3_na(self, value: float) -> None:
        """Set the d3_na property."""
        self._cards[4].set_value("d3/na", value)

    @property
    def d4_na(self) -> typing.Optional[float]:
        """Get or set the Define only if FLAG2=0: Fracture parameters in the Johnson-Cook fracture criterion.
        """ # nopep8
        return self._cards[4].get_value("d4/na")

    @d4_na.setter
    def d4_na(self, value: float) -> None:
        """Set the d4_na property."""
        self._cards[4].set_value("d4/na", value)

    @property
    def d5_na(self) -> typing.Optional[float]:
        """Get or set the Define only if FLAG2=0: Fracture parameters in the Johnson-Cook fracture criterion.
        """ # nopep8
        return self._cards[4].get_value("d5/na")

    @d5_na.setter
    def d5_na(self, value: float) -> None:
        """Set the d5_na property."""
        self._cards[4].set_value("d5/na", value)

    @property
    def tc(self) -> typing.Optional[float]:
        """Get or set the Critical temperature parameter. When the temperature T reaches this value, the element is eroded from the simulation.
        """ # nopep8
        return self._cards[5].get_value("tc")

    @tc.setter
    def tc(self, value: float) -> None:
        """Set the tc property."""
        self._cards[5].set_value("tc", value)

    @property
    def tauc(self) -> typing.Optional[float]:
        """Get or set the Critical shear stress parameter. When the maximum shear stress reaches this value, the element is eroded from the simulation.
        """ # nopep8
        return self._cards[5].get_value("tauc")

    @tauc.setter
    def tauc(self, value: float) -> None:
        """Set the tauc property."""
        self._cards[5].set_value("tauc", value)


class Mat107(MatModifiedJohnsonCook):
    """Alias for MAT keyword."""
    subkeyword = "107"
