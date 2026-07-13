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

"""Module providing the MatJohnsonCookStochastic class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATJOHNSONCOOKSTOCHASTIC_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("g", float, 20, 10, None),
    FieldSchema("e", float, 30, 10, None),
    FieldSchema("pr", float, 40, 10, None),
    FieldSchema("dtf", float, 50, 10, None),
    FieldSchema("vp", float, 60, 10, 0.0),
    FieldSchema("rateop", float, 70, 10, 0.0),
)

_MATJOHNSONCOOKSTOCHASTIC_CARD1 = (
    FieldSchema("a", float, 0, 10, None),
    FieldSchema("b", float, 10, 10, None),
    FieldSchema("n", float, 20, 10, None),
    FieldSchema("c", float, 30, 10, None),
    FieldSchema("m", float, 40, 10, None),
    FieldSchema("tm", float, 50, 10, None),
    FieldSchema("tr", float, 60, 10, None),
    FieldSchema("epso", float, 70, 10, None),
)

_MATJOHNSONCOOKSTOCHASTIC_CARD2 = (
    FieldSchema("cp", float, 0, 10, None),
    FieldSchema("pc", float, 10, 10, None),
    FieldSchema("spall", float, 20, 10, 2.0),
    FieldSchema("it", float, 30, 10, 0.0),
    FieldSchema("d1_beta", float, 40, 10, None, "d1/beta"),
    FieldSchema("d2_wc", float, 50, 10, None, "d2/wc"),
    FieldSchema("d3_dc", float, 60, 10, None, "d3/dc"),
    FieldSchema("d4_gamma", float, 70, 10, None, "d4/gamma"),
)

_MATJOHNSONCOOKSTOCHASTIC_CARD3 = (
    FieldSchema("d5", float, 0, 10, None),
    FieldSchema("c2_p_xnp_d", float, 10, 10, None, "c2/p/xnp/d"),
    FieldSchema("erod", float, 20, 10, None),
    FieldSchema("efmin", float, 30, 10, 1e-06),
    FieldSchema("numint", float, 40, 10, None),
    FieldSchema("k", float, 50, 10, None),
    FieldSchema("eps1", float, 60, 10, None),
    FieldSchema("dmodel", float, 70, 10, 0.0),
)

_MATJOHNSONCOOKSTOCHASTIC_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatJohnsonCookStochastic(KeywordBase):
    """DYNA MAT_JOHNSON_COOK_STOCHASTIC keyword"""

    keyword = "MAT"
    subkeyword = "JOHNSON_COOK_STOCHASTIC"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatJohnsonCookStochastic class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATJOHNSONCOOKSTOCHASTIC_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATJOHNSONCOOKSTOCHASTIC_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATJOHNSONCOOKSTOCHASTIC_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATJOHNSONCOOKSTOCHASTIC_CARD3,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = MatJohnsonCookStochastic._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATJOHNSONCOOKSTOCHASTIC_OPTION0_CARD0,
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
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus.
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[0].set_value("g", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's Modulus (shell elements only).
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio (shell elements only).
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def dtf(self) -> typing.Optional[float]:
        """Get or set the Minimum time step size for automatic element deletion (shell elements).
        """ # nopep8
        return self._cards[0].get_value("dtf")

    @dtf.setter
    def dtf(self, value: float) -> None:
        """Set the dtf property."""
        self._cards[0].set_value("dtf", value)

    @property
    def vp(self) -> float:
        """Get or set the Formulation for rate effects:
        EQ.0.0: Scale yield stress (default),
        EQ.1.0: Viscoplastic formulation.
        """ # nopep8
        return self._cards[0].get_value("vp")

    @vp.setter
    def vp(self, value: float) -> None:
        """Set the vp property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""vp must be `None` or one of {0.0,1.0}.""")
        self._cards[0].set_value("vp", value)

    @property
    def rateop(self) -> float:
        """Get or set the Optional forms of strain-rate term:
        EQ.0.0: Log-Linear Johnson-Cook (default),
        EQ.1.0: Log-Quadratic Huh-Kang (2 parameters),
        EQ.2.0: Exponential Allen-Rule-Jones,
        EQ.3.0: Exponential Cowper-Symonds (2 parameters).
        EQ.4.0: nonlinear rate coefficient (2 parameters)
        EQ.5.0: log - exponential Couque(4 parameters)
        """ # nopep8
        return self._cards[0].get_value("rateop")

    @rateop.setter
    def rateop(self, value: float) -> None:
        """Set the rateop property."""
        if value not in [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, None]:
            raise Exception("""rateop must be `None` or one of {0.0,1.0,2.0,3.0,4.0,5.0}.""")
        self._cards[0].set_value("rateop", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Constant A in the flow stress formula or load curve giving the plastic strain term in the flow stress formula: (see Remark 1):
        GT.0.0:	Constant A term in the flow stress formula
        LT.0.0:	|A| refers to a load curve ID for the curve giving the plastic strain term in the flow stress formula. The abscissa for the curve is the effective plastic strain.This curve is only available for solid elements.
        """ # nopep8
        return self._cards[1].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[1].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Constant B in the flow stress. See equations in Remark 1. This field is ignored if A < 0.0.
        """ # nopep8
        return self._cards[1].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[1].set_value("b", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Constant n in the flow stress. See equations in Remark 1. This field is ignored if A < 0.0.
        """ # nopep8
        return self._cards[1].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[1].set_value("n", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Constant C in the flow stress. See equations in Remarks 1 and 5
        """ # nopep8
        return self._cards[1].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[1].set_value("c", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Constant m in the flow stress. See equations in Remark 1
        """ # nopep8
        return self._cards[1].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[1].set_value("m", value)

    @property
    def tm(self) -> typing.Optional[float]:
        """Get or set the Melt temperature.
        """ # nopep8
        return self._cards[1].get_value("tm")

    @tm.setter
    def tm(self, value: float) -> None:
        """Set the tm property."""
        self._cards[1].set_value("tm", value)

    @property
    def tr(self) -> typing.Optional[float]:
        """Get or set the Room temperature.
        """ # nopep8
        return self._cards[1].get_value("tr")

    @tr.setter
    def tr(self, value: float) -> None:
        """Set the tr property."""
        self._cards[1].set_value("tr", value)

    @property
    def epso(self) -> typing.Optional[float]:
        """Get or set the Quasi-static threshold strain rate (see Remark 1).  Ideally, this value represents the highest strain rate for which no rate adjustment to the flow stress is needed and is input in units of [time ]^(-1).  For example, if strain rate effects on the flow stress first become apparent at strain rates greater than 10^(-2)  s^(-1), and the system of units for the model input is {kg, mm, ms}, then EPSO should be set to 10-5.
        """ # nopep8
        return self._cards[1].get_value("epso")

    @epso.setter
    def epso(self, value: float) -> None:
        """Set the epso property."""
        self._cards[1].set_value("epso", value)

    @property
    def cp(self) -> typing.Optional[float]:
        """Get or set the Specific heat (superseded by heat capacity in *MAT_THERMAL_OPTION if a coupled thermal/structural analysis)
        """ # nopep8
        return self._cards[2].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        """Set the cp property."""
        self._cards[2].set_value("cp", value)

    @property
    def pc(self) -> typing.Optional[float]:
        """Get or set the Tensile failure stress or tensile pressure cutoff (PC < 0.0)
        """ # nopep8
        return self._cards[2].get_value("pc")

    @pc.setter
    def pc(self, value: float) -> None:
        """Set the pc property."""
        self._cards[2].set_value("pc", value)

    @property
    def spall(self) -> float:
        """Get or set the Spall type (see Remark 3):
        EQ.0.0: Set to “2.0”(default).
        EQ.1.0 : Tensile pressure is limited by PC, that is, p is always >= PC.
        Shell Element Specific Behavior :
        EQ.2.0 : Shell elements are deleted when σ_max >= - PC .
        EQ.3.0 : Shell elements are deleted when p < PC .
        Solid Element Specific Behavior
        EQ.2.0 : For solid elements σ_max >= - PC  resets tensile stresses to zero.Compressive stresses are still allowed.
        EQ.3.0 : For solid elements p < PC  resets the pressure to zero, thereby disallowing tensile pressure.
        """ # nopep8
        return self._cards[2].get_value("spall")

    @spall.setter
    def spall(self, value: float) -> None:
        """Set the spall property."""
        self._cards[2].set_value("spall", value)

    @property
    def it(self) -> float:
        """Get or set the Plastic strain iteration options. This input applies to solid elements only since it is always necessary to iterate for the shell element plane stress condition.
        EQ. 0.0: no iterations (default),
        EQ. 1.0: accurate iterative solution for plastic strain. This option is much more expensive than the default.
        """ # nopep8
        return self._cards[2].get_value("it")

    @it.setter
    def it(self, value: float) -> None:
        """Set the it property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""it must be `None` or one of {0.0,1.0}.""")
        self._cards[2].set_value("it", value)

    @property
    def d1_beta(self) -> typing.Optional[float]:
        """Get or set the Failure parameter. See equations in the keyword manual page 62 (volume two).
        Damage coupling parameter; see Equation Error! Reference source not found. in *MAT_107.
        EQ.0.0: No coupling between ductile damage and the constitutive relation.
        EQ.1.0 : Full coupling between ductile damage and the constitutive relation.
        """ # nopep8
        return self._cards[2].get_value("d1_beta")

    @d1_beta.setter
    def d1_beta(self, value: float) -> None:
        """Set the d1_beta property."""
        self._cards[2].set_value("d1_beta", value)

    @property
    def d2_wc(self) -> typing.Optional[float]:
        """Get or set the Failure parameter. See equations in the keyword manual page 62 (volume two).
        Critical Cockcroft-Latham parameter W_c; see Equation Error! Reference source not found. in *MAT_107. When the plastic work per volume reaches this value, the element is eroded from the simulation. It defines the overall ductility of the material.
        """ # nopep8
        return self._cards[2].get_value("d2_wc")

    @d2_wc.setter
    def d2_wc(self, value: float) -> None:
        """Set the d2_wc property."""
        self._cards[2].set_value("d2_wc", value)

    @property
    def d3_dc(self) -> typing.Optional[float]:
        """Get or set the Failure parameter. See equations in the keyword manual page 62 (volume two).
        Critical Cockcroft-Latham damage parameter D_c; see Equations Error! Reference source not found. and Error! Reference source not found. in *MAT_107. When the damage reaches this value, the element is eroded from the calculation.
        """ # nopep8
        return self._cards[2].get_value("d3_dc")

    @d3_dc.setter
    def d3_dc(self, value: float) -> None:
        """Set the d3_dc property."""
        self._cards[2].set_value("d3_dc", value)

    @property
    def d4_gamma(self) -> typing.Optional[float]:
        """Get or set the Failure parameter. See equations in the keyword manual page 62 (volume two).
        Extended Cockcroft-Latham parameter γ; see Equation Error! Reference source not found. in *MAT_107. It controls the intensity of the damage's dependence on the stress state.
        """ # nopep8
        return self._cards[2].get_value("d4_gamma")

    @d4_gamma.setter
    def d4_gamma(self, value: float) -> None:
        """Set the d4_gamma property."""
        self._cards[2].set_value("d4_gamma", value)

    @property
    def d5(self) -> typing.Optional[float]:
        """Get or set the Failure parameter. Please see equations in the keyword manual page 62 (volume two).
        """ # nopep8
        return self._cards[3].get_value("d5")

    @d5.setter
    def d5(self, value: float) -> None:
        """Set the d5 property."""
        self._cards[3].set_value("d5", value)

    @property
    def c2_p_xnp_d(self) -> typing.Optional[float]:
        """Get or set the Optional strain-rate parameter for Huh-Kang (C2) or Cowper-Symonds (P) forms; see equations below
        """ # nopep8
        return self._cards[3].get_value("c2_p_xnp_d")

    @c2_p_xnp_d.setter
    def c2_p_xnp_d(self, value: float) -> None:
        """Set the c2_p_xnp_d property."""
        self._cards[3].set_value("c2_p_xnp_d", value)

    @property
    def erod(self) -> typing.Optional[float]:
        """Get or set the Erosion flag:
        EQ.0.0: element erosion allowed(default).
        NE.0.0: element does not erode; deviatoric stresses set to zero when element fails.
        """ # nopep8
        return self._cards[3].get_value("erod")

    @erod.setter
    def erod(self, value: float) -> None:
        """Set the erod property."""
        self._cards[3].set_value("erod", value)

    @property
    def efmin(self) -> float:
        """Get or set the The lower bound for calculated strain at fracture
        """ # nopep8
        return self._cards[3].get_value("efmin")

    @efmin.setter
    def efmin(self, value: float) -> None:
        """Set the efmin property."""
        self._cards[3].set_value("efmin", value)

    @property
    def numint(self) -> typing.Optional[float]:
        """Get or set the Number of through thickness integration points which must fail before the shell element is deleted. (If zero, all points must fail.)
        Since nodal fiber rotations limit strains at active integration points, the default, which is to require that all integration points fail, is not recommended, because elements undergoing large strain are often not deleted using this criterion.Better results may be obtained when NUMINT is set to 1 or a number less than one half of the number of through thickness points.
        """ # nopep8
        return self._cards[3].get_value("numint")

    @numint.setter
    def numint(self, value: float) -> None:
        """Set the numint property."""
        self._cards[3].set_value("numint", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Optional strain-rate parameter for Couque term
        """ # nopep8
        return self._cards[3].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        """Set the k property."""
        self._cards[3].set_value("k", value)

    @property
    def eps1(self) -> typing.Optional[float]:
        """Get or set the Optional reference strain rate for Couque term, characterizing the transition between the thermally activated regime and the viscous regime. Input in units of [time ]**(-1)
        """ # nopep8
        return self._cards[3].get_value("eps1")

    @eps1.setter
    def eps1(self, value: float) -> None:
        """Set the eps1 property."""
        self._cards[3].set_value("eps1", value)

    @property
    def dmodel(self) -> float:
        """Get or set the Damage model:
        EQ.0: Johnson - Cook damage and failure model(default).
        EQ.1 : Cockcroft - Latham damage model(same as in * MAT_107).
        EQ.2 : Extended Cockcroft - Latham damage model(same as in * MAT_107).
        """ # nopep8
        return self._cards[3].get_value("dmodel")

    @dmodel.setter
    def dmodel(self, value: float) -> None:
        """Set the dmodel property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""dmodel must be `None` or one of {0,1,2}.""")
        self._cards[3].set_value("dmodel", value)

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

