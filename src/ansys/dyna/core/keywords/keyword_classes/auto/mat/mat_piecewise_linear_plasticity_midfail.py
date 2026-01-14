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

"""Module providing the MatPiecewiseLinearPlasticityMidfail class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_MATPIECEWISELINEARPLASTICITYMIDFAIL_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("sigy", float, 40, 10, None),
    FieldSchema("etan", float, 50, 10, None),
    FieldSchema("fail", float, 60, 10, 1e+21),
    FieldSchema("tdel", float, 70, 10, None),
)

_MATPIECEWISELINEARPLASTICITYMIDFAIL_CARD1 = (
    FieldSchema("c", float, 0, 10, None),
    FieldSchema("p", float, 10, 10, None),
    FieldSchema("lcss", int, 20, 10, 0),
    FieldSchema("lcsr", int, 30, 10, 0),
    FieldSchema("vp", float, 40, 10, 0.0),
)

_MATPIECEWISELINEARPLASTICITYMIDFAIL_CARD2 = (
    FieldSchema("eps1", float, 0, 10, None),
    FieldSchema("eps2", float, 10, 10, None),
    FieldSchema("eps3", float, 20, 10, None),
    FieldSchema("eps4", float, 30, 10, None),
    FieldSchema("eps5", float, 40, 10, None),
    FieldSchema("eps6", float, 50, 10, None),
    FieldSchema("eps7", float, 60, 10, None),
    FieldSchema("eps8", float, 70, 10, None),
)

_MATPIECEWISELINEARPLASTICITYMIDFAIL_CARD3 = (
    FieldSchema("es1", float, 0, 10, None),
    FieldSchema("es2", float, 10, 10, None),
    FieldSchema("es3", float, 20, 10, None),
    FieldSchema("es4", float, 30, 10, None),
    FieldSchema("es5", float, 40, 10, None),
    FieldSchema("es6", float, 50, 10, None),
    FieldSchema("es7", float, 60, 10, None),
    FieldSchema("es8", float, 70, 10, None),
)

_MATPIECEWISELINEARPLASTICITYMIDFAIL_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatPiecewiseLinearPlasticityMidfail(KeywordBase):
    """DYNA MAT_PIECEWISE_LINEAR_PLASTICITY_MIDFAIL keyword"""

    keyword = "MAT"
    subkeyword = "PIECEWISE_LINEAR_PLASTICITY_MIDFAIL"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the MatPiecewiseLinearPlasticityMidfail class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATPIECEWISELINEARPLASTICITYMIDFAIL_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATPIECEWISELINEARPLASTICITYMIDFAIL_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATPIECEWISELINEARPLASTICITYMIDFAIL_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATPIECEWISELINEARPLASTICITYMIDFAIL_CARD3,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatPiecewiseLinearPlasticityMidfail.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATPIECEWISELINEARPLASTICITYMIDFAIL_OPTION0_CARD0,
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
        """Get or set the Yield stress.
        """ # nopep8
        return self._cards[0].get_value("sigy")

    @sigy.setter
    def sigy(self, value: float) -> None:
        """Set the sigy property."""
        self._cards[0].set_value("sigy", value)

    @property
    def etan(self) -> typing.Optional[float]:
        """Get or set the Tangent modulus, ignored if (LCSS.GT.0) is defined.
        """ # nopep8
        return self._cards[0].get_value("etan")

    @etan.setter
    def etan(self, value: float) -> None:
        """Set the etan property."""
        self._cards[0].set_value("etan", value)

    @property
    def fail(self) -> float:
        """Get or set the Failure flag:
        LT.0.0: User defined failure subroutine is called to determine failure
        EQ.0.0: Failure is not considered. Recommended if failure is not of interest.
        GT.0.0: Plastic strain to failure. When the plastic strain reaches this value, the element is deleted from the calculation.
        """ # nopep8
        return self._cards[0].get_value("fail")

    @fail.setter
    def fail(self, value: float) -> None:
        """Set the fail property."""
        self._cards[0].set_value("fail", value)

    @property
    def tdel(self) -> typing.Optional[float]:
        """Get or set the Minimum time step size for automatic element deletion.
        """ # nopep8
        return self._cards[0].get_value("tdel")

    @tdel.setter
    def tdel(self, value: float) -> None:
        """Set the tdel property."""
        self._cards[0].set_value("tdel", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter, C, see formula in keyword manual page 98 (volume two).
        """ # nopep8
        return self._cards[1].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[1].set_value("c", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter, P, see formula in keyword manual page 98 (volume two).
        """ # nopep8
        return self._cards[1].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        """Set the p property."""
        self._cards[1].set_value("p", value)

    @property
    def lcss(self) -> int:
        """Get or set the Load curve ID or Table ID.
        Load curve ID defining effective stress versus effective plastic strain. If defined EPS1-EPS8 and ES1-ES8 are ignored.
        The table ID defines for each strain rate value a load curve ID giving the stress versus effective plastic strain for that rate. The stress versus effective plastic strain curve for the lowest value of strain rate is used if the strain rate falls below the minimum value. Likewise, the stress versus effective plastic strain curve for the highest value of strain rate is used if the strain rate exceeds the maximum value. If defined C, P,curve ID, LCSR, EPS1-EPS8 and ES1-ES8 are ignored.
        """ # nopep8
        return self._cards[1].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        """Set the lcss property."""
        self._cards[1].set_value("lcss", value)

    @property
    def lcsr(self) -> int:
        """Get or set the Load curve ID defining strain rate scaling effect on yield stress.
        """ # nopep8
        return self._cards[1].get_value("lcsr")

    @lcsr.setter
    def lcsr(self, value: int) -> None:
        """Set the lcsr property."""
        self._cards[1].set_value("lcsr", value)

    @property
    def vp(self) -> float:
        """Get or set the Formulation for rate effects:
        EQ-1.0: Cowper-Symonds with effective deviatoric strain rate rather than total.
        EQ.0.0: Scale yield stress (default),
        EQ.1.0: Viscoplastic formulation.
        EQ.3.0:	same as VP=0, but with filtered effective total strain rates (only available for shells)
        """ # nopep8
        return self._cards[1].get_value("vp")

    @vp.setter
    def vp(self, value: float) -> None:
        """Set the vp property."""
        if value not in [0.0, 1.0, -1.0, 3.0, None]:
            raise Exception("""vp must be `None` or one of {0.0,1.0,-1.0,3.0}.""")
        self._cards[1].set_value("vp", value)

    @property
    def eps1(self) -> typing.Optional[float]:
        """Get or set the First effective plastic strain value (optional if SIGY is defined). At least 2 points should be defined. The first point must be zero corresponding to the initial yield stress.
        WARNING: If the first point is nonzero the yield stress is extrapolated to determine the initial yield. If this option is used SIGY and ETAN are ignored and may be input as zero.
        """ # nopep8
        return self._cards[2].get_value("eps1")

    @eps1.setter
    def eps1(self, value: float) -> None:
        """Set the eps1 property."""
        self._cards[2].set_value("eps1", value)

    @property
    def eps2(self) -> typing.Optional[float]:
        """Get or set the Second effective plastic strain value (optional if SIGY is defined). At least 2 points should be defined. The first point must be zero corresponding to the initial yield stress.
        """ # nopep8
        return self._cards[2].get_value("eps2")

    @eps2.setter
    def eps2(self, value: float) -> None:
        """Set the eps2 property."""
        self._cards[2].set_value("eps2", value)

    @property
    def eps3(self) -> typing.Optional[float]:
        """Get or set the Third effective plastic strain value (optional if SIGY is defined).
        """ # nopep8
        return self._cards[2].get_value("eps3")

    @eps3.setter
    def eps3(self, value: float) -> None:
        """Set the eps3 property."""
        self._cards[2].set_value("eps3", value)

    @property
    def eps4(self) -> typing.Optional[float]:
        """Get or set the Fourth effective plastic strain value (optional if SIGY is defined).
        """ # nopep8
        return self._cards[2].get_value("eps4")

    @eps4.setter
    def eps4(self, value: float) -> None:
        """Set the eps4 property."""
        self._cards[2].set_value("eps4", value)

    @property
    def eps5(self) -> typing.Optional[float]:
        """Get or set the Fifth effective plastic strain value (optional if SIGY is defined).
        """ # nopep8
        return self._cards[2].get_value("eps5")

    @eps5.setter
    def eps5(self, value: float) -> None:
        """Set the eps5 property."""
        self._cards[2].set_value("eps5", value)

    @property
    def eps6(self) -> typing.Optional[float]:
        """Get or set the Sixth effective plastic strain value (optional if SIGY is defined).
        """ # nopep8
        return self._cards[2].get_value("eps6")

    @eps6.setter
    def eps6(self, value: float) -> None:
        """Set the eps6 property."""
        self._cards[2].set_value("eps6", value)

    @property
    def eps7(self) -> typing.Optional[float]:
        """Get or set the Seventh effective plastic strain value (optiona l if SIGY is defined).
        """ # nopep8
        return self._cards[2].get_value("eps7")

    @eps7.setter
    def eps7(self, value: float) -> None:
        """Set the eps7 property."""
        self._cards[2].set_value("eps7", value)

    @property
    def eps8(self) -> typing.Optional[float]:
        """Get or set the Eighth effective plastic strain value (optional if SIGY is defined).
        """ # nopep8
        return self._cards[2].get_value("eps8")

    @eps8.setter
    def eps8(self, value: float) -> None:
        """Set the eps8 property."""
        self._cards[2].set_value("eps8", value)

    @property
    def es1(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress value to EPS1
        """ # nopep8
        return self._cards[3].get_value("es1")

    @es1.setter
    def es1(self, value: float) -> None:
        """Set the es1 property."""
        self._cards[3].set_value("es1", value)

    @property
    def es2(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress value to EPS2
        """ # nopep8
        return self._cards[3].get_value("es2")

    @es2.setter
    def es2(self, value: float) -> None:
        """Set the es2 property."""
        self._cards[3].set_value("es2", value)

    @property
    def es3(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress value to EPS3
        """ # nopep8
        return self._cards[3].get_value("es3")

    @es3.setter
    def es3(self, value: float) -> None:
        """Set the es3 property."""
        self._cards[3].set_value("es3", value)

    @property
    def es4(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress value to EPS4
        """ # nopep8
        return self._cards[3].get_value("es4")

    @es4.setter
    def es4(self, value: float) -> None:
        """Set the es4 property."""
        self._cards[3].set_value("es4", value)

    @property
    def es5(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress value to EPS5
        """ # nopep8
        return self._cards[3].get_value("es5")

    @es5.setter
    def es5(self, value: float) -> None:
        """Set the es5 property."""
        self._cards[3].set_value("es5", value)

    @property
    def es6(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress value to EPS6
        """ # nopep8
        return self._cards[3].get_value("es6")

    @es6.setter
    def es6(self, value: float) -> None:
        """Set the es6 property."""
        self._cards[3].set_value("es6", value)

    @property
    def es7(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress value to EPS7
        """ # nopep8
        return self._cards[3].get_value("es7")

    @es7.setter
    def es7(self, value: float) -> None:
        """Set the es7 property."""
        self._cards[3].set_value("es7", value)

    @property
    def es8(self) -> typing.Optional[float]:
        """Get or set the Corresponding yield stress value to EPS8
        """ # nopep8
        return self._cards[3].get_value("es8")

    @es8.setter
    def es8(self, value: float) -> None:
        """Set the es8 property."""
        self._cards[3].set_value("es8", value)

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

