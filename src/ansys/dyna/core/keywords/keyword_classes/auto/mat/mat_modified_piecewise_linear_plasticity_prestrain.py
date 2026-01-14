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

"""Module providing the MatModifiedPiecewiseLinearPlasticityPrestrain class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATMODIFIEDPIECEWISELINEARPLASTICITYPRESTRAIN_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("sigy", float, 40, 10, None),
    FieldSchema("etan", float, 50, 10, None),
    FieldSchema("fail", float, 60, 10, 1e+21),
    FieldSchema("tdel", float, 70, 10, None),
)

_MATMODIFIEDPIECEWISELINEARPLASTICITYPRESTRAIN_CARD1 = (
    FieldSchema("c", float, 0, 10, None),
    FieldSchema("p", float, 10, 10, None),
    FieldSchema("lcss", int, 20, 10, 0),
    FieldSchema("lcsr", int, 30, 10, 0),
    FieldSchema("vp", float, 40, 10, 0.0),
    FieldSchema("epsthin", float, 50, 10, None),
    FieldSchema("epsmaj", float, 60, 10, None),
    FieldSchema("numint", float, 70, 10, 0.0),
)

_MATMODIFIEDPIECEWISELINEARPLASTICITYPRESTRAIN_CARD2 = (
    FieldSchema("eps1", float, 0, 10, None),
    FieldSchema("eps2", float, 10, 10, None),
    FieldSchema("eps3", float, 20, 10, None),
    FieldSchema("eps4", float, 30, 10, None),
    FieldSchema("eps5", float, 40, 10, None),
    FieldSchema("eps6", float, 50, 10, None),
    FieldSchema("eps7", float, 60, 10, None),
    FieldSchema("eps8", float, 70, 10, None),
)

_MATMODIFIEDPIECEWISELINEARPLASTICITYPRESTRAIN_CARD3 = (
    FieldSchema("es1", float, 0, 10, None),
    FieldSchema("es2", float, 10, 10, None),
    FieldSchema("es3", float, 20, 10, None),
    FieldSchema("es4", float, 30, 10, None),
    FieldSchema("es5", float, 40, 10, None),
    FieldSchema("es6", float, 50, 10, None),
    FieldSchema("es7", float, 60, 10, None),
    FieldSchema("es8", float, 70, 10, None),
)

_MATMODIFIEDPIECEWISELINEARPLASTICITYPRESTRAIN_CARD4 = (
    FieldSchema("lctsrf", int, 0, 10, 0),
    FieldSchema("eps0", float, 10, 10, 0.0),
    FieldSchema("triax", float, 20, 10, 0.0),
    FieldSchema("ips", int, 30, 10, 0),
    FieldSchema("lcemod", int, 40, 10, 0),
    FieldSchema("beta", float, 50, 10, 0.0),
    FieldSchema("rfiltf", float, 60, 10, 0.0),
)

class MatModifiedPiecewiseLinearPlasticityPrestrain(KeywordBase):
    """DYNA MAT_MODIFIED_PIECEWISE_LINEAR_PLASTICITY_PRESTRAIN keyword"""

    keyword = "MAT"
    subkeyword = "MODIFIED_PIECEWISE_LINEAR_PLASTICITY_PRESTRAIN"
    _link_fields = {
        "lcss": LinkType.DEFINE_CURVE,
        "lcsr": LinkType.DEFINE_CURVE,
        "lctsrf": LinkType.DEFINE_CURVE,
        "lcemod": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatModifiedPiecewiseLinearPlasticityPrestrain class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATMODIFIEDPIECEWISELINEARPLASTICITYPRESTRAIN_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATMODIFIEDPIECEWISELINEARPLASTICITYPRESTRAIN_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATMODIFIEDPIECEWISELINEARPLASTICITYPRESTRAIN_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATMODIFIEDPIECEWISELINEARPLASTICITYPRESTRAIN_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATMODIFIEDPIECEWISELINEARPLASTICITYPRESTRAIN_CARD4,
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
        The table ID defines for each strain rate value a load curve ID giving the stress versus effective plastic strain for that rate. The stress versus effective plastic strain curve for the lowest value of strain rate is used if the strain rate falls below the minmimum value. Likewise, the stress versus effective plastic strain curve for the highest value of strain rate is used if the strain rate exceeds the maximum value. If defined C, P,curve ID, LCSR, EPS1-EPS8 and ES1-ES8 are ignored.
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
        """Get or set the Formulation for rate effects (currently not used with this model).
        """ # nopep8
        return self._cards[1].get_value("vp")

    @vp.setter
    def vp(self, value: float) -> None:
        """Set the vp property."""
        if value not in [0.0, 1.0, None]:
            raise Exception("""vp must be `None` or one of {0.0,1.0}.""")
        self._cards[1].set_value("vp", value)

    @property
    def epsthin(self) -> typing.Optional[float]:
        """Get or set the Thinning plastic strain at failure. This number should be given as positive number
        """ # nopep8
        return self._cards[1].get_value("epsthin")

    @epsthin.setter
    def epsthin(self, value: float) -> None:
        """Set the epsthin property."""
        self._cards[1].set_value("epsthin", value)

    @property
    def epsmaj(self) -> typing.Optional[float]:
        """Get or set the Major in plane strain at failure.
        """ # nopep8
        return self._cards[1].get_value("epsmaj")

    @epsmaj.setter
    def epsmaj(self, value: float) -> None:
        """Set the epsmaj property."""
        self._cards[1].set_value("epsmaj", value)

    @property
    def numint(self) -> float:
        """Get or set the No. of through thickness integration points which must fail before the element is deleted.(if zero, all points must fail)
        """ # nopep8
        return self._cards[1].get_value("numint")

    @numint.setter
    def numint(self, value: float) -> None:
        """Set the numint property."""
        self._cards[1].set_value("numint", value)

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
    def lctsrf(self) -> int:
        """Get or set the Load curve that defines the thinning plastic strain at failure as a function of the plastic strain rate
        """ # nopep8
        return self._cards[4].get_value("lctsrf")

    @lctsrf.setter
    def lctsrf(self, value: int) -> None:
        """Set the lctsrf property."""
        self._cards[4].set_value("lctsrf", value)

    @property
    def eps0(self) -> float:
        """Get or set the EPS0 parameter for RTCL damage.
        EQ.0.0: (default) RTCL damage is inactive.
        GT.0.0: RTCL damage is active
        """ # nopep8
        return self._cards[4].get_value("eps0")

    @eps0.setter
    def eps0(self, value: float) -> None:
        """Set the eps0 property."""
        self._cards[4].set_value("eps0", value)

    @property
    def triax(self) -> float:
        """Get or set the RTCL damage triaxiality limit.
        EQ.0.0: (default) No limit.
        GT.0.0: Damage does not accumulate when triaxiality exceeds TRIAX.
        """ # nopep8
        return self._cards[4].get_value("triax")

    @triax.setter
    def triax(self, value: float) -> None:
        """Set the triax property."""
        self._cards[4].set_value("triax", value)

    @property
    def ips(self) -> int:
        """Get or set the Flag to add prestrain when checking for major strain failure (see EPSMAJ above on Card 2) for the PRESTRAIN keyword option:
        EQ.0:	No prestrain added(default)
        EQ.1 : Initial strain set with * INITIAL_STRAIN_SHELL will be used as a prestrain when checking for major strain failure(VP = 0 and shells only).
        """ # nopep8
        return self._cards[4].get_value("ips")

    @ips.setter
    def ips(self, value: int) -> None:
        """Set the ips property."""
        if value not in [0, 1, None]:
            raise Exception("""ips must be `None` or one of {0,1}.""")
        self._cards[4].set_value("ips", value)

    @property
    def lcemod(self) -> int:
        """Get or set the Load curve ID defining Young’s modulus as function of effective strain rate. LCEMOD ≠ 0 activates viscoelasticity. See *MAT_187L for details. The parameters BETA and RFILTF have to be defined too.
        (If LCEMOD ≠ 0 is used, VP = 1 should be defined and failure options EPSTHIN, EPSMAJ,and RTCL are currently not available.)
        """ # nopep8
        return self._cards[4].get_value("lcemod")

    @lcemod.setter
    def lcemod(self, value: int) -> None:
        """Set the lcemod property."""
        self._cards[4].set_value("lcemod", value)

    @property
    def beta(self) -> float:
        """Get or set the Decay constant in viscoelastic law. BETA has the unit [1/time]. If LCEMOD > 0 is used, a non-zero value for BETA is mandatory.
        """ # nopep8
        return self._cards[4].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[4].set_value("beta", value)

    @property
    def rfiltf(self) -> float:
        """Get or set the Smoothing factor on the effective strain rate (default is 0.95). The filtered strain rate is used for the viscoelasticity (LCEMOD > 0).
        """ # nopep8
        return self._cards[4].get_value("rfiltf")

    @rfiltf.setter
    def rfiltf(self, value: float) -> None:
        """Set the rfiltf property."""
        self._cards[4].set_value("rfiltf", value)

    @property
    def lcss_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcss."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcss:
                return kwd
        return None

    @lcss_link.setter
    def lcss_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcss."""
        self.lcss = value.lcid

    @property
    def lcsr_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcsr."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcsr:
                return kwd
        return None

    @lcsr_link.setter
    def lcsr_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcsr."""
        self.lcsr = value.lcid

    @property
    def lctsrf_link(self) -> DefineCurve:
        """Get the DefineCurve object for lctsrf."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lctsrf:
                return kwd
        return None

    @lctsrf_link.setter
    def lctsrf_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lctsrf."""
        self.lctsrf = value.lcid

    @property
    def lcemod_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcemod."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcemod:
                return kwd
        return None

    @lcemod_link.setter
    def lcemod_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcemod."""
        self.lcemod = value.lcid

