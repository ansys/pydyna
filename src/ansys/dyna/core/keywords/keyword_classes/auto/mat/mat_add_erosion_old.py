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

"""Module providing the MatAddErosionOld class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATADDEROSIONOLD_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("excl", float, 10, 10, None),
    FieldSchema("mxpres", float, 20, 10, None),
    FieldSchema("mneps", float, 30, 10, None),
    FieldSchema("effeps", float, 40, 10, None),
    FieldSchema("voleps", float, 50, 10, None),
    FieldSchema("numfip", float, 60, 10, 1.0),
    FieldSchema("ncs", float, 70, 10, 1.0),
)

_MATADDEROSIONOLD_CARD1 = (
    FieldSchema("mnpres", float, 0, 10, None),
    FieldSchema("sigp1", float, 10, 10, None),
    FieldSchema("sigvm", float, 20, 10, None),
    FieldSchema("mxeps", float, 30, 10, None),
    FieldSchema("epssh", float, 40, 10, None),
    FieldSchema("sigth", float, 50, 10, None),
    FieldSchema("impulse", float, 60, 10, None),
    FieldSchema("failtm", float, 70, 10, None),
)

_MATADDEROSIONOLD_CARD2 = (
    FieldSchema("idam", int, 0, 10, None),
    FieldSchema("dmgtyp", float, 10, 10, None),
    FieldSchema("lcsdg", int, 20, 10, None),
    FieldSchema("ecrit", float, 30, 10, None),
    FieldSchema("dmgexp", float, 40, 10, 1.0),
    FieldSchema("dcrit", float, 50, 10, None),
    FieldSchema("fadexp", float, 60, 10, 1.0),
    FieldSchema("lcregd", int, 70, 10, None),
)

_MATADDEROSIONOLD_CARD3 = (
    FieldSchema("sizflg", int, 0, 10, 0),
    FieldSchema("refsz", float, 10, 10, None),
    FieldSchema("nahsv", float, 20, 10, None),
    FieldSchema("lcsrs", int, 30, 10, None),
    FieldSchema("shrf", float, 40, 10, None),
    FieldSchema("biaxf", float, 50, 10, None),
    FieldSchema("lcdlim", float, 60, 10, None),
    FieldSchema("midfail", float, 70, 10, None),
)

_MATADDEROSIONOLD_CARD4 = (
    FieldSchema("dityp", float, 0, 10, 0.0),
    FieldSchema("p1", int, 10, 10, None),
    FieldSchema("p2", float, 20, 10, None),
    FieldSchema("p3", float, 30, 10, None),
)

_MATADDEROSIONOLD_CARD5 = (
    FieldSchema("detyp", float, 0, 10, None),
    FieldSchema("dctyp", float, 10, 10, 0.0),
    FieldSchema("q1", float, 20, 10, None),
    FieldSchema("q2", float, 30, 10, None),
)

_MATADDEROSIONOLD_CARD6 = (
    FieldSchema("lcfld", int, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("epsthin", float, 20, 10, None),
    FieldSchema("engcrt", float, 30, 10, None),
    FieldSchema("radcrt", float, 40, 10, None),
)

_MATADDEROSIONOLD_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatAddErosionOld(KeywordBase):
    """DYNA MAT_ADD_EROSION_OLD keyword"""

    keyword = "MAT"
    subkeyword = "ADD_EROSION_OLD"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "mid": LinkType.MAT,
        "lcsdg": LinkType.DEFINE_CURVE,
        "lcregd": LinkType.DEFINE_CURVE,
        "lcsrs": LinkType.DEFINE_CURVE,
        "lcfld": LinkType.DEFINE_CURVE,
        "p1": LinkType.DEFINE_CURVE_OR_TABLE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatAddErosionOld class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATADDEROSIONOLD_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATADDEROSIONOLD_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATADDEROSIONOLD_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATADDEROSIONOLD_CARD3,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATADDEROSIONOLD_CARD4,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATADDEROSIONOLD_CARD5,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATADDEROSIONOLD_CARD6,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = MatAddErosionOld._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATADDEROSIONOLD_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification for which this erosion definition applies. A unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def excl(self) -> typing.Optional[float]:
        """Get or set the The exclusion number, which applies to the failure values defined on Cards 1, 2, and 7.
        When any of the failure values on these cards are set to the exclusion number,
        the associated failure criterion is not invoked. Or in other words, only the failure values not set to the exclusion number are invoked.
        The default value of EXCL is 0.0, which eliminates all failure criteria from consideration that have their constants left blank or set to 0.0. As an example,
        to prevent a material from developing tensile pressure, the user could specify an unusual value for
        the exclusion number, e.g., 1234, set MNPRES to 0.0, and set all the remaining failure values to 1234.
        However, use of an exclusion number may be considered nonessential since the same effect
        could be achieved without use of the exclusion number by setting MNPRES to a very small negative value.
        """ # nopep8
        return self._cards[0].get_value("excl")

    @excl.setter
    def excl(self, value: float) -> None:
        """Set the excl property."""
        self._cards[0].set_value("excl", value)

    @property
    def mxpres(self) -> typing.Optional[float]:
        """Get or set the Maximum pressure at failure,  . If the value is exactly zero, it is automatically excluded to maintain compatibility with old input files
        """ # nopep8
        return self._cards[0].get_value("mxpres")

    @mxpres.setter
    def mxpres(self, value: float) -> None:
        """Set the mxpres property."""
        self._cards[0].set_value("mxpres", value)

    @property
    def mneps(self) -> typing.Optional[float]:
        """Get or set the Minimum principal strain at failure,  . If the value is exactly zero, it is automatically excluded to maintain compatibility with old input files
        """ # nopep8
        return self._cards[0].get_value("mneps")

    @mneps.setter
    def mneps(self, value: float) -> None:
        """Set the mneps property."""
        self._cards[0].set_value("mneps", value)

    @property
    def effeps(self) -> typing.Optional[float]:
        """Get or set the Maximum effective strain at failure, . If the value is exactly zero, it is automatically excluded to maintain compatibility with old input files
        """ # nopep8
        return self._cards[0].get_value("effeps")

    @effeps.setter
    def effeps(self, value: float) -> None:
        """Set the effeps property."""
        self._cards[0].set_value("effeps", value)

    @property
    def voleps(self) -> typing.Optional[float]:
        """Get or set the Volumetric strain at failure,  . VOLEPS can be a positive or negative number depending on whether the failure is in tension or compression, respectively. If the value is exactly zero, it is automatically excluded to maintain compatibility with old input files
        """ # nopep8
        return self._cards[0].get_value("voleps")

    @voleps.setter
    def voleps(self, value: float) -> None:
        """Set the voleps property."""
        self._cards[0].set_value("voleps", value)

    @property
    def numfip(self) -> float:
        """Get or set the Number or percentage of failed integration points prior to element deletion (default is 1). See Remark 2.
        NUMFIP does not apply to higher order solid element types 24, 25, 26, 27, 28, and 29, rather see the variable VOLFRAC.
        GT.0.0: Number of integration points which must fail before element is deleted.
        LT.0.0: Applies only to shells. "|NUMFIP|" is the percentage of integration points which must exceed the failure criterion before the element fails.
        If NUMFIP < -100, then "|NUMFIP|-100"  is the number of failed integration points prior to element deletion.
        """ # nopep8
        return self._cards[0].get_value("numfip")

    @numfip.setter
    def numfip(self, value: float) -> None:
        """Set the numfip property."""
        self._cards[0].set_value("numfip", value)

    @property
    def ncs(self) -> float:
        """Get or set the Number of failure conditions to satisfy before failure occurs. For example, if SIGP1 and SIGVM are defined and if NCS=2, both failure criteria must be met before element deletion can occur. The default is set to unity.
        """ # nopep8
        return self._cards[0].get_value("ncs")

    @ncs.setter
    def ncs(self, value: float) -> None:
        """Set the ncs property."""
        self._cards[0].set_value("ncs", value)

    @property
    def mnpres(self) -> typing.Optional[float]:
        """Get or set the Minimum pressure at failure,
        """ # nopep8
        return self._cards[1].get_value("mnpres")

    @mnpres.setter
    def mnpres(self, value: float) -> None:
        """Set the mnpres property."""
        self._cards[1].set_value("mnpres", value)

    @property
    def sigp1(self) -> typing.Optional[float]:
        """Get or set the Maximum principal stress at failure, Sigma max.
        LT.0: -SIGP1 is a load curve ID giving the maximum principal stress at failure as a function of the effective strain rate (the curve should not extrapolate to zero or failure may occur at low strain). A filter can be applied to the effective strain rate according to DTEFLT (see Card 5)..
        """ # nopep8
        return self._cards[1].get_value("sigp1")

    @sigp1.setter
    def sigp1(self, value: float) -> None:
        """Set the sigp1 property."""
        self._cards[1].set_value("sigp1", value)

    @property
    def sigvm(self) -> typing.Optional[float]:
        """Get or set the Equivalent stress at failure, _max
        LT.0: -SIGVM is a load curve ID giving the equivalent stress at failure as a function of the effective strain rate(the curve should not extrapolate to zero or failure may occur at low strain).A filter can be applied to the effective strain rate according to DTEFLT(see Card 5).
        """ # nopep8
        return self._cards[1].get_value("sigvm")

    @sigvm.setter
    def sigvm(self, value: float) -> None:
        """Set the sigvm property."""
        self._cards[1].set_value("sigvm", value)

    @property
    def mxeps(self) -> typing.Optional[float]:
        """Get or set the Variable to invoke a failure criterion based on maximum principal strain.
        GT.0: Maximum principal strain at failure, eps_max.
        LT.0: -MXEPS is the ID of a curve giving maximum principal strain at failure as a function of effective strain rate.
        A filter is applied to the effective strain rate according to DTEFLT; see Card 8.
        """ # nopep8
        return self._cards[1].get_value("mxeps")

    @mxeps.setter
    def mxeps(self, value: float) -> None:
        """Set the mxeps property."""
        self._cards[1].set_value("mxeps", value)

    @property
    def epssh(self) -> typing.Optional[float]:
        """Get or set the Tensorial shear strain at failure, r max .
        """ # nopep8
        return self._cards[1].get_value("epssh")

    @epssh.setter
    def epssh(self, value: float) -> None:
        """Set the epssh property."""
        self._cards[1].set_value("epssh", value)

    @property
    def sigth(self) -> typing.Optional[float]:
        """Get or set the Threshold stress, Sigma 0 .
        """ # nopep8
        return self._cards[1].get_value("sigth")

    @sigth.setter
    def sigth(self, value: float) -> None:
        """Set the sigth property."""
        self._cards[1].set_value("sigth", value)

    @property
    def impulse(self) -> typing.Optional[float]:
        """Get or set the Stress impulse for failure, K f.
        """ # nopep8
        return self._cards[1].get_value("impulse")

    @impulse.setter
    def impulse(self, value: float) -> None:
        """Set the impulse property."""
        self._cards[1].set_value("impulse", value)

    @property
    def failtm(self) -> typing.Optional[float]:
        """Get or set the Failure time. When the problem time exceeds the failure time, the material is removed.
        GT.0: Failure time is active during any phase of the analysis.
        LT.0: Failure time is set to |FAILTM| but this criterion in inactive during the dynamic relaxation phase.
        """ # nopep8
        return self._cards[1].get_value("failtm")

    @failtm.setter
    def failtm(self, value: float) -> None:
        """Set the failtm property."""
        self._cards[1].set_value("failtm", value)

    @property
    def idam(self) -> typing.Optional[int]:
        """Get or set the Flag for damage model.
        EQ.0: no damage model is used.
        NE.0: Damage models GISSMO or DIEM, see manuals of R10 and before.
        Still available here for backward compatibility, but description actually moved to new keywords *MAT_ADD_DAMAGE_DIEM/GISSMO
        ,
        """ # nopep8
        return self._cards[2].get_value("idam")

    @idam.setter
    def idam(self, value: int) -> None:
        """Set the idam property."""
        self._cards[2].set_value("idam", value)

    @property
    def dmgtyp(self) -> typing.Optional[float]:
        """Get or set the For GISSMO damage type the following applies.
        EQ.0: Damage is accumulated, no coupling to flow stress, no failure.
        EQ.1: Damage is accumulated, element failure occurs for D=1. Coupling of damage to flow stress depending on parameters, see remarks below.
        For IDAM.LT.0 the following applies.
        EQ.0: No action is taken
        EQ.1: Damage history is initiated based on values of initial plastic strains and initial strain tensor, this is to be used in multistage analyses
        when damage history is unavailable from previous steps. This relies on having a zero initial stress and a non-zero initial strain state .
        """ # nopep8
        return self._cards[2].get_value("dmgtyp")

    @dmgtyp.setter
    def dmgtyp(self, value: float) -> None:
        """Set the dmgtyp property."""
        self._cards[2].set_value("dmgtyp", value)

    @property
    def lcsdg(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID. The load curve ID defines equivalent plastic
        strain to failure vs. triaxiality. The table ID defines for each Lode
        parameter (values between -1 and 1) a load curve ID giving the
        equivalent plastic strain to failure vs. triaxiality for that Lode parameter
        """ # nopep8
        return self._cards[2].get_value("lcsdg")

    @lcsdg.setter
    def lcsdg(self, value: int) -> None:
        """Set the lcsdg property."""
        self._cards[2].set_value("lcsdg", value)

    @property
    def ecrit(self) -> typing.Optional[float]:
        """Get or set the Critical plastic strain (material instability), see below.
        LT.0.0: |ECRIT| is load curve ID defining critical equivalent plastic strain vs. triaxiality.
        EQ.0.0: Fixed value DCRIT defining critical damage is read (see below)
        GT.0.0: Fixed value for stress-state independent critical equivalent plastic strain
        """ # nopep8
        return self._cards[2].get_value("ecrit")

    @ecrit.setter
    def ecrit(self, value: float) -> None:
        """Set the ecrit property."""
        self._cards[2].set_value("ecrit", value)

    @property
    def dmgexp(self) -> float:
        """Get or set the Exponent for nonlinear damage accumulation .
        """ # nopep8
        return self._cards[2].get_value("dmgexp")

    @dmgexp.setter
    def dmgexp(self, value: float) -> None:
        """Set the dmgexp property."""
        self._cards[2].set_value("dmgexp", value)

    @property
    def dcrit(self) -> typing.Optional[float]:
        """Get or set the Damage threshold value (critical damage). If a Load curve of critical plastic strain or fixed value is given by ECRIT,  input is ignored.
        """ # nopep8
        return self._cards[2].get_value("dcrit")

    @dcrit.setter
    def dcrit(self, value: float) -> None:
        """Set the dcrit property."""
        self._cards[2].set_value("dcrit", value)

    @property
    def fadexp(self) -> float:
        """Get or set the Exponent for damage-related stress fadeout.
        LT.0.0: |FADEXP| is load curve ID defining element-size dependent fading exponent.
        GT.0.0: Constant fading exponent.
        """ # nopep8
        return self._cards[2].get_value("fadexp")

    @fadexp.setter
    def fadexp(self, value: float) -> None:
        """Set the fadexp property."""
        self._cards[2].set_value("fadexp", value)

    @property
    def lcregd(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining element size dependent regularization factors for equivalent plastic strain to failure.
        """ # nopep8
        return self._cards[2].get_value("lcregd")

    @lcregd.setter
    def lcregd(self, value: int) -> None:
        """Set the lcregd property."""
        self._cards[2].set_value("lcregd", value)

    @property
    def sizflg(self) -> int:
        """Get or set the Flag for method of element size determination.
        EQ.0 (default): Element size is determined in undeformed configuration as square root of element area (shells), or cubic root of element volume (solids), respectively.
        EQ.1: Element size is updated every time step, and determined as mean edge length. (This option was added to ensure comparability with *MAT_120, and is not recommended for general purpose)
        """ # nopep8
        return self._cards[3].get_value("sizflg")

    @sizflg.setter
    def sizflg(self, value: int) -> None:
        """Set the sizflg property."""
        if value not in [0, 1, None]:
            raise Exception("""sizflg must be `None` or one of {0,1}.""")
        self._cards[3].set_value("sizflg", value)

    @property
    def refsz(self) -> typing.Optional[float]:
        """Get or set the Reference element size, for which an additional output of damage will be generated. This is necessary to ensure the applicability of resulting damage quantities when transferred to different mesh sizes .
        """ # nopep8
        return self._cards[3].get_value("refsz")

    @refsz.setter
    def refsz(self, value: float) -> None:
        """Set the refsz property."""
        self._cards[3].set_value("refsz", value)

    @property
    def nahsv(self) -> typing.Optional[float]:
        """Get or set the Number of history variables from damage model which should be stored in standard material history array for Postprocessing
        """ # nopep8
        return self._cards[3].get_value("nahsv")

    @nahsv.setter
    def nahsv(self, value: float) -> None:
        """Set the nahsv property."""
        self._cards[3].set_value("nahsv", value)

    @property
    def lcsrs(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining strain rate dependent scaling factors for equivalent plastic strain to failure
        """ # nopep8
        return self._cards[3].get_value("lcsrs")

    @lcsrs.setter
    def lcsrs(self, value: int) -> None:
        """Set the lcsrs property."""
        self._cards[3].set_value("lcsrs", value)

    @property
    def shrf(self) -> typing.Optional[float]:
        """Get or set the Scaling factor for regularization factor at triaxiality = 0
        """ # nopep8
        return self._cards[3].get_value("shrf")

    @shrf.setter
    def shrf(self, value: float) -> None:
        """Set the shrf property."""
        self._cards[3].set_value("shrf", value)

    @property
    def biaxf(self) -> typing.Optional[float]:
        """Get or set the Scaling factor for regularization factor at triaxiality = 2/3.
        """ # nopep8
        return self._cards[3].get_value("biaxf")

    @biaxf.setter
    def biaxf(self, value: float) -> None:
        """Set the biaxf property."""
        self._cards[3].set_value("biaxf", value)

    @property
    def lcdlim(self) -> typing.Optional[float]:
        """Get or set the Load curve ID defining damage limit values as a function of triaxiality. Damage can be restricted to values less than 1.0 to prevent further stress reduction and failure for certain triaxialities
        """ # nopep8
        return self._cards[3].get_value("lcdlim")

    @lcdlim.setter
    def lcdlim(self, value: float) -> None:
        """Set the lcdlim property."""
        self._cards[3].set_value("lcdlim", value)

    @property
    def midfail(self) -> typing.Optional[float]:
        """Get or set the Mid-plane failure option for shell elements and GISSMO. If active, then critical strain is only checked at the mid-plane integration point, i.e., an odd number for NIP should be used.
        The other integration points compute their damage, but no coupling to the stresses is done first.
        As soon as the mid-plane IP reaches ECRIT/DCRIT, then all the other IP's are also checked.
        Those of them that are already above their critical value immediately start to reduce the stresses.
        Those who are still below critical still do not couple, only if they reach their criterion.
        EQ.0.0: Inactive,
        EQ.1.0: Active.. Those of the non-mid-plane IPs that are already above their critical value immediately start to reduce the stresses. Those still below the critical value still do not couple, only if they reach their criterion.
        EQ.2.0: active.All of the non - mid - plane IPs immediately start to reduce the stresses.NUMFIP is active.
        EQ.3.0: active.Same as 2 but when D = 1 is reached in the middle integration point, the element is eroded instantaneously.NUMFIP is disregarded.
        EQ.4.0: active.Damage and failure is applied only on the midpoint.When D = 1 on the midpoint, the element is eroded.NUMFIP is disregarded.Integration points away from the midplane see no stress reduction and no failure
        """ # nopep8
        return self._cards[3].get_value("midfail")

    @midfail.setter
    def midfail(self, value: float) -> None:
        """Set the midfail property."""
        self._cards[3].set_value("midfail", value)

    @property
    def dityp(self) -> float:
        """Get or set the Damage initiation type
        EQ.0.0: Ductile based on stress triaxiality
        EQ.1.0: Shear
        EQ.2.0: MSFLD,
        EQ.3.0: FLD
        EQ.4.0: Ductile based on normalized principal stress
        """ # nopep8
        return self._cards[4].get_value("dityp")

    @dityp.setter
    def dityp(self, value: float) -> None:
        """Set the dityp property."""
        if value not in [0.0, 1.0, 2.0, None]:
            raise Exception("""dityp must be `None` or one of {0.0,1.0,2.0}.""")
        self._cards[4].set_value("dityp", value)

    @property
    def p1(self) -> typing.Optional[int]:
        """Get or set the Damage initiation parameter
        DITYP.EQ.0.0: Load curve/table ID representing plastic strain at onset of damage as function of stress triaxiality and optionally plastic strain rate.
        DITYP.EQ.1.0: Load curve/table ID representing plastic strain at onset of damage as function of shear influence and optionally plastic strain rate.
        DITYP.EQ.2.0: Load curve/table ID representing plastic strain at onset of damage as function of ratio of principal plastic strain rates and optionally plastic strain rate .
        DITYP.EQ.3.0:Load curve/table ID representing plastic strain at onset of damage as function of ratio of principal plastic strain rates and optionally plastic strain rate.
        DITYP.EQ.4.0:Load curve/table ID representing plastic strain at onset of damage as function of stress state parameter and optionally plastic strain rate.
        """ # nopep8
        return self._cards[4].get_value("p1")

    @p1.setter
    def p1(self, value: int) -> None:
        """Set the p1 property."""
        self._cards[4].set_value("p1", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the Damage initiation parameter
        DITYP.EQ.0.0: Not used
        DITYP.EQ.1.0: Pressure influence coefficient kS
        DITYP.EQ.2.0:DITYP.EQ.2.0: Layer specification
        EQ.0: Mid layer
        EQ.1: Outer layerNot used
        DITYP.EQ.3.0: Layer specification
        EQ.0: Mid layer
        EQ.1: Outer layer
        DITYP.EQ.4.0: Triaxiality influence parameter k_d
        """ # nopep8
        return self._cards[4].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        """Set the p2 property."""
        self._cards[4].set_value("p2", value)

    @property
    def p3(self) -> typing.Optional[float]:
        """Get or set the Damage initiation parameter
        DITYP.EQ.0.0: Not used
        DITYP.EQ.1.0: Not used
        DITYP.EQ.2.0: Initiation formulation
        EQ.0: Direct
        EQ.1: Incremental
        DITYP.EQ.3.0: Initiation formulation
        EQ.0: Direct
        EQ.1: Incremental
        DITYP.EQ.4.0: Not used
        """ # nopep8
        return self._cards[4].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        """Set the p3 property."""
        self._cards[4].set_value("p3", value)

    @property
    def detyp(self) -> typing.Optional[float]:
        """Get or set the Damage evolution type
        EQ.0.0: Linear,
        """ # nopep8
        return self._cards[5].get_value("detyp")

    @detyp.setter
    def detyp(self, value: float) -> None:
        """Set the detyp property."""
        self._cards[5].set_value("detyp", value)

    @property
    def dctyp(self) -> float:
        """Get or set the Damage composition option for multiple criteria
        EQ.0.0: Maximum
        EQ.1.0: Multiplicative .E
        Q.-1.0: Damage not coupled to stress
        """ # nopep8
        return self._cards[5].get_value("dctyp")

    @dctyp.setter
    def dctyp(self, value: float) -> None:
        """Set the dctyp property."""
        if value not in [0.0, 1.0, -1.0, None]:
            raise Exception("""dctyp must be `None` or one of {0.0,1.0,-1.0}.""")
        self._cards[5].set_value("dctyp", value)

    @property
    def q1(self) -> typing.Optional[float]:
        """Get or set the Damage evolution parameter
        DETYP.EQ.0.0: Plastic displacement at failure
        """ # nopep8
        return self._cards[5].get_value("q1")

    @q1.setter
    def q1(self, value: float) -> None:
        """Set the q1 property."""
        self._cards[5].set_value("q1", value)

    @property
    def q2(self) -> typing.Optional[float]:
        """Get or set the Set to 1.0 to output information to log files (messag and d3hsp) when an integration point fails.
        """ # nopep8
        return self._cards[5].get_value("q2")

    @q2.setter
    def q2(self, value: float) -> None:
        """Set the q2 property."""
        self._cards[5].set_value("q2", value)

    @property
    def lcfld(self) -> typing.Optional[int]:
        """Get or set the Load curve ID or Table ID. Load curve defines the Forming Limit Diagram, where minor engineering strains in percent are defined as abscissa values and major engineering strains in percent are defined as ordinate values. Table defines for each strain rate an associated FLD curve. In defining the curve, list pairs of minor and major strains starting with the left most point and ending with the right most point. This criterion is only available for shell elements.
        """ # nopep8
        return self._cards[6].get_value("lcfld")

    @lcfld.setter
    def lcfld(self, value: int) -> None:
        """Set the lcfld property."""
        self._cards[6].set_value("lcfld", value)

    @property
    def epsthin(self) -> typing.Optional[float]:
        """Get or set the Thinning strain at failure for thin and thick shells
        """ # nopep8
        return self._cards[6].get_value("epsthin")

    @epsthin.setter
    def epsthin(self, value: float) -> None:
        """Set the epsthin property."""
        self._cards[6].set_value("epsthin", value)

    @property
    def engcrt(self) -> typing.Optional[float]:
        """Get or set the Critical energy for nonlocal failure criterion
        """ # nopep8
        return self._cards[6].get_value("engcrt")

    @engcrt.setter
    def engcrt(self, value: float) -> None:
        """Set the engcrt property."""
        self._cards[6].set_value("engcrt", value)

    @property
    def radcrt(self) -> typing.Optional[float]:
        """Get or set the Critical radius for nonlocal failure criterion
        """ # nopep8
        return self._cards[6].get_value("radcrt")

    @radcrt.setter
    def radcrt(self, value: float) -> None:
        """Set the radcrt property."""
        self._cards[6].set_value("radcrt", value)

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
    def mid_link(self) -> typing.Optional[KeywordBase]:
        """Get the MAT_* keyword for mid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_type("MAT"):
            if kwd.mid == self.mid:
                return kwd
        return None

    @mid_link.setter
    def mid_link(self, value: KeywordBase) -> None:
        """Set the MAT_* keyword for mid."""
        self.mid = value.mid

    @property
    def lcsdg_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcsdg."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcsdg:
                return kwd
        return None

    @lcsdg_link.setter
    def lcsdg_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcsdg."""
        self.lcsdg = value.lcid

    @property
    def lcregd_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcregd."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcregd:
                return kwd
        return None

    @lcregd_link.setter
    def lcregd_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcregd."""
        self.lcregd = value.lcid

    @property
    def lcsrs_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcsrs."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcsrs:
                return kwd
        return None

    @lcsrs_link.setter
    def lcsrs_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcsrs."""
        self.lcsrs = value.lcid

    @property
    def lcfld_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcfld."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcfld:
                return kwd
        return None

    @lcfld_link.setter
    def lcfld_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcfld."""
        self.lcfld = value.lcid

    @property
    def p1_link(self) -> typing.Optional[KeywordBase]:
        """Get the linked DEFINE_CURVE or DEFINE_TABLE for p1."""
        if self.deck is None:
            return None
        field_value = self.p1
        if field_value is None or field_value == 0:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == field_value:
                return kwd
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "TABLE"):
            if kwd.tbid == field_value:
                return kwd
        return None

    @p1_link.setter
    def p1_link(self, value: KeywordBase) -> None:
        """Set the linked keyword for p1."""
        if hasattr(value, "lcid"):
            self.p1 = value.lcid
        elif hasattr(value, "tbid"):
            self.p1 = value.tbid

