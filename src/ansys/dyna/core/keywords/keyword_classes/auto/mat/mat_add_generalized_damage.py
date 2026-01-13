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

"""Module providing the MatAddGeneralizedDamage class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATADDGENERALIZEDDAMAGE_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("idam", int, 10, 10, 0),
    FieldSchema("dtyp", int, 20, 10, 0),
    FieldSchema("refsz", float, 30, 10, None),
    FieldSchema("numfip", float, 40, 10, 1.0),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("pddt", int, 60, 10, 0),
    FieldSchema("nhis", int, 70, 10, 1),
)

_MATADDGENERALIZEDDAMAGE_CARD1 = (
    FieldSchema("his1", int, 0, 10, 0),
    FieldSchema("his2", int, 10, 10, None),
    FieldSchema("his3", int, 20, 10, None),
    FieldSchema("iflg1", int, 30, 10, 0),
    FieldSchema("iflg2", int, 40, 10, 0),
    FieldSchema("iflg3", int, 50, 10, 0),
)

_MATADDGENERALIZEDDAMAGE_CARD2 = (
    FieldSchema("d11", int, 0, 10, None),
    FieldSchema("d22", int, 10, 10, None),
    FieldSchema("d33", int, 20, 10, None),
    FieldSchema("d44", int, 30, 10, None),
    FieldSchema("d55", int, 40, 10, None),
    FieldSchema("d66", int, 50, 10, None),
)

_MATADDGENERALIZEDDAMAGE_CARD3 = (
    FieldSchema("d12", int, 0, 10, None),
    FieldSchema("d21", int, 10, 10, None),
    FieldSchema("d24", int, 20, 10, None),
    FieldSchema("d42", int, 30, 10, None),
    FieldSchema("d14", int, 40, 10, None),
    FieldSchema("d41", int, 50, 10, None),
)

_MATADDGENERALIZEDDAMAGE_CARD4 = (
    FieldSchema("lcsdg", int, 0, 10, 0),
    FieldSchema("ecrit", float, 10, 10, None),
    FieldSchema("dmgexp", float, 20, 10, 1.0),
    FieldSchema("dcrit", float, 30, 10, None),
    FieldSchema("fadexp", float, 40, 10, 1.0),
    FieldSchema("lcreg", int, 50, 10, 0),
)

_MATADDGENERALIZEDDAMAGE_CARD5 = (
    FieldSchema("lcsrs", int, 0, 10, 0),
    FieldSchema("shrf", float, 10, 10, None),
    FieldSchema("biaxf", float, 20, 10, None),
    FieldSchema("lcdlim", int, 30, 10, 0),
    FieldSchema("midfail", float, 40, 10, 0.0),
    FieldSchema("nfloc", float, 50, 10, None),
)

_MATADDGENERALIZEDDAMAGE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatAddGeneralizedDamage(KeywordBase):
    """DYNA MAT_ADD_GENERALIZED_DAMAGE keyword"""

    keyword = "MAT"
    subkeyword = "ADD_GENERALIZED_DAMAGE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcsdg": LinkType.DEFINE_CURVE,
        "lcreg": LinkType.DEFINE_CURVE,
        "lcsrs": LinkType.DEFINE_CURVE,
        "lcdlim": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatAddGeneralizedDamage class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATADDGENERALIZEDDAMAGE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATADDGENERALIZEDDAMAGE_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATADDGENERALIZEDDAMAGE_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATADDGENERALIZEDDAMAGE_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATADDGENERALIZEDDAMAGE_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATADDGENERALIZEDDAMAGE_CARD5,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatAddGeneralizedDamage.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATADDGENERALIZEDDAMAGE_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material ID for which this generalized damage definition applies.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def idam(self) -> int:
        """Get or set the Flag for damage model.
        EQ.0: no damage model is used.
        EQ.1: GISSMO damage model..
        """ # nopep8
        return self._cards[0].get_value("idam")

    @idam.setter
    def idam(self, value: int) -> None:
        """Set the idam property."""
        if value not in [0, 1, None]:
            raise Exception("""idam must be `None` or one of {0,1}.""")
        self._cards[0].set_value("idam", value)

    @property
    def dtyp(self) -> int:
        """Get or set the Flag for damage behavior.
        EQ.0: Damage is accumulated, no coupling to flow stress, no failure.
        EQ.1: Damage is accumulated, element failure occurs for D = 1..
        """ # nopep8
        return self._cards[0].get_value("dtyp")

    @dtyp.setter
    def dtyp(self, value: int) -> None:
        """Set the dtyp property."""
        if value not in [0, 1, None]:
            raise Exception("""dtyp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("dtyp", value)

    @property
    def refsz(self) -> typing.Optional[float]:
        """Get or set the Reference element size, for which an additional output of damage
        will be generated. This is necessary to ensure the applicability of
        resulting damage quantities when transferred to different mesh sizes.
        """ # nopep8
        return self._cards[0].get_value("refsz")

    @refsz.setter
    def refsz(self, value: float) -> None:
        """Set the refsz property."""
        self._cards[0].set_value("refsz", value)

    @property
    def numfip(self) -> float:
        """Get or set the Number of failed integration points prior to element deletion. The default is unity.
        LT.0: |NUMFIP| is the percentage of layers which must fail before element fails..
        """ # nopep8
        return self._cards[0].get_value("numfip")

    @numfip.setter
    def numfip(self, value: float) -> None:
        """Set the numfip property."""
        self._cards[0].set_value("numfip", value)

    @property
    def pddt(self) -> int:
        """Get or set the Pre-defined damage tensors. If non-zero, damage tensor coefficients D11 to D66 on cards 3 and 4 will be ignored.See remarks for details.
        EQ.0:	No pre-defined damage tensor is used.
        EQ.1:	Isotropic damage tensor.
        EQ.2:	2-parameter isotropic damage tensor for volumetric-deviatoric split.
        EQ.3:	Anisotropic damage tensor as in MAT_104 (FLAG = -1).
        EQ.4:	3-parameter damage tensor associated with IFLG1=2.
        """ # nopep8
        return self._cards[0].get_value("pddt")

    @pddt.setter
    def pddt(self, value: int) -> None:
        """Set the pddt property."""
        if value not in [0, 1, 2, 3, 4, None]:
            raise Exception("""pddt must be `None` or one of {0,1,2,3,4}.""")
        self._cards[0].set_value("pddt", value)

    @property
    def nhis(self) -> int:
        """Get or set the Number of history variables as driving quantities (min = 1, max = 3).
        """ # nopep8
        return self._cards[0].get_value("nhis")

    @nhis.setter
    def nhis(self, value: int) -> None:
        """Set the nhis property."""
        self._cards[0].set_value("nhis", value)

    @property
    def his1(self) -> int:
        """Get or set the Choice of variable as driving quantity for damage, called "history value" in the following.
        EQ.0.0: Equivalent plastic strain rate is the driving quantity for
        the damage if IFLG1 = 0. Alternatively if IFLG1 = 1,
        components of the plastic strain rate tensor are driving quantities for damage (see remarks).
        GT.0.0: The rate of the additional history variable HISn is the
        driving quantity for damage. IFLG1 should be set to 0.
        LT.0.0: *DEFINE_FUNCTION IDs defining the damage driving
        quantities as a function of the components of the plastic strain rate tensor, IFLG1 should be set to 1.
        """ # nopep8
        return self._cards[1].get_value("his1")

    @his1.setter
    def his1(self, value: int) -> None:
        """Set the his1 property."""
        self._cards[1].set_value("his1", value)

    @property
    def his2(self) -> typing.Optional[int]:
        """Get or set the Choice of variable as driving quantity for damage, called "history value" in the following.
        EQ.0.0: Equivalent plastic strain rate is the driving quantity for
        the damage if IFLG1 = 0. Alternatively if IFLG1 = 1,
        components of the plastic strain rate tensor are driving quantities for damage (see remarks).
        GT.0.0: The rate of the additional history variable HISn is the
        driving quantity for damage. IFLG1 should be set to 0.
        LT.0.0: *DEFINE_FUNCTION IDs defining the damage driving
        quantities as a function of the components of the plastic strain rate tensor, IFLG1 should be set to 1.
        """ # nopep8
        return self._cards[1].get_value("his2")

    @his2.setter
    def his2(self, value: int) -> None:
        """Set the his2 property."""
        self._cards[1].set_value("his2", value)

    @property
    def his3(self) -> typing.Optional[int]:
        """Get or set the Choice of variable as driving quantity for damage, called "history value" in the following.
        EQ.0.0: Equivalent plastic strain rate is the driving quantity for
        the damage if IFLG1 = 0. Alternatively if IFLG1 = 1,
        components of the plastic strain rate tensor are driving quantities for damage (see remarks).
        GT.0.0: The rate of the additional history variable HISn is the
        driving quantity for damage. IFLG1 should be set to 0.
        LT.0.0: *DEFINE_FUNCTION IDs defining the damage driving
        quantities as a function of the components of the plastic strain rate tensor, IFLG1 should be set to 1.
        """ # nopep8
        return self._cards[1].get_value("his3")

    @his3.setter
    def his3(self, value: int) -> None:
        """Set the his3 property."""
        self._cards[1].set_value("his3", value)

    @property
    def iflg1(self) -> int:
        """Get or set the Damage driving quantities
        EQ.0.0: Rates of history variables HISn.
        EQ.1.0: Specific components of the plastic strain rate tensor, see remarks for details.
        EQ.2.0: Predefined functions of plastic strain rate components for
        orthotropic damage model, HISn inputs will be ignored, IFLG2 should be set to 1..
        """ # nopep8
        return self._cards[1].get_value("iflg1")

    @iflg1.setter
    def iflg1(self, value: int) -> None:
        """Set the iflg1 property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""iflg1 must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("iflg1", value)

    @property
    def iflg2(self) -> int:
        """Get or set the Damage strain coordinate system
        EQ.0.0: Local element system.
        EQ.1.0: Material system, only applicable for non-isotropic material models.Supported models for shell elements: all materials with AOPT feature. Supported models for solid elements: 22, 33, 41-50, 103, 122, 133, 157, 199, 233.
        EQ.2.0: Principal strain system (rotating).
        EQ.3.0: Principal strain system (fixed when instability/coupling starts)..
        """ # nopep8
        return self._cards[1].get_value("iflg2")

    @iflg2.setter
    def iflg2(self, value: int) -> None:
        """Set the iflg2 property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""iflg2 must be `None` or one of {0,1,2,3}.""")
        self._cards[1].set_value("iflg2", value)

    @property
    def iflg3(self) -> int:
        """Get or set the Erosion criteria and damage coupling system
        EQ.0.0: Erosion occurs when one of the damage parameters
        computer reaches unity, the damage tensor components
        are based on the individual damage parameters d1 to d3.
        EQ.1.0: Erosion occurs when a single damage parameter D
        reaches unity, the damage tensor components are based	on this single damage parameter.
        """ # nopep8
        return self._cards[1].get_value("iflg3")

    @iflg3.setter
    def iflg3(self, value: int) -> None:
        """Set the iflg3 property."""
        if value not in [0, 1, None]:
            raise Exception("""iflg3 must be `None` or one of {0,1}.""")
        self._cards[1].set_value("iflg3", value)

    @property
    def d11(self) -> typing.Optional[int]:
        """Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
        """ # nopep8
        return self._cards[2].get_value("d11")

    @d11.setter
    def d11(self, value: int) -> None:
        """Set the d11 property."""
        self._cards[2].set_value("d11", value)

    @property
    def d22(self) -> typing.Optional[int]:
        """Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
        """ # nopep8
        return self._cards[2].get_value("d22")

    @d22.setter
    def d22(self, value: int) -> None:
        """Set the d22 property."""
        self._cards[2].set_value("d22", value)

    @property
    def d33(self) -> typing.Optional[int]:
        """Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
        """ # nopep8
        return self._cards[2].get_value("d33")

    @d33.setter
    def d33(self, value: int) -> None:
        """Set the d33 property."""
        self._cards[2].set_value("d33", value)

    @property
    def d44(self) -> typing.Optional[int]:
        """Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
        """ # nopep8
        return self._cards[2].get_value("d44")

    @d44.setter
    def d44(self, value: int) -> None:
        """Set the d44 property."""
        self._cards[2].set_value("d44", value)

    @property
    def d55(self) -> typing.Optional[int]:
        """Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
        """ # nopep8
        return self._cards[2].get_value("d55")

    @d55.setter
    def d55(self, value: int) -> None:
        """Set the d55 property."""
        self._cards[2].set_value("d55", value)

    @property
    def d66(self) -> typing.Optional[int]:
        """Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
        """ # nopep8
        return self._cards[2].get_value("d66")

    @d66.setter
    def d66(self, value: int) -> None:
        """Set the d66 property."""
        self._cards[2].set_value("d66", value)

    @property
    def d12(self) -> typing.Optional[int]:
        """Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
        """ # nopep8
        return self._cards[3].get_value("d12")

    @d12.setter
    def d12(self, value: int) -> None:
        """Set the d12 property."""
        self._cards[3].set_value("d12", value)

    @property
    def d21(self) -> typing.Optional[int]:
        """Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
        """ # nopep8
        return self._cards[3].get_value("d21")

    @d21.setter
    def d21(self, value: int) -> None:
        """Set the d21 property."""
        self._cards[3].set_value("d21", value)

    @property
    def d24(self) -> typing.Optional[int]:
        """Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
        """ # nopep8
        return self._cards[3].get_value("d24")

    @d24.setter
    def d24(self, value: int) -> None:
        """Set the d24 property."""
        self._cards[3].set_value("d24", value)

    @property
    def d42(self) -> typing.Optional[int]:
        """Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
        """ # nopep8
        return self._cards[3].get_value("d42")

    @d42.setter
    def d42(self, value: int) -> None:
        """Set the d42 property."""
        self._cards[3].set_value("d42", value)

    @property
    def d14(self) -> typing.Optional[int]:
        """Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
        """ # nopep8
        return self._cards[3].get_value("d14")

    @d14.setter
    def d14(self, value: int) -> None:
        """Set the d14 property."""
        self._cards[3].set_value("d14", value)

    @property
    def d41(self) -> typing.Optional[int]:
        """Get or set the DEFINE_FUNCTION IDs for damage tensor coefficients, see remarks.
        """ # nopep8
        return self._cards[3].get_value("d41")

    @d41.setter
    def d41(self, value: int) -> None:
        """Set the d41 property."""
        self._cards[3].set_value("d41", value)

    @property
    def lcsdg(self) -> int:
        """Get or set the Load curve ID defining corresponding history value to failure vs. triaxiality.
        """ # nopep8
        return self._cards[4].get_value("lcsdg")

    @lcsdg.setter
    def lcsdg(self, value: int) -> None:
        """Set the lcsdg property."""
        self._cards[4].set_value("lcsdg", value)

    @property
    def ecrit(self) -> typing.Optional[float]:
        """Get or set the Critical history value (material instability), see below.
        LT.0.0: |ECRIT| is load curve ID defining critical history value vs. triaxiality.
        EQ.0.0: Fixed value DCRIT defining critical damage is read.
        GT.0.0: Fixed value for stress-state independent critical history value.
        """ # nopep8
        return self._cards[4].get_value("ecrit")

    @ecrit.setter
    def ecrit(self, value: float) -> None:
        """Set the ecrit property."""
        self._cards[4].set_value("ecrit", value)

    @property
    def dmgexp(self) -> float:
        """Get or set the Exponent for nonlinear damage accumulation.
        """ # nopep8
        return self._cards[4].get_value("dmgexp")

    @dmgexp.setter
    def dmgexp(self, value: float) -> None:
        """Set the dmgexp property."""
        self._cards[4].set_value("dmgexp", value)

    @property
    def dcrit(self) -> typing.Optional[float]:
        """Get or set the Damage threshold value (critical damage). If a Load curve of critical
        history value or fixed value is given by ECRIT, input is ignored.
        """ # nopep8
        return self._cards[4].get_value("dcrit")

    @dcrit.setter
    def dcrit(self, value: float) -> None:
        """Set the dcrit property."""
        self._cards[4].set_value("dcrit", value)

    @property
    def fadexp(self) -> float:
        """Get or set the Exponent for damage-related stress fadeout.
        LT.0.0: |FADEXP| is load curve ID defining element-size dependent fading exponent.
        GT.0.0: Constant fading exponent.
        """ # nopep8
        return self._cards[4].get_value("fadexp")

    @fadexp.setter
    def fadexp(self, value: float) -> None:
        """Set the fadexp property."""
        self._cards[4].set_value("fadexp", value)

    @property
    def lcreg(self) -> int:
        """Get or set the Load curve ID defining element size dependent regularization factors for history value to failure.
        """ # nopep8
        return self._cards[4].get_value("lcreg")

    @lcreg.setter
    def lcreg(self, value: int) -> None:
        """Set the lcreg property."""
        self._cards[4].set_value("lcreg", value)

    @property
    def lcsrs(self) -> int:
        """Get or set the Load curve ID defining failure history value scaling factor for
        LCSDG vs. history value rate. If the first rate value in the curve is
        negative, it is assumed that all rate values are given as natural logarithm of the history rate.
        GT.0: scale ECRIT, too
        LT.0: do not scale ECRIT.
        """ # nopep8
        return self._cards[5].get_value("lcsrs")

    @lcsrs.setter
    def lcsrs(self, value: int) -> None:
        """Set the lcsrs property."""
        self._cards[5].set_value("lcsrs", value)

    @property
    def shrf(self) -> typing.Optional[float]:
        """Get or set the Reduction factors for regularization at triaxiality = 0 (shear).
        """ # nopep8
        return self._cards[5].get_value("shrf")

    @shrf.setter
    def shrf(self, value: float) -> None:
        """Set the shrf property."""
        self._cards[5].set_value("shrf", value)

    @property
    def biaxf(self) -> typing.Optional[float]:
        """Get or set the Reduction factors for regularization at triaxiality = 2/3 (biaxial).
        """ # nopep8
        return self._cards[5].get_value("biaxf")

    @biaxf.setter
    def biaxf(self, value: float) -> None:
        """Set the biaxf property."""
        self._cards[5].set_value("biaxf", value)

    @property
    def lcdlim(self) -> int:
        """Get or set the Load curve ID defining damage limit values as a function of triaxiality.Damage can be restricted to values less than 1.0 to prevent further stress reduction and failure for certain triaxialities
        """ # nopep8
        return self._cards[5].get_value("lcdlim")

    @lcdlim.setter
    def lcdlim(self, value: int) -> None:
        """Set the lcdlim property."""
        self._cards[5].set_value("lcdlim", value)

    @property
    def midfail(self) -> float:
        """Get or set the Mid-plane failure option for shell elements. If active, then critical strain is only checked at the mid-plane integration point, meaning an odd number for NIP should be used. Damage is computed at the other integration points, but no coupling to the stresses is done first. As soon as the mid-plane IP reaches ECRIT/DCRIT, then all the other IPs are also checked (exception: MIDFAIL = 4).
        EQ.0.0:	Inactive
        EQ.1.0 : Active.The stresses immediately begin to reduce for non - mid - plane IPs that are already above their critical value.Coupling only occurs for IPs that reach their criterion.
        EQ.2.0 : Active.The stresses immediately begin to reduce for all the non - mid - plane IPs.NUMFIP is active
        EQ.3.0 : Active.Same as 2, but when D = 1 is reached in the middle integration point, the element is eroded instantaneously.NUMFIP is disregarded.
        EQ.4.0 : Active.Damage and failure is applied only on the midpoint.When D = 1 on the midpoint, the element is eroded.NUMFIP is disregarded.Integration points away from the midplane see no stress reduction and no failure.
        """ # nopep8
        return self._cards[5].get_value("midfail")

    @midfail.setter
    def midfail(self, value: float) -> None:
        """Set the midfail property."""
        if value not in [0.0, 1.0, 2.0, 3.0, 4.0, None]:
            raise Exception("""midfail must be `None` or one of {0.0,1.0,2.0,3.0,4.0}.""")
        self._cards[5].set_value("midfail", value)

    @property
    def nfloc(self) -> typing.Optional[float]:
        """Get or set the Optional “local” number of failed integration points prior to element deletion. Overwrites the definition of NUMFIP for history variable HISn
        """ # nopep8
        return self._cards[5].get_value("nfloc")

    @nfloc.setter
    def nfloc(self, value: float) -> None:
        """Set the nfloc property."""
        self._cards[5].set_value("nfloc", value)

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

    @property
    def lcsdg_link(self) -> DefineCurve:
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
    def lcreg_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcreg."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcreg:
                return kwd
        return None

    @lcreg_link.setter
    def lcreg_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcreg."""
        self.lcreg = value.lcid

    @property
    def lcsrs_link(self) -> DefineCurve:
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
    def lcdlim_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcdlim."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcdlim:
                return kwd
        return None

    @lcdlim_link.setter
    def lcdlim_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcdlim."""
        self.lcdlim = value.lcid

