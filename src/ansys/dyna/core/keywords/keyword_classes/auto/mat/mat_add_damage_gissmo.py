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

"""Module providing the MatAddDamageGissmo class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATADDDAMAGEGISSMO_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("unused", int, 10, 10, None),
    FieldSchema("dtyp", float, 20, 10, 0.0),
    FieldSchema("refsz", float, 30, 10, None),
    FieldSchema("numfip", float, 40, 10, 1.0),
)

_MATADDDAMAGEGISSMO_CARD1 = (
    FieldSchema("lcsdg", int, 0, 10, 0),
    FieldSchema("ecrit", float, 10, 10, None),
    FieldSchema("dmgexp", float, 20, 10, 1.0),
    FieldSchema("dcrit", float, 30, 10, None),
    FieldSchema("fadexp", float, 40, 10, 1.0),
    FieldSchema("lcregd", int, 50, 10, 0),
    FieldSchema("instf", int, 60, 10, None),
)

_MATADDDAMAGEGISSMO_CARD2 = (
    FieldSchema("lcsrs", int, 0, 10, None),
    FieldSchema("shrf", float, 10, 10, None),
    FieldSchema("biaxf", float, 20, 10, None),
    FieldSchema("lcdlim", int, 30, 10, 0),
    FieldSchema("midfail", float, 40, 10, None),
    FieldSchema("hisvn", float, 50, 10, None),
    FieldSchema("soft", float, 60, 10, None),
    FieldSchema("lp2bi", float, 70, 10, None),
)

_MATADDDAMAGEGISSMO_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatAddDamageGissmo(KeywordBase):
    """DYNA MAT_ADD_DAMAGE_GISSMO keyword"""

    keyword = "MAT"
    subkeyword = "ADD_DAMAGE_GISSMO"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcdlim": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatAddDamageGissmo class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATADDDAMAGEGISSMO_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATADDDAMAGEGISSMO_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATADDDAMAGEGISSMO_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatAddDamageGissmo.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATADDDAMAGEGISSMO_OPTION0_CARD0,
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
    def dtyp(self) -> float:
        """Get or set the DTYP is interpreted digit-wise as follows:
        DTYP=[NM]=M+10×N
        M.EQ.0:	damage is accumulated, but there is no coupling to flow stress and no failure.
        M.EQ.1:	damage is accumulated, and element failure occurs for D=1.  Coupling of damage to flow stress depending on parameters, see remarks below.
        N.EQ.0:	equivalent plastic strain is the driving quantity for the damage.  (To be more precise, it’s the history variable that LS-PrePost blindly labels as “plastic strain.”  What this history variable actually represents depends on the material model.)
        N.GT.0:	the Nth additional history variable is the driving quantity for damage.  These additional history variables are the same ones flagged by the *DATABASE_EXTENT_BINARY keyword’s NEIPS and NEIPH fields.  For example, for solid elements with *MAT_187, setting N=6 causes volumetric plastic strain to be the driving quantity for the GISSMO damage.
        """ # nopep8
        return self._cards[0].get_value("dtyp")

    @dtyp.setter
    def dtyp(self, value: float) -> None:
        """Set the dtyp property."""
        if value not in [0, 1, None]:
            raise Exception("""dtyp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("dtyp", value)

    @property
    def refsz(self) -> typing.Optional[float]:
        """Get or set the Reference element size, for which an additional output of damage will be generated.
        This is necessary to ensure the applicability of resulting damage quantities when transferred to different mesh sizes.
        """ # nopep8
        return self._cards[0].get_value("refsz")

    @refsz.setter
    def refsz(self, value: float) -> None:
        """Set the refsz property."""
        self._cards[0].set_value("refsz", value)

    @property
    def numfip(self) -> float:
        """Get or set the Number or percentage of failed integration points prior to element deletion (default value is 1).
        GT.0.0:	Number of integration points which must fail before element is deleted.
        LT.0.0:	Applies only to shells. |NUMFIP| is the percentage of layers which must fail before element fails.
        For shell formulations with 4 integration points per layer, the layer is considered failed if any of the integration points in the layer fails
        """ # nopep8
        return self._cards[0].get_value("numfip")

    @numfip.setter
    def numfip(self, value: float) -> None:
        """Set the numfip property."""
        self._cards[0].set_value("numfip", value)

    @property
    def lcsdg(self) -> int:
        """Get or set the Failure strain curve/table or function:
        GT.0.0:	Load curve ID or table ID.As a load curve, it defines equivalent plastic strain to failure as a function of triaxiality.As a table, it defines for each Lode parameter value(between - 1 and 1) a load curve ID giving the equivalent plastic strain to failure as a function of triaxiality for that Lode parameter value.With HISVN ≠ 0, a 3D table can be used, where failure strain is a function of the history variable(TABLE_3D), Lode parameter(TABLE),and triaxiality(CURVE).With HISVN = 0, a 3D table introduces thermal effects, that is, failure strain is a function of temperature(TABLE_3D), Lode parameter(TABLE),and triaxiality(CURVE).As a 4D table, failure strain is a function of strain rate(TABLE_4D), temperature(TABLE_3D), Lode parameter(TABLE),and triaxiality(CURVE).
        LT.0.0 : | LCSDG | is the ID of a function(*DEFINE_FUNCTION) with the arguments triaxiality η, Lode parameter L, plastic strain rate ε ̇^ p, temperature T, history variable HISVN ,and element size l_e : f(η,L,ε ̇ ^ p,T,HISVN,l_e).Note that the sequence of the arguments is important, not their names.
        """ # nopep8
        return self._cards[1].get_value("lcsdg")

    @lcsdg.setter
    def lcsdg(self, value: int) -> None:
        """Set the lcsdg property."""
        self._cards[1].set_value("lcsdg", value)

    @property
    def ecrit(self) -> typing.Optional[float]:
        """Get or set the Critical plastic strain (material instability); see below.
        LT.0.0: | ECRIT | is either a load curve ID defining critical equivalent plastic strain versus triaxiality or a table ID defining critical equivalent plastic strain as a function of triaxiality and Lode parameter(as in LCSDG).With HISVN ≠ 0, a 3D table can be used, where critical strain is a function of the history variable(TABLE_3D), Lode parameter(TABLE),and triaxiality(CURVE).With HISVN = 0, a 3D table introduces thermal effects, that is, critical strain is a function of temperature(TABLE_3D), Lode parameter(TABLE),and triaxiality(CURVE).As a 4D table, critical strain is a function of strain rate(TABLE_4D), temperature(TABLE_3D), Lode parameter(TABLE),and triaxiality(CURVE).
        EQ.0.0 : Fixed value DCRIT defining critical damage is read(see below).
        GT.0.0 : Fixed value for stress - state independent critical equivalent plastic strain
        """ # nopep8
        return self._cards[1].get_value("ecrit")

    @ecrit.setter
    def ecrit(self, value: float) -> None:
        """Set the ecrit property."""
        self._cards[1].set_value("ecrit", value)

    @property
    def dmgexp(self) -> float:
        """Get or set the Exponent for nonlinear damage accumulation, see remarks..
        """ # nopep8
        return self._cards[1].get_value("dmgexp")

    @dmgexp.setter
    def dmgexp(self, value: float) -> None:
        """Set the dmgexp property."""
        self._cards[1].set_value("dmgexp", value)

    @property
    def dcrit(self) -> typing.Optional[float]:
        """Get or set the Damage threshold value (critical damage). If a Load curve of critical plastic strain or fixed value is given by ECRIT, input is ignored.
        """ # nopep8
        return self._cards[1].get_value("dcrit")

    @dcrit.setter
    def dcrit(self, value: float) -> None:
        """Set the dcrit property."""
        self._cards[1].set_value("dcrit", value)

    @property
    def fadexp(self) -> float:
        """Get or set the Exponent for damage-related stress fadeout.
        LT.0.0:	|FADEXP|  is a load curve ID or table ID. As a load curve it gives the fading exponent as a function of element size. As a table, it specifies the fading exponent as a function triaxiality (TABLE) and element size (CURVE). For 3D tables, it specifies the fading exponent as a function Lode parameter (TABLE_3D), triaxiality (TABLE), and element size (CURVE).
        GT.0.0:	Constant fading exponent.
        """ # nopep8
        return self._cards[1].get_value("fadexp")

    @fadexp.setter
    def fadexp(self, value: float) -> None:
        """Set the fadexp property."""
        self._cards[1].set_value("fadexp", value)

    @property
    def lcregd(self) -> int:
        """Get or set the Load curve ID or Table ID defining element size dependent regulari-zation factors for equivalent plastic strain to failure.
        GT.0.0:	Load curve ID (reg. factor vs. element size) or Table ID (reg. factor vs. element size curves vs. effective rate)
        LT.0.0:	|LCREGD| is Table ID (reg. factor vs. element size curves vs. triaxiality) or a 3D table ID (regularization factor as function of Lode parameter, triaxiality, and element size).
        This table provides an alternative to the use of SHRF and BIAXF for defining the effect of triaxiality on element size regularization of equivalent plastic strain to failure.
        """ # nopep8
        return self._cards[1].get_value("lcregd")

    @lcregd.setter
    def lcregd(self, value: int) -> None:
        """Set the lcregd property."""
        self._cards[1].set_value("lcregd", value)

    @property
    def instf(self) -> typing.Optional[int]:
        """Get or set the Flag for governing the behavior of instability measure, F, and fading exponent, FADEXP (see Remarks):
        EQ.0:	F is incrementally updated,and FADEXP, if from a table, is allowed to vary.
        EQ.1 : F is incrementally updated,and FADEXP is kept constant after F = 1.
        EQ.0 : F is only 0 or 1 (after ECRIT is reached),and FADEXP, if from a table, is allowed to vary.
        EQ.1 : F is only 0 or 1 (after ECRIT is reached),and FADEXP is kept constant after F = 1.
        """ # nopep8
        return self._cards[1].get_value("instf")

    @instf.setter
    def instf(self, value: int) -> None:
        """Set the instf property."""
        self._cards[1].set_value("instf", value)

    @property
    def lcsrs(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining failure strain scaling factor for LCSDG vs. strain rate. If the first strain rate value in the curve is negative,
        it is assumed that all strain rate values are given as natural logarithm of the strain rate. The curve should not extrapolate to zero or failure may occur at low strain.
        GT.0:	scale ECRIT, too
        LT.0:	do not scale ECRIT.
        """ # nopep8
        return self._cards[2].get_value("lcsrs")

    @lcsrs.setter
    def lcsrs(self, value: int) -> None:
        """Set the lcsrs property."""
        self._cards[2].set_value("lcsrs", value)

    @property
    def shrf(self) -> typing.Optional[float]:
        """Get or set the Reduction factor for regularization for shear stress states. This parameter can be defined between -1.0 and +1.0. See remarks below.
        """ # nopep8
        return self._cards[2].get_value("shrf")

    @shrf.setter
    def shrf(self, value: float) -> None:
        """Set the shrf property."""
        self._cards[2].set_value("shrf", value)

    @property
    def biaxf(self) -> typing.Optional[float]:
        """Get or set the Reduction factor for regularization for biaxial stress states. This parameter can be defined between -1.0 and +1.0. See remarks below.
        """ # nopep8
        return self._cards[2].get_value("biaxf")

    @biaxf.setter
    def biaxf(self, value: float) -> None:
        """Set the biaxf property."""
        self._cards[2].set_value("biaxf", value)

    @property
    def lcdlim(self) -> int:
        """Get or set the Load curve ID defining damage limit values as a function of triaxiality. Damage can be restricted to values less than 1.0 to prevent further stress reduction and failure for certain triaxialities..
        """ # nopep8
        return self._cards[2].get_value("lcdlim")

    @lcdlim.setter
    def lcdlim(self, value: int) -> None:
        """Set the lcdlim property."""
        self._cards[2].set_value("lcdlim", value)

    @property
    def midfail(self) -> typing.Optional[float]:
        """Get or set the Mid-plane failure option for shell elements.
        If active, then critical strain is only checked at the mid-plane integration point, i.e., an odd number for NIP should be used.
        The other integration points compute their damage, but no coupling to the stresses is done first.
        As soon as the mid-plane IP reaches ECRIT/DCRIT, then all the other IP's are also checked.
        Those of them that are already above their critical value immediately start to reduce the stresses.
        Those who are still below critical still do not couple, only if they reach their criterion.
        EQ.0.0:	Inactive,
        EQ.1.0:	Active.
        """ # nopep8
        return self._cards[2].get_value("midfail")

    @midfail.setter
    def midfail(self, value: float) -> None:
        """Set the midfail property."""
        self._cards[2].set_value("midfail", value)

    @property
    def hisvn(self) -> typing.Optional[float]:
        """Get or set the History variable used to evaluate the 3D table LCSDG:
        GT.0.0:	constant value
        LT.0.0 : the constant value found at position  where  is the location in the history array of * INITIAL_STRESS_ SHELL / SOLID.
        """ # nopep8
        return self._cards[2].get_value("hisvn")

    @hisvn.setter
    def hisvn(self, value: float) -> None:
        """Set the hisvn property."""
        self._cards[2].set_value("hisvn", value)

    @property
    def soft(self) -> typing.Optional[float]:
        """Get or set the Softening reduction factor for failure strain in crashfront elements. Crashfront elements are elements that are direct neighbors of failed (deleted) elements.
        EQ.0.0:	inactive
        GT.0.0 : plastic failure strain,  (LCSDG),and critical plastic strain,  (ECRIT), will be scaled by SOFT.
        LT.0.0 : only plastic failure strain,  (LCSDG), will be scaled by | SOFT |.
        """ # nopep8
        return self._cards[2].get_value("soft")

    @soft.setter
    def soft(self, value: float) -> None:
        """Set the soft property."""
        self._cards[2].set_value("soft", value)

    @property
    def lp2bi(self) -> typing.Optional[float]:
        """Get or set the Option to use a bending indicator instead of the Lode parameter. Everyhwere in this keyword’s manual description, the term “Lode parameter” can/should be replaced by the expression “bending indicator”, which is adopted from *MAT_258 (cf. variable Ω). Only available for shell elements.
        EQ.0.0:	inactive.
        EQ.1.0:	active. Constant regularization (LCREGD) applied.
        EQ.2.0:	active. Regularization (LCRGED) fully applied under pure membrane loading (Ω=0), but not at all under pure bending (Ω=1). Linear interpolation in between.
        """ # nopep8
        return self._cards[2].get_value("lp2bi")

    @lp2bi.setter
    def lp2bi(self, value: float) -> None:
        """Set the lp2bi property."""
        self._cards[2].set_value("lp2bi", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

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

