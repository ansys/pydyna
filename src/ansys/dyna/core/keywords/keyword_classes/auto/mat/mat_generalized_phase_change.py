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

"""Module providing the MatGeneralizedPhaseChange class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATGENERALIZEDPHASECHANGE_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("n", int, 20, 10, None),
    FieldSchema("e", float, 30, 10, None),
    FieldSchema("pr", float, 40, 10, None),
    FieldSchema("mix", int, 50, 10, None),
    FieldSchema("mixr", int, 60, 10, None),
)

_MATGENERALIZEDPHASECHANGE_CARD1 = (
    FieldSchema("tastrt", float, 0, 10, None),
    FieldSchema("tend", float, 10, 10, None),
    FieldSchema("cte", float, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("epsini", int, 50, 10, None),
    FieldSchema("dtemp", float, 60, 10, None),
)

_MATGENERALIZEDPHASECHANGE_CARD2 = (
    FieldSchema("xastr", float, 0, 10, None),
    FieldSchema("xaend", float, 10, 10, None),
    FieldSchema("xa1pa1", int, 20, 10, None),
    FieldSchema("xa1pa2", int, 30, 10, None),
    FieldSchema("xa1pa3", int, 40, 10, None),
    FieldSchema("xafpa", float, 50, 10, None),
    FieldSchema("cteann", float, 60, 10, None),
)

_MATGENERALIZEDPHASECHANGE_CARD3 = (
    FieldSchema("ptlaw", int, 0, 10, None),
    FieldSchema("ptstr", int, 10, 10, None),
    FieldSchema("ptend", int, 20, 10, None),
    FieldSchema("ptx1", int, 30, 10, None),
    FieldSchema("ptx2", int, 40, 10, None),
    FieldSchema("ptx3", int, 50, 10, None),
    FieldSchema("ptx4", int, 60, 10, None),
    FieldSchema("ptx5", int, 70, 10, None),
)

_MATGENERALIZEDPHASECHANGE_CARD4 = (
    FieldSchema("pttab1", int, 0, 10, None),
    FieldSchema("pttab2", int, 10, 10, None),
    FieldSchema("pttab3", int, 20, 10, None),
    FieldSchema("pttab4", int, 30, 10, None),
    FieldSchema("pttab5", int, 40, 10, None),
    FieldSchema("pttab6", int, 50, 10, None),
    FieldSchema("pttab7", int, 60, 10, None),
)

_MATGENERALIZEDPHASECHANGE_CARD5 = (
    FieldSchema("pteps", int, 0, 10, None),
    FieldSchema("ptrip", int, 10, 10, None),
    FieldSchema("ptlat", int, 20, 10, None),
    FieldSchema("postv", int, 30, 10, None),
    FieldSchema("nushis", int, 40, 10, None),
    FieldSchema("grai", float, 50, 10, None),
    FieldSchema("t1phas", int, 60, 10, None),
    FieldSchema("t2phas", int, 70, 10, None),
)

_MATGENERALIZEDPHASECHANGE_CARD6 = (
    FieldSchema("fushi1", int, 0, 10, None),
    FieldSchema("fushi2", int, 10, 10, None),
    FieldSchema("fushi3", int, 20, 10, None),
    FieldSchema("fushi4", int, 30, 10, None),
    FieldSchema("fushi5", int, 40, 10, None),
    FieldSchema("fushi6", int, 50, 10, None),
    FieldSchema("fushi7", int, 60, 10, None),
    FieldSchema("fushi8", int, 70, 10, None),
)

_MATGENERALIZEDPHASECHANGE_CARD7 = (
    FieldSchema("sigy1", int, 0, 10, None),
    FieldSchema("sigy2", int, 10, 10, None),
    FieldSchema("sigy3", int, 20, 10, None),
    FieldSchema("sigy4", int, 30, 10, None),
    FieldSchema("sigy5", int, 40, 10, None),
    FieldSchema("sigy6", int, 50, 10, None),
    FieldSchema("sigy7", int, 60, 10, None),
    FieldSchema("sigy8", int, 70, 10, None),
)

_MATGENERALIZEDPHASECHANGE_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatGeneralizedPhaseChange(KeywordBase):
    """DYNA MAT_GENERALIZED_PHASE_CHANGE keyword"""

    keyword = "MAT"
    subkeyword = "GENERALIZED_PHASE_CHANGE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "mix": LinkType.DEFINE_CURVE,
        "mixr": LinkType.DEFINE_CURVE_OR_TABLE,
        "xa1pa1": LinkType.DEFINE_CURVE_OR_TABLE,
        "xa1pa2": LinkType.DEFINE_CURVE_OR_TABLE,
        "xa1pa3": LinkType.DEFINE_CURVE_OR_TABLE,
        "sigy1": LinkType.DEFINE_CURVE_OR_TABLE,
        "sigy2": LinkType.DEFINE_CURVE_OR_TABLE,
        "sigy3": LinkType.DEFINE_CURVE_OR_TABLE,
        "sigy4": LinkType.DEFINE_CURVE_OR_TABLE,
        "sigy5": LinkType.DEFINE_CURVE_OR_TABLE,
        "sigy6": LinkType.DEFINE_CURVE_OR_TABLE,
        "sigy7": LinkType.DEFINE_CURVE_OR_TABLE,
        "sigy8": LinkType.DEFINE_CURVE_OR_TABLE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatGeneralizedPhaseChange class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATGENERALIZEDPHASECHANGE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATGENERALIZEDPHASECHANGE_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATGENERALIZEDPHASECHANGE_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATGENERALIZEDPHASECHANGE_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATGENERALIZEDPHASECHANGE_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATGENERALIZEDPHASECHANGE_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATGENERALIZEDPHASECHANGE_CARD6,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATGENERALIZEDPHASECHANGE_CARD7,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatGeneralizedPhaseChange.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATGENERALIZEDPHASECHANGE_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number or label must be specified.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        """Set the mid property."""
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density 𝜌.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        """Set the ro property."""
        self._cards[0].set_value("ro", value)

    @property
    def n(self) -> typing.Optional[int]:
        """Get or set the Number of phases.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: int) -> None:
        """Set the n property."""
        self._cards[0].set_value("n", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Youngs' modulus:
        GT.0.0: constant value is used
        LT.0.0: LCID or TABID. Temperature dependent Youngs' modulus given by load curve ID = -E or a Table
        ID = -E. Use TABID to describe temperature dependent modulus for each phase individually.
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's modulus:
        GT.0.0: constant value is used
        LT.0.0: LCID or TABID. Temperature dependent Posson's ratio given by load curve ID = -E or a Table ID = -E.
        Use TABID to describe temperature dependent parameter for each phase individually..
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def mix(self) -> typing.Optional[int]:
        """Get or set the Load curve ID with initial phase concentrations.
        """ # nopep8
        return self._cards[0].get_value("mix")

    @mix.setter
    def mix(self, value: int) -> None:
        """Set the mix property."""
        self._cards[0].set_value("mix", value)

    @property
    def mixr(self) -> typing.Optional[int]:
        """Get or set the LCID or TABID for mixture rule. Use a TABID to define a temperature dependency.
        """ # nopep8
        return self._cards[0].get_value("mixr")

    @mixr.setter
    def mixr(self, value: int) -> None:
        """Set the mixr property."""
        self._cards[0].set_value("mixr", value)

    @property
    def tastrt(self) -> typing.Optional[float]:
        """Get or set the Temperature start for simple linear annealing.  See Remark 4.
        IF TASTAR.gt.0 and TAEND.eq.0, an enhanced annealing algorithm is used.In that case, TASTAR is interpreted as anneal optionand an additional card is required.Possible values for the extended anneal option are
        EQ.1:	linear annealing
        EQ.2 : JMAK
        """ # nopep8
        return self._cards[1].get_value("tastrt")

    @tastrt.setter
    def tastrt(self, value: float) -> None:
        """Set the tastrt property."""
        self._cards[1].set_value("tastrt", value)

    @property
    def tend(self) -> typing.Optional[float]:
        """Get or set the Temperature end for simple linear annealing.  See Remark 4.IF TASTAR.gt.0 and TAEND.eq.0, an enhanced annealing algorithm is used..
        """ # nopep8
        return self._cards[1].get_value("tend")

    @tend.setter
    def tend(self, value: float) -> None:
        """Set the tend property."""
        self._cards[1].set_value("tend", value)

    @property
    def cte(self) -> typing.Optional[float]:
        """Get or set the Coefficient of thermal expansion:
        GT.0.0: constant value is used
        LT.0.0: LCID or TABID. Temperature dependent CTE given	by load curve ID = -CTE or a Table ID = -CTE. Use
        Table ID to describe temperature dependent CTE for	each phase individually.
        """ # nopep8
        return self._cards[1].get_value("cte")

    @cte.setter
    def cte(self, value: float) -> None:
        """Set the cte property."""
        self._cards[1].set_value("cte", value)

    @property
    def epsini(self) -> typing.Optional[int]:
        """Get or set the Initial plastic strains, uniformly distributed within the whole part
        """ # nopep8
        return self._cards[1].get_value("epsini")

    @epsini.setter
    def epsini(self, value: int) -> None:
        """Set the epsini property."""
        self._cards[1].set_value("epsini", value)

    @property
    def dtemp(self) -> typing.Optional[float]:
        """Get or set the Maximum temperature variation within a time step. If exceeded during the analysis, a local sub-cycling is used for the calculation of phase transformations
        """ # nopep8
        return self._cards[1].get_value("dtemp")

    @dtemp.setter
    def dtemp(self, value: float) -> None:
        """Set the dtemp property."""
        self._cards[1].set_value("dtemp", value)

    @property
    def xastr(self) -> typing.Optional[float]:
        """Get or set the Annealing start temperature.
        """ # nopep8
        return self._cards[2].get_value("xastr")

    @xastr.setter
    def xastr(self, value: float) -> None:
        """Set the xastr property."""
        self._cards[2].set_value("xastr", value)

    @property
    def xaend(self) -> typing.Optional[float]:
        """Get or set the Annealing end temperature.
        """ # nopep8
        return self._cards[2].get_value("xaend")

    @xaend.setter
    def xaend(self, value: float) -> None:
        """Set the xaend property."""
        self._cards[2].set_value("xaend", value)

    @property
    def xa1pa1(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID defining the i-th parameter of the enhanced annealing option.  Interpretation of the parameter depends on TASTAR..
        """ # nopep8
        return self._cards[2].get_value("xa1pa1")

    @xa1pa1.setter
    def xa1pa1(self, value: int) -> None:
        """Set the xa1pa1 property."""
        self._cards[2].set_value("xa1pa1", value)

    @property
    def xa1pa2(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID defining the i-th parameter of the enhanced annealing option.  Interpretation of the parameter depends on TASTAR..
        """ # nopep8
        return self._cards[2].get_value("xa1pa2")

    @xa1pa2.setter
    def xa1pa2(self, value: int) -> None:
        """Set the xa1pa2 property."""
        self._cards[2].set_value("xa1pa2", value)

    @property
    def xa1pa3(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID defining the i-th parameter of the enhanced annealing option.  Interpretation of the parameter depends on TASTAR..
        """ # nopep8
        return self._cards[2].get_value("xa1pa3")

    @xa1pa3.setter
    def xa1pa3(self, value: int) -> None:
        """Set the xa1pa3 property."""
        self._cards[2].set_value("xa1pa3", value)

    @property
    def xafpa(self) -> typing.Optional[float]:
        """Get or set the Scalar parameter of the enhanced annealing option if applicable.  Interpretation of the parameter depends on TASTA.
        """ # nopep8
        return self._cards[2].get_value("xafpa")

    @xafpa.setter
    def xafpa(self, value: float) -> None:
        """Set the xafpa property."""
        self._cards[2].set_value("xafpa", value)

    @property
    def cteann(self) -> typing.Optional[float]:
        """Get or set the Annealing option for thermal expansion.
        LT.0: | CTEAN | defines the upper temperature limit(cut - off temperature) for evaluation of thermal strains.
        EQ.0 : no modification of thermal strains
        EQ.1 : XAEND defines the upper temperature limit(cut - off temperature) for evaluation of thermal strains
        """ # nopep8
        return self._cards[2].get_value("cteann")

    @cteann.setter
    def cteann(self, value: float) -> None:
        """Set the cteann property."""
        self._cards[2].set_value("cteann", value)

    @property
    def ptlaw(self) -> typing.Optional[int]:
        """Get or set the Table ID to define the phase transformation model as a function of source phase and target phase.  The values in *DEFINE_TABLE are the phase numbers before transformation.  The curves referenced by the table specify transformation model (ordinate) as a function of phase number after transformation.
        LT.0:	transformation model used in heating
        EQ.0 : no transformation
        GT.0 : transformation model is used in cooling
        There is a variety of possible transformation models which can be specified as ordinate values of the curves :
        EQ.1 : Koinstinen - Marburger
        EQ.2 : Johnson - Mehl - Avrami - Kolmogorov(JMAK)
        EQ.3 : Akerstrom(only for cooling)
        EQ.4 : Oddy(only for heating)
        EQ.5 : Phase Recovery I(only for heating)
        EQ.6 : Phase Recovery II(only for heating)
        EQ.7 : Parabolic Dissolution I(only for heating)
        EQ.8 : Parabolic Dissolution II(only for heating)
        EQ.9 : extended Koinstinen - Marburger(only for cooling)
        EQ.12 : JMAK for both coolingand heating
        """ # nopep8
        return self._cards[3].get_value("ptlaw")

    @ptlaw.setter
    def ptlaw(self, value: int) -> None:
        """Set the ptlaw property."""
        self._cards[3].set_value("ptlaw", value)

    @property
    def ptstr(self) -> typing.Optional[int]:
        """Get or set the Table ID to define start temperatures for the transformations as
        function of source phase and target phase. The values in
        *DEFINE_TABLE are the phase numbers before transformation
        (source phase). The curves referenced by the table specify start
        temperature (ordinate) versus phase number after transformation	(abscissa).
        """ # nopep8
        return self._cards[3].get_value("ptstr")

    @ptstr.setter
    def ptstr(self, value: int) -> None:
        """Set the ptstr property."""
        self._cards[3].set_value("ptstr", value)

    @property
    def ptend(self) -> typing.Optional[int]:
        """Get or set the Table ID to define end temperatures for the transformations as
        function of source phase and target phase. The values in
        *DEFINE_TABLE are the phase numbers before transformation
        (source phase). The curves referenced by the table specify end
        temperature (ordinate) versus phase number after transformation	(abscissa).
        """ # nopep8
        return self._cards[3].get_value("ptend")

    @ptend.setter
    def ptend(self, value: int) -> None:
        """Set the ptend property."""
        self._cards[3].set_value("ptend", value)

    @property
    def ptx1(self) -> typing.Optional[int]:
        """Get or set the Table ID defining the i-th scalar-valued phase transformation
        parameter as function of source phase and target phase (see
        description of transformation models below to see which
        parameters apply). The values in *DEFINE_TABLE are the phase
        numbers before transformation (source phase). The curves
        referenced by the table specify scalar parameter (ordinate) versus
        phase number after transformation (abscissa).
        """ # nopep8
        return self._cards[3].get_value("ptx1")

    @ptx1.setter
    def ptx1(self, value: int) -> None:
        """Set the ptx1 property."""
        self._cards[3].set_value("ptx1", value)

    @property
    def ptx2(self) -> typing.Optional[int]:
        """Get or set the Table ID defining the i-th scalar-valued phase transformation
        parameter as function of source phase and target phase (see
        description of transformation models below to see which
        parameters apply). The values in *DEFINE_TABLE are the phase
        numbers before transformation (source phase). The curves
        referenced by the table specify scalar parameter (ordinate) versus
        phase number after transformation (abscissa).
        """ # nopep8
        return self._cards[3].get_value("ptx2")

    @ptx2.setter
    def ptx2(self, value: int) -> None:
        """Set the ptx2 property."""
        self._cards[3].set_value("ptx2", value)

    @property
    def ptx3(self) -> typing.Optional[int]:
        """Get or set the Table ID defining the i-th scalar-valued phase transformation
        parameter as function of source phase and target phase (see
        description of transformation models below to see which
        parameters apply). The values in *DEFINE_TABLE are the phase
        numbers before transformation (source phase). The curves
        referenced by the table specify scalar parameter (ordinate) versus
        phase number after transformation (abscissa).
        """ # nopep8
        return self._cards[3].get_value("ptx3")

    @ptx3.setter
    def ptx3(self, value: int) -> None:
        """Set the ptx3 property."""
        self._cards[3].set_value("ptx3", value)

    @property
    def ptx4(self) -> typing.Optional[int]:
        """Get or set the Table ID defining the i-th scalar-valued phase transformation
        parameter as function of source phase and target phase (see
        description of transformation models below to see which
        parameters apply). The values in *DEFINE_TABLE are the phase
        numbers before transformation (source phase). The curves
        referenced by the table specify scalar parameter (ordinate) versus
        phase number after transformation (abscissa).
        """ # nopep8
        return self._cards[3].get_value("ptx4")

    @ptx4.setter
    def ptx4(self, value: int) -> None:
        """Set the ptx4 property."""
        self._cards[3].set_value("ptx4", value)

    @property
    def ptx5(self) -> typing.Optional[int]:
        """Get or set the Table ID defining the i-th scalar-valued phase transformation
        parameter as function of source phase and target phase (see
        description of transformation models below to see which
        parameters apply). The values in *DEFINE_TABLE are the phase
        numbers before transformation (source phase). The curves
        referenced by the table specify scalar parameter (ordinate) versus
        phase number after transformation (abscissa).
        """ # nopep8
        return self._cards[3].get_value("ptx5")

    @ptx5.setter
    def ptx5(self, value: int) -> None:
        """Set the ptx5 property."""
        self._cards[3].set_value("ptx5", value)

    @property
    def pttab1(self) -> typing.Optional[int]:
        """Get or set the Table ID of 3D table defining the i-th tabulated phase transformation
        parameter as function of source phase and target phase (see
        description of transformation models below to see which
        parameters apply). The values in *DEFINE_TABLE_3D are the
        phase numbers before transformation (source phase). The values in
        the 2D tables referenced by *DEFINE_TABLE_3D are the phase
        number after transformation. The curves referenced by the 2D
        tables specify tabulated parameter (ordinate) versus either
        temperature or temperature rate (abscissa).
        """ # nopep8
        return self._cards[4].get_value("pttab1")

    @pttab1.setter
    def pttab1(self, value: int) -> None:
        """Set the pttab1 property."""
        self._cards[4].set_value("pttab1", value)

    @property
    def pttab2(self) -> typing.Optional[int]:
        """Get or set the Table ID of 3D table defining the i-th tabulated phase transformation
        parameter as function of source phase and target phase (see
        description of transformation models below to see which
        parameters apply). The values in *DEFINE_TABLE_3D are the
        phase numbers before transformation (source phase). The values in
        the 2D tables referenced by *DEFINE_TABLE_3D are the phase
        number after transformation. The curves referenced by the 2D
        tables specify tabulated parameter (ordinate) versus either
        temperature or temperature rate (abscissa).
        """ # nopep8
        return self._cards[4].get_value("pttab2")

    @pttab2.setter
    def pttab2(self, value: int) -> None:
        """Set the pttab2 property."""
        self._cards[4].set_value("pttab2", value)

    @property
    def pttab3(self) -> typing.Optional[int]:
        """Get or set the Table ID of 3D table defining the i-th tabulated phase transformation
        parameter as function of source phase and target phase (see
        description of transformation models below to see which
        parameters apply). The values in *DEFINE_TABLE_3D are the
        phase numbers before transformation (source phase). The values in
        the 2D tables referenced by *DEFINE_TABLE_3D are the phase
        number after transformation. The curves referenced by the 2D
        tables specify tabulated parameter (ordinate) versus either
        temperature or temperature rate (abscissa).
        """ # nopep8
        return self._cards[4].get_value("pttab3")

    @pttab3.setter
    def pttab3(self, value: int) -> None:
        """Set the pttab3 property."""
        self._cards[4].set_value("pttab3", value)

    @property
    def pttab4(self) -> typing.Optional[int]:
        """Get or set the Table ID of 3D table defining the i-th tabulated phase transformation
        parameter as function of source phase and target phase (see
        description of transformation models below to see which
        parameters apply). The values in *DEFINE_TABLE_3D are the
        phase numbers before transformation (source phase). The values in
        the 2D tables referenced by *DEFINE_TABLE_3D are the phase
        number after transformation. The curves referenced by the 2D
        tables specify tabulated parameter (ordinate) versus either
        temperature or temperature rate (abscissa).
        """ # nopep8
        return self._cards[4].get_value("pttab4")

    @pttab4.setter
    def pttab4(self, value: int) -> None:
        """Set the pttab4 property."""
        self._cards[4].set_value("pttab4", value)

    @property
    def pttab5(self) -> typing.Optional[int]:
        """Get or set the Table ID of 3D table defining the i-th tabulated phase transformation
        parameter as function of source phase and target phase (see
        description of transformation models below to see which
        parameters apply). The values in *DEFINE_TABLE_3D are the
        phase numbers before transformation (source phase). The values in
        the 2D tables referenced by *DEFINE_TABLE_3D are the phase
        number after transformation. The curves referenced by the 2D
        tables specify tabulated parameter (ordinate) versus either
        temperature or temperature rate (abscissa).
        """ # nopep8
        return self._cards[4].get_value("pttab5")

    @pttab5.setter
    def pttab5(self, value: int) -> None:
        """Set the pttab5 property."""
        self._cards[4].set_value("pttab5", value)

    @property
    def pttab6(self) -> typing.Optional[int]:
        """Get or set the Table ID of 3D table defining the i-th tabulated phase transformation
        parameter as function of source phase and target phase (see
        description of transformation models below to see which
        parameters apply). The values in *DEFINE_TABLE_3D are the
        phase numbers before transformation (source phase). The values in
        the 2D tables referenced by *DEFINE_TABLE_3D are the phase
        number after transformation. The curves referenced by the 2D
        tables specify tabulated parameter (ordinate) versus either
        temperature or temperature rate (abscissa).
        """ # nopep8
        return self._cards[4].get_value("pttab6")

    @pttab6.setter
    def pttab6(self, value: int) -> None:
        """Set the pttab6 property."""
        self._cards[4].set_value("pttab6", value)

    @property
    def pttab7(self) -> typing.Optional[int]:
        """Get or set the Table ID of 3D table defining the i-th tabulated phase transformation
        parameter as function of source phase and target phase (see
        description of transformation models below to see which
        parameters apply). The values in *DEFINE_TABLE_3D are the
        phase numbers before transformation (source phase). The values in
        the 2D tables referenced by *DEFINE_TABLE_3D are the phase
        number after transformation. The curves referenced by the 2D
        tables specify tabulated parameter (ordinate) versus either
        temperature or temperature rate (abscissa).
        """ # nopep8
        return self._cards[4].get_value("pttab7")

    @pttab7.setter
    def pttab7(self, value: int) -> None:
        """Set the pttab7 property."""
        self._cards[4].set_value("pttab7", value)

    @property
    def pteps(self) -> typing.Optional[int]:
        """Get or set the Table ID containing transformation induced strains as function of
        source phase and target phase.
        """ # nopep8
        return self._cards[5].get_value("pteps")

    @pteps.setter
    def pteps(self, value: int) -> None:
        """Set the pteps property."""
        self._cards[5].set_value("pteps", value)

    @property
    def ptrip(self) -> typing.Optional[int]:
        """Get or set the Flag for transformation induced plasticity (TRIP). Algorithm active
        for positive value of PTRIP.
        """ # nopep8
        return self._cards[5].get_value("ptrip")

    @ptrip.setter
    def ptrip(self, value: int) -> None:
        """Set the ptrip property."""
        self._cards[5].set_value("ptrip", value)

    @property
    def ptlat(self) -> typing.Optional[int]:
        """Get or set the Table ID defining transformation induced heat generation .
        """ # nopep8
        return self._cards[5].get_value("ptlat")

    @ptlat.setter
    def ptlat(self, value: int) -> None:
        """Set the ptlat property."""
        self._cards[5].set_value("ptlat", value)

    @property
    def postv(self) -> typing.Optional[int]:
        """Get or set the Define additional pre-defined history variables that might be useful for post-processing.
        """ # nopep8
        return self._cards[5].get_value("postv")

    @postv.setter
    def postv(self, value: int) -> None:
        """Set the postv property."""
        self._cards[5].set_value("postv", value)

    @property
    def nushis(self) -> typing.Optional[int]:
        """Get or set the Number of additional user defined history variables.  .
        """ # nopep8
        return self._cards[5].get_value("nushis")

    @nushis.setter
    def nushis(self, value: int) -> None:
        """Set the nushis property."""
        self._cards[5].set_value("nushis", value)

    @property
    def grai(self) -> typing.Optional[float]:
        """Get or set the Initial grain size.
        """ # nopep8
        return self._cards[5].get_value("grai")

    @grai.setter
    def grai(self, value: float) -> None:
        """Set the grai property."""
        self._cards[5].set_value("grai", value)

    @property
    def t1phas(self) -> typing.Optional[int]:
        """Get or set the Lower temperature limit for cooling rate evaluation.  Cooling rate can be used as input for user defined variables.
        """ # nopep8
        return self._cards[5].get_value("t1phas")

    @t1phas.setter
    def t1phas(self, value: int) -> None:
        """Set the t1phas property."""
        self._cards[5].set_value("t1phas", value)

    @property
    def t2phas(self) -> typing.Optional[int]:
        """Get or set the Upper temperature limit for cooling rate evaluation.  Cooling rate can be used as input for user defined variables.
        """ # nopep8
        return self._cards[5].get_value("t2phas")

    @t2phas.setter
    def t2phas(self, value: int) -> None:
        """Set the t2phas property."""
        self._cards[5].set_value("t2phas", value)

    @property
    def fushi1(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables.
        """ # nopep8
        return self._cards[6].get_value("fushi1")

    @fushi1.setter
    def fushi1(self, value: int) -> None:
        """Set the fushi1 property."""
        self._cards[6].set_value("fushi1", value)

    @property
    def fushi2(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables.
        """ # nopep8
        return self._cards[6].get_value("fushi2")

    @fushi2.setter
    def fushi2(self, value: int) -> None:
        """Set the fushi2 property."""
        self._cards[6].set_value("fushi2", value)

    @property
    def fushi3(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables.
        """ # nopep8
        return self._cards[6].get_value("fushi3")

    @fushi3.setter
    def fushi3(self, value: int) -> None:
        """Set the fushi3 property."""
        self._cards[6].set_value("fushi3", value)

    @property
    def fushi4(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables.
        """ # nopep8
        return self._cards[6].get_value("fushi4")

    @fushi4.setter
    def fushi4(self, value: int) -> None:
        """Set the fushi4 property."""
        self._cards[6].set_value("fushi4", value)

    @property
    def fushi5(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables.
        """ # nopep8
        return self._cards[6].get_value("fushi5")

    @fushi5.setter
    def fushi5(self, value: int) -> None:
        """Set the fushi5 property."""
        self._cards[6].set_value("fushi5", value)

    @property
    def fushi6(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables.
        """ # nopep8
        return self._cards[6].get_value("fushi6")

    @fushi6.setter
    def fushi6(self, value: int) -> None:
        """Set the fushi6 property."""
        self._cards[6].set_value("fushi6", value)

    @property
    def fushi7(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables.
        """ # nopep8
        return self._cards[6].get_value("fushi7")

    @fushi7.setter
    def fushi7(self, value: int) -> None:
        """Set the fushi7 property."""
        self._cards[6].set_value("fushi7", value)

    @property
    def fushi8(self) -> typing.Optional[int]:
        """Get or set the Function ID for user defined history variables.
        """ # nopep8
        return self._cards[6].get_value("fushi8")

    @fushi8.setter
    def fushi8(self, value: int) -> None:
        """Set the fushi8 property."""
        self._cards[6].set_value("fushi8", value)

    @property
    def sigy1(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID for hardening of phase i..
        """ # nopep8
        return self._cards[7].get_value("sigy1")

    @sigy1.setter
    def sigy1(self, value: int) -> None:
        """Set the sigy1 property."""
        self._cards[7].set_value("sigy1", value)

    @property
    def sigy2(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID for hardening of phase i..
        """ # nopep8
        return self._cards[7].get_value("sigy2")

    @sigy2.setter
    def sigy2(self, value: int) -> None:
        """Set the sigy2 property."""
        self._cards[7].set_value("sigy2", value)

    @property
    def sigy3(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID for hardening of phase i..
        """ # nopep8
        return self._cards[7].get_value("sigy3")

    @sigy3.setter
    def sigy3(self, value: int) -> None:
        """Set the sigy3 property."""
        self._cards[7].set_value("sigy3", value)

    @property
    def sigy4(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID for hardening of phase i..
        """ # nopep8
        return self._cards[7].get_value("sigy4")

    @sigy4.setter
    def sigy4(self, value: int) -> None:
        """Set the sigy4 property."""
        self._cards[7].set_value("sigy4", value)

    @property
    def sigy5(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID for hardening of phase i..
        """ # nopep8
        return self._cards[7].get_value("sigy5")

    @sigy5.setter
    def sigy5(self, value: int) -> None:
        """Set the sigy5 property."""
        self._cards[7].set_value("sigy5", value)

    @property
    def sigy6(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID for hardening of phase i..
        """ # nopep8
        return self._cards[7].get_value("sigy6")

    @sigy6.setter
    def sigy6(self, value: int) -> None:
        """Set the sigy6 property."""
        self._cards[7].set_value("sigy6", value)

    @property
    def sigy7(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID for hardening of phase i..
        """ # nopep8
        return self._cards[7].get_value("sigy7")

    @sigy7.setter
    def sigy7(self, value: int) -> None:
        """Set the sigy7 property."""
        self._cards[7].set_value("sigy7", value)

    @property
    def sigy8(self) -> typing.Optional[int]:
        """Get or set the Load curve or table ID for hardening of phase i..
        """ # nopep8
        return self._cards[7].get_value("sigy8")

    @sigy8.setter
    def sigy8(self, value: int) -> None:
        """Set the sigy8 property."""
        self._cards[7].set_value("sigy8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[8].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[8].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def mix_link(self) -> DefineCurve:
        """Get the DefineCurve object for mix."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.mix:
                return kwd
        return None

    @mix_link.setter
    def mix_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for mix."""
        self.mix = value.lcid

    @property
    def mixr_link(self) -> KeywordBase:
        """Get the linked DEFINE_CURVE or DEFINE_TABLE for mixr."""
        if self.deck is None:
            return None
        field_value = self.mixr
        if field_value is None or field_value == 0:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == field_value:
                return kwd
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "TABLE"):
            if kwd.tbid == field_value:
                return kwd
        return None

    @mixr_link.setter
    def mixr_link(self, value: KeywordBase) -> None:
        """Set the linked keyword for mixr."""
        if hasattr(value, "lcid"):
            self.mixr = value.lcid
        elif hasattr(value, "tbid"):
            self.mixr = value.tbid

    @property
    def xa1pa1_link(self) -> KeywordBase:
        """Get the linked DEFINE_CURVE or DEFINE_TABLE for xa1pa1."""
        if self.deck is None:
            return None
        field_value = self.xa1pa1
        if field_value is None or field_value == 0:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == field_value:
                return kwd
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "TABLE"):
            if kwd.tbid == field_value:
                return kwd
        return None

    @xa1pa1_link.setter
    def xa1pa1_link(self, value: KeywordBase) -> None:
        """Set the linked keyword for xa1pa1."""
        if hasattr(value, "lcid"):
            self.xa1pa1 = value.lcid
        elif hasattr(value, "tbid"):
            self.xa1pa1 = value.tbid

    @property
    def xa1pa2_link(self) -> KeywordBase:
        """Get the linked DEFINE_CURVE or DEFINE_TABLE for xa1pa2."""
        if self.deck is None:
            return None
        field_value = self.xa1pa2
        if field_value is None or field_value == 0:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == field_value:
                return kwd
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "TABLE"):
            if kwd.tbid == field_value:
                return kwd
        return None

    @xa1pa2_link.setter
    def xa1pa2_link(self, value: KeywordBase) -> None:
        """Set the linked keyword for xa1pa2."""
        if hasattr(value, "lcid"):
            self.xa1pa2 = value.lcid
        elif hasattr(value, "tbid"):
            self.xa1pa2 = value.tbid

    @property
    def xa1pa3_link(self) -> KeywordBase:
        """Get the linked DEFINE_CURVE or DEFINE_TABLE for xa1pa3."""
        if self.deck is None:
            return None
        field_value = self.xa1pa3
        if field_value is None or field_value == 0:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == field_value:
                return kwd
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "TABLE"):
            if kwd.tbid == field_value:
                return kwd
        return None

    @xa1pa3_link.setter
    def xa1pa3_link(self, value: KeywordBase) -> None:
        """Set the linked keyword for xa1pa3."""
        if hasattr(value, "lcid"):
            self.xa1pa3 = value.lcid
        elif hasattr(value, "tbid"):
            self.xa1pa3 = value.tbid

    @property
    def sigy1_link(self) -> KeywordBase:
        """Get the linked DEFINE_CURVE or DEFINE_TABLE for sigy1."""
        if self.deck is None:
            return None
        field_value = self.sigy1
        if field_value is None or field_value == 0:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == field_value:
                return kwd
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "TABLE"):
            if kwd.tbid == field_value:
                return kwd
        return None

    @sigy1_link.setter
    def sigy1_link(self, value: KeywordBase) -> None:
        """Set the linked keyword for sigy1."""
        if hasattr(value, "lcid"):
            self.sigy1 = value.lcid
        elif hasattr(value, "tbid"):
            self.sigy1 = value.tbid

    @property
    def sigy2_link(self) -> KeywordBase:
        """Get the linked DEFINE_CURVE or DEFINE_TABLE for sigy2."""
        if self.deck is None:
            return None
        field_value = self.sigy2
        if field_value is None or field_value == 0:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == field_value:
                return kwd
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "TABLE"):
            if kwd.tbid == field_value:
                return kwd
        return None

    @sigy2_link.setter
    def sigy2_link(self, value: KeywordBase) -> None:
        """Set the linked keyword for sigy2."""
        if hasattr(value, "lcid"):
            self.sigy2 = value.lcid
        elif hasattr(value, "tbid"):
            self.sigy2 = value.tbid

    @property
    def sigy3_link(self) -> KeywordBase:
        """Get the linked DEFINE_CURVE or DEFINE_TABLE for sigy3."""
        if self.deck is None:
            return None
        field_value = self.sigy3
        if field_value is None or field_value == 0:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == field_value:
                return kwd
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "TABLE"):
            if kwd.tbid == field_value:
                return kwd
        return None

    @sigy3_link.setter
    def sigy3_link(self, value: KeywordBase) -> None:
        """Set the linked keyword for sigy3."""
        if hasattr(value, "lcid"):
            self.sigy3 = value.lcid
        elif hasattr(value, "tbid"):
            self.sigy3 = value.tbid

    @property
    def sigy4_link(self) -> KeywordBase:
        """Get the linked DEFINE_CURVE or DEFINE_TABLE for sigy4."""
        if self.deck is None:
            return None
        field_value = self.sigy4
        if field_value is None or field_value == 0:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == field_value:
                return kwd
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "TABLE"):
            if kwd.tbid == field_value:
                return kwd
        return None

    @sigy4_link.setter
    def sigy4_link(self, value: KeywordBase) -> None:
        """Set the linked keyword for sigy4."""
        if hasattr(value, "lcid"):
            self.sigy4 = value.lcid
        elif hasattr(value, "tbid"):
            self.sigy4 = value.tbid

    @property
    def sigy5_link(self) -> KeywordBase:
        """Get the linked DEFINE_CURVE or DEFINE_TABLE for sigy5."""
        if self.deck is None:
            return None
        field_value = self.sigy5
        if field_value is None or field_value == 0:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == field_value:
                return kwd
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "TABLE"):
            if kwd.tbid == field_value:
                return kwd
        return None

    @sigy5_link.setter
    def sigy5_link(self, value: KeywordBase) -> None:
        """Set the linked keyword for sigy5."""
        if hasattr(value, "lcid"):
            self.sigy5 = value.lcid
        elif hasattr(value, "tbid"):
            self.sigy5 = value.tbid

    @property
    def sigy6_link(self) -> KeywordBase:
        """Get the linked DEFINE_CURVE or DEFINE_TABLE for sigy6."""
        if self.deck is None:
            return None
        field_value = self.sigy6
        if field_value is None or field_value == 0:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == field_value:
                return kwd
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "TABLE"):
            if kwd.tbid == field_value:
                return kwd
        return None

    @sigy6_link.setter
    def sigy6_link(self, value: KeywordBase) -> None:
        """Set the linked keyword for sigy6."""
        if hasattr(value, "lcid"):
            self.sigy6 = value.lcid
        elif hasattr(value, "tbid"):
            self.sigy6 = value.tbid

    @property
    def sigy7_link(self) -> KeywordBase:
        """Get the linked DEFINE_CURVE or DEFINE_TABLE for sigy7."""
        if self.deck is None:
            return None
        field_value = self.sigy7
        if field_value is None or field_value == 0:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == field_value:
                return kwd
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "TABLE"):
            if kwd.tbid == field_value:
                return kwd
        return None

    @sigy7_link.setter
    def sigy7_link(self, value: KeywordBase) -> None:
        """Set the linked keyword for sigy7."""
        if hasattr(value, "lcid"):
            self.sigy7 = value.lcid
        elif hasattr(value, "tbid"):
            self.sigy7 = value.tbid

    @property
    def sigy8_link(self) -> KeywordBase:
        """Get the linked DEFINE_CURVE or DEFINE_TABLE for sigy8."""
        if self.deck is None:
            return None
        field_value = self.sigy8
        if field_value is None or field_value == 0:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == field_value:
                return kwd
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "TABLE"):
            if kwd.tbid == field_value:
                return kwd
        return None

    @sigy8_link.setter
    def sigy8_link(self, value: KeywordBase) -> None:
        """Set the linked keyword for sigy8."""
        if hasattr(value, "lcid"):
            self.sigy8 = value.lcid
        elif hasattr(value, "tbid"):
            self.sigy8 = value.tbid

