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

"""Module providing the MatShapeMemory class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_MATSHAPEMEMORY_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("lcss", float, 40, 10, 0.0),
    FieldSchema("lcssc", float, 50, 10, 0.0),
    FieldSchema("idpp", int, 60, 10, 0),
)

_MATSHAPEMEMORY_CARD1 = (
    FieldSchema("sig_ass", float, 0, 10, None),
    FieldSchema("sig_asf", float, 10, 10, None),
    FieldSchema("sig_sas", float, 20, 10, None),
    FieldSchema("sig_saf", float, 30, 10, None),
    FieldSchema("epsl", float, 40, 10, None),
    FieldSchema("alpha", float, 50, 10, None),
    FieldSchema("ymrt", float, 60, 10, None),
)

_MATSHAPEMEMORY_CARD2 = (
    FieldSchema("lcid_as", int, 0, 10, None),
    FieldSchema("lcid_sa", int, 10, 10, None),
)

_MATSHAPEMEMORY_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatShapeMemory(KeywordBase):
    """DYNA MAT_SHAPE_MEMORY keyword"""

    keyword = "MAT"
    subkeyword = "SHAPE_MEMORY"
    _option_spec_list = [
        OptionSpec("TITLE", "pre/1", 1),
    ]
    _link_fields = {
        "lcid_as": LinkType.DEFINE_CURVE_OR_TABLE,
        "lcid_sa": LinkType.DEFINE_CURVE_OR_TABLE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatShapeMemory class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATSHAPEMEMORY_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATSHAPEMEMORY_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _MATSHAPEMEMORY_CARD2,
                **kwargs,
            ),
            OptionCardSet(
                option_spec = MatShapeMemory._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATSHAPEMEMORY_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique has to be used.
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
    def lcss(self) -> float:
        """Get or set the The absolute value of LCSS is a load curve ID for effective stress as a function of effective plastic or effective total strain depending on the first data point (this load curve is optional).
        If the first data point has a positive ordinate value, it indicates the initial yield stress at zero plastic strain.If it is zero, it is a curve with respect to effective total strain.The first abscissa value must always be zero.
        For a negative value of LCSS, negative values of SIG_ASS, SIG_ASF, SIG_SAS, and SIG_SAF indicate dependence on plastic strain; see below.
        """ # nopep8
        return self._cards[0].get_value("lcss")

    @lcss.setter
    def lcss(self, value: float) -> None:
        """Set the lcss property."""
        self._cards[0].set_value("lcss", value)

    @property
    def lcssc(self) -> float:
        """Get or set the The absolute value of LCSSC  is a load curve ID for effective stress as a function of effective plastic or effective total strain (depending on the first data point) under compressive load (this load curve is optional).
        If the first data point has a positive ordinate value, it indicates the initial yield stress at zero plastic strain.If it is zero, it is a curve with respect to effective total strain.The first abscissa value must always be zero.
        A negative value of LCSSC scales the load curve with the parameter ALPHA.For example, LCSSC = -LCSS will give the same relation between compressive and tensile hardening as between compressive and tensile phase transformation stresses.
        If LCSSC is undefined or zero, LCSS specifies the behavior in compression.
        """ # nopep8
        return self._cards[0].get_value("lcssc")

    @lcssc.setter
    def lcssc(self, value: float) -> None:
        """Set the lcssc property."""
        self._cards[0].set_value("lcssc", value)

    @property
    def idpp(self) -> int:
        """Get or set the Plasticity model (see Remark 2):
        EQ.0: von Mises plasticity (default)
        EQ.1: Drucker-Prager-based plasticity.
        """ # nopep8
        return self._cards[0].get_value("idpp")

    @idpp.setter
    def idpp(self, value: int) -> None:
        """Set the idpp property."""
        if value not in [0, 1, None]:
            raise Exception("""idpp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("idpp", value)

    @property
    def sig_ass(self) -> typing.Optional[float]:
        """Get or set the Starting value for the forward phase transformation(conversion of austenite into martensite) in the case of a uniaxial tensile state of stress.
        LT.0.0: -SIG_ASS is a load curve ID defining the starting value as a function of temperature.If LCSS is also negative, then -SIG_ASS is either a load curve specifying the starting value as a function of effective plastic strain or a table of such load curves for different temperatures
        """ # nopep8
        return self._cards[1].get_value("sig_ass")

    @sig_ass.setter
    def sig_ass(self, value: float) -> None:
        """Set the sig_ass property."""
        self._cards[1].set_value("sig_ass", value)

    @property
    def sig_asf(self) -> typing.Optional[float]:
        """Get or set the Final value for the forward phase transformation (conversion of austenite into martensite) in the case of a uniaxial tensile state of stress.
        LT.0.0: -SIG_ASF is a load curve ID defining the final value as a function of temperature. If LCSS is also negative, -SIG_ASF is either a load curve specifying the final value as a function of effective plastic strain or a table of such load curves for different temperatures
        """ # nopep8
        return self._cards[1].get_value("sig_asf")

    @sig_asf.setter
    def sig_asf(self, value: float) -> None:
        """Set the sig_asf property."""
        self._cards[1].set_value("sig_asf", value)

    @property
    def sig_sas(self) -> typing.Optional[float]:
        """Get or set the Starting value for the reverse phase transformation (conversion of martensite into austenite) in the case of a uniaxial tensile state of stress.
        LT.0.0: -SIG_SAS is a load curve ID defining the starting value as a function of temperature.If LCSS is also negative, -SIG_SAS is either a load curve specifying the starting value as a function of effective plastic strain or a table of such load curves for different temperatures
        """ # nopep8
        return self._cards[1].get_value("sig_sas")

    @sig_sas.setter
    def sig_sas(self, value: float) -> None:
        """Set the sig_sas property."""
        self._cards[1].set_value("sig_sas", value)

    @property
    def sig_saf(self) -> typing.Optional[float]:
        """Get or set the Final value for the reverse phase transformation (conversion of martensite into austenite) in the case of a uniaxial tensile state of stress.
        LT.0.0: -SIG_SAF is a load curve ID specifying the reverse value as a function of temperature.If LCSS is also negative, -SIG_SAF is either a load curve specifying the final value as a function of effective plastic strain or a table of such load curves for different temperatures.
        """ # nopep8
        return self._cards[1].get_value("sig_saf")

    @sig_saf.setter
    def sig_saf(self, value: float) -> None:
        """Set the sig_saf property."""
        self._cards[1].set_value("sig_saf", value)

    @property
    def epsl(self) -> typing.Optional[float]:
        """Get or set the Recoverable strain or maximum residual strain. It is a measure of the maximum deformation obtainable for all the martensite in one direction
        """ # nopep8
        return self._cards[1].get_value("epsl")

    @epsl.setter
    def epsl(self, value: float) -> None:
        """Set the epsl property."""
        self._cards[1].set_value("epsl", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Parameter measuring the difference between material responses in tension and compression (set ALPHA = 0 for no difference). Also, see Remark 1.
        """ # nopep8
        return self._cards[1].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[1].set_value("alpha", value)

    @property
    def ymrt(self) -> typing.Optional[float]:
        """Get or set the Young's modulus for the martensite if it is different from the modulus for the austenite. Defaults to the austenite modulus if it is set to zero.
        """ # nopep8
        return self._cards[1].get_value("ymrt")

    @ymrt.setter
    def ymrt(self, value: float) -> None:
        """Set the ymrt property."""
        self._cards[1].set_value("ymrt", value)

    @property
    def lcid_as(self) -> typing.Optional[int]:
        """Get or set the Load curve ID or table ID for the forward phase change (conversion of austenite into martensite).
        1. When LCID_AS is a load curve ID the curve is taken to be effective stress as a function of martensite fraction(ranging from 0 to 1).
        2. When LCID_AS is a table ID the table defines for each phase transition rate(derivative of martensite fraction) a load curve ID specifying the stress as a function of martensite fraction for that phase transition rate.
        The stress as a function of martensite fraction curve for the smallest value of the phase transition rate is used if the phase transition rate falls below the minimum value. Likewise, the stress as a function of martensite fraction curve for the largest value of the phase transition rate is used if the phase transition rate exceeds the maximum value.
        """ # nopep8
        return self._cards[2].get_value("lcid_as")

    @lcid_as.setter
    def lcid_as(self, value: int) -> None:
        """Set the lcid_as property."""
        self._cards[2].set_value("lcid_as", value)

    @property
    def lcid_sa(self) -> typing.Optional[int]:
        """Get or set the Load curve ID or table ID for reverse phase change (conversion of martensite into austenite).
        1. When LCID_SA is a load curve ID, the curve is taken to be effective stress as a function of martensite fraction(ranging from 0 to 1).
        2. When LCID_SA is a table ID, the table defines for each phase transition rate(derivative of martensite fraction) a load curve ID specifying the stress as a function of martensite fraction for that phase transition rate.
        The stress as a function of martensite fraction curve for the lowest value of the phase transition rate is used if the phase transition rate falls below the minimum value. Likewise, the stress as a function of martensite fraction curve for the highest value of phase transition rate is used if phase transition rate exceeds the maximum value.
        3. The values of SIG_ASS and SIG_ASF are overwritten when this option is used..
        """ # nopep8
        return self._cards[2].get_value("lcid_sa")

    @lcid_sa.setter
    def lcid_sa(self, value: int) -> None:
        """Set the lcid_sa property."""
        self._cards[2].set_value("lcid_sa", value)

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
    def lcid_as_link(self) -> typing.Optional[KeywordBase]:
        """Get the linked DEFINE_CURVE or DEFINE_TABLE for lcid_as."""
        if self.deck is None:
            return None
        field_value = self.lcid_as
        if field_value is None or field_value == 0:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == field_value:
                return kwd
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "TABLE"):
            if kwd.tbid == field_value:
                return kwd
        return None

    @lcid_as_link.setter
    def lcid_as_link(self, value: KeywordBase) -> None:
        """Set the linked keyword for lcid_as."""
        if hasattr(value, "lcid"):
            self.lcid_as = value.lcid
        elif hasattr(value, "tbid"):
            self.lcid_as = value.tbid

    @property
    def lcid_sa_link(self) -> typing.Optional[KeywordBase]:
        """Get the linked DEFINE_CURVE or DEFINE_TABLE for lcid_sa."""
        if self.deck is None:
            return None
        field_value = self.lcid_sa
        if field_value is None or field_value == 0:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == field_value:
                return kwd
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "TABLE"):
            if kwd.tbid == field_value:
                return kwd
        return None

    @lcid_sa_link.setter
    def lcid_sa_link(self, value: KeywordBase) -> None:
        """Set the linked keyword for lcid_sa."""
        if hasattr(value, "lcid"):
            self.lcid_sa = value.lcid
        elif hasattr(value, "tbid"):
            self.lcid_sa = value.tbid

