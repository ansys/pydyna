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

"""Module providing the MatViscoplasticMixedHardening class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MATVISCOPLASTICMIXEDHARDENING_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
    FieldSchema("lcss", int, 40, 10, None),
    FieldSchema("beta", float, 50, 10, None),
)

_MATVISCOPLASTICMIXEDHARDENING_CARD1 = (
    FieldSchema("fail", float, 0, 10, 1e+20),
)

_MATVISCOPLASTICMIXEDHARDENING_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class MatViscoplasticMixedHardening(KeywordBase):
    """DYNA MAT_VISCOPLASTIC_MIXED_HARDENING keyword"""

    keyword = "MAT"
    subkeyword = "VISCOPLASTIC_MIXED_HARDENING"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcss": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the MatViscoplasticMixedHardening class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MATVISCOPLASTICMIXEDHARDENING_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MATVISCOPLASTICMIXEDHARDENING_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = MatViscoplasticMixedHardening.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MATVISCOPLASTICMIXEDHARDENING_OPTION0_CARD0,
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
    def lcss(self) -> typing.Optional[int]:
        """Get or set the Load curve ID or Table ID. Load curve ID defining effective stress
        versus effective plastic strain The table ID defines for each strain
        rate value a load curve ID giving the stress versus effective plastic
        strain for that rate, See Figure M24-1. The stress versus effective
        plastic strain curve for the lowest value of strain rate is used if the
        strain rate falls below the minimum value. Likewise, the stress
        versus effective plastic strain curve for the highest value of strain
        rate is used if the strain rate exceeds the maximum value. NOTE:
        The strain rate values defined in the table may be given as the
        natural logarithm of the strain rate. If the first stress-strain curve in
        the table corresponds to a negative strain rate, LS-DYNA assumes
        that the natural logarithm of the strain rate value is used. Since the
        tables are internally discretized to equally space the points, natural
        logarithms are necessary, for example, if the curves correspond to
        rates from 10.e-04 to 10.e+04.
        """ # nopep8
        return self._cards[0].get_value("lcss")

    @lcss.setter
    def lcss(self, value: int) -> None:
        """Set the lcss property."""
        self._cards[0].set_value("lcss", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the Hardening parameter, 0 < BETA < 1.
        EQ.0.0:  Pure kinematic hardening
        EQ.1.0:  Pure isotropic hardening
        0.0 < BETA < 1.0: Mixed hardening.
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[0].set_value("beta", value)

    @property
    def fail(self) -> float:
        """Get or set the Failure flag.
        LT.0.0: User defined failure subroutine is called to determine failure
        EQ.0.0: Failure is not considered. This option is recommended if failure is not of interest since many calculations will be saved.
        GT.0.0: Plastic strain to failure. When the plastic strain reach-esthis value, the element is deleted from the calculation.
        """ # nopep8
        return self._cards[1].get_value("fail")

    @fail.setter
    def fail(self, value: float) -> None:
        """Set the fail property."""
        self._cards[1].set_value("fail", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[2].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def lcss_link(self) -> typing.Optional[DefineCurve]:
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

