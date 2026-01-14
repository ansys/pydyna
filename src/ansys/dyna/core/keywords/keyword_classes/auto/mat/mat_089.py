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

"""Module providing the Mat089 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_MAT089_CARD0 = (
    FieldSchema("mid", int, 0, 10, None),
    FieldSchema("ro", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("pr", float, 30, 10, None),
)

_MAT089_CARD1 = (
    FieldSchema("c", float, 0, 10, None),
    FieldSchema("p", float, 10, 10, None),
    FieldSchema("lcss", int, 20, 10, 0),
    FieldSchema("lcsr", int, 30, 10, 0),
)

_MAT089_CARD2 = (
    FieldSchema("eftx", float, 0, 10, 0.0),
    FieldSchema("damp", float, 10, 10, None),
    FieldSchema("ratefac", float, 20, 10, None),
    FieldSchema("lcfail", int, 30, 10, 0),
    FieldSchema("numint", float, 40, 10, 0.0),
)

_MAT089_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class Mat089(KeywordBase):
    """DYNA MAT_089 keyword"""

    keyword = "MAT"
    subkeyword = "089"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "lcss": LinkType.DEFINE_CURVE,
        "lcsr": LinkType.DEFINE_CURVE,
        "lcfail": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the Mat089 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _MAT089_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT089_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _MAT089_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = Mat089.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _MAT089_OPTION0_CARD0,
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
    def c(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter, C.
        """ # nopep8
        return self._cards[1].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[1].set_value("c", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the Strain rate parameter, P.
        """ # nopep8
        return self._cards[1].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        """Set the p property."""
        self._cards[1].set_value("p", value)

    @property
    def lcss(self) -> int:
        """Get or set the Load curve ID defining effective stress versus total effective strain. The stress versus effective plastic strain curve for the lowest value of strain rate is used if the strain rate falls below the minimum value. Likewise, the stress versus effective plastic strain curve for the highest value of strain rate is used if the strain rate exceeds the maximum value.
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
    def eftx(self) -> float:
        """Get or set the Failure flag:
        EQ.0.0: failure determined by maximum tensile strain (default),
        EQ.1.0: failure determined only by tensile strain in local x direction,
        EQ.2.0: failure determined only by tensile strain in local y direction.
        """ # nopep8
        return self._cards[2].get_value("eftx")

    @eftx.setter
    def eftx(self, value: float) -> None:
        """Set the eftx property."""
        if value not in [0.0, 1.0, 2.0, None]:
            raise Exception("""eftx must be `None` or one of {0.0,1.0,2.0}.""")
        self._cards[2].set_value("eftx", value)

    @property
    def damp(self) -> typing.Optional[float]:
        """Get or set the Stiffness-propotional damping ratio. Typical values are 1e-3 or 1e-4. If set too high instabilites can result.
        """ # nopep8
        return self._cards[2].get_value("damp")

    @damp.setter
    def damp(self, value: float) -> None:
        """Set the damp property."""
        self._cards[2].set_value("damp", value)

    @property
    def ratefac(self) -> typing.Optional[float]:
        """Get or set the Filtering factor for strain rate effects. Must be between 0 (no filtering) and 1 (infinite filtering) The filter is a simple low pass filter to remove high frequency oscillation from the strain rates before they are used in rate effect calculations. The cut off frequency of the filter is [(1 - RATEFAC)/ timestep] rad/sec.
        """ # nopep8
        return self._cards[2].get_value("ratefac")

    @ratefac.setter
    def ratefac(self, value: float) -> None:
        """Set the ratefac property."""
        self._cards[2].set_value("ratefac", value)

    @property
    def lcfail(self) -> int:
        """Get or set the Load curve ID giving variation of failure strain with strain rate. The points on the x-axis should be natural log of strain rate, the y-axis should be the true strain to failure. Typically this is measured by uniaxial tensile test, and the strain values converted to true strain.
        """ # nopep8
        return self._cards[2].get_value("lcfail")

    @lcfail.setter
    def lcfail(self, value: int) -> None:
        """Set the lcfail property."""
        self._cards[2].set_value("lcfail", value)

    @property
    def numint(self) -> float:
        """Get or set the Number of integration points which must fail before the element is deleted. This option is available for shells only.
        LT.0.0:	|NUMINT| is percentage of integration points/layers which must fail before shell element fails.
        """ # nopep8
        return self._cards[2].get_value("numint")

    @numint.setter
    def numint(self, value: float) -> None:
        """Set the numint property."""
        self._cards[2].set_value("numint", value)

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
    def lcfail_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcfail."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcfail:
                return kwd
        return None

    @lcfail_link.setter
    def lcfail_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcfail."""
        self.lcfail = value.lcid

