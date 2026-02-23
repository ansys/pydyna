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

"""Module providing the LoadSuperplasticForming class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_LOADSUPERPLASTICFORMING_CARD0 = (
    FieldSchema("lcp1", int, 0, 10, None),
    FieldSchema("csp1", int, 10, 10, None),
    FieldSchema("ncp1", float, 20, 10, None),
    FieldSchema("lcp2", int, 30, 10, None),
    FieldSchema("csp2", int, 40, 10, None),
    FieldSchema("ncp2", float, 50, 10, None),
)

_LOADSUPERPLASTICFORMING_CARD1 = (
    FieldSchema("erate", float, 0, 10, None),
    FieldSchema("scmin", float, 10, 10, None),
    FieldSchema("scmax", float, 20, 10, None),
    FieldSchema("ncyl", int, 30, 10, 0),
)

class LoadSuperplasticForming(KeywordBase):
    """DYNA LOAD_SUPERPLASTIC_FORMING keyword"""

    keyword = "LOAD"
    subkeyword = "SUPERPLASTIC_FORMING"
    _link_fields = {
        "lcp1": LinkType.DEFINE_CURVE,
        "lcp2": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadSuperplasticForming class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADSUPERPLASTICFORMING_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADSUPERPLASTICFORMING_CARD1,
                **kwargs,
            ),        ]
    @property
    def lcp1(self) -> typing.Optional[int]:
        """Get or set the Load curve number for Phase I pressure loading, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[0].get_value("lcp1")

    @lcp1.setter
    def lcp1(self, value: int) -> None:
        """Set the lcp1 property."""
        self._cards[0].set_value("lcp1", value)

    @property
    def csp1(self) -> typing.Optional[int]:
        """Get or set the Contact surface number to determine completion of Phase 1.
        """ # nopep8
        return self._cards[0].get_value("csp1")

    @csp1.setter
    def csp1(self, value: int) -> None:
        """Set the csp1 property."""
        self._cards[0].set_value("csp1", value)

    @property
    def ncp1(self) -> typing.Optional[float]:
        """Get or set the Percent of nodes in contact to terminate Phase I, see *CONTACT_OPTION.
        """ # nopep8
        return self._cards[0].get_value("ncp1")

    @ncp1.setter
    def ncp1(self, value: float) -> None:
        """Set the ncp1 property."""
        self._cards[0].set_value("ncp1", value)

    @property
    def lcp2(self) -> typing.Optional[int]:
        """Get or set the Load curve number for Phase II pressure loading (reverse), see *DEFINE_ CURVE.
        """ # nopep8
        return self._cards[0].get_value("lcp2")

    @lcp2.setter
    def lcp2(self, value: int) -> None:
        """Set the lcp2 property."""
        self._cards[0].set_value("lcp2", value)

    @property
    def csp2(self) -> typing.Optional[int]:
        """Get or set the Contact surface number to determine completion of Phase II, see *CONTACT_OPTION.
        """ # nopep8
        return self._cards[0].get_value("csp2")

    @csp2.setter
    def csp2(self, value: int) -> None:
        """Set the csp2 property."""
        self._cards[0].set_value("csp2", value)

    @property
    def ncp2(self) -> typing.Optional[float]:
        """Get or set the Percent of nodes in contact to terminate Phase II.
        """ # nopep8
        return self._cards[0].get_value("ncp2")

    @ncp2.setter
    def ncp2(self, value: float) -> None:
        """Set the ncp2 property."""
        self._cards[0].set_value("ncp2", value)

    @property
    def erate(self) -> typing.Optional[float]:
        """Get or set the Desired strain rate. This is the time derivative of the logarithmic strain.
        """ # nopep8
        return self._cards[1].get_value("erate")

    @erate.setter
    def erate(self, value: float) -> None:
        """Set the erate property."""
        self._cards[1].set_value("erate", value)

    @property
    def scmin(self) -> typing.Optional[float]:
        """Get or set the Minimum allowable value for load curve scale factor. To maintain a constant strain rate the pressure curve is scaled. In the case of a snap through buckling the pressure may be removed completely. By putting a value here the pressure will continue to act but at a value given by this scale factor multiplying the pressure curve.
        """ # nopep8
        return self._cards[1].get_value("scmin")

    @scmin.setter
    def scmin(self, value: float) -> None:
        """Set the scmin property."""
        self._cards[1].set_value("scmin", value)

    @property
    def scmax(self) -> typing.Optional[float]:
        """Get or set the Maximum allowable value for load curve scale factor. Generally, it is a good idea to put a value here to keep the pressure from going to unreasonable values after full contact has been attained. When full contact is achieved the strain rates will approach zero and pressure will go to infinity unless it is limited or the calculation terminates.
        """ # nopep8
        return self._cards[1].get_value("scmax")

    @scmax.setter
    def scmax(self, value: float) -> None:
        """Set the scmax property."""
        self._cards[1].set_value("scmax", value)

    @property
    def ncyl(self) -> int:
        """Get or set the Number of cycles for monotonic pressure after reversal.
        """ # nopep8
        return self._cards[1].get_value("ncyl")

    @ncyl.setter
    def ncyl(self, value: int) -> None:
        """Set the ncyl property."""
        self._cards[1].set_value("ncyl", value)

    @property
    def lcp1_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcp1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcp1:
                return kwd
        return None

    @lcp1_link.setter
    def lcp1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcp1."""
        self.lcp1 = value.lcid

    @property
    def lcp2_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcp2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcp2:
                return kwd
        return None

    @lcp2_link.setter
    def lcp2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcp2."""
        self.lcp2 = value.lcid

