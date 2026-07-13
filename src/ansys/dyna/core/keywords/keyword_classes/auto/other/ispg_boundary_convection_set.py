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

"""Module providing the IspgBoundaryConvectionSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_ISPGBOUNDARYCONVECTIONSET_CARD0 = (
    FieldSchema("fp", int, 0, 10, None),
    FieldSchema("fptype", int, 10, 10, 0),
)

_ISPGBOUNDARYCONVECTIONSET_CARD1 = (
    FieldSchema("hlcid", int, 0, 10, None),
    FieldSchema("hmult", float, 10, 10, None),
    FieldSchema("tlcid", int, 20, 10, None),
    FieldSchema("tmult", float, 30, 10, None),
)

class IspgBoundaryConvectionSet(KeywordBase):
    """DYNA ISPG_BOUNDARY_CONVECTION_SET keyword"""

    keyword = "ISPG"
    subkeyword = "BOUNDARY_CONVECTION_SET"
    _link_fields = {
        "hlcid": LinkType.DEFINE_CURVE,
        "tlcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the IspgBoundaryConvectionSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ISPGBOUNDARYCONVECTIONSET_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ISPGBOUNDARYCONVECTIONSET_CARD1,
                **kwargs,
            ),
        ]
    @property
    def fp(self) -> typing.Optional[int]:
        """Get or set the Part ID for the fluid particles
        """ # nopep8
        return self._cards[0].get_value("fp")

    @fp.setter
    def fp(self, value: int) -> None:
        """Set the fp property."""
        self._cards[0].set_value("fp", value)

    @property
    def fptype(self) -> int:
        """Get or set the Type for FP:
        EQ.0:	Part set ID
        EQ.1 : Part ID
        """ # nopep8
        return self._cards[0].get_value("fptype")

    @fptype.setter
    def fptype(self, value: int) -> None:
        """Set the fptype property."""
        if value not in [0, 1, None]:
            raise Exception("""fptype must be `None` or one of {0,1}.""")
        self._cards[0].set_value("fptype", value)

    @property
    def hlcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for the curve giving the convection heat transfer coefficient, h, as a function of time.
        EQ.0:	h is a constant defined by the value HMULT.
        """ # nopep8
        return self._cards[1].get_value("hlcid")

    @hlcid.setter
    def hlcid(self, value: int) -> None:
        """Set the hlcid property."""
        self._cards[1].set_value("hlcid", value)

    @property
    def hmult(self) -> typing.Optional[float]:
        """Get or set the Convection heat transfer coefficient, h. Ignored if HLCID > 0.
        """ # nopep8
        return self._cards[1].get_value("hmult")

    @hmult.setter
    def hmult(self, value: float) -> None:
        """Set the hmult property."""
        self._cards[1].set_value("hmult", value)

    @property
    def tlcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for the curve giving the environment temperature, T_?, as a function of time.
        EQ.0:	T_? is a constant given by the value TMULT.
        """ # nopep8
        return self._cards[1].get_value("tlcid")

    @tlcid.setter
    def tlcid(self, value: int) -> None:
        """Set the tlcid property."""
        self._cards[1].set_value("tlcid", value)

    @property
    def tmult(self) -> typing.Optional[float]:
        """Get or set the Environment temperature, T_?. Ignored if TLCID > 0.
        """ # nopep8
        return self._cards[1].get_value("tmult")

    @tmult.setter
    def tmult(self, value: float) -> None:
        """Set the tmult property."""
        self._cards[1].set_value("tmult", value)

    @property
    def hlcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for hlcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.hlcid:
                return kwd
        return None

    @hlcid_link.setter
    def hlcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for hlcid."""
        self.hlcid = value.lcid

    @property
    def tlcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for tlcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.tlcid:
                return kwd
        return None

    @tlcid_link.setter
    def tlcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for tlcid."""
        self.tlcid = value.lcid

