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

"""Module providing the EmIsopotentialConnect class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_EMISOPOTENTIALCONNECT_CARD0 = (
    FieldSchema("conid", int, 0, 10, None),
    FieldSchema("contype", int, 10, 10, 1),
    FieldSchema("isoid1", int, 20, 10, None),
    FieldSchema("isoid2", int, 30, 10, None),
    FieldSchema("val", float, 40, 10, None),
    FieldSchema("lcid_rdlid", int, 50, 10, None, "lcid/rdlid"),
    FieldSchema("psid", int, 60, 10, None),
)

_EMISOPOTENTIALCONNECT_CARD1 = (
    FieldSchema("l", float, 0, 10, None),
    FieldSchema("c", float, 10, 10, None),
    FieldSchema("v0", float, 20, 10, None),
)

class EmIsopotentialConnect(KeywordBase):
    """DYNA EM_ISOPOTENTIAL_CONNECT keyword"""

    keyword = "EM"
    subkeyword = "ISOPOTENTIAL_CONNECT"
    _link_fields = {
        "lcid_rdlid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the EmIsopotentialConnect class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMISOPOTENTIALCONNECT_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMISOPOTENTIALCONNECT_CARD1,
                active_func=lambda: self.contype == 6,
                **kwargs,
            ),        ]
    @property
    def conid(self) -> typing.Optional[int]:
        """Get or set the Connection ID.
        """ # nopep8
        return self._cards[0].get_value("conid")

    @conid.setter
    def conid(self, value: int) -> None:
        """Set the conid property."""
        self._cards[0].set_value("conid", value)

    @property
    def contype(self) -> int:
        """Get or set the Connection type:
        EQ.1:Short Circuit.
        EQ.2:Resistance.
        EQ.3:Voltage Source.
        EQ.4:Current Source.
        EQ.5:Meshless Randles circuit (used to represent a cell by one lumped Randles circuit)
        EQ.6:R, L, C circuit
        """ # nopep8
        return self._cards[0].get_value("contype")

    @contype.setter
    def contype(self, value: int) -> None:
        """Set the contype property."""
        if value not in [1, 2, 3, 4, 5, 6, None]:
            raise Exception("""contype must be `None` or one of {1,2,3,4,5,6}.""")
        self._cards[0].set_value("contype", value)

    @property
    def isoid1(self) -> typing.Optional[int]:
        """Get or set the ID of the first isopotential to be connected.
        """ # nopep8
        return self._cards[0].get_value("isoid1")

    @isoid1.setter
    def isoid1(self, value: int) -> None:
        """Set the isoid1 property."""
        self._cards[0].set_value("isoid1", value)

    @property
    def isoid2(self) -> typing.Optional[int]:
        """Get or set the ID of the second isopotential to be connected.
        """ # nopep8
        return self._cards[0].get_value("isoid2")

    @isoid2.setter
    def isoid2(self, value: int) -> None:
        """Set the isoid2 property."""
        self._cards[0].set_value("isoid2", value)

    @property
    def val(self) -> typing.Optional[float]:
        """Get or set the Value of the resistance,voltage,or current depending on CONTYPE.Ignored if LCID defined.
        """ # nopep8
        return self._cards[0].get_value("val")

    @val.setter
    def val(self, value: float) -> None:
        """Set the val property."""
        self._cards[0].set_value("val", value)

    @property
    def lcid_rdlid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining the value of the resistance,voltage,or current function of time and depending on CONTYPE.
        If not defined,VAL will be used.
        """ # nopep8
        return self._cards[0].get_value("lcid_rdlid")

    @lcid_rdlid.setter
    def lcid_rdlid(self, value: int) -> None:
        """Set the lcid_rdlid property."""
        self._cards[0].set_value("lcid_rdlid", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part Set ID where the joule heating corresponding to the resistance r0 in *EM_RANDLES_MESHLESS is added, averaged over the part set.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

    @property
    def l(self) -> typing.Optional[float]:
        """Get or set the Circuit inductance, capacity and initial voltage. Resistance is given by VAL.
        """ # nopep8
        return self._cards[1].get_value("l")

    @l.setter
    def l(self, value: float) -> None:
        """Set the l property."""
        self._cards[1].set_value("l", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Circuit inductance, capacity and initial voltage. Resistance is given by VAL.
        """ # nopep8
        return self._cards[1].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[1].set_value("c", value)

    @property
    def v0(self) -> typing.Optional[float]:
        """Get or set the Circuit inductance, capacity and initial voltage. Resistance is given by VAL.
        """ # nopep8
        return self._cards[1].get_value("v0")

    @v0.setter
    def v0(self, value: float) -> None:
        """Set the v0 property."""
        self._cards[1].set_value("v0", value)

    @property
    def lcid_rdlid_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcid_rdlid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid_rdlid:
                return kwd
        return None

    @lcid_rdlid_link.setter
    def lcid_rdlid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid_rdlid."""
        self.lcid_rdlid = value.lcid

