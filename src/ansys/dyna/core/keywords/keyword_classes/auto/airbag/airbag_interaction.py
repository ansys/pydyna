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

"""Module providing the AirbagInteraction class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_AIRBAGINTERACTION_CARD0 = (
    FieldSchema("ab1", int, 0, 10, None),
    FieldSchema("ab2", int, 10, 10, None),
    FieldSchema("area", float, 20, 10, None),
    FieldSchema("sf", float, 30, 10, None),
    FieldSchema("pid", int, 40, 10, 0),
    FieldSchema("lcid", int, 50, 10, 0),
    FieldSchema("iflow", int, 60, 10, 0),
)

class AirbagInteraction(KeywordBase):
    """DYNA AIRBAG_INTERACTION keyword"""

    keyword = "AIRBAG"
    subkeyword = "INTERACTION"
    _link_fields = {
        "lcid": LinkType.DEFINE_CURVE,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the AirbagInteraction class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _AIRBAGINTERACTION_CARD0,
                **kwargs,
            ),        ]
    @property
    def ab1(self) -> typing.Optional[int]:
        """Get or set the First airbag ID, as defined on *AIRBAG card.
        """ # nopep8
        return self._cards[0].get_value("ab1")

    @ab1.setter
    def ab1(self, value: int) -> None:
        """Set the ab1 property."""
        self._cards[0].set_value("ab1", value)

    @property
    def ab2(self) -> typing.Optional[int]:
        """Get or set the Second airbag ID, as defined on *AIRBAG card.
        """ # nopep8
        return self._cards[0].get_value("ab2")

    @ab2.setter
    def ab2(self, value: int) -> None:
        """Set the ab2 property."""
        self._cards[0].set_value("ab2", value)

    @property
    def area(self) -> typing.Optional[float]:
        """Get or set the Orifice area between connected bags.
        LT.0.0: |AREA| is the load curve ID defining the orifice area as a function of absolute pressure,
        EQ.0.0: AREA is taken as the surface area of the part ID defined below.
        """ # nopep8
        return self._cards[0].get_value("area")

    @area.setter
    def area(self, value: float) -> None:
        """Set the area property."""
        self._cards[0].set_value("area", value)

    @property
    def sf(self) -> typing.Optional[float]:
        """Get or set the Shape factor.
        LT.0.0: |SF| is the load curve ID defining vent orifice coefficient as a function of relative time.
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[0].set_value("sf", value)

    @property
    def pid(self) -> int:
        """Get or set the Optional part ID of the partition between the interacting control volumes. AREA is based on this part ID.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def lcid(self) -> int:
        """Get or set the Load curve ID defining mass flow rate versus pressure difference, see *DEFINE_CURVE. If LCID is defined AREA, SF and PID are ignored.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def iflow(self) -> int:
        """Get or set the Flow direction.
        LT.0: One-way flow from AB1 to AB2 only,
        EQ.0: Two-way flow between AB1 and AB2,
        GT.0: One-way flow from AB2 to AB1 only.
        """ # nopep8
        return self._cards[0].get_value("iflow")

    @iflow.setter
    def iflow(self, value: int) -> None:
        """Set the iflow property."""
        self._cards[0].set_value("iflow", value)

    @property
    def lcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for lcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcid:
                return kwd
        return None

    @lcid_link.setter
    def lcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcid."""
        self.lcid = value.lcid

    @property
    def pid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

