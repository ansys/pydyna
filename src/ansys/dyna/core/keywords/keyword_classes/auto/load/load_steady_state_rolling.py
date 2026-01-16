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

"""Module providing the LoadSteadyStateRolling class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_LOADSTEADYSTATEROLLING_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("psid", int, 10, 10, None),
)

_LOADSTEADYSTATEROLLING_CARD1 = (
    FieldSchema("n1", int, 0, 10, None),
    FieldSchema("n2", int, 10, 10, None),
    FieldSchema("lcd1", int, 20, 10, None),
    FieldSchema("lcd1r", int, 30, 10, None),
)

_LOADSTEADYSTATEROLLING_CARD2 = (
    FieldSchema("n3", int, 0, 10, None),
    FieldSchema("n4", int, 10, 10, None),
    FieldSchema("lcd2", int, 20, 10, None),
    FieldSchema("lcd2r", int, 30, 10, None),
)

_LOADSTEADYSTATEROLLING_CARD3 = (
    FieldSchema("n5", int, 0, 10, None),
    FieldSchema("n6", int, 10, 10, None),
    FieldSchema("lcd3", int, 20, 10, None),
    FieldSchema("lcd3r", int, 30, 10, None),
)

class LoadSteadyStateRolling(KeywordBase):
    """DYNA LOAD_STEADY_STATE_ROLLING keyword"""

    keyword = "LOAD"
    subkeyword = "STEADY_STATE_ROLLING"
    _link_fields = {
        "n1": LinkType.NODE,
        "n2": LinkType.NODE,
        "n3": LinkType.NODE,
        "n4": LinkType.NODE,
        "n5": LinkType.NODE,
        "n6": LinkType.NODE,
        "lcd1": LinkType.DEFINE_CURVE,
        "lcd1r": LinkType.DEFINE_CURVE,
        "lcd2": LinkType.DEFINE_CURVE,
        "lcd2r": LinkType.DEFINE_CURVE,
        "lcd3": LinkType.DEFINE_CURVE,
        "lcd3r": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadSteadyStateRolling class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADSTEADYSTATEROLLING_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADSTEADYSTATEROLLING_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADSTEADYSTATEROLLING_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADSTEADYSTATEROLLING_CARD3,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Load steady state rolling ID
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part Set ID
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Node 1 defining rotational axis
        """ # nopep8
        return self._cards[1].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[1].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Node 2 defining rotational axis
        """ # nopep8
        return self._cards[1].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[1].set_value("n2", value)

    @property
    def lcd1(self) -> typing.Optional[int]:
        """Get or set the Load curve defining angular velocity around rotational axis.
        """ # nopep8
        return self._cards[1].get_value("lcd1")

    @lcd1.setter
    def lcd1(self, value: int) -> None:
        """Set the lcd1 property."""
        self._cards[1].set_value("lcd1", value)

    @property
    def lcd1r(self) -> typing.Optional[int]:
        """Get or set the Optional load curve defining angular velocity around rotational axis for dynamic relaxation. LCD1 is used during dynamic relaxation if LCD1R isn’t defined.
        """ # nopep8
        return self._cards[1].get_value("lcd1r")

    @lcd1r.setter
    def lcd1r(self, value: int) -> None:
        """Set the lcd1r property."""
        self._cards[1].set_value("lcd1r", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Node 3 defining turning axis
        """ # nopep8
        return self._cards[2].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        """Set the n3 property."""
        self._cards[2].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Node 4 defining turning axis
        """ # nopep8
        return self._cards[2].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        """Set the n4 property."""
        self._cards[2].set_value("n4", value)

    @property
    def lcd2(self) -> typing.Optional[int]:
        """Get or set the Load curve defining angular velocity around turning axis.
        """ # nopep8
        return self._cards[2].get_value("lcd2")

    @lcd2.setter
    def lcd2(self, value: int) -> None:
        """Set the lcd2 property."""
        self._cards[2].set_value("lcd2", value)

    @property
    def lcd2r(self) -> typing.Optional[int]:
        """Get or set the Optional load curve defining angular velocity around turning axis for dynamic relaxation. LCD2 is used during dynamic relaxation if LCD2R isn’t defined
        """ # nopep8
        return self._cards[2].get_value("lcd2r")

    @lcd2r.setter
    def lcd2r(self, value: int) -> None:
        """Set the lcd2r property."""
        self._cards[2].set_value("lcd2r", value)

    @property
    def n5(self) -> typing.Optional[int]:
        """Get or set the Node 5 defining translational direction
        """ # nopep8
        return self._cards[3].get_value("n5")

    @n5.setter
    def n5(self, value: int) -> None:
        """Set the n5 property."""
        self._cards[3].set_value("n5", value)

    @property
    def n6(self) -> typing.Optional[int]:
        """Get or set the Node 6 defining translational direction
        """ # nopep8
        return self._cards[3].get_value("n6")

    @n6.setter
    def n6(self, value: int) -> None:
        """Set the n6 property."""
        self._cards[3].set_value("n6", value)

    @property
    def lcd3(self) -> typing.Optional[int]:
        """Get or set the Load curve defining translational velocity in translational direction.
        """ # nopep8
        return self._cards[3].get_value("lcd3")

    @lcd3.setter
    def lcd3(self, value: int) -> None:
        """Set the lcd3 property."""
        self._cards[3].set_value("lcd3", value)

    @property
    def lcd3r(self) -> typing.Optional[int]:
        """Get or set the Optional load curve defining translational velocity in translational direction. LCD3 is used during dynamic relaxation if LCD3R isn’t defined.
        """ # nopep8
        return self._cards[3].get_value("lcd3r")

    @lcd3r.setter
    def lcd3r(self, value: int) -> None:
        """Set the lcd3r property."""
        self._cards[3].set_value("lcd3r", value)

    @property
    def n1_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n1."""
        return self._get_link_by_attr("NODE", "nid", self.n1, "parts")

    @property
    def n2_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n2."""
        return self._get_link_by_attr("NODE", "nid", self.n2, "parts")

    @property
    def n3_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n3."""
        return self._get_link_by_attr("NODE", "nid", self.n3, "parts")

    @property
    def n4_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n4."""
        return self._get_link_by_attr("NODE", "nid", self.n4, "parts")

    @property
    def n5_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n5."""
        return self._get_link_by_attr("NODE", "nid", self.n5, "parts")

    @property
    def n6_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given n6."""
        return self._get_link_by_attr("NODE", "nid", self.n6, "parts")

    @property
    def lcd1_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcd1."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcd1:
                return kwd
        return None

    @lcd1_link.setter
    def lcd1_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcd1."""
        self.lcd1 = value.lcid

    @property
    def lcd1r_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcd1r."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcd1r:
                return kwd
        return None

    @lcd1r_link.setter
    def lcd1r_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcd1r."""
        self.lcd1r = value.lcid

    @property
    def lcd2_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcd2."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcd2:
                return kwd
        return None

    @lcd2_link.setter
    def lcd2_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcd2."""
        self.lcd2 = value.lcid

    @property
    def lcd2r_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcd2r."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcd2r:
                return kwd
        return None

    @lcd2r_link.setter
    def lcd2r_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcd2r."""
        self.lcd2r = value.lcid

    @property
    def lcd3_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcd3."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcd3:
                return kwd
        return None

    @lcd3_link.setter
    def lcd3_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcd3."""
        self.lcd3 = value.lcid

    @property
    def lcd3r_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcd3r."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcd3r:
                return kwd
        return None

    @lcd3r_link.setter
    def lcd3r_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcd3r."""
        self.lcd3r = value.lcid

