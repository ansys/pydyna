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

"""Module providing the LoadThermalVariable class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_box import DefineBox

_LOADTHERMALVARIABLE_CARD0 = (
    FieldSchema("nsid", int, 0, 10, None),
    FieldSchema("nsidex", int, 10, 10, 0),
    FieldSchema("boxid", int, 20, 10, 0),
)

_LOADTHERMALVARIABLE_CARD1 = (
    FieldSchema("ts", float, 0, 10, 0.0),
    FieldSchema("tb", float, 10, 10, 0.0),
    FieldSchema("lcid", int, 20, 10, None),
    FieldSchema("tse", float, 30, 10, 0.0),
    FieldSchema("tbe", float, 40, 10, 0.0),
    FieldSchema("lcide", int, 50, 10, None),
    FieldSchema("lcidr", int, 60, 10, None),
    FieldSchema("lcidedr", int, 70, 10, None),
)

class LoadThermalVariable(KeywordBase):
    """DYNA LOAD_THERMAL_VARIABLE keyword"""

    keyword = "LOAD"
    subkeyword = "THERMAL_VARIABLE"
    _link_fields = {
        "lcide": LinkType.DEFINE_CURVE,
        "lcidr": LinkType.DEFINE_CURVE,
        "lcidedr": LinkType.DEFINE_CURVE,
        "boxid": LinkType.DEFINE_BOX,
        "nsid": LinkType.SET_NODE,
        "nsidex": LinkType.SET_NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadThermalVariable class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADTHERMALVARIABLE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADTHERMALVARIABLE_CARD1,
                **kwargs,
            ),        ]
    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID containing nodes, see *SET_NODE:
        EQ.0: all nodes are included.
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[0].set_value("nsid", value)

    @property
    def nsidex(self) -> int:
        """Get or set the Node set ID containing nodes that are exempted (optional), see *SET_ NODE.
        """ # nopep8
        return self._cards[0].get_value("nsidex")

    @nsidex.setter
    def nsidex(self, value: int) -> None:
        """Set the nsidex property."""
        self._cards[0].set_value("nsidex", value)

    @property
    def boxid(self) -> int:
        """Get or set the All nodes in box which belong to NSID are initialized. Others are excluded.
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        """Set the boxid property."""
        self._cards[0].set_value("boxid", value)

    @property
    def ts(self) -> float:
        """Get or set the Scaled temperature.
        """ # nopep8
        return self._cards[1].get_value("ts")

    @ts.setter
    def ts(self, value: float) -> None:
        """Set the ts property."""
        self._cards[1].set_value("ts", value)

    @property
    def tb(self) -> float:
        """Get or set the Base temperature.
        """ # nopep8
        return self._cards[1].get_value("tb")

    @tb.setter
    def tb(self, value: float) -> None:
        """Set the tb property."""
        self._cards[1].set_value("tb", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID that multiplies the scaled temperature, see *DEFINE_ CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[1].set_value("lcid", value)

    @property
    def tse(self) -> float:
        """Get or set the Scaled temperature of the exempted nodes (optional).
        """ # nopep8
        return self._cards[1].get_value("tse")

    @tse.setter
    def tse(self, value: float) -> None:
        """Set the tse property."""
        self._cards[1].set_value("tse", value)

    @property
    def tbe(self) -> float:
        """Get or set the Base temperature of the exempted nodes (optional).
        """ # nopep8
        return self._cards[1].get_value("tbe")

    @tbe.setter
    def tbe(self, value: float) -> None:
        """Set the tbe property."""
        self._cards[1].set_value("tbe", value)

    @property
    def lcide(self) -> typing.Optional[int]:
        """Get or set the Load curve ID that multiplies the scaled temperature of the exempted nodes (optional), see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcide")

    @lcide.setter
    def lcide(self, value: int) -> None:
        """Set the lcide property."""
        self._cards[1].set_value("lcide", value)

    @property
    def lcidr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID that multiplies the scaled temperature for dynamic relaxation phase
        """ # nopep8
        return self._cards[1].get_value("lcidr")

    @lcidr.setter
    def lcidr(self, value: int) -> None:
        """Set the lcidr property."""
        self._cards[1].set_value("lcidr", value)

    @property
    def lcidedr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID that multiplies the scaled temperature of the exempted nodes (optional) for dynamic relaxation phase.
        """ # nopep8
        return self._cards[1].get_value("lcidedr")

    @lcidedr.setter
    def lcidedr(self, value: int) -> None:
        """Set the lcidedr property."""
        self._cards[1].set_value("lcidedr", value)

    @property
    def lcide_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcide."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcide:
                return kwd
        return None

    @lcide_link.setter
    def lcide_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcide."""
        self.lcide = value.lcid

    @property
    def lcidr_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidr."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidr:
                return kwd
        return None

    @lcidr_link.setter
    def lcidr_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidr."""
        self.lcidr = value.lcid

    @property
    def lcidedr_link(self) -> DefineCurve:
        """Get the DefineCurve object for lcidedr."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.lcidedr:
                return kwd
        return None

    @lcidedr_link.setter
    def lcidedr_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for lcidedr."""
        self.lcidedr = value.lcid

    @property
    def boxid_link(self) -> DefineBox:
        """Get the DefineBox object for boxid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "BOX"):
            if kwd.boxid == self.boxid:
                return kwd
        return None

    @boxid_link.setter
    def boxid_link(self, value: DefineBox) -> None:
        """Set the DefineBox object for boxid."""
        self.boxid = value.boxid

    @property
    def nsid_link(self) -> KeywordBase:
        """Get the SET_NODE_* keyword for nsid."""
        return self._get_set_link("NODE", self.nsid)

    @nsid_link.setter
    def nsid_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsid."""
        self.nsid = value.sid

    @property
    def nsidex_link(self) -> KeywordBase:
        """Get the SET_NODE_* keyword for nsidex."""
        return self._get_set_link("NODE", self.nsidex)

    @nsidex_link.setter
    def nsidex_link(self, value: KeywordBase) -> None:
        """Set the SET_NODE_* keyword for nsidex."""
        self.nsidex = value.sid

