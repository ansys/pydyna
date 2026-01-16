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

"""Module providing the DefineElementDeathSolidSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_box import DefineBox
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_DEFINEELEMENTDEATHSOLIDSET_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("time", float, 10, 10, 0.0),
    FieldSchema("boxid", int, 20, 10, None),
    FieldSchema("inout", int, 30, 10, 0),
    FieldSchema("idgrp", int, 40, 10, 0),
    FieldSchema("cid", int, 50, 10, 0),
    FieldSchema("percent", float, 60, 10, 0.0),
)

_DEFINEELEMENTDEATHSOLIDSET_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineElementDeathSolidSet(KeywordBase):
    """DYNA DEFINE_ELEMENT_DEATH_SOLID_SET keyword"""

    keyword = "DEFINE"
    subkeyword = "ELEMENT_DEATH_SOLID_SET"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "boxid": LinkType.DEFINE_BOX,
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineElementDeathSolidSet class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEELEMENTDEATHSOLIDSET_CARD0,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineElementDeathSolidSet.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEELEMENTDEATHSOLIDSET_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Element Set ID
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def time(self) -> float:
        """Get or set the Deletion time for elimination of the element or element set. If BOXID is nonzero, a TIME value of zero is restt to 1.0E+16.
        """ # nopep8
        return self._cards[0].get_value("time")

    @time.setter
    def time(self, value: float) -> None:
        """Set the time property."""
        self._cards[0].set_value("time", value)

    @property
    def boxid(self) -> typing.Optional[int]:
        """Get or set the Element inside or outside of defined box are deleted depending on the value INOUT
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        """Set the boxid property."""
        self._cards[0].set_value("boxid", value)

    @property
    def inout(self) -> int:
        """Get or set the Location of deleted element:
        EQ.0:Element inside box are deleted.
        EQ.1:Element outside of box are deleted
        """ # nopep8
        return self._cards[0].get_value("inout")

    @inout.setter
    def inout(self, value: int) -> None:
        """Set the inout property."""
        if value not in [0, 1, None]:
            raise Exception("""inout must be `None` or one of {0,1}.""")
        self._cards[0].set_value("inout", value)

    @property
    def idgrp(self) -> int:
        """Get or set the Group ID. Elements sharing the same positive value of IDGRP
        are considered to be in the same group. All elements in a group
        will be simultaneously deleted one cycle after a percentage of the elements (specified in PERCENT) fail.
        There is no requirement that each *DEFINE_ELEMENT_DEATH
        command have a unique IDGRP. In other words, elements in a
        single group can come from multiple *DEFINE_ELEMENT_DEATH commands.
        Elements in which IDGRP = 0 are not assigned to a group and
        thus deletion of one element does not cause deletion of the other elements.
        """ # nopep8
        return self._cards[0].get_value("idgrp")

    @idgrp.setter
    def idgrp(self, value: int) -> None:
        """Set the idgrp property."""
        self._cards[0].set_value("idgrp", value)

    @property
    def cid(self) -> int:
        """Get or set the Coordinate ID for transforming box BOXID. If CID is not
        specified, the box is in the global coordinate system. The box
        rotates and translates with the coordinate system only if the
        coordinate system is flagged for an update every time step
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def percent(self) -> float:
        """Get or set the Deletion percentage.
        EQ.0.0: When one element fails, all elements in the group will be deleted (default).
        GT.0.0: Percentage of elements failed before elements in group IDGRP are deleted
        """ # nopep8
        return self._cards[0].get_value("percent")

    @percent.setter
    def percent(self, value: float) -> None:
        """Set the percent property."""
        self._cards[0].set_value("percent", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[1].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

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
    def cid_link(self) -> DefineCoordinateSystem:
        """Get the DefineCoordinateSystem object for cid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "COORDINATE_SYSTEM"):
            if kwd.cid == self.cid:
                return kwd
        return None

    @cid_link.setter
    def cid_link(self, value: DefineCoordinateSystem) -> None:
        """Set the DefineCoordinateSystem object for cid."""
        self.cid = value.cid

