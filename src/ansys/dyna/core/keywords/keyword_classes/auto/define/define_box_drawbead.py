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

"""Module providing the DefineBoxDrawbead class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_coordinate_system import DefineCoordinateSystem

_DEFINEBOXDRAWBEAD_CARD0 = (
    FieldSchema("boxid", int, 0, 10, 0),
    FieldSchema("pid", int, 10, 10, 0),
    FieldSchema("sid", int, 20, 10, 0),
    FieldSchema("idir", int, 30, 10, 1),
    FieldSchema("stype", int, 40, 10, 4),
    FieldSchema("radius", float, 50, 10, 0.0),
    FieldSchema("cid", int, 60, 10, 0),
)

_DEFINEBOXDRAWBEAD_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineBoxDrawbead(KeywordBase):
    """DYNA DEFINE_BOX_DRAWBEAD keyword"""

    keyword = "DEFINE"
    subkeyword = "BOX_DRAWBEAD"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "cid": LinkType.DEFINE_COORDINATE_SYSTEM,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineBoxDrawbead class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEBOXDRAWBEAD_CARD0,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineBoxDrawbead.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEBOXDRAWBEAD_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def boxid(self) -> int:
        """Get or set the Box ID. Define unique numbers.
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        """Set the boxid property."""
        self._cards[0].set_value("boxid", value)

    @property
    def pid(self) -> int:
        """Get or set the Part ID of blank.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def sid(self) -> int:
        """Get or set the set ID defining along the drawbead.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def idir(self) -> int:
        """Get or set the Direction of tooling movement:
        EQ.1: tooling moves in x-direction (default),
        EQ.2: tooling moves in y-direction,
        EQ.3: tooling moves in z-direction.
        """ # nopep8
        return self._cards[0].get_value("idir")

    @idir.setter
    def idir(self, value: int) -> None:
        """Set the idir property."""
        if value not in [1, 2, 3, None]:
            raise Exception("""idir must be `None` or one of {1,2,3}.""")
        self._cards[0].set_value("idir", value)

    @property
    def stype(self) -> int:
        """Get or set the Set type:
        EQ.2:  part set ID,
        EQ.3:  part ID,
        EQ.4:  node set ID.
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        """Set the stype property."""
        if value not in [4, 2, 3, 0, None]:
            raise Exception("""stype must be `None` or one of {4,2,3,0}.""")
        self._cards[0].set_value("stype", value)

    @property
    def radius(self) -> float:
        """Get or set the The radius of the tube, which is centered around the draw bead.  Elements of part ID, PID, that lie within the tube will be included in the contact.    If the radius is not defined, a rectangular box is used instead.  This option is recommended for curved draw beads and for draw beads that are not aligned with the global axes.
        """ # nopep8
        return self._cards[0].get_value("radius")

    @radius.setter
    def radius(self, value: float) -> None:
        """Set the radius property."""
        self._cards[0].set_value("radius", value)

    @property
    def cid(self) -> int:
        """Get or set the Optional coordinate system ID. This optional is only available for the tubular drawbead
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

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

    @property
    def pid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

