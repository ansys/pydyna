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

"""Module providing the ControlFormingProjection class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_CONTROLFORMINGPROJECTION_CARD0 = (
    FieldSchema("pidb", int, 0, 10, None),
    FieldSchema("pidt", int, 10, 10, None),
    FieldSchema("gap", float, 20, 10, None),
    FieldSchema("nrbst", int, 30, 10, 0),
    FieldSchema("nrtst", int, 40, 10, 0),
)

class ControlFormingProjection(KeywordBase):
    """DYNA CONTROL_FORMING_PROJECTION keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_PROJECTION"
    _link_fields = {
        "pidb": LinkType.PART,
        "pidt": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ControlFormingProjection class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLFORMINGPROJECTION_CARD0,
                **kwargs,
            ),        ]
    @property
    def pidb(self) -> typing.Optional[int]:
        """Get or set the Part ID for the blank.
        """ # nopep8
        return self._cards[0].get_value("pidb")

    @pidb.setter
    def pidb(self, value: int) -> None:
        """Set the pidb property."""
        self._cards[0].set_value("pidb", value)

    @property
    def pidt(self) -> typing.Optional[int]:
        """Get or set the Part ID for the tool.
        """ # nopep8
        return self._cards[0].get_value("pidt")

    @pidt.setter
    def pidt(self, value: int) -> None:
        """Set the pidt property."""
        self._cards[0].set_value("pidt", value)

    @property
    def gap(self) -> typing.Optional[float]:
        """Get or set the A distance, which defines the minimum gap required.
        """ # nopep8
        return self._cards[0].get_value("gap")

    @gap.setter
    def gap(self, value: float) -> None:
        """Set the gap property."""
        self._cards[0].set_value("gap", value)

    @property
    def nrbst(self) -> int:
        """Get or set the Specify whether the blank will move along its normal direction. If its moves along the normal of blank, then this flag also specifies the direction the normal is pointing with respect to the tool.
        EQ.0: Move the blank’s nodes along the blank’s normal.The normal to the surface of the blank is pointing towards the tool.
        EQ.1 : Move the blank’s nodes along the blank’s normal.The normal to the surface of the blank is pointing away from the tool.
        EQ.2 : Move the blank nodes along the tool's normal direction.This case is useful for contact between a guide pinand blank.
        """ # nopep8
        return self._cards[0].get_value("nrbst")

    @nrbst.setter
    def nrbst(self, value: int) -> None:
        """Set the nrbst property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""nrbst must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("nrbst", value)

    @property
    def nrtst(self) -> int:
        """Get or set the Normal direction of tool:
        EQ.0: the normal to the surface of the tool is pointing towards the blank,
        EQ.1: the normal to the surface of the tool is pointing away from blank.
        """ # nopep8
        return self._cards[0].get_value("nrtst")

    @nrtst.setter
    def nrtst(self, value: int) -> None:
        """Set the nrtst property."""
        if value not in [0, 1, None]:
            raise Exception("""nrtst must be `None` or one of {0,1}.""")
        self._cards[0].set_value("nrtst", value)

    @property
    def pidb_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pidb."""
        return self._get_link_by_attr("PART", "pid", self.pidb, "parts")

    @property
    def pidt_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pidt."""
        return self._get_link_by_attr("PART", "pid", self.pidt, "parts")

