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

"""Module providing the SetPartListGenerateIncrement class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_SETPARTLISTGENERATEINCREMENT_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("da1", float, 10, 10, 0.0),
    FieldSchema("da2", float, 20, 10, 0.0),
    FieldSchema("da3", float, 30, 10, 0.0),
    FieldSchema("da4", float, 40, 10, 0.0),
    FieldSchema("solver", str, 50, 10, "MECH"),
)

_SETPARTLISTGENERATEINCREMENT_CARD1 = (
    FieldSchema("bbeg", int, 0, 10, None),
    FieldSchema("bend", int, 10, 10, None),
    FieldSchema("incr", int, 20, 10, None),
)

_SETPARTLISTGENERATEINCREMENT_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SetPartListGenerateIncrement(KeywordBase):
    """DYNA SET_PART_LIST_GENERATE_INCREMENT keyword"""

    keyword = "SET"
    subkeyword = "PART_LIST_GENERATE_INCREMENT"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "bbeg": LinkType.PART,
        "bend": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the SetPartListGenerateIncrement class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SETPARTLISTGENERATEINCREMENT_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _SETPARTLISTGENERATEINCREMENT_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = SetPartListGenerateIncrement.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SETPARTLISTGENERATEINCREMENT_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Part set ID. All part sets should have a unique set ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def da1(self) -> float:
        """Get or set the First attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da1")

    @da1.setter
    def da1(self, value: float) -> None:
        """Set the da1 property."""
        self._cards[0].set_value("da1", value)

    @property
    def da2(self) -> float:
        """Get or set the Second attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da2")

    @da2.setter
    def da2(self, value: float) -> None:
        """Set the da2 property."""
        self._cards[0].set_value("da2", value)

    @property
    def da3(self) -> float:
        """Get or set the Third attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da3")

    @da3.setter
    def da3(self, value: float) -> None:
        """Set the da3 property."""
        self._cards[0].set_value("da3", value)

    @property
    def da4(self) -> float:
        """Get or set the Fourth attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da4")

    @da4.setter
    def da4(self, value: float) -> None:
        """Set the da4 property."""
        self._cards[0].set_value("da4", value)

    @property
    def solver(self) -> str:
        """Get or set the EQ.MECH: mechanics.
        EQ.CESE: CE/SE compressible fluid flow solver.
        EQ.ICFD: Incompressible fluid flow solver.
        """ # nopep8
        return self._cards[0].get_value("solver")

    @solver.setter
    def solver(self, value: str) -> None:
        """Set the solver property."""
        if value not in ["MECH", "CESE", "ICFD", None]:
            raise Exception("""solver must be `None` or one of {"MECH","CESE","ICFD"}.""")
        self._cards[0].set_value("solver", value)

    @property
    def bbeg(self) -> typing.Optional[int]:
        """Get or set the First part ID in block.
        """ # nopep8
        return self._cards[1].get_value("bbeg")

    @bbeg.setter
    def bbeg(self, value: int) -> None:
        """Set the bbeg property."""
        self._cards[1].set_value("bbeg", value)

    @property
    def bend(self) -> typing.Optional[int]:
        """Get or set the Last part ID in block.
        """ # nopep8
        return self._cards[1].get_value("bend")

    @bend.setter
    def bend(self, value: int) -> None:
        """Set the bend property."""
        self._cards[1].set_value("bend", value)

    @property
    def incr(self) -> typing.Optional[int]:
        """Get or set the Part ID increment. Part IDs BBEG, BBEG+INCR, BBEG + 2*INCR, and so on through BEND are added to the set.
        """ # nopep8
        return self._cards[1].get_value("incr")

    @incr.setter
    def incr(self, value: int) -> None:
        """Set the incr property."""
        self._cards[1].set_value("incr", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[2].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

    @property
    def bbeg_link(self) -> KeywordBase:
        """Get the PART keyword containing the given bbeg."""
        return self._get_link_by_attr("PART", "pid", self.bbeg, "parts")

    @property
    def bend_link(self) -> KeywordBase:
        """Get the PART keyword containing the given bend."""
        return self._get_link_by_attr("PART", "pid", self.bend, "parts")

