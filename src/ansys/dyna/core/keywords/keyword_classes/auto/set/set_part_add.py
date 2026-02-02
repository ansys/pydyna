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

"""Module providing the SetPartAdd class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.series_card import SeriesCard
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_SETPARTADD_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("da1", float, 10, 10, 0.0),
    FieldSchema("da2", float, 20, 10, 0.0),
    FieldSchema("da3", float, 30, 10, 0.0),
    FieldSchema("da4", float, 40, 10, 0.0),
    FieldSchema("solver", str, 50, 10, "MECH"),
)

_SETPARTADD_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class SetPartAdd(KeywordBase):
    """DYNA SET_PART_ADD keyword"""

    keyword = "SET"
    subkeyword = "PART_ADD"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "psid1": LinkType.SET_PART,
        "psid2": LinkType.SET_PART,
        "psid3": LinkType.SET_PART,
        "psid4": LinkType.SET_PART,
        "psid5": LinkType.SET_PART,
        "psid6": LinkType.SET_PART,
        "psid7": LinkType.SET_PART,
        "psid8": LinkType.SET_PART,
    }

    def __init__(self, **kwargs):
        """Initialize the SetPartAdd class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _SETPARTADD_CARD0,
                **kwargs,
            ),            SeriesCard(
                "parts",
                8,
                10,
                int,
                None,
                data = kwargs.get("parts")),            OptionCardSet(
                option_spec = SetPartAdd.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _SETPARTADD_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set ID,All part sets should have a unique set ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def da1(self) -> float:
        """Get or set the First nodal attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da1")

    @da1.setter
    def da1(self, value: float) -> None:
        """Set the da1 property."""
        self._cards[0].set_value("da1", value)

    @property
    def da2(self) -> float:
        """Get or set the Second nodal attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da2")

    @da2.setter
    def da2(self, value: float) -> None:
        """Set the da2 property."""
        self._cards[0].set_value("da2", value)

    @property
    def da3(self) -> float:
        """Get or set the Third nodal attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da3")

    @da3.setter
    def da3(self, value: float) -> None:
        """Set the da3 property."""
        self._cards[0].set_value("da3", value)

    @property
    def da4(self) -> float:
        """Get or set the Fourth nodal attribute default value is 0.0.
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
    def parts(self) -> SeriesCard:
        """dynamic array of part set ids.."""
        return self._cards[1]

    @parts.setter
    def parts(self, value: typing.List) -> None:
        self._cards[1].data = value

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
    def psid1_link(self) -> KeywordBase:
        """Get the SET_PART_* keyword for psid1."""
        return self._get_set_link("PART", self.psid1)

    @psid1_link.setter
    def psid1_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psid1."""
        self.psid1 = value.sid

    @property
    def psid2_link(self) -> KeywordBase:
        """Get the SET_PART_* keyword for psid2."""
        return self._get_set_link("PART", self.psid2)

    @psid2_link.setter
    def psid2_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psid2."""
        self.psid2 = value.sid

    @property
    def psid3_link(self) -> KeywordBase:
        """Get the SET_PART_* keyword for psid3."""
        return self._get_set_link("PART", self.psid3)

    @psid3_link.setter
    def psid3_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psid3."""
        self.psid3 = value.sid

    @property
    def psid4_link(self) -> KeywordBase:
        """Get the SET_PART_* keyword for psid4."""
        return self._get_set_link("PART", self.psid4)

    @psid4_link.setter
    def psid4_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psid4."""
        self.psid4 = value.sid

    @property
    def psid5_link(self) -> KeywordBase:
        """Get the SET_PART_* keyword for psid5."""
        return self._get_set_link("PART", self.psid5)

    @psid5_link.setter
    def psid5_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psid5."""
        self.psid5 = value.sid

    @property
    def psid6_link(self) -> KeywordBase:
        """Get the SET_PART_* keyword for psid6."""
        return self._get_set_link("PART", self.psid6)

    @psid6_link.setter
    def psid6_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psid6."""
        self.psid6 = value.sid

    @property
    def psid7_link(self) -> KeywordBase:
        """Get the SET_PART_* keyword for psid7."""
        return self._get_set_link("PART", self.psid7)

    @psid7_link.setter
    def psid7_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psid7."""
        self.psid7 = value.sid

    @property
    def psid8_link(self) -> KeywordBase:
        """Get the SET_PART_* keyword for psid8."""
        return self._get_set_link("PART", self.psid8)

    @psid8_link.setter
    def psid8_link(self, value: KeywordBase) -> None:
        """Set the SET_PART_* keyword for psid8."""
        self.psid8 = value.sid

