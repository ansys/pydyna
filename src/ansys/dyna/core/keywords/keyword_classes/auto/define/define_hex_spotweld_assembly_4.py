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

"""Module providing the DefineHexSpotweldAssembly4 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DEFINEHEXSPOTWELDASSEMBLY4_CARD0 = (
    FieldSchema("id_sw", int, 0, 10, None),
)

_DEFINEHEXSPOTWELDASSEMBLY4_CARD1 = (
    FieldSchema("eid1", int, 0, 10, None),
    FieldSchema("eid2", int, 10, 10, None),
    FieldSchema("eid3", int, 20, 10, None),
    FieldSchema("eid4", int, 30, 10, None),
    FieldSchema("eid5", int, 40, 10, None),
    FieldSchema("eid6", int, 50, 10, None),
    FieldSchema("eid7", int, 60, 10, None),
    FieldSchema("eid8", int, 70, 10, None),
)

_DEFINEHEXSPOTWELDASSEMBLY4_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineHexSpotweldAssembly4(KeywordBase):
    """DYNA DEFINE_HEX_SPOTWELD_ASSEMBLY_4 keyword"""

    keyword = "DEFINE"
    subkeyword = "HEX_SPOTWELD_ASSEMBLY_4"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]
    _link_fields = {
        "eid1": LinkType.ELEMENT_SOLID,
        "eid2": LinkType.ELEMENT_SOLID,
        "eid3": LinkType.ELEMENT_SOLID,
        "eid4": LinkType.ELEMENT_SOLID,
        "eid5": LinkType.ELEMENT_SOLID,
        "eid6": LinkType.ELEMENT_SOLID,
        "eid7": LinkType.ELEMENT_SOLID,
        "eid8": LinkType.ELEMENT_SOLID,
    }

    def __init__(self, **kwargs):
        """Initialize the DefineHexSpotweldAssembly4 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEHEXSPOTWELDASSEMBLY4_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEHEXSPOTWELDASSEMBLY4_CARD1,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineHexSpotweldAssembly4.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEHEXSPOTWELDASSEMBLY4_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def id_sw(self) -> typing.Optional[int]:
        """Get or set the spot weld ID. A uniquie ID number must be used.
        """ # nopep8
        return self._cards[0].get_value("id_sw")

    @id_sw.setter
    def id_sw(self, value: int) -> None:
        """Set the id_sw property."""
        self._cards[0].set_value("id_sw", value)

    @property
    def eid1(self) -> typing.Optional[int]:
        """Get or set the Element ID n for up to 16 solid hexahedral elements
        """ # nopep8
        return self._cards[1].get_value("eid1")

    @eid1.setter
    def eid1(self, value: int) -> None:
        """Set the eid1 property."""
        self._cards[1].set_value("eid1", value)

    @property
    def eid2(self) -> typing.Optional[int]:
        """Get or set the Element ID n for up to 16 solid hexahedral elements.
        """ # nopep8
        return self._cards[1].get_value("eid2")

    @eid2.setter
    def eid2(self, value: int) -> None:
        """Set the eid2 property."""
        self._cards[1].set_value("eid2", value)

    @property
    def eid3(self) -> typing.Optional[int]:
        """Get or set the Element ID n for up to 16 solid hexahedral elements
        """ # nopep8
        return self._cards[1].get_value("eid3")

    @eid3.setter
    def eid3(self, value: int) -> None:
        """Set the eid3 property."""
        self._cards[1].set_value("eid3", value)

    @property
    def eid4(self) -> typing.Optional[int]:
        """Get or set the Element ID n for up to 16 solid hexahedral elements.
        """ # nopep8
        return self._cards[1].get_value("eid4")

    @eid4.setter
    def eid4(self, value: int) -> None:
        """Set the eid4 property."""
        self._cards[1].set_value("eid4", value)

    @property
    def eid5(self) -> typing.Optional[int]:
        """Get or set the Element ID n for up to 16 solid hexahedral elements
        """ # nopep8
        return self._cards[1].get_value("eid5")

    @eid5.setter
    def eid5(self, value: int) -> None:
        """Set the eid5 property."""
        self._cards[1].set_value("eid5", value)

    @property
    def eid6(self) -> typing.Optional[int]:
        """Get or set the Element ID n for up to 16 solid hexahedral elements
        """ # nopep8
        return self._cards[1].get_value("eid6")

    @eid6.setter
    def eid6(self, value: int) -> None:
        """Set the eid6 property."""
        self._cards[1].set_value("eid6", value)

    @property
    def eid7(self) -> typing.Optional[int]:
        """Get or set the Element ID n for up to 16 solid hexahedral elements
        """ # nopep8
        return self._cards[1].get_value("eid7")

    @eid7.setter
    def eid7(self, value: int) -> None:
        """Set the eid7 property."""
        self._cards[1].set_value("eid7", value)

    @property
    def eid8(self) -> typing.Optional[int]:
        """Get or set the Element ID n for up to 16 solid hexahedral elements
        """ # nopep8
        return self._cards[1].get_value("eid8")

    @eid8.setter
    def eid8(self, value: int) -> None:
        """Set the eid8 property."""
        self._cards[1].set_value("eid8", value)

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
    def eid1_link(self) -> typing.Optional[KeywordBase]:
        """Get the ELEMENT keyword containing the given eid1."""
        return self._get_link_by_attr("ELEMENT", "eid", self.eid1, "parts")

    @property
    def eid2_link(self) -> typing.Optional[KeywordBase]:
        """Get the ELEMENT keyword containing the given eid2."""
        return self._get_link_by_attr("ELEMENT", "eid", self.eid2, "parts")

    @property
    def eid3_link(self) -> typing.Optional[KeywordBase]:
        """Get the ELEMENT keyword containing the given eid3."""
        return self._get_link_by_attr("ELEMENT", "eid", self.eid3, "parts")

    @property
    def eid4_link(self) -> typing.Optional[KeywordBase]:
        """Get the ELEMENT keyword containing the given eid4."""
        return self._get_link_by_attr("ELEMENT", "eid", self.eid4, "parts")

    @property
    def eid5_link(self) -> typing.Optional[KeywordBase]:
        """Get the ELEMENT keyword containing the given eid5."""
        return self._get_link_by_attr("ELEMENT", "eid", self.eid5, "parts")

    @property
    def eid6_link(self) -> typing.Optional[KeywordBase]:
        """Get the ELEMENT keyword containing the given eid6."""
        return self._get_link_by_attr("ELEMENT", "eid", self.eid6, "parts")

    @property
    def eid7_link(self) -> typing.Optional[KeywordBase]:
        """Get the ELEMENT keyword containing the given eid7."""
        return self._get_link_by_attr("ELEMENT", "eid", self.eid7, "parts")

    @property
    def eid8_link(self) -> typing.Optional[KeywordBase]:
        """Get the ELEMENT keyword containing the given eid8."""
        return self._get_link_by_attr("ELEMENT", "eid", self.eid8, "parts")

