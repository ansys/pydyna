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

"""Module providing the DefineHexSpotweldAssembly16 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEHEXSPOTWELDASSEMBLY16_CARD0 = (
    FieldSchema("id_sw", int, 0, 10, None),
)

_DEFINEHEXSPOTWELDASSEMBLY16_CARD1 = (
    FieldSchema("eid1", int, 0, 10, None),
    FieldSchema("eid2", int, 10, 10, None),
    FieldSchema("eid3", int, 20, 10, None),
    FieldSchema("eid4", int, 30, 10, None),
    FieldSchema("eid5", int, 40, 10, None),
    FieldSchema("eid6", int, 50, 10, None),
    FieldSchema("eid7", int, 60, 10, None),
    FieldSchema("eid8", int, 70, 10, None),
)

_DEFINEHEXSPOTWELDASSEMBLY16_CARD2 = (
    FieldSchema("eid9", int, 0, 10, None),
    FieldSchema("eid10", int, 10, 10, None),
    FieldSchema("eid11", int, 20, 10, None),
    FieldSchema("eid12", int, 30, 10, None),
    FieldSchema("eid13", int, 40, 10, None),
    FieldSchema("eid14", int, 50, 10, None),
    FieldSchema("eid15", int, 60, 10, None),
    FieldSchema("eid16", int, 70, 10, None),
)

class DefineHexSpotweldAssembly16(KeywordBase):
    """DYNA DEFINE_HEX_SPOTWELD_ASSEMBLY_16 keyword"""

    keyword = "DEFINE"
    subkeyword = "HEX_SPOTWELD_ASSEMBLY_16"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineHexSpotweldAssembly16 class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEHEXSPOTWELDASSEMBLY16_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEHEXSPOTWELDASSEMBLY16_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINEHEXSPOTWELDASSEMBLY16_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineHexSpotweldAssembly16.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
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
    def eid9(self) -> typing.Optional[int]:
        """Get or set the Element ID n for up to 16 solid hexahedral elements
        """ # nopep8
        return self._cards[2].get_value("eid9")

    @eid9.setter
    def eid9(self, value: int) -> None:
        """Set the eid9 property."""
        self._cards[2].set_value("eid9", value)

    @property
    def eid10(self) -> typing.Optional[int]:
        """Get or set the Element ID n for up to 16 solid hexahedral elements
        """ # nopep8
        return self._cards[2].get_value("eid10")

    @eid10.setter
    def eid10(self, value: int) -> None:
        """Set the eid10 property."""
        self._cards[2].set_value("eid10", value)

    @property
    def eid11(self) -> typing.Optional[int]:
        """Get or set the Element ID n for up to 16 solid hexahedral elements
        """ # nopep8
        return self._cards[2].get_value("eid11")

    @eid11.setter
    def eid11(self, value: int) -> None:
        """Set the eid11 property."""
        self._cards[2].set_value("eid11", value)

    @property
    def eid12(self) -> typing.Optional[int]:
        """Get or set the Element ID n for up to 16 solid hexahedral elements.
        """ # nopep8
        return self._cards[2].get_value("eid12")

    @eid12.setter
    def eid12(self, value: int) -> None:
        """Set the eid12 property."""
        self._cards[2].set_value("eid12", value)

    @property
    def eid13(self) -> typing.Optional[int]:
        """Get or set the Element ID n for up to 16 solid hexahedral elements
        """ # nopep8
        return self._cards[2].get_value("eid13")

    @eid13.setter
    def eid13(self, value: int) -> None:
        """Set the eid13 property."""
        self._cards[2].set_value("eid13", value)

    @property
    def eid14(self) -> typing.Optional[int]:
        """Get or set the Element ID n for up to 16 solid hexahedral elements.
        """ # nopep8
        return self._cards[2].get_value("eid14")

    @eid14.setter
    def eid14(self, value: int) -> None:
        """Set the eid14 property."""
        self._cards[2].set_value("eid14", value)

    @property
    def eid15(self) -> typing.Optional[int]:
        """Get or set the Element ID n for up to 16 solid hexahedral elements
        """ # nopep8
        return self._cards[2].get_value("eid15")

    @eid15.setter
    def eid15(self, value: int) -> None:
        """Set the eid15 property."""
        self._cards[2].set_value("eid15", value)

    @property
    def eid16(self) -> typing.Optional[int]:
        """Get or set the Element ID n for up to 16 solid hexahedral elements
        """ # nopep8
        return self._cards[2].get_value("eid16")

    @eid16.setter
    def eid16(self, value: int) -> None:
        """Set the eid16 property."""
        self._cards[2].set_value("eid16", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

