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

"""Module providing the DefineFabricAssemblies class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEFABRICASSEMBLIES_CARD0 = (
    FieldSchema("spid1", int, 0, 10, None),
    FieldSchema("spid2", int, 10, 10, None),
    FieldSchema("spid3", int, 20, 10, None),
    FieldSchema("spid4", int, 30, 10, None),
    FieldSchema("spid5", int, 40, 10, None),
    FieldSchema("spid6", int, 50, 10, None),
    FieldSchema("spid7", int, 60, 10, None),
    FieldSchema("spid8", int, 70, 10, None),
)

_DEFINEFABRICASSEMBLIES_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineFabricAssemblies(KeywordBase):
    """DYNA DEFINE_FABRIC_ASSEMBLIES keyword"""

    keyword = "DEFINE"
    subkeyword = "FABRIC_ASSEMBLIES"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineFabricAssemblies class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEFABRICASSEMBLIES_CARD0,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineFabricAssemblies.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEFABRICASSEMBLIES_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def spid1(self) -> typing.Optional[int]:
        """Get or set the Part set ID that comprises an assembly.
        """ # nopep8
        return self._cards[0].get_value("spid1")

    @spid1.setter
    def spid1(self, value: int) -> None:
        """Set the spid1 property."""
        self._cards[0].set_value("spid1", value)

    @property
    def spid2(self) -> typing.Optional[int]:
        """Get or set the Part set ID that comprises an assembly.
        """ # nopep8
        return self._cards[0].get_value("spid2")

    @spid2.setter
    def spid2(self, value: int) -> None:
        """Set the spid2 property."""
        self._cards[0].set_value("spid2", value)

    @property
    def spid3(self) -> typing.Optional[int]:
        """Get or set the Part set ID that comprises an assembly.
        """ # nopep8
        return self._cards[0].get_value("spid3")

    @spid3.setter
    def spid3(self, value: int) -> None:
        """Set the spid3 property."""
        self._cards[0].set_value("spid3", value)

    @property
    def spid4(self) -> typing.Optional[int]:
        """Get or set the Part set ID that comprises an assembly.
        """ # nopep8
        return self._cards[0].get_value("spid4")

    @spid4.setter
    def spid4(self, value: int) -> None:
        """Set the spid4 property."""
        self._cards[0].set_value("spid4", value)

    @property
    def spid5(self) -> typing.Optional[int]:
        """Get or set the Part set ID that comprises an assembly.
        """ # nopep8
        return self._cards[0].get_value("spid5")

    @spid5.setter
    def spid5(self, value: int) -> None:
        """Set the spid5 property."""
        self._cards[0].set_value("spid5", value)

    @property
    def spid6(self) -> typing.Optional[int]:
        """Get or set the Part set ID that comprises an assembly.
        """ # nopep8
        return self._cards[0].get_value("spid6")

    @spid6.setter
    def spid6(self, value: int) -> None:
        """Set the spid6 property."""
        self._cards[0].set_value("spid6", value)

    @property
    def spid7(self) -> typing.Optional[int]:
        """Get or set the Part set ID that comprises an assembly.
        """ # nopep8
        return self._cards[0].get_value("spid7")

    @spid7.setter
    def spid7(self, value: int) -> None:
        """Set the spid7 property."""
        self._cards[0].set_value("spid7", value)

    @property
    def spid8(self) -> typing.Optional[int]:
        """Get or set the Part set ID that comprises an assembly.
        """ # nopep8
        return self._cards[0].get_value("spid8")

    @spid8.setter
    def spid8(self, value: int) -> None:
        """Set the spid8 property."""
        self._cards[0].set_value("spid8", value)

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

