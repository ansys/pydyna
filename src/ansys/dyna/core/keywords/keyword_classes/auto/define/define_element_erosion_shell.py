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

"""Module providing the DefineElementErosionShell class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEELEMENTEROSIONSHELL_CARD0 = (
    FieldSchema("sid", int, 0, 10, None),
    FieldSchema("styp", int, 10, 10, 1),
    FieldSchema("numfip", float, 20, 10, -100.0),
    FieldSchema("nifp", int, 30, 10, 1),
)

class DefineElementErosionShell(KeywordBase):
    """DYNA DEFINE_ELEMENT_EROSION_SHELL keyword"""

    keyword = "DEFINE"
    subkeyword = "ELEMENT_EROSION_SHELL"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineElementErosionShell class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEELEMENTEROSIONSHELL_CARD0,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineElementErosionShell.option_specs[0],
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
    def sid(self) -> typing.Optional[int]:
        """Get or set the Element ID, element set ID, part ID, or part set ID, see *PART, *SET_PART or *SET_SHELL_OPTION.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def styp(self) -> int:
        """Get or set the ID type of SID:
        EQ.1:	shell element ID
        EQ.2:	shell element set ID
        EQ.3:	part ID
        EQ.4:	part set ID.
        """ # nopep8
        return self._cards[0].get_value("styp")

    @styp.setter
    def styp(self, value: int) -> None:
        """Set the styp property."""
        if value not in [1, 2, 3, 4, None]:
            raise Exception("""styp must be `None` or one of {1,2,3,4}.""")
        self._cards[0].set_value("styp", value)

    @property
    def numfip(self) -> float:
        """Get or set the Number of layers which must fail prior to element deletion.
        LT.0.0:	 is the percentage of layers which must fail prior to element deletion.
        """ # nopep8
        return self._cards[0].get_value("numfip")

    @numfip.setter
    def numfip(self, value: float) -> None:
        """Set the numfip property."""
        self._cards[0].set_value("numfip", value)

    @property
    def nifp(self) -> int:
        """Get or set the Number of integration points within one layer that need to fail, to indicate a failed layer.
        """ # nopep8
        return self._cards[0].get_value("nifp")

    @nifp.setter
    def nifp(self, value: int) -> None:
        """Set the nifp property."""
        self._cards[0].set_value("nifp", value)

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

