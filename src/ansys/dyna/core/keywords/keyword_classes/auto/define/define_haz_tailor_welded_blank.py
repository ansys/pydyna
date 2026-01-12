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

"""Module providing the DefineHazTailorWeldedBlank class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEHAZTAILORWELDEDBLANK_CARD0 = (
    FieldSchema("idtwb", int, 0, 10, 0),
    FieldSchema("idns", int, 10, 10, 0),
    FieldSchema("idp", int, 20, 10, 0),
    FieldSchema("ipflag", int, 30, 10, 0),
    FieldSchema("imonflag", int, 40, 10, 0),
)

class DefineHazTailorWeldedBlank(KeywordBase):
    """DYNA DEFINE_HAZ_TAILOR_WELDED_BLANK keyword"""

    keyword = "DEFINE"
    subkeyword = "HAZ_TAILOR_WELDED_BLANK"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineHazTailorWeldedBlank class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEHAZTAILORWELDEDBLANK_CARD0,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineHazTailorWeldedBlank.option_specs[0],
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
    def idtwb(self) -> int:
        """Get or set the Tailor Welded Blank ID.
        """ # nopep8
        return self._cards[0].get_value("idtwb")

    @idtwb.setter
    def idtwb(self, value: int) -> None:
        """Set the idtwb property."""
        self._cards[0].set_value("idtwb", value)

    @property
    def idns(self) -> int:
        """Get or set the Node Set ID defining the location of the line weld.
        """ # nopep8
        return self._cards[0].get_value("idns")

    @idns.setter
    def idns(self, value: int) -> None:
        """Set the idns property."""
        self._cards[0].set_value("idns", value)

    @property
    def idp(self) -> int:
        """Get or set the Part or part set ID. Applies to all HAZ parts if IDP = 0 (default).
        """ # nopep8
        return self._cards[0].get_value("idp")

    @idp.setter
    def idp(self, value: int) -> None:
        """Set the idp property."""
        self._cards[0].set_value("idp", value)

    @property
    def ipflag(self) -> int:
        """Get or set the IDP type:
        EQ.0:	part ID(default)
        EQ.1 : part set ID.
        """ # nopep8
        return self._cards[0].get_value("ipflag")

    @ipflag.setter
    def ipflag(self, value: int) -> None:
        """Set the ipflag property."""
        if value not in [0, 1, None]:
            raise Exception("""ipflag must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ipflag", value)

    @property
    def imonflag(self) -> int:
        """Get or set the Monotonicity flag for load curves ISW and IFW on *DEFINE_HAZ_PROPERTIES:
        EQ.0:	ISW and IFW increase monotonically.
        EQ.1 : ISW and IFW are allowed to be arbitrary load curves.
        """ # nopep8
        return self._cards[0].get_value("imonflag")

    @imonflag.setter
    def imonflag(self, value: int) -> None:
        """Set the imonflag property."""
        if value not in [0, 1, None]:
            raise Exception("""imonflag must be `None` or one of {0,1}.""")
        self._cards[0].set_value("imonflag", value)

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

