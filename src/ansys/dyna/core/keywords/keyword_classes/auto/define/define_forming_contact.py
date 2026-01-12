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

"""Module providing the DefineFormingContact class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINEFORMINGCONTACT_CARD0 = (
    FieldSchema("ips", int, 0, 10, None),
    FieldSchema("ipm", int, 10, 10, None),
    FieldSchema("fs", float, 20, 10, None),
    FieldSchema("oneway", int, 30, 10, 0),
)

_DEFINEFORMINGCONTACT_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineFormingContact(KeywordBase):
    """DYNA DEFINE_FORMING_CONTACT keyword"""

    keyword = "DEFINE"
    subkeyword = "FORMING_CONTACT"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineFormingContact class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINEFORMINGCONTACT_CARD0,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineFormingContact.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINEFORMINGCONTACT_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def ips(self) -> typing.Optional[int]:
        """Get or set the Part ID of a slave sliding member, typically a deformable sheet metal blank.
        """ # nopep8
        return self._cards[0].get_value("ips")

    @ips.setter
    def ips(self, value: int) -> None:
        """Set the ips property."""
        self._cards[0].set_value("ips", value)

    @property
    def ipm(self) -> typing.Optional[int]:
        """Get or set the Part ID of a master sliding member, typically a tool or die defined as a rigid body.
        """ # nopep8
        return self._cards[0].get_value("ipm")

    @ipm.setter
    def ipm(self, value: int) -> None:
        """Set the ipm property."""
        self._cards[0].set_value("ipm", value)

    @property
    def fs(self) -> typing.Optional[float]:
        """Get or set the Coulomb friction coefficient.
        """ # nopep8
        return self._cards[0].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        """Set the fs property."""
        self._cards[0].set_value("fs", value)

    @property
    def oneway(self) -> int:
        """Get or set the Define FORMING contact type:
        EQ.0:	The contact is FORMING_ONE_WAY_SURFACE_TO_ SURFACE.
        EQ.1:	The contact is FORMING_ SURFACE_TO_ SURFACE.
        """ # nopep8
        return self._cards[0].get_value("oneway")

    @oneway.setter
    def oneway(self, value: int) -> None:
        """Set the oneway property."""
        if value not in [0, 1, None]:
            raise Exception("""oneway must be `None` or one of {0,1}.""")
        self._cards[0].set_value("oneway", value)

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

