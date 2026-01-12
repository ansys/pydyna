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

"""Module providing the DefineStagedConstructionPartSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINESTAGEDCONSTRUCTIONPARTSET_CARD0 = (
    FieldSchema("psid", int, 0, 10, None),
    FieldSchema("stga", int, 10, 10, None),
    FieldSchema("stgr", int, 20, 10, None),
)

_DEFINESTAGEDCONSTRUCTIONPARTSET_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineStagedConstructionPartSet(KeywordBase):
    """DYNA DEFINE_STAGED_CONSTRUCTION_PART_SET keyword"""

    keyword = "DEFINE"
    subkeyword = "STAGED_CONSTRUCTION_PART_SET"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineStagedConstructionPartSet class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINESTAGEDCONSTRUCTIONPARTSET_CARD0,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineStagedConstructionPartSet.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINESTAGEDCONSTRUCTIONPARTSET_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part set ID.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

    @property
    def stga(self) -> typing.Optional[int]:
        """Get or set the Construction stage at which part is added.
        """ # nopep8
        return self._cards[0].get_value("stga")

    @stga.setter
    def stga(self, value: int) -> None:
        """Set the stga property."""
        self._cards[0].set_value("stga", value)

    @property
    def stgr(self) -> typing.Optional[int]:
        """Get or set the Construction stage at which part is removed.
        """ # nopep8
        return self._cards[0].get_value("stgr")

    @stgr.setter
    def stgr(self, value: int) -> None:
        """Set the stgr property."""
        self._cards[0].set_value("stgr", value)

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

