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

"""Module providing the DefineControlVolumeInteraction class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINECONTROLVOLUMEINTERACTION_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("cvid1", int, 10, 10, None),
    FieldSchema("cvid2", int, 20, 10, None),
    FieldSchema("lcid_", int, 30, 10, None, "lcid "),
    FieldSchema("area_", float, 40, 10, None, "area "),
)

_DEFINECONTROLVOLUMEINTERACTION_OPTION0_CARD0 = (
    FieldSchema("title", str, 0, 80, None),
)

class DefineControlVolumeInteraction(KeywordBase):
    """DYNA DEFINE_CONTROL_VOLUME_INTERACTION keyword"""

    keyword = "DEFINE"
    subkeyword = "CONTROL_VOLUME_INTERACTION"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineControlVolumeInteraction class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINECONTROLVOLUMEINTERACTION_CARD0,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineControlVolumeInteraction.option_specs[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _DEFINECONTROLVOLUMEINTERACTION_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Fluid cavity interaction ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def cvid1(self) -> typing.Optional[int]:
        """Get or set the First control volume ID
        """ # nopep8
        return self._cards[0].get_value("cvid1")

    @cvid1.setter
    def cvid1(self, value: int) -> None:
        """Set the cvid1 property."""
        self._cards[0].set_value("cvid1", value)

    @property
    def cvid2(self) -> typing.Optional[int]:
        """Get or set the Second control volume ID
        """ # nopep8
        return self._cards[0].get_value("cvid2")

    @cvid2.setter
    def cvid2(self, value: int) -> None:
        """Set the cvid2 property."""
        self._cards[0].set_value("cvid2", value)

    @property
    def lcid_(self) -> typing.Optional[int]:
        """Get or set the Load curve id (*DEFINE_CURVE_FUNCTION). Tables, see *DEFINE_TABLE, and load curves may not share common IDs.
        LS-DYNA allows load curves IDs and table IDs to be used interchangeably.
        A unique number has to be defined.
        """ # nopep8
        return self._cards[0].get_value("lcid_")

    @lcid_.setter
    def lcid_(self, value: int) -> None:
        """Set the lcid_ property."""
        self._cards[0].set_value("lcid_", value)

    @property
    def area_(self) -> typing.Optional[float]:
        """Get or set the This is a constant area for the case when a flow area definition is not defined
        """ # nopep8
        return self._cards[0].get_value("area_")

    @area_.setter
    def area_(self, value: float) -> None:
        """Set the area_ property."""
        self._cards[0].set_value("area_", value)

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

