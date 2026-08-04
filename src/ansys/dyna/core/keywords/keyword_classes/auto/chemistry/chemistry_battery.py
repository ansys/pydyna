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

"""Module providing the ChemistryBattery class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CHEMISTRYBATTERY_CARD0 = (
    FieldSchema("file1", str, 0, 80, None),
)

_CHEMISTRYBATTERY_CARD1 = (
    FieldSchema("file1", str, 0, 80, None),
)

_CHEMISTRYBATTERY_CARD2 = (
    FieldSchema("file1", str, 0, 80, None),
)

class ChemistryBattery(KeywordBase):
    """DYNA CHEMISTRY_BATTERY keyword"""

    keyword = "CHEMISTRY"
    subkeyword = "BATTERY"

    def __init__(self, **kwargs):
        """Initialize the ChemistryBattery class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CHEMISTRYBATTERY_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CHEMISTRYBATTERY_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _CHEMISTRYBATTERY_CARD2,
                **kwargs,
            ),
        ]
    @property
    def file1(self) -> typing.Optional[str]:
        """Get or set the Name of the file containing the Chemkin-compatible input
        """ # nopep8
        return self._cards[0].get_value("file1")

    @file1.setter
    def file1(self, value: str) -> None:
        """Set the file1 property."""
        self._cards[0].set_value("file1", value)

    @property
    def file1(self) -> typing.Optional[str]:
        """Get or set the Name of the file containing the chemistry thermodynamics database
        """ # nopep8
        return self._cards[1].get_value("file1")

    @file1.setter
    def file1(self, value: str) -> None:
        """Set the file1 property."""
        self._cards[1].set_value("file1", value)

    @property
    def file1(self) -> typing.Optional[str]:
        """Get or set the Name of the file containing the chemistry transport properties database
        """ # nopep8
        return self._cards[2].get_value("file1")

    @file1.setter
    def file1(self, value: str) -> None:
        """Set the file1 property."""
        self._cards[2].set_value("file1", value)

