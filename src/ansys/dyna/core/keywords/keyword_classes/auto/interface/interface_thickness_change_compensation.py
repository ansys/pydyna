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

"""Module providing the InterfaceThicknessChangeCompensation class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_INTERFACETHICKNESSCHANGECOMPENSATION_CARD0 = (
    FieldSchema("iflg", int, 0, 10, None),
    FieldSchema("thk0", float, 10, 10, None),
)

class InterfaceThicknessChangeCompensation(KeywordBase):
    """DYNA INTERFACE_THICKNESS_CHANGE_COMPENSATION keyword"""

    keyword = "INTERFACE"
    subkeyword = "THICKNESS_CHANGE_COMPENSATION"

    def __init__(self, **kwargs):
        """Initialize the InterfaceThicknessChangeCompensation class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INTERFACETHICKNESSCHANGECOMPENSATION_CARD0,
                **kwargs,
            ),        ]
    @property
    def iflg(self) -> typing.Optional[int]:
        """Get or set the Activation flag; set to �1� to invoke the option.
        """ # nopep8
        return self._cards[0].get_value("iflg")

    @iflg.setter
    def iflg(self, value: int) -> None:
        """Set the iflg property."""
        self._cards[0].set_value("iflg", value)

    @property
    def thk0(self) -> typing.Optional[float]:
        """Get or set the Initial thickness of the sheet blank
        """ # nopep8
        return self._cards[0].get_value("thk0")

    @thk0.setter
    def thk0(self, value: float) -> None:
        """Set the thk0 property."""
        self._cards[0].set_value("thk0", value)

