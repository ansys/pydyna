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

"""Module providing the InitialInternalDofSolidType4 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_INITIALINTERNALDOFSOLIDTYPE4_CARD0 = (
    FieldSchema("lid", int, 0, 10, None),
)

_INITIALINTERNALDOFSOLIDTYPE4_CARD1 = (
    FieldSchema("valx", float, 0, 10, None),
    FieldSchema("valy", float, 10, 10, None),
    FieldSchema("valz", float, 20, 10, None),
)

class InitialInternalDofSolidType4(KeywordBase):
    """DYNA INITIAL_INTERNAL_DOF_SOLID_TYPE4 keyword"""

    keyword = "INITIAL"
    subkeyword = "INTERNAL_DOF_SOLID_TYPE4"

    def __init__(self, **kwargs):
        """Initialize the InitialInternalDofSolidType4 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALINTERNALDOFSOLIDTYPE4_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INITIALINTERNALDOFSOLIDTYPE4_CARD1,
                **kwargs,
            ),        ]
    @property
    def lid(self) -> typing.Optional[int]:
        """Get or set the Element ID.
        """ # nopep8
        return self._cards[0].get_value("lid")

    @lid.setter
    def lid(self, value: int) -> None:
        """Set the lid property."""
        self._cards[0].set_value("lid", value)

    @property
    def valx(self) -> typing.Optional[float]:
        """Get or set the X component of internal degree of freedom.
        """ # nopep8
        return self._cards[1].get_value("valx")

    @valx.setter
    def valx(self, value: float) -> None:
        """Set the valx property."""
        self._cards[1].set_value("valx", value)

    @property
    def valy(self) -> typing.Optional[float]:
        """Get or set the Y component of internal degree of freedom.
        """ # nopep8
        return self._cards[1].get_value("valy")

    @valy.setter
    def valy(self, value: float) -> None:
        """Set the valy property."""
        self._cards[1].set_value("valy", value)

    @property
    def valz(self) -> typing.Optional[float]:
        """Get or set the Z component of internal degree of freedom.
        """ # nopep8
        return self._cards[1].get_value("valz")

    @valz.setter
    def valz(self, value: float) -> None:
        """Set the valz property."""
        self._cards[1].set_value("valz", value)

