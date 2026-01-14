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

"""Module providing the BoundaryElementMethodNeighbor class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BOUNDARYELEMENTMETHODNEIGHBOR_CARD0 = (
    FieldSchema("nelem", int, 0, 10, None),
    FieldSchema("nabor1", int, 10, 10, None),
    FieldSchema("nabor2", int, 20, 10, None),
    FieldSchema("nabor3", int, 30, 10, None),
    FieldSchema("nabor4", int, 40, 10, None),
)

class BoundaryElementMethodNeighbor(KeywordBase):
    """DYNA BOUNDARY_ELEMENT_METHOD_NEIGHBOR keyword"""

    keyword = "BOUNDARY"
    subkeyword = "ELEMENT_METHOD_NEIGHBOR"

    def __init__(self, **kwargs):
        """Initialize the BoundaryElementMethodNeighbor class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYELEMENTMETHODNEIGHBOR_CARD0,
                **kwargs,
            ),        ]
    @property
    def nelem(self) -> typing.Optional[int]:
        """Get or set the Element number.
        """ # nopep8
        return self._cards[0].get_value("nelem")

    @nelem.setter
    def nelem(self, value: int) -> None:
        """Set the nelem property."""
        self._cards[0].set_value("nelem", value)

    @property
    def nabor1(self) -> typing.Optional[int]:
        """Get or set the Neighbor for side 1 of NELEM.
        """ # nopep8
        return self._cards[0].get_value("nabor1")

    @nabor1.setter
    def nabor1(self, value: int) -> None:
        """Set the nabor1 property."""
        self._cards[0].set_value("nabor1", value)

    @property
    def nabor2(self) -> typing.Optional[int]:
        """Get or set the Neighbor for side 2 of NELEM.
        """ # nopep8
        return self._cards[0].get_value("nabor2")

    @nabor2.setter
    def nabor2(self, value: int) -> None:
        """Set the nabor2 property."""
        self._cards[0].set_value("nabor2", value)

    @property
    def nabor3(self) -> typing.Optional[int]:
        """Get or set the Neighbor for side 3 of NELEM.
        """ # nopep8
        return self._cards[0].get_value("nabor3")

    @nabor3.setter
    def nabor3(self, value: int) -> None:
        """Set the nabor3 property."""
        self._cards[0].set_value("nabor3", value)

    @property
    def nabor4(self) -> typing.Optional[int]:
        """Get or set the Neighbor for side 4 of NELEM.
        """ # nopep8
        return self._cards[0].get_value("nabor4")

    @nabor4.setter
    def nabor4(self, value: int) -> None:
        """Set the nabor4 property."""
        self._cards[0].set_value("nabor4", value)

