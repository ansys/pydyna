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

"""Module providing the BoundaryElementMethodWake class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BOUNDARYELEMENTMETHODWAKE_CARD0 = (
    FieldSchema("nelem", int, 0, 10, None),
    FieldSchema("nside", int, 10, 10, None),
)

class BoundaryElementMethodWake(KeywordBase):
    """DYNA BOUNDARY_ELEMENT_METHOD_WAKE keyword"""

    keyword = "BOUNDARY"
    subkeyword = "ELEMENT_METHOD_WAKE"

    def __init__(self, **kwargs):
        """Initialize the BoundaryElementMethodWake class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYELEMENTMETHODWAKE_CARD0,
                **kwargs,
            ),        ]
    @property
    def nelem(self) -> typing.Optional[int]:
        """Get or set the Element number to which a wake is attached.
        """ # nopep8
        return self._cards[0].get_value("nelem")

    @nelem.setter
    def nelem(self, value: int) -> None:
        """Set the nelem property."""
        self._cards[0].set_value("nelem", value)

    @property
    def nside(self) -> typing.Optional[int]:
        """Get or set the The side of NELEM to which the wake is attached. This should be the downstream side of NELEM.
        """ # nopep8
        return self._cards[0].get_value("nside")

    @nside.setter
    def nside(self, value: int) -> None:
        """Set the nside property."""
        self._cards[0].set_value("nside", value)

