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

"""Module providing the BoundaryNonReflecting2D class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BOUNDARYNONREFLECTING2D_CARD0 = (
    FieldSchema("nsid", int, 0, 10, None),
    FieldSchema("ad", int, 10, 10, 0),
    FieldSchema("as_", int, 20, 10, 0, "as"),
)

class BoundaryNonReflecting2D(KeywordBase):
    """DYNA BOUNDARY_NON_REFLECTING_2D keyword"""

    keyword = "BOUNDARY"
    subkeyword = "NON_REFLECTING_2D"

    def __init__(self, **kwargs):
        """Initialize the BoundaryNonReflecting2D class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYNONREFLECTING2D_CARD0,
                **kwargs,
            ),        ]
    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID, see *SET_NODE.
        LT.0.0:|NSID| is the id of *SET_SEGMENT
        """ # nopep8
        return self._cards[0].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[0].set_value("nsid", value)

    @property
    def ad(self) -> int:
        """Get or set the Default activation flag for dilatational waves.
        EQ.0: on (default),
        NE.0: off.
        """ # nopep8
        return self._cards[0].get_value("ad")

    @ad.setter
    def ad(self, value: int) -> None:
        """Set the ad property."""
        self._cards[0].set_value("ad", value)

    @property
    def as_(self) -> int:
        """Get or set the Default activation flag for shear waves.
        EQ.0: on (default),
        NE.0: off.
        """ # nopep8
        return self._cards[0].get_value("as_")

    @as_.setter
    def as_(self, value: int) -> None:
        """Set the as_ property."""
        self._cards[0].set_value("as_", value)

