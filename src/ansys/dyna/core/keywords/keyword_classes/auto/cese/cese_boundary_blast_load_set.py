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

"""Module providing the CeseBoundaryBlastLoadSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CESEBOUNDARYBLASTLOADSET_CARD0 = (
    FieldSchema("bid", int, 0, 10, None),
    FieldSchema("ssid", int, 10, 10, None),
)

class CeseBoundaryBlastLoadSet(KeywordBase):
    """DYNA CESE_BOUNDARY_BLAST_LOAD_SET keyword"""

    keyword = "CESE"
    subkeyword = "BOUNDARY_BLAST_LOAD_SET"

    def __init__(self, **kwargs):
        """Initialize the CeseBoundaryBlastLoadSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CESEBOUNDARYBLASTLOADSET_CARD0,
                **kwargs,
            ),        ]
    @property
    def bid(self) -> typing.Optional[int]:
        """Get or set the Blast source ID.
        """ # nopep8
        return self._cards[0].get_value("bid")

    @bid.setter
    def bid(self, value: int) -> None:
        """Set the bid property."""
        self._cards[0].set_value("bid", value)

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[0].set_value("ssid", value)

