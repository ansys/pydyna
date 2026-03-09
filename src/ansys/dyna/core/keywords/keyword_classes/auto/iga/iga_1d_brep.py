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

"""Module providing the Iga1DBrep class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.series_card import SeriesCard
from ansys.dyna.core.lib.keyword_base import KeywordBase

_IGA1DBREP_CARD0 = (
    FieldSchema("brid", int, 0, 10, None),
)

class Iga1DBrep(KeywordBase):
    """DYNA IGA_1D_BREP keyword"""

    keyword = "IGA"
    subkeyword = "1D_BREP"

    def __init__(self, **kwargs):
        """Initialize the Iga1DBrep class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _IGA1DBREP_CARD0,
                **kwargs,
            ),
            SeriesCard(
                "eid",
                8,
                10,
                int,
                None,
                data = kwargs.get("eid")),
        ]
    @property
    def brid(self) -> typing.Optional[int]:
        """Get or set the One-dimensional boundary representation ID. A unique number must be chosen.
        """ # nopep8
        return self._cards[0].get_value("brid")

    @brid.setter
    def brid(self, value: int) -> None:
        """Set the brid property."""
        self._cards[0].set_value("brid", value)

    @property
    def eid(self) -> SeriesCard:
        """Parametric Edge IDs.."""
        return self._cards[1]

    @eid.setter
    def eid(self, value: typing.List) -> None:
        self._cards[1].data = value

