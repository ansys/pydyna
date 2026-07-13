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

"""Module providing the BoundaryFluidmBottom class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BOUNDARYFLUIDMBOTTOM_CARD0 = (
    FieldSchema("distbt", float, 0, 10, None),
    FieldSchema("cxbt", float, 10, 10, None),
    FieldSchema("cybt", float, 20, 10, None),
    FieldSchema("czbt", float, 30, 10, None),
    FieldSchema("bnorm", float, 40, 10, 1.0),
)

class BoundaryFluidmBottom(KeywordBase):
    """DYNA BOUNDARY_FLUIDM_BOTTOM keyword"""

    keyword = "BOUNDARY"
    subkeyword = "FLUIDM_BOTTOM"

    def __init__(self, **kwargs):
        """Initialize the BoundaryFluidmBottom class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYFLUIDMBOTTOM_CARD0,
                **kwargs,
            ),
        ]
    @property
    def distbt(self) -> typing.Optional[float]:
        """Get or set the Perpendicular distance from the plane of the bottom to the origin of the global coordinate system
        """ # nopep8
        return self._cards[0].get_value("distbt")

    @distbt.setter
    def distbt(self, value: float) -> None:
        """Set the distbt property."""
        self._cards[0].set_value("distbt", value)

    @property
    def cxbt(self) -> typing.Optional[float]:
        """Get or set the X-direction cosine for the normal vector of the bottom pointing into the bottom
        """ # nopep8
        return self._cards[0].get_value("cxbt")

    @cxbt.setter
    def cxbt(self, value: float) -> None:
        """Set the cxbt property."""
        self._cards[0].set_value("cxbt", value)

    @property
    def cybt(self) -> typing.Optional[float]:
        """Get or set the Y-direction cosine for the normal vector of the bottom pointing into the bottom
        """ # nopep8
        return self._cards[0].get_value("cybt")

    @cybt.setter
    def cybt(self, value: float) -> None:
        """Set the cybt property."""
        self._cards[0].set_value("cybt", value)

    @property
    def czbt(self) -> typing.Optional[float]:
        """Get or set the Z-direction cosine for the normal vector of the bottom pointing into the bottom
        """ # nopep8
        return self._cards[0].get_value("czbt")

    @czbt.setter
    def czbt(self, value: float) -> None:
        """Set the czbt property."""
        self._cards[0].set_value("czbt", value)

    @property
    def bnorm(self) -> float:
        """Get or set the Low frequency bottom reflection coefficient (0 < BNORM  1)
        """ # nopep8
        return self._cards[0].get_value("bnorm")

    @bnorm.setter
    def bnorm(self, value: float) -> None:
        """Set the bnorm property."""
        self._cards[0].set_value("bnorm", value)

