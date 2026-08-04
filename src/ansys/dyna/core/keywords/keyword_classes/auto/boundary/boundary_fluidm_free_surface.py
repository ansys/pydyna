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

"""Module providing the BoundaryFluidmFreeSurface class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_BOUNDARYFLUIDMFREESURFACE_CARD0 = (
    FieldSchema("distfs", float, 0, 10, None),
    FieldSchema("cxfs", float, 10, 10, None),
    FieldSchema("cyfs", float, 20, 10, None),
    FieldSchema("czfs", float, 30, 10, None),
)

class BoundaryFluidmFreeSurface(KeywordBase):
    """DYNA BOUNDARY_FLUIDM_FREE_SURFACE keyword"""

    keyword = "BOUNDARY"
    subkeyword = "FLUIDM_FREE_SURFACE"

    def __init__(self, **kwargs):
        """Initialize the BoundaryFluidmFreeSurface class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _BOUNDARYFLUIDMFREESURFACE_CARD0,
                **kwargs,
            ),
        ]
    @property
    def distfs(self) -> typing.Optional[float]:
        """Get or set the Perpendicular distance from the plane of the free surface to the origin of the global coordinate system
        """ # nopep8
        return self._cards[0].get_value("distfs")

    @distfs.setter
    def distfs(self, value: float) -> None:
        """Set the distfs property."""
        self._cards[0].set_value("distfs", value)

    @property
    def cxfs(self) -> typing.Optional[float]:
        """Get or set the X-direction cosine for the normal vector of the free surface pointing into the air
        """ # nopep8
        return self._cards[0].get_value("cxfs")

    @cxfs.setter
    def cxfs(self, value: float) -> None:
        """Set the cxfs property."""
        self._cards[0].set_value("cxfs", value)

    @property
    def cyfs(self) -> typing.Optional[float]:
        """Get or set the Y-direction cosine for the normal vector of the free surface pointing into the air
        """ # nopep8
        return self._cards[0].get_value("cyfs")

    @cyfs.setter
    def cyfs(self, value: float) -> None:
        """Set the cyfs property."""
        self._cards[0].set_value("cyfs", value)

    @property
    def czfs(self) -> typing.Optional[float]:
        """Get or set the Z-direction cosine for the normal vector of the free surface pointing into the air
        """ # nopep8
        return self._cards[0].get_value("czfs")

    @czfs.setter
    def czfs(self, value: float) -> None:
        """Set the czfs property."""
        self._cards[0].set_value("czfs", value)

