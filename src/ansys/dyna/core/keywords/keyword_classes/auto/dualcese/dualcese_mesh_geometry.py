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

"""Module providing the DualceseMeshGeometry class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEMESHGEOMETRY_CARD0 = (
    FieldSchema("geom", str, 0, 10, None),
    FieldSchema("idc", float, 10, 10, 0.25),
    FieldSchema("sfnbckt", float, 20, 10, 1.0),
)

class DualceseMeshGeometry(KeywordBase):
    """DYNA DUALCESE_MESH_GEOMETRY keyword"""

    keyword = "DUALCESE"
    subkeyword = "MESH_GEOMETRY"

    def __init__(self, **kwargs):
        """Initialize the DualceseMeshGeometry class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEMESHGEOMETRY_CARD0,
                **kwargs,
            ),
        ]
    @property
    def geom(self) -> typing.Optional[str]:
        """Get or set the Sets the geometric dimension:
        EQ.2D: Two - dimensional(2D) problem
        EQ.3D: Three - dimensional(3D) problem
        EQ.AXI: 2D axisymmetric
        """ # nopep8
        return self._cards[0].get_value("geom")

    @geom.setter
    def geom(self, value: str) -> None:
        """Set the geom property."""
        self._cards[0].set_value("geom", value)

    @property
    def idc(self) -> float:
        """Get or set the Contact interaction detection coefficient (for FSI problems)
        """ # nopep8
        return self._cards[0].get_value("idc")

    @idc.setter
    def idc(self, value: float) -> None:
        """Set the idc property."""
        self._cards[0].set_value("idc", value)

    @property
    def sfnbckt(self) -> float:
        """Get or set the Scale factor to increase (> 1.0) or decrease (< 1.0) the number of buckets used internally in the dual CESE IBM FSI solver bucket sort search routines.
        """ # nopep8
        return self._cards[0].get_value("sfnbckt")

    @sfnbckt.setter
    def sfnbckt(self, value: float) -> None:
        """Set the sfnbckt property."""
        self._cards[0].set_value("sfnbckt", value)

