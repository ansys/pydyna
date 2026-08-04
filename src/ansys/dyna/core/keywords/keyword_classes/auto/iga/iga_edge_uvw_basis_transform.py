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

"""Module providing the IgaEdgeUvwBasisTransform class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_IGAEDGEUVWBASISTRANSFORM_CARD0 = (
    FieldSchema("eid", int, 0, 10, None),
    FieldSchema("exyzid", int, 10, 10, None),
    FieldSchema("patchid", int, 20, 10, None),
    FieldSchema("sense", int, 30, 10, 0),
    FieldSchema("rstart", float, 40, 20, None),
    FieldSchema("rend", float, 60, 20, None),
)

_IGAEDGEUVWBASISTRANSFORM_CARD1 = (
    FieldSchema("elid", int, 0, 10, None),
    FieldSchema("edgeid", int, 10, 10, None),
)

class IgaEdgeUvwBasisTransform(KeywordBase):
    """DYNA IGA_EDGE_UVW_BASIS_TRANSFORM keyword"""

    keyword = "IGA"
    subkeyword = "EDGE_UVW_BASIS_TRANSFORM"

    def __init__(self, **kwargs):
        """Initialize the IgaEdgeUvwBasisTransform class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _IGAEDGEUVWBASISTRANSFORM_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _IGAEDGEUVWBASISTRANSFORM_CARD1,
                **kwargs,
            ),
        ]
    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Parametric edge ID. A unique number must be chosen.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        """Set the eid property."""
        self._cards[0].set_value("eid", value)

    @property
    def exyzid(self) -> typing.Optional[int]:
        """Get or set the Physical edge IDs, see *IGA_EDGE_XYZ and Remark 1 and 3.
        """ # nopep8
        return self._cards[0].get_value("exyzid")

    @exyzid.setter
    def exyzid(self, value: int) -> None:
        """Set the exyzid property."""
        self._cards[0].set_value("exyzid", value)

    @property
    def patchid(self) -> typing.Optional[int]:
        """Get or set the Parametric univariate NURBS patch ID. See *IGA_1D_NURBS_UVW and Remarks 2, 3, and 5. PATCHID is ignored for the BASIS_TRANSFORM keyword option.
        """ # nopep8
        return self._cards[0].get_value("patchid")

    @patchid.setter
    def patchid(self, value: int) -> None:
        """Set the patchid property."""
        self._cards[0].set_value("patchid", value)

    @property
    def sense(self) -> int:
        """Get or set the Sense of Orientation with respect to the physical edge.
        EQ. 0:Same(default)
        EQ.1: Reversed.
        """ # nopep8
        return self._cards[0].get_value("sense")

    @sense.setter
    def sense(self, value: int) -> None:
        """Set the sense property."""
        if value not in [0, 1, None]:
            raise Exception("""sense must be `None` or one of {0,1}.""")
        self._cards[0].set_value("sense", value)

    @property
    def rstart(self) -> typing.Optional[float]:
        """Get or set the Parametric coordinate defining the start of the trimmed parametric NURBS, see Remark 4 and 5.
        """ # nopep8
        return self._cards[0].get_value("rstart")

    @rstart.setter
    def rstart(self, value: float) -> None:
        """Set the rstart property."""
        self._cards[0].set_value("rstart", value)

    @property
    def rend(self) -> typing.Optional[float]:
        """Get or set the Parametric coordinate defining the end of the trimmed parametric NURBS, see Remark 4 and 5.
        """ # nopep8
        return self._cards[0].get_value("rend")

    @rend.setter
    def rend(self, value: float) -> None:
        """Set the rend property."""
        self._cards[0].set_value("rend", value)

    @property
    def elid(self) -> typing.Optional[int]:
        """Get or set the Element ID local to a basis transform.
        See *IGA_2D_BASIS_TRANSFORM_XYZ and Remark 6. A unique number must be chosen.
        """ # nopep8
        return self._cards[1].get_value("elid")

    @elid.setter
    def elid(self, value: int) -> None:
        """Set the elid property."""
        self._cards[1].set_value("elid", value)

    @property
    def edgeid(self) -> typing.Optional[int]:
        """Get or set the Edge ID. See Remark 7.
        """ # nopep8
        return self._cards[1].get_value("edgeid")

    @edgeid.setter
    def edgeid(self, value: int) -> None:
        """Set the edgeid property."""
        self._cards[1].set_value("edgeid", value)

