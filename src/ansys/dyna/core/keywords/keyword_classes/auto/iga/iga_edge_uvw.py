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

"""Module providing the IgaEdgeUvw class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_IGAEDGEUVW_CARD0 = (
    FieldSchema("eid", int, 0, 10, None),
    FieldSchema("exyzid", int, 10, 10, None),
    FieldSchema("nid", int, 20, 10, None),
    FieldSchema("sense", int, 30, 10, 0),
    FieldSchema("rstart", float, 40, 20, None),
    FieldSchema("rend", float, 60, 20, None),
)

class IgaEdgeUvw(KeywordBase):
    """DYNA IGA_EDGE_UVW keyword"""

    keyword = "IGA"
    subkeyword = "EDGE_UVW"

    def __init__(self, **kwargs):
        """Initialize the IgaEdgeUvw class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _IGAEDGEUVW_CARD0,
                **kwargs,
            ),        ]
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
        """Get or set the Physical edge IDs, see *IGA_EDGE_XYZ and Remark 1.
        """ # nopep8
        return self._cards[0].get_value("exyzid")

    @exyzid.setter
    def exyzid(self, value: int) -> None:
        """Set the exyzid property."""
        self._cards[0].set_value("exyzid", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Parametric univariate NURBS ID, see *IGA_1D_NURBS_UVW, see Remark 2.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[0].set_value("nid", value)

    @property
    def sense(self) -> int:
        """Get or set the Sense of Orientation with respect to the physical edge.
        EQ. 0:Same(default)
        EQ.1 : Reversed.
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
        """Get or set the Parametric coordinate defining the start of the trimmed parametric NURBS, see Remark 3.
        """ # nopep8
        return self._cards[0].get_value("rstart")

    @rstart.setter
    def rstart(self, value: float) -> None:
        """Set the rstart property."""
        self._cards[0].set_value("rstart", value)

    @property
    def rend(self) -> typing.Optional[float]:
        """Get or set the Parametric coordinate defining the end of the trimmed parametric NURBS, see Remark 3.
        """ # nopep8
        return self._cards[0].get_value("rend")

    @rend.setter
    def rend(self, value: float) -> None:
        """Set the rend property."""
        self._cards[0].set_value("rend", value)

