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

"""Module providing the DualceseBlockmesh class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEBLOCKMESH_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
)

_DUALCESEBLOCKMESH_CARD1 = (
    FieldSchema("axtype", str, 0, 10, "X_AXIS"),
    FieldSchema("segid_l", float, 10, 10, 1.0),
    FieldSchema("segid_h", float, 20, 10, 2.0),
    FieldSchema("coord_l", float, 30, 10, None),
    FieldSchema("scale", float, 40, 10, 1.0),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("unused", int, 70, 10, None),
)

_DUALCESEBLOCKMESH_CARD2 = (
    FieldSchema("nele", int, 0, 10, None),
    FieldSchema("alen", int, 10, 10, None),
    FieldSchema("ratio", float, 20, 10, None),
    FieldSchema("cratio", float, 30, 10, None),
    FieldSchema("wid_a", float, 40, 10, None),
    FieldSchema("wid_b", float, 50, 10, None),
)

class DualceseBlockmesh(KeywordBase):
    """DYNA DUALCESE_BLOCKMESH keyword"""

    keyword = "DUALCESE"
    subkeyword = "BLOCKMESH"

    def __init__(self, **kwargs):
        """Initialize the DualceseBlockmesh class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEBLOCKMESH_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEBLOCKMESH_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEBLOCKMESH_CARD2,
                **kwargs,
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Dual CESE part ID for all elements in the block mesh. See *DUALCESE_MESH_PART.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def axtype(self) -> str:
        """Get or set the Axis to define, meaning one of �X_AXIS�, �Y_AXIS�, or �Z_AXIS�.  One of each is required for a 3D mesh, and the �X_AXIS� and �Y_AXIS� are required for a 2D mesh.
        """ # nopep8
        return self._cards[1].get_value("axtype")

    @axtype.setter
    def axtype(self, value: str) -> None:
        """Set the axtype property."""
        if value not in ["X_AXIS", "Y_AXIS", "Z_AXIS", None]:
            raise Exception("""axtype must be `None` or one of {"X_AXIS","Y_AXIS","Z_AXIS"}.""")
        self._cards[1].set_value("axtype", value)

    @property
    def segid_l(self) -> float:
        """Get or set the ID to use for the boundary segment set to be created at the low-index boundary
        """ # nopep8
        return self._cards[1].get_value("segid_l")

    @segid_l.setter
    def segid_l(self, value: float) -> None:
        """Set the segid_l property."""
        self._cards[1].set_value("segid_l", value)

    @property
    def segid_h(self) -> float:
        """Get or set the ID to use for the boundary segment set to be created at the high-index boundary
        """ # nopep8
        return self._cards[1].get_value("segid_h")

    @segid_h.setter
    def segid_h(self, value: float) -> None:
        """Set the segid_h property."""
        self._cards[1].set_value("segid_h", value)

    @property
    def coord_l(self) -> typing.Optional[float]:
        """Get or set the Minimum value of the coordinate on the AXTYPE axis
        """ # nopep8
        return self._cards[1].get_value("coord_l")

    @coord_l.setter
    def coord_l(self, value: float) -> None:
        """Set the coord_l property."""
        self._cards[1].set_value("coord_l", value)

    @property
    def scale(self) -> float:
        """Get or set the Factor to use to scale all positions on the AXTYPE axis
        """ # nopep8
        return self._cards[1].get_value("scale")

    @scale.setter
    def scale(self, value: float) -> None:
        """Set the scale property."""
        self._cards[1].set_value("scale", value)

    @property
    def nele(self) -> typing.Optional[int]:
        """Get or set the Number of elements to put into the current mesh block
        """ # nopep8
        return self._cards[2].get_value("nele")

    @nele.setter
    def nele(self, value: int) -> None:
        """Set the nele property."""
        self._cards[2].set_value("nele", value)

    @property
    def alen(self) -> typing.Optional[int]:
        """Get or set the Length of the given mesh block
        """ # nopep8
        return self._cards[2].get_value("alen")

    @alen.setter
    def alen(self, value: int) -> None:
        """Set the alen property."""
        self._cards[2].set_value("alen", value)

    @property
    def ratio(self) -> typing.Optional[float]:
        """Get or set the Total growth ratio, meaning the ratio by which the length of the last element in the block changes from the first element
        """ # nopep8
        return self._cards[2].get_value("ratio")

    @ratio.setter
    def ratio(self, value: float) -> None:
        """Set the ratio property."""
        self._cards[2].set_value("ratio", value)

    @property
    def cratio(self) -> typing.Optional[float]:
        """Get or set the Element-to-element growth ratio
        """ # nopep8
        return self._cards[2].get_value("cratio")

    @cratio.setter
    def cratio(self, value: float) -> None:
        """Set the cratio property."""
        self._cards[2].set_value("cratio", value)

    @property
    def wid_a(self) -> typing.Optional[float]:
        """Get or set the Width of the initial element in the block
        """ # nopep8
        return self._cards[2].get_value("wid_a")

    @wid_a.setter
    def wid_a(self, value: float) -> None:
        """Set the wid_a property."""
        self._cards[2].set_value("wid_a", value)

    @property
    def wid_b(self) -> typing.Optional[float]:
        """Get or set the Width of the last element in the block
        """ # nopep8
        return self._cards[2].get_value("wid_b")

    @wid_b.setter
    def wid_b(self, value: float) -> None:
        """Set the wid_b property."""
        self._cards[2].set_value("wid_b", value)

