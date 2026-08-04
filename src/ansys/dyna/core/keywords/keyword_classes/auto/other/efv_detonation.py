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

"""Module providing the EfvDetonation class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVDETONATION_CARD0 = (
    FieldSchema("idgeom", int, 0, 10, 0),
    FieldSchema("time_ig", float, 10, 10, None),
    FieldSchema("idpath", int, 20, 10, 0),
)

_EFVDETONATION_CARD1 = (
    FieldSchema("x", float, 0, 10, None),
    FieldSchema("y", float, 10, 10, None),
    FieldSchema("z", float, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
)

_EFVDETONATION_CARD2 = (
    FieldSchema("x1", float, 0, 10, None),
    FieldSchema("y1", float, 10, 10, None),
    FieldSchema("x2", float, 20, 10, None),
    FieldSchema("y2", float, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
)

class EfvDetonation(KeywordBase):
    """DYNA EFV_DETONATION keyword"""

    keyword = "EFV"
    subkeyword = "DETONATION"

    def __init__(self, **kwargs):
        """Initialize the EfvDetonation class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVDETONATION_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVDETONATION_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVDETONATION_CARD2,
                **kwargs,
            ),
        ]
    @property
    def idgeom(self) -> int:
        """Get or set the Detonation geometrical type:
        EQ.0: Point
        EQ.1: Line for IDIM = 2 (2D) or plane for IDIM = 3 (3D). See IDIM on *SECTION_EFV
        EQ.2: Circle for IDIM = 2 (2D) or cylinder for IDIM = 3 (3D). See IDIM on *SECTION_EFV. See Remarks 1 and 3
        EQ.3: Circle Circle for IDIM = 2 (2D) or sphere for IDIM = 3 (3D). See IDIM on *SECTION_EFV. See Remarks 1 and 3
        EQ.4: Manual detonation region for which all the elements specified with Card 3b burn at the time indicated by TIME_IG. See Remark 4
        """ # nopep8
        return self._cards[0].get_value("idgeom")

    @idgeom.setter
    def idgeom(self, value: int) -> None:
        """Set the idgeom property."""
        if value not in [0, 1, 2, 3, 4, None]:
            raise Exception("""idgeom must be `None` or one of {0,1,2,3,4}.""")
        self._cards[0].set_value("idgeom", value)

    @property
    def time_ig(self) -> typing.Optional[float]:
        """Get or set the Ignition time:
        GE.0.0: Time
        EQ.-1: The ignition is computed by EFV
        """ # nopep8
        return self._cards[0].get_value("time_ig")

    @time_ig.setter
    def time_ig(self, value: float) -> None:
        """Set the time_ig property."""
        self._cards[0].set_value("time_ig", value)

    @property
    def idpath(self) -> int:
        """Get or set the Detonation path:
        EQ.0: Direct path
        EQ.1: Indirect path
        """ # nopep8
        return self._cards[0].get_value("idpath")

    @idpath.setter
    def idpath(self, value: int) -> None:
        """Set the idpath property."""
        if value not in [0, 1, None]:
            raise Exception("""idpath must be `None` or one of {0,1}.""")
        self._cards[0].set_value("idpath", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the x-coordinate of the detonation point
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        """Set the x property."""
        self._cards[1].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of the detonation point
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[1].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the z-coordinate of the detonation point
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        """Set the z property."""
        self._cards[1].set_value("z", value)

    @property
    def x1(self) -> typing.Optional[float]:
        """Get or set the x-coordinate of the first point on the detonation line
        """ # nopep8
        return self._cards[2].get_value("x1")

    @x1.setter
    def x1(self, value: float) -> None:
        """Set the x1 property."""
        self._cards[2].set_value("x1", value)

    @property
    def y1(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of the first point on the detonation line
        """ # nopep8
        return self._cards[2].get_value("y1")

    @y1.setter
    def y1(self, value: float) -> None:
        """Set the y1 property."""
        self._cards[2].set_value("y1", value)

    @property
    def x2(self) -> typing.Optional[float]:
        """Get or set the x-coordinate of the second point on the detonation line
        """ # nopep8
        return self._cards[2].get_value("x2")

    @x2.setter
    def x2(self, value: float) -> None:
        """Set the x2 property."""
        self._cards[2].set_value("x2", value)

    @property
    def y2(self) -> typing.Optional[float]:
        """Get or set the y-coordinate of the second point on the detonation line
        """ # nopep8
        return self._cards[2].get_value("y2")

    @y2.setter
    def y2(self, value: float) -> None:
        """Set the y2 property."""
        self._cards[2].set_value("y2", value)

