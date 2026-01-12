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

"""Module providing the EmPermanentMagnet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EMPERMANENTMAGNET_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("partid", int, 10, 10, None),
    FieldSchema("mtype", int, 20, 10, 0),
    FieldSchema("north", int, 30, 10, None),
    FieldSchema("south", int, 40, 10, None),
    FieldSchema("hc", float, 50, 10, None),
)

_EMPERMANENTMAGNET_CARD1 = (
    FieldSchema("x/nid1", float, 0, 10, None),
    FieldSchema("y/nid2", float, 10, 10, None),
    FieldSchema("z", float, 20, 10, None),
)

class EmPermanentMagnet(KeywordBase):
    """DYNA EM_PERMANENT_MAGNET keyword"""

    keyword = "EM"
    subkeyword = "PERMANENT_MAGNET"

    def __init__(self, **kwargs):
        """Initialize the EmPermanentMagnet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMPERMANENTMAGNET_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EMPERMANENTMAGNET_CARD1,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the MID
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def partid(self) -> typing.Optional[int]:
        """Get or set the PART ID
        """ # nopep8
        return self._cards[0].get_value("partid")

    @partid.setter
    def partid(self, value: int) -> None:
        """Set the partid property."""
        self._cards[0].set_value("partid", value)

    @property
    def mtype(self) -> int:
        """Get or set the Magnet definition type :
        EQ.0 : Magnet defined by two node sets for Northand South Poles.
        EQ.1 : Magnet defined by two segments sets for Northand South Poles.
        EQ.3 : Magnet defined by a global vector orientation.
        EQ.4 : Magnet defined by a global vector orientation given by two node IDs.
        """ # nopep8
        return self._cards[0].get_value("mtype")

    @mtype.setter
    def mtype(self, value: int) -> None:
        """Set the mtype property."""
        if value not in [0, 1, 3, 4, None]:
            raise Exception("""mtype must be `None` or one of {0,1,3,4}.""")
        self._cards[0].set_value("mtype", value)

    @property
    def north(self) -> typing.Optional[int]:
        """Get or set the Set of nodes/segments  of the north face of magnet
        """ # nopep8
        return self._cards[0].get_value("north")

    @north.setter
    def north(self, value: int) -> None:
        """Set the north property."""
        self._cards[0].set_value("north", value)

    @property
    def south(self) -> typing.Optional[int]:
        """Get or set the Set of nodes/segments  of the south face of magnet
        """ # nopep8
        return self._cards[0].get_value("south")

    @south.setter
    def south(self, value: int) -> None:
        """Set the south property."""
        self._cards[0].set_value("south", value)

    @property
    def hc(self) -> typing.Optional[float]:
        """Get or set the Coercive force. If a negative value is entered, it will give the value as a function of time
        """ # nopep8
        return self._cards[0].get_value("hc")

    @hc.setter
    def hc(self, value: float) -> None:
        """Set the hc property."""
        self._cards[0].set_value("hc", value)

    @property
    def x_nid1(self) -> typing.Optional[float]:
        """Get or set the Orientation of magnetization vector if MTYPE=3.
        Two node IDs defining the magnetization vector if MTYPE=4.
        """ # nopep8
        return self._cards[1].get_value("x/nid1")

    @x_nid1.setter
    def x_nid1(self, value: float) -> None:
        """Set the x_nid1 property."""
        self._cards[1].set_value("x/nid1", value)

    @property
    def y_nid2(self) -> typing.Optional[float]:
        """Get or set the Orientation of magnetization vector if MTYPE=3.
        Two node IDs defining the magnetization vector if MTYPE=4.
        """ # nopep8
        return self._cards[1].get_value("y/nid2")

    @y_nid2.setter
    def y_nid2(self, value: float) -> None:
        """Set the y_nid2 property."""
        self._cards[1].set_value("y/nid2", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the Orientation of magnetization vector if MTYPE=3.
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        """Set the z property."""
        self._cards[1].set_value("z", value)

