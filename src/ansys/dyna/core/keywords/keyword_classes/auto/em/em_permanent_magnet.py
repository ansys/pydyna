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
from ansys.dyna.core.lib.keyword_base import LinkType

_EMPERMANENTMAGNET_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("pid", int, 10, 10, None),
    FieldSchema("mtype", int, 20, 10, 0),
    FieldSchema("north", int, 30, 10, None),
    FieldSchema("south", int, 40, 10, None),
    FieldSchema("hc", float, 50, 10, None),
)

_EMPERMANENTMAGNET_CARD1 = (
    FieldSchema("x", float, 0, 10, None),
    FieldSchema("y", float, 10, 10, None),
    FieldSchema("z", float, 20, 10, None),
)

_EMPERMANENTMAGNET_CARD2 = (
    FieldSchema("nid1", float, 0, 10, None),
    FieldSchema("nid2", float, 10, 10, None),
)

class EmPermanentMagnet(KeywordBase):
    """DYNA EM_PERMANENT_MAGNET keyword"""

    keyword = "EM"
    subkeyword = "PERMANENT_MAGNET"
    _link_fields = {
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the EmPermanentMagnet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMPERMANENTMAGNET_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EMPERMANENTMAGNET_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EMPERMANENTMAGNET_CARD2,
                **kwargs,
            ),
        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the ID of the magnet
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the PART ID
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def mtype(self) -> int:
        """Get or set the Magnet definition type:
        EQ.0: Magnet defined by two node sets for Northand South Poles.
        EQ.1: Magnet defined by two segments sets for Northand South Poles.
        EQ.3: Magnet defined by a global vector orientation.
        EQ.4: Magnet defined by a global vector orientation given by two node IDs.
        EQ.5: Magnetic gear defined by an number of magnets (NDIVIS) around an AXIS, with an alternance of north pole, south pole orientations given by 360/NDIVIS and starting along the vector given by X2/Y2/Z2.
        EQ.6: Magnetic gear defined by a number of magnets(NDIVIS) around an AXIS, with an alternance of north pole, south pole orientations given by 360 / NDIVIS and starting along the vector given by DIR.
        EQ.7: Same as 5 except the North and South orientation of the magnets follows the gear rotation axis (see AXIS in Card 2c.) rather than its radial direction.
        EQ.8: Same as 6 except the North and South orientation of the magnets follows the gear rotation axis (see AXIS in Card 2c) rather than its radial direction.
        """ # nopep8
        return self._cards[0].get_value("mtype")

    @mtype.setter
    def mtype(self, value: int) -> None:
        """Set the mtype property."""
        if value not in [0, 1, 3, 4, 5, 6, 7, 8, None]:
            raise Exception("""mtype must be `None` or one of {0,1,3,4,5,6,7,8}.""")
        self._cards[0].set_value("mtype", value)

    @property
    def north(self) -> typing.Optional[int]:
        """Get or set the Set ID of the magnet north face for MTYPE = 0 and 1
        """ # nopep8
        return self._cards[0].get_value("north")

    @north.setter
    def north(self, value: int) -> None:
        """Set the north property."""
        self._cards[0].set_value("north", value)

    @property
    def south(self) -> typing.Optional[int]:
        """Get or set the Set ID of the magnet south face for MTYPE = 0 and 1
        """ # nopep8
        return self._cards[0].get_value("south")

    @south.setter
    def south(self, value: int) -> None:
        """Set the south property."""
        self._cards[0].set_value("south", value)

    @property
    def hc(self) -> typing.Optional[float]:
        """Get or set the Coercive force, H_c. See Remark 1.
        LT.0.0: | HC | refers to a load curve ID giving the coercive force as a function of time
        """ # nopep8
        return self._cards[0].get_value("hc")

    @hc.setter
    def hc(self, value: float) -> None:
        """Set the hc property."""
        self._cards[0].set_value("hc", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the Orientation of magnetization vector if MTYPE=3
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        """Set the x property."""
        self._cards[1].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the Orientation of magnetization vector if MTYPE=3
        """ # nopep8
        return self._cards[1].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[1].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the Orientation of magnetization vector if MTYPE=3.
        """ # nopep8
        return self._cards[1].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        """Set the z property."""
        self._cards[1].set_value("z", value)

    @property
    def nid1(self) -> typing.Optional[float]:
        """Get or set the Two node IDs defining the magnetization vector for MTYPE=4
        """ # nopep8
        return self._cards[2].get_value("nid1")

    @nid1.setter
    def nid1(self, value: float) -> None:
        """Set the nid1 property."""
        self._cards[2].set_value("nid1", value)

    @property
    def nid2(self) -> typing.Optional[float]:
        """Get or set the Two node IDs defining the magnetization vector for MTYPE=4
        """ # nopep8
        return self._cards[2].get_value("nid2")

    @nid2.setter
    def nid2(self, value: float) -> None:
        """Set the nid2 property."""
        self._cards[2].set_value("nid2", value)

    @property
    def pid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

