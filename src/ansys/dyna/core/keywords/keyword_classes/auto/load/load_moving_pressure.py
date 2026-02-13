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

"""Module providing the LoadMovingPressure class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_LOADMOVINGPRESSURE_CARD0 = (
    FieldSchema("loadid", int, 0, 10, None),
)

_LOADMOVINGPRESSURE_CARD1 = (
    FieldSchema("node1", int, 0, 10, None),
    FieldSchema("node2", int, 10, 10, None),
    FieldSchema("lcid", int, 20, 10, None),
    FieldSchema("cutoff", float, 30, 10, None),
    FieldSchema("lcidt", int, 40, 10, None),
    FieldSchema("lcidd", int, 50, 10, None),
    FieldSchema("idir", int, 60, 10, 0),
    FieldSchema("lsflg", int, 70, 10, 0),
)

_LOADMOVINGPRESSURE_CARD2 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("idtype", int, 10, 10, 0),
    FieldSchema("nip", int, 20, 10, None),
)

class LoadMovingPressure(KeywordBase):
    """DYNA LOAD_MOVING_PRESSURE keyword"""

    keyword = "LOAD"
    subkeyword = "MOVING_PRESSURE"
    _link_fields = {
        "node1": LinkType.NODE,
        "node2": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadMovingPressure class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADMOVINGPRESSURE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADMOVINGPRESSURE_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADMOVINGPRESSURE_CARD2,
                **kwargs,
            ),        ]
    @property
    def loadid(self) -> typing.Optional[int]:
        """Get or set the Loading ID
        """ # nopep8
        return self._cards[0].get_value("loadid")

    @loadid.setter
    def loadid(self, value: int) -> None:
        """Set the loadid property."""
        self._cards[0].set_value("loadid", value)

    @property
    def node1(self) -> typing.Optional[int]:
        """Get or set the Node located at the origin of the nozzle
        """ # nopep8
        return self._cards[1].get_value("node1")

    @node1.setter
    def node1(self, value: int) -> None:
        """Set the node1 property."""
        self._cards[1].set_value("node1", value)

    @property
    def node2(self) -> typing.Optional[int]:
        """Get or set the Node located at the head of the nozzle
        """ # nopep8
        return self._cards[1].get_value("node2")

    @node2.setter
    def node2(self, value: int) -> None:
        """Set the node2 property."""
        self._cards[1].set_value("node2", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve or function (see *DEFINE_FUNCTION) ID defining pressure versus radial distance from the center of the jet
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[1].set_value("lcid", value)

    @property
    def cutoff(self) -> typing.Optional[float]:
        """Get or set the Outer radius of jet.  The pressure acting outside this radius is set to zero
        """ # nopep8
        return self._cards[1].get_value("cutoff")

    @cutoff.setter
    def cutoff(self, value: float) -> None:
        """Set the cutoff property."""
        self._cards[1].set_value("cutoff", value)

    @property
    def lcidt(self) -> typing.Optional[int]:
        """Get or set the Load curve or function (see *DEFINE_FUNCTION) ID, which scales the pressure as a function of time.
        If a load curve isn't specified, the scale factor defaults to 1.0.
        """ # nopep8
        return self._cards[1].get_value("lcidt")

    @lcidt.setter
    def lcidt(self, value: int) -> None:
        """Set the lcidt property."""
        self._cards[1].set_value("lcidt", value)

    @property
    def lcidd(self) -> typing.Optional[int]:
        """Get or set the Load curve or function (see *DEFINE_FUNCTION) ID, which scales the pressure as a function of distance from the nozzle.
        If a load curve isn't specified, the scale factor defaults to 1.0.
        """ # nopep8
        return self._cards[1].get_value("lcidd")

    @lcidd.setter
    def lcidd(self, value: int) -> None:
        """Set the lcidd property."""
        self._cards[1].set_value("lcidd", value)

    @property
    def idir(self) -> int:
        """Get or set the Value that determines the direction of the pressure applied on the segments (see Remark 1)
        EQ.0:	the normal direction of the segments
        EQ.1 : the direction from the nozzle(NODE1) to the segments
        EQ.2 : the direction from NODE1 to NODE2
        EQ.3 : pressure is in the direction from NODE1 to NODE2 but only the normal component is applied on the segments.
        """ # nopep8
        return self._cards[1].get_value("idir")

    @idir.setter
    def idir(self, value: int) -> None:
        """Set the idir property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""idir must be `None` or one of {0,1,2,3}.""")
        self._cards[1].set_value("idir", value)

    @property
    def lsflg(self) -> int:
        """Get or set the Line-of-sight flag
        EQ.0:	see Remark 2
        EQ.1 : pressure is applied on the first - hit segments from the nozzle
        """ # nopep8
        return self._cards[1].get_value("lsflg")

    @lsflg.setter
    def lsflg(self, value: int) -> None:
        """Set the lsflg property."""
        if value not in [0, 1, None]:
            raise Exception("""lsflg must be `None` or one of {0,1}.""")
        self._cards[1].set_value("lsflg", value)

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Segment set ID, shell element set ID, part set ID, or part ID.  See IDT below
        """ # nopep8
        return self._cards[2].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[2].set_value("id", value)

    @property
    def idtype(self) -> int:
        """Get or set the Value that determines the meaning of variable ID:
        EQ.0:	ID is a segment set ID,
        EQ.1:	ID is a shell set ID,
        EQ.2:	ID is a part set ID,
        EQ.3:	ID is a part ID,
        """ # nopep8
        return self._cards[2].get_value("idtype")

    @idtype.setter
    def idtype(self, value: int) -> None:
        """Set the idtype property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""idtype must be `None` or one of {0,1,2,3}.""")
        self._cards[2].set_value("idtype", value)

    @property
    def nip(self) -> typing.Optional[int]:
        """Get or set the Number of integration in segment used to compute pressure loads.
        """ # nopep8
        return self._cards[2].get_value("nip")

    @nip.setter
    def nip(self, value: int) -> None:
        """Set the nip property."""
        self._cards[2].set_value("nip", value)

    @property
    def node1_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given node1."""
        return self._get_link_by_attr("NODE", "nid", self.node1, "parts")

    @property
    def node2_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given node2."""
        return self._get_link_by_attr("NODE", "nid", self.node2, "parts")

