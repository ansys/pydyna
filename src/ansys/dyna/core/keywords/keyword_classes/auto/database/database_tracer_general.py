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

"""Module providing the DatabaseTracerGeneral class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_DATABASETRACERGENERAL_CARD0 = (
    FieldSchema("node", int, 0, 10, 0),
    FieldSchema("elem", int, 10, 10, 0),
    FieldSchema("typm", int, 20, 10, 1),
    FieldSchema("move", int, 30, 10, 0),
    FieldSchema("set", int, 40, 10, 0),
    FieldSchema("typs", int, 50, 10, 0),
    FieldSchema("cor", int, 60, 10, 0),
    FieldSchema("unused", int, 70, 10, None),
)

_DATABASETRACERGENERAL_CARD1 = (
    FieldSchema("dt", float, 0, 10, 0.0),
    FieldSchema("tbeg", float, 10, 10, 0.0),
    FieldSchema("tend", float, 20, 10, 1e+20),
    FieldSchema("fid", int, 30, 10, 0),
)

_DATABASETRACERGENERAL_CARD2 = (
    FieldSchema("varloc", int, 0, 10, 0),
    FieldSchema("varepl", int, 10, 10, 0),
)

class DatabaseTracerGeneral(KeywordBase):
    """DYNA DATABASE_TRACER_GENERAL keyword"""

    keyword = "DATABASE"
    subkeyword = "TRACER_GENERAL"
    _link_fields = {
        "node": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the DatabaseTracerGeneral class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DATABASETRACERGENERAL_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DATABASETRACERGENERAL_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DATABASETRACERGENERAL_CARD2,
                **kwargs,
            ),
        ]
    @property
    def node(self) -> int:
        """Get or set the Node ID that locates the tracer (see Remark 1)
        """ # nopep8
        return self._cards[0].get_value("node")

    @node.setter
    def node(self, value: int) -> None:
        """Set the node property."""
        self._cards[0].set_value("node", value)

    @property
    def elem(self) -> int:
        """Get or set the Element ID that controls the tracer motion (see Remarks 1 and 2)
        GT.0: Data are output for ELEM if the tracer is located inside ELEM
        LT.0: Data are not output for ELEM if the tracer is located inside ELEM
        """ # nopep8
        return self._cards[0].get_value("elem")

    @elem.setter
    def elem(self, value: int) -> None:
        """Set the elem property."""
        self._cards[0].set_value("elem", value)

    @property
    def typm(self) -> int:
        """Get or set the ELEM type:
        EQ.1: Solid
        EQ.2: Beam
        EQ.3: Shell
        EQ.4: Tshell
        """ # nopep8
        return self._cards[0].get_value("typm")

    @typm.setter
    def typm(self, value: int) -> None:
        """Set the typm property."""
        if value not in [1, 2, 3, 4, None]:
            raise Exception("""typm must be `None` or one of {1,2,3,4}.""")
        self._cards[0].set_value("typm", value)

    @property
    def move(self) -> int:
        """Get or set the Flag to define how the tracer moves (see Remark 1):
        EQ.0: The tracer does not move with ELEM
        EQ.1: The tracer velocity is interpolated from ELEM nodal velocities
        EQ.2: The tracer position is interpolated from ELEM nodal positions.
        """ # nopep8
        return self._cards[0].get_value("move")

    @move.setter
    def move(self, value: int) -> None:
        """Set the move property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""move must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("move", value)

    @property
    def set(self) -> int:
        """Get or set the Element set for which the data are output by the tracer (see Remark 2)
        """ # nopep8
        return self._cards[0].get_value("set")

    @set.setter
    def set(self, value: int) -> None:
        """Set the set property."""
        self._cards[0].set_value("set", value)

    @property
    def typs(self) -> int:
        """Get or set the SET type:
        EQ.0: Part
        EQ.1: Solid
        EQ.2: Beam
        EQ.3: Shell
        EQ.4: Tshell .
        """ # nopep8
        return self._cards[0].get_value("typs")

    @typs.setter
    def typs(self, value: int) -> None:
        """Set the typs property."""
        if value not in [0, 1, 2, 3, 4, None]:
            raise Exception("""typs must be `None` or one of {0,1,2,3,4}.""")
        self._cards[0].set_value("typs", value)

    @property
    def cor(self) -> int:
        """Get or set the Flag to correct the tracer position if found outside the tracked mesh during the initialization:
        EQ.0: No correction
        EQ.1: The tracer is moved to the center of the closest element
        EQ.2: The tracer is moved to the nearest face center or node of the closest element
        """ # nopep8
        return self._cards[0].get_value("cor")

    @cor.setter
    def cor(self, value: int) -> None:
        """Set the cor property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""cor must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("cor", value)

    @property
    def dt(self) -> float:
        """Get or set the Interval time between outputs (See Remark 3)
        """ # nopep8
        return self._cards[1].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        """Set the dt property."""
        self._cards[1].set_value("dt", value)

    @property
    def tbeg(self) -> float:
        """Get or set the Time to start the outputs.
        """ # nopep8
        return self._cards[1].get_value("tbeg")

    @tbeg.setter
    def tbeg(self, value: float) -> None:
        """Set the tbeg property."""
        self._cards[1].set_value("tbeg", value)

    @property
    def tend(self) -> float:
        """Get or set the Time to stop the outputs
        """ # nopep8
        return self._cards[1].get_value("tend")

    @tend.setter
    def tend(self, value: float) -> None:
        """Set the tend property."""
        self._cards[1].set_value("tend", value)

    @property
    def fid(self) -> int:
        """Get or set the Id to be append to trcrgal_binout (See Remark 3).
        """ # nopep8
        return self._cards[1].get_value("fid")

    @fid.setter
    def fid(self, value: int) -> None:
        """Set the fid property."""
        self._cards[1].set_value("fid", value)

    @property
    def varloc(self) -> int:
        """Get or set the Variable location in trcrgal_binout to be replaced with the variable specified in the VAREPL field:
        EQ.4: -velocity
        EQ.5: -velocity
        EQ.6: -velocity
        EQ.7: -stress
        EQ.8: -stress
        EQ.9: -stress
        EQ.10: -stress
        EQ.11: -stress
        EQ.12: -stress
        EQ.13: Plastic strain
        EQ.14: Nodal mass
        EQ.15: Undefined
        GE.16 and LE.30: Other auxiliary variables
        """ # nopep8
        return self._cards[2].get_value("varloc")

    @varloc.setter
    def varloc(self, value: int) -> None:
        """Set the varloc property."""
        self._cards[2].set_value("varloc", value)

    @property
    def varepl(self) -> int:
        """Get or set the Data to be output to the trcrgal_binout file instead of the variable located at VARLOC.  The interpretation of VAREPL is enumerated in the following list:
        EQ.1: -acceleration
        EQ.2: - acceleration
        EQ.3: - acceleration
        EQ.4: Nodal temperature
        EQ.5: Density
        EQ.6: Compression ratio
        EQ.7: Pressure.
        """ # nopep8
        return self._cards[2].get_value("varepl")

    @varepl.setter
    def varepl(self, value: int) -> None:
        """Set the varepl property."""
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, None]:
            raise Exception("""varepl must be `None` or one of {0,1,2,3,4,5,6,7}.""")
        self._cards[2].set_value("varepl", value)

    @property
    def node_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given node."""
        return self._get_link_by_attr("NODE", "nid", self.node, "parts")

