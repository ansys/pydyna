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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DatabaseTracerGeneral(KeywordBase):
    """DYNA DATABASE_TRACER_GENERAL keyword"""

    keyword = "DATABASE"
    subkeyword = "TRACER_GENERAL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "node",
                        int,
                        0,
                        10,
                        kwargs.get("node", 0)
                    ),
                    Field(
                        "elem",
                        int,
                        10,
                        10,
                        kwargs.get("elem", 0)
                    ),
                    Field(
                        "typm",
                        int,
                        20,
                        10,
                        kwargs.get("typm", 1)
                    ),
                    Field(
                        "move",
                        int,
                        30,
                        10,
                        kwargs.get("move", 0)
                    ),
                    Field(
                        "set",
                        int,
                        40,
                        10,
                        kwargs.get("set", 0)
                    ),
                    Field(
                        "typs",
                        int,
                        50,
                        10,
                        kwargs.get("typs", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dt",
                        float,
                        0,
                        10,
                        kwargs.get("dt", 0.0)
                    ),
                    Field(
                        "tbeg",
                        float,
                        10,
                        10,
                        kwargs.get("tbeg", 0.0)
                    ),
                    Field(
                        "tend",
                        float,
                        20,
                        10,
                        kwargs.get("tend", 1e20)
                    ),
                    Field(
                        "fid",
                        int,
                        30,
                        10,
                        kwargs.get("fid", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "varloc",
                        int,
                        0,
                        10,
                        kwargs.get("varloc", 0)
                    ),
                    Field(
                        "varepl",
                        int,
                        10,
                        10,
                        kwargs.get("varepl", 0)
                    ),
                ],
            ),
        ]

    @property
    def node(self) -> int:
        """Get or set the Node ID that locates the tracer (see Remark 1)
        """ # nopep8
        return self._cards[0].get_value("node")

    @node.setter
    def node(self, value: int) -> None:
        self._cards[0].set_value("node", value)

    @property
    def elem(self) -> int:
        """Get or set the Element ID that controls the tracer motion (see Remarks 1 and 2)
        GT.0: Data are output for ELEM
        LT.0: Data are not output for ELEM.
        """ # nopep8
        return self._cards[0].get_value("elem")

    @elem.setter
    def elem(self, value: int) -> None:
        self._cards[0].set_value("elem", value)

    @property
    def typm(self) -> int:
        """Get or set the ELEM type:
        EQ.1: solid
        EQ.2: beam
        EQ.3: shell
        EQ.4: tshell
        """ # nopep8
        return self._cards[0].get_value("typm")

    @typm.setter
    def typm(self, value: int) -> None:
        if value not in [1, 2, 3, 4]:
            raise Exception("""typm must be one of {1,2,3,4}""")
        self._cards[0].set_value("typm", value)

    @property
    def move(self) -> int:
        """Get or set the Flag to define how the tracer moves (see Remark 1):
        EQ.0: the tracer does not move with ELEM
        EQ.1: the tracer velocity is interpolated from ELEM nodal velocities
        EQ.2: the tracer position is interpolated from ELEM nodal positions.
        """ # nopep8
        return self._cards[0].get_value("move")

    @move.setter
    def move(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""move must be one of {0,1,2}""")
        self._cards[0].set_value("move", value)

    @property
    def set(self) -> int:
        """Get or set the Element set for which the data are output by the tracer (see Remark 2)
        """ # nopep8
        return self._cards[0].get_value("set")

    @set.setter
    def set(self, value: int) -> None:
        self._cards[0].set_value("set", value)

    @property
    def typs(self) -> int:
        """Get or set the SET type:
        EQ.0: part
        EQ.1: solid
        EQ.2: beam
        EQ.3: shell
        EQ.4: tshell .
        """ # nopep8
        return self._cards[0].get_value("typs")

    @typs.setter
    def typs(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4]:
            raise Exception("""typs must be one of {0,1,2,3,4}""")
        self._cards[0].set_value("typs", value)

    @property
    def dt(self) -> float:
        """Get or set the Interval time between outputs (See Remark 3)
        """ # nopep8
        return self._cards[1].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[1].set_value("dt", value)

    @property
    def tbeg(self) -> float:
        """Get or set the Time to start the outputs.
        """ # nopep8
        return self._cards[1].get_value("tbeg")

    @tbeg.setter
    def tbeg(self, value: float) -> None:
        self._cards[1].set_value("tbeg", value)

    @property
    def tend(self) -> float:
        """Get or set the Time to stop the outputs
        """ # nopep8
        return self._cards[1].get_value("tend")

    @tend.setter
    def tend(self, value: float) -> None:
        self._cards[1].set_value("tend", value)

    @property
    def fid(self) -> int:
        """Get or set the Id to be append to trcrgal_binout (See Remark 3).
        """ # nopep8
        return self._cards[1].get_value("fid")

    @fid.setter
    def fid(self, value: int) -> None:
        self._cards[1].set_value("fid", value)

    @property
    def varloc(self) -> int:
        """Get or set the Variable location in trcrgal_binout to be replaced with the variable specified in the VAREPL field:
        EQ.4:	-velocity
        EQ.5:	-velocity
        EQ.6:	-velocity
        EQ.7:	-stress
        EQ.8:	-stress
        EQ.9:	-stress
        EQ.10:	-stress
        EQ.11:	-stress
        EQ.12:	-stress
        EQ.13:	plastic strain
        EQ.14:	nodal mass
        EQ.15:	undefined
        GE.16 and LE.30:	other auxiliary variables
        """ # nopep8
        return self._cards[2].get_value("varloc")

    @varloc.setter
    def varloc(self, value: int) -> None:
        self._cards[2].set_value("varloc", value)

    @property
    def varepl(self) -> int:
        """Get or set the Data to be output to the trcrgal_binout file instead of the variable located at VARLOC.  The interpretation of VAREPL is enumerated in the following list:
        EQ.1:	-acceleration
        EQ.2:	- acceleration
        EQ.3:	- acceleration
        EQ.4:	nodal temperature
        EQ.5:	density
        EQ.6:	compression ratio
        EQ.7:	pressure.
        """ # nopep8
        return self._cards[2].get_value("varepl")

    @varepl.setter
    def varepl(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""varepl must be one of {0,1,2,3,4,5,6,7}""")
        self._cards[2].set_value("varepl", value)

