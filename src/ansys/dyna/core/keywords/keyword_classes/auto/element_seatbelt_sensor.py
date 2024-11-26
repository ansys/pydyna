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

class ElementSeatbeltSensor(KeywordBase):
    """DYNA ELEMENT_SEATBELT_SENSOR keyword"""

    keyword = "ELEMENT"
    subkeyword = "SEATBELT_SENSOR"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "sbsid",
                        int,
                        0,
                        10,
                        kwargs.get("sbsid", 0)
                    ),
                    Field(
                        "sbstyp",
                        int,
                        10,
                        10,
                        kwargs.get("sbstyp", 1)
                    ),
                    Field(
                        "sbsfl",
                        int,
                        20,
                        10,
                        kwargs.get("sbsfl", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nid",
                        int,
                        0,
                        10,
                        kwargs.get("nid", 0)
                    ),
                    Field(
                        "dof",
                        int,
                        10,
                        10,
                        kwargs.get("dof", 1)
                    ),
                    Field(
                        "acc",
                        float,
                        20,
                        10,
                        kwargs.get("acc", 0.0)
                    ),
                    Field(
                        "atime",
                        float,
                        30,
                        10,
                        kwargs.get("atime", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sbrid",
                        int,
                        0,
                        10,
                        kwargs.get("sbrid", 0)
                    ),
                    Field(
                        "pulrat",
                        float,
                        10,
                        10,
                        kwargs.get("pulrat", 0.0)
                    ),
                    Field(
                        "pultim",
                        float,
                        20,
                        10,
                        kwargs.get("pultim", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "time",
                        float,
                        0,
                        10,
                        kwargs.get("time", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nid1",
                        int,
                        0,
                        10,
                        kwargs.get("nid1", 0)
                    ),
                    Field(
                        "nid2",
                        int,
                        10,
                        10,
                        kwargs.get("nid2", 0)
                    ),
                    Field(
                        "dmx",
                        float,
                        20,
                        10,
                        kwargs.get("dmx", 0.0)
                    ),
                    Field(
                        "dmn",
                        float,
                        30,
                        10,
                        kwargs.get("dmn", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sbrid",
                        int,
                        0,
                        10,
                        kwargs.get("sbrid", 0)
                    ),
                    Field(
                        "pulmx",
                        float,
                        10,
                        10,
                        kwargs.get("pulmx", 1.0E+16)
                    ),
                    Field(
                        "pulmn",
                        float,
                        20,
                        10,
                        kwargs.get("pulmn", -1.0E+16)
                    ),
                ],
            ),
        ]

    @property
    def sbsid(self) -> int:
        """Get or set the Sensor ID. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("sbsid")

    @sbsid.setter
    def sbsid(self, value: int) -> None:
        self._cards[0].set_value("sbsid", value)

    @property
    def sbstyp(self) -> int:
        """Get or set the Sensor type:
        EQ.1: acceleration of node (default),
        EQ.2: retractor pull-out rate,
        EQ.3: time,
        EQ.4: distance between nodes.
        EQ.5:	retractor pull-out
        """ # nopep8
        return self._cards[0].get_value("sbstyp")

    @sbstyp.setter
    def sbstyp(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5]:
            raise Exception("""sbstyp must be one of {1,2,3,4,5}""")
        self._cards[0].set_value("sbstyp", value)

    @property
    def sbsfl(self) -> int:
        """Get or set the Sensor flag:
        EQ.0: sensor active during dynamic relaxation,
        EQ.1: sensor can be triggered during dynamic relaxation.
        """ # nopep8
        return self._cards[0].get_value("sbsfl")

    @sbsfl.setter
    def sbsfl(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""sbsfl must be one of {0,1}""")
        self._cards[0].set_value("sbsfl", value)

    @property
    def nid(self) -> int:
        """Get or set the Node ID of sensor
        """ # nopep8
        return self._cards[1].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[1].set_value("nid", value)

    @property
    def dof(self) -> int:
        """Get or set the Degree of freedom:
        EQ.1: x,
        EQ.2: y,
        EQ.3: z.
        """ # nopep8
        return self._cards[1].get_value("dof")

    @dof.setter
    def dof(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""dof must be one of {1,2,3}""")
        self._cards[1].set_value("dof", value)

    @property
    def acc(self) -> float:
        """Get or set the Activating acceleration
        """ # nopep8
        return self._cards[1].get_value("acc")

    @acc.setter
    def acc(self, value: float) -> None:
        self._cards[1].set_value("acc", value)

    @property
    def atime(self) -> float:
        """Get or set the Time over which acceleration must be exceeded
        """ # nopep8
        return self._cards[1].get_value("atime")

    @atime.setter
    def atime(self, value: float) -> None:
        self._cards[1].set_value("atime", value)

    @property
    def sbrid(self) -> int:
        """Get or set the Retractor ID, see *ELEMENT_SEATBELT_RETRACTOR.
        """ # nopep8
        return self._cards[2].get_value("sbrid")

    @sbrid.setter
    def sbrid(self, value: int) -> None:
        self._cards[2].set_value("sbrid", value)

    @property
    def pulrat(self) -> float:
        """Get or set the Rate of pull-out (length/time units)
        """ # nopep8
        return self._cards[2].get_value("pulrat")

    @pulrat.setter
    def pulrat(self, value: float) -> None:
        self._cards[2].set_value("pulrat", value)

    @property
    def pultim(self) -> float:
        """Get or set the Time over which rate of pull-out must be exceeded
        """ # nopep8
        return self._cards[2].get_value("pultim")

    @pultim.setter
    def pultim(self, value: float) -> None:
        self._cards[2].set_value("pultim", value)

    @property
    def time(self) -> float:
        """Get or set the Time at which sensor triggers
        """ # nopep8
        return self._cards[3].get_value("time")

    @time.setter
    def time(self, value: float) -> None:
        self._cards[3].set_value("time", value)

    @property
    def nid1(self) -> int:
        """Get or set the Node 1 ID
        """ # nopep8
        return self._cards[4].get_value("nid1")

    @nid1.setter
    def nid1(self, value: int) -> None:
        self._cards[4].set_value("nid1", value)

    @property
    def nid2(self) -> int:
        """Get or set the Node 2 ID
        """ # nopep8
        return self._cards[4].get_value("nid2")

    @nid2.setter
    def nid2(self, value: int) -> None:
        self._cards[4].set_value("nid2", value)

    @property
    def dmx(self) -> float:
        """Get or set the Maximum distance
        """ # nopep8
        return self._cards[4].get_value("dmx")

    @dmx.setter
    def dmx(self, value: float) -> None:
        self._cards[4].set_value("dmx", value)

    @property
    def dmn(self) -> float:
        """Get or set the Minimum distance
        """ # nopep8
        return self._cards[4].get_value("dmn")

    @dmn.setter
    def dmn(self, value: float) -> None:
        self._cards[4].set_value("dmn", value)

    @property
    def sbrid(self) -> int:
        """Get or set the Retractor ID: Elelemt Seatbelt_retractor
        """ # nopep8
        return self._cards[5].get_value("sbrid")

    @sbrid.setter
    def sbrid(self, value: int) -> None:
        self._cards[5].set_value("sbrid", value)

    @property
    def pulmx(self) -> float:
        """Get or set the Maximum pull-out
        """ # nopep8
        return self._cards[5].get_value("pulmx")

    @pulmx.setter
    def pulmx(self, value: float) -> None:
        self._cards[5].set_value("pulmx", value)

    @property
    def pulmn(self) -> float:
        """Get or set the Minimum pull-out
        """ # nopep8
        return self._cards[5].get_value("pulmn")

    @pulmn.setter
    def pulmn(self, value: float) -> None:
        self._cards[5].set_value("pulmn", value)

