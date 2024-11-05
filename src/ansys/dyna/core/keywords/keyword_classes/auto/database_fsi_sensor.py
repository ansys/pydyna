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

class DatabaseFsiSensor(KeywordBase):
    """DYNA DATABASE_FSI_SENSOR keyword"""

    keyword = "DATABASE"
    subkeyword = "FSI_SENSOR"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "dtout",
                        float,
                        0,
                        10,
                        kwargs.get("dtout")
                    ),
                    Field(
                        "binary",
                        int,
                        10,
                        10,
                        kwargs.get("binary", 1)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dbfsi_id",
                        int,
                        0,
                        10,
                        kwargs.get("dbfsi_id")
                    ),
                    Field(
                        "nid",
                        int,
                        10,
                        10,
                        kwargs.get("nid")
                    ),
                    Field(
                        "segmid",
                        int,
                        20,
                        10,
                        kwargs.get("segmid")
                    ),
                    Field(
                        "offset",
                        float,
                        30,
                        10,
                        kwargs.get("offset")
                    ),
                    Field(
                        "nd1",
                        int,
                        40,
                        10,
                        kwargs.get("nd1")
                    ),
                    Field(
                        "nd2",
                        int,
                        50,
                        10,
                        kwargs.get("nd2")
                    ),
                    Field(
                        "nd3",
                        int,
                        60,
                        10,
                        kwargs.get("nd3")
                    ),
                ],
            ),
        ]

    @property
    def dtout(self) -> typing.Optional[float]:
        """Get or set the Output interval
        """ # nopep8
        return self._cards[0].get_value("dtout")

    @dtout.setter
    def dtout(self, value: float) -> None:
        self._cards[0].set_value("dtout", value)

    @property
    def binary(self) -> int:
        """Get or set the Flag for binary output.  See remarks under "Output Files and Post-Processing" in Appendix O, "LS-DYNA MPP User Guide."
        EQ.1:	ASCII file is written:	This is the default for shared memory parallel (SMP) LS-DYNA executables.
        EQ.2:	Data written to a binary database binout, which contains data that would otherwise be output to the ASCII file.
        The ASCII file in this case is not created.  This is the default for MPP LS-DYNA executables.
        EQ.3:	ASCII file is written, and the data is also written to the binary database (NOTE: MPP LS-DYNA executables will only produce the binary database).
        """ # nopep8
        return self._cards[0].get_value("binary")

    @binary.setter
    def binary(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""binary must be one of {1,2,3}""")
        self._cards[0].set_value("binary", value)

    @property
    def dbfsi_id(self) -> typing.Optional[int]:
        """Get or set the Pressure-Sensor ID
        """ # nopep8
        return self._cards[1].get_value("dbfsi_id")

    @dbfsi_id.setter
    def dbfsi_id(self, value: int) -> None:
        self._cards[1].set_value("dbfsi_id", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID,If NID=0 or blank, the sensor will be placed in the center of a four-sided segment defined by SEGMID.
        """ # nopep8
        return self._cards[1].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[1].set_value("nid", value)

    @property
    def segmid(self) -> typing.Optional[int]:
        """Get or set the SegmentID (Lagrangian shell element ID) associated with the above node ID.
        """ # nopep8
        return self._cards[1].get_value("segmid")

    @segmid.setter
    def segmid(self, value: int) -> None:
        self._cards[1].set_value("segmid", value)

    @property
    def offset(self) -> typing.Optional[float]:
        """Get or set the Offset distance between the pressure sensor and the Lagrangian segment surface.  If it is positive, it is on the side pointed to by the segment normal vector.
        """ # nopep8
        return self._cards[1].get_value("offset")

    @offset.setter
    def offset(self, value: float) -> None:
        self._cards[1].set_value("offset", value)

    @property
    def nd1(self) -> typing.Optional[int]:
        """Get or set the Nodes defining the solid face for the solid element in the three-dimensional model
        or shell side for the shell element in the two-dimensional model, from which the sensor is located.
        In three dimensions, if the solid face has 4 nodes, only the diagonal opposites ND1 and ND2 are required.
        If the solid face is triangular, a third node ND3 should be provided. In two dimensions, only ND1 and ND2 are required to define the shell side
        """ # nopep8
        return self._cards[1].get_value("nd1")

    @nd1.setter
    def nd1(self, value: int) -> None:
        self._cards[1].set_value("nd1", value)

    @property
    def nd2(self) -> typing.Optional[int]:
        """Get or set the Nodes defining the solid face for the solid element in the three-dimensional model
        or shell side for the shell element in the two-dimensional model, from which the sensor is located.
        In three dimensions, if the solid face has 4 nodes, only the diagonal opposites ND1 and ND2 are required.
        If the solid face is triangular, a third node ND3 should be provided. In two dimensions, only ND1 and ND2 are required to define the shell side
        """ # nopep8
        return self._cards[1].get_value("nd2")

    @nd2.setter
    def nd2(self, value: int) -> None:
        self._cards[1].set_value("nd2", value)

    @property
    def nd3(self) -> typing.Optional[int]:
        """Get or set the Nodes defining the solid face for the solid element in the three-dimensional model
        or shell side for the shell element in the two-dimensional model, from which the sensor is located.
        In three dimensions, if the solid face has 4 nodes, only the diagonal opposites ND1 and ND2 are required.
        If the solid face is triangular, a third node ND3 should be provided. In two dimensions, only ND1 and ND2 are required to define the shell side
        """ # nopep8
        return self._cards[1].get_value("nd3")

    @nd3.setter
    def nd3(self, value: int) -> None:
        self._cards[1].set_value("nd3", value)

