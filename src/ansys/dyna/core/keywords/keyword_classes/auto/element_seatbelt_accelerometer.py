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

class ElementSeatbeltAccelerometer(KeywordBase):
    """DYNA ELEMENT_SEATBELT_ACCELEROMETER keyword"""

    keyword = "ELEMENT"
    subkeyword = "SEATBELT_ACCELEROMETER"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "sbacid",
                        int,
                        0,
                        10,
                        kwargs.get("sbacid", 0)
                    ),
                    Field(
                        "nid1",
                        int,
                        10,
                        10,
                        kwargs.get("nid1", 0)
                    ),
                    Field(
                        "nid2",
                        int,
                        20,
                        10,
                        kwargs.get("nid2", 0)
                    ),
                    Field(
                        "nid3",
                        int,
                        30,
                        10,
                        kwargs.get("nid3", 0)
                    ),
                    Field(
                        "igrav",
                        int,
                        40,
                        10,
                        kwargs.get("igrav", 0)
                    ),
                    Field(
                        "intopt",
                        int,
                        50,
                        10,
                        kwargs.get("intopt", 0)
                    ),
                    Field(
                        "mass",
                        float,
                        60,
                        10,
                        kwargs.get("mass")
                    ),
                ],
            ),
        ]

    @property
    def sbacid(self) -> int:
        """Get or set the Accelerometer ID. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("sbacid")

    @sbacid.setter
    def sbacid(self, value: int) -> None:
        self._cards[0].set_value("sbacid", value)

    @property
    def nid1(self) -> int:
        """Get or set the Node 1 ID
        """ # nopep8
        return self._cards[0].get_value("nid1")

    @nid1.setter
    def nid1(self, value: int) -> None:
        self._cards[0].set_value("nid1", value)

    @property
    def nid2(self) -> int:
        """Get or set the Node 2 ID
        """ # nopep8
        return self._cards[0].get_value("nid2")

    @nid2.setter
    def nid2(self, value: int) -> None:
        self._cards[0].set_value("nid2", value)

    @property
    def nid3(self) -> int:
        """Get or set the Node 3 ID
        """ # nopep8
        return self._cards[0].get_value("nid3")

    @nid3.setter
    def nid3(self, value: int) -> None:
        self._cards[0].set_value("nid3", value)

    @property
    def igrav(self) -> int:
        """Get or set the Gravitational accelerations due to body force loads.
        EQ.-6:  Z and X components removed from acceleration output
        EQ.-5   Y and Z components removed from acceleration output
        EQ.-4:  X and Y components removed from acceleration output
        EQ.-3:  Z component removed from acceleration output
        EQ.-2:  Y component removed from acceleration output
        EQ.-1:  X component removed from acceleration output
        EQ. 0:  all components included in acceleration output
        EQ. 1:  all components removed from acceleration output.
        """ # nopep8
        return self._cards[0].get_value("igrav")

    @igrav.setter
    def igrav(self, value: int) -> None:
        if value not in [0, -6, -5, -5, -3, -1, 1]:
            raise Exception("""igrav must be one of {0,-6,-5,-5,-3,-1,1}""")
        self._cards[0].set_value("igrav", value)

    @property
    def intopt(self) -> int:
        """Get or set the Integration option. If the accelerometer undergoes rigid body translation without rotation this option has no effect; however, if rotation occurs, option 1 may provide better agreement with the output of the accelerometer.
        EQ.0: velocities are integrated from the global accelerations and transfromed into the local system of the accelerometer
        EQ.1: velocities are integrated directly from the local accelerations of the accelerometer.
        """ # nopep8
        return self._cards[0].get_value("intopt")

    @intopt.setter
    def intopt(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""intopt must be one of {0,1}""")
        self._cards[0].set_value("intopt", value)

    @property
    def mass(self) -> typing.Optional[float]:
        """Get or set the Optional added mass for accelerometer. This mass is equally distributed to nodal points NID1, NID2, and NID3.  This option avoids the need to use the *ELEMENT_MASS keyword input if additional mass is required.
        """ # nopep8
        return self._cards[0].get_value("mass")

    @mass.setter
    def mass(self, value: float) -> None:
        self._cards[0].set_value("mass", value)

