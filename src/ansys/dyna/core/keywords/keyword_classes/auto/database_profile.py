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

class DatabaseProfile(KeywordBase):
    """DYNA DATABASE_PROFILE keyword"""

    keyword = "DATABASE"
    subkeyword = "PROFILE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "dt",
                        int,
                        0,
                        10,
                        kwargs.get("dt")
                    ),
                    Field(
                        "sid",
                        int,
                        10,
                        10,
                        kwargs.get("sid")
                    ),
                    Field(
                        "stype",
                        int,
                        20,
                        10,
                        kwargs.get("stype", 1)
                    ),
                    Field(
                        "data",
                        int,
                        30,
                        10,
                        kwargs.get("data", 1)
                    ),
                    Field(
                        "dir",
                        int,
                        40,
                        10,
                        kwargs.get("dir", 1)
                    ),
                    Field(
                        "updloc",
                        int,
                        50,
                        10,
                        kwargs.get("updloc", 0)
                    ),
                    Field(
                        "mmg",
                        int,
                        60,
                        10,
                        kwargs.get("mmg")
                    ),
                ],
            ),
        ]

    @property
    def dt(self) -> typing.Optional[int]:
        """Get or set the Interval time.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: int) -> None:
        self._cards[0].set_value("dt", value)

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set ID
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def stype(self) -> int:
        """Get or set the Set type:
        EQ.1:	Node Set,
        EQ.2:	Solid Set,
        EQ.3:	Shell Set,
        EQ.4:	Segment Set,
        EQ.5:	Beam Set.
        EQ.6:	tshell set
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 6]:
            raise Exception("""stype must be one of {1,2,3,4,5,6}""")
        self._cards[0].set_value("stype", value)

    @property
    def data(self) -> int:
        """Get or set the Data type:
        EQ.1:	x-velocity,
        EQ.2:	y-velocity,
        EQ.3:	z-velocity,
        EQ.4:	velocity magnitude,
        EQ.5:	x-acceleration,
        EQ.6:	y-acceleration,
        EQ.7:	z-acceleration,
        EQ.8:	acceleration magnitude,
        EQ.9:	pressure,
        EQ.10:	xx-stress,
        EQ.11:	yy-stress,
        EQ.12:	zz-stress,
        EQ.13:	xy-stress,
        EQ.14:	yz-stress,
        EQ.15:	zx-stress,
        EQ.16:	temperature,
        EQ.17:	volume fraction,
        EQ.18:	kinetic energy,
        EQ.19:	internal energy,
        EQ.20:	density.
        EQ.21:	xx-strain,
        EQ.22:	yy-strain,
        EQ.23:	zz-strain,
        EQ.24:	xy-strain,
        EQ.25:	yz-strain,
        EQ.26:	zx-strain.
        EQ.27:	effective plastic strain
        """ # nopep8
        return self._cards[0].get_value("data")

    @data.setter
    def data(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27]:
            raise Exception("""data must be one of {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27}""")
        self._cards[0].set_value("data", value)

    @property
    def dir(self) -> int:
        """Get or set the Direction:
        EQ.1:	x-direction,
        EQ.2:	y-direction,
        EQ.3:	z-direction,
        EQ.4:	Curvilinear (relative distances between elements of set ID are added up in the order defined by the set)
        """ # nopep8
        return self._cards[0].get_value("dir")

    @dir.setter
    def dir(self, value: int) -> None:
        if value not in [1, 2, 3, 4]:
            raise Exception("""dir must be one of {1,2,3,4}""")
        self._cards[0].set_value("dir", value)

    @property
    def updloc(self) -> int:
        """Get or set the Flag to update the set location
        EQ.0:	Only the initial position of set ID is considered
        EQ.1:	The positions of the elements composing the set are updated each DT
        """ # nopep8
        return self._cards[0].get_value("updloc")

    @updloc.setter
    def updloc(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""updloc must be one of {0,1}""")
        self._cards[0].set_value("updloc", value)

    @property
    def mmg(self) -> typing.Optional[int]:
        """Get or set the Multi-Material ALE group id. See Remark 2.
        GT.0:	Multi-Material ALE group id
        LT.0:	|MMG| is the id of a *SET_MULTI-MATERIAL_GROUP_LIST that can list several Multi-Material ALE group ids.
        """ # nopep8
        return self._cards[0].get_value("mmg")

    @mmg.setter
    def mmg(self, value: int) -> None:
        self._cards[0].set_value("mmg", value)

