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

class DatabaseCrossSectionSet(KeywordBase):
    """DYNA DATABASE_CROSS_SECTION_SET keyword"""

    keyword = "DATABASE"
    subkeyword = "CROSS_SECTION_SET"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "csid",
                        int,
                        0,
                        10,
                        kwargs.get("csid")
                    ),
                    Field(
                        "title",
                        str,
                        10,
                        70,
                        kwargs.get("title")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nsid",
                        int,
                        0,
                        10,
                        kwargs.get("nsid", 0)
                    ),
                    Field(
                        "hsid",
                        int,
                        10,
                        10,
                        kwargs.get("hsid", 0)
                    ),
                    Field(
                        "bsid",
                        int,
                        20,
                        10,
                        kwargs.get("bsid", 0)
                    ),
                    Field(
                        "ssid",
                        int,
                        30,
                        10,
                        kwargs.get("ssid", 0)
                    ),
                    Field(
                        "tsid",
                        int,
                        40,
                        10,
                        kwargs.get("tsid", 0)
                    ),
                    Field(
                        "dsid",
                        int,
                        50,
                        10,
                        kwargs.get("dsid", 0)
                    ),
                    Field(
                        "id",
                        int,
                        60,
                        10,
                        kwargs.get("id")
                    ),
                    Field(
                        "itype",
                        int,
                        70,
                        10,
                        kwargs.get("itype", 0)
                    ),
                ],
            ),
        ]

    @property
    def csid(self) -> typing.Optional[int]:
        """Get or set the Optional ID for cross section. If not specified cross section ID is taken to be the cross section order in the input deck.
        """ # nopep8
        return self._cards[0].get_value("csid")

    @csid.setter
    def csid(self, value: int) -> None:
        self._cards[0].set_value("csid", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Crowss section descriptor. It is suggested that unique descriptions be used.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[0].set_value("title", value)

    @property
    def nsid(self) -> int:
        """Get or set the Nodal set ID, see *SET_NODE_option.
        """ # nopep8
        return self._cards[1].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        self._cards[1].set_value("nsid", value)

    @property
    def hsid(self) -> int:
        """Get or set the Solid element set ID, see *SET_SOLID.
        """ # nopep8
        return self._cards[1].get_value("hsid")

    @hsid.setter
    def hsid(self, value: int) -> None:
        self._cards[1].set_value("hsid", value)

    @property
    def bsid(self) -> int:
        """Get or set the Beam element set ID, see *SET_BEAM.
        """ # nopep8
        return self._cards[1].get_value("bsid")

    @bsid.setter
    def bsid(self, value: int) -> None:
        self._cards[1].set_value("bsid", value)

    @property
    def ssid(self) -> int:
        """Get or set the Shell element set ID, see *SET_SHELL_option.
        """ # nopep8
        return self._cards[1].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        self._cards[1].set_value("ssid", value)

    @property
    def tsid(self) -> int:
        """Get or set the Thick shell element set ID, see *SET_TSHELL.
        """ # nopep8
        return self._cards[1].get_value("tsid")

    @tsid.setter
    def tsid(self, value: int) -> None:
        self._cards[1].set_value("tsid", value)

    @property
    def dsid(self) -> int:
        """Get or set the Discrete element set ID, see *SET_DISCRETE.
        """ # nopep8
        return self._cards[1].get_value("dsid")

    @dsid.setter
    def dsid(self, value: int) -> None:
        self._cards[1].set_value("dsid", value)

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Rigid body (see *MAT_RIGID, type 20) or accelerometer ID (see *ELEMENT_ SEATBELT_ACCELEROMETER). The force resultants are output in the updated local system of the rigid body or accelerometer.
        """ # nopep8
        return self._cards[1].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[1].set_value("id", value)

    @property
    def itype(self) -> int:
        """Get or set the Flag for local system type:
        EQ. 0: rigid body (default),
        EQ. 1: accelerometer.
        EQ. 2: coordinate ID.
        """ # nopep8
        return self._cards[1].get_value("itype")

    @itype.setter
    def itype(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""itype must be one of {0,1,2}""")
        self._cards[1].set_value("itype", value)

