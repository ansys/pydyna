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

class BatteryEchemCellGeometry(KeywordBase):
    """DYNA BATTERY_ECHEM_CELL_GEOMETRY keyword"""

    keyword = "BATTERY"
    subkeyword = "ECHEM_CELL_GEOMETRY"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "imodel",
                        int,
                        0,
                        10,
                        kwargs.get("imodel")
                    ),
                    Field(
                        "alen",
                        float,
                        10,
                        10,
                        kwargs.get("alen")
                    ),
                    Field(
                        "slen",
                        float,
                        20,
                        10,
                        kwargs.get("slen")
                    ),
                    Field(
                        "clen",
                        float,
                        30,
                        10,
                        kwargs.get("clen")
                    ),
                    Field(
                        "acclen",
                        float,
                        40,
                        10,
                        kwargs.get("acclen")
                    ),
                    Field(
                        "cclen",
                        float,
                        50,
                        10,
                        kwargs.get("cclen")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "amesh",
                        int,
                        0,
                        10,
                        kwargs.get("amesh")
                    ),
                    Field(
                        "smesh",
                        int,
                        10,
                        10,
                        kwargs.get("smesh")
                    ),
                    Field(
                        "cmesh",
                        int,
                        20,
                        10,
                        kwargs.get("cmesh")
                    ),
                    Field(
                        "accmesh",
                        int,
                        30,
                        10,
                        kwargs.get("accmesh")
                    ),
                    Field(
                        "cccmesh",
                        int,
                        40,
                        10,
                        kwargs.get("cccmesh")
                    ),
                ],
            ),
        ]

    @property
    def imodel(self) -> typing.Optional[int]:
        """Get or set the A battery model identifier
        """ # nopep8
        return self._cards[0].get_value("imodel")

    @imodel.setter
    def imodel(self, value: int) -> None:
        self._cards[0].set_value("imodel", value)

    @property
    def alen(self) -> typing.Optional[float]:
        """Get or set the The length of anode side electrode
        """ # nopep8
        return self._cards[0].get_value("alen")

    @alen.setter
    def alen(self, value: float) -> None:
        self._cards[0].set_value("alen", value)

    @property
    def slen(self) -> typing.Optional[float]:
        """Get or set the The length of separator
        """ # nopep8
        return self._cards[0].get_value("slen")

    @slen.setter
    def slen(self, value: float) -> None:
        self._cards[0].set_value("slen", value)

    @property
    def clen(self) -> typing.Optional[float]:
        """Get or set the The length of cathode side electrode
        """ # nopep8
        return self._cards[0].get_value("clen")

    @clen.setter
    def clen(self, value: float) -> None:
        self._cards[0].set_value("clen", value)

    @property
    def acclen(self) -> typing.Optional[float]:
        """Get or set the The length of negative current collector
        """ # nopep8
        return self._cards[0].get_value("acclen")

    @acclen.setter
    def acclen(self, value: float) -> None:
        self._cards[0].set_value("acclen", value)

    @property
    def cclen(self) -> typing.Optional[float]:
        """Get or set the The length of positive current collector
        """ # nopep8
        return self._cards[0].get_value("cclen")

    @cclen.setter
    def cclen(self, value: float) -> None:
        self._cards[0].set_value("cclen", value)

    @property
    def amesh(self) -> typing.Optional[int]:
        """Get or set the The number of anode side meshes
        """ # nopep8
        return self._cards[1].get_value("amesh")

    @amesh.setter
    def amesh(self, value: int) -> None:
        self._cards[1].set_value("amesh", value)

    @property
    def smesh(self) -> typing.Optional[int]:
        """Get or set the The number of separator.
        """ # nopep8
        return self._cards[1].get_value("smesh")

    @smesh.setter
    def smesh(self, value: int) -> None:
        self._cards[1].set_value("smesh", value)

    @property
    def cmesh(self) -> typing.Optional[int]:
        """Get or set the The number of cathode side electrode
        """ # nopep8
        return self._cards[1].get_value("cmesh")

    @cmesh.setter
    def cmesh(self, value: int) -> None:
        self._cards[1].set_value("cmesh", value)

    @property
    def accmesh(self) -> typing.Optional[int]:
        """Get or set the The number of negative current collector
        """ # nopep8
        return self._cards[1].get_value("accmesh")

    @accmesh.setter
    def accmesh(self, value: int) -> None:
        self._cards[1].set_value("accmesh", value)

    @property
    def cccmesh(self) -> typing.Optional[int]:
        """Get or set the The number of positive current collector
        """ # nopep8
        return self._cards[1].get_value("cccmesh")

    @cccmesh.setter
    def cccmesh(self, value: int) -> None:
        self._cards[1].set_value("cccmesh", value)

