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

class LoadPyroActuator(KeywordBase):
    """DYNA LOAD_PYRO_ACTUATOR keyword"""

    keyword = "LOAD"
    subkeyword = "PYRO_ACTUATOR"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id")
                    ),
                    Field(
                        "id1",
                        int,
                        10,
                        10,
                        kwargs.get("id1")
                    ),
                    Field(
                        "id2",
                        int,
                        20,
                        10,
                        kwargs.get("id2")
                    ),
                    Field(
                        "csa",
                        float,
                        30,
                        10,
                        kwargs.get("csa")
                    ),
                    Field(
                        "vol",
                        float,
                        40,
                        10,
                        kwargs.get("vol")
                    ),
                    Field(
                        "prs",
                        float,
                        50,
                        10,
                        kwargs.get("prs")
                    ),
                    Field(
                        "dens",
                        float,
                        60,
                        10,
                        kwargs.get("dens")
                    ),
                    Field(
                        "atime",
                        float,
                        70,
                        10,
                        kwargs.get("atime")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "mcid",
                        int,
                        0,
                        10,
                        kwargs.get("mcid")
                    ),
                    Field(
                        "cv",
                        float,
                        10,
                        10,
                        kwargs.get("cv")
                    ),
                    Field(
                        "cp",
                        float,
                        20,
                        10,
                        kwargs.get("cp")
                    ),
                    Field(
                        "temp",
                        float,
                        30,
                        10,
                        kwargs.get("temp")
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Unique ID for actuator.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def id1(self) -> typing.Optional[int]:
        """Get or set the GT.0:	Node ID 1
        LT.0:	Segment set ID 1.
        """ # nopep8
        return self._cards[0].get_value("id1")

    @id1.setter
    def id1(self, value: int) -> None:
        self._cards[0].set_value("id1", value)

    @property
    def id2(self) -> typing.Optional[int]:
        """Get or set the GT.0:	Node ID 2
        LT.0:	Segment set ID 2.
        """ # nopep8
        return self._cards[0].get_value("id2")

    @id2.setter
    def id2(self, value: int) -> None:
        self._cards[0].set_value("id2", value)

    @property
    def csa(self) -> typing.Optional[float]:
        """Get or set the Chamber cross section area.
        """ # nopep8
        return self._cards[0].get_value("csa")

    @csa.setter
    def csa(self, value: float) -> None:
        self._cards[0].set_value("csa", value)

    @property
    def vol(self) -> typing.Optional[float]:
        """Get or set the GT.0:	Initial chamber volume
        EQ.0:	Initial chamber volume given by distance between ID1 and ID2, see Remarks.
        """ # nopep8
        return self._cards[0].get_value("vol")

    @vol.setter
    def vol(self, value: float) -> None:
        self._cards[0].set_value("vol", value)

    @property
    def prs(self) -> typing.Optional[float]:
        """Get or set the Ambient pressure.
        """ # nopep8
        return self._cards[0].get_value("prs")

    @prs.setter
    def prs(self, value: float) -> None:
        self._cards[0].set_value("prs", value)

    @property
    def dens(self) -> typing.Optional[float]:
        """Get or set the Ambient gas density.
        """ # nopep8
        return self._cards[0].get_value("dens")

    @dens.setter
    def dens(self, value: float) -> None:
        self._cards[0].set_value("dens", value)

    @property
    def atime(self) -> typing.Optional[float]:
        """Get or set the Activation time.
        """ # nopep8
        return self._cards[0].get_value("atime")

    @atime.setter
    def atime(self, value: float) -> None:
        self._cards[0].set_value("atime", value)

    @property
    def mcid(self) -> typing.Optional[int]:
        """Get or set the Mass flow curve ID (mass flow as function of time).
        """ # nopep8
        return self._cards[1].get_value("mcid")

    @mcid.setter
    def mcid(self, value: int) -> None:
        self._cards[1].set_value("mcid", value)

    @property
    def cv(self) -> typing.Optional[float]:
        """Get or set the Specific heat capacity at constant pressure.
        """ # nopep8
        return self._cards[1].get_value("cv")

    @cv.setter
    def cv(self, value: float) -> None:
        self._cards[1].set_value("cv", value)

    @property
    def cp(self) -> typing.Optional[float]:
        """Get or set the Specific heat capacity at constant volume.
        """ # nopep8
        return self._cards[1].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        self._cards[1].set_value("cp", value)

    @property
    def temp(self) -> typing.Optional[float]:
        """Get or set the Gas generator temperature.
        """ # nopep8
        return self._cards[1].get_value("temp")

    @temp.setter
    def temp(self, value: float) -> None:
        self._cards[1].set_value("temp", value)

