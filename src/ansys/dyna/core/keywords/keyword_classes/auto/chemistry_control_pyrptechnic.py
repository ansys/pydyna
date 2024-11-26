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

class ChemistryControlPyrptechnic(KeywordBase):
    """DYNA CHEMISTRY_CONTROL_PYRPTECHNIC keyword"""

    keyword = "CHEMISTRY"
    subkeyword = "CONTROL_PYRPTECHNIC"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "comp1id",
                        int,
                        0,
                        10,
                        kwargs.get("comp1id")
                    ),
                    Field(
                        "vol1",
                        float,
                        10,
                        10,
                        kwargs.get("vol1")
                    ),
                    Field(
                        "area1",
                        float,
                        20,
                        10,
                        kwargs.get("area1")
                    ),
                    Field(
                        "cd1",
                        float,
                        30,
                        10,
                        kwargs.get("cd1")
                    ),
                    Field(
                        "p1",
                        float,
                        40,
                        10,
                        kwargs.get("p1")
                    ),
                    Field(
                        "t1",
                        float,
                        50,
                        10,
                        kwargs.get("t1")
                    ),
                    Field(
                        "delp1",
                        float,
                        60,
                        10,
                        kwargs.get("delp1")
                    ),
                    Field(
                        "tflame",
                        float,
                        70,
                        10,
                        kwargs.get("tflame")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "comp2id",
                        int,
                        0,
                        10,
                        kwargs.get("comp2id")
                    ),
                    Field(
                        "vol2",
                        float,
                        10,
                        10,
                        kwargs.get("vol2")
                    ),
                    Field(
                        "area2",
                        float,
                        20,
                        10,
                        kwargs.get("area2")
                    ),
                    Field(
                        "cd2",
                        float,
                        30,
                        10,
                        kwargs.get("cd2")
                    ),
                    Field(
                        "p2",
                        float,
                        40,
                        10,
                        kwargs.get("p2")
                    ),
                    Field(
                        "t2",
                        float,
                        50,
                        10,
                        kwargs.get("t2")
                    ),
                    Field(
                        "delp2",
                        float,
                        60,
                        10,
                        kwargs.get("delp2")
                    ),
                    Field(
                        "truntime",
                        float,
                        70,
                        10,
                        kwargs.get("truntime")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "comp3id",
                        int,
                        0,
                        10,
                        kwargs.get("comp3id")
                    ),
                    Field(
                        "vol3",
                        float,
                        10,
                        10,
                        kwargs.get("vol3")
                    ),
                    Field(
                        "p3",
                        float,
                        20,
                        10,
                        kwargs.get("p3")
                    ),
                    Field(
                        "t3",
                        float,
                        30,
                        10,
                        kwargs.get("t3")
                    ),
                    Field(
                        "ptime",
                        float,
                        40,
                        10,
                        kwargs.get("ptime")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "file",
                        str,
                        0,
                        80,
                        kwargs.get("file")
                    ),
                ],
            ),
        ]

    @property
    def comp1id(self) -> typing.Optional[int]:
        """Get or set the Chemical composition identifier of composition to use in the chamber
        """ # nopep8
        return self._cards[0].get_value("comp1id")

    @comp1id.setter
    def comp1id(self, value: int) -> None:
        self._cards[0].set_value("comp1id", value)

    @property
    def vol1(self) -> typing.Optional[float]:
        """Get or set the Volume of the chamber.
        """ # nopep8
        return self._cards[0].get_value("vol1")

    @vol1.setter
    def vol1(self, value: float) -> None:
        self._cards[0].set_value("vol1", value)

    @property
    def area1(self) -> typing.Optional[float]:
        """Get or set the Area of the chamber
        """ # nopep8
        return self._cards[0].get_value("area1")

    @area1.setter
    def area1(self, value: float) -> None:
        self._cards[0].set_value("area1", value)

    @property
    def cd1(self) -> typing.Optional[float]:
        """Get or set the Discharge coefficient of the chamber
        """ # nopep8
        return self._cards[0].get_value("cd1")

    @cd1.setter
    def cd1(self, value: float) -> None:
        self._cards[0].set_value("cd1", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the Pressure in the chamber
        """ # nopep8
        return self._cards[0].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        self._cards[0].set_value("p1", value)

    @property
    def t1(self) -> typing.Optional[float]:
        """Get or set the Temperature in the chamber
        """ # nopep8
        return self._cards[0].get_value("t1")

    @t1.setter
    def t1(self, value: float) -> None:
        self._cards[0].set_value("t1", value)

    @property
    def delp1(self) -> typing.Optional[float]:
        """Get or set the Rupture pressure in the chamber
        """ # nopep8
        return self._cards[0].get_value("delp1")

    @delp1.setter
    def delp1(self, value: float) -> None:
        self._cards[0].set_value("delp1", value)

    @property
    def tflame(self) -> typing.Optional[float]:
        """Get or set the Adiabatic flame temperature.
        """ # nopep8
        return self._cards[0].get_value("tflame")

    @tflame.setter
    def tflame(self, value: float) -> None:
        self._cards[0].set_value("tflame", value)

    @property
    def comp2id(self) -> typing.Optional[int]:
        """Get or set the Chemical composition identifier of composition to use in the plenum
        """ # nopep8
        return self._cards[1].get_value("comp2id")

    @comp2id.setter
    def comp2id(self, value: int) -> None:
        self._cards[1].set_value("comp2id", value)

    @property
    def vol2(self) -> typing.Optional[float]:
        """Get or set the Volume of the plenum
        """ # nopep8
        return self._cards[1].get_value("vol2")

    @vol2.setter
    def vol2(self, value: float) -> None:
        self._cards[1].set_value("vol2", value)

    @property
    def area2(self) -> typing.Optional[float]:
        """Get or set the Area of the plenum
        """ # nopep8
        return self._cards[1].get_value("area2")

    @area2.setter
    def area2(self, value: float) -> None:
        self._cards[1].set_value("area2", value)

    @property
    def cd2(self) -> typing.Optional[float]:
        """Get or set the Discharge coefficient of the plenum
        """ # nopep8
        return self._cards[1].get_value("cd2")

    @cd2.setter
    def cd2(self, value: float) -> None:
        self._cards[1].set_value("cd2", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the Pressure in the plenum
        """ # nopep8
        return self._cards[1].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        self._cards[1].set_value("p2", value)

    @property
    def t2(self) -> typing.Optional[float]:
        """Get or set the Temperature in the plenum
        """ # nopep8
        return self._cards[1].get_value("t2")

    @t2.setter
    def t2(self, value: float) -> None:
        self._cards[1].set_value("t2", value)

    @property
    def delp2(self) -> typing.Optional[float]:
        """Get or set the Rupture pressure in the plenum
        """ # nopep8
        return self._cards[1].get_value("delp2")

    @delp2.setter
    def delp2(self, value: float) -> None:
        self._cards[1].set_value("delp2", value)

    @property
    def truntime(self) -> typing.Optional[float]:
        """Get or set the Total run time
        """ # nopep8
        return self._cards[1].get_value("truntime")

    @truntime.setter
    def truntime(self, value: float) -> None:
        self._cards[1].set_value("truntime", value)

    @property
    def comp3id(self) -> typing.Optional[int]:
        """Get or set the Chemical composition identifier of composition to use in the airbag
        """ # nopep8
        return self._cards[2].get_value("comp3id")

    @comp3id.setter
    def comp3id(self, value: int) -> None:
        self._cards[2].set_value("comp3id", value)

    @property
    def vol3(self) -> typing.Optional[float]:
        """Get or set the Volume of the airbag
        """ # nopep8
        return self._cards[2].get_value("vol3")

    @vol3.setter
    def vol3(self, value: float) -> None:
        self._cards[2].set_value("vol3", value)

    @property
    def p3(self) -> typing.Optional[float]:
        """Get or set the Pressure in the airbag
        """ # nopep8
        return self._cards[2].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        self._cards[2].set_value("p3", value)

    @property
    def t3(self) -> typing.Optional[float]:
        """Get or set the Temperature in the airbag.
        """ # nopep8
        return self._cards[2].get_value("t3")

    @t3.setter
    def t3(self, value: float) -> None:
        self._cards[2].set_value("t3", value)

    @property
    def ptime(self) -> typing.Optional[float]:
        """Get or set the Time interval for output of time history data to FILE.
        """ # nopep8
        return self._cards[2].get_value("ptime")

    @ptime.setter
    def ptime(self, value: float) -> None:
        self._cards[2].set_value("ptime", value)

    @property
    def file(self) -> typing.Optional[str]:
        """Get or set the Name of the lsda file in which to write the results of the inflator simulation.Two load curves are written out to this file: mass flow rate and total temperature as a function of time.
        """ # nopep8
        return self._cards[3].get_value("file")

    @file.setter
    def file(self, value: str) -> None:
        self._cards[3].set_value("file", value)

