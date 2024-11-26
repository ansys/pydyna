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

class ChemistryInflatorProperties(KeywordBase):
    """DYNA CHEMISTRY_INFLATOR_PROPERTIES keyword"""

    keyword = "CHEMISTRY"
    subkeyword = "INFLATOR_PROPERTIES"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "comp_id",
                        int,
                        0,
                        10,
                        kwargs.get("comp_id", 1)
                    ),
                    Field(
                        "pdia",
                        float,
                        10,
                        10,
                        kwargs.get("pdia")
                    ),
                    Field(
                        "pheight",
                        float,
                        20,
                        10,
                        kwargs.get("pheight")
                    ),
                    Field(
                        "pmass",
                        float,
                        30,
                        10,
                        kwargs.get("pmass")
                    ),
                    Field(
                        "totmass",
                        float,
                        40,
                        10,
                        kwargs.get("totmass")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "tflame",
                        float,
                        0,
                        10,
                        kwargs.get("tflame")
                    ),
                    Field(
                        "pindex",
                        float,
                        10,
                        10,
                        kwargs.get("pindex")
                    ),
                    Field(
                        "a0",
                        float,
                        20,
                        10,
                        kwargs.get("a0")
                    ),
                    Field(
                        "tdelay",
                        float,
                        30,
                        10,
                        kwargs.get("tdelay")
                    ),
                    Field(
                        "risetime",
                        float,
                        40,
                        10,
                        kwargs.get("risetime")
                    ),
                ],
            ),
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
                        "delt1",
                        float,
                        70,
                        10,
                        kwargs.get("delt1")
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
                        "delt2",
                        float,
                        70,
                        10,
                        kwargs.get("delt2")
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
                ],
            ),
            Card(
                [
                    Field(
                        "com42id",
                        int,
                        0,
                        10,
                        kwargs.get("com42id")
                    ),
                    Field(
                        "vol4",
                        float,
                        10,
                        10,
                        kwargs.get("vol4")
                    ),
                    Field(
                        "area4",
                        float,
                        20,
                        10,
                        kwargs.get("area4")
                    ),
                    Field(
                        "cd4",
                        float,
                        30,
                        10,
                        kwargs.get("cd4")
                    ),
                    Field(
                        "p4",
                        float,
                        40,
                        10,
                        kwargs.get("p4")
                    ),
                    Field(
                        "t4",
                        float,
                        50,
                        10,
                        kwargs.get("t4")
                    ),
                    Field(
                        "delp4",
                        float,
                        60,
                        10,
                        kwargs.get("delp4")
                    ),
                    Field(
                        "delt4",
                        float,
                        70,
                        10,
                        kwargs.get("delt4")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "comp5id",
                        int,
                        0,
                        10,
                        kwargs.get("comp5id")
                    ),
                    Field(
                        "vol5",
                        float,
                        10,
                        10,
                        kwargs.get("vol5")
                    ),
                    Field(
                        "area5",
                        float,
                        20,
                        10,
                        kwargs.get("area5")
                    ),
                    Field(
                        "cd5",
                        float,
                        30,
                        10,
                        kwargs.get("cd5")
                    ),
                    Field(
                        "p5",
                        float,
                        40,
                        10,
                        kwargs.get("p5")
                    ),
                    Field(
                        "t5",
                        float,
                        50,
                        10,
                        kwargs.get("t5")
                    ),
                    Field(
                        "delp5",
                        float,
                        60,
                        10,
                        kwargs.get("delp5")
                    ),
                    Field(
                        "delt5",
                        float,
                        70,
                        10,
                        kwargs.get("delt5")
                    ),
                ],
            ),
        ]

    @property
    def comp_id(self) -> int:
        """Get or set the Chemical composition identifier of the composition for the steady-state propellant combustion.
        """ # nopep8
        return self._cards[0].get_value("comp_id")

    @comp_id.setter
    def comp_id(self, value: int) -> None:
        self._cards[0].set_value("comp_id", value)

    @property
    def pdia(self) -> typing.Optional[float]:
        """Get or set the Propellant diameter.
        """ # nopep8
        return self._cards[0].get_value("pdia")

    @pdia.setter
    def pdia(self, value: float) -> None:
        self._cards[0].set_value("pdia", value)

    @property
    def pheight(self) -> typing.Optional[float]:
        """Get or set the Propellant height.
        """ # nopep8
        return self._cards[0].get_value("pheight")

    @pheight.setter
    def pheight(self, value: float) -> None:
        self._cards[0].set_value("pheight", value)

    @property
    def pmass(self) -> typing.Optional[float]:
        """Get or set the Individual cylinder propellant mass.
        """ # nopep8
        return self._cards[0].get_value("pmass")

    @pmass.setter
    def pmass(self, value: float) -> None:
        self._cards[0].set_value("pmass", value)

    @property
    def totmass(self) -> typing.Optional[float]:
        """Get or set the Total propellant mass.
        """ # nopep8
        return self._cards[0].get_value("totmass")

    @totmass.setter
    def totmass(self, value: float) -> None:
        self._cards[0].set_value("totmass", value)

    @property
    def tflame(self) -> typing.Optional[float]:
        """Get or set the Adiabatic flame temperature.
        """ # nopep8
        return self._cards[1].get_value("tflame")

    @tflame.setter
    def tflame(self, value: float) -> None:
        self._cards[1].set_value("tflame", value)

    @property
    def pindex(self) -> typing.Optional[float]:
        """Get or set the Power of the pressure in rate of burn model.
        """ # nopep8
        return self._cards[1].get_value("pindex")

    @pindex.setter
    def pindex(self, value: float) -> None:
        self._cards[1].set_value("pindex", value)

    @property
    def a0(self) -> typing.Optional[float]:
        """Get or set the Steady-state constant.
        """ # nopep8
        return self._cards[1].get_value("a0")

    @a0.setter
    def a0(self, value: float) -> None:
        self._cards[1].set_value("a0", value)

    @property
    def tdelay(self) -> typing.Optional[float]:
        """Get or set the Ignition time delay.
        """ # nopep8
        return self._cards[1].get_value("tdelay")

    @tdelay.setter
    def tdelay(self, value: float) -> None:
        self._cards[1].set_value("tdelay", value)

    @property
    def risetime(self) -> typing.Optional[float]:
        """Get or set the Rise time.
        """ # nopep8
        return self._cards[1].get_value("risetime")

    @risetime.setter
    def risetime(self, value: float) -> None:
        self._cards[1].set_value("risetime", value)

    @property
    def comp1id(self) -> typing.Optional[int]:
        """Get or set the Chemical composition identifier of composition to use in the combustion chamber.
        """ # nopep8
        return self._cards[2].get_value("comp1id")

    @comp1id.setter
    def comp1id(self, value: int) -> None:
        self._cards[2].set_value("comp1id", value)

    @property
    def vol1(self) -> typing.Optional[float]:
        """Get or set the Volume of the combustion chamber.
        """ # nopep8
        return self._cards[2].get_value("vol1")

    @vol1.setter
    def vol1(self, value: float) -> None:
        self._cards[2].set_value("vol1", value)

    @property
    def area1(self) -> typing.Optional[float]:
        """Get or set the Area of the combustion chamber.
        """ # nopep8
        return self._cards[2].get_value("area1")

    @area1.setter
    def area1(self, value: float) -> None:
        self._cards[2].set_value("area1", value)

    @property
    def cd1(self) -> typing.Optional[float]:
        """Get or set the Discharge coefficient of the combustion chamber.
        """ # nopep8
        return self._cards[2].get_value("cd1")

    @cd1.setter
    def cd1(self, value: float) -> None:
        self._cards[2].set_value("cd1", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the Pressure in the combustion chamber.
        """ # nopep8
        return self._cards[2].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        self._cards[2].set_value("p1", value)

    @property
    def t1(self) -> typing.Optional[float]:
        """Get or set the Temperature in the combustion chamber.
        """ # nopep8
        return self._cards[2].get_value("t1")

    @t1.setter
    def t1(self, value: float) -> None:
        self._cards[2].set_value("t1", value)

    @property
    def delp1(self) -> typing.Optional[float]:
        """Get or set the Rupture pressure in the combustion chamber.
        """ # nopep8
        return self._cards[2].get_value("delp1")

    @delp1.setter
    def delp1(self, value: float) -> None:
        self._cards[2].set_value("delp1", value)

    @property
    def delt1(self) -> typing.Optional[float]:
        """Get or set the Elapsed time for breaking the burst disk between the chambers
        """ # nopep8
        return self._cards[2].get_value("delt1")

    @delt1.setter
    def delt1(self, value: float) -> None:
        self._cards[2].set_value("delt1", value)

    @property
    def comp2id(self) -> typing.Optional[int]:
        """Get or set the Chemical composition identifier of composition to use in the gas plenum.
        """ # nopep8
        return self._cards[3].get_value("comp2id")

    @comp2id.setter
    def comp2id(self, value: int) -> None:
        self._cards[3].set_value("comp2id", value)

    @property
    def vol2(self) -> typing.Optional[float]:
        """Get or set the Volume of the gas plenum.
        """ # nopep8
        return self._cards[3].get_value("vol2")

    @vol2.setter
    def vol2(self, value: float) -> None:
        self._cards[3].set_value("vol2", value)

    @property
    def area2(self) -> typing.Optional[float]:
        """Get or set the Area of the gas plenum.
        """ # nopep8
        return self._cards[3].get_value("area2")

    @area2.setter
    def area2(self, value: float) -> None:
        self._cards[3].set_value("area2", value)

    @property
    def cd2(self) -> typing.Optional[float]:
        """Get or set the Discharge coefficient of the gas plenum.
        """ # nopep8
        return self._cards[3].get_value("cd2")

    @cd2.setter
    def cd2(self, value: float) -> None:
        self._cards[3].set_value("cd2", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the Pressure in the gas plenum.
        """ # nopep8
        return self._cards[3].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        self._cards[3].set_value("p2", value)

    @property
    def t2(self) -> typing.Optional[float]:
        """Get or set the Temperature in the gas plenum.
        """ # nopep8
        return self._cards[3].get_value("t2")

    @t2.setter
    def t2(self, value: float) -> None:
        self._cards[3].set_value("t2", value)

    @property
    def delp2(self) -> typing.Optional[float]:
        """Get or set the Rupture pressure in the gas plenum.
        """ # nopep8
        return self._cards[3].get_value("delp2")

    @delp2.setter
    def delp2(self, value: float) -> None:
        self._cards[3].set_value("delp2", value)

    @property
    def delt2(self) -> typing.Optional[float]:
        """Get or set the Elapsed time for breaking the burst disk between the chambers
        """ # nopep8
        return self._cards[3].get_value("delt2")

    @delt2.setter
    def delt2(self, value: float) -> None:
        self._cards[3].set_value("delt2", value)

    @property
    def comp3id(self) -> typing.Optional[int]:
        """Get or set the Chemical composition identifier of composition to use in the tank.
        """ # nopep8
        return self._cards[4].get_value("comp3id")

    @comp3id.setter
    def comp3id(self, value: int) -> None:
        self._cards[4].set_value("comp3id", value)

    @property
    def vol3(self) -> typing.Optional[float]:
        """Get or set the Volume of the tank.
        """ # nopep8
        return self._cards[4].get_value("vol3")

    @vol3.setter
    def vol3(self, value: float) -> None:
        self._cards[4].set_value("vol3", value)

    @property
    def p3(self) -> typing.Optional[float]:
        """Get or set the Pressure in the tank.
        """ # nopep8
        return self._cards[4].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        self._cards[4].set_value("p3", value)

    @property
    def t3(self) -> typing.Optional[float]:
        """Get or set the Temperature in the tank.
        """ # nopep8
        return self._cards[4].get_value("t3")

    @t3.setter
    def t3(self, value: float) -> None:
        self._cards[4].set_value("t3", value)

    @property
    def com42id(self) -> typing.Optional[int]:
        """Get or set the Chemical composition identifier of composition to use in the gas plenum.
        """ # nopep8
        return self._cards[5].get_value("com42id")

    @com42id.setter
    def com42id(self, value: int) -> None:
        self._cards[5].set_value("com42id", value)

    @property
    def vol4(self) -> typing.Optional[float]:
        """Get or set the Volume of the gas plenum.
        """ # nopep8
        return self._cards[5].get_value("vol4")

    @vol4.setter
    def vol4(self, value: float) -> None:
        self._cards[5].set_value("vol4", value)

    @property
    def area4(self) -> typing.Optional[float]:
        """Get or set the Area of the gas plenum.
        """ # nopep8
        return self._cards[5].get_value("area4")

    @area4.setter
    def area4(self, value: float) -> None:
        self._cards[5].set_value("area4", value)

    @property
    def cd4(self) -> typing.Optional[float]:
        """Get or set the Discharge coefficient of the gas plenum.
        """ # nopep8
        return self._cards[5].get_value("cd4")

    @cd4.setter
    def cd4(self, value: float) -> None:
        self._cards[5].set_value("cd4", value)

    @property
    def p4(self) -> typing.Optional[float]:
        """Get or set the Pressure in the gas plenum.
        """ # nopep8
        return self._cards[5].get_value("p4")

    @p4.setter
    def p4(self, value: float) -> None:
        self._cards[5].set_value("p4", value)

    @property
    def t4(self) -> typing.Optional[float]:
        """Get or set the Temperature in the gas plenum.
        """ # nopep8
        return self._cards[5].get_value("t4")

    @t4.setter
    def t4(self, value: float) -> None:
        self._cards[5].set_value("t4", value)

    @property
    def delp4(self) -> typing.Optional[float]:
        """Get or set the Rupture pressure in the gas plenum.
        """ # nopep8
        return self._cards[5].get_value("delp4")

    @delp4.setter
    def delp4(self, value: float) -> None:
        self._cards[5].set_value("delp4", value)

    @property
    def delt4(self) -> typing.Optional[float]:
        """Get or set the Elapsed time for breaking the burst disk between the chambers
        """ # nopep8
        return self._cards[5].get_value("delt4")

    @delt4.setter
    def delt4(self, value: float) -> None:
        self._cards[5].set_value("delt4", value)

    @property
    def comp5id(self) -> typing.Optional[int]:
        """Get or set the Chemical composition identifier of composition to use in the gas plenum.
        """ # nopep8
        return self._cards[6].get_value("comp5id")

    @comp5id.setter
    def comp5id(self, value: int) -> None:
        self._cards[6].set_value("comp5id", value)

    @property
    def vol5(self) -> typing.Optional[float]:
        """Get or set the Volume of the gas plenum.
        """ # nopep8
        return self._cards[6].get_value("vol5")

    @vol5.setter
    def vol5(self, value: float) -> None:
        self._cards[6].set_value("vol5", value)

    @property
    def area5(self) -> typing.Optional[float]:
        """Get or set the Area of the gas plenum.
        """ # nopep8
        return self._cards[6].get_value("area5")

    @area5.setter
    def area5(self, value: float) -> None:
        self._cards[6].set_value("area5", value)

    @property
    def cd5(self) -> typing.Optional[float]:
        """Get or set the Discharge coefficient of the gas plenum.
        """ # nopep8
        return self._cards[6].get_value("cd5")

    @cd5.setter
    def cd5(self, value: float) -> None:
        self._cards[6].set_value("cd5", value)

    @property
    def p5(self) -> typing.Optional[float]:
        """Get or set the Pressure in the gas plenum.
        """ # nopep8
        return self._cards[6].get_value("p5")

    @p5.setter
    def p5(self, value: float) -> None:
        self._cards[6].set_value("p5", value)

    @property
    def t5(self) -> typing.Optional[float]:
        """Get or set the Temperature in the gas plenum.
        """ # nopep8
        return self._cards[6].get_value("t5")

    @t5.setter
    def t5(self, value: float) -> None:
        self._cards[6].set_value("t5", value)

    @property
    def delp5(self) -> typing.Optional[float]:
        """Get or set the Rupture pressure in the gas plenum.
        """ # nopep8
        return self._cards[6].get_value("delp5")

    @delp5.setter
    def delp5(self, value: float) -> None:
        self._cards[6].set_value("delp5", value)

    @property
    def delt5(self) -> typing.Optional[float]:
        """Get or set the Elapsed time for breaking the burst disk between the chambers
        """ # nopep8
        return self._cards[6].get_value("delt5")

    @delt5.setter
    def delt5(self, value: float) -> None:
        self._cards[6].set_value("delt5", value)

