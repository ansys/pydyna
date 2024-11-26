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

class DatabaseAle(KeywordBase):
    """DYNA DATABASE_ALE keyword"""

    keyword = "DATABASE"
    subkeyword = "ALE"

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
                        "setid",
                        int,
                        10,
                        10,
                        kwargs.get("setid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "var1",
                        int,
                        0,
                        10,
                        kwargs.get("var1")
                    ),
                    Field(
                        "var2",
                        int,
                        10,
                        10,
                        kwargs.get("var2")
                    ),
                    Field(
                        "var3",
                        int,
                        20,
                        10,
                        kwargs.get("var3")
                    ),
                    Field(
                        "var4",
                        int,
                        30,
                        10,
                        kwargs.get("var4")
                    ),
                    Field(
                        "var5",
                        int,
                        40,
                        10,
                        kwargs.get("var5")
                    ),
                    Field(
                        "var6",
                        int,
                        50,
                        10,
                        kwargs.get("var6")
                    ),
                    Field(
                        "var7",
                        int,
                        60,
                        10,
                        kwargs.get("var7")
                    ),
                    Field(
                        "var8",
                        int,
                        70,
                        10,
                        kwargs.get("var8")
                    ),
                ],
            ),
        ]

    @property
    def dtout(self) -> typing.Optional[float]:
        """Get or set the Time interval between the outputs.
        """ # nopep8
        return self._cards[0].get_value("dtout")

    @dtout.setter
    def dtout(self, value: float) -> None:
        self._cards[0].set_value("dtout", value)

    @property
    def setid(self) -> typing.Optional[int]:
        """Get or set the ALE element set ID.
        If the model is 1D (*SECTION_ALE1D), the set should be *SET_BEAM
        If the model is 2D (*SECTION_ALE2D), the set should be *SET_SHELL
        If the model is 3D (*SECTION_SOLID), the set should be *SET_SOLID.
        """ # nopep8
        return self._cards[0].get_value("setid")

    @setid.setter
    def setid(self, value: int) -> None:
        self._cards[0].set_value("setid", value)

    @property
    def var1(self) -> typing.Optional[int]:
        """Get or set the Variable rank in the following list:
        LT.0:	|VAR| : rank of the auxiliary variable to be replaced in d3plot (see remark 3)
        EQ.1: xx-stress
        EQ.2: yy-stress
        EQ.3: zz-stress
        EQ.4: xy-stress
        EQ.5: yz-stress
        EQ.6: zx-stress
        EQ.7: plastic strain
        EQ.8: internal energy
        EQ.9: bulk viscosity
        EQ.10: previous volume
        EQ.11: pressure
        EQ.12: mass
        EQ.13: volume
        EQ.14: density
        EQ.15:	kinetic energy
        EQ.16: The 6 stresses are added to the database.
        EQ.17:	Impulse (pressure integrated over time)
        If there is a blank column between 2 variable ranks, the list between these 2 ranks is selected.For example, if the card is as follows :
        1, ,6
        The 6 stresses are added to the database.
        """ # nopep8
        return self._cards[1].get_value("var1")

    @var1.setter
    def var1(self, value: int) -> None:
        self._cards[1].set_value("var1", value)

    @property
    def var2(self) -> typing.Optional[int]:
        """Get or set the Variable rank in the following list:
        LT.0:	|VAR| : rank of the auxiliary variable to be replaced in d3plot (see remark 3)
        EQ.1: xx-stress
        EQ.2: yy-stress
        EQ.3: zz-stress
        EQ.4: xy-stress
        EQ.5: yz-stress
        EQ.6: zx-stress
        EQ.7: plastic strain
        EQ.8: internal energy
        EQ.9: bulk viscosity
        EQ.10: previous volume
        EQ.11: pressure
        EQ.12: mass
        EQ.13: volume
        EQ.14: density
        EQ.15:	kinetic energy
        EQ.16: The 6 stresses are added to the database.
        EQ.17:	Impulse (pressure integrated over time)
        If there is a blank column between 2 variable ranks, the list between these 2 ranks is selected.For example, if the card is as follows :
        1, ,6
        The 6 stresses are added to the database.
        """ # nopep8
        return self._cards[1].get_value("var2")

    @var2.setter
    def var2(self, value: int) -> None:
        self._cards[1].set_value("var2", value)

    @property
    def var3(self) -> typing.Optional[int]:
        """Get or set the Variable rank in the following list:
        LT.0:	|VAR| : rank of the auxiliary variable to be replaced in d3plot (see remark 3)
        EQ.1: xx-stress
        EQ.2: yy-stress
        EQ.3: zz-stress
        EQ.4: xy-stress
        EQ.5: yz-stress
        EQ.6: zx-stress
        EQ.7: plastic strain
        EQ.8: internal energy
        EQ.9: bulk viscosity
        EQ.10: previous volume
        EQ.11: pressure
        EQ.12: mass
        EQ.13: volume
        EQ.14: density
        EQ.15:	kinetic energy
        EQ.16: The 6 stresses are added to the database.
        EQ.17:	Impulse (pressure integrated over time)
        If there is a blank column between 2 variable ranks, the list between these 2 ranks is selected.For example, if the card is as follows :
        1, ,6
        The 6 stresses are added to the database.
        """ # nopep8
        return self._cards[1].get_value("var3")

    @var3.setter
    def var3(self, value: int) -> None:
        self._cards[1].set_value("var3", value)

    @property
    def var4(self) -> typing.Optional[int]:
        """Get or set the Variable rank in the following list:
        LT.0:	|VAR| : rank of the auxiliary variable to be replaced in d3plot (see remark 3)
        EQ.1: xx-stress
        EQ.2: yy-stress
        EQ.3: zz-stress
        EQ.4: xy-stress
        EQ.5: yz-stress
        EQ.6: zx-stress
        EQ.7: plastic strain
        EQ.8: internal energy
        EQ.9: bulk viscosity
        EQ.10: previous volume
        EQ.11: pressure
        EQ.12: mass
        EQ.13: volume
        EQ.14: density
        EQ.15:	kinetic energy
        EQ.16: The 6 stresses are added to the database.
        EQ.17:	Impulse (pressure integrated over time)
        If there is a blank column between 2 variable ranks, the list between these 2 ranks is selected.For example, if the card is as follows :
        1, ,6
        The 6 stresses are added to the database.
        """ # nopep8
        return self._cards[1].get_value("var4")

    @var4.setter
    def var4(self, value: int) -> None:
        self._cards[1].set_value("var4", value)

    @property
    def var5(self) -> typing.Optional[int]:
        """Get or set the Variable rank in the following list:
        LT.0:	|VAR| : rank of the auxiliary variable to be replaced in d3plot (see remark 3)
        EQ.1: xx-stress
        EQ.2: yy-stress
        EQ.3: zz-stress
        EQ.4: xy-stress
        EQ.5: yz-stress
        EQ.6: zx-stress
        EQ.7: plastic strain
        EQ.8: internal energy
        EQ.9: bulk viscosity
        EQ.10: previous volume
        EQ.11: pressure
        EQ.12: mass
        EQ.13: volume
        EQ.14: density
        EQ.15:	kinetic energy
        EQ.16: The 6 stresses are added to the database.
        EQ.17:	Impulse (pressure integrated over time)
        If there is a blank column between 2 variable ranks, the list between these 2 ranks is selected.For example, if the card is as follows :
        1, ,6
        The 6 stresses are added to the database.
        """ # nopep8
        return self._cards[1].get_value("var5")

    @var5.setter
    def var5(self, value: int) -> None:
        self._cards[1].set_value("var5", value)

    @property
    def var6(self) -> typing.Optional[int]:
        """Get or set the Variable rank in the following list:
        LT.0:	|VAR| : rank of the auxiliary variable to be replaced in d3plot (see remark 3)
        EQ.1: xx-stress
        EQ.2: yy-stress
        EQ.3: zz-stress
        EQ.4: xy-stress
        EQ.5: yz-stress
        EQ.6: zx-stress
        EQ.7: plastic strain
        EQ.8: internal energy
        EQ.9: bulk viscosity
        EQ.10: previous volume
        EQ.11: pressure
        EQ.12: mass
        EQ.13: volume
        EQ.14: density
        EQ.15:	kinetic energy
        EQ.16: The 6 stresses are added to the database.
        EQ.17:	Impulse (pressure integrated over time)
        If there is a blank column between 2 variable ranks, the list between these 2 ranks is selected.For example, if the card is as follows :
        1, ,6
        The 6 stresses are added to the database.
        """ # nopep8
        return self._cards[1].get_value("var6")

    @var6.setter
    def var6(self, value: int) -> None:
        self._cards[1].set_value("var6", value)

    @property
    def var7(self) -> typing.Optional[int]:
        """Get or set the Variable rank in the following list:
        LT.0:	|VAR| : rank of the auxiliary variable to be replaced in d3plot (see remark 3)
        EQ.1: xx-stress
        EQ.2: yy-stress
        EQ.3: zz-stress
        EQ.4: xy-stress
        EQ.5: yz-stress
        EQ.6: zx-stress
        EQ.7: plastic strain
        EQ.8: internal energy
        EQ.9: bulk viscosity
        EQ.10: previous volume
        EQ.11: pressure
        EQ.12: mass
        EQ.13: volume
        EQ.14: density
        EQ.15:	kinetic energy
        EQ.16: The 6 stresses are added to the database.
        EQ.17:	Impulse (pressure integrated over time)
        If there is a blank column between 2 variable ranks, the list between these 2 ranks is selected.For example, if the card is as follows :
        1, ,6
        The 6 stresses are added to the database.
        """ # nopep8
        return self._cards[1].get_value("var7")

    @var7.setter
    def var7(self, value: int) -> None:
        self._cards[1].set_value("var7", value)

    @property
    def var8(self) -> typing.Optional[int]:
        """Get or set the Variable rank in the following list:
        LT.0:	|VAR| : rank of the auxiliary variable to be replaced in d3plot (see remark 3)
        EQ.1: xx-stress
        EQ.2: yy-stress
        EQ.3: zz-stress
        EQ.4: xy-stress
        EQ.5: yz-stress
        EQ.6: zx-stress
        EQ.7: plastic strain
        EQ.8: internal energy
        EQ.9: bulk viscosity
        EQ.10: previous volume
        EQ.11: pressure
        EQ.12: mass
        EQ.13: volume
        EQ.14: density
        EQ.15:	kinetic energy
        EQ.16: The 6 stresses are added to the database.
        EQ.17:	Impulse (pressure integrated over time)
        If there is a blank column between 2 variable ranks, the list between these 2 ranks is selected.For example, if the card is as follows :
        1, ,6
        The 6 stresses are added to the database.
        """ # nopep8
        return self._cards[1].get_value("var8")

    @var8.setter
    def var8(self, value: int) -> None:
        self._cards[1].set_value("var8", value)

