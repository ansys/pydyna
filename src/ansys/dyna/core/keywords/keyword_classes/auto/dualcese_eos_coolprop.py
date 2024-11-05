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

class DualceseEosCoolprop(KeywordBase):
    """DYNA DUALCESE_EOS_COOLPROP keyword"""

    keyword = "DUALCESE"
    subkeyword = "EOS_COOLPROP"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eosid",
                        int,
                        0,
                        10,
                        kwargs.get("eosid")
                    ),
                    Field(
                        "ncomp",
                        int,
                        10,
                        10,
                        kwargs.get("ncomp")
                    ),
                    Field(
                        "type",
                        str,
                        20,
                        10,
                        kwargs.get("type")
                    ),
                    Field(
                        "phase",
                        str,
                        30,
                        10,
                        kwargs.get("phase", "GAS")
                    ),
                    Field(
                        "tabular",
                        str,
                        40,
                        10,
                        kwargs.get("tabular")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "mol_fr1",
                        float,
                        0,
                        10,
                        kwargs.get("mol_fr1")
                    ),
                    Field(
                        "mol_fr2",
                        float,
                        10,
                        10,
                        kwargs.get("mol_fr2")
                    ),
                    Field(
                        "mol_fr3",
                        float,
                        20,
                        10,
                        kwargs.get("mol_fr3")
                    ),
                    Field(
                        "mol_fr4",
                        float,
                        30,
                        10,
                        kwargs.get("mol_fr4")
                    ),
                    Field(
                        "mol_fr5",
                        float,
                        40,
                        10,
                        kwargs.get("mol_fr5")
                    ),
                    Field(
                        "mol_fr6",
                        float,
                        50,
                        10,
                        kwargs.get("mol_fr6")
                    ),
                    Field(
                        "mol_fr7",
                        float,
                        60,
                        10,
                        kwargs.get("mol_fr7")
                    ),
                    Field(
                        "mol_fr8",
                        float,
                        70,
                        10,
                        kwargs.get("mol_fr8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "n_t",
                        float,
                        0,
                        10,
                        kwargs.get("n_t")
                    ),
                    Field(
                        "n_den",
                        float,
                        10,
                        10,
                        kwargs.get("n_den")
                    ),
                    Field(
                        "den_low",
                        float,
                        20,
                        10,
                        kwargs.get("den_low")
                    ),
                    Field(
                        "den_high",
                        float,
                        30,
                        10,
                        kwargs.get("den_high")
                    ),
                    Field(
                        "t_low",
                        float,
                        40,
                        10,
                        kwargs.get("t_low")
                    ),
                    Field(
                        "t_high",
                        float,
                        50,
                        10,
                        kwargs.get("t_high")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "fluidname",
                        str,
                        0,
                        80,
                        kwargs.get("fluidname")
                    ),
                ],
            ),
        ]

    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the ID for this EOS
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        self._cards[0].set_value("eosid", value)

    @property
    def ncomp(self) -> typing.Optional[int]:
        """Get or set the Number of components in the fluid composition
        """ # nopep8
        return self._cards[0].get_value("ncomp")

    @ncomp.setter
    def ncomp(self, value: int) -> None:
        self._cards[0].set_value("ncomp", value)

    @property
    def type(self) -> typing.Optional[str]:
        """Get or set the The fluid type.
        EQ.PURE:  A single component fluid(default)
        EQ.PSEUDOPURE : A predefined fluid mixture
        amespace
        Q.MIXTURE : A fluid mixture made up of NCOMP components
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: str) -> None:
        self._cards[0].set_value("type", value)

    @property
    def phase(self) -> str:
        """Get or set the Phase of the fluid.
        EQ.GAS: gas phase
        EQ.LIQUID : liquid phase
        """ # nopep8
        return self._cards[0].get_value("phase")

    @phase.setter
    def phase(self, value: str) -> None:
        self._cards[0].set_value("phase", value)

    @property
    def tabular(self) -> typing.Optional[str]:
        """Get or set the Type of lookup tables to build for this EOS.
        EQ.BLANK: Default(no table lookup)
        EQ.P_EIN : Build tables of pressure and internal energy, both as a function of densityand temperature.
        """ # nopep8
        return self._cards[0].get_value("tabular")

    @tabular.setter
    def tabular(self, value: str) -> None:
        self._cards[0].set_value("tabular", value)

    @property
    def mol_fr1(self) -> typing.Optional[float]:
        """Get or set the Mole fraction
        """ # nopep8
        return self._cards[1].get_value("mol_fr1")

    @mol_fr1.setter
    def mol_fr1(self, value: float) -> None:
        self._cards[1].set_value("mol_fr1", value)

    @property
    def mol_fr2(self) -> typing.Optional[float]:
        """Get or set the Mole fraction
        """ # nopep8
        return self._cards[1].get_value("mol_fr2")

    @mol_fr2.setter
    def mol_fr2(self, value: float) -> None:
        self._cards[1].set_value("mol_fr2", value)

    @property
    def mol_fr3(self) -> typing.Optional[float]:
        """Get or set the Mole fraction
        """ # nopep8
        return self._cards[1].get_value("mol_fr3")

    @mol_fr3.setter
    def mol_fr3(self, value: float) -> None:
        self._cards[1].set_value("mol_fr3", value)

    @property
    def mol_fr4(self) -> typing.Optional[float]:
        """Get or set the Mole fraction
        """ # nopep8
        return self._cards[1].get_value("mol_fr4")

    @mol_fr4.setter
    def mol_fr4(self, value: float) -> None:
        self._cards[1].set_value("mol_fr4", value)

    @property
    def mol_fr5(self) -> typing.Optional[float]:
        """Get or set the Mole fraction
        """ # nopep8
        return self._cards[1].get_value("mol_fr5")

    @mol_fr5.setter
    def mol_fr5(self, value: float) -> None:
        self._cards[1].set_value("mol_fr5", value)

    @property
    def mol_fr6(self) -> typing.Optional[float]:
        """Get or set the Mole fraction
        """ # nopep8
        return self._cards[1].get_value("mol_fr6")

    @mol_fr6.setter
    def mol_fr6(self, value: float) -> None:
        self._cards[1].set_value("mol_fr6", value)

    @property
    def mol_fr7(self) -> typing.Optional[float]:
        """Get or set the Mole fraction
        """ # nopep8
        return self._cards[1].get_value("mol_fr7")

    @mol_fr7.setter
    def mol_fr7(self, value: float) -> None:
        self._cards[1].set_value("mol_fr7", value)

    @property
    def mol_fr8(self) -> typing.Optional[float]:
        """Get or set the Mole fraction
        """ # nopep8
        return self._cards[1].get_value("mol_fr8")

    @mol_fr8.setter
    def mol_fr8(self, value: float) -> None:
        self._cards[1].set_value("mol_fr8", value)

    @property
    def n_t(self) -> typing.Optional[float]:
        """Get or set the Number of temperature values in the tables
        """ # nopep8
        return self._cards[2].get_value("n_t")

    @n_t.setter
    def n_t(self, value: float) -> None:
        self._cards[2].set_value("n_t", value)

    @property
    def n_den(self) -> typing.Optional[float]:
        """Get or set the Number of density values (on a log scale) in the tables
        """ # nopep8
        return self._cards[2].get_value("n_den")

    @n_den.setter
    def n_den(self, value: float) -> None:
        self._cards[2].set_value("n_den", value)

    @property
    def den_low(self) -> typing.Optional[float]:
        """Get or set the Minimum density available in the tables (in model units)
        """ # nopep8
        return self._cards[2].get_value("den_low")

    @den_low.setter
    def den_low(self, value: float) -> None:
        self._cards[2].set_value("den_low", value)

    @property
    def den_high(self) -> typing.Optional[float]:
        """Get or set the Maximum density available in the tables (in model units)
        """ # nopep8
        return self._cards[2].get_value("den_high")

    @den_high.setter
    def den_high(self, value: float) -> None:
        self._cards[2].set_value("den_high", value)

    @property
    def t_low(self) -> typing.Optional[float]:
        """Get or set the Minimum temperature available in the tables (in model units)
        """ # nopep8
        return self._cards[2].get_value("t_low")

    @t_low.setter
    def t_low(self, value: float) -> None:
        self._cards[2].set_value("t_low", value)

    @property
    def t_high(self) -> typing.Optional[float]:
        """Get or set the Maximum temperature available in the tables (in model units)
        """ # nopep8
        return self._cards[2].get_value("t_high")

    @t_high.setter
    def t_high(self, value: float) -> None:
        self._cards[2].set_value("t_high", value)

    @property
    def fluidname(self) -> typing.Optional[str]:
        """Get or set the Name of a fluid that has an EOS in CoolProp. For a list of the supported pure and pseudo-pure fluids
        """ # nopep8
        return self._cards[3].get_value("fluidname")

    @fluidname.setter
    def fluidname(self, value: str) -> None:
        self._cards[3].set_value("fluidname", value)

