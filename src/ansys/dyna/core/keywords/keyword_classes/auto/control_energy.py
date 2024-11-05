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

class ControlEnergy(KeywordBase):
    """DYNA CONTROL_ENERGY keyword"""

    keyword = "CONTROL"
    subkeyword = "ENERGY"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "hgen",
                        int,
                        0,
                        10,
                        kwargs.get("hgen", 1)
                    ),
                    Field(
                        "rwen",
                        int,
                        10,
                        10,
                        kwargs.get("rwen", 2)
                    ),
                    Field(
                        "slnten",
                        int,
                        20,
                        10,
                        kwargs.get("slnten", 1)
                    ),
                    Field(
                        "rylen",
                        int,
                        30,
                        10,
                        kwargs.get("rylen", 1)
                    ),
                    Field(
                        "irgen",
                        int,
                        40,
                        10,
                        kwargs.get("irgen", 2)
                    ),
                    Field(
                        "maten",
                        int,
                        50,
                        10,
                        kwargs.get("maten", 1)
                    ),
                    Field(
                        "drlen",
                        int,
                        60,
                        10,
                        kwargs.get("drlen", 1)
                    ),
                    Field(
                        "disen",
                        int,
                        70,
                        10,
                        kwargs.get("disen", 1)
                    ),
                ],
            ),
        ]

    @property
    def hgen(self) -> int:
        """Get or set the Hourglass energy calculation option.
        EQ.1: hourglass energy is not computed (default),
        EQ.2: hourglass energy is computed and included in the energy balance.
        """ # nopep8
        return self._cards[0].get_value("hgen")

    @hgen.setter
    def hgen(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""hgen must be one of {1,2}""")
        self._cards[0].set_value("hgen", value)

    @property
    def rwen(self) -> int:
        """Get or set the Stonewall energy dissipation option:
        EQ.1: energy dissipation is not computed,
        EQ.2: energy dissipation is computed and included in the energy balance (default).
        """ # nopep8
        return self._cards[0].get_value("rwen")

    @rwen.setter
    def rwen(self, value: int) -> None:
        if value not in [2, 1]:
            raise Exception("""rwen must be one of {2,1}""")
        self._cards[0].set_value("rwen", value)

    @property
    def slnten(self) -> int:
        """Get or set the Sliding interface energy dissipation option:
        EQ.1: energy dissipation is not computed,
        EQ.2: energy dissipation is computed and included in the energy balance.
        """ # nopep8
        return self._cards[0].get_value("slnten")

    @slnten.setter
    def slnten(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""slnten must be one of {1,2}""")
        self._cards[0].set_value("slnten", value)

    @property
    def rylen(self) -> int:
        """Get or set the Rayleigh energy dissipation option (damping energy dissipation):
        EQ.1: energy dissipation is not computed (default),
        EQ.2: energy dissipation is computed and included in the energy balance.
        """ # nopep8
        return self._cards[0].get_value("rylen")

    @rylen.setter
    def rylen(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""rylen must be one of {1,2}""")
        self._cards[0].set_value("rylen", value)

    @property
    def irgen(self) -> int:
        """Get or set the Initial reference geometry energy option (included in internal energy, resulting from *INITIAL_FOAM_REFERENCE_GEOMETRY):
        EQ.1:	initial reference geometry energy is not computed,
        EQ.2:	initial reference geometry energy is computed and included in the energy balance as part of the internal energy (default).
        """ # nopep8
        return self._cards[0].get_value("irgen")

    @irgen.setter
    def irgen(self, value: int) -> None:
        if value not in [2, 1]:
            raise Exception("""irgen must be one of {2,1}""")
        self._cards[0].set_value("irgen", value)

    @property
    def maten(self) -> int:
        """Get or set the Detailed material energies option. For a choice of material models (currently supported are 3, 4, 15, 19, 24, 63,81, 82, 98, 104, 105, 106, 107, 123, 124, 188, 224, 225, 240, and 251 for shell and solid elements), internal energy is additionally split into elastic, plastic and damage portions:
        EQ.1:	detailed material energies are not computed(default).
        EQ.2 : detailed material energies are computed and reported as mat_energy_elastic, mat_energy_plastic,and mat_energy_ damage in ASCII file glstatand matsum
        """ # nopep8
        return self._cards[0].get_value("maten")

    @maten.setter
    def maten(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""maten must be one of {1,2}""")
        self._cards[0].set_value("maten", value)

    @property
    def drlen(self) -> int:
        """Get or set the Drilling energy calculation option, for implicit and with use of DRCPSID/DRCPRM on *CONTROL_SHELL:
        EQ.1:	Drilling energy is not computed(default).
        EQ.2 : Drilling energy is computed and included in the energy balance.The drilling energies are reported in the ASCII file glstat, see* DATABASE_OPTION.
        """ # nopep8
        return self._cards[0].get_value("drlen")

    @drlen.setter
    def drlen(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""drlen must be one of {1,2}""")
        self._cards[0].set_value("drlen", value)

    @property
    def disen(self) -> int:
        """Get or set the Dissipation energy calculation option, for implicit:
        EQ.1:	Dissipated energy is not computed(default).
        EQ.2 : Dissipated kinetic and internal energy is computed and included in the energy balance.The dissipation energies are reported in the ASCII file glstat, see* DATABASE_OPTION.
        """ # nopep8
        return self._cards[0].get_value("disen")

    @disen.setter
    def disen(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""disen must be one of {1,2}""")
        self._cards[0].set_value("disen", value)

