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

class ChemistryControl0D(KeywordBase):
    """DYNA CHEMISTRY_CONTROL_0D keyword"""

    keyword = "CHEMISTRY"
    subkeyword = "CONTROL_0D"

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
                        "compid",
                        int,
                        10,
                        10,
                        kwargs.get("compid")
                    ),
                    Field(
                        "soltyp",
                        int,
                        20,
                        10,
                        kwargs.get("soltyp", 1)
                    ),
                    Field(
                        "plotdt",
                        float,
                        30,
                        10,
                        kwargs.get("plotdt", 1.0e-6)
                    ),
                    Field(
                        "csp_sel",
                        int,
                        40,
                        10,
                        kwargs.get("csp_sel", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dt",
                        float,
                        0,
                        10,
                        kwargs.get("dt")
                    ),
                    Field(
                        "tlimit",
                        float,
                        10,
                        10,
                        kwargs.get("tlimit")
                    ),
                    Field(
                        "tic",
                        float,
                        20,
                        10,
                        kwargs.get("tic")
                    ),
                    Field(
                        "pic",
                        float,
                        30,
                        10,
                        kwargs.get("pic")
                    ),
                    Field(
                        "ric",
                        float,
                        40,
                        10,
                        kwargs.get("ric")
                    ),
                    Field(
                        "eic",
                        float,
                        50,
                        10,
                        kwargs.get("eic")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ampl",
                        float,
                        0,
                        10,
                        kwargs.get("ampl")
                    ),
                    Field(
                        "ycut",
                        float,
                        10,
                        10,
                        kwargs.get("ycut")
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Identifier for this 0D computation.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def compid(self) -> typing.Optional[int]:
        """Get or set the Chemical composition identifier of composition to use.
        """ # nopep8
        return self._cards[0].get_value("compid")

    @compid.setter
    def compid(self, value: int) -> None:
        self._cards[0].set_value("compid", value)

    @property
    def soltyp(self) -> int:
        """Get or set the Type of 0D calculation:
        EQ.1: Isochoric
        EQ.2: Isobaric
        """ # nopep8
        return self._cards[0].get_value("soltyp")

    @soltyp.setter
    def soltyp(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""soltyp must be one of {1,2}""")
        self._cards[0].set_value("soltyp", value)

    @property
    def plotdt(self) -> float:
        """Get or set the Error tolerance for the calculation.
        """ # nopep8
        return self._cards[0].get_value("plotdt")

    @plotdt.setter
    def plotdt(self, value: float) -> None:
        self._cards[0].set_value("plotdt", value)

    @property
    def csp_sel(self) -> int:
        """Get or set the CSP solver option:
        EQ.0: Do not use the CSP solver, and ignore the AMPL and YCUT parameters (default).
        GT.0: Use the CSP solver, with the AMPL and YCUT parameters.
        """ # nopep8
        return self._cards[0].get_value("csp_sel")

    @csp_sel.setter
    def csp_sel(self, value: int) -> None:
        self._cards[0].set_value("csp_sel", value)

    @property
    def dt(self) -> typing.Optional[float]:
        """Get or set the Initial time step.
        """ # nopep8
        return self._cards[1].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[1].set_value("dt", value)

    @property
    def tlimit(self) -> typing.Optional[float]:
        """Get or set the Time limit for the simulation.
        """ # nopep8
        return self._cards[1].get_value("tlimit")

    @tlimit.setter
    def tlimit(self, value: float) -> None:
        self._cards[1].set_value("tlimit", value)

    @property
    def tic(self) -> typing.Optional[float]:
        """Get or set the Initial temperature.
        """ # nopep8
        return self._cards[1].get_value("tic")

    @tic.setter
    def tic(self, value: float) -> None:
        self._cards[1].set_value("tic", value)

    @property
    def pic(self) -> typing.Optional[float]:
        """Get or set the Initial pressure.
        """ # nopep8
        return self._cards[1].get_value("pic")

    @pic.setter
    def pic(self, value: float) -> None:
        self._cards[1].set_value("pic", value)

    @property
    def ric(self) -> typing.Optional[float]:
        """Get or set the Initial density.
        """ # nopep8
        return self._cards[1].get_value("ric")

    @ric.setter
    def ric(self, value: float) -> None:
        self._cards[1].set_value("ric", value)

    @property
    def eic(self) -> typing.Optional[float]:
        """Get or set the Initial internal energy.
        """ # nopep8
        return self._cards[1].get_value("eic")

    @eic.setter
    def eic(self, value: float) -> None:
        self._cards[1].set_value("eic", value)

    @property
    def ampl(self) -> typing.Optional[float]:
        """Get or set the Relative accuracy for the mass fraction of a chemical species in the Chemkin input file.
        """ # nopep8
        return self._cards[2].get_value("ampl")

    @ampl.setter
    def ampl(self, value: float) -> None:
        self._cards[2].set_value("ampl", value)

    @property
    def ycut(self) -> typing.Optional[float]:
        """Get or set the Absolute accuracy for the mass fraction of a chemical species in the Chemkin input file.
        """ # nopep8
        return self._cards[2].get_value("ycut")

    @ycut.setter
    def ycut(self, value: float) -> None:
        self._cards[2].set_value("ycut", value)

