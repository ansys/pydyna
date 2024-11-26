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

class ChemistryControlInflator(KeywordBase):
    """DYNA CHEMISTRY_CONTROL_INFLATOR keyword"""

    keyword = "CHEMISTRY"
    subkeyword = "CONTROL_INFLATOR"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "model",
                        int,
                        0,
                        10,
                        kwargs.get("model", 1)
                    ),
                    Field(
                        "out_type",
                        int,
                        10,
                        10,
                        kwargs.get("out_type", 0)
                    ),
                    Field(
                        "truntim",
                        float,
                        20,
                        10,
                        kwargs.get("truntim")
                    ),
                    Field(
                        "delt",
                        float,
                        30,
                        10,
                        kwargs.get("delt")
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
            Card(
                [
                    Field(
                        "density",
                        str,
                        0,
                        10,
                        kwargs.get("density")
                    ),
                    Field(
                        "species name",
                        str,
                        10,
                        70,
                        kwargs.get("species name")
                    ),
                ],
            ),
        ]

    @property
    def model(self) -> int:
        """Get or set the Type of inflator model to compute.
        EQ.1:Pyrotechnic model.
        EQ.2:Hybrid model with cold flow option in the gas chamber.
        EQ.3:Hybrid model with heat flow in the gas chamber.
        """ # nopep8
        return self._cards[0].get_value("model")

    @model.setter
    def model(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""model must be one of {1,2,3}""")
        self._cards[0].set_value("model", value)

    @property
    def out_type(self) -> int:
        """Get or set the Selects the output file format that will be used in an airbag simulation.EQ.0:Screen output.
        EQ.1:CESE compressible flow solver.
        EQ.2:ALE solver.
        EQ.3:CPM solver(with 2nd-order expansion of Cp)
        EQ.4:CPM solver(with 4th-order expansion of Cp)
        """ # nopep8
        return self._cards[0].get_value("out_type")

    @out_type.setter
    def out_type(self, value: int) -> None:
        if value not in [0, 1, 2, 3, 4]:
            raise Exception("""out_type must be one of {0,1,2,3,4}""")
        self._cards[0].set_value("out_type", value)

    @property
    def truntim(self) -> typing.Optional[float]:
        """Get or set the Total run time.
        """ # nopep8
        return self._cards[0].get_value("truntim")

    @truntim.setter
    def truntim(self, value: float) -> None:
        self._cards[0].set_value("truntim", value)

    @property
    def delt(self) -> typing.Optional[float]:
        """Get or set the Delta(t) to use in the model calculation.
        """ # nopep8
        return self._cards[0].get_value("delt")

    @delt.setter
    def delt(self, value: float) -> None:
        self._cards[0].set_value("delt", value)

    @property
    def ptime(self) -> typing.Optional[float]:
        """Get or set the Time interval for output of time history data to FILE.
        """ # nopep8
        return self._cards[0].get_value("ptime")

    @ptime.setter
    def ptime(self, value: float) -> None:
        self._cards[0].set_value("ptime", value)

    @property
    def file(self) -> typing.Optional[str]:
        """Get or set the Name of the ASCII file in which to write the time history data and other data output by the inflator simulation.
        """ # nopep8
        return self._cards[1].get_value("file")

    @file.setter
    def file(self, value: str) -> None:
        self._cards[1].set_value("file", value)

    @property
    def density(self) -> typing.Optional[str]:
        """Get or set the Density of a condensed-phase species present in the inflator.
        """ # nopep8
        return self._cards[2].get_value("density")

    @density.setter
    def density(self, value: str) -> None:
        self._cards[2].set_value("density", value)

    @property
    def species_name(self) -> typing.Optional[str]:
        """Get or set the Chemkin-compatible name of a condensed-phase species.
        """ # nopep8
        return self._cards[2].get_value("species name")

    @species_name.setter
    def species_name(self, value: str) -> None:
        self._cards[2].set_value("species name", value)

