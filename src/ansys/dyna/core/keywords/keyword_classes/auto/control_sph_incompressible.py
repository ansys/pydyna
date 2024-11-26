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

class ControlSphIncompressible(KeywordBase):
    """DYNA CONTROL_SPH_INCOMPRESSIBLE keyword"""

    keyword = "CONTROL"
    subkeyword = "SPH_INCOMPRESSIBLE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ibndp",
                        int,
                        0,
                        10,
                        kwargs.get("ibndp", 0)
                    ),
                    Field(
                        "tavg",
                        float,
                        10,
                        10,
                        kwargs.get("tavg", 1.0E-2)
                    ),
                    Field(
                        "tmax",
                        float,
                        20,
                        10,
                        kwargs.get("tmax", 1.0E+20)
                    ),
                    Field(
                        "rol",
                        float,
                        30,
                        10,
                        kwargs.get("rol", 1.0E+20)
                    ),
                    Field(
                        "ihtc",
                        int,
                        40,
                        10,
                        kwargs.get("ihtc", 0)
                    ),
                    Field(
                        "imat",
                        int,
                        50,
                        10,
                        kwargs.get("imat", 0)
                    ),
                ],
            ),
        ]

    @property
    def ibndp(self) -> int:
        """Get or set the Pressure treatment of boundary particles:
        EQ.0:	Pressure on boundary particles is extrapolated from fluid particles.
        EQ.1 : Pressure on boundary particles is explicitly calculated
        """ # nopep8
        return self._cards[0].get_value("ibndp")

    @ibndp.setter
    def ibndp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ibndp must be one of {0,1}""")
        self._cards[0].set_value("ibndp", value)

    @property
    def tavg(self) -> float:
        """Get or set the Tolerance criteria for convergence. If the average relative density (ρ/ρ_0) of particles under compression is below TAVG, this condition is satisfied
        """ # nopep8
        return self._cards[0].get_value("tavg")

    @tavg.setter
    def tavg(self, value: float) -> None:
        self._cards[0].set_value("tavg", value)

    @property
    def tmax(self) -> float:
        """Get or set the Tolerance criteria for convergence. If the maximum relative density (ρ/ρ_0) of particles under compression is below TMAX, this condition is satisfied
        """ # nopep8
        return self._cards[0].get_value("tmax")

    @tmax.setter
    def tmax(self, value: float) -> None:
        self._cards[0].set_value("tmax", value)

    @property
    def rol(self) -> float:
        """Get or set the In certain scenarios, some fluid particles can end up stuck between moving structures and as a result accumulate very large pressure values. If a particle’s relative density contribution from boundaries is above ROL, it is deactivated.
        """ # nopep8
        return self._cards[0].get_value("rol")

    @rol.setter
    def rol(self, value: float) -> None:
        self._cards[0].set_value("rol", value)

    @property
    def ihtc(self) -> int:
        """Get or set the Flag for Heat Transfer Coefficient calculation.
        EQ.0:	HTCs are not calculated.
        EQ.1 : HTCs are calculated based on fluid properties given in * MAT_SPH_INCOMPRESSIBLE_FLUID cards.
        """ # nopep8
        return self._cards[0].get_value("ihtc")

    @ihtc.setter
    def ihtc(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ihtc must be one of {0,1}""")
        self._cards[0].set_value("ihtc", value)

    @property
    def imat(self) -> int:
        """Get or set the Flag for MAT_SPH_INCOMPRESSIBLE_* formulations.
        EQ.0:	Surface tension and surface adhesion forces are calculated based on numerical parameters given in the material cards.
        EQ.1 : Surface tension and surface adhesion forces are calculated based on physical properties given in the material cards.
        """ # nopep8
        return self._cards[0].get_value("imat")

    @imat.setter
    def imat(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""imat must be one of {0,1}""")
        self._cards[0].set_value("imat", value)

