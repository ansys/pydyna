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

"""Module providing the ControlSphIncompressible class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLSPHINCOMPRESSIBLE_CARD0 = (
    FieldSchema("ibndp", int, 0, 10, 0),
    FieldSchema("tavg", float, 10, 10, 0.01),
    FieldSchema("tmax", float, 20, 10, 1e+20),
    FieldSchema("rol", float, 30, 10, 1e+20),
    FieldSchema("ihtc", int, 40, 10, 0),
    FieldSchema("imat", int, 50, 10, 0),
)

class ControlSphIncompressible(KeywordBase):
    """DYNA CONTROL_SPH_INCOMPRESSIBLE keyword"""

    keyword = "CONTROL"
    subkeyword = "SPH_INCOMPRESSIBLE"

    def __init__(self, **kwargs):
        """Initialize the ControlSphIncompressible class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLSPHINCOMPRESSIBLE_CARD0,
                **kwargs,
            ),        ]
    @property
    def ibndp(self) -> int:
        """Get or set the Pressure treatment of boundary particles:
        EQ.0:	Pressure on boundary particles is extrapolated from fluid particles.
        EQ.1 : Pressure on boundary particles is explicitly calculated
        """ # nopep8
        return self._cards[0].get_value("ibndp")

    @ibndp.setter
    def ibndp(self, value: int) -> None:
        """Set the ibndp property."""
        if value not in [0, 1, None]:
            raise Exception("""ibndp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ibndp", value)

    @property
    def tavg(self) -> float:
        """Get or set the Tolerance criteria for convergence. If the average relative density (ρ/ρ_0) of particles under compression is below TAVG, this condition is satisfied
        """ # nopep8
        return self._cards[0].get_value("tavg")

    @tavg.setter
    def tavg(self, value: float) -> None:
        """Set the tavg property."""
        self._cards[0].set_value("tavg", value)

    @property
    def tmax(self) -> float:
        """Get or set the Tolerance criteria for convergence. If the maximum relative density (ρ/ρ_0) of particles under compression is below TMAX, this condition is satisfied
        """ # nopep8
        return self._cards[0].get_value("tmax")

    @tmax.setter
    def tmax(self, value: float) -> None:
        """Set the tmax property."""
        self._cards[0].set_value("tmax", value)

    @property
    def rol(self) -> float:
        """Get or set the In certain scenarios, some fluid particles can end up stuck between moving structures and as a result accumulate very large pressure values. If a particle’s relative density contribution from boundaries is above ROL, it is deactivated.
        """ # nopep8
        return self._cards[0].get_value("rol")

    @rol.setter
    def rol(self, value: float) -> None:
        """Set the rol property."""
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
        """Set the ihtc property."""
        if value not in [0, 1, None]:
            raise Exception("""ihtc must be `None` or one of {0,1}.""")
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
        """Set the imat property."""
        if value not in [0, 1, None]:
            raise Exception("""imat must be `None` or one of {0,1}.""")
        self._cards[0].set_value("imat", value)

