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
    FieldSchema("irmv", int, 60, 10, 0),
    FieldSchema("acmp", float, 70, 10, 0.0),
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
            ),
        ]
    @property
    def ibndp(self) -> int:
        """Get or set the Pressure treatment of boundary particles:
        EQ.0: Pressure on boundary particles is extrapolated from fluid particles.
        EQ.1: Pressure on boundary particles is explicitly calculated
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
        """Get or set the Tolerance criteria for convergence. If the average relative density (/_0) of particles under compression is below TAVG, this condition is satisfied
        """ # nopep8
        return self._cards[0].get_value("tavg")

    @tavg.setter
    def tavg(self, value: float) -> None:
        """Set the tavg property."""
        self._cards[0].set_value("tavg", value)

    @property
    def tmax(self) -> float:
        """Get or set the Tolerance criteria for convergence. If the maximum relative density (/_0) of particles under compression is below TMAX, this condition is satisfied
        """ # nopep8
        return self._cards[0].get_value("tmax")

    @tmax.setter
    def tmax(self, value: float) -> None:
        """Set the tmax property."""
        self._cards[0].set_value("tmax", value)

    @property
    def rol(self) -> float:
        """Get or set the Stuck particle detection criteria. In certain scenarios, some fluid particles can end up stuck between moving structures and as a result accumulate very large pressure values. If a particles relative density contribution from boundaries is above ROL, it is deactivated. Its displacement is prescribed from the interpolated structure motion until it is sufficiently away from the structure at which point it is activated again.
        """ # nopep8
        return self._cards[0].get_value("rol")

    @rol.setter
    def rol(self, value: float) -> None:
        """Set the rol property."""
        self._cards[0].set_value("rol", value)

    @property
    def ihtc(self) -> int:
        """Get or set the Flag for Heat Transfer Coefficient calculation.
        EQ.0: HTCs are not calculated.
        EQ.1: HTCs are calculated based on fluid properties given in *MAT_SPH_INCOMPRESSIBLE_FLUID cards.
        LT.0: |IHTC| is a function ID defining HTCs based on seven arguments: heat capacity, thermal conductivity, dynamic viscosity, density, tangential velocity relative to the closest segment, distance traveled along the surface, and the ratio of the distance from the closest segment to particle radius. (see *DEFINE_FUNCTION).
        """ # nopep8
        return self._cards[0].get_value("ihtc")

    @ihtc.setter
    def ihtc(self, value: int) -> None:
        """Set the ihtc property."""
        self._cards[0].set_value("ihtc", value)

    @property
    def imat(self) -> int:
        """Get or set the Flag for MAT_SPH_INCOMPRESSIBLE_* formulations.
        EQ.0: Surface tension and surface adhesion forces are calculated based on numerical parameters given in the material cards.
        EQ.1: Surface tension and surface adhesion forces are calculated based on the physical properties given in the material cards.
        """ # nopep8
        return self._cards[0].get_value("imat")

    @imat.setter
    def imat(self, value: int) -> None:
        """Set the imat property."""
        if value not in [0, 1, None]:
            raise Exception("""imat must be `None` or one of {0,1}.""")
        self._cards[0].set_value("imat", value)

    @property
    def irmv(self) -> int:
        """Get or set the Remove initially interpenetrated particles. Fluid particles that are too close to structural particles will be deactivated on initialization:
        EQ.0: Do not remove interpenetrated particles.
        EQ.1: Remove interpenetrated particles.
        """ # nopep8
        return self._cards[0].get_value("irmv")

    @irmv.setter
    def irmv(self, value: int) -> None:
        """Set the irmv property."""
        if value not in [0, 1, None]:
            raise Exception("""irmv must be `None` or one of {0,1}.""")
        self._cards[0].set_value("irmv", value)

    @property
    def acmp(self) -> float:
        """Get or set the Artificial compressibility term. For an analysis with a time-varying time step, we strongly recommend adding artificial compressibility to avoid oscillations and instabilities. We typically recommend a small value of [1.0x10]**(-8)  Pa**(-1). Note that this parameter is unit sensitive. Thus, the recommended value needs to be converted to the unit system used in the model.
        """ # nopep8
        return self._cards[0].get_value("acmp")

    @acmp.setter
    def acmp(self, value: float) -> None:
        """Set the acmp property."""
        self._cards[0].set_value("acmp", value)

