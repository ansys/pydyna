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

"""Module providing the IcfdDatabaseTwinbuilder class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDDATABASETWINBUILDER_CARD0 = (
    FieldSchema("outrp", str, 0, 10, None),
    FieldSchema("inex1", str, 10, 10, None),
    FieldSchema("inex2", str, 20, 10, None),
    FieldSchema("inex3", str, 30, 10, None),
    FieldSchema("inex4", str, 40, 10, None),
    FieldSchema("inex5", str, 50, 10, None),
    FieldSchema("inex6", str, 60, 10, None),
    FieldSchema("inex7", str, 70, 10, None),
)

class IcfdDatabaseTwinbuilder(KeywordBase):
    """DYNA ICFD_DATABASE_TWINBUILDER keyword"""

    keyword = "ICFD"
    subkeyword = "DATABASE_TWINBUILDER"

    def __init__(self, **kwargs):
        """Initialize the IcfdDatabaseTwinbuilder class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDDATABASETWINBUILDER_CARD0,
                **kwargs,
            ),
        ]
    @property
    def outrp(self) -> typing.Optional[str]:
        """Get or set the Name of the output variable or response. All the fields are nodal.
        EQ.pres: Pressure
        EQ.apres: Time average pressure
        EQ.vel: Velocity
        EQ.avel: Time average velocity
        EQ.lset: Level Set
        EQ.temp: Temperature
        EQ.atemp: Time average temperature
        """ # nopep8
        return self._cards[0].get_value("outrp")

    @outrp.setter
    def outrp(self, value: str) -> None:
        """Set the outrp property."""
        self._cards[0].set_value("outrp", value)

    @property
    def inex1(self) -> typing.Optional[str]:
        """Get or set the Name of the input variables or excitations. Field values like pressure, velocity and temperature are nodal arrays of boundary conditions. In the case of material properties, the values written to the excitation's files are scalars. A combination of up to seven excitations can be used for each output.
        EQ.pres: Pressure values at imposed pressure boundary condition.
        EQ.vel: Velocity values at inflow boundary conditions.
        EQ.temp: Temperature values at imposed temperature boundary conditions.
        EQ.rho: Fluid density
        EQ.mu: Fluid viscosity
        EQ.hc: Heat capacity
        EQ.tc: Thermal conductivity
        """ # nopep8
        return self._cards[0].get_value("inex1")

    @inex1.setter
    def inex1(self, value: str) -> None:
        """Set the inex1 property."""
        self._cards[0].set_value("inex1", value)

    @property
    def inex2(self) -> typing.Optional[str]:
        """Get or set the Name of the input variables or excitations. Field values like pressure, velocity and temperature are nodal arrays of boundary conditions. In the case of material properties, the values written to the excitation's files are scalars. A combination of up to seven excitations can be used for each output.
        EQ.pres: Pressure values at imposed pressure boundary condition.
        EQ.vel: Velocity values at inflow boundary conditions.
        EQ.temp: Temperature values at imposed temperature boundary conditions.
        EQ.rho: Fluid density
        EQ.mu: Fluid viscosity
        EQ.hc: Heat capacity
        EQ.tc: Thermal conductivity
        """ # nopep8
        return self._cards[0].get_value("inex2")

    @inex2.setter
    def inex2(self, value: str) -> None:
        """Set the inex2 property."""
        self._cards[0].set_value("inex2", value)

    @property
    def inex3(self) -> typing.Optional[str]:
        """Get or set the Name of the input variables or excitations. Field values like pressure, velocity and temperature are nodal arrays of boundary conditions. In the case of material properties, the values written to the excitation's files are scalars. A combination of up to seven excitations can be used for each output.
        EQ.pres: Pressure values at imposed pressure boundary condition.
        EQ.vel: Velocity values at inflow boundary conditions.
        EQ.temp: Temperature values at imposed temperature boundary conditions.
        EQ.rho: Fluid density
        EQ.mu: Fluid viscosity
        EQ.hc: Heat capacity
        EQ.tc: Thermal conductivity
        """ # nopep8
        return self._cards[0].get_value("inex3")

    @inex3.setter
    def inex3(self, value: str) -> None:
        """Set the inex3 property."""
        self._cards[0].set_value("inex3", value)

    @property
    def inex4(self) -> typing.Optional[str]:
        """Get or set the Name of the input variables or excitations. Field values like pressure, velocity and temperature are nodal arrays of boundary conditions. In the case of material properties, the values written to the excitation's files are scalars. A combination of up to seven excitations can be used for each output.
        EQ.pres: Pressure values at imposed pressure boundary condition.
        EQ.vel: Velocity values at inflow boundary conditions.
        EQ.temp: Temperature values at imposed temperature boundary conditions.
        EQ.rho: Fluid density
        EQ.mu: Fluid viscosity
        EQ.hc: Heat capacity
        EQ.tc: Thermal conductivity
        """ # nopep8
        return self._cards[0].get_value("inex4")

    @inex4.setter
    def inex4(self, value: str) -> None:
        """Set the inex4 property."""
        self._cards[0].set_value("inex4", value)

    @property
    def inex5(self) -> typing.Optional[str]:
        """Get or set the Name of the input variables or excitations. Field values like pressure, velocity and temperature are nodal arrays of boundary conditions. In the case of material properties, the values written to the excitation's files are scalars. A combination of up to seven excitations can be used for each output.
        EQ.pres: Pressure values at imposed pressure boundary condition.
        EQ.vel: Velocity values at inflow boundary conditions.
        EQ.temp: Temperature values at imposed temperature boundary conditions.
        EQ.rho: Fluid density
        EQ.mu: Fluid viscosity
        EQ.hc: Heat capacity
        EQ.tc: Thermal conductivity
        """ # nopep8
        return self._cards[0].get_value("inex5")

    @inex5.setter
    def inex5(self, value: str) -> None:
        """Set the inex5 property."""
        self._cards[0].set_value("inex5", value)

    @property
    def inex6(self) -> typing.Optional[str]:
        """Get or set the Name of the input variables or excitations. Field values like pressure, velocity and temperature are nodal arrays of boundary conditions. In the case of material properties, the values written to the excitation's files are scalars. A combination of up to seven excitations can be used for each output.
        EQ.pres: Pressure values at imposed pressure boundary condition.
        EQ.vel: Velocity values at inflow boundary conditions.
        EQ.temp: Temperature values at imposed temperature boundary conditions.
        EQ.rho: Fluid density
        EQ.mu: Fluid viscosity
        EQ.hc: Heat capacity
        EQ.tc: Thermal conductivity
        """ # nopep8
        return self._cards[0].get_value("inex6")

    @inex6.setter
    def inex6(self, value: str) -> None:
        """Set the inex6 property."""
        self._cards[0].set_value("inex6", value)

    @property
    def inex7(self) -> typing.Optional[str]:
        """Get or set the Name of the input variables or excitations. Field values like pressure, velocity and temperature are nodal arrays of boundary conditions. In the case of material properties, the values written to the excitation's files are scalars. A combination of up to seven excitations can be used for each output.
        EQ.pres: Pressure values at imposed pressure boundary condition.
        EQ.vel: Velocity values at inflow boundary conditions.
        EQ.temp: Temperature values at imposed temperature boundary conditions.
        EQ.rho: Fluid density
        EQ.mu: Fluid viscosity
        EQ.hc: Heat capacity
        EQ.tc: Thermal conductivity
        """ # nopep8
        return self._cards[0].get_value("inex7")

    @inex7.setter
    def inex7(self, value: str) -> None:
        """Set the inex7 property."""
        self._cards[0].set_value("inex7", value)

