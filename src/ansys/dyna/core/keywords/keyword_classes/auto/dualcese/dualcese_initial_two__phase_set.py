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

"""Module providing the DualceseInitialTwo_PhaseSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEINITIALTWO_PHASESET_CARD0 = (
    FieldSchema("esid", int, 0, 10, None),
    FieldSchema("ifunc", int, 10, 10, None),
)

_DUALCESEINITIALTWO_PHASESET_CARD1 = (
    FieldSchema("z1", float, 0, 10, None),
    FieldSchema("uic", float, 10, 10, None),
    FieldSchema("vic", float, 20, 10, None),
    FieldSchema("wic", float, 30, 10, None),
    FieldSchema("rho_1", float, 40, 10, None),
    FieldSchema("rho_2", float, 50, 10, None),
    FieldSchema("pic", float, 60, 10, None),
    FieldSchema("tic", float, 70, 10, None),
)

class DualceseInitialTwo_PhaseSet(KeywordBase):
    """DYNA DUALCESE_INITIAL_TWO-PHASE_SET keyword"""

    keyword = "DUALCESE"
    subkeyword = "INITIAL_TWO-PHASE_SET"

    def __init__(self, **kwargs):
        """Initialize the DualceseInitialTwo_PhaseSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEINITIALTWO_PHASESET_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DUALCESEINITIALTWO_PHASESET_CARD1,
                **kwargs,
            ),        ]
    @property
    def esid(self) -> typing.Optional[int]:
        """Get or set the Element set ID (see *DUALCESE_ELEMENTSET)
        """ # nopep8
        return self._cards[0].get_value("esid")

    @esid.setter
    def esid(self, value: int) -> None:
        """Set the esid property."""
        self._cards[0].set_value("esid", value)

    @property
    def ifunc(self) -> typing.Optional[int]:
        """Get or set the Option to define initial conditions using *DEFINE_FUNCTION cards:
        EQ.0:	Not in use.
        EQ.1:All values for initial velocity, pressure, density, and temperature now refer to *DEFINE_FUNCTION IDs. In these functions, the following parameters are allowed: f(x,y,z), meaning that each variable’s initial profile is a function of position
        """ # nopep8
        return self._cards[0].get_value("ifunc")

    @ifunc.setter
    def ifunc(self, value: int) -> None:
        """Set the ifunc property."""
        self._cards[0].set_value("ifunc", value)

    @property
    def z1(self) -> typing.Optional[float]:
        """Get or set the Volume fraction of material 1 (or color function).This is usually a value of 0 or 1. For numerical stability, however, use a very small value instead of zero
        """ # nopep8
        return self._cards[1].get_value("z1")

    @z1.setter
    def z1(self, value: float) -> None:
        """Set the z1 property."""
        self._cards[1].set_value("z1", value)

    @property
    def uic(self) -> typing.Optional[float]:
        """Get or set the Multiphase flow velocity components in the x, y,and z - directions, respectively
        """ # nopep8
        return self._cards[1].get_value("uic")

    @uic.setter
    def uic(self, value: float) -> None:
        """Set the uic property."""
        self._cards[1].set_value("uic", value)

    @property
    def vic(self) -> typing.Optional[float]:
        """Get or set the Multiphase flow velocity components in the x, y,and z - directions, respectively
        """ # nopep8
        return self._cards[1].get_value("vic")

    @vic.setter
    def vic(self, value: float) -> None:
        """Set the vic property."""
        self._cards[1].set_value("vic", value)

    @property
    def wic(self) -> typing.Optional[float]:
        """Get or set the Multiphase flow velocity components in the x, y,and z - directions, respectively
        """ # nopep8
        return self._cards[1].get_value("wic")

    @wic.setter
    def wic(self, value: float) -> None:
        """Set the wic property."""
        self._cards[1].set_value("wic", value)

    @property
    def rho_1(self) -> typing.Optional[float]:
        """Get or set the Density of fluid  1
        """ # nopep8
        return self._cards[1].get_value("rho_1")

    @rho_1.setter
    def rho_1(self, value: float) -> None:
        """Set the rho_1 property."""
        self._cards[1].set_value("rho_1", value)

    @property
    def rho_2(self) -> typing.Optional[float]:
        """Get or set the Density of fluid  2
        """ # nopep8
        return self._cards[1].get_value("rho_2")

    @rho_2.setter
    def rho_2(self, value: float) -> None:
        """Set the rho_2 property."""
        self._cards[1].set_value("rho_2", value)

    @property
    def pic(self) -> typing.Optional[float]:
        """Get or set the Equilibrium multifluid pressure
        """ # nopep8
        return self._cards[1].get_value("pic")

    @pic.setter
    def pic(self, value: float) -> None:
        """Set the pic property."""
        self._cards[1].set_value("pic", value)

    @property
    def tic(self) -> typing.Optional[float]:
        """Get or set the Equilibrium multifluid temperaturee
        """ # nopep8
        return self._cards[1].get_value("tic")

    @tic.setter
    def tic(self, value: float) -> None:
        """Set the tic property."""
        self._cards[1].set_value("tic", value)

