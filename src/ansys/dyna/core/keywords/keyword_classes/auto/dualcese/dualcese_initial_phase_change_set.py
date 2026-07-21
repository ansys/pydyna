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

"""Module providing the DualceseInitialPhaseChangeSet class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEINITIALPHASECHANGESET_CARD0 = (
    FieldSchema("esid", int, 0, 10, None),
    FieldSchema("ifunc", int, 10, 10, None),
)

_DUALCESEINITIALPHASECHANGESET_CARD1 = (
    FieldSchema("uic", float, 0, 10, None),
    FieldSchema("vic", float, 10, 10, None),
    FieldSchema("wic", float, 20, 10, None),
    FieldSchema("rho", float, 30, 10, None),
    FieldSchema("pic", float, 40, 10, None),
    FieldSchema("tic", float, 50, 10, None),
    FieldSchema("y1", float, 60, 10, None),
    FieldSchema("y2", float, 70, 10, None),
)

_DUALCESEINITIALPHASECHANGESET_CARD2 = (
    FieldSchema("y3", float, 0, 10, None),
)

class DualceseInitialPhaseChangeSet(KeywordBase):
    """DYNA DUALCESE_INITIAL_PHASE_CHANGE_SET keyword"""

    keyword = "DUALCESE"
    subkeyword = "INITIAL_PHASE_CHANGE_SET"

    def __init__(self, **kwargs):
        """Initialize the DualceseInitialPhaseChangeSet class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEINITIALPHASECHANGESET_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEINITIALPHASECHANGESET_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEINITIALPHASECHANGESET_CARD2,
                **kwargs,
            ),
        ]
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
        """Get or set the Option to define initial conditions with *DEFINE_FUNCTION:
        EQ.0: Not in use.
        EQ.1: All values for initial velocity, pressure,and temperature now refer to *DEFINE_FUNCTION IDs.In these functions, the following parameters are allowed: f(x, y, z).Thus, each variables initial profile is a function of position.
        """ # nopep8
        return self._cards[0].get_value("ifunc")

    @ifunc.setter
    def ifunc(self, value: int) -> None:
        """Set the ifunc property."""
        self._cards[0].set_value("ifunc", value)

    @property
    def uic(self) -> typing.Optional[float]:
        """Get or set the Multiphase flow velocity components in the x, y, and z-directions, respectively
        """ # nopep8
        return self._cards[1].get_value("uic")

    @uic.setter
    def uic(self, value: float) -> None:
        """Set the uic property."""
        self._cards[1].set_value("uic", value)

    @property
    def vic(self) -> typing.Optional[float]:
        """Get or set the Multiphase flow velocity components in the x, y, and z-directions, respectively
        """ # nopep8
        return self._cards[1].get_value("vic")

    @vic.setter
    def vic(self, value: float) -> None:
        """Set the vic property."""
        self._cards[1].set_value("vic", value)

    @property
    def wic(self) -> typing.Optional[float]:
        """Get or set the Multiphase flow velocity components in the x, y, and z-directions, respectively
        """ # nopep8
        return self._cards[1].get_value("wic")

    @wic.setter
    def wic(self, value: float) -> None:
        """Set the wic property."""
        self._cards[1].set_value("wic", value)

    @property
    def rho(self) -> typing.Optional[float]:
        """Get or set the Mixture density
        """ # nopep8
        return self._cards[1].get_value("rho")

    @rho.setter
    def rho(self, value: float) -> None:
        """Set the rho property."""
        self._cards[1].set_value("rho", value)

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
        """Get or set the Equilibrium multifluid temperature
        """ # nopep8
        return self._cards[1].get_value("tic")

    @tic.setter
    def tic(self, value: float) -> None:
        """Set the tic property."""
        self._cards[1].set_value("tic", value)

    @property
    def y1(self) -> typing.Optional[float]:
        """Get or set the Mass fraction of fluid 2
        """ # nopep8
        return self._cards[1].get_value("y1")

    @y1.setter
    def y1(self, value: float) -> None:
        """Set the y1 property."""
        self._cards[1].set_value("y1", value)

    @property
    def y2(self) -> typing.Optional[float]:
        """Get or set the Mass fraction of fluid 3
        """ # nopep8
        return self._cards[1].get_value("y2")

    @y2.setter
    def y2(self, value: float) -> None:
        """Set the y2 property."""
        self._cards[1].set_value("y2", value)

    @property
    def y3(self) -> typing.Optional[float]:
        """Get or set the Mass fraction of fluid 3
        """ # nopep8
        return self._cards[2].get_value("y3")

    @y3.setter
    def y3(self, value: float) -> None:
        """Set the y3 property."""
        self._cards[2].set_value("y3", value)

