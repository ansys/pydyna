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

"""Module providing the DualceseInitialRegion class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESEINITIALREGION_CARD0 = (
    FieldSchema("regid", int, 0, 10, None),
    FieldSchema("ifunc", int, 10, 10, 0),
)

_DUALCESEINITIALREGION_CARD1 = (
    FieldSchema("u", float, 0, 10, None),
    FieldSchema("v", float, 10, 10, None),
    FieldSchema("w", float, 20, 10, None),
    FieldSchema("rho", float, 30, 10, None),
    FieldSchema("p", float, 40, 10, None),
    FieldSchema("t", float, 50, 10, None),
)

class DualceseInitialRegion(KeywordBase):
    """DYNA DUALCESE_INITIAL_REGION keyword"""

    keyword = "DUALCESE"
    subkeyword = "INITIAL_REGION"

    def __init__(self, **kwargs):
        """Initialize the DualceseInitialRegion class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEINITIALREGION_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _DUALCESEINITIALREGION_CARD1,
                **kwargs,
            ),
        ]
    @property
    def regid(self) -> typing.Optional[int]:
        """Get or set the Region ID (see *DEFINE_REGION).
        """ # nopep8
        return self._cards[0].get_value("regid")

    @regid.setter
    def regid(self, value: int) -> None:
        """Set the regid property."""
        self._cards[0].set_value("regid", value)

    @property
    def ifunc(self) -> int:
        """Get or set the Flag to specify initial conditions using *DEFINE_FUNCTION:
        EQ.0:	Not in use.
        EQ.1 : All values for initial velocity, pressure, temperature, etc.now refer to* DEFINE_FUNCTION IDs.In these functions, the following parameters are allowed : f(x, y, z).Thus, each variable�s initial profile is a function of position.
        """ # nopep8
        return self._cards[0].get_value("ifunc")

    @ifunc.setter
    def ifunc(self, value: int) -> None:
        """Set the ifunc property."""
        if value not in [0, 1, None]:
            raise Exception("""ifunc must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ifunc", value)

    @property
    def u(self) -> typing.Optional[float]:
        """Get or set the x, y, and z components of velocity, respectively
        """ # nopep8
        return self._cards[1].get_value("u")

    @u.setter
    def u(self, value: float) -> None:
        """Set the u property."""
        self._cards[1].set_value("u", value)

    @property
    def v(self) -> typing.Optional[float]:
        """Get or set the x, y, and z components of velocity, respectively
        """ # nopep8
        return self._cards[1].get_value("v")

    @v.setter
    def v(self, value: float) -> None:
        """Set the v property."""
        self._cards[1].set_value("v", value)

    @property
    def w(self) -> typing.Optional[float]:
        """Get or set the x, y, and z components of velocity, respectively
        """ # nopep8
        return self._cards[1].get_value("w")

    @w.setter
    def w(self, value: float) -> None:
        """Set the w property."""
        self._cards[1].set_value("w", value)

    @property
    def rho(self) -> typing.Optional[float]:
        """Get or set the Density
        """ # nopep8
        return self._cards[1].get_value("rho")

    @rho.setter
    def rho(self, value: float) -> None:
        """Set the rho property."""
        self._cards[1].set_value("rho", value)

    @property
    def p(self) -> typing.Optional[float]:
        """Get or set the Pressure
        """ # nopep8
        return self._cards[1].get_value("p")

    @p.setter
    def p(self, value: float) -> None:
        """Set the p property."""
        self._cards[1].set_value("p", value)

    @property
    def t(self) -> typing.Optional[float]:
        """Get or set the Temperature
        """ # nopep8
        return self._cards[1].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        """Set the t property."""
        self._cards[1].set_value("t", value)

