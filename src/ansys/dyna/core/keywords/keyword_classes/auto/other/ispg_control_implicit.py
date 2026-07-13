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

"""Module providing the IspgControlImplicit class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ISPGCONTROLIMPLICIT_CARD0 = (
    FieldSchema("beta", float, 0, 10, 0.38),
    FieldSchema("gamma", float, 10, 10, 0.6),
    FieldSchema("cfl", float, 20, 10, 1.0),
    FieldSchema("mx_subs", int, 30, 10, 512),
    FieldSchema("mx_iters", int, 40, 10, 20),
)

_ISPGCONTROLIMPLICIT_CARD1 = (
    FieldSchema("birth", float, 0, 10, 0.0),
    FieldSchema("dtmp", float, 10, 10, 0.0),
)

class IspgControlImplicit(KeywordBase):
    """DYNA ISPG_CONTROL_IMPLICIT keyword"""

    keyword = "ISPG"
    subkeyword = "CONTROL_IMPLICIT"

    def __init__(self, **kwargs):
        """Initialize the IspgControlImplicit class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ISPGCONTROLIMPLICIT_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ISPGCONTROLIMPLICIT_CARD1,
                **kwargs,
            ),
        ]
    @property
    def beta(self) -> float:
        """Get or set the b used by the Newton-Raphson iteration algorithm
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[0].set_value("beta", value)

    @property
    def gamma(self) -> float:
        """Get or set the r used by the Newton-Raphson iteration algorithm
        """ # nopep8
        return self._cards[0].get_value("gamma")

    @gamma.setter
    def gamma(self, value: float) -> None:
        """Set the gamma property."""
        self._cards[0].set_value("gamma", value)

    @property
    def cfl(self) -> float:
        """Get or set the Courant number
        """ # nopep8
        return self._cards[0].get_value("cfl")

    @cfl.setter
    def cfl(self, value: float) -> None:
        """Set the cfl property."""
        self._cards[0].set_value("cfl", value)

    @property
    def mx_subs(self) -> int:
        """Get or set the Maximum subcycles of the ISPG solver for each structural time step
        """ # nopep8
        return self._cards[0].get_value("mx_subs")

    @mx_subs.setter
    def mx_subs(self, value: int) -> None:
        """Set the mx_subs property."""
        self._cards[0].set_value("mx_subs", value)

    @property
    def mx_iters(self) -> int:
        """Get or set the Maximum number of iterations for the ISPG implicit method in each sub-step
        """ # nopep8
        return self._cards[0].get_value("mx_iters")

    @mx_iters.setter
    def mx_iters(self, value: int) -> None:
        """Set the mx_iters property."""
        self._cards[0].set_value("mx_iters", value)

    @property
    def birth(self) -> float:
        """Get or set the Starting time for the fully implicit ISPG iteration. Before BIRTH, only one-way coupling occurs, meaning no forces from the solder return to the structure. The implicit time step for ISPG parts is DTIMP instead of the structural implicit step to guarantee that the fluid moves with the solid boundaries. After BIRTH, two-way coupling occurs, and the ISPG solver performs a full iteration with the structural implicit step. This option is very useful for cases where the structural simulation time is very long (e.g., in seconds or minutes) while the reflow process to steady state is very short.
        """ # nopep8
        return self._cards[1].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        """Set the birth property."""
        self._cards[1].set_value("birth", value)

    @property
    def dtmp(self) -> float:
        """Get or set the Implicit time step for ISPG parts during the simulation time before BIRTH.
        """ # nopep8
        return self._cards[1].get_value("dtmp")

    @dtmp.setter
    def dtmp(self, value: float) -> None:
        """Set the dtmp property."""
        self._cards[1].set_value("dtmp", value)

