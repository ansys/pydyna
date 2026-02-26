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

"""Module providing the IcfdSolverSplit class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDSOLVERSPLIT_CARD0 = (
    FieldSchema("nit", int, 0, 10, 1),
    FieldSchema("tol", float, 10, 10, 0.001),
)

class IcfdSolverSplit(KeywordBase):
    """DYNA ICFD_SOLVER_SPLIT keyword"""

    keyword = "ICFD"
    subkeyword = "SOLVER_SPLIT"

    def __init__(self, **kwargs):
        """Initialize the IcfdSolverSplit class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDSOLVERSPLIT_CARD0,
                **kwargs,
            ),        ]
    @property
    def nit(self) -> int:
        """Get or set the Maximum Number of iterations of the system for each fluid time step. If TOL criteria is not reached after NIT iterations, the run will proceed.
        """ # nopep8
        return self._cards[0].get_value("nit")

    @nit.setter
    def nit(self, value: int) -> None:
        """Set the nit property."""
        self._cards[0].set_value("nit", value)

    @property
    def tol(self) -> float:
        """Get or set the Tolerance Criteria for the pressure residual during the fluid system solve.
        """ # nopep8
        return self._cards[0].get_value("tol")

    @tol.setter
    def tol(self, value: float) -> None:
        """Set the tol property."""
        self._cards[0].set_value("tol", value)

