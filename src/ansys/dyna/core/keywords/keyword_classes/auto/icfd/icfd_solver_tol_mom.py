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

"""Module providing the IcfdSolverTolMom class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDSOLVERTOLMOM_CARD0 = (
    FieldSchema("atol", float, 0, 10, 1e-08),
    FieldSchema("rtol", float, 10, 10, 1e-08),
    FieldSchema("unused", int, 20, 10, None),
    FieldSchema("maxit", int, 30, 10, 1000),
)

class IcfdSolverTolMom(KeywordBase):
    """DYNA ICFD_SOLVER_TOL_MOM keyword"""

    keyword = "ICFD"
    subkeyword = "SOLVER_TOL_MOM"

    def __init__(self, **kwargs):
        """Initialize the IcfdSolverTolMom class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDSOLVERTOLMOM_CARD0,
                **kwargs,
            ),        ]
    @property
    def atol(self) -> float:
        """Get or set the Absolute convergence criteria. Convergence is achieved when Residual𝑖+1 −Residual𝑖 ≤ ATOL. If a negative integer is entered,then that value will be used as a load curve ID for ATOL.
        """ # nopep8
        return self._cards[0].get_value("atol")

    @atol.setter
    def atol(self, value: float) -> None:
        """Set the atol property."""
        self._cards[0].set_value("atol", value)

    @property
    def rtol(self) -> float:
        """Get or set the Relative convergence criteria. Convergence is achieved when (Residual𝑖+1 − Residual𝑖)⁄Residualinitial ≤ RTOL. If a negative integer is entered, then that value will be used as a load curve ID for RTOL.
        """ # nopep8
        return self._cards[0].get_value("rtol")

    @rtol.setter
    def rtol(self, value: float) -> None:
        """Set the rtol property."""
        self._cards[0].set_value("rtol", value)

    @property
    def maxit(self) -> int:
        """Get or set the Maximum number of iterations allowed to achieve convergence. If a negative integer is entered, then that value will be used as a load curve ID for MAXIT.
        """ # nopep8
        return self._cards[0].get_value("maxit")

    @maxit.setter
    def maxit(self, value: int) -> None:
        """Set the maxit property."""
        self._cards[0].set_value("maxit", value)

