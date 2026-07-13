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

"""Module providing the EmSolverFembemMonolithic class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EMSOLVERFEMBEMMONOLITHIC_CARD0 = (
    FieldSchema("mtype", int, 0, 10, 0),
    FieldSchema("stype", int, 10, 10, 0),
    FieldSchema("abstol", float, 20, 10, 1e-06),
    FieldSchema("reltol", float, 30, 10, 0.0001),
    FieldSchema("maxit", int, 40, 10, 500),
)

_EMSOLVERFEMBEMMONOLITHIC_CARD1 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("ntstol", float, 0, 10, 1e-05),
    FieldSchema("ntatol", float, 20, 10, None),
    FieldSchema("ntrtol", float, 30, 10, 0.0001),
    FieldSchema("ntmit", int, 40, 10, 20),
)

_EMSOLVERFEMBEMMONOLITHIC_CARD2 = (
    FieldSchema("ls_on", int, 0, 10, 0),
)

class EmSolverFembemMonolithic(KeywordBase):
    """DYNA EM_SOLVER_FEMBEM_MONOLITHIC keyword"""

    keyword = "EM"
    subkeyword = "SOLVER_FEMBEM_MONOLITHIC"

    def __init__(self, **kwargs):
        """Initialize the EmSolverFembemMonolithic class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMSOLVERFEMBEMMONOLITHIC_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EMSOLVERFEMBEMMONOLITHIC_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EMSOLVERFEMBEMMONOLITHIC_CARD2,
                **kwargs,
            ),
        ]
    @property
    def mtype(self) -> int:
        """Get or set the Monolithic solver type (See Remark 1):
        EQ.0: Direct symmetric solver.
        """ # nopep8
        return self._cards[0].get_value("mtype")

    @mtype.setter
    def mtype(self, value: int) -> None:
        """Set the mtype property."""
        self._cards[0].set_value("mtype", value)

    @property
    def stype(self) -> int:
        """Get or set the Solver type:
        EQ.0: MINRES iterative solver.
        EQ.1: GMRES iterative solver.
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        """Set the stype property."""
        if value not in [0, 1, None]:
            raise Exception("""stype must be `None` or one of {0,1}.""")
        self._cards[0].set_value("stype", value)

    @property
    def abstol(self) -> float:
        """Get or set the Absolute tolerance
        """ # nopep8
        return self._cards[0].get_value("abstol")

    @abstol.setter
    def abstol(self, value: float) -> None:
        """Set the abstol property."""
        self._cards[0].set_value("abstol", value)

    @property
    def reltol(self) -> float:
        """Get or set the Relative tolerance
        """ # nopep8
        return self._cards[0].get_value("reltol")

    @reltol.setter
    def reltol(self, value: float) -> None:
        """Set the reltol property."""
        self._cards[0].set_value("reltol", value)

    @property
    def maxit(self) -> int:
        """Get or set the Maximum number of iterations
        """ # nopep8
        return self._cards[0].get_value("maxit")

    @maxit.setter
    def maxit(self, value: int) -> None:
        """Set the maxit property."""
        self._cards[0].set_value("maxit", value)

    @property
    def ntstol(self) -> float:
        """Get or set the Solution change tolerance, absolute tolerance, and relative tolerance used during the nonlinear Newton step. They are only used when the EM model includes nonlinear magnetic materials).
        """ # nopep8
        return self._cards[1].get_value("ntstol")

    @ntstol.setter
    def ntstol(self, value: float) -> None:
        """Set the ntstol property."""
        self._cards[1].set_value("ntstol", value)

    @property
    def ntatol(self) -> typing.Optional[float]:
        """Get or set the Solution change tolerance, absolute tolerance, and relative tolerance used during the nonlinear Newton step. They are only used when the EM model includes nonlinear magnetic materials).
        """ # nopep8
        return self._cards[1].get_value("ntatol")

    @ntatol.setter
    def ntatol(self, value: float) -> None:
        """Set the ntatol property."""
        self._cards[1].set_value("ntatol", value)

    @property
    def ntrtol(self) -> float:
        """Get or set the Solution change tolerance, absolute tolerance, and relative tolerance used during the nonlinear Newton step. They are only used when the EM model includes nonlinear magnetic materials).
        """ # nopep8
        return self._cards[1].get_value("ntrtol")

    @ntrtol.setter
    def ntrtol(self, value: float) -> None:
        """Set the ntrtol property."""
        self._cards[1].set_value("ntrtol", value)

    @property
    def ntmit(self) -> int:
        """Get or set the Maximum nonlinear iterations
        """ # nopep8
        return self._cards[1].get_value("ntmit")

    @ntmit.setter
    def ntmit(self, value: int) -> None:
        """Set the ntmit property."""
        self._cards[1].set_value("ntmit", value)

    @property
    def ls_on(self) -> int:
        """Get or set the Line search:
        EQ.0: Off
        EQ.1: On(only used for nonlinear magnetic materials)
        """ # nopep8
        return self._cards[2].get_value("ls_on")

    @ls_on.setter
    def ls_on(self, value: int) -> None:
        """Set the ls_on property."""
        if value not in [0, 1, None]:
            raise Exception("""ls_on must be `None` or one of {0,1}.""")
        self._cards[2].set_value("ls_on", value)

