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

"""Module providing the ControlPzelectric class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLPZELECTRIC_CARD0 = (
    FieldSchema("solver", int, 0, 10, 11),
    FieldSchema("msgitr", int, 10, 10, 0),
    FieldSchema("maxitr", int, 20, 10, 500),
    FieldSchema("abstol", float, 30, 10, 1e-20),
    FieldSchema("reltol", int, 40, 10, 0),
    FieldSchema("ndtrfk", int, 50, 10, 1),
    FieldSchema("epzmsg", int, 60, 10, 0),
)

class ControlPzelectric(KeywordBase):
    """DYNA CONTROL_PZELECTRIC keyword"""

    keyword = "CONTROL"
    subkeyword = "PZELECTRIC"

    def __init__(self, **kwargs):
        """Initialize the ControlPzelectric class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLPZELECTRIC_CARD0,
                **kwargs,
            ),        ]
    @property
    def solver(self) -> int:
        """Get or set the Piezoelectric solver type:
        EQ.11:	Direct solver
        EQ.12 : Diagonal scaling conjugate gradient iterative, recommended for MPP for better scalability
        """ # nopep8
        return self._cards[0].get_value("solver")

    @solver.setter
    def solver(self, value: int) -> None:
        """Set the solver property."""
        if value not in [11, 12, None]:
            raise Exception("""solver must be `None` or one of {11,12}.""")
        self._cards[0].set_value("solver", value)

    @property
    def msgitr(self) -> int:
        """Get or set the Output iteration message level for SOLVER = 12:
        EQ.0:	No output(default)
        EQ.1 : Summary information
        """ # nopep8
        return self._cards[0].get_value("msgitr")

    @msgitr.setter
    def msgitr(self, value: int) -> None:
        """Set the msgitr property."""
        if value not in [0, 1, None]:
            raise Exception("""msgitr must be `None` or one of {0,1}.""")
        self._cards[0].set_value("msgitr", value)

    @property
    def maxitr(self) -> int:
        """Get or set the Maximum number of iterations for SOLVER = 12.
        EQ.0:	Use default value 100.
        """ # nopep8
        return self._cards[0].get_value("maxitr")

    @maxitr.setter
    def maxitr(self, value: int) -> None:
        """Set the maxitr property."""
        self._cards[0].set_value("maxitr", value)

    @property
    def abstol(self) -> float:
        """Get or set the Absolute convergence tolerance, for SOLVER =12.
        EQ.0.0:	Use default value 10 - 20.
        """ # nopep8
        return self._cards[0].get_value("abstol")

    @abstol.setter
    def abstol(self, value: float) -> None:
        """Set the abstol property."""
        self._cards[0].set_value("abstol", value)

    @property
    def reltol(self) -> int:
        """Get or set the Relative convergence tolerance, for SOLVER = 12.
        EQ.0.0:	Use default value 10 - 10.
        """ # nopep8
        return self._cards[0].get_value("reltol")

    @reltol.setter
    def reltol(self, value: int) -> None:
        """Set the reltol property."""
        self._cards[0].set_value("reltol", value)

    @property
    def ndtrfk(self) -> int:
        """Get or set the Reform the dielectric stiffness matrix for every NDTRFK time steps.
        LT.0:	Curve |NDTRFK | defines the stiffness reformation time step as a function of time.
        """ # nopep8
        return self._cards[0].get_value("ndtrfk")

    @ndtrfk.setter
    def ndtrfk(self, value: int) -> None:
        """Set the ndtrfk property."""
        self._cards[0].set_value("ndtrfk", value)

    @property
    def epzmsg(self) -> int:
        """Get or set the Flag to determine if electric flux and electric field at the element center of piezoelectric material is output to d3plot:
        EQ.0:	No electric flux or electric field output to d3plot
        EQ.1 : x, y,and z strain slots in d3plot store the electric flux along the x, y,and z directions, respectively.xy, yz,and zx strain slots in d3plot store the electric field along the x, y,and z directions, respectively
        """ # nopep8
        return self._cards[0].get_value("epzmsg")

    @epzmsg.setter
    def epzmsg(self, value: int) -> None:
        """Set the epzmsg property."""
        self._cards[0].set_value("epzmsg", value)

