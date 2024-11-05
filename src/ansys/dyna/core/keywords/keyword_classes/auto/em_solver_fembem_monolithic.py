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

import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class EmSolverFembemMonolithic(KeywordBase):
    """DYNA EM_SOLVER_FEMBEM_MONOLITHIC keyword"""

    keyword = "EM"
    subkeyword = "SOLVER_FEMBEM_MONOLITHIC"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "mtype",
                        int,
                        0,
                        10,
                        kwargs.get("mtype", 0)
                    ),
                    Field(
                        "stype",
                        int,
                        10,
                        10,
                        kwargs.get("stype", 0)
                    ),
                    Field(
                        "abstol",
                        float,
                        20,
                        10,
                        kwargs.get("abstol", 1.0E-6)
                    ),
                    Field(
                        "reltol",
                        float,
                        30,
                        10,
                        kwargs.get("reltol", 1.0E-4)
                    ),
                    Field(
                        "maxit",
                        int,
                        40,
                        10,
                        kwargs.get("maxit", 500)
                    ),
                ],
            ),
        ]

    @property
    def mtype(self) -> int:
        """Get or set the Monolithic solver type (See Remark 1):
        EQ.0:	Direct symmetric solver.
        """ # nopep8
        return self._cards[0].get_value("mtype")

    @mtype.setter
    def mtype(self, value: int) -> None:
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
        if value not in [0, 1]:
            raise Exception("""stype must be one of {0,1}""")
        self._cards[0].set_value("stype", value)

    @property
    def abstol(self) -> float:
        """Get or set the Absolute tolerance
        """ # nopep8
        return self._cards[0].get_value("abstol")

    @abstol.setter
    def abstol(self, value: float) -> None:
        self._cards[0].set_value("abstol", value)

    @property
    def reltol(self) -> float:
        """Get or set the Relative tolerance
        """ # nopep8
        return self._cards[0].get_value("reltol")

    @reltol.setter
    def reltol(self, value: float) -> None:
        self._cards[0].set_value("reltol", value)

    @property
    def maxit(self) -> int:
        """Get or set the Maximum number of iterations
        """ # nopep8
        return self._cards[0].get_value("maxit")

    @maxit.setter
    def maxit(self, value: int) -> None:
        self._cards[0].set_value("maxit", value)

