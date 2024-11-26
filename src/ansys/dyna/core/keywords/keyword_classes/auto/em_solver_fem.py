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

class EmSolverFem(KeywordBase):
    """DYNA EM_SOLVER_FEM keyword"""

    keyword = "EM"
    subkeyword = "SOLVER_FEM"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "reltol",
                        float,
                        0,
                        10,
                        kwargs.get("reltol", 1e-3)
                    ),
                    Field(
                        "maxite",
                        int,
                        10,
                        10,
                        kwargs.get("maxite", 1000)
                    ),
                    Field(
                        "stype",
                        int,
                        20,
                        10,
                        kwargs.get("stype", 1)
                    ),
                    Field(
                        "precon",
                        int,
                        30,
                        10,
                        kwargs.get("precon", 1)
                    ),
                    Field(
                        "uselast",
                        int,
                        40,
                        10,
                        kwargs.get("uselast", 1)
                    ),
                    Field(
                        "ncyclfem",
                        int,
                        50,
                        10,
                        kwargs.get("ncyclfem", 5000)
                    ),
                ],
            ),
        ]

    @property
    def reltol(self) -> float:
        """Get or set the Relative tolerance for the solver. The user should try to decrease this tolerance if the results are not accurate enough. More iterations will then be needed.
        """ # nopep8
        return self._cards[0].get_value("reltol")

    @reltol.setter
    def reltol(self, value: float) -> None:
        self._cards[0].set_value("reltol", value)

    @property
    def maxite(self) -> int:
        """Get or set the Maximal number of iterations.
        """ # nopep8
        return self._cards[0].get_value("maxite")

    @maxite.setter
    def maxite(self, value: int) -> None:
        self._cards[0].set_value("maxite", value)

    @property
    def stype(self) -> int:
        """Get or set the Solver type:
        EQ.1: Direct solve
        EQ.2: Conditioned Gradient Method (PCG)
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""stype must be one of {1,2}""")
        self._cards[0].set_value("stype", value)

    @property
    def precon(self) -> int:
        """Get or set the Preconditioner type for PCG.
        EQ.0: no preconditioner
        EQ.1: Diagonal line

        """ # nopep8
        return self._cards[0].get_value("precon")

    @precon.setter
    def precon(self, value: int) -> None:
        if value not in [1, 0]:
            raise Exception("""precon must be one of {1,0}""")
        self._cards[0].set_value("precon", value)

    @property
    def uselast(self) -> int:
        """Get or set the This is used only for iterative solvers (PCG).
        EQ.-1 : starts from 0 as initial solution of the linear system.
        EQ.1: starts from previous solution normalized by the rhs change.

        """ # nopep8
        return self._cards[0].get_value("uselast")

    @uselast.setter
    def uselast(self, value: int) -> None:
        if value not in [1, -1]:
            raise Exception("""uselast must be one of {1,-1}""")
        self._cards[0].set_value("uselast", value)

    @property
    def ncyclfem(self) -> int:
        """Get or set the Number of electromagnetism cycles between the recalculation of FEM matrices.
        """ # nopep8
        return self._cards[0].get_value("ncyclfem")

    @ncyclfem.setter
    def ncyclfem(self, value: int) -> None:
        self._cards[0].set_value("ncyclfem", value)

