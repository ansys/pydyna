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

class EmSolverBem(KeywordBase):
    """DYNA EM_SOLVER_BEM keyword"""

    keyword = "EM"
    subkeyword = "SOLVER_BEM"

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
                        kwargs.get("reltol", 1.e-6)
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
                        kwargs.get("stype", 2)
                    ),
                    Field(
                        "precon",
                        int,
                        30,
                        10,
                        kwargs.get("precon", 2)
                    ),
                    Field(
                        "uselast",
                        int,
                        40,
                        10,
                        kwargs.get("uselast", 1)
                    ),
                    Field(
                        "ncyclbem",
                        int,
                        50,
                        10,
                        kwargs.get("ncyclbem", 5000)
                    ),
                ],
            ),
        ]

    @property
    def reltol(self) -> float:
        """Get or set the Relative tolerance for the iterative solvers (PCG or GMRES). The user should try to decrease this tolerance if the results are not accurate enough. More iterations will then be needed.
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
        EQ 1: Direct solve - the matrices will then be dense.
        EQ 2: Pre-Conditioned Gradient method (PCG) - this allows to have block matrices with low rank blocks and thus to reduce the memory.
        EQ 3 : GMRES method - this allows to have block matrices with low rank blocks and thus to reduce the memory. The GMRES option only works in Serial for now.
        Note: the GMRES capability is not fully implemented yet, so we advise to use either the PCG method or the direct solve for now

        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        if value not in [2, 1, 3]:
            raise Exception("""stype must be one of {2,1,3}""")
        self._cards[0].set_value("stype", value)

    @property
    def precon(self) -> int:
        """Get or set the Preconditioner type for PCG or GMRES iterative solves:
        EQ 0: no preconditioner
        EQ 1: Diagonal line
        EQ 2: diagonal block
        EQ.3: broad diagonal including all neighbor faces
        EQ.4: LLT factorization. The LLT factorization option only works in Serial for now

        """ # nopep8
        return self._cards[0].get_value("precon")

    @precon.setter
    def precon(self, value: int) -> None:
        if value not in [2, 0, 1, 3, 4]:
            raise Exception("""precon must be one of {2,0,1,3,4}""")
        self._cards[0].set_value("precon", value)

    @property
    def uselast(self) -> int:
        """Get or set the This is used only for iterative solvers (PCG or GMRES).
        EQ.-1 : starts from 0 as initial solution of the linear system.
        EQ.1: starts from previous solution normalized by the rhs change. Note: using USELAST=1 can save many iterations in the further solves if the vector solution of the present solve is assumed to be nearly parallel to the vector solution of the previous solve, like it usually happens in time domain eddy-current problems

        """ # nopep8
        return self._cards[0].get_value("uselast")

    @uselast.setter
    def uselast(self, value: int) -> None:
        if value not in [1, -1]:
            raise Exception("""uselast must be one of {1,-1}""")
        self._cards[0].set_value("uselast", value)

    @property
    def ncyclbem(self) -> int:
        """Get or set the Number of electromagnetism cycles between the recalculation of BEM matrices.
        """ # nopep8
        return self._cards[0].get_value("ncyclbem")

    @ncyclbem.setter
    def ncyclbem(self, value: int) -> None:
        self._cards[0].set_value("ncyclbem", value)

