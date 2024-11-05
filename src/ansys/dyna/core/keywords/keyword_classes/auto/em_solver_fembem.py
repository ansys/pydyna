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

class EmSolverFembem(KeywordBase):
    """DYNA EM_SOLVER_FEMBEM keyword"""

    keyword = "EM"
    subkeyword = "SOLVER_FEMBEM"

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
                        kwargs.get("reltol", 1.e-2)
                    ),
                    Field(
                        "maxite",
                        int,
                        10,
                        10,
                        kwargs.get("maxite", 50)
                    ),
                    Field(
                        "forcon",
                        int,
                        20,
                        10,
                        kwargs.get("forcon", 0)
                    ),
                ],
            ),
        ]

    @property
    def reltol(self) -> float:
        """Get or set the Relative tolerance for the FEM/BEM system solve. If the results are not accurate enough, try decreasing this tolerance. A smaller tolerance will, however, require more iterations.
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
    def forcon(self) -> int:
        """Get or set the Force Convergence :
        EQ.0: The code stops with an error if no convergence
        EQ.1: The code continues to the next time step even if the RELTOL convergence criteria has not been reached.
        """ # nopep8
        return self._cards[0].get_value("forcon")

    @forcon.setter
    def forcon(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""forcon must be one of {0,1}""")
        self._cards[0].set_value("forcon", value)

