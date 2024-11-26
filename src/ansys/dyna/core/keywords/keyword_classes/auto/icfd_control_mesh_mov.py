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

class IcfdControlMeshMov(KeywordBase):
    """DYNA ICFD_CONTROL_MESH_MOV keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_MESH_MOV"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "mmsh",
                        int,
                        0,
                        10,
                        kwargs.get("mmsh", 2)
                    ),
                    Field(
                        "lim_iter",
                        int,
                        10,
                        10,
                        kwargs.get("lim_iter", 100)
                    ),
                    Field(
                        "reltol",
                        float,
                        20,
                        10,
                        kwargs.get("reltol", 1.0e-3)
                    ),
                ],
            ),
        ]

    @property
    def mmsh(self) -> int:
        """Get or set the Mesh motion selector:
        EQ.-1: Completely shuts off any mesh movement
        EQ. 1: mesh moves based on the distance to moving walls.
        EQ. 2: mesh moves by solving a linear elasticity problem using the element sizes as stiffness.
        EQ.3: mesh uses a Laplacian smoothing with stiffness on edges and from node to opposite faces. Very robust but costly.
        EQ 4: full lagrangian. The mesh moves with the velocity of the flow.
        EQ.11: mesh moves using an implicit ball-vertex spring method
        EQ.20 : mesh moves by solving a linear elasticity problem using a constant size. This can be useful to avoid large distortions in rotating problems that involve large discrepancies in mesh sizes (typically in cases involving boundary layer mesh).
        """ # nopep8
        return self._cards[0].get_value("mmsh")

    @mmsh.setter
    def mmsh(self, value: int) -> None:
        if value not in [2, -1, 1, 3, 4, 11, 20]:
            raise Exception("""mmsh must be one of {2,-1,1,3,4,11,20}""")
        self._cards[0].set_value("mmsh", value)

    @property
    def lim_iter(self) -> int:
        """Get or set the Maximum number of linear solver iterations for the ball-vertex linear system.
        """ # nopep8
        return self._cards[0].get_value("lim_iter")

    @lim_iter.setter
    def lim_iter(self, value: int) -> None:
        self._cards[0].set_value("lim_iter", value)

    @property
    def reltol(self) -> float:
        """Get or set the Relative tolerance to use as a stopping criterion for the ball-vertex method iterative linear solver (conjugate gradient solver with diagonal scaling preconditioner).
        """ # nopep8
        return self._cards[0].get_value("reltol")

    @reltol.setter
    def reltol(self, value: float) -> None:
        self._cards[0].set_value("reltol", value)

