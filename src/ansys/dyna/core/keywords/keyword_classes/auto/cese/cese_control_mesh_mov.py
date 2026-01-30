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

"""Module providing the CeseControlMeshMov class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CESECONTROLMESHMOV_CARD0 = (
    FieldSchema("mmsh", int, 0, 10, 1),
    FieldSchema("lim_iter", int, 10, 10, 100),
    FieldSchema("reltol", float, 20, 10, 0.001),
    FieldSchema("abstol", float, 30, 10, 0.001),
)

class CeseControlMeshMov(KeywordBase):
    """DYNA CESE_CONTROL_MESH_MOV keyword"""

    keyword = "CESE"
    subkeyword = "CONTROL_MESH_MOV"

    def __init__(self, **kwargs):
        """Initialize the CeseControlMeshMov class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CESECONTROLMESHMOV_CARD0,
                **kwargs,
            ),        ]
    @property
    def mmsh(self) -> int:
        """Get or set the Mesh motion selector:
        EQ.1: mesh moves using an implicit ball-vertex spring method.
        EQ.9: the IDW scheme is used to move the mesh.
        """ # nopep8
        return self._cards[0].get_value("mmsh")

    @mmsh.setter
    def mmsh(self, value: int) -> None:
        """Set the mmsh property."""
        if value not in [1, 9, None]:
            raise Exception("""mmsh must be `None` or one of {1,9}.""")
        self._cards[0].set_value("mmsh", value)

    @property
    def lim_iter(self) -> int:
        """Get or set the Maximum number of linear solver iterations for the ball-vertex linear system.
        """ # nopep8
        return self._cards[0].get_value("lim_iter")

    @lim_iter.setter
    def lim_iter(self, value: int) -> None:
        """Set the lim_iter property."""
        self._cards[0].set_value("lim_iter", value)

    @property
    def reltol(self) -> float:
        """Get or set the Relative tolerance to use as a stopping criterion for the iterative linear solver (conjugate gradient solver with diagonal scaling preconditioner).
        """ # nopep8
        return self._cards[0].get_value("reltol")

    @reltol.setter
    def reltol(self, value: float) -> None:
        """Set the reltol property."""
        self._cards[0].set_value("reltol", value)

    @property
    def abstol(self) -> float:
        """Get or set the Absolute tolerance measure for the size of mesh displacement changes to use as a stopping criterion for the iterative linear solver.
        """ # nopep8
        return self._cards[0].get_value("abstol")

    @abstol.setter
    def abstol(self, value: float) -> None:
        """Set the abstol property."""
        self._cards[0].set_value("abstol", value)

