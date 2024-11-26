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

class DualceseControlMeshMov(KeywordBase):
    """DYNA DUALCESE_CONTROL_MESH_MOV keyword"""

    keyword = "DUALCESE"
    subkeyword = "CONTROL_MESH_MOV"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id")
                    ),
                    Field(
                        "ialg",
                        int,
                        10,
                        10,
                        kwargs.get("ialg", 9)
                    ),
                    Field(
                        "ninter",
                        int,
                        20,
                        10,
                        kwargs.get("ninter", 100)
                    ),
                    Field(
                        "relerr",
                        float,
                        30,
                        10,
                        kwargs.get("relerr", 1.0E-3)
                    ),
                    Field(
                        "mxdispr",
                        float,
                        40,
                        10,
                        kwargs.get("mxdispr", 1.0E-2)
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the ID for this mesh motion algorithm
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def ialg(self) -> int:
        """Get or set the Mesh motion selector:.
        EQ.9 (default) : the IDW scheme
        """ # nopep8
        return self._cards[0].get_value("ialg")

    @ialg.setter
    def ialg(self, value: int) -> None:
        self._cards[0].set_value("ialg", value)

    @property
    def ninter(self) -> int:
        """Get or set the Number of linear solver iterations (when using a linear solver specified in IALG). No linear solvers have been implemented at this time, so this field is ignored
        """ # nopep8
        return self._cards[0].get_value("ninter")

    @ninter.setter
    def ninter(self, value: int) -> None:
        self._cards[0].set_value("ninter", value)

    @property
    def relerr(self) -> float:
        """Get or set the Relative error for determining convergence when using a linear solver specified in IALG. No linear solvers have been implemented at this time, so this field is ignored
        """ # nopep8
        return self._cards[0].get_value("relerr")

    @relerr.setter
    def relerr(self, value: float) -> None:
        self._cards[0].set_value("relerr", value)

    @property
    def mxdispr(self) -> float:
        """Get or set the Maximum displacement relative to element size to use as a criterion for avoiding the full calculation of the motion of the DUALCESE part on a given time step. If the full calculation can be avoided, the elements touching an FSI interface are still morphed, but it is assumed that this approximation will not lead to elements that are overly distorted.
        """ # nopep8
        return self._cards[0].get_value("mxdispr")

    @mxdispr.setter
    def mxdispr(self, value: float) -> None:
        self._cards[0].set_value("mxdispr", value)

