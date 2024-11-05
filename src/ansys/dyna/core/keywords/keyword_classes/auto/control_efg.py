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

class ControlEfg(KeywordBase):
    """DYNA CONTROL_EFG keyword"""

    keyword = "CONTROL"
    subkeyword = "EFG"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "isplane",
                        int,
                        0,
                        10,
                        kwargs.get("isplane", 0)
                    ),
                    Field(
                        "idila",
                        int,
                        10,
                        10,
                        kwargs.get("idila", 0)
                    ),
                    Field(
                        "inint",
                        int,
                        20,
                        10,
                        kwargs.get("inint", 12)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "imlm",
                        int,
                        0,
                        10,
                        kwargs.get("imlm", 0)
                    ),
                    Field(
                        "etol",
                        float,
                        10,
                        10,
                        kwargs.get("etol", 1.e-4)
                    ),
                ],
            ),
        ]

    @property
    def isplane(self) -> int:
        """Get or set the Optional choice for the mesh-free kernal functions:
        EQ.0: Cubic spline function (default)
        EQ.1: Quadratic spline function
        """ # nopep8
        return self._cards[0].get_value("isplane")

    @isplane.setter
    def isplane(self, value: int) -> None:
        self._cards[0].set_value("isplane", value)

    @property
    def idila(self) -> int:
        """Get or set the Optional choice for the normalized dilation parameter:
        .EQ.0: use individual dilation parameter for each mesh-free particle (default)
        EQ.1: use the largest dilation parameter. The EFG processor will calculate the largest dilation parameter for each mesh-free part. Every particle associated with that mesh-free part will be assigned to the same value for the mesh-free computation.
        """ # nopep8
        return self._cards[0].get_value("idila")

    @idila.setter
    def idila(self, value: int) -> None:
        self._cards[0].set_value("idila", value)

    @property
    def inint(self) -> int:
        """Get or set the This is the factor needed for the estimation of maximum workspace (MWSPAC) that can be used during the initialization phase.
        """ # nopep8
        return self._cards[0].get_value("inint")

    @inint.setter
    def inint(self, value: int) -> None:
        self._cards[0].set_value("inint", value)

    @property
    def imlm(self) -> int:
        """Get or set the Optional choice for the matrix operation, linear solving and memory usage:
        EQ.1: Original BCSLIB-EXT solvers.
        EQ.2: EFGPACK
        """ # nopep8
        return self._cards[1].get_value("imlm")

    @imlm.setter
    def imlm(self, value: int) -> None:
        self._cards[1].set_value("imlm", value)

    @property
    def etol(self) -> float:
        """Get or set the Error tolerance in the IMLM. When IMLM=2 is used, ININT in card one becomes redundant. IMLM=2 is recommended.
        """ # nopep8
        return self._cards[1].get_value("etol")

    @etol.setter
    def etol(self, value: float) -> None:
        self._cards[1].set_value("etol", value)

