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

class ControlImplicitResidualVector(KeywordBase):
    """DYNA CONTROL_IMPLICIT_RESIDUAL_VECTOR keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_RESIDUAL_VECTOR"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "iresvec",
                        int,
                        0,
                        10,
                        kwargs.get("iresvec", 0)
                    ),
                    Field(
                        "neig",
                        int,
                        10,
                        10,
                        kwargs.get("neig", 0)
                    ),
                    Field(
                        "iformat",
                        int,
                        20,
                        10,
                        kwargs.get("iformat", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "rv_filenam",
                        str,
                        0,
                        256,
                        kwargs.get("rv_filenam")
                    ),
                ],
            ),
        ]

    @property
    def iresvec(self) -> int:
        """Get or set the Residual vector control flag:
        EQ.0:	do not compute residual vectors.
        GT.0:	compute residual vectors.
        """ # nopep8
        return self._cards[0].get_value("iresvec")

    @iresvec.setter
    def iresvec(self, value: int) -> None:
        self._cards[0].set_value("iresvec", value)

    @property
    def neig(self) -> int:
        """Get or set the Number of eigenmodes to compute for the purpose of orthogonalizing the computed load:
        EQ.0:	read the eigenmodes from the file Eigen_‌Vectors which is the file used to for dumping eignevectors; see EVDUMP on *CONTROL_‌IMPLICIT_‌EIGENVALUE.
        GT.0:	compute NEIG eigenmodes.
        """ # nopep8
        return self._cards[0].get_value("neig")

    @neig.setter
    def neig(self, value: int) -> None:
        self._cards[0].set_value("neig", value)

    @property
    def iformat(self) -> int:
        """Get or set the Format for processing eigenmodes:
        If NEIG = 0
        if IRESVEC > 0:
        read from Eigen_Vectors when NEIG = 0:
        LT.0:	file is in binary format.
        GT.0:	file is in ASCII format.
        Note that if IRSEVEC > 0 and NEIG = 0, IFORMAT = 0 is not allowed.
        If NEIG > 0:
        EQ.0:	do not dump the computed eigenmodes.
        LT.0:	dump the computed eigenmodes in binary format.
        GT.0:	dump the computed eignemodes in ASCII format.
        """ # nopep8
        return self._cards[0].get_value("iformat")

    @iformat.setter
    def iformat(self, value: int) -> None:
        self._cards[0].set_value("iformat", value)

    @property
    def rv_filenam(self) -> typing.Optional[str]:
        """Get or set the ENIG!=0 Name of the file for dumping the eigenvector
        NEIG=0 Name of the file read to obtain the eigenvectors. See EVDUMP on *CONTROL_IMPLICIT_EIGENVALUE.
        """ # nopep8
        return self._cards[1].get_value("rv_filenam")

    @rv_filenam.setter
    def rv_filenam(self, value: str) -> None:
        self._cards[1].set_value("rv_filenam", value)

