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

class CeseControlSolver(KeywordBase):
    """DYNA CESE_CONTROL_SOLVER keyword"""

    keyword = "CESE"
    subkeyword = "CONTROL_SOLVER"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "icese",
                        int,
                        0,
                        10,
                        kwargs.get("icese", 0)
                    ),
                    Field(
                        "iflow",
                        int,
                        10,
                        10,
                        kwargs.get("iflow", 0)
                    ),
                    Field(
                        "igeom",
                        int,
                        20,
                        10,
                        kwargs.get("igeom", 0)
                    ),
                    Field(
                        "iframe",
                        int,
                        30,
                        10,
                        kwargs.get("iframe", 0)
                    ),
                ],
            ),
        ]

    @property
    def icese(self) -> int:
        """Get or set the Sets the framework of the CESE solver:   EQ.0: Fixed Eulerian
        EQ. 100: Moving Mesh FSI
        EQ. 200: Immersed boundary FSI.
        """ # nopep8
        return self._cards[0].get_value("icese")

    @icese.setter
    def icese(self, value: int) -> None:
        if value not in [0, 100, 200]:
            raise Exception("""icese must be one of {0,100,200}""")
        self._cards[0].set_value("icese", value)

    @property
    def iflow(self) -> int:
        """Get or set the Sets the compressible flow types:
        EQ.0: Viscous flows (laminar)
        EQ.1: Inviscid flows
        .
        """ # nopep8
        return self._cards[0].get_value("iflow")

    @iflow.setter
    def iflow(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""iflow must be one of {0,1}""")
        self._cards[0].set_value("iflow", value)

    @property
    def igeom(self) -> int:
        """Get or set the Set the geometric dimension:
        EQ.0:   2D or 3D, it will be decided by the mesh & and the given boundary conditions.
        EQ.2:   two dimension (2D) problem
        EQ.3:   three dimension (3D) problem
        EQ.101  2D axis-symmetric
        .
        """ # nopep8
        return self._cards[0].get_value("igeom")

    @igeom.setter
    def igeom(self, value: int) -> None:
        if value not in [0, 2, 3, 101]:
            raise Exception("""igeom must be one of {0,2,3,101}""")
        self._cards[0].set_value("igeom", value)

    @property
    def iframe(self) -> int:
        """Get or set the Choose the frame of reference:
        EQ.0: Usual non-moving reference frame (default)
        EQ.1000: Non-inertial rotating reference frame.
        """ # nopep8
        return self._cards[0].get_value("iframe")

    @iframe.setter
    def iframe(self, value: int) -> None:
        if value not in [0, 1000]:
            raise Exception("""iframe must be one of {0,1000}""")
        self._cards[0].set_value("iframe", value)

