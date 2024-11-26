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

class DualceseControlSolver(KeywordBase):
    """DYNA DUALCESE_CONTROL_SOLVER keyword"""

    keyword = "DUALCESE"
    subkeyword = "CONTROL_SOLVER"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eqns",
                        str,
                        0,
                        10,
                        kwargs.get("eqns", "EULER")
                    ),
                    Field(
                        "igeom",
                        str,
                        10,
                        10,
                        kwargs.get("igeom")
                    ),
                    Field(
                        "iframe",
                        str,
                        20,
                        10,
                        kwargs.get("iframe", "FIXED")
                    ),
                    Field(
                        "mixtype",
                        str,
                        30,
                        10,
                        kwargs.get("mixtype")
                    ),
                    Field(
                        "idc",
                        float,
                        40,
                        10,
                        kwargs.get("idc", 0.25)
                    ),
                    Field(
                        "isnan",
                        int,
                        50,
                        10,
                        kwargs.get("isnan", 0)
                    ),
                ],
            ),
        ]

    @property
    def eqns(self) -> str:
        """Get or set the Select the equations being solved with the dual CESE solver.
        EQ.NS:	Navier - Stokes equations
        EQ.EULER : Euler equations
        """ # nopep8
        return self._cards[0].get_value("eqns")

    @eqns.setter
    def eqns(self, value: str) -> None:
        self._cards[0].set_value("eqns", value)

    @property
    def igeom(self) -> typing.Optional[str]:
        """Get or set the Sets the geometric dimension:
        EQ.2D:	Two - dimensional(2D) problem
        EQ.3D : Three - dimensional(3D) problem
        EQ.AXI : 2D axisymmetric
        """ # nopep8
        return self._cards[0].get_value("igeom")

    @igeom.setter
    def igeom(self, value: str) -> None:
        self._cards[0].set_value("igeom", value)

    @property
    def iframe(self) -> str:
        """Get or set the Choose the frame of reference:
        EQ.FIXED:	Usual non - moving reference frame(default).
        EQ.ROT : Non - inertial rotating reference frame.
        EQ.ROTATING : Non - inertial rotating reference frame
        """ # nopep8
        return self._cards[0].get_value("iframe")

    @iframe.setter
    def iframe(self, value: str) -> None:
        if value not in ["FIXED", "ROT", "ROTATING"]:
            raise Exception("""iframe must be one of {"FIXED","ROT","ROTATING"}""")
        self._cards[0].set_value("iframe", value)

    @property
    def mixtype(self) -> typing.Optional[str]:
        """Get or set the Select the mix or multiphase model solver (if any):
        EQ.<blank>: No mix or multiphase model(default).
        EQ.HYBRID : Hybrid multiphase model solver.
        EQ.TWO - PHASE : Two - phase multiphase solver.
        """ # nopep8
        return self._cards[0].get_value("mixtype")

    @mixtype.setter
    def mixtype(self, value: str) -> None:
        self._cards[0].set_value("mixtype", value)

    @property
    def idc(self) -> float:
        """Get or set the Contact interaction detection coefficient (for FSI and conjugate heat transfer problems).
        """ # nopep8
        return self._cards[0].get_value("idc")

    @idc.setter
    def idc(self, value: float) -> None:
        self._cards[0].set_value("idc", value)

    @property
    def isnan(self) -> int:
        """Get or set the Flag to check for a NaN in the dual CESE solver solution arrays at the completion of each time step. This option can be useful for debugging purposes. There is a cost overhead when this option is active.
        EQ.0: No checking,
        EQ.1 : Checking is active
        """ # nopep8
        return self._cards[0].get_value("isnan")

    @isnan.setter
    def isnan(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""isnan must be one of {0,1}""")
        self._cards[0].set_value("isnan", value)

