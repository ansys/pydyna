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

class IcfdControlFsi(KeywordBase):
    """DYNA ICFD_CONTROL_FSI keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_FSI"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "owc",
                        int,
                        0,
                        10,
                        kwargs.get("owc", 0)
                    ),
                    Field(
                        "bt",
                        float,
                        10,
                        10,
                        kwargs.get("bt", 0.0)
                    ),
                    Field(
                        "dt",
                        float,
                        20,
                        10,
                        kwargs.get("dt", 1.0E28)
                    ),
                    Field(
                        "idc",
                        float,
                        30,
                        10,
                        kwargs.get("idc", 0.25)
                    ),
                    Field(
                        "lcidsf",
                        int,
                        40,
                        10,
                        kwargs.get("lcidsf")
                    ),
                    Field(
                        "xproj",
                        int,
                        50,
                        10,
                        kwargs.get("xproj", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nsub",
                        int,
                        0,
                        10,
                        kwargs.get("nsub")
                    ),
                ],
            ),
        ]

    @property
    def owc(self) -> int:
        """Get or set the Indicates the coupling direction to the solver.
        EQ.0:	Two - way coupling.Loads and displacements are transferred across the FSI interface and the full non - linear problem is solved.Weak FSI coupling when coupled to explicit mechanical solver, strong FSI coupling when coupled to implicit mechanical solver.
        EQ.1 : One - way coupling.The solid mechanics solver transfers displacements to the fluid solver.
        EQ.2 : One - way coupling.The fluid solver transfers stresses to the solid mechanics solver.
        EQ.3 : Two - way coupling.Forces weak coupling(no sub - stepping) with implicit mechanical solver.
        """ # nopep8
        return self._cards[0].get_value("owc")

    @owc.setter
    def owc(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""owc must be one of {0,1,2,3}""")
        self._cards[0].set_value("owc", value)

    @property
    def bt(self) -> float:
        """Get or set the Birth time for the FSI coupling. Before BT the fluid solver will not pass any loads to the structure but it will receive displacements from the solid solver.
        """ # nopep8
        return self._cards[0].get_value("bt")

    @bt.setter
    def bt(self, value: float) -> None:
        self._cards[0].set_value("bt", value)

    @property
    def dt(self) -> float:
        """Get or set the Death time for the FSI coupling. After DT the fluid solver will not trans# fer any loads to the solid solver but it will continue to deform with the solid.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[0].set_value("dt", value)

    @property
    def idc(self) -> float:
        """Get or set the Interaction detection coefficient.
        """ # nopep8
        return self._cards[0].get_value("idc")

    @idc.setter
    def idc(self, value: float) -> None:
        self._cards[0].set_value("idc", value)

    @property
    def lcidsf(self) -> typing.Optional[int]:
        """Get or set the Optional load curve ID to apply a scaling factor on the forces transferred to the solid :
        GT.0: Load curve ID function of iterations.
        LT.0: Load curve ID function of time.
        """ # nopep8
        return self._cards[0].get_value("lcidsf")

    @lcidsf.setter
    def lcidsf(self, value: int) -> None:
        self._cards[0].set_value("lcidsf", value)

    @property
    def xproj(self) -> int:
        """Get or set the Projection of the nodes of the CFD domain that are at the FSI interface onto the structural mesh.
        EQ.0:No projection
        EQ.1:Projection
        """ # nopep8
        return self._cards[0].get_value("xproj")

    @xproj.setter
    def xproj(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""xproj must be one of {0,1}""")
        self._cards[0].set_value("xproj", value)

    @property
    def nsub(self) -> typing.Optional[int]:
        """Get or set the Optional limit on the number of FSI fluid subiterations. This avoids the sometimes unneeded excessive number of FSI subiterations when the fluid and very light structures (like parachutes) develop a resonance-like mode inside the FSI subiterations (coupling iterations)
        """ # nopep8
        return self._cards[1].get_value("nsub")

    @nsub.setter
    def nsub(self, value: int) -> None:
        self._cards[1].set_value("nsub", value)

