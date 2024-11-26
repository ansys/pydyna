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

class IcfdControlOutputVar(KeywordBase):
    """DYNA ICFD_CONTROL_OUTPUT_VAR keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_OUTPUT_VAR"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "vel",
                        int,
                        0,
                        10,
                        kwargs.get("vel", 0)
                    ),
                    Field(
                        "avgvel",
                        int,
                        10,
                        10,
                        kwargs.get("avgvel", 0)
                    ),
                    Field(
                        "vort",
                        int,
                        20,
                        10,
                        kwargs.get("vort", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pre",
                        int,
                        0,
                        10,
                        kwargs.get("pre", 0)
                    ),
                    Field(
                        "preavg",
                        int,
                        10,
                        10,
                        kwargs.get("preavg", 0)
                    ),
                    Field(
                        "lset",
                        int,
                        20,
                        10,
                        kwargs.get("lset", 0)
                    ),
                    Field(
                        "oc",
                        int,
                        30,
                        10,
                        kwargs.get("oc", 0)
                    ),
                    Field(
                        "cfl",
                        int,
                        40,
                        10,
                        kwargs.get("cfl", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "temp",
                        int,
                        0,
                        10,
                        kwargs.get("temp", 0)
                    ),
                    Field(
                        "tempavg",
                        int,
                        10,
                        10,
                        kwargs.get("tempavg", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "kp",
                        int,
                        0,
                        10,
                        kwargs.get("kp", 0)
                    ),
                    Field(
                        "ep",
                        int,
                        10,
                        10,
                        kwargs.get("ep", 0)
                    ),
                    Field(
                        "mut",
                        int,
                        20,
                        10,
                        kwargs.get("mut", 0)
                    ),
                    Field(
                        "int",
                        int,
                        30,
                        10,
                        kwargs.get("int", 0)
                    ),
                    Field(
                        "cmu",
                        int,
                        40,
                        10,
                        kwargs.get("cmu", 0)
                    ),
                ],
            ),
        ]

    @property
    def vel(self) -> int:
        """Get or set the Velocity :
        EQ.0:	Is output.
        EQ.1:	Is not output.
        """ # nopep8
        return self._cards[0].get_value("vel")

    @vel.setter
    def vel(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""vel must be one of {0,1}""")
        self._cards[0].set_value("vel", value)

    @property
    def avgvel(self) -> int:
        """Get or set the average velocity :
        EQ.0:	Is output.
        EQ.1:	Is not output.
        """ # nopep8
        return self._cards[0].get_value("avgvel")

    @avgvel.setter
    def avgvel(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""avgvel must be one of {0,1}""")
        self._cards[0].set_value("avgvel", value)

    @property
    def vort(self) -> int:
        """Get or set the vorticity :
        EQ.0:	Is output.
        EQ.1:	Is not output.
        """ # nopep8
        return self._cards[0].get_value("vort")

    @vort.setter
    def vort(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""vort must be one of {0,1}""")
        self._cards[0].set_value("vort", value)

    @property
    def pre(self) -> int:
        """Get or set the pressure:
        EQ.0:	Is output.
        EQ.1:	Is not output
        """ # nopep8
        return self._cards[1].get_value("pre")

    @pre.setter
    def pre(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""pre must be one of {0,1}""")
        self._cards[1].set_value("pre", value)

    @property
    def preavg(self) -> int:
        """Get or set the average pressure, levelset, Q criterion, CFL number :
        EQ.0:	Is output.
        EQ.1:	Is not output
        """ # nopep8
        return self._cards[1].get_value("preavg")

    @preavg.setter
    def preavg(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""preavg must be one of {0,1}""")
        self._cards[1].set_value("preavg", value)

    @property
    def lset(self) -> int:
        """Get or set the levelset :
        EQ.0:	Is output.
        EQ.1:	Is not output
        """ # nopep8
        return self._cards[1].get_value("lset")

    @lset.setter
    def lset(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""lset must be one of {0,1}""")
        self._cards[1].set_value("lset", value)

    @property
    def oc(self) -> int:
        """Get or set the Q criterion:
        EQ.0:	Is output.
        EQ.1:	Is not output
        """ # nopep8
        return self._cards[1].get_value("oc")

    @oc.setter
    def oc(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""oc must be one of {0,1}""")
        self._cards[1].set_value("oc", value)

    @property
    def cfl(self) -> int:
        """Get or set the CFL number :
        EQ.0:	Is output.
        EQ.1:	Is not output
        """ # nopep8
        return self._cards[1].get_value("cfl")

    @cfl.setter
    def cfl(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""cfl must be one of {0,1}""")
        self._cards[1].set_value("cfl", value)

    @property
    def temp(self) -> int:
        """Get or set the Temperature :
        EQ.0:	Is output.
        EQ.1:	Is not output
        """ # nopep8
        return self._cards[2].get_value("temp")

    @temp.setter
    def temp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""temp must be one of {0,1}""")
        self._cards[2].set_value("temp", value)

    @property
    def tempavg(self) -> int:
        """Get or set the average temperature  :
        EQ.0:	Is output.
        EQ.1:	Is not output
        """ # nopep8
        return self._cards[2].get_value("tempavg")

    @tempavg.setter
    def tempavg(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""tempavg must be one of {0,1}""")
        self._cards[2].set_value("tempavg", value)

    @property
    def kp(self) -> int:
        """Get or set the RANS output variables, kinetic energy, diffusion, turbulent viscosity, turbulent intensity, Cmu variable ::
        EQ.0:	Is output.
        EQ.1:	Is not output
        """ # nopep8
        return self._cards[3].get_value("kp")

    @kp.setter
    def kp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""kp must be one of {0,1}""")
        self._cards[3].set_value("kp", value)

    @property
    def ep(self) -> int:
        """Get or set the RANS output variables, kinetic energy, diffusion, turbulent viscosity, turbulent intensity, Cmu variable ::
        EQ.0:	Is output.
        EQ.1:	Is not output
        """ # nopep8
        return self._cards[3].get_value("ep")

    @ep.setter
    def ep(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ep must be one of {0,1}""")
        self._cards[3].set_value("ep", value)

    @property
    def mut(self) -> int:
        """Get or set the RANS output variables, kinetic energy, diffusion, turbulent viscosity, turbulent intensity, Cmu variable ::
        EQ.0:	Is output.
        EQ.1:	Is not output
        """ # nopep8
        return self._cards[3].get_value("mut")

    @mut.setter
    def mut(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""mut must be one of {0,1}""")
        self._cards[3].set_value("mut", value)

    @property
    def int_(self) -> int:
        """Get or set the RANS output variables, kinetic energy, diffusion, turbulent viscosity, turbulent intensity, Cmu variable ::
        EQ.0:	Is output.
        EQ.1:	Is not output
        """ # nopep8
        return self._cards[3].get_value("int")

    @int_.setter
    def int_(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""int_ must be one of {0,1}""")
        self._cards[3].set_value("int", value)

    @property
    def cmu(self) -> int:
        """Get or set the RANS output variables, kinetic energy, diffusion, turbulent viscosity, turbulent intensity, Cmu variable ::
        EQ.0:	Is output.
        EQ.1:	Is not output
        """ # nopep8
        return self._cards[3].get_value("cmu")

    @cmu.setter
    def cmu(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""cmu must be one of {0,1}""")
        self._cards[3].set_value("cmu", value)

