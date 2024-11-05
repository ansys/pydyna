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

class EmRandlesMeshless(KeywordBase):
    """DYNA EM_RANDLES_MESHLESS keyword"""

    keyword = "EM"
    subkeyword = "RANDLES_MESHLESS"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "rdlid",
                        int,
                        0,
                        10,
                        kwargs.get("rdlid")
                    ),
                    Field(
                        "rdltype",
                        int,
                        10,
                        10,
                        kwargs.get("rdltype", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "q",
                        float,
                        0,
                        10,
                        kwargs.get("q")
                    ),
                    Field(
                        "cq",
                        float,
                        10,
                        10,
                        kwargs.get("cq")
                    ),
                    Field(
                        "socinit",
                        float,
                        20,
                        10,
                        kwargs.get("socinit")
                    ),
                    Field(
                        "soctou",
                        float,
                        30,
                        10,
                        kwargs.get("soctou")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "r0cha",
                        float,
                        0,
                        10,
                        kwargs.get("r0cha")
                    ),
                    Field(
                        "r0dis",
                        float,
                        10,
                        10,
                        kwargs.get("r0dis")
                    ),
                    Field(
                        "r10cha",
                        float,
                        20,
                        10,
                        kwargs.get("r10cha")
                    ),
                    Field(
                        "r10dis",
                        float,
                        30,
                        10,
                        kwargs.get("r10dis")
                    ),
                    Field(
                        "c10cha",
                        float,
                        40,
                        10,
                        kwargs.get("c10cha")
                    ),
                    Field(
                        "c10dis",
                        float,
                        50,
                        10,
                        kwargs.get("c10dis")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "r20cha",
                        float,
                        0,
                        10,
                        kwargs.get("r20cha")
                    ),
                    Field(
                        "r20dis",
                        float,
                        10,
                        10,
                        kwargs.get("r20dis")
                    ),
                    Field(
                        "c20cha",
                        float,
                        20,
                        10,
                        kwargs.get("c20cha")
                    ),
                    Field(
                        "c20dis",
                        float,
                        30,
                        10,
                        kwargs.get("c20dis")
                    ),
                    Field(
                        "r30cha",
                        float,
                        40,
                        10,
                        kwargs.get("r30cha")
                    ),
                    Field(
                        "r30dis",
                        float,
                        50,
                        10,
                        kwargs.get("r30dis")
                    ),
                    Field(
                        "c30cha",
                        float,
                        60,
                        10,
                        kwargs.get("c30cha")
                    ),
                    Field(
                        "c30dis",
                        float,
                        70,
                        10,
                        kwargs.get("c30dis")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "temp",
                        float,
                        0,
                        10,
                        kwargs.get("temp")
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "dudt",
                        float,
                        30,
                        10,
                        kwargs.get("dudt")
                    ),
                    Field(
                        "tempu",
                        int,
                        40,
                        10,
                        kwargs.get("tempu", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "usesocs",
                        int,
                        0,
                        10,
                        kwargs.get("usesocs", 0)
                    ),
                    Field(
                        "tau",
                        float,
                        10,
                        10,
                        kwargs.get("tau")
                    ),
                    Field(
                        "flcid",
                        int,
                        20,
                        10,
                        kwargs.get("flcid")
                    ),
                ],
            ),
        ]

    @property
    def rdlid(self) -> typing.Optional[int]:
        """Get or set the Id of the Randles Cell.
        """ # nopep8
        return self._cards[0].get_value("rdlid")

    @rdlid.setter
    def rdlid(self, value: int) -> None:
        self._cards[0].set_value("rdlid", value)

    @property
    def rdltype(self) -> int:
        """Get or set the Type of Randles Cell
        EQ.0:0-order Randles Cell.
        EQ.1:1-order Randles Cell.
        EQ.2: 2-order Randles Cell.
        EQ.3: 3-order Randles Cell.
        """ # nopep8
        return self._cards[0].get_value("rdltype")

    @rdltype.setter
    def rdltype(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""rdltype must be one of {0,1,2,3}""")
        self._cards[0].set_value("rdltype", value)

    @property
    def q(self) -> typing.Optional[float]:
        """Get or set the Cell capacity.
        """ # nopep8
        return self._cards[1].get_value("q")

    @q.setter
    def q(self, value: float) -> None:
        self._cards[1].set_value("q", value)

    @property
    def cq(self) -> typing.Optional[float]:
        """Get or set the SOC conversion factor (%/s), known to be equal to 1/36 in S.I units.
        """ # nopep8
        return self._cards[1].get_value("cq")

    @cq.setter
    def cq(self, value: float) -> None:
        self._cards[1].set_value("cq", value)

    @property
    def socinit(self) -> typing.Optional[float]:
        """Get or set the Initial state of charge of the cell.
        """ # nopep8
        return self._cards[1].get_value("socinit")

    @socinit.setter
    def socinit(self, value: float) -> None:
        self._cards[1].set_value("socinit", value)

    @property
    def soctou(self) -> typing.Optional[float]:
        """Get or set the Constant value if positive or load curve ID if negative integer defining the equilibrium voltage (OCV) as a function of the state of charge (SOC).
        """ # nopep8
        return self._cards[1].get_value("soctou")

    @soctou.setter
    def soctou(self, value: float) -> None:
        self._cards[1].set_value("soctou", value)

    @property
    def r0cha(self) -> typing.Optional[float]:
        """Get or set the Constant if positive value, or load curve(if negative integer) defining r0/r10/c10 when the current flows in the charge direction as a function of SOC.
        """ # nopep8
        return self._cards[2].get_value("r0cha")

    @r0cha.setter
    def r0cha(self, value: float) -> None:
        self._cards[2].set_value("r0cha", value)

    @property
    def r0dis(self) -> typing.Optional[float]:
        """Get or set the Constant if positive value, or load curve (if negative integer) defining r0/r10/c10 when the current flows in the discharge direction as a function of SOC.
        """ # nopep8
        return self._cards[2].get_value("r0dis")

    @r0dis.setter
    def r0dis(self, value: float) -> None:
        self._cards[2].set_value("r0dis", value)

    @property
    def r10cha(self) -> typing.Optional[float]:
        """Get or set the Constant if positive value, or load curve(if negative integer) defining r0/r10/c10 when the current flows in the charge direction as a function of SOC.
        """ # nopep8
        return self._cards[2].get_value("r10cha")

    @r10cha.setter
    def r10cha(self, value: float) -> None:
        self._cards[2].set_value("r10cha", value)

    @property
    def r10dis(self) -> typing.Optional[float]:
        """Get or set the Constant if positive value, or load curve (if negative integer) defining r0/r10/c10 when the current flows in the discharge direction as a function of SOC.
        """ # nopep8
        return self._cards[2].get_value("r10dis")

    @r10dis.setter
    def r10dis(self, value: float) -> None:
        self._cards[2].set_value("r10dis", value)

    @property
    def c10cha(self) -> typing.Optional[float]:
        """Get or set the Constant if positive value, or load curve(if negative integer) defining r0/r10/c10 when the current flows in the charge direction as a function of SOC.
        """ # nopep8
        return self._cards[2].get_value("c10cha")

    @c10cha.setter
    def c10cha(self, value: float) -> None:
        self._cards[2].set_value("c10cha", value)

    @property
    def c10dis(self) -> typing.Optional[float]:
        """Get or set the Constant if positive value, or load curve (if negative integer) defining r0/r10/c10 when the current flows in the discharge direction as a function of SOC.
        """ # nopep8
        return self._cards[2].get_value("c10dis")

    @c10dis.setter
    def c10dis(self, value: float) -> None:
        self._cards[2].set_value("c10dis", value)

    @property
    def r20cha(self) -> typing.Optional[float]:
        """Get or set the r20 when the current flows in the charge direction:
        GE.0.0:constant value.
        LT.0.0:absolute value is a define function or table ID.
        """ # nopep8
        return self._cards[3].get_value("r20cha")

    @r20cha.setter
    def r20cha(self, value: float) -> None:
        self._cards[3].set_value("r20cha", value)

    @property
    def r20dis(self) -> typing.Optional[float]:
        """Get or set the r20 when the current flows in the discharge direction:
        GE.0.0:constant value.
        LT.0.0:absolute value is a define function or table ID.
        """ # nopep8
        return self._cards[3].get_value("r20dis")

    @r20dis.setter
    def r20dis(self, value: float) -> None:
        self._cards[3].set_value("r20dis", value)

    @property
    def c20cha(self) -> typing.Optional[float]:
        """Get or set the c20 when the current flows in the charge direction:
        GE.0.0:constant value.
        LT.0.0:absolute value is a define function or table ID.
        """ # nopep8
        return self._cards[3].get_value("c20cha")

    @c20cha.setter
    def c20cha(self, value: float) -> None:
        self._cards[3].set_value("c20cha", value)

    @property
    def c20dis(self) -> typing.Optional[float]:
        """Get or set the c20 when the current flows in the discharge direction:
        GE.0.0:constant value.
        LT.0.0:absolute value is a define function or table ID.
        """ # nopep8
        return self._cards[3].get_value("c20dis")

    @c20dis.setter
    def c20dis(self, value: float) -> None:
        self._cards[3].set_value("c20dis", value)

    @property
    def r30cha(self) -> typing.Optional[float]:
        """Get or set the r30 when the current flows in the charge direction:
        GE.0.0:constant value.
        LT.0.0:absolute value is a define function or table ID.
        """ # nopep8
        return self._cards[3].get_value("r30cha")

    @r30cha.setter
    def r30cha(self, value: float) -> None:
        self._cards[3].set_value("r30cha", value)

    @property
    def r30dis(self) -> typing.Optional[float]:
        """Get or set the r30 when the current flows in the discharge direction:
        GE.0.0:constant value.
        LT.0.0:absolute value is a define function or table ID.
        """ # nopep8
        return self._cards[3].get_value("r30dis")

    @r30dis.setter
    def r30dis(self, value: float) -> None:
        self._cards[3].set_value("r30dis", value)

    @property
    def c30cha(self) -> typing.Optional[float]:
        """Get or set the c30 when the current flows in the charge direction:
        GE.0.0:constant value.
        LT.0.0:absolute value is a define function or table ID.
        """ # nopep8
        return self._cards[3].get_value("c30cha")

    @c30cha.setter
    def c30cha(self, value: float) -> None:
        self._cards[3].set_value("c30cha", value)

    @property
    def c30dis(self) -> typing.Optional[float]:
        """Get or set the c30 when the current flows in the discharge direction:
        GE.0.0:constant value.
        LT.0.0:absolute value is a define function or table ID.
        """ # nopep8
        return self._cards[3].get_value("c30dis")

    @c30dis.setter
    def c30dis(self, value: float) -> None:
        self._cards[3].set_value("c30dis", value)

    @property
    def temp(self) -> typing.Optional[float]:
        """Get or set the Constant temperature value used for the Randles circuit parameters in case there is no coupling with the thermal solver.
        """ # nopep8
        return self._cards[4].get_value("temp")

    @temp.setter
    def temp(self, value: float) -> None:
        self._cards[4].set_value("temp", value)

    @property
    def dudt(self) -> typing.Optional[float]:
        """Get or set the If negative integer, load curve ID of the reversible heat as a function of SOC.
        """ # nopep8
        return self._cards[4].get_value("dudt")

    @dudt.setter
    def dudt(self, value: float) -> None:
        self._cards[4].set_value("dudt", value)

    @property
    def tempu(self) -> int:
        """Get or set the Temperature Unit :
        EQ.0: The temperature is in Celsius
        EQ.1: The Temperature is in Kelvin.
        """ # nopep8
        return self._cards[4].get_value("tempu")

    @tempu.setter
    def tempu(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""tempu must be one of {0,1}""")
        self._cards[4].set_value("tempu", value)

    @property
    def usesocs(self) -> int:
        """Get or set the Use SOC shift:
        EQ.0:Don't use the added SOCshift
        EQ.1:Use the added SOCshift.
        """ # nopep8
        return self._cards[5].get_value("usesocs")

    @usesocs.setter
    def usesocs(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""usesocs must be one of {0,1}""")
        self._cards[5].set_value("usesocs", value)

    @property
    def tau(self) -> typing.Optional[float]:
        """Get or set the Damping time in the SOCshift equation.
        """ # nopep8
        return self._cards[5].get_value("tau")

    @tau.setter
    def tau(self, value: float) -> None:
        self._cards[5].set_value("tau", value)

    @property
    def flcid(self) -> typing.Optional[int]:
        """Get or set the Load curve giving f(i) where I is the total current in the unit cell.
        """ # nopep8
        return self._cards[5].get_value("flcid")

    @flcid.setter
    def flcid(self, value: int) -> None:
        self._cards[5].set_value("flcid", value)

