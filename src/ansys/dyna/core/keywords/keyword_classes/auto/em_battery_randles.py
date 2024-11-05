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

class EmBatteryRandles(KeywordBase):
    """DYNA EM_BATTERY_RANDLES keyword"""

    keyword = "EM"
    subkeyword = "BATTERY_RANDLES"

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
                        kwargs.get("rdltype")
                    ),
                    Field(
                        "rdlarea",
                        int,
                        20,
                        10,
                        kwargs.get("rdlarea", 0)
                    ),
                    Field(
                        "ccppart",
                        int,
                        30,
                        10,
                        kwargs.get("ccppart")
                    ),
                    Field(
                        "ccnpart",
                        int,
                        40,
                        10,
                        kwargs.get("ccnpart")
                    ),
                    Field(
                        "seppart",
                        int,
                        50,
                        10,
                        kwargs.get("seppart")
                    ),
                    Field(
                        "poselpart",
                        int,
                        60,
                        10,
                        kwargs.get("poselpart")
                    ),
                    Field(
                        "negelpart",
                        int,
                        70,
                        10,
                        kwargs.get("negelpart")
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
                        "temp",
                        float,
                        0,
                        10,
                        kwargs.get("temp")
                    ),
                    Field(
                        "frtherm",
                        int,
                        10,
                        10,
                        kwargs.get("frtherm", 0)
                    ),
                    Field(
                        "r0toth",
                        int,
                        20,
                        10,
                        kwargs.get("r0toth", 0)
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
                        "tausocs",
                        float,
                        10,
                        10,
                        kwargs.get("tausocs")
                    ),
                    Field(
                        "sicslcid",
                        int,
                        20,
                        10,
                        kwargs.get("sicslcid")
                    ),
                ],
            ),
        ]

    @property
    def rdlid(self) -> typing.Optional[int]:
        """Get or set the Id of the Randle Cell
        """ # nopep8
        return self._cards[0].get_value("rdlid")

    @rdlid.setter
    def rdlid(self, value: int) -> None:
        self._cards[0].set_value("rdlid", value)

    @property
    def rdltype(self) -> typing.Optional[int]:
        """Get or set the Type of Randle Cell
        """ # nopep8
        return self._cards[0].get_value("rdltype")

    @rdltype.setter
    def rdltype(self, value: int) -> None:
        self._cards[0].set_value("rdltype", value)

    @property
    def rdlarea(self) -> int:
        """Get or set the Randle Area:
        EQ.0:	Default.The parameters are not scaled by area factors.
        EQ.1:	The parameters are per unit area and will be scaled in each Randle circuit by a factor depending on the local area of the circuit.
        EQ.2:	The parameters are defined for the whole unit cell and will be scaled in each Randle circuit by a factor depending on the local area of the circuit and the global area of the cell.
        """ # nopep8
        return self._cards[0].get_value("rdlarea")

    @rdlarea.setter
    def rdlarea(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""rdlarea must be one of {0,1,2}""")
        self._cards[0].set_value("rdlarea", value)

    @property
    def ccppart(self) -> typing.Optional[int]:
        """Get or set the CCP Part ID.
        """ # nopep8
        return self._cards[0].get_value("ccppart")

    @ccppart.setter
    def ccppart(self, value: int) -> None:
        self._cards[0].set_value("ccppart", value)

    @property
    def ccnpart(self) -> typing.Optional[int]:
        """Get or set the CCN Part ID.
        """ # nopep8
        return self._cards[0].get_value("ccnpart")

    @ccnpart.setter
    def ccnpart(self, value: int) -> None:
        self._cards[0].set_value("ccnpart", value)

    @property
    def seppart(self) -> typing.Optional[int]:
        """Get or set the Separator Part ID
        """ # nopep8
        return self._cards[0].get_value("seppart")

    @seppart.setter
    def seppart(self, value: int) -> None:
        self._cards[0].set_value("seppart", value)

    @property
    def poselpart(self) -> typing.Optional[int]:
        """Get or set the Positive Electrode Part ID
        """ # nopep8
        return self._cards[0].get_value("poselpart")

    @poselpart.setter
    def poselpart(self, value: int) -> None:
        self._cards[0].set_value("poselpart", value)

    @property
    def negelpart(self) -> typing.Optional[int]:
        """Get or set the Negative Electrode Part ID
        """ # nopep8
        return self._cards[0].get_value("negelpart")

    @negelpart.setter
    def negelpart(self, value: int) -> None:
        self._cards[0].set_value("negelpart", value)

    @property
    def q(self) -> typing.Optional[float]:
        """Get or set the Unit cell capacity
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
        """Get or set the Initial state of charge of the unit cell.
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
        """Get or set the Constant if positive value or load curve or table id (if negative integer) defining r0/r10/c10 when the current flows in the charge direction as a function of:
        -SOC if load curve
        -SOC and Temperature if table.
        """ # nopep8
        return self._cards[2].get_value("r0cha")

    @r0cha.setter
    def r0cha(self, value: float) -> None:
        self._cards[2].set_value("r0cha", value)

    @property
    def r0dis(self) -> typing.Optional[float]:
        """Get or set the Constant if positive value or load curve or table id (if negative integer) defining r0/r10/c10 when the current flows in the discharge direction as a function of:
        -SOC if load curve
        -SOC and Temperature if table.
        """ # nopep8
        return self._cards[2].get_value("r0dis")

    @r0dis.setter
    def r0dis(self, value: float) -> None:
        self._cards[2].set_value("r0dis", value)

    @property
    def r10cha(self) -> typing.Optional[float]:
        """Get or set the Constant if positive value or load curve or table id (if negative integer) defining r0/r10/c10 when the current flows in the charge direction as a function of:
        -SOC if load curve
        -SOC and Temperature if table.
        """ # nopep8
        return self._cards[2].get_value("r10cha")

    @r10cha.setter
    def r10cha(self, value: float) -> None:
        self._cards[2].set_value("r10cha", value)

    @property
    def r10dis(self) -> typing.Optional[float]:
        """Get or set the Constant if positive value or load curve or table id (if negative integer) defining r0/r10/c10 when the current flows in the discharge direction as a function of:
        -SOC if load curve
        -SOC and Temperature if table.
        """ # nopep8
        return self._cards[2].get_value("r10dis")

    @r10dis.setter
    def r10dis(self, value: float) -> None:
        self._cards[2].set_value("r10dis", value)

    @property
    def c10cha(self) -> typing.Optional[float]:
        """Get or set the Constant if positive value or load curve or table id (if negative integer) defining r0/r10/c10 when the current flows in the charge direction as a function of:
        -SOC if load curve
        -SOC and Temperature if table.
        """ # nopep8
        return self._cards[2].get_value("c10cha")

    @c10cha.setter
    def c10cha(self, value: float) -> None:
        self._cards[2].set_value("c10cha", value)

    @property
    def c10dis(self) -> typing.Optional[float]:
        """Get or set the Constant if positive value or load curve or table id (if negative integer) defining r0/r10/c10 when the current flows in the discharge direction as a function of:
        -SOC if load curve
        -SOC and Temperature if table.
        """ # nopep8
        return self._cards[2].get_value("c10dis")

    @c10dis.setter
    def c10dis(self, value: float) -> None:
        self._cards[2].set_value("c10dis", value)

    @property
    def temp(self) -> typing.Optional[float]:
        """Get or set the Constant temperature value used for the Randle circuit parameters in case there is no coupling with the thermal solver (FRTHERM =0)
        """ # nopep8
        return self._cards[3].get_value("temp")

    @temp.setter
    def temp(self, value: float) -> None:
        self._cards[3].set_value("temp", value)

    @property
    def frtherm(self) -> int:
        """Get or set the From Thermal :
        EQ.0:	The temperature used in the Randle circuit parameters is TEMP
        EQ.1:	The temperature used in the Randle circuit parameter is the temperature from the thermal solver.
        """ # nopep8
        return self._cards[3].get_value("frtherm")

    @frtherm.setter
    def frtherm(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""frtherm must be one of {0,1}""")
        self._cards[3].set_value("frtherm", value)

    @property
    def r0toth(self) -> int:
        """Get or set the R0 to Thermal :
        EQ.0:	The joule heating in the resistance r0 is not added to the thermal solver
        EQ.1:	The joule heating in the resistance r0 is added to the thermal solver
        """ # nopep8
        return self._cards[3].get_value("r0toth")

    @r0toth.setter
    def r0toth(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""r0toth must be one of {0,1}""")
        self._cards[3].set_value("r0toth", value)

    @property
    def dudt(self) -> typing.Optional[float]:
        """Get or set the If negative integer, load curve ID of the reversible heat as a function of SOC.
        """ # nopep8
        return self._cards[3].get_value("dudt")

    @dudt.setter
    def dudt(self, value: float) -> None:
        self._cards[3].set_value("dudt", value)

    @property
    def tempu(self) -> int:
        """Get or set the Temperature Unit :
        EQ.0:	The temperature is in Celsius
        EQ.1:	The Temperature is in Kelvin
        """ # nopep8
        return self._cards[3].get_value("tempu")

    @tempu.setter
    def tempu(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""tempu must be one of {0,1}""")
        self._cards[3].set_value("tempu", value)

    @property
    def usesocs(self) -> int:
        """Get or set the Use SOC shift  :
        EQ.0:	Don't use the added SOCshift
        EQ.1:	Use the added SOCshift
        """ # nopep8
        return self._cards[4].get_value("usesocs")

    @usesocs.setter
    def usesocs(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""usesocs must be one of {0,1}""")
        self._cards[4].set_value("usesocs", value)

    @property
    def tausocs(self) -> typing.Optional[float]:
        """Get or set the Damping time in the SOCshift equation
        """ # nopep8
        return self._cards[4].get_value("tausocs")

    @tausocs.setter
    def tausocs(self, value: float) -> None:
        self._cards[4].set_value("tausocs", value)

    @property
    def sicslcid(self) -> typing.Optional[int]:
        """Get or set the Load curve giving f(i) where I is the total current in the unit cell
        """ # nopep8
        return self._cards[4].get_value("sicslcid")

    @sicslcid.setter
    def sicslcid(self, value: int) -> None:
        self._cards[4].set_value("sicslcid", value)

