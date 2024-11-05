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

class AirbagSimpleAirbagModel(KeywordBase):
    """DYNA AIRBAG_SIMPLE_AIRBAG_MODEL keyword"""

    keyword = "AIRBAG"
    subkeyword = "SIMPLE_AIRBAG_MODEL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "sid",
                        int,
                        0,
                        10,
                        kwargs.get("sid")
                    ),
                    Field(
                        "sidtyp",
                        int,
                        10,
                        10,
                        kwargs.get("sidtyp", 0)
                    ),
                    Field(
                        "rbid",
                        int,
                        20,
                        10,
                        kwargs.get("rbid", 0)
                    ),
                    Field(
                        "vsca",
                        float,
                        30,
                        10,
                        kwargs.get("vsca", 1.0)
                    ),
                    Field(
                        "psca",
                        float,
                        40,
                        10,
                        kwargs.get("psca", 1.0)
                    ),
                    Field(
                        "vini",
                        float,
                        50,
                        10,
                        kwargs.get("vini", 0.0)
                    ),
                    Field(
                        "mwd",
                        float,
                        60,
                        10,
                        kwargs.get("mwd", 0.0)
                    ),
                    Field(
                        "spsf",
                        float,
                        70,
                        10,
                        kwargs.get("spsf", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cv",
                        float,
                        0,
                        10,
                        kwargs.get("cv")
                    ),
                    Field(
                        "cp",
                        float,
                        10,
                        10,
                        kwargs.get("cp")
                    ),
                    Field(
                        "t",
                        float,
                        20,
                        10,
                        kwargs.get("t")
                    ),
                    Field(
                        "lcid",
                        int,
                        30,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "mu",
                        float,
                        40,
                        10,
                        kwargs.get("mu")
                    ),
                    Field(
                        "area",
                        float,
                        50,
                        10,
                        kwargs.get("area")
                    ),
                    Field(
                        "pe",
                        float,
                        60,
                        10,
                        kwargs.get("pe")
                    ),
                    Field(
                        "ro",
                        float,
                        70,
                        10,
                        kwargs.get("ro")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lou",
                        int,
                        0,
                        10,
                        kwargs.get("lou", 0)
                    ),
                    Field(
                        "text",
                        float,
                        10,
                        10,
                        kwargs.get("text", 0.0)
                    ),
                    Field(
                        "a",
                        float,
                        20,
                        10,
                        kwargs.get("a", 0.0)
                    ),
                    Field(
                        "b",
                        float,
                        30,
                        10,
                        kwargs.get("b", 0.0)
                    ),
                    Field(
                        "mw",
                        float,
                        40,
                        10,
                        kwargs.get("mw", 0.0)
                    ),
                    Field(
                        "gasc",
                        float,
                        50,
                        10,
                        kwargs.get("gasc", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def sid(self) -> typing.Optional[int]:
        """Get or set the Set ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def sidtyp(self) -> int:
        """Get or set the Set type:
        EQ.0: segment,
        EQ.1: part IDs.
        """ # nopep8
        return self._cards[0].get_value("sidtyp")

    @sidtyp.setter
    def sidtyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""sidtyp must be one of {0,1}""")
        self._cards[0].set_value("sidtyp", value)

    @property
    def rbid(self) -> int:
        """Get or set the Rigid body part ID for user defined activation subroutine:
        EQ.-RBID: sensor subroutine flags initiates the inflator. Load curves are offset by initiation time,
        EQ.0: the control volume is active from time zero,
        EQ.RBID: user sensor subroutine flags the start of the inflation. Load curves are offset by initiation time.
        """ # nopep8
        return self._cards[0].get_value("rbid")

    @rbid.setter
    def rbid(self, value: int) -> None:
        self._cards[0].set_value("rbid", value)

    @property
    def vsca(self) -> float:
        """Get or set the Volume scale factor, V-sca (default=1.0).
        """ # nopep8
        return self._cards[0].get_value("vsca")

    @vsca.setter
    def vsca(self, value: float) -> None:
        self._cards[0].set_value("vsca", value)

    @property
    def psca(self) -> float:
        """Get or set the Pressure scale factor, P-sca (default=1.0).
        """ # nopep8
        return self._cards[0].get_value("psca")

    @psca.setter
    def psca(self, value: float) -> None:
        self._cards[0].set_value("psca", value)

    @property
    def vini(self) -> float:
        """Get or set the Initial filled volume, V-ini (default=0.0).
        """ # nopep8
        return self._cards[0].get_value("vini")

    @vini.setter
    def vini(self, value: float) -> None:
        self._cards[0].set_value("vini", value)

    @property
    def mwd(self) -> float:
        """Get or set the Mass weighted damping factor, D (default=0.0).
        """ # nopep8
        return self._cards[0].get_value("mwd")

    @mwd.setter
    def mwd(self, value: float) -> None:
        self._cards[0].set_value("mwd", value)

    @property
    def spsf(self) -> float:
        """Get or set the Stagnation pressure scale factor, 0.0 <= gamma <= 1.0.
        """ # nopep8
        return self._cards[0].get_value("spsf")

    @spsf.setter
    def spsf(self, value: float) -> None:
        self._cards[0].set_value("spsf", value)

    @property
    def cv(self) -> typing.Optional[float]:
        """Get or set the Heat capacity at constant volume.
        """ # nopep8
        return self._cards[1].get_value("cv")

    @cv.setter
    def cv(self, value: float) -> None:
        self._cards[1].set_value("cv", value)

    @property
    def cp(self) -> typing.Optional[float]:
        """Get or set the Heat capacity at constant pressure.
        """ # nopep8
        return self._cards[1].get_value("cp")

    @cp.setter
    def cp(self, value: float) -> None:
        self._cards[1].set_value("cp", value)

    @property
    def t(self) -> typing.Optional[float]:
        """Get or set the Temperature of input gas.
        """ # nopep8
        return self._cards[1].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        self._cards[1].set_value("t", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID specifying input mass flow rate. See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[1].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[1].set_value("lcid", value)

    @property
    def mu(self) -> typing.Optional[float]:
        """Get or set the Shape factor for exit hole, mu:
        LT.0.0: |mu| is the load curve number defining the shape factor as a function of absolute pressure.
        """ # nopep8
        return self._cards[1].get_value("mu")

    @mu.setter
    def mu(self, value: float) -> None:
        self._cards[1].set_value("mu", value)

    @property
    def area(self) -> typing.Optional[float]:
        """Get or set the Exit area, A:
        GE.0.0: A is the exit area and is constant in time,
        LT.0.0: |A| is the load curve number defining the exit area as a function of absolute pressure.
        """ # nopep8
        return self._cards[1].get_value("area")

    @area.setter
    def area(self, value: float) -> None:
        self._cards[1].set_value("area", value)

    @property
    def pe(self) -> typing.Optional[float]:
        """Get or set the Ambient pressure, pe.
        """ # nopep8
        return self._cards[1].get_value("pe")

    @pe.setter
    def pe(self, value: float) -> None:
        self._cards[1].set_value("pe", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Ambient density, rho.
        """ # nopep8
        return self._cards[1].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[1].set_value("ro", value)

    @property
    def lou(self) -> int:
        """Get or set the Optional load curve ID giving mass flow out versus gauge pressure in bag. See *DEFINE_CURVE.
        """ # nopep8
        return self._cards[2].get_value("lou")

    @lou.setter
    def lou(self, value: int) -> None:
        self._cards[2].set_value("lou", value)

    @property
    def text(self) -> float:
        """Get or set the Ambient temperature. (Define if and only if CV=0.0).
        """ # nopep8
        return self._cards[2].get_value("text")

    @text.setter
    def text(self, value: float) -> None:
        self._cards[2].set_value("text", value)

    @property
    def a(self) -> float:
        """Get or set the First heat capacity coefficient of inflator gas (e.g., Joules/mole/o K). (Define if and only if CV=0.0).
        """ # nopep8
        return self._cards[2].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        self._cards[2].set_value("a", value)

    @property
    def b(self) -> float:
        """Get or set the Second heat capacity coefficient of inflator gas, (e.g., Joules/mole/o K**2 ).  (Define if and only if CV=0.0).
        """ # nopep8
        return self._cards[2].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        self._cards[2].set_value("b", value)

    @property
    def mw(self) -> float:
        """Get or set the Molecular weight of inflator gas (e.g., Kg/mole). (Define if and only if CV=0.0).
        """ # nopep8
        return self._cards[2].get_value("mw")

    @mw.setter
    def mw(self, value: float) -> None:
        self._cards[2].set_value("mw", value)

    @property
    def gasc(self) -> float:
        """Get or set the Universal gas constant of inflator gas (e.g., 8.314 Joules/mole/o K).  (Define if and only if CV=0.0).
        """ # nopep8
        return self._cards[2].get_value("gasc")

    @gasc.setter
    def gasc(self, value: float) -> None:
        self._cards[2].set_value("gasc", value)

