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

class EmControlTimestep(KeywordBase):
    """DYNA EM_CONTROL_TIMESTEP keyword"""

    keyword = "EM"
    subkeyword = "CONTROL_TIMESTEP"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "tstype",
                        int,
                        0,
                        10,
                        kwargs.get("tstype", 1)
                    ),
                    Field(
                        "dtcons",
                        float,
                        10,
                        10,
                        kwargs.get("dtcons")
                    ),
                    Field(
                        "lcid",
                        int,
                        20,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "factor",
                        float,
                        30,
                        10,
                        kwargs.get("factor", 1.0)
                    ),
                    Field(
                        "tsmin",
                        float,
                        40,
                        10,
                        kwargs.get("tsmin")
                    ),
                    Field(
                        "tsmas",
                        float,
                        50,
                        10,
                        kwargs.get("tsmas")
                    ),
                    Field(
                        "rlcsf",
                        int,
                        60,
                        10,
                        kwargs.get("rlcsf", 25)
                    ),
                    Field(
                        "mecats",
                        int,
                        70,
                        10,
                        kwargs.get("mecats", 0)
                    ),
                ],
            ),
        ]

    @property
    def tstype(self) -> int:
        """Get or set the Time Step type
        EQ.1 : constant time step given in DTCONST
        EQ.2 : time step as a function of time given by a load curve specified in LCID
        EQ.3 : Automatic time step computation, depending on the solver type. This time step is then multiplied by FACTOR
        """ # nopep8
        return self._cards[0].get_value("tstype")

    @tstype.setter
    def tstype(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""tstype must be one of {1,2,3}""")
        self._cards[0].set_value("tstype", value)

    @property
    def dtcons(self) -> typing.Optional[float]:
        """Get or set the Constant value for the time step for TSTYPE=1
        """ # nopep8
        return self._cards[0].get_value("dtcons")

    @dtcons.setter
    def dtcons(self, value: float) -> None:
        self._cards[0].set_value("dtcons", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID giving the time step vs time for TSTYPE=2
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def factor(self) -> float:
        """Get or set the Multiplicative factor applied to the time step for TSTYPE=3
        """ # nopep8
        return self._cards[0].get_value("factor")

    @factor.setter
    def factor(self, value: float) -> None:
        self._cards[0].set_value("factor", value)

    @property
    def tsmin(self) -> typing.Optional[float]:
        """Get or set the Minimum time step. When TSMIN is defined, the EM time step cannot drop below TSMIN. A negative value will refer to a time dependent load curve.
        """ # nopep8
        return self._cards[0].get_value("tsmin")

    @tsmin.setter
    def tsmin(self, value: float) -> None:
        self._cards[0].set_value("tsmin", value)

    @property
    def tsmas(self) -> typing.Optional[float]:
        """Get or set the Maximum time step. When TSMAX is defined, the EM time step cannot increase beyond TSMAX. A negative value will refer to a time dependent load curve.
        """ # nopep8
        return self._cards[0].get_value("tsmas")

    @tsmas.setter
    def tsmas(self, value: float) -> None:
        self._cards[0].set_value("tsmas", value)

    @property
    def rlcsf(self) -> int:
        """Get or set the RLC Circuit time step scale factor.
        """ # nopep8
        return self._cards[0].get_value("rlcsf")

    @rlcsf.setter
    def rlcsf(self, value: int) -> None:
        self._cards[0].set_value("rlcsf", value)

    @property
    def mecats(self) -> int:
        """Get or set the Mechanical time step handling in cases where the EM solver time step becomes smaller (see Remark 3):
        EQ.0: Default.The EM time step will go below the solid mechanics timestep,and several EM solves will occur between two solid mechanics time steps to ensure time consistency.
        EQ.1: The solid mechanics time step will adapt and decrease to the EM time step value so that only one EM solve occurs between two solid mechanics solves.
        """ # nopep8
        return self._cards[0].get_value("mecats")

    @mecats.setter
    def mecats(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""mecats must be one of {0,1}""")
        self._cards[0].set_value("mecats", value)

