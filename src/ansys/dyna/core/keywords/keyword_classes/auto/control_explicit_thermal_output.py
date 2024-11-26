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

class ControlExplicitThermalOutput(KeywordBase):
    """DYNA CONTROL_EXPLICIT_THERMAL_OUTPUT keyword"""

    keyword = "CONTROL"
    subkeyword = "EXPLICIT_THERMAL_OUTPUT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "dtout",
                        float,
                        0,
                        10,
                        kwargs.get("dtout")
                    ),
                    Field(
                        "dtoutyp",
                        int,
                        10,
                        10,
                        kwargs.get("dtoutyp", 0)
                    ),
                    Field(
                        "setid",
                        int,
                        20,
                        10,
                        kwargs.get("setid", 0)
                    ),
                    Field(
                        "setyp",
                        int,
                        30,
                        10,
                        kwargs.get("setyp", 1)
                    ),
                ],
            ),
        ]

    @property
    def dtout(self) -> typing.Optional[float]:
        """Get or set the Time interval between outputs.
        """ # nopep8
        return self._cards[0].get_value("dtout")

    @dtout.setter
    def dtout(self, value: float) -> None:
        self._cards[0].set_value("dtout", value)

    @property
    def dtoutyp(self) -> int:
        """Get or set the Type of DTOUT:
        EQ.0:	DTOUT is a constant
        EQ.1 : DTOUT is the ID of * DEFINE_CURVE defining a table of  time as function of DTOUT
        """ # nopep8
        return self._cards[0].get_value("dtoutyp")

    @dtoutyp.setter
    def dtoutyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""dtoutyp must be one of {0,1}""")
        self._cards[0].set_value("dtoutyp", value)

    @property
    def setid(self) -> int:
        """Get or set the Set ID.  If SETID = 0, then temperatures and enthalpies are output for the whole model.  See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("setid")

    @setid.setter
    def setid(self, value: int) -> None:
        self._cards[0].set_value("setid", value)

    @property
    def setyp(self) -> int:
        """Get or set the Type of set:
        EQ.1:	solid set(see *SET_SOLID)
        EQ.2 : shell set(see *SET_SHELL)
        EQ.3 : beam set(see * SET_BEAM)
        EQ.4 : thick shell set(see *SET_TSHELL).
        """ # nopep8
        return self._cards[0].get_value("setyp")

    @setyp.setter
    def setyp(self, value: int) -> None:
        if value not in [1, 2, 3, 4]:
            raise Exception("""setyp must be one of {1,2,3,4}""")
        self._cards[0].set_value("setyp", value)

