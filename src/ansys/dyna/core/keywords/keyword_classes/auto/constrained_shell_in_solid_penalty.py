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

class ConstrainedShellInSolidPenalty(KeywordBase):
    """DYNA CONSTRAINED_SHELL_IN_SOLID_PENALTY keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "SHELL_IN_SOLID_PENALTY"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "coupid",
                        int,
                        0,
                        10,
                        kwargs.get("coupid")
                    ),
                    Field(
                        "title",
                        str,
                        10,
                        70,
                        kwargs.get("title")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "shsid",
                        int,
                        0,
                        10,
                        kwargs.get("shsid")
                    ),
                    Field(
                        "ssid",
                        int,
                        10,
                        10,
                        kwargs.get("ssid")
                    ),
                    Field(
                        "shstyp",
                        int,
                        20,
                        10,
                        kwargs.get("shstyp", 0)
                    ),
                    Field(
                        "sstyp",
                        int,
                        30,
                        10,
                        kwargs.get("sstyp", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "start",
                        float,
                        0,
                        10,
                        kwargs.get("start", 0.0)
                    ),
                    Field(
                        "end",
                        float,
                        10,
                        10,
                        kwargs.get("end", 10E20)
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "pssf",
                        float,
                        50,
                        10,
                        kwargs.get("pssf", 0.1)
                    ),
                ],
            ),
        ]

    @property
    def coupid(self) -> typing.Optional[int]:
        """Get or set the Coupling card ID number
        """ # nopep8
        return self._cards[0].get_value("coupid")

    @coupid.setter
    def coupid(self, value: int) -> None:
        self._cards[0].set_value("coupid", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the A description of this coupling definition
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[0].set_value("title", value)

    @property
    def shsid(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID of the Lagrangian shell structure(see *PART,* SET_PART)
        """ # nopep8
        return self._cards[1].get_value("shsid")

    @shsid.setter
    def shsid(self, value: int) -> None:
        self._cards[1].set_value("shsid", value)

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Part or part set ID of the Lagrangian solid elements or thick shell element(see *PART,* SET_PART)
        """ # nopep8
        return self._cards[1].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        self._cards[1].set_value("ssid", value)

    @property
    def shstyp(self) -> int:
        """Get or set the Set type of SHSID
        EQ.0: part set ID (PSID).
        EQ.1: part ID (PID).
        """ # nopep8
        return self._cards[1].get_value("shstyp")

    @shstyp.setter
    def shstyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""shstyp must be one of {0,1}""")
        self._cards[1].set_value("shstyp", value)

    @property
    def sstyp(self) -> int:
        """Get or set the Set type of SSID
        EQ.0: part set ID (PSID).
        EQ.1: part ID (PID).
        """ # nopep8
        return self._cards[1].get_value("sstyp")

    @sstyp.setter
    def sstyp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""sstyp must be one of {0,1}""")
        self._cards[1].set_value("sstyp", value)

    @property
    def start(self) -> float:
        """Get or set the Start time to activate the coupling
        LT.0:	Start time is set to |START|.  When negative, start time is followed during the dynamic relaxation phase of the calculation.  After the completion of dynamic relaxation, coupling is activated regardless of the value of END.EQ.0:	Start time is inactive, meaning coupling is always active
        GT.0 : If END = -9999, START is interpreted as the curve or table ID defining multiple pairs of start - time and end - time.Otherwise, if END > 0, start time applies both duringand after dynamic relaxation.
        """ # nopep8
        return self._cards[2].get_value("start")

    @start.setter
    def start(self, value: float) -> None:
        self._cards[2].set_value("start", value)

    @property
    def end(self) -> float:
        """Get or set the End time to deactive the coupling
        LT.0:	If END = -9999, START is interpreted as the curve or table ID defining multiple pairs of start-time and end-time.  Otherwise, negative END indicates that coupling is inactive during dynamic relaxation.  After dynamic relaxation the start and end times are followed and set to |START| and |END|, respectively.EQ.0:	END defaults to 1020.
        GT.0 : END sets the time at which the coupling is deactivated.
        """ # nopep8
        return self._cards[2].get_value("end")

    @end.setter
    def end(self, value: float) -> None:
        self._cards[2].set_value("end", value)

    @property
    def pssf(self) -> float:
        """Get or set the Penalty spring stiffness scale factor. Only available in penalty form.
        """ # nopep8
        return self._cards[2].get_value("pssf")

    @pssf.setter
    def pssf(self, value: float) -> None:
        self._cards[2].set_value("pssf", value)
