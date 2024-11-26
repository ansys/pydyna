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

class InitialStressTshell(KeywordBase):
    """DYNA INITIAL_STRESS_TSHELL keyword"""

    keyword = "INITIAL"
    subkeyword = "STRESS_TSHELL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eid",
                        int,
                        0,
                        10,
                        kwargs.get("eid")
                    ),
                    Field(
                        "nplane",
                        int,
                        10,
                        10,
                        kwargs.get("nplane")
                    ),
                    Field(
                        "nthick",
                        int,
                        20,
                        10,
                        kwargs.get("nthick")
                    ),
                    Field(
                        "nhisv",
                        int,
                        30,
                        10,
                        kwargs.get("nhisv")
                    ),
                    Field(
                        "large",
                        int,
                        40,
                        10,
                        kwargs.get("large", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "t",
                        float,
                        0,
                        10,
                        kwargs.get("t")
                    ),
                    Field(
                        "sigxx",
                        float,
                        10,
                        10,
                        kwargs.get("sigxx", 0.0)
                    ),
                    Field(
                        "sigyy",
                        float,
                        20,
                        10,
                        kwargs.get("sigyy", 0.0)
                    ),
                    Field(
                        "sigzz",
                        float,
                        30,
                        10,
                        kwargs.get("sigzz", 0.0)
                    ),
                    Field(
                        "sigxy",
                        float,
                        40,
                        10,
                        kwargs.get("sigxy", 0.0)
                    ),
                    Field(
                        "sigyz",
                        float,
                        50,
                        10,
                        kwargs.get("sigyz", 0.0)
                    ),
                    Field(
                        "sigzx",
                        float,
                        60,
                        10,
                        kwargs.get("sigzx", 0.0)
                    ),
                    Field(
                        "eps",
                        float,
                        70,
                        10,
                        kwargs.get("eps", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the tShell element ID.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        self._cards[0].set_value("eid", value)

    @property
    def nplane(self) -> typing.Optional[int]:
        """Get or set the Number of in plane integration points being output
        """ # nopep8
        return self._cards[0].get_value("nplane")

    @nplane.setter
    def nplane(self, value: int) -> None:
        self._cards[0].set_value("nplane", value)

    @property
    def nthick(self) -> typing.Optional[int]:
        """Get or set the Number of through thickness integration points.
        """ # nopep8
        return self._cards[0].get_value("nthick")

    @nthick.setter
    def nthick(self, value: int) -> None:
        self._cards[0].set_value("nthick", value)

    @property
    def nhisv(self) -> typing.Optional[int]:
        """Get or set the Number of additional history variables
        """ # nopep8
        return self._cards[0].get_value("nhisv")

    @nhisv.setter
    def nhisv(self, value: int) -> None:
        self._cards[0].set_value("nhisv", value)

    @property
    def large(self) -> int:
        """Get or set the Format size (0:off or 1:on).
        """ # nopep8
        return self._cards[0].get_value("large")

    @large.setter
    def large(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""large must be one of {0,1}""")
        self._cards[0].set_value("large", value)

    @property
    def t(self) -> typing.Optional[float]:
        """Get or set the Parametric coordinate of through thickness integration point. Between -1 and 1 inclusive.
        """ # nopep8
        return self._cards[1].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        self._cards[1].set_value("t", value)

    @property
    def sigxx(self) -> float:
        """Get or set the Define the xx stress component (global cartesian system).
        """ # nopep8
        return self._cards[1].get_value("sigxx")

    @sigxx.setter
    def sigxx(self, value: float) -> None:
        self._cards[1].set_value("sigxx", value)

    @property
    def sigyy(self) -> float:
        """Get or set the Define the yy stress component (global cartesian system).
        """ # nopep8
        return self._cards[1].get_value("sigyy")

    @sigyy.setter
    def sigyy(self, value: float) -> None:
        self._cards[1].set_value("sigyy", value)

    @property
    def sigzz(self) -> float:
        """Get or set the Define the zz stress component (global cartesian system).
        """ # nopep8
        return self._cards[1].get_value("sigzz")

    @sigzz.setter
    def sigzz(self, value: float) -> None:
        self._cards[1].set_value("sigzz", value)

    @property
    def sigxy(self) -> float:
        """Get or set the Define the xy stress component (global cartesian system).
        """ # nopep8
        return self._cards[1].get_value("sigxy")

    @sigxy.setter
    def sigxy(self, value: float) -> None:
        self._cards[1].set_value("sigxy", value)

    @property
    def sigyz(self) -> float:
        """Get or set the Define the yz stress component (global cartesian system).
        """ # nopep8
        return self._cards[1].get_value("sigyz")

    @sigyz.setter
    def sigyz(self, value: float) -> None:
        self._cards[1].set_value("sigyz", value)

    @property
    def sigzx(self) -> float:
        """Get or set the Define the zx stress component (global cartesian system).
        """ # nopep8
        return self._cards[1].get_value("sigzx")

    @sigzx.setter
    def sigzx(self, value: float) -> None:
        self._cards[1].set_value("sigzx", value)

    @property
    def eps(self) -> float:
        """Get or set the Effective plastic strain.
        """ # nopep8
        return self._cards[1].get_value("eps")

    @eps.setter
    def eps(self, value: float) -> None:
        self._cards[1].set_value("eps", value)

