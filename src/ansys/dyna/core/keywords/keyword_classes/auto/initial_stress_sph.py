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

class InitialStressSph(KeywordBase):
    """DYNA INITIAL_STRESS_SPH keyword"""

    keyword = "INITIAL"
    subkeyword = "STRESS_SPH"

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
        """Get or set the SPH particle ID.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        self._cards[0].set_value("eid", value)

    @property
    def sigxx(self) -> float:
        """Get or set the Define the xx stress component (global cartesian system).
        """ # nopep8
        return self._cards[0].get_value("sigxx")

    @sigxx.setter
    def sigxx(self, value: float) -> None:
        self._cards[0].set_value("sigxx", value)

    @property
    def sigyy(self) -> float:
        """Get or set the Define the yy stress component (global cartesian system).
        """ # nopep8
        return self._cards[0].get_value("sigyy")

    @sigyy.setter
    def sigyy(self, value: float) -> None:
        self._cards[0].set_value("sigyy", value)

    @property
    def sigzz(self) -> float:
        """Get or set the Define the zz stress component (global cartesian system).
        """ # nopep8
        return self._cards[0].get_value("sigzz")

    @sigzz.setter
    def sigzz(self, value: float) -> None:
        self._cards[0].set_value("sigzz", value)

    @property
    def sigxy(self) -> float:
        """Get or set the Define the xy stress component (global cartesian system).
        """ # nopep8
        return self._cards[0].get_value("sigxy")

    @sigxy.setter
    def sigxy(self, value: float) -> None:
        self._cards[0].set_value("sigxy", value)

    @property
    def sigyz(self) -> float:
        """Get or set the Define the yz stress component (global cartesian system).
        """ # nopep8
        return self._cards[0].get_value("sigyz")

    @sigyz.setter
    def sigyz(self, value: float) -> None:
        self._cards[0].set_value("sigyz", value)

    @property
    def sigzx(self) -> float:
        """Get or set the Define the zx stress component (global cartesian system).
        """ # nopep8
        return self._cards[0].get_value("sigzx")

    @sigzx.setter
    def sigzx(self, value: float) -> None:
        self._cards[0].set_value("sigzx", value)

    @property
    def eps(self) -> float:
        """Get or set the Effective plastic strain.
        """ # nopep8
        return self._cards[0].get_value("eps")

    @eps.setter
    def eps(self, value: float) -> None:
        self._cards[0].set_value("eps", value)

