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

class ConstrainedEulerInEuler(KeywordBase):
    """DYNA CONSTRAINED_EULER_IN_EULER keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "EULER_IN_EULER"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "psid1",
                        int,
                        0,
                        10,
                        kwargs.get("psid1", 0)
                    ),
                    Field(
                        "psid2",
                        int,
                        10,
                        10,
                        kwargs.get("psid2", 0)
                    ),
                    Field(
                        "pfac",
                        float,
                        20,
                        10,
                        kwargs.get("pfac", 0.1)
                    ),
                ],
            ),
        ]

    @property
    def psid1(self) -> int:
        """Get or set the Part set ID of the 1st ALE or Eulerian set of mesh(es.
        """ # nopep8
        return self._cards[0].get_value("psid1")

    @psid1.setter
    def psid1(self, value: int) -> None:
        self._cards[0].set_value("psid1", value)

    @property
    def psid2(self) -> int:
        """Get or set the Part set ID of the 2nd ALE or Eulerian set of mesh(es).
        """ # nopep8
        return self._cards[0].get_value("psid2")

    @psid2.setter
    def psid2(self, value: int) -> None:
        self._cards[0].set_value("psid2", value)

    @property
    def pfac(self) -> float:
        """Get or set the A penalty factor for the coupling interaction between the two PSIDs.
        """ # nopep8
        return self._cards[0].get_value("pfac")

    @pfac.setter
    def pfac(self, value: float) -> None:
        self._cards[0].set_value("pfac", value)

