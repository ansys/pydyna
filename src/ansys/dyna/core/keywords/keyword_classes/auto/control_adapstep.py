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

class ControlAdapstep(KeywordBase):
    """DYNA CONTROL_ADAPSTEP keyword"""

    keyword = "CONTROL"
    subkeyword = "ADAPSTEP"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "factin",
                        float,
                        0,
                        10,
                        kwargs.get("factin", 1.0)
                    ),
                    Field(
                        "dfactr",
                        float,
                        10,
                        10,
                        kwargs.get("dfactr", 0.01)
                    ),
                ],
            ),
        ]

    @property
    def factin(self) -> float:
        """Get or set the Initial relaxation factor for contact force during each adaptive remesh. Unless stability problems occur in the contact, FACTIN=1.0 is recommended since this option can create some numerical noise in the resultant tooling forces. A typical value for this parameter is 0.10.
        """ # nopep8
        return self._cards[0].get_value("factin")

    @factin.setter
    def factin(self, value: float) -> None:
        self._cards[0].set_value("factin", value)

    @property
    def dfactr(self) -> float:
        """Get or set the Incremental increase of FACTIN during each time step after the adaptive step. FACTIN is not allowed to exceed unity. A typical value might be 0.01 (default).
        """ # nopep8
        return self._cards[0].get_value("dfactr")

    @dfactr.setter
    def dfactr(self, value: float) -> None:
        self._cards[0].set_value("dfactr", value)

