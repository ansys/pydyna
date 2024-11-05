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

class ElementSph(KeywordBase):
    """DYNA ELEMENT_SPH keyword"""

    keyword = "ELEMENT"
    subkeyword = "SPH"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nid",
                        int,
                        0,
                        8,
                        kwargs.get("nid")
                    ),
                    Field(
                        "pid",
                        int,
                        8,
                        8,
                        kwargs.get("pid")
                    ),
                    Field(
                        "mass",
                        float,
                        16,
                        16,
                        kwargs.get("mass", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID and Element ID are the same for the SPH option.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[0].set_value("nid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID to which this node (element) belongs.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def mass(self) -> float:
        """Get or set the Mass or volume of SPH element.
        GT.0: Mass value.
        LT.0: Volume. The absolute value will be used as volume. The density(rho) will be retrieved from the material card defined in PID. SPH element mass is calculated by abs(MASS)*rho
        """ # nopep8
        return self._cards[0].get_value("mass")

    @mass.setter
    def mass(self, value: float) -> None:
        self._cards[0].set_value("mass", value)

