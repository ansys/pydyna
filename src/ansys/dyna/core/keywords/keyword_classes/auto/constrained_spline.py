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

class ConstrainedSpline(KeywordBase):
    """DYNA CONSTRAINED_SPLINE keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "SPLINE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "splid",
                        int,
                        0,
                        10,
                        kwargs.get("splid")
                    ),
                    Field(
                        "dlratio",
                        float,
                        10,
                        10,
                        kwargs.get("dlratio", 0.10)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nid",
                        int,
                        0,
                        10,
                        kwargs.get("nid")
                    ),
                    Field(
                        "dof",
                        int,
                        10,
                        10,
                        kwargs.get("dof")
                    ),
                ],
            ),
        ]

    @property
    def splid(self) -> typing.Optional[int]:
        """Get or set the spline constrained ID.
        """ # nopep8
        return self._cards[0].get_value("splid")

    @splid.setter
    def splid(self, value: int) -> None:
        self._cards[0].set_value("splid", value)

    @property
    def dlratio(self) -> float:
        """Get or set the Ratio of bending to torsional stiffness for an elastic tubular beam which connects the independent degrees of freedom. The default value is set to 0.10.
        """ # nopep8
        return self._cards[0].get_value("dlratio")

    @dlratio.setter
    def dlratio(self, value: float) -> None:
        self._cards[0].set_value("dlratio", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Independent/dependent node ID. For explicit problems this node should not be a member of a rigid body, or elsewhere constrained in the input.
        """ # nopep8
        return self._cards[1].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[1].set_value("nid", value)

    @property
    def dof(self) -> typing.Optional[int]:
        """Get or set the Degrees-of-dreedom. The list of dependent degrees-of-freedom consists of a number with up to six digits, with each digit representing a degree of dreedom. For example, the value 1356 indicates that degrees of freedom 1,3,5, and 6 are constrolled by the constrainet. The default is 123456. Digit:degree of freedom ID's:
        EQ:1 x
        EQ:2 Y
        EQ:3:z
        EQ:4:rotation about x axis
        EQ:5:rotation about y axis
        EQ:6:rotation about z axis
        """ # nopep8
        return self._cards[1].get_value("dof")

    @dof.setter
    def dof(self, value: int) -> None:
        self._cards[1].set_value("dof", value)

