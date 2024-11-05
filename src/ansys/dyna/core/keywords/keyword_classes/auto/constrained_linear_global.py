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

class ConstrainedLinearGlobal(KeywordBase):
    """DYNA CONSTRAINED_LINEAR_GLOBAL keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "LINEAR_GLOBAL"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "licd",
                        int,
                        0,
                        10,
                        kwargs.get("licd")
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
                        kwargs.get("dof", 1)
                    ),
                    Field(
                        "coef",
                        float,
                        20,
                        10,
                        kwargs.get("coef", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def licd(self) -> typing.Optional[int]:
        """Get or set the Linear constraint definition ID. This ID can be used to identify a set to which this constraint is a member.
        """ # nopep8
        return self._cards[0].get_value("licd")

    @licd.setter
    def licd(self, value: int) -> None:
        self._cards[0].set_value("licd", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node ID.
        """ # nopep8
        return self._cards[1].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[1].set_value("nid", value)

    @property
    def dof(self) -> int:
        """Get or set the Degree of freedom in the local coordinate system;
        EQ.1:displacement along global x-direction.
        EQ.2:displacement along global y-direction.
        EQ.3:displacement along global z-direction.
        EQ.4: global rotation about global x-axis.
        EQ.5: global rotation about global y-axis.
        EQ.6: global rotation about global z-axis.
        EQ.7:	Nodal electric voltage of piezoelectric material; see *MAT_ADD_PZELECTRIC.
        The voltage of the 1st node can only be defined as a linear combination of the voltage of other nodes, meaning all DOFs must be 7 for such an application.
        """ # nopep8
        return self._cards[1].get_value("dof")

    @dof.setter
    def dof(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 6, 7]:
            raise Exception("""dof must be one of {1,2,3,4,5,6,7}""")
        self._cards[1].set_value("dof", value)

    @property
    def coef(self) -> float:
        """Get or set the Nonzero coefficient, Ck
        """ # nopep8
        return self._cards[1].get_value("coef")

    @coef.setter
    def coef(self, value: float) -> None:
        self._cards[1].set_value("coef", value)

