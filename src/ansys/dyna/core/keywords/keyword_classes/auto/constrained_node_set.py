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

class ConstrainedNodeSet(KeywordBase):
    """DYNA CONSTRAINED_NODE_SET keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "NODE_SET"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "cnsid",
                        int,
                        0,
                        10,
                        kwargs.get("cnsid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nsid",
                        int,
                        0,
                        10,
                        kwargs.get("nsid")
                    ),
                    Field(
                        "dof",
                        int,
                        10,
                        10,
                        kwargs.get("dof", 1)
                    ),
                    Field(
                        "tf",
                        float,
                        20,
                        10,
                        kwargs.get("tf", 1.0E+20)
                    ),
                ],
            ),
        ]

    @property
    def cnsid(self) -> typing.Optional[int]:
        """Get or set the Optional constrained node set ID
        """ # nopep8
        return self._cards[0].get_value("cnsid")

    @cnsid.setter
    def cnsid(self, value: int) -> None:
        self._cards[0].set_value("cnsid", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID, see *SET_NODE.
        """ # nopep8
        return self._cards[1].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        self._cards[1].set_value("nsid", value)

    @property
    def dof(self) -> int:
        """Get or set the Applicable degrees-of-freedom:
        EQ.1: x-translational degree-of-freedom,
        EQ.2: y-translational degree-of-freedom,
        EQ.3: z-translational degree-of-freedom,
        EQ.4: x and y-translational degrees-of-freedom,
        EQ.5: y and z-translational degrees-of-freedom,
        EQ.6: z and x-translational degrees-of-freedom,
        EQ.7: x, y, and z-translational degrees-of-freedom.
        EQ.8: electric potential of piezoelectric material.
        """ # nopep8
        return self._cards[1].get_value("dof")

    @dof.setter
    def dof(self, value: int) -> None:
        if value not in [1, 2, 3, 4, 5, 6, 7, 8]:
            raise Exception("""dof must be one of {1,2,3,4,5,6,7,8}""")
        self._cards[1].set_value("dof", value)

    @property
    def tf(self) -> float:
        """Get or set the Failure time for nodal constraint set.
        """ # nopep8
        return self._cards[1].get_value("tf")

    @tf.setter
    def tf(self, value: float) -> None:
        self._cards[1].set_value("tf", value)

