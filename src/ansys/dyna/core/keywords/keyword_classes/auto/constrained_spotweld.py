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

class ConstrainedSpotweld(KeywordBase):
    """DYNA CONSTRAINED_SPOTWELD keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "SPOTWELD"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "wid",
                        int,
                        0,
                        10,
                        kwargs.get("wid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "n1",
                        int,
                        0,
                        10,
                        kwargs.get("n1")
                    ),
                    Field(
                        "n2",
                        int,
                        10,
                        10,
                        kwargs.get("n2")
                    ),
                    Field(
                        "sn",
                        float,
                        20,
                        10,
                        kwargs.get("sn")
                    ),
                    Field(
                        "ss",
                        float,
                        30,
                        10,
                        kwargs.get("ss")
                    ),
                    Field(
                        "n",
                        float,
                        40,
                        10,
                        kwargs.get("n")
                    ),
                    Field(
                        "m",
                        float,
                        50,
                        10,
                        kwargs.get("m")
                    ),
                    Field(
                        "tf",
                        float,
                        60,
                        10,
                        kwargs.get("tf", 1.0E+20)
                    ),
                    Field(
                        "ep",
                        float,
                        70,
                        10,
                        kwargs.get("ep", 1.0E+20)
                    ),
                ],
            ),
        ]

    @property
    def wid(self) -> typing.Optional[int]:
        """Get or set the Optional weld ID
        """ # nopep8
        return self._cards[0].get_value("wid")

    @wid.setter
    def wid(self, value: int) -> None:
        self._cards[0].set_value("wid", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Node ID of node 1.
        """ # nopep8
        return self._cards[1].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        self._cards[1].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Node ID of node 2.
        """ # nopep8
        return self._cards[1].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        self._cards[1].set_value("n2", value)

    @property
    def sn(self) -> typing.Optional[float]:
        """Get or set the Normal force at spotweld failure (optional, see Remark 2 in user's manual).
        EQ.0.0:	the failure criteria is disabled
        GT.0.0:	normal force at spot weld failure
        LT.0.0:	curve ID which specifies the normal force at spot weld failure         as a function of the nodal temperature
        """ # nopep8
        return self._cards[1].get_value("sn")

    @sn.setter
    def sn(self, value: float) -> None:
        self._cards[1].set_value("sn", value)

    @property
    def ss(self) -> typing.Optional[float]:
        """Get or set the Shear force at spotweld failure (optional, see Remark 2 in user's manual).
        EQ.0.0:	the failure criteria is disabled
        GT.0.0:	shear force at spot weld failure
        LT.0.0:	curve ID which specifies the shear force at spot weld failure         as a function of the nodal temperature
        """ # nopep8
        return self._cards[1].get_value("ss")

    @ss.setter
    def ss(self, value: float) -> None:
        self._cards[1].set_value("ss", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Exponent for normal spotweld force (optional, see Remark 2 in user's manual).
        """ # nopep8
        return self._cards[1].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        self._cards[1].set_value("n", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Exponent for shear spotweld force (optional, see Remark 2 in user's manual).
        """ # nopep8
        return self._cards[1].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        self._cards[1].set_value("m", value)

    @property
    def tf(self) -> float:
        """Get or set the Failure time for nodal constraint set (default=1.0E+20).
        """ # nopep8
        return self._cards[1].get_value("tf")

    @tf.setter
    def tf(self, value: float) -> None:
        self._cards[1].set_value("tf", value)

    @property
    def ep(self) -> float:
        """Get or set the Effective plastic strain at failure (default=1.0E+20).
        """ # nopep8
        return self._cards[1].get_value("ep")

    @ep.setter
    def ep(self, value: float) -> None:
        self._cards[1].set_value("ep", value)

