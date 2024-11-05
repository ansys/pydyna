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

class AleSmoothing(KeywordBase):
    """DYNA ALE_SMOOTHING keyword"""

    keyword = "ALE"
    subkeyword = "SMOOTHING"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "dnid",
                        int,
                        0,
                        10,
                        kwargs.get("dnid")
                    ),
                    Field(
                        "nid1",
                        int,
                        10,
                        10,
                        kwargs.get("nid1")
                    ),
                    Field(
                        "nid2",
                        int,
                        20,
                        10,
                        kwargs.get("nid2")
                    ),
                    Field(
                        "ipre",
                        int,
                        30,
                        10,
                        kwargs.get("ipre", 0)
                    ),
                    Field(
                        "xco",
                        float,
                        40,
                        10,
                        kwargs.get("xco", 0.0)
                    ),
                    Field(
                        "yco",
                        float,
                        50,
                        10,
                        kwargs.get("yco", 0.0)
                    ),
                    Field(
                        "zco",
                        float,
                        60,
                        10,
                        kwargs.get("zco", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def dnid(self) -> typing.Optional[int]:
        """Get or set the Dependent node or node set ID:
        GT.0: DNID is an ALE node.
        EQ.0 : The dependent nodes are the nodes of an ALE mesh connected to the nodes in INID1.See Remark 2
        LT.0: -DNID is the ID of an ALE node set.See Remark 2
        """ # nopep8
        return self._cards[0].get_value("dnid")

    @dnid.setter
    def dnid(self, value: int) -> None:
        self._cards[0].set_value("dnid", value)

    @property
    def nid1(self) -> typing.Optional[int]:
        """Get or set the ID of first node or set for constraining the dependent nodes:
        GT.0: NID1 is a node.
        LT.0 : -NID1 is a segment set ID if XCO = YCO = ZCO = 0.0.
        Otherwise, -NID1 is a node set ID.See Remark 2
        """ # nopep8
        return self._cards[0].get_value("nid1")

    @nid1.setter
    def nid1(self, value: int) -> None:
        self._cards[0].set_value("nid1", value)

    @property
    def nid2(self) -> typing.Optional[int]:
        """Get or set the ID of second node or node set for constraining the dependent nodes:
        GT.0 : NID2 is a node.
        EQ.0 : The dependent node motion is solely controlled by
        NID1.See Remarks 2 and 3.
        LT.0 : -NID2 is a node set ID.See Remark 2.
        """ # nopep8
        return self._cards[0].get_value("nid2")

    @nid2.setter
    def nid2(self, value: int) -> None:
        self._cards[0].set_value("nid2", value)

    @property
    def ipre(self) -> int:
        """Get or set the EQ.0: smoothing constraints are performed after mesh relaxation,
        EQ.1: Smoothing constraints are performed before mesh relaxation.
        """ # nopep8
        return self._cards[0].get_value("ipre")

    @ipre.setter
    def ipre(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ipre must be one of {0,1}""")
        self._cards[0].set_value("ipre", value)

    @property
    def xco(self) -> float:
        """Get or set the x-coordinate of constraint vector.
        """ # nopep8
        return self._cards[0].get_value("xco")

    @xco.setter
    def xco(self, value: float) -> None:
        self._cards[0].set_value("xco", value)

    @property
    def yco(self) -> float:
        """Get or set the y-coordinate of constraint vector.
        """ # nopep8
        return self._cards[0].get_value("yco")

    @yco.setter
    def yco(self, value: float) -> None:
        self._cards[0].set_value("yco", value)

    @property
    def zco(self) -> float:
        """Get or set the z-coordinate of constraint vector.
        """ # nopep8
        return self._cards[0].get_value("zco")

    @zco.setter
    def zco(self, value: float) -> None:
        self._cards[0].set_value("zco", value)

