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

class ControlCoarsen(KeywordBase):
    """DYNA CONTROL_COARSEN keyword"""

    keyword = "CONTROL"
    subkeyword = "COARSEN"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "icoarse",
                        int,
                        0,
                        10,
                        kwargs.get("icoarse", 0)
                    ),
                    Field(
                        "angle",
                        float,
                        10,
                        10,
                        kwargs.get("angle")
                    ),
                    Field(
                        "nseed",
                        int,
                        20,
                        10,
                        kwargs.get("nseed", 0)
                    ),
                    Field(
                        "psid",
                        int,
                        30,
                        10,
                        kwargs.get("psid")
                    ),
                    Field(
                        "smax",
                        float,
                        40,
                        10,
                        kwargs.get("smax")
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
                        kwargs.get("n1", 0)
                    ),
                    Field(
                        "n2",
                        int,
                        10,
                        10,
                        kwargs.get("n2", 0)
                    ),
                    Field(
                        "n3",
                        int,
                        20,
                        10,
                        kwargs.get("n3", 0)
                    ),
                    Field(
                        "n4",
                        int,
                        30,
                        10,
                        kwargs.get("n4", 0)
                    ),
                    Field(
                        "n5",
                        int,
                        40,
                        10,
                        kwargs.get("n5", 0)
                    ),
                    Field(
                        "n6",
                        int,
                        50,
                        10,
                        kwargs.get("n6", 0)
                    ),
                    Field(
                        "n7",
                        int,
                        60,
                        10,
                        kwargs.get("n7", 0)
                    ),
                    Field(
                        "n8",
                        int,
                        70,
                        10,
                        kwargs.get("n8", 0)
                    ),
                ],
            ),
        ]

    @property
    def icoarse(self) -> int:
        """Get or set the Coarsening flag:
        EQ.0: Do not coarsen (default),
        EQ.1: Coarsen mesh at beginning of simulation.
        EQ.2: Coarsen mesh at beginning of simulation for forming model
        """ # nopep8
        return self._cards[0].get_value("icoarse")

    @icoarse.setter
    def icoarse(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""icoarse must be one of {0,1,2}""")
        self._cards[0].set_value("icoarse", value)

    @property
    def angle(self) -> typing.Optional[float]:
        """Get or set the Allowable angle change between neighboring elements. Adjacent elements which are flat to within ANGLE degrees are merged.
        Suggested starting value = 8.0 degrees.
        """ # nopep8
        return self._cards[0].get_value("angle")

    @angle.setter
    def angle(self, value: float) -> None:
        self._cards[0].set_value("angle", value)

    @property
    def nseed(self) -> int:
        """Get or set the Number of seed nodes (optional).
        EQ.0: use only automatic searching (default).
        EQ.n: also search starting with node IDs given below (maximum = 8 nodes).
        """ # nopep8
        return self._cards[0].get_value("nseed")

    @nseed.setter
    def nseed(self, value: int) -> None:
        self._cards[0].set_value("nseed", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part set ID. all the parts defined in this set will be prevented from been coarsened.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[0].set_value("psid", value)

    @property
    def smax(self) -> typing.Optional[float]:
        """Get or set the Max element size. For ICOARSE=2 no elements larger then this size will be created.
        """ # nopep8
        return self._cards[0].get_value("smax")

    @smax.setter
    def smax(self, value: float) -> None:
        self._cards[0].set_value("smax", value)

    @property
    def n1(self) -> int:
        """Get or set the Optional list of seed node IDs for extra searching.
        """ # nopep8
        return self._cards[1].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        self._cards[1].set_value("n1", value)

    @property
    def n2(self) -> int:
        """Get or set the Optional list of seed node IDs for extra searching.
        """ # nopep8
        return self._cards[1].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        self._cards[1].set_value("n2", value)

    @property
    def n3(self) -> int:
        """Get or set the Optional list of seed node IDs for extra searching.
        """ # nopep8
        return self._cards[1].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        self._cards[1].set_value("n3", value)

    @property
    def n4(self) -> int:
        """Get or set the Optional list of seed node IDs for extra searching.
        """ # nopep8
        return self._cards[1].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        self._cards[1].set_value("n4", value)

    @property
    def n5(self) -> int:
        """Get or set the Optional list of seed node IDs for extra searching.
        """ # nopep8
        return self._cards[1].get_value("n5")

    @n5.setter
    def n5(self, value: int) -> None:
        self._cards[1].set_value("n5", value)

    @property
    def n6(self) -> int:
        """Get or set the Optional list of seed node IDs for extra searching.
        """ # nopep8
        return self._cards[1].get_value("n6")

    @n6.setter
    def n6(self, value: int) -> None:
        self._cards[1].set_value("n6", value)

    @property
    def n7(self) -> int:
        """Get or set the Optional list of seed node IDs for extra searching.
        """ # nopep8
        return self._cards[1].get_value("n7")

    @n7.setter
    def n7(self, value: int) -> None:
        self._cards[1].set_value("n7", value)

    @property
    def n8(self) -> int:
        """Get or set the Optional list of seed node IDs for extra searching.
        """ # nopep8
        return self._cards[1].get_value("n8")

    @n8.setter
    def n8(self, value: int) -> None:
        self._cards[1].set_value("n8", value)

