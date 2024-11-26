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

class Eos005(KeywordBase):
    """DYNA EOS_005 keyword"""

    keyword = "EOS"
    subkeyword = "005"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eosid",
                        int,
                        0,
                        10,
                        kwargs.get("eosid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a10",
                        float,
                        0,
                        20,
                        kwargs.get("a10", 0.0)
                    ),
                    Field(
                        "a11",
                        float,
                        20,
                        20,
                        kwargs.get("a11", 0.0)
                    ),
                    Field(
                        "a12",
                        float,
                        40,
                        20,
                        kwargs.get("a12", 0.0)
                    ),
                    Field(
                        "a13",
                        float,
                        60,
                        20,
                        kwargs.get("a13", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a20",
                        float,
                        0,
                        20,
                        kwargs.get("a20", 0.0)
                    ),
                    Field(
                        "a21",
                        float,
                        20,
                        20,
                        kwargs.get("a21", 0.0)
                    ),
                    Field(
                        "a22",
                        float,
                        40,
                        20,
                        kwargs.get("a22", 0.0)
                    ),
                    Field(
                        "a23",
                        float,
                        60,
                        20,
                        kwargs.get("a23", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a30",
                        float,
                        0,
                        20,
                        kwargs.get("a30", 0.0)
                    ),
                    Field(
                        "a31",
                        float,
                        20,
                        20,
                        kwargs.get("a31", 0.0)
                    ),
                    Field(
                        "a32",
                        float,
                        40,
                        20,
                        kwargs.get("a32", 0.0)
                    ),
                    Field(
                        "a33",
                        float,
                        60,
                        20,
                        kwargs.get("a33", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a40",
                        float,
                        0,
                        20,
                        kwargs.get("a40", 0.0)
                    ),
                    Field(
                        "a41",
                        float,
                        20,
                        20,
                        kwargs.get("a41", 0.0)
                    ),
                    Field(
                        "a42",
                        float,
                        40,
                        20,
                        kwargs.get("a42", 0.0)
                    ),
                    Field(
                        "a43",
                        float,
                        60,
                        20,
                        kwargs.get("a43", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a50",
                        float,
                        0,
                        20,
                        kwargs.get("a50", 0.0)
                    ),
                    Field(
                        "a51",
                        float,
                        20,
                        20,
                        kwargs.get("a51", 0.0)
                    ),
                    Field(
                        "a52",
                        float,
                        40,
                        20,
                        kwargs.get("a52", 0.0)
                    ),
                    Field(
                        "a53",
                        float,
                        60,
                        20,
                        kwargs.get("a53", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a60",
                        float,
                        0,
                        20,
                        kwargs.get("a60", 0.0)
                    ),
                    Field(
                        "a61",
                        float,
                        20,
                        20,
                        kwargs.get("a61", 0.0)
                    ),
                    Field(
                        "a62",
                        float,
                        40,
                        20,
                        kwargs.get("a62", 0.0)
                    ),
                    Field(
                        "a63",
                        float,
                        60,
                        20,
                        kwargs.get("a63", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a70",
                        float,
                        0,
                        20,
                        kwargs.get("a70", 0.0)
                    ),
                    Field(
                        "a71",
                        float,
                        20,
                        20,
                        kwargs.get("a71", 0.0)
                    ),
                    Field(
                        "a72",
                        float,
                        40,
                        20,
                        kwargs.get("a72", 0.0)
                    ),
                    Field(
                        "a73",
                        float,
                        60,
                        20,
                        kwargs.get("a73", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "a14",
                        float,
                        0,
                        20,
                        kwargs.get("a14")
                    ),
                    Field(
                        "a24",
                        float,
                        20,
                        20,
                        kwargs.get("a24")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "alph",
                        float,
                        0,
                        20,
                        kwargs.get("alph")
                    ),
                    Field(
                        "beta",
                        float,
                        20,
                        20,
                        kwargs.get("beta")
                    ),
                    Field(
                        "e0",
                        float,
                        40,
                        20,
                        kwargs.get("e0")
                    ),
                    Field(
                        "v0",
                        float,
                        60,
                        20,
                        kwargs.get("v0")
                    ),
                ],
            ),
        ]

    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state ID.
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        self._cards[0].set_value("eosid", value)

    @property
    def a10(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[1].get_value("a10")

    @a10.setter
    def a10(self, value: float) -> None:
        self._cards[1].set_value("a10", value)

    @property
    def a11(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[1].get_value("a11")

    @a11.setter
    def a11(self, value: float) -> None:
        self._cards[1].set_value("a11", value)

    @property
    def a12(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[1].get_value("a12")

    @a12.setter
    def a12(self, value: float) -> None:
        self._cards[1].set_value("a12", value)

    @property
    def a13(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[1].get_value("a13")

    @a13.setter
    def a13(self, value: float) -> None:
        self._cards[1].set_value("a13", value)

    @property
    def a20(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[2].get_value("a20")

    @a20.setter
    def a20(self, value: float) -> None:
        self._cards[2].set_value("a20", value)

    @property
    def a21(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[2].get_value("a21")

    @a21.setter
    def a21(self, value: float) -> None:
        self._cards[2].set_value("a21", value)

    @property
    def a22(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[2].get_value("a22")

    @a22.setter
    def a22(self, value: float) -> None:
        self._cards[2].set_value("a22", value)

    @property
    def a23(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[2].get_value("a23")

    @a23.setter
    def a23(self, value: float) -> None:
        self._cards[2].set_value("a23", value)

    @property
    def a30(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[3].get_value("a30")

    @a30.setter
    def a30(self, value: float) -> None:
        self._cards[3].set_value("a30", value)

    @property
    def a31(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[3].get_value("a31")

    @a31.setter
    def a31(self, value: float) -> None:
        self._cards[3].set_value("a31", value)

    @property
    def a32(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[3].get_value("a32")

    @a32.setter
    def a32(self, value: float) -> None:
        self._cards[3].set_value("a32", value)

    @property
    def a33(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[3].get_value("a33")

    @a33.setter
    def a33(self, value: float) -> None:
        self._cards[3].set_value("a33", value)

    @property
    def a40(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[4].get_value("a40")

    @a40.setter
    def a40(self, value: float) -> None:
        self._cards[4].set_value("a40", value)

    @property
    def a41(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[4].get_value("a41")

    @a41.setter
    def a41(self, value: float) -> None:
        self._cards[4].set_value("a41", value)

    @property
    def a42(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[4].get_value("a42")

    @a42.setter
    def a42(self, value: float) -> None:
        self._cards[4].set_value("a42", value)

    @property
    def a43(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[4].get_value("a43")

    @a43.setter
    def a43(self, value: float) -> None:
        self._cards[4].set_value("a43", value)

    @property
    def a50(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[5].get_value("a50")

    @a50.setter
    def a50(self, value: float) -> None:
        self._cards[5].set_value("a50", value)

    @property
    def a51(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[5].get_value("a51")

    @a51.setter
    def a51(self, value: float) -> None:
        self._cards[5].set_value("a51", value)

    @property
    def a52(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[5].get_value("a52")

    @a52.setter
    def a52(self, value: float) -> None:
        self._cards[5].set_value("a52", value)

    @property
    def a53(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[5].get_value("a53")

    @a53.setter
    def a53(self, value: float) -> None:
        self._cards[5].set_value("a53", value)

    @property
    def a60(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[6].get_value("a60")

    @a60.setter
    def a60(self, value: float) -> None:
        self._cards[6].set_value("a60", value)

    @property
    def a61(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[6].get_value("a61")

    @a61.setter
    def a61(self, value: float) -> None:
        self._cards[6].set_value("a61", value)

    @property
    def a62(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[6].get_value("a62")

    @a62.setter
    def a62(self, value: float) -> None:
        self._cards[6].set_value("a62", value)

    @property
    def a63(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[6].get_value("a63")

    @a63.setter
    def a63(self, value: float) -> None:
        self._cards[6].set_value("a63", value)

    @property
    def a70(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[7].get_value("a70")

    @a70.setter
    def a70(self, value: float) -> None:
        self._cards[7].set_value("a70", value)

    @property
    def a71(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[7].get_value("a71")

    @a71.setter
    def a71(self, value: float) -> None:
        self._cards[7].set_value("a71", value)

    @property
    def a72(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[7].get_value("a72")

    @a72.setter
    def a72(self, value: float) -> None:
        self._cards[7].set_value("a72", value)

    @property
    def a73(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[7].get_value("a73")

    @a73.setter
    def a73(self, value: float) -> None:
        self._cards[7].set_value("a73", value)

    @property
    def a14(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[8].get_value("a14")

    @a14.setter
    def a14(self, value: float) -> None:
        self._cards[8].set_value("a14", value)

    @property
    def a24(self) -> typing.Optional[float]:
        """Get or set the 
        """ # nopep8
        return self._cards[8].get_value("a24")

    @a24.setter
    def a24(self, value: float) -> None:
        self._cards[8].set_value("a24", value)

    @property
    def alph(self) -> typing.Optional[float]:
        """Get or set the alpha
        """ # nopep8
        return self._cards[9].get_value("alph")

    @alph.setter
    def alph(self, value: float) -> None:
        self._cards[9].set_value("alph", value)

    @property
    def beta(self) -> typing.Optional[float]:
        """Get or set the beta
        """ # nopep8
        return self._cards[9].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        self._cards[9].set_value("beta", value)

    @property
    def e0(self) -> typing.Optional[float]:
        """Get or set the Initial internal energy.
        """ # nopep8
        return self._cards[9].get_value("e0")

    @e0.setter
    def e0(self, value: float) -> None:
        self._cards[9].set_value("e0", value)

    @property
    def v0(self) -> typing.Optional[float]:
        """Get or set the Initial relative volume.
        """ # nopep8
        return self._cards[9].get_value("v0")

    @v0.setter
    def v0(self, value: float) -> None:
        self._cards[9].set_value("v0", value)

