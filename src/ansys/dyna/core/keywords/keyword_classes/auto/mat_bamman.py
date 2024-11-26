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
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class MatBamman(KeywordBase):
    """DYNA MAT_BAMMAN keyword"""

    keyword = "MAT"
    subkeyword = "BAMMAN"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "mid",
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "e",
                        float,
                        20,
                        10,
                        kwargs.get("e")
                    ),
                    Field(
                        "pr",
                        float,
                        30,
                        10,
                        kwargs.get("pr")
                    ),
                    Field(
                        "t",
                        float,
                        40,
                        10,
                        kwargs.get("t")
                    ),
                    Field(
                        "hc",
                        float,
                        50,
                        10,
                        kwargs.get("hc")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c1",
                        float,
                        0,
                        10,
                        kwargs.get("c1")
                    ),
                    Field(
                        "c2",
                        float,
                        10,
                        10,
                        kwargs.get("c2")
                    ),
                    Field(
                        "c3",
                        float,
                        20,
                        10,
                        kwargs.get("c3")
                    ),
                    Field(
                        "c4",
                        float,
                        30,
                        10,
                        kwargs.get("c4")
                    ),
                    Field(
                        "c5",
                        float,
                        40,
                        10,
                        kwargs.get("c5")
                    ),
                    Field(
                        "c6",
                        float,
                        50,
                        10,
                        kwargs.get("c6")
                    ),
                    Field(
                        "c7",
                        float,
                        60,
                        10,
                        kwargs.get("c7")
                    ),
                    Field(
                        "c8",
                        float,
                        70,
                        10,
                        kwargs.get("c8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c9",
                        float,
                        0,
                        10,
                        kwargs.get("c9")
                    ),
                    Field(
                        "c10",
                        float,
                        10,
                        10,
                        kwargs.get("c10")
                    ),
                    Field(
                        "c11",
                        float,
                        20,
                        10,
                        kwargs.get("c11")
                    ),
                    Field(
                        "c12",
                        float,
                        30,
                        10,
                        kwargs.get("c12")
                    ),
                    Field(
                        "c13",
                        float,
                        40,
                        10,
                        kwargs.get("c13")
                    ),
                    Field(
                        "c14",
                        float,
                        50,
                        10,
                        kwargs.get("c14")
                    ),
                    Field(
                        "c15",
                        float,
                        60,
                        10,
                        kwargs.get("c15")
                    ),
                    Field(
                        "c16",
                        float,
                        70,
                        10,
                        kwargs.get("c16")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c17",
                        float,
                        0,
                        10,
                        kwargs.get("c17")
                    ),
                    Field(
                        "c18",
                        float,
                        10,
                        10,
                        kwargs.get("c18")
                    ),
                    Field(
                        "a1",
                        float,
                        20,
                        10,
                        kwargs.get("a1")
                    ),
                    Field(
                        "a2",
                        float,
                        30,
                        10,
                        kwargs.get("a2")
                    ),
                    Field(
                        "a3",
                        float,
                        40,
                        10,
                        kwargs.get("a3")
                    ),
                    Field(
                        "a4",
                        float,
                        50,
                        10,
                        kwargs.get("a4")
                    ),
                    Field(
                        "a5",
                        float,
                        60,
                        10,
                        kwargs.get("a5")
                    ),
                    Field(
                        "a6",
                        float,
                        70,
                        10,
                        kwargs.get("a6")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatBamman.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]

    @property
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Young's modulus (psi).
        """ # nopep8
        return self._cards[0].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        self._cards[0].set_value("e", value)

    @property
    def pr(self) -> typing.Optional[float]:
        """Get or set the Poisson's ratio.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: float) -> None:
        self._cards[0].set_value("pr", value)

    @property
    def t(self) -> typing.Optional[float]:
        """Get or set the Initial temperature (R').
        """ # nopep8
        return self._cards[0].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        self._cards[0].set_value("t", value)

    @property
    def hc(self) -> typing.Optional[float]:
        """Get or set the HC Heat generation coefficient (R'/psi)
        """ # nopep8
        return self._cards[0].get_value("hc")

    @hc.setter
    def hc(self, value: float) -> None:
        self._cards[0].set_value("hc", value)

    @property
    def c1(self) -> typing.Optional[float]:
        """Get or set the Psi
        """ # nopep8
        return self._cards[1].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        self._cards[1].set_value("c1", value)

    @property
    def c2(self) -> typing.Optional[float]:
        """Get or set the R'
        """ # nopep8
        return self._cards[1].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        self._cards[1].set_value("c2", value)

    @property
    def c3(self) -> typing.Optional[float]:
        """Get or set the Psi
        """ # nopep8
        return self._cards[1].get_value("c3")

    @c3.setter
    def c3(self, value: float) -> None:
        self._cards[1].set_value("c3", value)

    @property
    def c4(self) -> typing.Optional[float]:
        """Get or set the R'
        """ # nopep8
        return self._cards[1].get_value("c4")

    @c4.setter
    def c4(self, value: float) -> None:
        self._cards[1].set_value("c4", value)

    @property
    def c5(self) -> typing.Optional[float]:
        """Get or set the 1/s
        """ # nopep8
        return self._cards[1].get_value("c5")

    @c5.setter
    def c5(self, value: float) -> None:
        self._cards[1].set_value("c5", value)

    @property
    def c6(self) -> typing.Optional[float]:
        """Get or set the R'
        """ # nopep8
        return self._cards[1].get_value("c6")

    @c6.setter
    def c6(self, value: float) -> None:
        self._cards[1].set_value("c6", value)

    @property
    def c7(self) -> typing.Optional[float]:
        """Get or set the 1/psi
        """ # nopep8
        return self._cards[1].get_value("c7")

    @c7.setter
    def c7(self, value: float) -> None:
        self._cards[1].set_value("c7", value)

    @property
    def c8(self) -> typing.Optional[float]:
        """Get or set the R'
        """ # nopep8
        return self._cards[1].get_value("c8")

    @c8.setter
    def c8(self, value: float) -> None:
        self._cards[1].set_value("c8", value)

    @property
    def c9(self) -> typing.Optional[float]:
        """Get or set the psi
        """ # nopep8
        return self._cards[2].get_value("c9")

    @c9.setter
    def c9(self, value: float) -> None:
        self._cards[2].set_value("c9", value)

    @property
    def c10(self) -> typing.Optional[float]:
        """Get or set the R'
        """ # nopep8
        return self._cards[2].get_value("c10")

    @c10.setter
    def c10(self, value: float) -> None:
        self._cards[2].set_value("c10", value)

    @property
    def c11(self) -> typing.Optional[float]:
        """Get or set the 1/psi-s
        """ # nopep8
        return self._cards[2].get_value("c11")

    @c11.setter
    def c11(self, value: float) -> None:
        self._cards[2].set_value("c11", value)

    @property
    def c12(self) -> typing.Optional[float]:
        """Get or set the R'
        """ # nopep8
        return self._cards[2].get_value("c12")

    @c12.setter
    def c12(self, value: float) -> None:
        self._cards[2].set_value("c12", value)

    @property
    def c13(self) -> typing.Optional[float]:
        """Get or set the 1/psi
        """ # nopep8
        return self._cards[2].get_value("c13")

    @c13.setter
    def c13(self, value: float) -> None:
        self._cards[2].set_value("c13", value)

    @property
    def c14(self) -> typing.Optional[float]:
        """Get or set the R'
        """ # nopep8
        return self._cards[2].get_value("c14")

    @c14.setter
    def c14(self, value: float) -> None:
        self._cards[2].set_value("c14", value)

    @property
    def c15(self) -> typing.Optional[float]:
        """Get or set the psi
        """ # nopep8
        return self._cards[2].get_value("c15")

    @c15.setter
    def c15(self, value: float) -> None:
        self._cards[2].set_value("c15", value)

    @property
    def c16(self) -> typing.Optional[float]:
        """Get or set the R'
        """ # nopep8
        return self._cards[2].get_value("c16")

    @c16.setter
    def c16(self, value: float) -> None:
        self._cards[2].set_value("c16", value)

    @property
    def c17(self) -> typing.Optional[float]:
        """Get or set the 1/psi-s
        """ # nopep8
        return self._cards[3].get_value("c17")

    @c17.setter
    def c17(self, value: float) -> None:
        self._cards[3].set_value("c17", value)

    @property
    def c18(self) -> typing.Optional[float]:
        """Get or set the R'
        """ # nopep8
        return self._cards[3].get_value("c18")

    @c18.setter
    def c18(self, value: float) -> None:
        self._cards[3].set_value("c18", value)

    @property
    def a1(self) -> typing.Optional[float]:
        """Get or set the alpha-1, initial value of internal state variable 1.
        """ # nopep8
        return self._cards[3].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        self._cards[3].set_value("a1", value)

    @property
    def a2(self) -> typing.Optional[float]:
        """Get or set the alpha-2, initial value of internal state variable 2.
        """ # nopep8
        return self._cards[3].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        self._cards[3].set_value("a2", value)

    @property
    def a3(self) -> typing.Optional[float]:
        """Get or set the alpha-3, initial value of internal state variable 3.
        """ # nopep8
        return self._cards[3].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        self._cards[3].set_value("a3", value)

    @property
    def a4(self) -> typing.Optional[float]:
        """Get or set the alpha-4, initial value of internal state variable 4.
        """ # nopep8
        return self._cards[3].get_value("a4")

    @a4.setter
    def a4(self, value: float) -> None:
        self._cards[3].set_value("a4", value)

    @property
    def a5(self) -> typing.Optional[float]:
        """Get or set the alpha-5, initial value of internal state variable 5.
        """ # nopep8
        return self._cards[3].get_value("a5")

    @a5.setter
    def a5(self, value: float) -> None:
        self._cards[3].set_value("a5", value)

    @property
    def a6(self) -> typing.Optional[float]:
        """Get or set the alpha-6, initial value of internal state variable 6.
        """ # nopep8
        return self._cards[3].get_value("a6")

    @a6.setter
    def a6(self, value: float) -> None:
        self._cards[3].set_value("a6", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

