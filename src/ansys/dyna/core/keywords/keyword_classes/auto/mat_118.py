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

class Mat118(KeywordBase):
    """DYNA MAT_118 keyword"""

    keyword = "MAT"
    subkeyword = "118"
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
                ],
            ),
            Card(
                [
                    Field(
                        "c11",
                        float,
                        0,
                        10,
                        kwargs.get("c11")
                    ),
                    Field(
                        "c12",
                        float,
                        10,
                        10,
                        kwargs.get("c12")
                    ),
                    Field(
                        "c22",
                        float,
                        20,
                        10,
                        kwargs.get("c22")
                    ),
                    Field(
                        "c13",
                        float,
                        30,
                        10,
                        kwargs.get("c13")
                    ),
                    Field(
                        "c23",
                        float,
                        40,
                        10,
                        kwargs.get("c23")
                    ),
                    Field(
                        "c33",
                        float,
                        50,
                        10,
                        kwargs.get("c33")
                    ),
                    Field(
                        "c14",
                        float,
                        60,
                        10,
                        kwargs.get("c14")
                    ),
                    Field(
                        "c24",
                        float,
                        70,
                        10,
                        kwargs.get("c24")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "c34",
                        float,
                        0,
                        10,
                        kwargs.get("c34")
                    ),
                    Field(
                        "c44",
                        float,
                        10,
                        10,
                        kwargs.get("c44")
                    ),
                    Field(
                        "c15",
                        float,
                        20,
                        10,
                        kwargs.get("c15")
                    ),
                    Field(
                        "c25",
                        float,
                        30,
                        10,
                        kwargs.get("c25")
                    ),
                    Field(
                        "c35",
                        float,
                        40,
                        10,
                        kwargs.get("c35")
                    ),
                    Field(
                        "c45",
                        float,
                        50,
                        10,
                        kwargs.get("c45")
                    ),
                    Field(
                        "c55",
                        float,
                        60,
                        10,
                        kwargs.get("c55")
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
                        "c26",
                        float,
                        0,
                        10,
                        kwargs.get("c26")
                    ),
                    Field(
                        "c36",
                        float,
                        10,
                        10,
                        kwargs.get("c36")
                    ),
                    Field(
                        "c46",
                        float,
                        20,
                        10,
                        kwargs.get("c46")
                    ),
                    Field(
                        "c56",
                        float,
                        30,
                        10,
                        kwargs.get("c56")
                    ),
                    Field(
                        "c66",
                        float,
                        40,
                        10,
                        kwargs.get("c66")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat118.option_specs[0],
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
    def c11(self) -> typing.Optional[float]:
        """Get or set the C11 coefficient of the stiffness matrix.
        """ # nopep8
        return self._cards[1].get_value("c11")

    @c11.setter
    def c11(self, value: float) -> None:
        self._cards[1].set_value("c11", value)

    @property
    def c12(self) -> typing.Optional[float]:
        """Get or set the C12 coefficient of the stiffness matrix.
        """ # nopep8
        return self._cards[1].get_value("c12")

    @c12.setter
    def c12(self, value: float) -> None:
        self._cards[1].set_value("c12", value)

    @property
    def c22(self) -> typing.Optional[float]:
        """Get or set the C22 coefficient of the stiffness matrix.
        """ # nopep8
        return self._cards[1].get_value("c22")

    @c22.setter
    def c22(self, value: float) -> None:
        self._cards[1].set_value("c22", value)

    @property
    def c13(self) -> typing.Optional[float]:
        """Get or set the C13 coefficient of the stiffness matrix.
        """ # nopep8
        return self._cards[1].get_value("c13")

    @c13.setter
    def c13(self, value: float) -> None:
        self._cards[1].set_value("c13", value)

    @property
    def c23(self) -> typing.Optional[float]:
        """Get or set the C23 coefficient of the stiffness matrix.
        """ # nopep8
        return self._cards[1].get_value("c23")

    @c23.setter
    def c23(self, value: float) -> None:
        self._cards[1].set_value("c23", value)

    @property
    def c33(self) -> typing.Optional[float]:
        """Get or set the C33 coefficient of the stiffness matrix.
        """ # nopep8
        return self._cards[1].get_value("c33")

    @c33.setter
    def c33(self, value: float) -> None:
        self._cards[1].set_value("c33", value)

    @property
    def c14(self) -> typing.Optional[float]:
        """Get or set the C14 coefficient of the stiffness matrix.
        """ # nopep8
        return self._cards[1].get_value("c14")

    @c14.setter
    def c14(self, value: float) -> None:
        self._cards[1].set_value("c14", value)

    @property
    def c24(self) -> typing.Optional[float]:
        """Get or set the C24 coefficient of the stiffness matrix.
        """ # nopep8
        return self._cards[1].get_value("c24")

    @c24.setter
    def c24(self, value: float) -> None:
        self._cards[1].set_value("c24", value)

    @property
    def c34(self) -> typing.Optional[float]:
        """Get or set the C34 coefficient of the stiffness matrix.
        """ # nopep8
        return self._cards[2].get_value("c34")

    @c34.setter
    def c34(self, value: float) -> None:
        self._cards[2].set_value("c34", value)

    @property
    def c44(self) -> typing.Optional[float]:
        """Get or set the C44 coefficient of the stiffness matrix.
        """ # nopep8
        return self._cards[2].get_value("c44")

    @c44.setter
    def c44(self, value: float) -> None:
        self._cards[2].set_value("c44", value)

    @property
    def c15(self) -> typing.Optional[float]:
        """Get or set the C15 coefficient of the stiffness matrix.
        """ # nopep8
        return self._cards[2].get_value("c15")

    @c15.setter
    def c15(self, value: float) -> None:
        self._cards[2].set_value("c15", value)

    @property
    def c25(self) -> typing.Optional[float]:
        """Get or set the C25 coefficient of the stiffness matrix.
        """ # nopep8
        return self._cards[2].get_value("c25")

    @c25.setter
    def c25(self, value: float) -> None:
        self._cards[2].set_value("c25", value)

    @property
    def c35(self) -> typing.Optional[float]:
        """Get or set the C35 coefficient of the stiffness matrix.
        """ # nopep8
        return self._cards[2].get_value("c35")

    @c35.setter
    def c35(self, value: float) -> None:
        self._cards[2].set_value("c35", value)

    @property
    def c45(self) -> typing.Optional[float]:
        """Get or set the C45 coefficient of the stiffness matrix.
        """ # nopep8
        return self._cards[2].get_value("c45")

    @c45.setter
    def c45(self, value: float) -> None:
        self._cards[2].set_value("c45", value)

    @property
    def c55(self) -> typing.Optional[float]:
        """Get or set the C55 coefficient of the stiffness matrix.
        """ # nopep8
        return self._cards[2].get_value("c55")

    @c55.setter
    def c55(self, value: float) -> None:
        self._cards[2].set_value("c55", value)

    @property
    def c16(self) -> typing.Optional[float]:
        """Get or set the C16 coefficient of the stiffness matrix.
        """ # nopep8
        return self._cards[2].get_value("c16")

    @c16.setter
    def c16(self, value: float) -> None:
        self._cards[2].set_value("c16", value)

    @property
    def c26(self) -> typing.Optional[float]:
        """Get or set the C26 coefficient of the stiffness matrix.
        """ # nopep8
        return self._cards[3].get_value("c26")

    @c26.setter
    def c26(self, value: float) -> None:
        self._cards[3].set_value("c26", value)

    @property
    def c36(self) -> typing.Optional[float]:
        """Get or set the C36 coefficient of the stiffness matrix.
        """ # nopep8
        return self._cards[3].get_value("c36")

    @c36.setter
    def c36(self, value: float) -> None:
        self._cards[3].set_value("c36", value)

    @property
    def c46(self) -> typing.Optional[float]:
        """Get or set the C46 coefficient of the stiffness matrix.
        """ # nopep8
        return self._cards[3].get_value("c46")

    @c46.setter
    def c46(self, value: float) -> None:
        self._cards[3].set_value("c46", value)

    @property
    def c56(self) -> typing.Optional[float]:
        """Get or set the C56 coefficient of the stiffness matrix.
        """ # nopep8
        return self._cards[3].get_value("c56")

    @c56.setter
    def c56(self, value: float) -> None:
        self._cards[3].set_value("c56", value)

    @property
    def c66(self) -> typing.Optional[float]:
        """Get or set the C66 coefficient of the stiffness matrix.
        """ # nopep8
        return self._cards[3].get_value("c66")

    @c66.setter
    def c66(self, value: float) -> None:
        self._cards[3].set_value("c66", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[4].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[4].cards[0].set_value("title", value)

