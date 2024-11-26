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

class DefineDeathTimesSet(KeywordBase):
    """DYNA DEFINE_DEATH_TIMES_SET keyword"""

    keyword = "DEFINE"
    subkeyword = "DEATH_TIMES_SET"
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
                        "geo",
                        int,
                        0,
                        10,
                        kwargs.get("geo")
                    ),
                    Field(
                        "n1",
                        int,
                        10,
                        10,
                        kwargs.get("n1")
                    ),
                    Field(
                        "n2",
                        int,
                        20,
                        10,
                        kwargs.get("n2")
                    ),
                    Field(
                        "n3",
                        int,
                        30,
                        10,
                        kwargs.get("n3")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "x_t",
                        float,
                        0,
                        10,
                        kwargs.get("x_t")
                    ),
                    Field(
                        "y_t",
                        float,
                        10,
                        10,
                        kwargs.get("y_t")
                    ),
                    Field(
                        "z_t",
                        float,
                        20,
                        10,
                        kwargs.get("z_t")
                    ),
                    Field(
                        "x_h",
                        float,
                        30,
                        10,
                        kwargs.get("x_h")
                    ),
                    Field(
                        "y_h",
                        float,
                        40,
                        10,
                        kwargs.get("y_h")
                    ),
                    Field(
                        "x_h",
                        float,
                        50,
                        10,
                        kwargs.get("x_h")
                    ),
                    Field(
                        "r",
                        float,
                        60,
                        10,
                        kwargs.get("r")
                    ),
                    Field(
                        "flag",
                        int,
                        70,
                        10,
                        kwargs.get("flag")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sid1",
                        int,
                        0,
                        10,
                        kwargs.get("sid1")
                    ),
                    Field(
                        "sid2",
                        int,
                        10,
                        10,
                        kwargs.get("sid2")
                    ),
                    Field(
                        "sid3",
                        int,
                        20,
                        10,
                        kwargs.get("sid3")
                    ),
                    Field(
                        "sid4",
                        int,
                        30,
                        10,
                        kwargs.get("sid4")
                    ),
                    Field(
                        "sid5",
                        int,
                        40,
                        10,
                        kwargs.get("sid5")
                    ),
                    Field(
                        "sid6",
                        int,
                        50,
                        10,
                        kwargs.get("sid6")
                    ),
                    Field(
                        "sid7",
                        int,
                        60,
                        10,
                        kwargs.get("sid7")
                    ),
                    Field(
                        "sid8",
                        int,
                        70,
                        10,
                        kwargs.get("sid8")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineDeathTimesSet.option_specs[0],
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
    def geo(self) -> typing.Optional[int]:
        """Get or set the Geometric entity type. =1 plane, =2 infinite cylinder, =3 sphere
        """ # nopep8
        return self._cards[0].get_value("geo")

    @geo.setter
    def geo(self, value: int) -> None:
        self._cards[0].set_value("geo", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Node defining the origin of the geometric entity (optional).
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        self._cards[0].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Node defining the tail of the orientation vector (optional).
        """ # nopep8
        return self._cards[0].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        self._cards[0].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Node defining the head of the orientation vector (optional).
        """ # nopep8
        return self._cards[0].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        self._cards[0].set_value("n3", value)

    @property
    def x_t(self) -> typing.Optional[float]:
        """Get or set the X coordinate of the origin of the geometric entity and the tail of the orientation vector
        """ # nopep8
        return self._cards[1].get_value("x_t")

    @x_t.setter
    def x_t(self, value: float) -> None:
        self._cards[1].set_value("x_t", value)

    @property
    def y_t(self) -> typing.Optional[float]:
        """Get or set the Y coordinate of the origin of the geometric entity and the tail of the orientation vector.
        """ # nopep8
        return self._cards[1].get_value("y_t")

    @y_t.setter
    def y_t(self, value: float) -> None:
        self._cards[1].set_value("y_t", value)

    @property
    def z_t(self) -> typing.Optional[float]:
        """Get or set the Z coordinate of the origin of the geometric entity and the tail of the orientation vector
        """ # nopep8
        return self._cards[1].get_value("z_t")

    @z_t.setter
    def z_t(self, value: float) -> None:
        self._cards[1].set_value("z_t", value)

    @property
    def x_h(self) -> typing.Optional[float]:
        """Get or set the X coordinate of the head of the orientation vector.
        """ # nopep8
        return self._cards[1].get_value("x_h")

    @x_h.setter
    def x_h(self, value: float) -> None:
        self._cards[1].set_value("x_h", value)

    @property
    def y_h(self) -> typing.Optional[float]:
        """Get or set the Y coordinate of the head of the orientation vector.
        """ # nopep8
        return self._cards[1].get_value("y_h")

    @y_h.setter
    def y_h(self, value: float) -> None:
        self._cards[1].set_value("y_h", value)

    @property
    def x_h(self) -> typing.Optional[float]:
        """Get or set the Z coordinate of the head of the orientation vector
        """ # nopep8
        return self._cards[1].get_value("x_h")

    @x_h.setter
    def x_h(self, value: float) -> None:
        self._cards[1].set_value("x_h", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Radius of cylinder or sphere.
        """ # nopep8
        return self._cards[1].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        self._cards[1].set_value("r", value)

    @property
    def flag(self) -> typing.Optional[int]:
        """Get or set the +1 for killing motion when the node is outside of the geometric entity or on the positive side of the plane as defined by the normal direction, or -1 for the inside.
        """ # nopep8
        return self._cards[1].get_value("flag")

    @flag.setter
    def flag(self, value: int) -> None:
        self._cards[1].set_value("flag", value)

    @property
    def sid1(self) -> typing.Optional[int]:
        """Get or set the First node set ID
        """ # nopep8
        return self._cards[2].get_value("sid1")

    @sid1.setter
    def sid1(self, value: int) -> None:
        self._cards[2].set_value("sid1", value)

    @property
    def sid2(self) -> typing.Optional[int]:
        """Get or set the Second node set ID.
        """ # nopep8
        return self._cards[2].get_value("sid2")

    @sid2.setter
    def sid2(self, value: int) -> None:
        self._cards[2].set_value("sid2", value)

    @property
    def sid3(self) -> typing.Optional[int]:
        """Get or set the Third node set ID
        """ # nopep8
        return self._cards[2].get_value("sid3")

    @sid3.setter
    def sid3(self, value: int) -> None:
        self._cards[2].set_value("sid3", value)

    @property
    def sid4(self) -> typing.Optional[int]:
        """Get or set the Fourth node set ID.
        """ # nopep8
        return self._cards[2].get_value("sid4")

    @sid4.setter
    def sid4(self, value: int) -> None:
        self._cards[2].set_value("sid4", value)

    @property
    def sid5(self) -> typing.Optional[int]:
        """Get or set the Fifth node set ID
        """ # nopep8
        return self._cards[2].get_value("sid5")

    @sid5.setter
    def sid5(self, value: int) -> None:
        self._cards[2].set_value("sid5", value)

    @property
    def sid6(self) -> typing.Optional[int]:
        """Get or set the Sixth node set ID
        """ # nopep8
        return self._cards[2].get_value("sid6")

    @sid6.setter
    def sid6(self, value: int) -> None:
        self._cards[2].set_value("sid6", value)

    @property
    def sid7(self) -> typing.Optional[int]:
        """Get or set the Seventh node set ID.
        """ # nopep8
        return self._cards[2].get_value("sid7")

    @sid7.setter
    def sid7(self, value: int) -> None:
        self._cards[2].set_value("sid7", value)

    @property
    def sid8(self) -> typing.Optional[int]:
        """Get or set the Eighth node set ID.
        """ # nopep8
        return self._cards[2].get_value("sid8")

    @sid8.setter
    def sid8(self, value: int) -> None:
        self._cards[2].set_value("sid8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

