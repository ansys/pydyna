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

class DefineDeathTimesRigid(KeywordBase):
    """DYNA DEFINE_DEATH_TIMES_RIGID keyword"""

    keyword = "DEFINE"
    subkeyword = "DEATH_TIMES_RIGID"
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
            OptionCardSet(
                option_spec = DefineDeathTimesRigid.option_specs[0],
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
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

