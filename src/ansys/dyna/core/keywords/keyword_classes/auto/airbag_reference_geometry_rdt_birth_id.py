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

class AirbagReferenceGeometryRdtBirthId(KeywordBase):
    """DYNA AIRBAG_REFERENCE_GEOMETRY_RDT_BIRTH_ID keyword"""

    keyword = "AIRBAG"
    subkeyword = "REFERENCE_GEOMETRY_RDT_BIRTH_ID"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id")
                    ),
                    Field(
                        "sx",
                        float,
                        10,
                        10,
                        kwargs.get("sx")
                    ),
                    Field(
                        "sy",
                        float,
                        20,
                        10,
                        kwargs.get("sy")
                    ),
                    Field(
                        "sz",
                        float,
                        30,
                        10,
                        kwargs.get("sz")
                    ),
                    Field(
                        "nido",
                        int,
                        40,
                        10,
                        kwargs.get("nido")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "birth",
                        float,
                        0,
                        10,
                        kwargs.get("birth", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nid",
                        int,
                        0,
                        8,
                        kwargs.get("nid")
                    ),
                    Field(
                        "x",
                        float,
                        8,
                        16,
                        kwargs.get("x", 0.0)
                    ),
                    Field(
                        "y",
                        float,
                        24,
                        16,
                        kwargs.get("y", 0.0)
                    ),
                    Field(
                        "z",
                        float,
                        40,
                        16,
                        kwargs.get("z", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Card ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def sx(self) -> typing.Optional[float]:
        """Get or set the Scale factor for X direction.
        """ # nopep8
        return self._cards[0].get_value("sx")

    @sx.setter
    def sx(self, value: float) -> None:
        self._cards[0].set_value("sx", value)

    @property
    def sy(self) -> typing.Optional[float]:
        """Get or set the Scale factor for Y direction.
        """ # nopep8
        return self._cards[0].get_value("sy")

    @sy.setter
    def sy(self, value: float) -> None:
        self._cards[0].set_value("sy", value)

    @property
    def sz(self) -> typing.Optional[float]:
        """Get or set the Scale factor for Z direction.
        """ # nopep8
        return self._cards[0].get_value("sz")

    @sz.setter
    def sz(self, value: float) -> None:
        self._cards[0].set_value("sz", value)

    @property
    def nido(self) -> typing.Optional[int]:
        """Get or set the Node ID for origin. Default is the first node ID defined in this keyword.
        """ # nopep8
        return self._cards[0].get_value("nido")

    @nido.setter
    def nido(self, value: int) -> None:
        self._cards[0].set_value("nido", value)

    @property
    def birth(self) -> float:
        """Get or set the Time at which the reference geometry activates (default=0.0).
        """ # nopep8
        return self._cards[1].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        self._cards[1].set_value("birth", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node number.
        """ # nopep8
        return self._cards[2].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[2].set_value("nid", value)

    @property
    def x(self) -> float:
        """Get or set the x-coordinate.
        """ # nopep8
        return self._cards[2].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        self._cards[2].set_value("x", value)

    @property
    def y(self) -> float:
        """Get or set the y-coordinate.
        """ # nopep8
        return self._cards[2].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        self._cards[2].set_value("y", value)

    @property
    def z(self) -> float:
        """Get or set the z-coordinate.
        """ # nopep8
        return self._cards[2].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        self._cards[2].set_value("z", value)

