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

class IgaPointUvw(KeywordBase):
    """DYNA IGA_POINT_UVW keyword"""

    keyword = "IGA"
    subkeyword = "POINT_UVW"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "nid",
                        int,
                        10,
                        10,
                        kwargs.get("nid")
                    ),
                    Field(
                        "u",
                        float,
                        20,
                        20,
                        kwargs.get("u", 0.0)
                    ),
                    Field(
                        "v",
                        float,
                        40,
                        20,
                        kwargs.get("v", 0.0)
                    ),
                    Field(
                        "w",
                        float,
                        60,
                        20,
                        kwargs.get("w", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Parametric point ID. A unique number must be chosen.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Node IDs, see *NODE, see Remark 3.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[0].set_value("nid", value)

    @property
    def u(self) -> float:
        """Get or set the Coordinates in the parametric u-direction.
        """ # nopep8
        return self._cards[0].get_value("u")

    @u.setter
    def u(self, value: float) -> None:
        self._cards[0].set_value("u", value)

    @property
    def v(self) -> float:
        """Get or set the Coordinates in the parametric v-direction.
        """ # nopep8
        return self._cards[0].get_value("v")

    @v.setter
    def v(self, value: float) -> None:
        self._cards[0].set_value("v", value)

    @property
    def w(self) -> float:
        """Get or set the Coordinates in the parametric w-direction.
        """ # nopep8
        return self._cards[0].get_value("w")

    @w.setter
    def w(self, value: float) -> None:
        self._cards[0].set_value("w", value)

