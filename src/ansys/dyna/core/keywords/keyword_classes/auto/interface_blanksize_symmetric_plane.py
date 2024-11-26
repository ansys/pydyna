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

class InterfaceBlanksizeSymmetricPlane(KeywordBase):
    """DYNA INTERFACE_BLANKSIZE_SYMMETRIC_PLANE keyword"""

    keyword = "INTERFACE"
    subkeyword = "BLANKSIZE_SYMMETRIC_PLANE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "x0",
                        float,
                        0,
                        10,
                        kwargs.get("x0", 0.0)
                    ),
                    Field(
                        "y0",
                        float,
                        10,
                        10,
                        kwargs.get("y0", 0.0)
                    ),
                    Field(
                        "z0",
                        float,
                        20,
                        10,
                        kwargs.get("z0", 0.0)
                    ),
                    Field(
                        "v1",
                        float,
                        30,
                        10,
                        kwargs.get("v1", 1.0)
                    ),
                    Field(
                        "v2",
                        float,
                        40,
                        10,
                        kwargs.get("v2", 0.0)
                    ),
                    Field(
                        "v3",
                        float,
                        50,
                        10,
                        kwargs.get("v3", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def x0(self) -> float:
        """Get or set the x, y, z coordinates of a point on the symmetric plane.  See example in  Scale Factor and Symmetric Plane.
        """ # nopep8
        return self._cards[0].get_value("x0")

    @x0.setter
    def x0(self, value: float) -> None:
        self._cards[0].set_value("x0", value)

    @property
    def y0(self) -> float:
        """Get or set the x, y, z coordinates of a point on the symmetric plane.  See example in  Scale Factor and Symmetric Plane.
        """ # nopep8
        return self._cards[0].get_value("y0")

    @y0.setter
    def y0(self, value: float) -> None:
        self._cards[0].set_value("y0", value)

    @property
    def z0(self) -> float:
        """Get or set the x, y, z coordinates of a point on the symmetric plane.  See example in  Scale Factor and Symmetric Plane.
        """ # nopep8
        return self._cards[0].get_value("z0")

    @z0.setter
    def z0(self, value: float) -> None:
        self._cards[0].set_value("z0", value)

    @property
    def v1(self) -> float:
        """Get or set the Vector components of the symmetric planes normal.  See example in  Scale Factor and Symmetric Plane.
        """ # nopep8
        return self._cards[0].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[0].set_value("v1", value)

    @property
    def v2(self) -> float:
        """Get or set the Vector components of the symmetric planes normal.  See example in  Scale Factor and Symmetric Plane.
        """ # nopep8
        return self._cards[0].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[0].set_value("v2", value)

    @property
    def v3(self) -> float:
        """Get or set the Vector components of the symmetric planes normal.  See example in  Scale Factor and Symmetric Plane.
        """ # nopep8
        return self._cards[0].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[0].set_value("v3", value)

