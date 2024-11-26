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

class ControlMppDecompositionTransformation(KeywordBase):
    """DYNA CONTROL_MPP_DECOMPOSITION_TRANSFORMATION keyword"""

    keyword = "CONTROL"
    subkeyword = "MPP_DECOMPOSITION_TRANSFORMATION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "type",
                        str,
                        0,
                        10,
                        kwargs.get("type", "RX")
                    ),
                    Field(
                        "v1",
                        float,
                        10,
                        10,
                        kwargs.get("v1", 0.0)
                    ),
                    Field(
                        "v2",
                        float,
                        20,
                        10,
                        kwargs.get("v2", 0.0)
                    ),
                    Field(
                        "v3",
                        float,
                        30,
                        10,
                        kwargs.get("v3", 0.0)
                    ),
                    Field(
                        "v4",
                        float,
                        40,
                        10,
                        kwargs.get("v4", 0.0)
                    ),
                    Field(
                        "v5",
                        float,
                        50,
                        10,
                        kwargs.get("v5", 0.0)
                    ),
                    Field(
                        "v6",
                        float,
                        60,
                        10,
                        kwargs.get("v6", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "v7",
                        float,
                        0,
                        10,
                        kwargs.get("v7", 0.0)
                    ),
                    Field(
                        "v8",
                        float,
                        10,
                        10,
                        kwargs.get("v8", 0.0)
                    ),
                    Field(
                        "v9",
                        float,
                        20,
                        10,
                        kwargs.get("v9", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def type(self) -> str:
        """Get or set the Which transformation to apply. The possible values are :VEC3,C2R,S2R,MAT.
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: str) -> None:
        if value not in ["RX", "RY", "RZ", "SX", "SY", "SZ", "VEC3", "C2R", "S2R", "MAT"]:
            raise Exception("""type must be one of {"RX","RY","RZ","SX","SY","SZ","VEC3","C2R","S2R","MAT"}""")
        self._cards[0].set_value("type", value)

    @property
    def v1(self) -> float:
        """Get or set the Parameters to the transformation.
        """ # nopep8
        return self._cards[0].get_value("v1")

    @v1.setter
    def v1(self, value: float) -> None:
        self._cards[0].set_value("v1", value)

    @property
    def v2(self) -> float:
        """Get or set the Parameters to the transformation.
        """ # nopep8
        return self._cards[0].get_value("v2")

    @v2.setter
    def v2(self, value: float) -> None:
        self._cards[0].set_value("v2", value)

    @property
    def v3(self) -> float:
        """Get or set the Parameters to the transformation.
        """ # nopep8
        return self._cards[0].get_value("v3")

    @v3.setter
    def v3(self, value: float) -> None:
        self._cards[0].set_value("v3", value)

    @property
    def v4(self) -> float:
        """Get or set the Parameters to the transformation.
        """ # nopep8
        return self._cards[0].get_value("v4")

    @v4.setter
    def v4(self, value: float) -> None:
        self._cards[0].set_value("v4", value)

    @property
    def v5(self) -> float:
        """Get or set the Parameters to the transformation.
        """ # nopep8
        return self._cards[0].get_value("v5")

    @v5.setter
    def v5(self, value: float) -> None:
        self._cards[0].set_value("v5", value)

    @property
    def v6(self) -> float:
        """Get or set the Parameters to the transformation.
        """ # nopep8
        return self._cards[0].get_value("v6")

    @v6.setter
    def v6(self, value: float) -> None:
        self._cards[0].set_value("v6", value)

    @property
    def v7(self) -> float:
        """Get or set the Parameters to the transformation.
        """ # nopep8
        return self._cards[1].get_value("v7")

    @v7.setter
    def v7(self, value: float) -> None:
        self._cards[1].set_value("v7", value)

    @property
    def v8(self) -> float:
        """Get or set the Parameters to the transformation.
        """ # nopep8
        return self._cards[1].get_value("v8")

    @v8.setter
    def v8(self, value: float) -> None:
        self._cards[1].set_value("v8", value)

    @property
    def v9(self) -> float:
        """Get or set the Parameters to the transformation.
        """ # nopep8
        return self._cards[1].get_value("v9")

    @v9.setter
    def v9(self, value: float) -> None:
        self._cards[1].set_value("v9", value)

