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

class IgaEdgeXyz(KeywordBase):
    """DYNA IGA_EDGE_XYZ keyword"""

    keyword = "IGA"
    subkeyword = "EDGE_XYZ"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eid",
                        int,
                        0,
                        10,
                        kwargs.get("eid")
                    ),
                    Field(
                        "nid",
                        int,
                        10,
                        10,
                        kwargs.get("nid")
                    ),
                    Field(
                        "ori",
                        int,
                        20,
                        10,
                        kwargs.get("ori", 0)
                    ),
                    Field(
                        "pidstart",
                        int,
                        30,
                        10,
                        kwargs.get("pidstart")
                    ),
                    Field(
                        "pidend",
                        int,
                        40,
                        10,
                        kwargs.get("pidend")
                    ),
                    Field(
                        "psid",
                        int,
                        50,
                        10,
                        kwargs.get("psid")
                    ),
                ],
            ),
        ]

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Physical edge ID. A unique number must be chosen.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        self._cards[0].set_value("eid", value)

    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Physical univariate NURBS ID, see *IGA_1D_NURBS_XYZ.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        self._cards[0].set_value("nid", value)

    @property
    def ori(self) -> int:
        """Get or set the Orientation with respect to the parametric univariate NURBS.
        EQ. 0:Same(default)
        EQ.1 : Reversed.
        """ # nopep8
        return self._cards[0].get_value("ori")

    @ori.setter
    def ori(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ori must be one of {0,1}""")
        self._cards[0].set_value("ori", value)

    @property
    def pidstart(self) -> typing.Optional[int]:
        """Get or set the Parametric point ID defining the start of the trimmed physical NURBS. If
        PIDSTART = 0, the physical univariate NURBS is not trimmed at its start,
        see Remark 1 and Remark 2.
        """ # nopep8
        return self._cards[0].get_value("pidstart")

    @pidstart.setter
    def pidstart(self, value: int) -> None:
        self._cards[0].set_value("pidstart", value)

    @property
    def pidend(self) -> typing.Optional[int]:
        """Get or set the Parametric point ID defining the end of the trimmed physical NURBS. If
        PIDEND = 0, the physical univariate NURBS is not trimmed at its end, see
        Remark 1 and Remark 2.
        """ # nopep8
        return self._cards[0].get_value("pidend")

    @pidend.setter
    def pidend(self, value: int) -> None:
        self._cards[0].set_value("pidend", value)

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Parametric point set ID, see *IGA_POINT_UVW and
        * SET_IGA_POINT_UVW, see Remark 3.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[0].set_value("psid", value)

