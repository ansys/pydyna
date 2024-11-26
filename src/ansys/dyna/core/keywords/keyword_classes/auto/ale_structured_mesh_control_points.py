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

class AleStructuredMeshControlPoints(KeywordBase):
    """DYNA ALE_STRUCTURED_MESH_CONTROL_POINTS keyword"""

    keyword = "ALE"
    subkeyword = "STRUCTURED_MESH_CONTROL_POINTS"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "cpid",
                        int,
                        0,
                        10,
                        kwargs.get("cpid", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "icase",
                        int,
                        20,
                        10,
                        kwargs.get("icase", 0)
                    ),
                    Field(
                        "sfo",
                        float,
                        30,
                        10,
                        kwargs.get("sfo", 1.0)
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "offo",
                        float,
                        50,
                        10,
                        kwargs.get("offo", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "n",
                        int,
                        0,
                        20,
                        kwargs.get("n", 0)
                    ),
                    Field(
                        "x",
                        float,
                        20,
                        20,
                        kwargs.get("x")
                    ),
                    Field(
                        "ratio",
                        float,
                        40,
                        20,
                        kwargs.get("ratio", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def cpid(self) -> int:
        """Get or set the Control Points ID. A unique number must be specified. This ID is to be
        referred in the three fields marked up CPIDX, CPIDY, CPIDZ in *ALE_	STRUCTURED_MESH.
        """ # nopep8
        return self._cards[0].get_value("cpid")

    @cpid.setter
    def cpid(self, value: int) -> None:
        self._cards[0].set_value("cpid", value)

    @property
    def icase(self) -> int:
        """Get or set the A flag to trigger special logic for a more user-friendly input format for progressive mesh spacing. Please see examples sections below on ICASE usage.
        """ # nopep8
        return self._cards[0].get_value("icase")

    @icase.setter
    def icase(self, value: int) -> None:
        self._cards[0].set_value("icase", value)

    @property
    def sfo(self) -> float:
        """Get or set the Scale factor for ordinate value. This is useful for simple modifications.	EQ.0.0: default set to 1.0.
        """ # nopep8
        return self._cards[0].get_value("sfo")

    @sfo.setter
    def sfo(self, value: float) -> None:
        self._cards[0].set_value("sfo", value)

    @property
    def offo(self) -> float:
        """Get or set the Offset for ordinate values. See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("offo")

    @offo.setter
    def offo(self, value: float) -> None:
        self._cards[0].set_value("offo", value)

    @property
    def n(self) -> int:
        """Get or set the Control point node number.
        """ # nopep8
        return self._cards[1].get_value("n")

    @n.setter
    def n(self, value: int) -> None:
        self._cards[1].set_value("n", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the Control point position.
        """ # nopep8
        return self._cards[1].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        self._cards[1].set_value("x", value)

    @property
    def ratio(self) -> float:
        """Get or set the Ratio for progressive mesh spacing.  Progressively larger or smaller mesh will be generated between the control point that has nonzero ratio specified and the control point following it.  See remark 2.
        GT.0.0:	mesh size increases; dl(n+1)=dl_n*(1+ratio)
        LT.0.0:	mesh size decreases; dl(n+1)=dl_n/(1-ratio).
        """ # nopep8
        return self._cards[1].get_value("ratio")

    @ratio.setter
    def ratio(self, value: float) -> None:
        self._cards[1].set_value("ratio", value)

