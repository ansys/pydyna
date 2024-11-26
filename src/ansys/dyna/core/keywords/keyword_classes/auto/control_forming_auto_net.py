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

class ControlFormingAutoNet(KeywordBase):
    """DYNA CONTROL_FORMING_AUTO_NET keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_AUTO_NET"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "idnet",
                        int,
                        0,
                        10,
                        kwargs.get("idnet")
                    ),
                    Field(
                        "itype",
                        str,
                        10,
                        10,
                        kwargs.get("itype")
                    ),
                    Field(
                        "idv",
                        int,
                        20,
                        10,
                        kwargs.get("idv", 0)
                    ),
                    Field(
                        "idp",
                        int,
                        30,
                        10,
                        kwargs.get("idp", 0)
                    ),
                    Field(
                        "x",
                        float,
                        40,
                        10,
                        kwargs.get("x", 0.0)
                    ),
                    Field(
                        "y",
                        float,
                        50,
                        10,
                        kwargs.get("y", 0.0)
                    ),
                    Field(
                        "z",
                        float,
                        60,
                        10,
                        kwargs.get("z", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sx",
                        float,
                        0,
                        10,
                        kwargs.get("sx", 0.0)
                    ),
                    Field(
                        "sy",
                        float,
                        10,
                        10,
                        kwargs.get("sy", 0.0)
                    ),
                    Field(
                        "offset",
                        float,
                        20,
                        10,
                        kwargs.get("offset", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def idnet(self) -> typing.Optional[int]:
        """Get or set the ID of the net; must be unique.
        """ # nopep8
        return self._cards[0].get_value("idnet")

    @idnet.setter
    def idnet(self, value: int) -> None:
        self._cards[0].set_value("idnet", value)

    @property
    def itype(self) -> typing.Optional[str]:
        """Get or set the Not used at this time
        """ # nopep8
        return self._cards[0].get_value("itype")

    @itype.setter
    def itype(self, value: str) -> None:
        self._cards[0].set_value("itype", value)

    @property
    def idv(self) -> int:
        """Get or set the Vector ID indicating the direction of the net to be generated. See *DEFINE_VECTOR for details. If not defined, the net will be generated along the global Z-axis
        """ # nopep8
        return self._cards[0].get_value("idv")

    @idv.setter
    def idv(self, value: int) -> None:
        self._cards[0].set_value("idv", value)

    @property
    def idp(self) -> int:
        """Get or set the Part ID of the panel undergoing springback simulation.
        """ # nopep8
        return self._cards[0].get_value("idp")

    @idp.setter
    def idp(self, value: int) -> None:
        self._cards[0].set_value("idp", value)

    @property
    def x(self) -> float:
        """Get or set the X-coordinate of a reference point for the net to be generated
        """ # nopep8
        return self._cards[0].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        self._cards[0].set_value("x", value)

    @property
    def y(self) -> float:
        """Get or set the Y-coordinate of a reference point for the net to be generated
        """ # nopep8
        return self._cards[0].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        self._cards[0].set_value("y", value)

    @property
    def z(self) -> float:
        """Get or set the Z-coordinate of a reference point for the net to be generated
        """ # nopep8
        return self._cards[0].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        self._cards[0].set_value("z", value)

    @property
    def sx(self) -> float:
        """Get or set the Length of the net along X-axis.
        """ # nopep8
        return self._cards[1].get_value("sx")

    @sx.setter
    def sx(self, value: float) -> None:
        self._cards[1].set_value("sx", value)

    @property
    def sy(self) -> float:
        """Get or set the Length of the net along X-axis.
        """ # nopep8
        return self._cards[1].get_value("sy")

    @sy.setter
    def sy(self, value: float) -> None:
        self._cards[1].set_value("sy", value)

    @property
    def offset(self) -> float:
        """Get or set the The net will be generated at this offset distance away from the reference point.
        GT.0: the net will be on the global +Z side, or on the vector head side if IDV is defined.
        LT.0: the net will be on the global ¨CZ side, or on the vector tail side if IDV is defined
        """ # nopep8
        return self._cards[1].get_value("offset")

    @offset.setter
    def offset(self, value: float) -> None:
        self._cards[1].set_value("offset", value)

