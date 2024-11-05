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

class DefineCurveEntity(KeywordBase):
    """DYNA DEFINE_CURVE_ENTITY keyword"""

    keyword = "DEFINE"
    subkeyword = "CURVE_ENTITY"
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
                        "lcid",
                        int,
                        0,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "sfa",
                        float,
                        10,
                        10,
                        kwargs.get("sfa", 1.0)
                    ),
                    Field(
                        "sfo",
                        float,
                        20,
                        10,
                        kwargs.get("sfo", 1.0)
                    ),
                    Field(
                        "sfr",
                        float,
                        30,
                        10,
                        kwargs.get("sfr", 1.0)
                    ),
                    Field(
                        "offa",
                        float,
                        40,
                        10,
                        kwargs.get("offa", 0.0)
                    ),
                    Field(
                        "offo",
                        float,
                        50,
                        10,
                        kwargs.get("offo", 0.0)
                    ),
                    Field(
                        "offr",
                        float,
                        60,
                        10,
                        kwargs.get("offr", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ai",
                        float,
                        0,
                        20,
                        kwargs.get("ai", 0.0)
                    ),
                    Field(
                        "oi",
                        float,
                        20,
                        20,
                        kwargs.get("oi", 0.0)
                    ),
                    Field(
                        "ri",
                        float,
                        40,
                        20,
                        kwargs.get("ri", 0.0)
                    ),
                    Field(
                        "iflag",
                        int,
                        60,
                        20,
                        kwargs.get("iflag", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineCurveEntity.option_specs[0],
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
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID. Tables (see *DEFINE_TABLE) and load curves may not share common ID's. LS-DYNA3D allows load curve ID's and table ID's to be used interchangeably. A unique number has to be defined. Note: The magnitude of LCID is restricted to 5 significant digits. This limitation will be removed in a future release of LS-DYNA3D.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def sfa(self) -> float:
        """Get or set the Scale factor for axis value. This is useful for simple modifications.	EQ.0.0: default set to 1.0.
        """ # nopep8
        return self._cards[0].get_value("sfa")

    @sfa.setter
    def sfa(self, value: float) -> None:
        self._cards[0].set_value("sfa", value)

    @property
    def sfo(self) -> float:
        """Get or set the Scale factor for radius values. This is useful for simple modifications.	EQ.0.0: default set to 1.0.
        """ # nopep8
        return self._cards[0].get_value("sfo")

    @sfo.setter
    def sfo(self, value: float) -> None:
        self._cards[0].set_value("sfo", value)

    @property
    def sfr(self) -> float:
        """Get or set the Scale factor for circular radius. This is useful for simple	modifications. EQ.0.0: default set to 1.0.
        """ # nopep8
        return self._cards[0].get_value("sfr")

    @sfr.setter
    def sfr(self, value: float) -> None:
        self._cards[0].set_value("sfr", value)

    @property
    def offa(self) -> float:
        """Get or set the Offset for axis values, see explanation below.
        """ # nopep8
        return self._cards[0].get_value("offa")

    @offa.setter
    def offa(self, value: float) -> None:
        self._cards[0].set_value("offa", value)

    @property
    def offo(self) -> float:
        """Get or set the Offset for radius values, see explanation below.
        """ # nopep8
        return self._cards[0].get_value("offo")

    @offo.setter
    def offo(self, value: float) -> None:
        self._cards[0].set_value("offo", value)

    @property
    def offr(self) -> float:
        """Get or set the Offset for circular radius, see explanation below.
        """ # nopep8
        return self._cards[0].get_value("offr")

    @offr.setter
    def offr(self, value: float) -> None:
        self._cards[0].set_value("offr", value)

    @property
    def ai(self) -> float:
        """Get or set the Z-axis coordinates along the axis of rotation.
        """ # nopep8
        return self._cards[1].get_value("ai")

    @ai.setter
    def ai(self, value: float) -> None:
        self._cards[1].set_value("ai", value)

    @property
    def oi(self) -> float:
        """Get or set the Radial coordinates from the axis of rotation
        """ # nopep8
        return self._cards[1].get_value("oi")

    @oi.setter
    def oi(self, value: float) -> None:
        self._cards[1].set_value("oi", value)

    @property
    def ri(self) -> float:
        """Get or set the Radius of arc between points (Ai,Oi) and (Ai+1,Oi+1). If zero, a straight line segment is assumed.
        """ # nopep8
        return self._cards[1].get_value("ri")

    @ri.setter
    def ri(self, value: float) -> None:
        self._cards[1].set_value("ri", value)

    @property
    def iflag(self) -> int:
        """Get or set the Defined if |Ri| > 0. Set to 1 if center of arc is inside axisymmetric surface and to -1 if the center is outside the axisymmetric surface.
        """ # nopep8
        return self._cards[1].get_value("iflag")

    @iflag.setter
    def iflag(self, value: int) -> None:
        self._cards[1].set_value("iflag", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

