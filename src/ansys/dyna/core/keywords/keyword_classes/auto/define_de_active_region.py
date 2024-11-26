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

class DefineDeActiveRegion(KeywordBase):
    """DYNA DEFINE_DE_ACTIVE_REGION keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_ACTIVE_REGION"
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
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id")
                    ),
                    Field(
                        "type",
                        int,
                        10,
                        10,
                        kwargs.get("type", 0)
                    ),
                    Field(
                        "xm/r",
                        float,
                        20,
                        10,
                        kwargs.get("xm/r")
                    ),
                    Field(
                        "ym",
                        float,
                        30,
                        10,
                        kwargs.get("ym")
                    ),
                    Field(
                        "zm",
                        float,
                        40,
                        10,
                        kwargs.get("zm")
                    ),
                    Field(
                        "tbirth",
                        float,
                        50,
                        10,
                        kwargs.get("tbirth")
                    ),
                    Field(
                        "tdeath",
                        float,
                        60,
                        10,
                        kwargs.get("tdeath", 1.E+20)
                    ),
                    Field(
                        "nfreq",
                        int,
                        70,
                        10,
                        kwargs.get("nfreq", 1)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineDeActiveRegion.option_specs[0],
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
    def id(self) -> typing.Optional[int]:
        """Get or set the Set ID/Box ID/Node ID
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def type(self) -> int:
        """Get or set the EQ.0: Part set ID
        1: BOX ID
        EQ.2: Node ID
        EQ.3: Noide ID (box shaped region that moves with this node)
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""type must be one of {0,1,2,3}""")
        self._cards[0].set_value("type", value)

    @property
    def xm_r(self) -> typing.Optional[float]:
        """Get or set the For TYPE = 0 or 1, factor for region's margin on each direction based on region length.The static coordinates limits are determined either by part set or box option. To extended those limits to provide a buffer zone, these factors can be used. The margin in each direction is calculated in the following way.
        Limits for X-direction: Xmin, Xmax
        DX = Xmax-Xmin
        X_margin = Xm*DX
        Xmax = Xmax + X_margin
        Xmin = Xmin - X_margin,R is the radius of the region with center at Node ID (TYPE=2),
        """ # nopep8
        return self._cards[0].get_value("xm/r")

    @xm_r.setter
    def xm_r(self, value: float) -> None:
        self._cards[0].set_value("xm/r", value)

    @property
    def ym(self) -> typing.Optional[float]:
        """Get or set the For TYPE = 0 or 1, Factor for region's margin on each direction based on region length.The static coordinates limits are determined either by part set or box option. To extended those limits to provide a buffer zone, these factors can be used. The margin in each direction is calculated in the following way.
        Limits for Y-direction: Ymin, Ymax
        DY = Ymax-Ymin
        Y_margin = Ym*DY
        Ymax = Ymax + Y_margin
        Ymin = Ymin - Y_margin. R is the radius of the region with center at Node ID (TYPE=2),
        """ # nopep8
        return self._cards[0].get_value("ym")

    @ym.setter
    def ym(self, value: float) -> None:
        self._cards[0].set_value("ym", value)

    @property
    def zm(self) -> typing.Optional[float]:
        """Get or set the For TYPE = 0 or 1, Factor for region's margin on each direction based on region length.The static coordinates limits are determined either by part set or box option. To extended those limits to provide a buffer zone, these factors can be used. The margin in each direction is calculated in the following way.
        Limits for Z-direction: Zmin, Zmax
        DZ = Zmax-Zmin
        Z_margin = Zm*DZ
        Zmax = Zmax + Z_margin
        Zmin = Zmin - Z_margin. R is the radius of the region with center at Node ID (TYPE=2),
        """ # nopep8
        return self._cards[0].get_value("zm")

    @zm.setter
    def zm(self, value: float) -> None:
        self._cards[0].set_value("zm", value)

    @property
    def tbirth(self) -> typing.Optional[float]:
        """Get or set the Birth time for the active region when Node ID is used (TYPE=2).
        """ # nopep8
        return self._cards[0].get_value("tbirth")

    @tbirth.setter
    def tbirth(self, value: float) -> None:
        self._cards[0].set_value("tbirth", value)

    @property
    def tdeath(self) -> float:
        """Get or set the Death time for the active region when Node ID is used (TYPE=2).
        """ # nopep8
        return self._cards[0].get_value("tdeath")

    @tdeath.setter
    def tdeath(self, value: float) -> None:
        self._cards[0].set_value("tdeath", value)

    @property
    def nfreq(self) -> int:
        """Get or set the Number of cycles between updates the region's location for they =3
        """ # nopep8
        return self._cards[0].get_value("nfreq")

    @nfreq.setter
    def nfreq(self, value: int) -> None:
        self._cards[0].set_value("nfreq", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

