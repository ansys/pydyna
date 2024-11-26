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

class Mat084(KeywordBase):
    """DYNA MAT_084 keyword"""

    keyword = "MAT"
    subkeyword = "084"
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
                        "eid1/unused",
                        int,
                        0,
                        10,
                        kwargs.get("eid1/unused")
                    ),
                    Field(
                        "eid2/pid",
                        int,
                        10,
                        10,
                        kwargs.get("eid2/pid")
                    ),
                    Field(
                        "inc/axis",
                        int,
                        20,
                        10,
                        kwargs.get("inc/axis")
                    ),
                    Field(
                        "xr/coor",
                        float,
                        30,
                        10,
                        kwargs.get("xr/coor")
                    ),
                    Field(
                        "yr/rqa",
                        float,
                        40,
                        10,
                        kwargs.get("yr/rqa")
                    ),
                    Field(
                        "zr/rqb",
                        float,
                        50,
                        10,
                        kwargs.get("zr/rqb")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat084.option_specs[0],
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
    def eid1_unused(self) -> typing.Optional[int]:
        """Get or set the First element ID in group. Left blank to active card of option 2.
        """ # nopep8
        return self._cards[0].get_value("eid1/unused")

    @eid1_unused.setter
    def eid1_unused(self, value: int) -> None:
        self._cards[0].set_value("eid1/unused", value)

    @property
    def eid2_pid(self) -> typing.Optional[int]:
        """Get or set the Last element ID in group.
        PID: Part ID of reinforced elements
        """ # nopep8
        return self._cards[0].get_value("eid2/pid")

    @eid2_pid.setter
    def eid2_pid(self, value: int) -> None:
        self._cards[0].set_value("eid2/pid", value)

    @property
    def inc_axis(self) -> typing.Optional[int]:
        """Get or set the Element increment for generation.
        AXIS: Axis normal to layer:
        EQ.1: A and B are parallel to global Y and Z, respectively (default),
        EQ.2: A and B are parallel to global Z and X, respectively,
        EQ.3: A and B are parallel to global X and Y, respectively.
        """ # nopep8
        return self._cards[0].get_value("inc/axis")

    @inc_axis.setter
    def inc_axis(self, value: int) -> None:
        self._cards[0].set_value("inc/axis", value)

    @property
    def xr_coor(self) -> typing.Optional[float]:
        """Get or set the X-reinforcement quantity (for bars running parallel to global x-axis).
        COOR: Coordinate location of layer
        If AXIS.EQ.1: X-coordinate ,
        If AXIS.EQ.2: Y-coordinate,
        If AXIS.EQ.3: Z-coordinate
        """ # nopep8
        return self._cards[0].get_value("xr/coor")

    @xr_coor.setter
    def xr_coor(self, value: float) -> None:
        self._cards[0].set_value("xr/coor", value)

    @property
    def yr_rqa(self) -> typing.Optional[float]:
        """Get or set the Y-reinforcement quantity (for bars running parallel to global y-axis).
        RQA: Reinforcement quantity (A).
        """ # nopep8
        return self._cards[0].get_value("yr/rqa")

    @yr_rqa.setter
    def yr_rqa(self, value: float) -> None:
        self._cards[0].set_value("yr/rqa", value)

    @property
    def zr_rqb(self) -> typing.Optional[float]:
        """Get or set the Z-reinforcement quantity (for bars running parallel to global z-axis).
        RQB: Reinforcement quantity (B).
        """ # nopep8
        return self._cards[0].get_value("zr/rqb")

    @zr_rqb.setter
    def zr_rqb(self, value: float) -> None:
        self._cards[0].set_value("zr/rqb", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

