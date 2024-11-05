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

class DefineBoxAdaptive(KeywordBase):
    """DYNA DEFINE_BOX_ADAPTIVE keyword"""

    keyword = "DEFINE"
    subkeyword = "BOX_ADAPTIVE"
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
                        "boxid",
                        int,
                        0,
                        10,
                        kwargs.get("boxid")
                    ),
                    Field(
                        "xmn",
                        float,
                        10,
                        10,
                        kwargs.get("xmn", 0.0)
                    ),
                    Field(
                        "xmx",
                        float,
                        20,
                        10,
                        kwargs.get("xmx", 0.0)
                    ),
                    Field(
                        "ymn",
                        float,
                        30,
                        10,
                        kwargs.get("ymn", 0.0)
                    ),
                    Field(
                        "ymx",
                        float,
                        40,
                        10,
                        kwargs.get("ymx", 0.0)
                    ),
                    Field(
                        "zmn",
                        float,
                        50,
                        10,
                        kwargs.get("zmn", 0.0)
                    ),
                    Field(
                        "zmx",
                        float,
                        60,
                        10,
                        kwargs.get("zmx", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid", 0)
                    ),
                    Field(
                        "level",
                        int,
                        10,
                        10,
                        kwargs.get("level", 1)
                    ),
                    Field(
                        "lidx/ndid",
                        int,
                        20,
                        10,
                        kwargs.get("lidx/ndid", 0)
                    ),
                    Field(
                        "lidy",
                        int,
                        30,
                        10,
                        kwargs.get("lidy", 0)
                    ),
                    Field(
                        "lidz",
                        int,
                        40,
                        10,
                        kwargs.get("lidz", 0)
                    ),
                    Field(
                        "brmin",
                        float,
                        50,
                        10,
                        kwargs.get("brmin", 0.0)
                    ),
                    Field(
                        "brmax",
                        float,
                        60,
                        10,
                        kwargs.get("brmax", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineBoxAdaptive.option_specs[0],
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
    def boxid(self) -> typing.Optional[int]:
        """Get or set the Box ID. Define unique numbers.
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        self._cards[0].set_value("boxid", value)

    @property
    def xmn(self) -> float:
        """Get or set the Minimum x-coordinate.
        """ # nopep8
        return self._cards[0].get_value("xmn")

    @xmn.setter
    def xmn(self, value: float) -> None:
        self._cards[0].set_value("xmn", value)

    @property
    def xmx(self) -> float:
        """Get or set the Maximum x-coordinate.
        """ # nopep8
        return self._cards[0].get_value("xmx")

    @xmx.setter
    def xmx(self, value: float) -> None:
        self._cards[0].set_value("xmx", value)

    @property
    def ymn(self) -> float:
        """Get or set the Minimum y-coordinate.
        """ # nopep8
        return self._cards[0].get_value("ymn")

    @ymn.setter
    def ymn(self, value: float) -> None:
        self._cards[0].set_value("ymn", value)

    @property
    def ymx(self) -> float:
        """Get or set the Maximum y-coordinate.
        """ # nopep8
        return self._cards[0].get_value("ymx")

    @ymx.setter
    def ymx(self, value: float) -> None:
        self._cards[0].set_value("ymx", value)

    @property
    def zmn(self) -> float:
        """Get or set the Minimum z-coordinate.
        """ # nopep8
        return self._cards[0].get_value("zmn")

    @zmn.setter
    def zmn(self, value: float) -> None:
        self._cards[0].set_value("zmn", value)

    @property
    def zmx(self) -> float:
        """Get or set the Maximum z-coordinate.
        """ # nopep8
        return self._cards[0].get_value("zmx")

    @zmx.setter
    def zmx(self, value: float) -> None:
        self._cards[0].set_value("zmx", value)

    @property
    def pid(self) -> int:
        """Get or set the Part ID. If zero, all active elements within box are considered.
        """ # nopep8
        return self._cards[1].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[1].set_value("pid", value)

    @property
    def level(self) -> int:
        """Get or set the Maximum number of refinement levels for elements that are contained in the box. Values of 1, 2, 3, 4,... allow a maximum of 1, 4, 16, 64, ...  elements, respectively, to be created for each original element.
        """ # nopep8
        return self._cards[1].get_value("level")

    @level.setter
    def level(self, value: int) -> None:
        self._cards[1].set_value("level", value)

    @property
    def lidx_ndid(self) -> int:
        """Get or set the Load curve ID/Node ID.
        GT.0: load curve ID. Define adaptive box movement (displacement
        vs. time) in global X axis.
        LT.0: absolute value is a node ID, whose movement will be
        followed by the moving adaptive box. The node ID can be
        on a moving rigid body.
        EQ.0: no movement.
        """ # nopep8
        return self._cards[1].get_value("lidx/ndid")

    @lidx_ndid.setter
    def lidx_ndid(self, value: int) -> None:
        self._cards[1].set_value("lidx/ndid", value)

    @property
    def lidy(self) -> int:
        """Get or set the Load curve ID.
        GT.0: load curve ID. Define adaptive box movement (displacement
        vs. time) in global Y axis.
        EQ.0: no movement.
        """ # nopep8
        return self._cards[1].get_value("lidy")

    @lidy.setter
    def lidy(self, value: int) -> None:
        self._cards[1].set_value("lidy", value)

    @property
    def lidz(self) -> int:
        """Get or set the Load curve ID.
        GT.0: load curve ID. Define adaptive box movement (displacement
        vs. time) in global Y axis.
        EQ.0: no movement.
        """ # nopep8
        return self._cards[1].get_value("lidz")

    @lidz.setter
    def lidz(self, value: int) -> None:
        self._cards[1].set_value("lidz", value)

    @property
    def brmin(self) -> float:
        """Get or set the Minimum mesh size in 3D tetrahedron adaptivity.
        """ # nopep8
        return self._cards[1].get_value("brmin")

    @brmin.setter
    def brmin(self, value: float) -> None:
        self._cards[1].set_value("brmin", value)

    @property
    def brmax(self) -> float:
        """Get or set the Maximum mesh size in 3D tetrahedron adaptivity.
        """ # nopep8
        return self._cards[1].get_value("brmax")

    @brmax.setter
    def brmax(self, value: float) -> None:
        self._cards[1].set_value("brmax", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

