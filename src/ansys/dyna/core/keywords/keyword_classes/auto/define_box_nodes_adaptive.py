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

"""Module providing the DefineBoxNodesAdaptive class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DefineBoxNodesAdaptive(KeywordBase):
    """DYNA DEFINE_BOX_NODES_ADAPTIVE keyword"""

    keyword = "DEFINE"
    subkeyword = "BOX_NODES_ADAPTIVE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineBoxNodesAdaptive class."""
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
                        0,
                        **kwargs,
                    ),
                    Field(
                        "node",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "lcx",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcy",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "lcz",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "itype",
                        int,
                        50,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "radius",
                        float,
                        60,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "npiece",
                        int,
                        70,
                        10,
                        0,
                        **kwargs,
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
                        **kwargs,
                    ),
                    Field(
                        "level",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineBoxNodesAdaptive.option_specs[0],
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
    def boxid(self) -> int:
        """Get or set the Box ID. Define unique numbers.
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        """Set the boxid property."""
        self._cards[0].set_value("boxid", value)

    @property
    def node(self) -> int:
        """Get or set the Part ID of blank.
        """ # nopep8
        return self._cards[0].get_value("node")

    @node.setter
    def node(self, value: int) -> None:
        """Set the node property."""
        self._cards[0].set_value("node", value)

    @property
    def lcx(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs (see *DEFINE_CURVE) that define the path of the tool in the global X, Y, and Z directions, respectively
        """ # nopep8
        return self._cards[0].get_value("lcx")

    @lcx.setter
    def lcx(self, value: int) -> None:
        """Set the lcx property."""
        self._cards[0].set_value("lcx", value)

    @property
    def lcy(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs (see *DEFINE_CURVE) that define the path of the tool in the global X, Y, and Z directions, respectively
        """ # nopep8
        return self._cards[0].get_value("lcy")

    @lcy.setter
    def lcy(self, value: int) -> None:
        """Set the lcy property."""
        self._cards[0].set_value("lcy", value)

    @property
    def lcz(self) -> typing.Optional[int]:
        """Get or set the Load curve IDs (see *DEFINE_CURVE) that define the path of the tool in the global X, Y, and Z directions, respectively
        """ # nopep8
        return self._cards[0].get_value("lcz")

    @lcz.setter
    def lcz(self, value: int) -> None:
        """Set the lcz property."""
        self._cards[0].set_value("lcz", value)

    @property
    def itype(self) -> int:
        """Get or set the Type of curves LCX, LCY and LCZ.  Currently only time as a function of displacement load curves are supported.
        EQ.2:	LCX, LCYand LCZ are defined as time as a function of displacement..
        """ # nopep8
        return self._cards[0].get_value("itype")

    @itype.setter
    def itype(self, value: int) -> None:
        """Set the itype property."""
        self._cards[0].set_value("itype", value)

    @property
    def radius(self) -> float:
        """Get or set the The radius of the tube that defines the fission/fusion boundary.
        """ # nopep8
        return self._cards[0].get_value("radius")

    @radius.setter
    def radius(self, value: float) -> None:
        """Set the radius property."""
        self._cards[0].set_value("radius", value)

    @property
    def npiece(self) -> int:
        """Get or set the Number of segments used to approximate the tool path in one adaptive step.  Note that the tool’s path is divided into several linear segments for approximation
        """ # nopep8
        return self._cards[0].get_value("npiece")

    @npiece.setter
    def npiece(self, value: int) -> None:
        """Set the npiece property."""
        self._cards[0].set_value("npiece", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the The deformable part or part set ID on which the tube adaptivity is to be applied (see *PART).
        GT.0:	Part ID
        LT.0 : | PID | is
        a part set ID.A part set ID can be useful for simulating the forming of tailor welded blanks.
        """ # nopep8
        return self._cards[1].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[1].set_value("pid", value)

    @property
    def level(self) -> typing.Optional[int]:
        """Get or set the Desired mesh refinement level. Level set to a value of 1, 2, 3, … allows a maximum of 1, 4, 16, … elements to be created for each original element in the “tube region”.
        """ # nopep8
        return self._cards[1].get_value("level")

    @level.setter
    def level(self, value: int) -> None:
        """Set the level property."""
        self._cards[1].set_value("level", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[2].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

