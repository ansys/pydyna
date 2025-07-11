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

"""Module providing the DefineDePatternOutput class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DefineDePatternOutput(KeywordBase):
    """DYNA DEFINE_DE_PATTERN_OUTPUT keyword"""

    keyword = "DEFINE"
    subkeyword = "DE_PATTERN_OUTPUT"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineDePatternOutput class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "ptype",
                        int,
                        10,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "xo",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "yo",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "zo",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "xh",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "yh",
                        float,
                        60,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "zh",
                        float,
                        70,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nset",
                        int,
                        0,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dist1",
                        float,
                        0,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "dist2",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "dist3",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "dist4",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "dist5",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "dist6",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "dist7",
                        float,
                        60,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "dist8",
                        float,
                        70,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineDePatternOutput.option_specs[0],
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
    def pid(self) -> int:
        """Get or set the Part or part set ID of the DES
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def ptype(self) -> int:
        """Get or set the EQ.0:	Part
        EQ.1:	Part set
        """ # nopep8
        return self._cards[0].get_value("ptype")

    @ptype.setter
    def ptype(self, value: int) -> None:
        """Set the ptype property."""
        if value not in [0, 1, None]:
            raise Exception("""ptype must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ptype", value)

    @property
    def xo(self) -> float:
        """Get or set the Coordinates of the origin.from which the distance to the planes is measured along the vector (XH,YH,ZH )-(XO,YO,ZO)
        """ # nopep8
        return self._cards[0].get_value("xo")

    @xo.setter
    def xo(self, value: float) -> None:
        """Set the xo property."""
        self._cards[0].set_value("xo", value)

    @property
    def yo(self) -> float:
        """Get or set the Coordinates of the origin.from which the distance to the planes is measured along the vector(XH,YH,ZH) - (XO,YO,ZO)
        """ # nopep8
        return self._cards[0].get_value("yo")

    @yo.setter
    def yo(self, value: float) -> None:
        """Set the yo property."""
        self._cards[0].set_value("yo", value)

    @property
    def zo(self) -> float:
        """Get or set the Coordinates of the origin.from which the distance to the planes is measured along the vector (XH,YH,ZH )-(XO,YO,ZO)
        """ # nopep8
        return self._cards[0].get_value("zo")

    @zo.setter
    def zo(self, value: float) -> None:
        """Set the zo property."""
        self._cards[0].set_value("zo", value)

    @property
    def xh(self) -> float:
        """Get or set the Head of the direction to which the planes are orthogonal .
        """ # nopep8
        return self._cards[0].get_value("xh")

    @xh.setter
    def xh(self, value: float) -> None:
        """Set the xh property."""
        self._cards[0].set_value("xh", value)

    @property
    def yh(self) -> float:
        """Get or set the Head of the direction to which the planes are orthogonal .
        """ # nopep8
        return self._cards[0].get_value("yh")

    @yh.setter
    def yh(self, value: float) -> None:
        """Set the yh property."""
        self._cards[0].set_value("yh", value)

    @property
    def zh(self) -> float:
        """Get or set the Head of the direction to which the planes are orthogonal .
        """ # nopep8
        return self._cards[0].get_value("zh")

    @zh.setter
    def zh(self, value: float) -> None:
        """Set the zh property."""
        self._cards[0].set_value("zh", value)

    @property
    def nset(self) -> int:
        """Get or set the Number of planes for which patterns will be recorded
        """ # nopep8
        return self._cards[1].get_value("nset")

    @nset.setter
    def nset(self, value: int) -> None:
        """Set the nset property."""
        self._cards[1].set_value("nset", value)

    @property
    def dist1(self) -> float:
        """Get or set the Distance of each plane from XO, YO, ZO
        """ # nopep8
        return self._cards[2].get_value("dist1")

    @dist1.setter
    def dist1(self, value: float) -> None:
        """Set the dist1 property."""
        self._cards[2].set_value("dist1", value)

    @property
    def dist2(self) -> float:
        """Get or set the Distance of each plane from XO, YO, ZO
        """ # nopep8
        return self._cards[2].get_value("dist2")

    @dist2.setter
    def dist2(self, value: float) -> None:
        """Set the dist2 property."""
        self._cards[2].set_value("dist2", value)

    @property
    def dist3(self) -> float:
        """Get or set the Distance of each plane from XO, YO, ZO
        """ # nopep8
        return self._cards[2].get_value("dist3")

    @dist3.setter
    def dist3(self, value: float) -> None:
        """Set the dist3 property."""
        self._cards[2].set_value("dist3", value)

    @property
    def dist4(self) -> float:
        """Get or set the Distance of each plane from XO, YO, ZO
        """ # nopep8
        return self._cards[2].get_value("dist4")

    @dist4.setter
    def dist4(self, value: float) -> None:
        """Set the dist4 property."""
        self._cards[2].set_value("dist4", value)

    @property
    def dist5(self) -> float:
        """Get or set the Distance of each plane from XO, YO, ZO
        """ # nopep8
        return self._cards[2].get_value("dist5")

    @dist5.setter
    def dist5(self, value: float) -> None:
        """Set the dist5 property."""
        self._cards[2].set_value("dist5", value)

    @property
    def dist6(self) -> float:
        """Get or set the Distance of each plane from XO, YO, ZO
        """ # nopep8
        return self._cards[2].get_value("dist6")

    @dist6.setter
    def dist6(self, value: float) -> None:
        """Set the dist6 property."""
        self._cards[2].set_value("dist6", value)

    @property
    def dist7(self) -> float:
        """Get or set the Distance of each plane from XO, YO, ZO
        """ # nopep8
        return self._cards[2].get_value("dist7")

    @dist7.setter
    def dist7(self, value: float) -> None:
        """Set the dist7 property."""
        self._cards[2].set_value("dist7", value)

    @property
    def dist8(self) -> float:
        """Get or set the Distance of each plane from XO, YO, ZO
        """ # nopep8
        return self._cards[2].get_value("dist8")

    @dist8.setter
    def dist8(self, value: float) -> None:
        """Set the dist8 property."""
        self._cards[2].set_value("dist8", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

