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

"""Module providing the DefineCoordinateSystem class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DefineCoordinateSystem(KeywordBase):
    """DYNA DEFINE_COORDINATE_SYSTEM keyword"""

    keyword = "DEFINE"
    subkeyword = "COORDINATE_SYSTEM"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineCoordinateSystem class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "cid",
                        int,
                        0,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "xo",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "yo",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "zo",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "xl",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "yl",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "zl",
                        float,
                        60,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "cidl",
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
                        "xp",
                        float,
                        0,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "yp",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "zp",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineCoordinateSystem.option_specs[0],
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
    def cid(self) -> int:
        """Get or set the Coordinate system ID. A unique number has to be defined.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def xo(self) -> float:
        """Get or set the x-coordinate of origin.
        """ # nopep8
        return self._cards[0].get_value("xo")

    @xo.setter
    def xo(self, value: float) -> None:
        """Set the xo property."""
        self._cards[0].set_value("xo", value)

    @property
    def yo(self) -> float:
        """Get or set the y-coordinate of origin.
        """ # nopep8
        return self._cards[0].get_value("yo")

    @yo.setter
    def yo(self, value: float) -> None:
        """Set the yo property."""
        self._cards[0].set_value("yo", value)

    @property
    def zo(self) -> float:
        """Get or set the z-coordinate of origin.
        """ # nopep8
        return self._cards[0].get_value("zo")

    @zo.setter
    def zo(self, value: float) -> None:
        """Set the zo property."""
        self._cards[0].set_value("zo", value)

    @property
    def xl(self) -> float:
        """Get or set the x-coordinate of point on local x-axis.
        """ # nopep8
        return self._cards[0].get_value("xl")

    @xl.setter
    def xl(self, value: float) -> None:
        """Set the xl property."""
        self._cards[0].set_value("xl", value)

    @property
    def yl(self) -> float:
        """Get or set the y-coordinate of point on local x-axis.
        """ # nopep8
        return self._cards[0].get_value("yl")

    @yl.setter
    def yl(self, value: float) -> None:
        """Set the yl property."""
        self._cards[0].set_value("yl", value)

    @property
    def zl(self) -> float:
        """Get or set the z-coordinate of point on local x-axis.
        """ # nopep8
        return self._cards[0].get_value("zl")

    @zl.setter
    def zl(self, value: float) -> None:
        """Set the zl property."""
        self._cards[0].set_value("zl", value)

    @property
    def cidl(self) -> int:
        """Get or set the Coordinate system ID applied to the coordinates used to define the
        current system. The coordinates X0, Y0, Z0, XL, YL, ZL, XP, YP, and
        ZP are defined with respect to the coordinate system CIDL.
        """ # nopep8
        return self._cards[0].get_value("cidl")

    @cidl.setter
    def cidl(self, value: int) -> None:
        """Set the cidl property."""
        self._cards[0].set_value("cidl", value)

    @property
    def xp(self) -> float:
        """Get or set the x-coordinate of point in local x-y plane.
        """ # nopep8
        return self._cards[1].get_value("xp")

    @xp.setter
    def xp(self, value: float) -> None:
        """Set the xp property."""
        self._cards[1].set_value("xp", value)

    @property
    def yp(self) -> float:
        """Get or set the y-coordinate of point in local x-y plane.
        """ # nopep8
        return self._cards[1].get_value("yp")

    @yp.setter
    def yp(self, value: float) -> None:
        """Set the yp property."""
        self._cards[1].set_value("yp", value)

    @property
    def zp(self) -> float:
        """Get or set the z-coordinate of point in local x-y plane.
        """ # nopep8
        return self._cards[1].get_value("zp")

    @zp.setter
    def zp(self, value: float) -> None:
        """Set the zp property."""
        self._cards[1].set_value("zp", value)

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

