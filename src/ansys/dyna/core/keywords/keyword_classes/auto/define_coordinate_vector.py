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

"""Module providing the DefineCoordinateVector class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DefineCoordinateVector(KeywordBase):
    """DYNA DEFINE_COORDINATE_VECTOR keyword"""

    keyword = "DEFINE"
    subkeyword = "COORDINATE_VECTOR"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineCoordinateVector class."""
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
                        "xx",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "yx",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "zx",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "xv",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "yv",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "zv",
                        float,
                        60,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "nid",
                        int,
                        70,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineCoordinateVector.option_specs[0],
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
    def xx(self) -> float:
        """Get or set the x-coordinate on local x-axis. Origin lies at (0,0,0).
        """ # nopep8
        return self._cards[0].get_value("xx")

    @xx.setter
    def xx(self, value: float) -> None:
        """Set the xx property."""
        self._cards[0].set_value("xx", value)

    @property
    def yx(self) -> float:
        """Get or set the y-coordinate on local x-axis.
        """ # nopep8
        return self._cards[0].get_value("yx")

    @yx.setter
    def yx(self, value: float) -> None:
        """Set the yx property."""
        self._cards[0].set_value("yx", value)

    @property
    def zx(self) -> float:
        """Get or set the z-coordinate on local x-axis.
        """ # nopep8
        return self._cards[0].get_value("zx")

    @zx.setter
    def zx(self, value: float) -> None:
        """Set the zx property."""
        self._cards[0].set_value("zx", value)

    @property
    def xv(self) -> float:
        """Get or set the x-coordinate of local x-y vector.
        """ # nopep8
        return self._cards[0].get_value("xv")

    @xv.setter
    def xv(self, value: float) -> None:
        """Set the xv property."""
        self._cards[0].set_value("xv", value)

    @property
    def yv(self) -> float:
        """Get or set the y-coordinate of local x-y vector.
        """ # nopep8
        return self._cards[0].get_value("yv")

    @yv.setter
    def yv(self, value: float) -> None:
        """Set the yv property."""
        self._cards[0].set_value("yv", value)

    @property
    def zv(self) -> float:
        """Get or set the z-coordinate of local x-y vector.
        """ # nopep8
        return self._cards[0].get_value("zv")

    @zv.setter
    def zv(self, value: float) -> None:
        """Set the zv property."""
        self._cards[0].set_value("zv", value)

    @property
    def nid(self) -> int:
        """Get or set the Optional nodal point ID.  The coordinate system rotates with the rotation of this node.  If the node is not defined, the coordinate system is stationary
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[0].set_value("nid", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[1].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

