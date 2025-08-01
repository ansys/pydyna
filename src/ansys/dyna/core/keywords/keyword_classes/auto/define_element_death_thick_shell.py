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

"""Module providing the DefineElementDeathThickShell class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DefineElementDeathThickShell(KeywordBase):
    """DYNA DEFINE_ELEMENT_DEATH_THICK_SHELL keyword"""

    keyword = "DEFINE"
    subkeyword = "ELEMENT_DEATH_THICK_SHELL"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineElementDeathThickShell class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "eid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "time",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "boxid",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "inout",
                        int,
                        30,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "idgrp",
                        int,
                        40,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "cid",
                        int,
                        50,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "percent",
                        float,
                        60,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineElementDeathThickShell.option_specs[0],
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
    def eid(self) -> typing.Optional[int]:
        """Get or set the Element ID
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        """Set the eid property."""
        self._cards[0].set_value("eid", value)

    @property
    def time(self) -> float:
        """Get or set the Deletion time for elimination of the element or element set. If BOXID is nonzero, a TIME value of zero is restt to 1.0E+16.
        """ # nopep8
        return self._cards[0].get_value("time")

    @time.setter
    def time(self, value: float) -> None:
        """Set the time property."""
        self._cards[0].set_value("time", value)

    @property
    def boxid(self) -> typing.Optional[int]:
        """Get or set the Element inside or outside of defined box are deleted depending on the value INOUT
        """ # nopep8
        return self._cards[0].get_value("boxid")

    @boxid.setter
    def boxid(self, value: int) -> None:
        """Set the boxid property."""
        self._cards[0].set_value("boxid", value)

    @property
    def inout(self) -> int:
        """Get or set the Location of deleted element:
        EQ.0:Element inside box are deleted.
        EQ.1:Element outside of box are deleted
        """ # nopep8
        return self._cards[0].get_value("inout")

    @inout.setter
    def inout(self, value: int) -> None:
        """Set the inout property."""
        if value not in [0, 1, None]:
            raise Exception("""inout must be `None` or one of {0,1}.""")
        self._cards[0].set_value("inout", value)

    @property
    def idgrp(self) -> int:
        """Get or set the Group ID. Elements sharing the same positive value of IDGRP
        are considered to be in the same group. All elements in a group
        will be simultaneously deleted one cycle after a percentage of the elements (specified in PERCENT) fail.
        There is no requirement that each *DEFINE_ELEMENT_DEATH
        command have a unique IDGRP. In other words, elements in a
        single group can come from multiple *DEFINE_ELEMENT_DEATH commands.
        Elements in which IDGRP = 0 are not assigned to a group and
        thus deletion of one element does not cause deletion of the other elements.
        """ # nopep8
        return self._cards[0].get_value("idgrp")

    @idgrp.setter
    def idgrp(self, value: int) -> None:
        """Set the idgrp property."""
        self._cards[0].set_value("idgrp", value)

    @property
    def cid(self) -> int:
        """Get or set the Coordinate ID for transforming box BOXID. If CID is not
        specified, the box is in the global coordinate system. The box
        rotates and translates with the coordinate system only if the
        coordinate system is flagged for an update every time step
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def percent(self) -> float:
        """Get or set the Deletion percentage.
        EQ.0.0: When one element fails, all elements in the group will be deleted (default).
        GT.0.0: Percentage of elements failed before elements in group IDGRP are deleted
        """ # nopep8
        return self._cards[0].get_value("percent")

    @percent.setter
    def percent(self, value: float) -> None:
        """Set the percent property."""
        self._cards[0].set_value("percent", value)

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

