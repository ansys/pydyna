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

"""Module providing the SetPartCollect class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

class SetPartCollect(KeywordBase):
    """DYNA SET_PART_COLLECT keyword"""

    keyword = "SET"
    subkeyword = "PART_COLLECT"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the SetPartCollect class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card(
                [
                    Field(
                        "sid",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "da1",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "da2",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "da3",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "da4",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "solver",
                        str,
                        50,
                        10,
                        "MECH",
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pid1",
                        int,
                        0,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pid2",
                        int,
                        10,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pid3",
                        int,
                        20,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pid4",
                        int,
                        30,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pid5",
                        int,
                        40,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pid6",
                        int,
                        50,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pid7",
                        int,
                        60,
                        10,
                        **kwargs,
                    ),
                    Field(
                        "pid8",
                        int,
                        70,
                        10,
                        **kwargs,
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SetPartCollect.option_specs[0],
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
    def sid(self) -> typing.Optional[int]:
        """Get or set the Part set ID. All part sets should have a unique set ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        """Set the sid property."""
        self._cards[0].set_value("sid", value)

    @property
    def da1(self) -> float:
        """Get or set the First attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da1")

    @da1.setter
    def da1(self, value: float) -> None:
        """Set the da1 property."""
        self._cards[0].set_value("da1", value)

    @property
    def da2(self) -> float:
        """Get or set the Second attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da2")

    @da2.setter
    def da2(self, value: float) -> None:
        """Set the da2 property."""
        self._cards[0].set_value("da2", value)

    @property
    def da3(self) -> float:
        """Get or set the Third attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da3")

    @da3.setter
    def da3(self, value: float) -> None:
        """Set the da3 property."""
        self._cards[0].set_value("da3", value)

    @property
    def da4(self) -> float:
        """Get or set the Fourth attribute default value is 0.0.
        """ # nopep8
        return self._cards[0].get_value("da4")

    @da4.setter
    def da4(self, value: float) -> None:
        """Set the da4 property."""
        self._cards[0].set_value("da4", value)

    @property
    def solver(self) -> str:
        """Get or set the EQ.MECH: mechanics.
        EQ.CESE: CE/SE compressible fluid flow solver.
        EQ.ICFD: Incompressible fluid flow solver.
        """ # nopep8
        return self._cards[0].get_value("solver")

    @solver.setter
    def solver(self, value: str) -> None:
        """Set the solver property."""
        if value not in ["MECH", "CESE", "ICFD", None]:
            raise Exception("""solver must be `None` or one of {"MECH","CESE","ICFD"}.""")
        self._cards[0].set_value("solver", value)

    @property
    def pid1(self) -> typing.Optional[int]:
        """Get or set the First part ID of the set.
        """ # nopep8
        return self._cards[1].get_value("pid1")

    @pid1.setter
    def pid1(self, value: int) -> None:
        """Set the pid1 property."""
        self._cards[1].set_value("pid1", value)

    @property
    def pid2(self) -> typing.Optional[int]:
        """Get or set the Second part ID of the set.
        """ # nopep8
        return self._cards[1].get_value("pid2")

    @pid2.setter
    def pid2(self, value: int) -> None:
        """Set the pid2 property."""
        self._cards[1].set_value("pid2", value)

    @property
    def pid3(self) -> typing.Optional[int]:
        """Get or set the Third part ID of the set.
        """ # nopep8
        return self._cards[1].get_value("pid3")

    @pid3.setter
    def pid3(self, value: int) -> None:
        """Set the pid3 property."""
        self._cards[1].set_value("pid3", value)

    @property
    def pid4(self) -> typing.Optional[int]:
        """Get or set the Fourth part ID of the set.
        """ # nopep8
        return self._cards[1].get_value("pid4")

    @pid4.setter
    def pid4(self, value: int) -> None:
        """Set the pid4 property."""
        self._cards[1].set_value("pid4", value)

    @property
    def pid5(self) -> typing.Optional[int]:
        """Get or set the Fifth part ID of the set.
        """ # nopep8
        return self._cards[1].get_value("pid5")

    @pid5.setter
    def pid5(self, value: int) -> None:
        """Set the pid5 property."""
        self._cards[1].set_value("pid5", value)

    @property
    def pid6(self) -> typing.Optional[int]:
        """Get or set the Sixth part ID of the set.
        """ # nopep8
        return self._cards[1].get_value("pid6")

    @pid6.setter
    def pid6(self, value: int) -> None:
        """Set the pid6 property."""
        self._cards[1].set_value("pid6", value)

    @property
    def pid7(self) -> typing.Optional[int]:
        """Get or set the Seventh part ID of the set.
        """ # nopep8
        return self._cards[1].get_value("pid7")

    @pid7.setter
    def pid7(self, value: int) -> None:
        """Set the pid7 property."""
        self._cards[1].set_value("pid7", value)

    @property
    def pid8(self) -> typing.Optional[int]:
        """Get or set the Eighth part ID of the set.
        """ # nopep8
        return self._cards[1].get_value("pid8")

    @pid8.setter
    def pid8(self, value: int) -> None:
        """Set the pid8 property."""
        self._cards[1].set_value("pid8", value)

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

