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

class MatNonlocal(KeywordBase):
    """DYNA MAT_NONLOCAL keyword"""

    keyword = "MAT"
    subkeyword = "NONLOCAL"
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
                        "idnl",
                        int,
                        0,
                        10,
                        kwargs.get("idnl")
                    ),
                    Field(
                        "pid",
                        int,
                        10,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "p",
                        int,
                        20,
                        10,
                        kwargs.get("p")
                    ),
                    Field(
                        "q",
                        int,
                        30,
                        10,
                        kwargs.get("q")
                    ),
                    Field(
                        "l",
                        float,
                        40,
                        10,
                        kwargs.get("l")
                    ),
                    Field(
                        "nfreq",
                        int,
                        50,
                        10,
                        kwargs.get("nfreq")
                    ),
                    Field(
                        "nhv",
                        int,
                        60,
                        10,
                        kwargs.get("nhv")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nl1",
                        int,
                        0,
                        10,
                        kwargs.get("nl1")
                    ),
                    Field(
                        "nl2",
                        int,
                        10,
                        10,
                        kwargs.get("nl2")
                    ),
                    Field(
                        "nl3",
                        int,
                        20,
                        10,
                        kwargs.get("nl3")
                    ),
                    Field(
                        "nl4",
                        int,
                        30,
                        10,
                        kwargs.get("nl4")
                    ),
                    Field(
                        "nl5",
                        int,
                        40,
                        10,
                        kwargs.get("nl5")
                    ),
                    Field(
                        "nl6",
                        int,
                        50,
                        10,
                        kwargs.get("nl6")
                    ),
                    Field(
                        "nl7",
                        int,
                        60,
                        10,
                        kwargs.get("nl7")
                    ),
                    Field(
                        "nl8",
                        int,
                        70,
                        10,
                        kwargs.get("nl8")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xc1",
                        float,
                        0,
                        10,
                        kwargs.get("xc1")
                    ),
                    Field(
                        "yc1",
                        float,
                        10,
                        10,
                        kwargs.get("yc1")
                    ),
                    Field(
                        "zc1",
                        float,
                        20,
                        10,
                        kwargs.get("zc1")
                    ),
                    Field(
                        "xc2",
                        float,
                        30,
                        10,
                        kwargs.get("xc2")
                    ),
                    Field(
                        "yc2",
                        float,
                        40,
                        10,
                        kwargs.get("yc2")
                    ),
                    Field(
                        "zc2",
                        float,
                        50,
                        10,
                        kwargs.get("zc2")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = MatNonlocal.option_specs[0],
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
    def idnl(self) -> typing.Optional[int]:
        """Get or set the Nonlocal material input ID
        """ # nopep8
        return self._cards[0].get_value("idnl")

    @idnl.setter
    def idnl(self, value: int) -> None:
        self._cards[0].set_value("idnl", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID for nonlocal material
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def p(self) -> typing.Optional[int]:
        """Get or set the Exponent of weighting function. A typical value might be 8 depending somewhat on the choice of L. See equations in keyword user's manual.
        """ # nopep8
        return self._cards[0].get_value("p")

    @p.setter
    def p(self, value: int) -> None:
        self._cards[0].set_value("p", value)

    @property
    def q(self) -> typing.Optional[int]:
        """Get or set the Exponent of weighting function. A typical value might be 2. See equations in keyword user's manual.
        """ # nopep8
        return self._cards[0].get_value("q")

    @q.setter
    def q(self, value: int) -> None:
        self._cards[0].set_value("q", value)

    @property
    def l(self) -> typing.Optional[float]:
        """Get or set the Characteristic length.  This length should span a few elements. See equations in keyword user's manual.
        """ # nopep8
        return self._cards[0].get_value("l")

    @l.setter
    def l(self, value: float) -> None:
        self._cards[0].set_value("l", value)

    @property
    def nfreq(self) -> typing.Optional[int]:
        """Get or set the Number of time steps between update of neighbours. The nearest neighbour search can add significant computational time so it is suggested that NFREQ be set to value of 10 to 100 depending on the problem. This parameter may be somewhat problem dependent.
        """ # nopep8
        return self._cards[0].get_value("nfreq")

    @nfreq.setter
    def nfreq(self, value: int) -> None:
        self._cards[0].set_value("nfreq", value)

    @property
    def nhv(self) -> typing.Optional[int]:
        """Get or set the Define the number of history variables for nonlocal treatment..
        """ # nopep8
        return self._cards[0].get_value("nhv")

    @nhv.setter
    def nhv(self, value: int) -> None:
        self._cards[0].set_value("nhv", value)

    @property
    def nl1(self) -> typing.Optional[int]:
        """Get or set the 1st history variable ID for nonlocal treatment
        """ # nopep8
        return self._cards[1].get_value("nl1")

    @nl1.setter
    def nl1(self, value: int) -> None:
        self._cards[1].set_value("nl1", value)

    @property
    def nl2(self) -> typing.Optional[int]:
        """Get or set the 2nd history variable ID's for nonlocal treatment
        """ # nopep8
        return self._cards[1].get_value("nl2")

    @nl2.setter
    def nl2(self, value: int) -> None:
        self._cards[1].set_value("nl2", value)

    @property
    def nl3(self) -> typing.Optional[int]:
        """Get or set the 3rd history variable ID's for nonlocal treatment
        """ # nopep8
        return self._cards[1].get_value("nl3")

    @nl3.setter
    def nl3(self, value: int) -> None:
        self._cards[1].set_value("nl3", value)

    @property
    def nl4(self) -> typing.Optional[int]:
        """Get or set the 4th history variable ID's for nonlocal treatment
        """ # nopep8
        return self._cards[1].get_value("nl4")

    @nl4.setter
    def nl4(self, value: int) -> None:
        self._cards[1].set_value("nl4", value)

    @property
    def nl5(self) -> typing.Optional[int]:
        """Get or set the 5th history variable ID's for nonlocal treatment
        """ # nopep8
        return self._cards[1].get_value("nl5")

    @nl5.setter
    def nl5(self, value: int) -> None:
        self._cards[1].set_value("nl5", value)

    @property
    def nl6(self) -> typing.Optional[int]:
        """Get or set the 6th history variable ID's for nonlocal treatment
        """ # nopep8
        return self._cards[1].get_value("nl6")

    @nl6.setter
    def nl6(self, value: int) -> None:
        self._cards[1].set_value("nl6", value)

    @property
    def nl7(self) -> typing.Optional[int]:
        """Get or set the 7th history variable ID's for nonlocal treatment
        """ # nopep8
        return self._cards[1].get_value("nl7")

    @nl7.setter
    def nl7(self, value: int) -> None:
        self._cards[1].set_value("nl7", value)

    @property
    def nl8(self) -> typing.Optional[int]:
        """Get or set the 8th history variable ID's for nonlocal treatment
        """ # nopep8
        return self._cards[1].get_value("nl8")

    @nl8.setter
    def nl8(self, value: int) -> None:
        self._cards[1].set_value("nl8", value)

    @property
    def xc1(self) -> typing.Optional[float]:
        """Get or set the X-coordinate of point on symmetry plane.
        """ # nopep8
        return self._cards[2].get_value("xc1")

    @xc1.setter
    def xc1(self, value: float) -> None:
        self._cards[2].set_value("xc1", value)

    @property
    def yc1(self) -> typing.Optional[float]:
        """Get or set the Y-coordinate of point on symmetry plane.
        """ # nopep8
        return self._cards[2].get_value("yc1")

    @yc1.setter
    def yc1(self, value: float) -> None:
        self._cards[2].set_value("yc1", value)

    @property
    def zc1(self) -> typing.Optional[float]:
        """Get or set the Z-coordinate of point on symmetry plane.
        """ # nopep8
        return self._cards[2].get_value("zc1")

    @zc1.setter
    def zc1(self, value: float) -> None:
        self._cards[2].set_value("zc1", value)

    @property
    def xc2(self) -> typing.Optional[float]:
        """Get or set the X-coordinate of a point along the normal vector.
        """ # nopep8
        return self._cards[2].get_value("xc2")

    @xc2.setter
    def xc2(self, value: float) -> None:
        self._cards[2].set_value("xc2", value)

    @property
    def yc2(self) -> typing.Optional[float]:
        """Get or set the Y-coordinate of a point along the normal vector.
        """ # nopep8
        return self._cards[2].get_value("yc2")

    @yc2.setter
    def yc2(self, value: float) -> None:
        self._cards[2].set_value("yc2", value)

    @property
    def zc2(self) -> typing.Optional[float]:
        """Get or set the Z-coordinate of a point along the normal vector.
        """ # nopep8
        return self._cards[2].get_value("zc2")

    @zc2.setter
    def zc2(self, value: float) -> None:
        self._cards[2].set_value("zc2", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

