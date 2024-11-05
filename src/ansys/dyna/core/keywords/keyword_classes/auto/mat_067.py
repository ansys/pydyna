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

class Mat067(KeywordBase):
    """DYNA MAT_067 keyword"""

    keyword = "MAT"
    subkeyword = "067"
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
                        "mid",
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "lcidtr",
                        int,
                        20,
                        10,
                        kwargs.get("lcidtr")
                    ),
                    Field(
                        "lcidts",
                        int,
                        30,
                        10,
                        kwargs.get("lcidts")
                    ),
                    Field(
                        "lcidtt",
                        int,
                        40,
                        10,
                        kwargs.get("lcidtt")
                    ),
                    Field(
                        "lcidrr",
                        int,
                        50,
                        10,
                        kwargs.get("lcidrr")
                    ),
                    Field(
                        "lcidrs",
                        int,
                        60,
                        10,
                        kwargs.get("lcidrs")
                    ),
                    Field(
                        "lcidrt",
                        int,
                        70,
                        10,
                        kwargs.get("lcidrt")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcidtdr",
                        int,
                        0,
                        10,
                        kwargs.get("lcidtdr")
                    ),
                    Field(
                        "lcidtds",
                        int,
                        10,
                        10,
                        kwargs.get("lcidtds")
                    ),
                    Field(
                        "lcidtdt",
                        int,
                        20,
                        10,
                        kwargs.get("lcidtdt")
                    ),
                    Field(
                        "lcidrdr",
                        int,
                        30,
                        10,
                        kwargs.get("lcidrdr")
                    ),
                    Field(
                        "lcidrds",
                        int,
                        40,
                        10,
                        kwargs.get("lcidrds")
                    ),
                    Field(
                        "lcidrdt",
                        int,
                        50,
                        10,
                        kwargs.get("lcidrdt")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "for",
                        float,
                        0,
                        10,
                        kwargs.get("for")
                    ),
                    Field(
                        "fos",
                        float,
                        10,
                        10,
                        kwargs.get("fos")
                    ),
                    Field(
                        "fot",
                        float,
                        20,
                        10,
                        kwargs.get("fot")
                    ),
                    Field(
                        "mor",
                        float,
                        30,
                        10,
                        kwargs.get("mor")
                    ),
                    Field(
                        "mos",
                        float,
                        40,
                        10,
                        kwargs.get("mos")
                    ),
                    Field(
                        "mot",
                        float,
                        50,
                        10,
                        kwargs.get("mot")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ffailr",
                        float,
                        0,
                        10,
                        kwargs.get("ffailr")
                    ),
                    Field(
                        "ffails",
                        float,
                        10,
                        10,
                        kwargs.get("ffails")
                    ),
                    Field(
                        "ffailt",
                        float,
                        20,
                        10,
                        kwargs.get("ffailt")
                    ),
                    Field(
                        "mfailr",
                        float,
                        30,
                        10,
                        kwargs.get("mfailr")
                    ),
                    Field(
                        "mfails",
                        float,
                        40,
                        10,
                        kwargs.get("mfails")
                    ),
                    Field(
                        "mfailt",
                        float,
                        50,
                        10,
                        kwargs.get("mfailt")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "ufailr",
                        float,
                        0,
                        10,
                        kwargs.get("ufailr")
                    ),
                    Field(
                        "ufails",
                        float,
                        10,
                        10,
                        kwargs.get("ufails")
                    ),
                    Field(
                        "ufailt",
                        float,
                        20,
                        10,
                        kwargs.get("ufailt")
                    ),
                    Field(
                        "tfailr",
                        float,
                        30,
                        10,
                        kwargs.get("tfailr")
                    ),
                    Field(
                        "tfails",
                        float,
                        40,
                        10,
                        kwargs.get("tfails")
                    ),
                    Field(
                        "tfailt",
                        float,
                        50,
                        10,
                        kwargs.get("tfailt")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat067.option_specs[0],
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
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density, see also volume in *SECTION_BEAM definition.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def lcidtr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational force resultant along local r-axis versus relative translational displacement.
        """ # nopep8
        return self._cards[0].get_value("lcidtr")

    @lcidtr.setter
    def lcidtr(self, value: int) -> None:
        self._cards[0].set_value("lcidtr", value)

    @property
    def lcidts(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational force resultant along local s-axis versus relative translational displacement.
        """ # nopep8
        return self._cards[0].get_value("lcidts")

    @lcidts.setter
    def lcidts(self, value: int) -> None:
        self._cards[0].set_value("lcidts", value)

    @property
    def lcidtt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational force resultant along local t-axis versus relative translational displacement.
        """ # nopep8
        return self._cards[0].get_value("lcidtt")

    @lcidtt.setter
    def lcidtt(self, value: int) -> None:
        self._cards[0].set_value("lcidtt", value)

    @property
    def lcidrr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining rotational moment resultant about local r-axis versus relative rotational displacement.
        """ # nopep8
        return self._cards[0].get_value("lcidrr")

    @lcidrr.setter
    def lcidrr(self, value: int) -> None:
        self._cards[0].set_value("lcidrr", value)

    @property
    def lcidrs(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining rotational moment resultant about local s-axis versus relative rotational displacement.
        """ # nopep8
        return self._cards[0].get_value("lcidrs")

    @lcidrs.setter
    def lcidrs(self, value: int) -> None:
        self._cards[0].set_value("lcidrs", value)

    @property
    def lcidrt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining rotational moment resultant about local t-axis versus relative rotational displacement.
        """ # nopep8
        return self._cards[0].get_value("lcidrt")

    @lcidrt.setter
    def lcidrt(self, value: int) -> None:
        self._cards[0].set_value("lcidrt", value)

    @property
    def lcidtdr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational damping force resultant along local r-axis versus relative translational velocity.
        """ # nopep8
        return self._cards[1].get_value("lcidtdr")

    @lcidtdr.setter
    def lcidtdr(self, value: int) -> None:
        self._cards[1].set_value("lcidtdr", value)

    @property
    def lcidtds(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational damping force resultant along local s-axis versus relative translational velocity.
        """ # nopep8
        return self._cards[1].get_value("lcidtds")

    @lcidtds.setter
    def lcidtds(self, value: int) -> None:
        self._cards[1].set_value("lcidtds", value)

    @property
    def lcidtdt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining translational damping force resultant along local t-axis versus relative translational velocity.
        """ # nopep8
        return self._cards[1].get_value("lcidtdt")

    @lcidtdt.setter
    def lcidtdt(self, value: int) -> None:
        self._cards[1].set_value("lcidtdt", value)

    @property
    def lcidrdr(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining rotational damping moment resultant about local r-axis versus relative rotational velocity.
        """ # nopep8
        return self._cards[1].get_value("lcidrdr")

    @lcidrdr.setter
    def lcidrdr(self, value: int) -> None:
        self._cards[1].set_value("lcidrdr", value)

    @property
    def lcidrds(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining rotational damping moment resultant about local s-axis versus relative rotational velocity.
        """ # nopep8
        return self._cards[1].get_value("lcidrds")

    @lcidrds.setter
    def lcidrds(self, value: int) -> None:
        self._cards[1].set_value("lcidrds", value)

    @property
    def lcidrdt(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining rotational damping moment resultant about local t-axis versus relative rotational velocity.
        """ # nopep8
        return self._cards[1].get_value("lcidrdt")

    @lcidrdt.setter
    def lcidrdt(self, value: int) -> None:
        self._cards[1].set_value("lcidrdt", value)

    @property
    def for_(self) -> typing.Optional[float]:
        """Get or set the Preload force in r-direction
        """ # nopep8
        return self._cards[2].get_value("for")

    @for_.setter
    def for_(self, value: float) -> None:
        self._cards[2].set_value("for", value)

    @property
    def fos(self) -> typing.Optional[float]:
        """Get or set the Preload force in s-direction
        """ # nopep8
        return self._cards[2].get_value("fos")

    @fos.setter
    def fos(self, value: float) -> None:
        self._cards[2].set_value("fos", value)

    @property
    def fot(self) -> typing.Optional[float]:
        """Get or set the Preload force in t-direction
        """ # nopep8
        return self._cards[2].get_value("fot")

    @fot.setter
    def fot(self, value: float) -> None:
        self._cards[2].set_value("fot", value)

    @property
    def mor(self) -> typing.Optional[float]:
        """Get or set the Preload moment about r-axis
        """ # nopep8
        return self._cards[2].get_value("mor")

    @mor.setter
    def mor(self, value: float) -> None:
        self._cards[2].set_value("mor", value)

    @property
    def mos(self) -> typing.Optional[float]:
        """Get or set the Preload moment about s-axis
        """ # nopep8
        return self._cards[2].get_value("mos")

    @mos.setter
    def mos(self, value: float) -> None:
        self._cards[2].set_value("mos", value)

    @property
    def mot(self) -> typing.Optional[float]:
        """Get or set the Preload moment about t-axis
        """ # nopep8
        return self._cards[2].get_value("mot")

    @mot.setter
    def mot(self, value: float) -> None:
        self._cards[2].set_value("mot", value)

    @property
    def ffailr(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter. If zero, the corresponding force, Fr, is not considered in the failure calculation
        """ # nopep8
        return self._cards[3].get_value("ffailr")

    @ffailr.setter
    def ffailr(self, value: float) -> None:
        self._cards[3].set_value("ffailr", value)

    @property
    def ffails(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter. If zero, the corresponding force, Fs, is not considered in the failure calculation
        """ # nopep8
        return self._cards[3].get_value("ffails")

    @ffails.setter
    def ffails(self, value: float) -> None:
        self._cards[3].set_value("ffails", value)

    @property
    def ffailt(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter. If zero, the corresponding force, Ft, is not considered in the failure calculation
        """ # nopep8
        return self._cards[3].get_value("ffailt")

    @ffailt.setter
    def ffailt(self, value: float) -> None:
        self._cards[3].set_value("ffailt", value)

    @property
    def mfailr(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter. If zero, the corresponding moment, Mr, is not considered in the failure calculation
        """ # nopep8
        return self._cards[3].get_value("mfailr")

    @mfailr.setter
    def mfailr(self, value: float) -> None:
        self._cards[3].set_value("mfailr", value)

    @property
    def mfails(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter. If zero, the corresponding moment, Ms, is not considered in the failure calculation
        """ # nopep8
        return self._cards[3].get_value("mfails")

    @mfails.setter
    def mfails(self, value: float) -> None:
        self._cards[3].set_value("mfails", value)

    @property
    def mfailt(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter. If zero, the corresponding moment, Mt, is not considered in the failure calculation
        """ # nopep8
        return self._cards[3].get_value("mfailt")

    @mfailt.setter
    def mfailt(self, value: float) -> None:
        self._cards[3].set_value("mfailt", value)

    @property
    def ufailr(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter. If zero, the corresponding displacement, Ur, is not considered in the failure calculation
        """ # nopep8
        return self._cards[4].get_value("ufailr")

    @ufailr.setter
    def ufailr(self, value: float) -> None:
        self._cards[4].set_value("ufailr", value)

    @property
    def ufails(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter. If zero, the corresponding displacement, Us, is not considered in the failure calculation
        """ # nopep8
        return self._cards[4].get_value("ufails")

    @ufails.setter
    def ufails(self, value: float) -> None:
        self._cards[4].set_value("ufails", value)

    @property
    def ufailt(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter. If zero, the corresponding displacement, Ut, is not considered in the failure calculation
        """ # nopep8
        return self._cards[4].get_value("ufailt")

    @ufailt.setter
    def ufailt(self, value: float) -> None:
        self._cards[4].set_value("ufailt", value)

    @property
    def tfailr(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter. If zero, the corresponding rotation, Qr, is not considered in the failure calculation
        """ # nopep8
        return self._cards[4].get_value("tfailr")

    @tfailr.setter
    def tfailr(self, value: float) -> None:
        self._cards[4].set_value("tfailr", value)

    @property
    def tfails(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter. If zero, the corresponding rotation, Qs, is not considered in the failure calculation
        """ # nopep8
        return self._cards[4].get_value("tfails")

    @tfails.setter
    def tfails(self, value: float) -> None:
        self._cards[4].set_value("tfails", value)

    @property
    def tfailt(self) -> typing.Optional[float]:
        """Get or set the Optional failure parameter. If zero, the corresponding rotation, Qt, is not considered in the failure calculation
        """ # nopep8
        return self._cards[4].get_value("tfailt")

    @tfailt.setter
    def tfailt(self, value: float) -> None:
        self._cards[4].set_value("tfailt", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[5].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[5].cards[0].set_value("title", value)

