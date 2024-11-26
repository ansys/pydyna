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

class SetBeamGeneralCollect(KeywordBase):
    """DYNA SET_BEAM_GENERAL_COLLECT keyword"""

    keyword = "SET"
    subkeyword = "BEAM_GENERAL_COLLECT"
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
                        "sid",
                        int,
                        0,
                        10,
                        kwargs.get("sid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "option",
                        str,
                        0,
                        10,
                        kwargs.get("option", "ALL")
                    ),
                    Field(
                        "e1",
                        int,
                        10,
                        10,
                        kwargs.get("e1")
                    ),
                    Field(
                        "e2",
                        int,
                        20,
                        10,
                        kwargs.get("e2")
                    ),
                    Field(
                        "e3",
                        int,
                        30,
                        10,
                        kwargs.get("e3")
                    ),
                    Field(
                        "e4",
                        int,
                        40,
                        10,
                        kwargs.get("e4")
                    ),
                    Field(
                        "e5",
                        int,
                        50,
                        10,
                        kwargs.get("e5")
                    ),
                    Field(
                        "e6",
                        int,
                        60,
                        10,
                        kwargs.get("e6")
                    ),
                    Field(
                        "e7",
                        int,
                        70,
                        10,
                        kwargs.get("e7")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SetBeamGeneralCollect.option_specs[0],
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
        """Get or set the Beam element set ID.
        """ # nopep8
        return self._cards[0].get_value("sid")

    @sid.setter
    def sid(self, value: int) -> None:
        self._cards[0].set_value("sid", value)

    @property
    def option(self) -> str:
        """Get or set the OPTION.EQ.ALL: All beam elements will be included in the set,
        OPTION.EQ.ELEM: Beam elements E1...E7 will be included in the current set,
        OPTION.EQ.DELEM: Beam elements E1...E7 previously added will be excluded from the current set,
        OPTION.EQ.PART: Beam elements from parts E1...E7 will be included in the current set,
        OPTION.EQ.DPART: Beam elements from parts E1...E7 previously added will be excluded from the current set,
        OPTION.EQ.BOX: Beam elements inside boxes E1...E7 will be included in the current set,
        OPTION.EQ.DBOX: Beam elements inside boxes E1...E7 previously added will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("option")

    @option.setter
    def option(self, value: str) -> None:
        if value not in ["ALL", "ELEM", "DELEM", "PART", "DPART", "BOX", "DBOX"]:
            raise Exception("""option must be one of {"ALL","ELEM","DELEM","PART","DPART","BOX","DBOX"}""")
        self._cards[1].set_value("option", value)

    @property
    def e1(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E1 not used,
        OPTION.EQ.ELEM: Beam element E1 will be included in the current set,
        OPTION.EQ.DELEM: Beam element E1 will be excluded from the current set,
        OPTION.EQ.PART: Beam elements from part E1 will be included in the current set,
        OPTION.EQ.DPART: Beam elements from part E1 will be excluded from the current set,
        OPTION.EQ.BOX: Beam elements inside box E1 will be included in the current set,
        OPTION.EQ.DBOX: Beam elements inside box E1 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e1")

    @e1.setter
    def e1(self, value: int) -> None:
        self._cards[1].set_value("e1", value)

    @property
    def e2(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E2 not used,
        OPTION.EQ.ELEM: Beam element E2 will be included in the current set,
        OPTION.EQ.DELEM: Beam element E2 will be excluded from the current set,
        OPTION.EQ.PART: Beam elements from part E2 will be included in the current set,
        OPTION.EQ.DPART: Beam elements from part E2 will be excluded from the current set,
        OPTION.EQ.BOX: Beam elements inside box E2 will be included in the current set,
        OPTION.EQ.DBOX: Beam elements inside box E2 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e2")

    @e2.setter
    def e2(self, value: int) -> None:
        self._cards[1].set_value("e2", value)

    @property
    def e3(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E3 not used,
        OPTION.EQ.ELEM: Beam element E3 will be included in the current set,
        OPTION.EQ.DELEM: Beam element E3 will be excluded from the current set,
        OPTION.EQ.PART: Beam elements from part E3 will be included in the current set,
        OPTION.EQ.DPART: Beam elements from part E3 will be excluded from the current set,
        OPTION.EQ.BOX: Beam elements inside box E3 will be included in the current set,
        OPTION.EQ.DBOX: Beam elements inside box E3 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e3")

    @e3.setter
    def e3(self, value: int) -> None:
        self._cards[1].set_value("e3", value)

    @property
    def e4(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E4 not used,
        OPTION.EQ.ELEM: Beam element E4 will be included in the current set,
        OPTION.EQ.DELEM: Beam element E4 will be excluded from the current set,
        OPTION.EQ.PART: Beam elements from part E4 will be included in the current set,
        OPTION.EQ.DPART: Beam elements from part E4 will be excluded from the current set,
        OPTION.EQ.BOX: Beam elements inside box E4 will be included in the current set,
        OPTION.EQ.DBOX: Beam elements inside box E4 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e4")

    @e4.setter
    def e4(self, value: int) -> None:
        self._cards[1].set_value("e4", value)

    @property
    def e5(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E5 not used,
        OPTION.EQ.ELEM: Beam element E5 will be included in the current set,
        OPTION.EQ.DELEM: Beam element E5 will be excluded from the current set,
        OPTION.EQ.PART: Beam elements from part E5 will be included in the current set,
        OPTION.EQ.DPART: Beam elements from part E5 will be excluded from the current set,
        OPTION.EQ.BOX: Beam elements inside box E5 will be included in the current set,
        OPTION.EQ.DBOX: Beam elements inside box E5 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e5")

    @e5.setter
    def e5(self, value: int) -> None:
        self._cards[1].set_value("e5", value)

    @property
    def e6(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E6 not used,
        OPTION.EQ.ELEM: Beam element E6 will be included in the current set,
        OPTION.EQ.DELEM: Beam element E6 will be excluded from the current set,
        OPTION.EQ.PART: Beam elements from part E6 will be included in the current set,
        OPTION.EQ.DPART: Beam elements from part E6 will be excluded from the current set,
        OPTION.EQ.BOX: Beam elements inside box E6 will be included in the current set,
        OPTION.EQ.DBOX: Beam elements inside box E6 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e6")

    @e6.setter
    def e6(self, value: int) -> None:
        self._cards[1].set_value("e6", value)

    @property
    def e7(self) -> typing.Optional[int]:
        """Get or set the OPTION.EQ.ALL: E7 not used,
        OPTION.EQ.ELEM: Beam element E7 will be included in the current set,
        OPTION.EQ.DELEM: Beam element E7 will be excluded from the current set,
        OPTION.EQ.PART: Beam elements from part E7 will be included in the current set,
        OPTION.EQ.DPART: Beam elements from part E7 will be excluded from the current set,
        OPTION.EQ.BOX: Beam elements inside box E7 will be included in the current set,
        OPTION.EQ.DBOX: Beam elements inside box E7 will be excluded from the current set.
        """ # nopep8
        return self._cards[1].get_value("e7")

    @e7.setter
    def e7(self, value: int) -> None:
        self._cards[1].set_value("e7", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

