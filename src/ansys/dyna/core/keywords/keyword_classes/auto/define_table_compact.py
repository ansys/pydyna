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

class DefineTableCompact(KeywordBase):
    """DYNA DEFINE_TABLE_COMPACT keyword"""

    keyword = "DEFINE"
    subkeyword = "TABLE_COMPACT"
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
                        "tbid",
                        int,
                        0,
                        10,
                        kwargs.get("tbid")
                    ),
                    Field(
                        "nvar",
                        int,
                        10,
                        10,
                        kwargs.get("nvar")
                    ),
                    Field(
                        "lcint",
                        int,
                        20,
                        10,
                        kwargs.get("lcint", 0)
                    ),
                    Field(
                        "mathis",
                        int,
                        30,
                        10,
                        kwargs.get("mathis", 0)
                    ),
                    Field(
                        "inexect",
                        int,
                        40,
                        10,
                        kwargs.get("inexect", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        70,
                        10,
                        kwargs.get("unused")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        int,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "his1",
                        int,
                        10,
                        10,
                        kwargs.get("his1")
                    ),
                    Field(
                        "his2",
                        int,
                        20,
                        10,
                        kwargs.get("his2")
                    ),
                    Field(
                        "his3",
                        int,
                        30,
                        10,
                        kwargs.get("his3")
                    ),
                    Field(
                        "his4",
                        int,
                        40,
                        10,
                        kwargs.get("his4")
                    ),
                    Field(
                        "his5",
                        int,
                        50,
                        10,
                        kwargs.get("his5")
                    ),
                    Field(
                        "his6",
                        int,
                        60,
                        10,
                        kwargs.get("his6")
                    ),
                    Field(
                        "his7",
                        int,
                        70,
                        10,
                        kwargs.get("his7")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        int,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "his8",
                        int,
                        10,
                        10,
                        kwargs.get("his8")
                    ),
                    Field(
                        "his9",
                        int,
                        20,
                        10,
                        kwargs.get("his9")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        int,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "ixe1",
                        int,
                        10,
                        10,
                        kwargs.get("ixe1")
                    ),
                    Field(
                        "ixe2",
                        int,
                        20,
                        10,
                        kwargs.get("ixe2")
                    ),
                    Field(
                        "ixe3",
                        int,
                        30,
                        10,
                        kwargs.get("ixe3")
                    ),
                    Field(
                        "ixe4",
                        int,
                        40,
                        10,
                        kwargs.get("ixe4")
                    ),
                    Field(
                        "ixe5",
                        int,
                        50,
                        10,
                        kwargs.get("ixe5")
                    ),
                    Field(
                        "ixe6",
                        int,
                        60,
                        10,
                        kwargs.get("ixe6")
                    ),
                    Field(
                        "ixe7",
                        int,
                        70,
                        10,
                        kwargs.get("ixe7")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        int,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "ixe8",
                        int,
                        10,
                        10,
                        kwargs.get("ixe8")
                    ),
                    Field(
                        "ixe9",
                        int,
                        20,
                        10,
                        kwargs.get("ixe9")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "o1",
                        float,
                        0,
                        10,
                        kwargs.get("o1")
                    ),
                    Field(
                        "a1.1",
                        float,
                        10,
                        10,
                        kwargs.get("a1.1")
                    ),
                    Field(
                        "a1.2",
                        float,
                        20,
                        10,
                        kwargs.get("a1.2")
                    ),
                    Field(
                        "a1.3",
                        float,
                        30,
                        10,
                        kwargs.get("a1.3")
                    ),
                    Field(
                        "a1.4",
                        float,
                        40,
                        10,
                        kwargs.get("a1.4")
                    ),
                    Field(
                        "a1.5",
                        float,
                        50,
                        10,
                        kwargs.get("a1.5")
                    ),
                    Field(
                        "a1.6",
                        float,
                        60,
                        10,
                        kwargs.get("a1.6")
                    ),
                    Field(
                        "a1.7",
                        float,
                        70,
                        10,
                        kwargs.get("a1.7")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "unused",
                        int,
                        0,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "a1.8",
                        float,
                        10,
                        10,
                        kwargs.get("a1.8")
                    ),
                    Field(
                        "a1.9",
                        float,
                        20,
                        10,
                        kwargs.get("a1.9")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineTableCompact.option_specs[0],
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
    def tbid(self) -> typing.Optional[int]:
        """Get or set the Table ID. Table ID.  Tables and load curves may not share common IDs.  LS DYNA allows load curve IDs and table IDs to be used interchangeably
        """ # nopep8
        return self._cards[0].get_value("tbid")

    @tbid.setter
    def tbid(self, value: int) -> None:
        self._cards[0].set_value("tbid", value)

    @property
    def nvar(self) -> typing.Optional[int]:
        """Get or set the Number of variables (dimension of the table). Current maximum is 9.
        """ # nopep8
        return self._cards[0].get_value("nvar")

    @nvar.setter
    def nvar(self, value: int) -> None:
        self._cards[0].set_value("nvar", value)

    @property
    def lcint(self) -> int:
        """Get or set the Number of discretization points.
        EQ.0:	Value of LCINT from * CONTROL_‌SOLUTION will be used.
        """ # nopep8
        return self._cards[0].get_value("lcint")

    @lcint.setter
    def lcint(self, value: int) -> None:
        self._cards[0].set_value("lcint", value)

    @property
    def mathis(self) -> int:
        """Get or set the Material history flag. Option to identify the abscissa variables as a specified  history variable number(s) (see Remarks 3 and 6). Additional Card 2 (and possibly Card 2.1) is read if this option is active.
        EQ.0:	Off
        EQ.1 : On.
        """ # nopep8
        return self._cards[0].get_value("mathis")

    @mathis.setter
    def mathis(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""mathis must be one of {0,1}""")
        self._cards[0].set_value("mathis", value)

    @property
    def inexect(self) -> int:
        """Get or set the Extra curve settings flag. Option to assign settings about curve discretization, extrapolation and interpolation for each abscissa variable. Additional Card 3 (and possibly Card 3.1) is read if this option is active.
        EQ.0:	Off
        EQ.1 : On.
        """ # nopep8
        return self._cards[0].get_value("inexect")

    @inexect.setter
    def inexect(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""inexect must be one of {0,1}""")
        self._cards[0].set_value("inexect", value)

    @property
    def his1(self) -> typing.Optional[int]:
        """Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
        """ # nopep8
        return self._cards[1].get_value("his1")

    @his1.setter
    def his1(self, value: int) -> None:
        self._cards[1].set_value("his1", value)

    @property
    def his2(self) -> typing.Optional[int]:
        """Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
        """ # nopep8
        return self._cards[1].get_value("his2")

    @his2.setter
    def his2(self, value: int) -> None:
        self._cards[1].set_value("his2", value)

    @property
    def his3(self) -> typing.Optional[int]:
        """Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
        """ # nopep8
        return self._cards[1].get_value("his3")

    @his3.setter
    def his3(self, value: int) -> None:
        self._cards[1].set_value("his3", value)

    @property
    def his4(self) -> typing.Optional[int]:
        """Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
        """ # nopep8
        return self._cards[1].get_value("his4")

    @his4.setter
    def his4(self, value: int) -> None:
        self._cards[1].set_value("his4", value)

    @property
    def his5(self) -> typing.Optional[int]:
        """Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
        """ # nopep8
        return self._cards[1].get_value("his5")

    @his5.setter
    def his5(self, value: int) -> None:
        self._cards[1].set_value("his5", value)

    @property
    def his6(self) -> typing.Optional[int]:
        """Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
        """ # nopep8
        return self._cards[1].get_value("his6")

    @his6.setter
    def his6(self, value: int) -> None:
        self._cards[1].set_value("his6", value)

    @property
    def his7(self) -> typing.Optional[int]:
        """Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
        """ # nopep8
        return self._cards[1].get_value("his7")

    @his7.setter
    def his7(self, value: int) -> None:
        self._cards[1].set_value("his7", value)

    @property
    def his8(self) -> typing.Optional[int]:
        """Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
        """ # nopep8
        return self._cards[2].get_value("his8")

    @his8.setter
    def his8(self, value: int) -> None:
        self._cards[2].set_value("his8", value)

    @property
    def his9(self) -> typing.Optional[int]:
        """Get or set the History variable numbers which indicate that variable X is HISX. For instance, setting HIS2 = 6 indicates that variable 2 is history variable 6 for the material.  A value of 0 indicates that the abscissa variable is not a history variable.
        """ # nopep8
        return self._cards[2].get_value("his9")

    @his9.setter
    def his9(self, value: int) -> None:
        self._cards[2].set_value("his9", value)

    @property
    def ixe1(self) -> typing.Optional[int]:
        """Get or set the Extra settings assigned to abscissa values.
        """ # nopep8
        return self._cards[3].get_value("ixe1")

    @ixe1.setter
    def ixe1(self, value: int) -> None:
        self._cards[3].set_value("ixe1", value)

    @property
    def ixe2(self) -> typing.Optional[int]:
        """Get or set the Extra settings assigned to abscissa values.
        """ # nopep8
        return self._cards[3].get_value("ixe2")

    @ixe2.setter
    def ixe2(self, value: int) -> None:
        self._cards[3].set_value("ixe2", value)

    @property
    def ixe3(self) -> typing.Optional[int]:
        """Get or set the Extra settings assigned to abscissa values.
        """ # nopep8
        return self._cards[3].get_value("ixe3")

    @ixe3.setter
    def ixe3(self, value: int) -> None:
        self._cards[3].set_value("ixe3", value)

    @property
    def ixe4(self) -> typing.Optional[int]:
        """Get or set the Extra settings assigned to abscissa values.
        """ # nopep8
        return self._cards[3].get_value("ixe4")

    @ixe4.setter
    def ixe4(self, value: int) -> None:
        self._cards[3].set_value("ixe4", value)

    @property
    def ixe5(self) -> typing.Optional[int]:
        """Get or set the Extra settings assigned to abscissa values.
        """ # nopep8
        return self._cards[3].get_value("ixe5")

    @ixe5.setter
    def ixe5(self, value: int) -> None:
        self._cards[3].set_value("ixe5", value)

    @property
    def ixe6(self) -> typing.Optional[int]:
        """Get or set the Extra settings assigned to abscissa values.
        """ # nopep8
        return self._cards[3].get_value("ixe6")

    @ixe6.setter
    def ixe6(self, value: int) -> None:
        self._cards[3].set_value("ixe6", value)

    @property
    def ixe7(self) -> typing.Optional[int]:
        """Get or set the Extra settings assigned to abscissa values.
        """ # nopep8
        return self._cards[3].get_value("ixe7")

    @ixe7.setter
    def ixe7(self, value: int) -> None:
        self._cards[3].set_value("ixe7", value)

    @property
    def ixe8(self) -> typing.Optional[int]:
        """Get or set the Extra settings assigned to abscissa values.
        """ # nopep8
        return self._cards[4].get_value("ixe8")

    @ixe8.setter
    def ixe8(self, value: int) -> None:
        self._cards[4].set_value("ixe8", value)

    @property
    def ixe9(self) -> typing.Optional[int]:
        """Get or set the Extra settings assigned to abscissa values.
        """ # nopep8
        return self._cards[4].get_value("ixe9")

    @ixe9.setter
    def ixe9(self, value: int) -> None:
        self._cards[4].set_value("ixe9", value)

    @property
    def o1(self) -> typing.Optional[float]:
        """Get or set the Ordinate (function) values..
        """ # nopep8
        return self._cards[5].get_value("o1")

    @o1.setter
    def o1(self, value: float) -> None:
        self._cards[5].set_value("o1", value)

    @property
    def a1_1(self) -> typing.Optional[float]:
        """Get or set the Abscissa values of variable X.
        """ # nopep8
        return self._cards[5].get_value("a1.1")

    @a1_1.setter
    def a1_1(self, value: float) -> None:
        self._cards[5].set_value("a1.1", value)

    @property
    def a1_2(self) -> typing.Optional[float]:
        """Get or set the Abscissa values of variable X.
        """ # nopep8
        return self._cards[5].get_value("a1.2")

    @a1_2.setter
    def a1_2(self, value: float) -> None:
        self._cards[5].set_value("a1.2", value)

    @property
    def a1_3(self) -> typing.Optional[float]:
        """Get or set the Abscissa values of variable X.
        """ # nopep8
        return self._cards[5].get_value("a1.3")

    @a1_3.setter
    def a1_3(self, value: float) -> None:
        self._cards[5].set_value("a1.3", value)

    @property
    def a1_4(self) -> typing.Optional[float]:
        """Get or set the Abscissa values of variable X.
        """ # nopep8
        return self._cards[5].get_value("a1.4")

    @a1_4.setter
    def a1_4(self, value: float) -> None:
        self._cards[5].set_value("a1.4", value)

    @property
    def a1_5(self) -> typing.Optional[float]:
        """Get or set the Abscissa values of variable X.
        """ # nopep8
        return self._cards[5].get_value("a1.5")

    @a1_5.setter
    def a1_5(self, value: float) -> None:
        self._cards[5].set_value("a1.5", value)

    @property
    def a1_6(self) -> typing.Optional[float]:
        """Get or set the Abscissa values of variable X.
        """ # nopep8
        return self._cards[5].get_value("a1.6")

    @a1_6.setter
    def a1_6(self, value: float) -> None:
        self._cards[5].set_value("a1.6", value)

    @property
    def a1_7(self) -> typing.Optional[float]:
        """Get or set the Abscissa values of variable X.
        """ # nopep8
        return self._cards[5].get_value("a1.7")

    @a1_7.setter
    def a1_7(self, value: float) -> None:
        self._cards[5].set_value("a1.7", value)

    @property
    def a1_8(self) -> typing.Optional[float]:
        """Get or set the Abscissa values of variable X.
        """ # nopep8
        return self._cards[6].get_value("a1.8")

    @a1_8.setter
    def a1_8(self, value: float) -> None:
        self._cards[6].set_value("a1.8", value)

    @property
    def a1_9(self) -> typing.Optional[float]:
        """Get or set the Abscissa values of variable X.
        """ # nopep8
        return self._cards[6].get_value("a1.9")

    @a1_9.setter
    def a1_9(self, value: float) -> None:
        self._cards[6].set_value("a1.9", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[7].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[7].cards[0].set_value("title", value)

