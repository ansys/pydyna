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
from ansys.dyna.core.lib.keyword_base import KeywordBase

class DatabaseElout(KeywordBase):
    """DYNA DATABASE_ELOUT keyword"""

    keyword = "DATABASE"
    subkeyword = "ELOUT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "dt",
                        float,
                        0,
                        10,
                        kwargs.get("dt", 0.0)
                    ),
                    Field(
                        "binary",
                        int,
                        10,
                        10,
                        kwargs.get("binary", 0)
                    ),
                    Field(
                        "lcur",
                        int,
                        20,
                        10,
                        kwargs.get("lcur", 0)
                    ),
                    Field(
                        "ioopt",
                        int,
                        30,
                        10,
                        kwargs.get("ioopt", 1)
                    ),
                    Field(
                        "option1",
                        int,
                        40,
                        10,
                        kwargs.get("option1", 0)
                    ),
                    Field(
                        "option2",
                        int,
                        50,
                        10,
                        kwargs.get("option2", 0)
                    ),
                    Field(
                        "option3",
                        int,
                        60,
                        10,
                        kwargs.get("option3", 0)
                    ),
                    Field(
                        "option4",
                        int,
                        70,
                        10,
                        kwargs.get("option4", 0)
                    ),
                ],
            ),
        ]

    @property
    def dt(self) -> float:
        """Get or set the Time interval between outputs. If DT is zero, no output is printed, This field will be used for all selected ASCII_options that have no unique DT value specified
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: float) -> None:
        self._cards[0].set_value("dt", value)

    @property
    def binary(self) -> int:
        """Get or set the Flag for binary file
        EQ.1: ASCII file is written. This is the default on serial and shared memory computers.
        EQ.2:Data written to a binary database, which contains data that would otherwise be output to the ASCII file. The ASCII file in this case is not created. This is the default on distributed memory computers.
        EQ.3: ASCII file is written and the data is also written to the binary database.
        """ # nopep8
        return self._cards[0].get_value("binary")

    @binary.setter
    def binary(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""binary must be one of {0,1,2,3}""")
        self._cards[0].set_value("binary", value)

    @property
    def lcur(self) -> int:
        """Get or set the Optional load curveid specifying time interval between dumps.
        """ # nopep8
        return self._cards[0].get_value("lcur")

    @lcur.setter
    def lcur(self, value: int) -> None:
        self._cards[0].set_value("lcur", value)

    @property
    def ioopt(self) -> int:
        """Get or set the Flag to govern behavior of the plot frequency load curve defined by LCUR:
        EQ.1: At the time each plot is generated, the load curve value is added to the current time to determine the next plot time. (default)
        EQ.2: At the time each plot is generated, the next plot time T is computed so that T = the current time plus the load curve value at time T.
        EQ.3: A plot is generated for each abscissa point in the load curve definition. The actual value of the load curve is ignored.
        """ # nopep8
        return self._cards[0].get_value("ioopt")

    @ioopt.setter
    def ioopt(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""ioopt must be one of {1,2,3}""")
        self._cards[0].set_value("ioopt", value)

    @property
    def option1(self) -> int:
        """Get or set the OPTION1 applies to either the NODOUT or ELOUT files. For the NODOUT file OPTION1 is a real variable that defines the time interval between outputs for the high frequency file, NODOUTHF. If OPTION1 is zero, no output is printed. Nodal points that are to be output at a higher frequency are flagged using HFO in the DATABASE_HISTORY_NODE_LOCAL input. For the ELOUT file OPTION1 is a integer variable that gives the number of additional history variables written into the ELOUT file for each integration point in the solid elements.
        """ # nopep8
        return self._cards[0].get_value("option1")

    @option1.setter
    def option1(self, value: int) -> None:
        self._cards[0].set_value("option1", value)

    @property
    def option2(self) -> int:
        """Get or set the OPTION2 applies to either the NODOUTHF or ELOUT files. For the NODOUTHF OPTION2 defines the binary file flag for the high frequency NODOUTHF file. See BINARY above. For the ELOUT file OPTION2 is a integer variable that gives the number of additional history variables written into the ELOUT file for each integration point in the shell elements.
        """ # nopep8
        return self._cards[0].get_value("option2")

    @option2.setter
    def option2(self, value: int) -> None:
        self._cards[0].set_value("option2", value)

    @property
    def option3(self) -> int:
        """Get or set the OPTION3 applies to the ELOUT file only. For the ELOUT file OPTION3 is a integer variable that gives the number of additional history variables written into the ELOUT file for each integration point in the thick shell elements.
        """ # nopep8
        return self._cards[0].get_value("option3")

    @option3.setter
    def option3(self, value: int) -> None:
        self._cards[0].set_value("option3", value)

    @property
    def option4(self) -> int:
        """Get or set the OPTION4 applies to the ELOUT file only. For the ELOUT file OPTION4 is a integer variable that gives the number of additional history variables written into the ELOUT file for each integration point in the beam elements.
        """ # nopep8
        return self._cards[0].get_value("option4")

    @option4.setter
    def option4(self, value: int) -> None:
        self._cards[0].set_value("option4", value)

