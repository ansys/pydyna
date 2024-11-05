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

class IcfdDatabaseNodout(KeywordBase):
    """DYNA ICFD_DATABASE_NODOUT keyword"""

    keyword = "ICFD"
    subkeyword = "DATABASE_NODOUT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "outlv",
                        int,
                        0,
                        10,
                        kwargs.get("outlv", 0)
                    ),
                    Field(
                        "dtout",
                        float,
                        10,
                        10,
                        kwargs.get("dtout", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nid1",
                        int,
                        0,
                        10,
                        kwargs.get("nid1")
                    ),
                    Field(
                        "nid2",
                        int,
                        10,
                        10,
                        kwargs.get("nid2")
                    ),
                    Field(
                        "nid3",
                        int,
                        20,
                        10,
                        kwargs.get("nid3")
                    ),
                    Field(
                        "nid4",
                        int,
                        30,
                        10,
                        kwargs.get("nid4")
                    ),
                    Field(
                        "nid5",
                        int,
                        40,
                        10,
                        kwargs.get("nid5")
                    ),
                    Field(
                        "nid6",
                        int,
                        50,
                        10,
                        kwargs.get("nid6")
                    ),
                    Field(
                        "nid7",
                        int,
                        60,
                        10,
                        kwargs.get("nid7")
                    ),
                    Field(
                        "nid8",
                        int,
                        70,
                        10,
                        kwargs.get("nid8")
                    ),
                ],
            ),
        ]

    @property
    def outlv(self) -> int:
        """Get or set the Determines if the output file should be dumped.
        EQ.0: No output file is generated.
        EQ.1: The output file is generated.
        """ # nopep8
        return self._cards[0].get_value("outlv")

    @outlv.setter
    def outlv(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""outlv must be one of {0,1}""")
        self._cards[0].set_value("outlv", value)

    @property
    def dtout(self) -> float:
        """Get or set the Time interval to print the output. If DTOUT is equal to 0.0, then the ICFD timestep will be used.
        """ # nopep8
        return self._cards[0].get_value("dtout")

    @dtout.setter
    def dtout(self, value: float) -> None:
        self._cards[0].set_value("dtout", value)

    @property
    def nid1(self) -> typing.Optional[int]:
        """Get or set the Node IDs.
        """ # nopep8
        return self._cards[1].get_value("nid1")

    @nid1.setter
    def nid1(self, value: int) -> None:
        self._cards[1].set_value("nid1", value)

    @property
    def nid2(self) -> typing.Optional[int]:
        """Get or set the Node IDs.
        """ # nopep8
        return self._cards[1].get_value("nid2")

    @nid2.setter
    def nid2(self, value: int) -> None:
        self._cards[1].set_value("nid2", value)

    @property
    def nid3(self) -> typing.Optional[int]:
        """Get or set the Node IDs.
        """ # nopep8
        return self._cards[1].get_value("nid3")

    @nid3.setter
    def nid3(self, value: int) -> None:
        self._cards[1].set_value("nid3", value)

    @property
    def nid4(self) -> typing.Optional[int]:
        """Get or set the Node IDs.
        """ # nopep8
        return self._cards[1].get_value("nid4")

    @nid4.setter
    def nid4(self, value: int) -> None:
        self._cards[1].set_value("nid4", value)

    @property
    def nid5(self) -> typing.Optional[int]:
        """Get or set the Node IDs.
        """ # nopep8
        return self._cards[1].get_value("nid5")

    @nid5.setter
    def nid5(self, value: int) -> None:
        self._cards[1].set_value("nid5", value)

    @property
    def nid6(self) -> typing.Optional[int]:
        """Get or set the Node IDs.
        """ # nopep8
        return self._cards[1].get_value("nid6")

    @nid6.setter
    def nid6(self, value: int) -> None:
        self._cards[1].set_value("nid6", value)

    @property
    def nid7(self) -> typing.Optional[int]:
        """Get or set the Node IDs.
        """ # nopep8
        return self._cards[1].get_value("nid7")

    @nid7.setter
    def nid7(self, value: int) -> None:
        self._cards[1].set_value("nid7", value)

    @property
    def nid8(self) -> typing.Optional[int]:
        """Get or set the Node IDs.
        """ # nopep8
        return self._cards[1].get_value("nid8")

    @nid8.setter
    def nid8(self, value: int) -> None:
        self._cards[1].set_value("nid8", value)

