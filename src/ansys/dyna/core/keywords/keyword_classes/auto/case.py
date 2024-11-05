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

class Case(KeywordBase):
    """DYNA CASE keyword"""

    keyword = "CASE"
    subkeyword = "CASE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "caseid",
                        int,
                        0,
                        10,
                        kwargs.get("caseid")
                    ),
                    Field(
                        "jobid",
                        str,
                        10,
                        70,
                        kwargs.get("jobid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "commands",
                        str,
                        0,
                        80,
                        kwargs.get("commands")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "scid1",
                        int,
                        0,
                        10,
                        kwargs.get("scid1")
                    ),
                    Field(
                        "scid2",
                        int,
                        10,
                        10,
                        kwargs.get("scid2")
                    ),
                    Field(
                        "scid3",
                        int,
                        20,
                        10,
                        kwargs.get("scid3")
                    ),
                    Field(
                        "scid4",
                        int,
                        30,
                        10,
                        kwargs.get("scid4")
                    ),
                    Field(
                        "scid5",
                        int,
                        40,
                        10,
                        kwargs.get("scid5")
                    ),
                    Field(
                        "scid6",
                        int,
                        50,
                        10,
                        kwargs.get("scid6")
                    ),
                    Field(
                        "scid7",
                        int,
                        60,
                        10,
                        kwargs.get("scid7")
                    ),
                    Field(
                        "scid8",
                        int,
                        70,
                        10,
                        kwargs.get("scid8")
                    ),
                ],
            ),
        ]

    @property
    def caseid(self) -> typing.Optional[int]:
        """Get or set the Identification number for case.
        """ # nopep8
        return self._cards[0].get_value("caseid")

    @caseid.setter
    def caseid(self, value: int) -> None:
        self._cards[0].set_value("caseid", value)

    @property
    def jobid(self) -> typing.Optional[str]:
        """Get or set the Optional string (no spaces) to be used as the jobid for this case.
        If no JOBID is specified, the string CASEXX is used, where XX is the CASEID in field 1.
        """ # nopep8
        return self._cards[0].get_value("jobid")

    @jobid.setter
    def jobid(self, value: str) -> None:
        self._cards[0].set_value("jobid", value)

    @property
    def commands(self) -> typing.Optional[str]:
        """Get or set the Command line arguments.
        """ # nopep8
        return self._cards[1].get_value("commands")

    @commands.setter
    def commands(self, value: str) -> None:
        self._cards[1].set_value("commands", value)

    @property
    def scid1(self) -> typing.Optional[int]:
        """Get or set the Subcase ID active for case CASEID.
        """ # nopep8
        return self._cards[2].get_value("scid1")

    @scid1.setter
    def scid1(self, value: int) -> None:
        self._cards[2].set_value("scid1", value)

    @property
    def scid2(self) -> typing.Optional[int]:
        """Get or set the Subcase ID active for case CASEID.
        """ # nopep8
        return self._cards[2].get_value("scid2")

    @scid2.setter
    def scid2(self, value: int) -> None:
        self._cards[2].set_value("scid2", value)

    @property
    def scid3(self) -> typing.Optional[int]:
        """Get or set the Subcase ID active for case CASEID.
        """ # nopep8
        return self._cards[2].get_value("scid3")

    @scid3.setter
    def scid3(self, value: int) -> None:
        self._cards[2].set_value("scid3", value)

    @property
    def scid4(self) -> typing.Optional[int]:
        """Get or set the Subcase ID active for case CASEID.
        """ # nopep8
        return self._cards[2].get_value("scid4")

    @scid4.setter
    def scid4(self, value: int) -> None:
        self._cards[2].set_value("scid4", value)

    @property
    def scid5(self) -> typing.Optional[int]:
        """Get or set the Subcase ID active for case CASEID.
        """ # nopep8
        return self._cards[2].get_value("scid5")

    @scid5.setter
    def scid5(self, value: int) -> None:
        self._cards[2].set_value("scid5", value)

    @property
    def scid6(self) -> typing.Optional[int]:
        """Get or set the Subcase ID active for case CASEID.
        """ # nopep8
        return self._cards[2].get_value("scid6")

    @scid6.setter
    def scid6(self, value: int) -> None:
        self._cards[2].set_value("scid6", value)

    @property
    def scid7(self) -> typing.Optional[int]:
        """Get or set the Subcase ID active for case CASEID.
        """ # nopep8
        return self._cards[2].get_value("scid7")

    @scid7.setter
    def scid7(self, value: int) -> None:
        self._cards[2].set_value("scid7", value)

    @property
    def scid8(self) -> typing.Optional[int]:
        """Get or set the Subcase ID active for case CASEID.
        """ # nopep8
        return self._cards[2].get_value("scid8")

    @scid8.setter
    def scid8(self, value: int) -> None:
        self._cards[2].set_value("scid8", value)

