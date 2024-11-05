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

class InterfaceCompensation(KeywordBase):
    """DYNA INTERFACE_COMPENSATION keyword"""

    keyword = "INTERFACE"
    subkeyword = "COMPENSATION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "stage",
                        int,
                        0,
                        10,
                        kwargs.get("stage", 1)
                    ),
                    Field(
                        "psidt",
                        int,
                        10,
                        10,
                        kwargs.get("psidt")
                    ),
                    Field(
                        "psidb",
                        int,
                        20,
                        10,
                        kwargs.get("psidb")
                    ),
                    Field(
                        "smooth",
                        int,
                        30,
                        10,
                        kwargs.get("smooth", 3)
                    ),
                    Field(
                        "scale",
                        float,
                        40,
                        10,
                        kwargs.get("scale", 1.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dbname",
                        str,
                        0,
                        80,
                        kwargs.get("dbname", "lscomp")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "outname",
                        str,
                        0,
                        80,
                        kwargs.get("outname", "lstool")
                    ),
                ],
            ),
        ]

    @property
    def stage(self) -> int:
        """Get or set the Stage of this simulation in overall process. Stamping and springback must be finished before compensation can be performed.
        EQ.1: stamping
        EQ.2: springback
        EQ.3: compensation(generate new tools).
        """ # nopep8
        return self._cards[0].get_value("stage")

    @stage.setter
    def stage(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""stage must be one of {1,2,3}""")
        self._cards[0].set_value("stage", value)

    @property
    def psidt(self) -> typing.Optional[int]:
        """Get or set the Part set ID including all tool parts(not required for STAGE=2)
        """ # nopep8
        return self._cards[0].get_value("psidt")

    @psidt.setter
    def psidt(self, value: int) -> None:
        self._cards[0].set_value("psidt", value)

    @property
    def psidb(self) -> typing.Optional[int]:
        """Get or set the Part set ID including all blank(sheet)parts.
        """ # nopep8
        return self._cards[0].get_value("psidb")

    @psidb.setter
    def psidb(self, value: int) -> None:
        self._cards[0].set_value("psidb", value)

    @property
    def smooth(self) -> int:
        """Get or set the Extrapolation method for tool surfaces outside final part (STAGE=3 only) A negative value can be used if undercutting occurs.
        EQ.1: Preserve boundary slope.
        EQ.2:Zero slope.
        EQ.3: Smoothing method A(DEFAULT).
        EQ.4: Smoothing method B.
        EQ.5: Smoothing method c.
        """ # nopep8
        return self._cards[0].get_value("smooth")

    @smooth.setter
    def smooth(self, value: int) -> None:
        if value not in [3, 1, 2, 4, 5]:
            raise Exception("""smooth must be one of {3,1,2,4,5}""")
        self._cards[0].set_value("smooth", value)

    @property
    def scale(self) -> float:
        """Get or set the Compensation scale factor (STAGE=3 only).
        """ # nopep8
        return self._cards[0].get_value("scale")

    @scale.setter
    def scale(self, value: float) -> None:
        self._cards[0].set_value("scale", value)

    @property
    def dbname(self) -> str:
        """Get or set the File name for binary compensation database(DEFAULT=lscomp)
        """ # nopep8
        return self._cards[1].get_value("dbname")

    @dbname.setter
    def dbname(self, value: str) -> None:
        self._cards[1].set_value("dbname", value)

    @property
    def outname(self) -> str:
        """Get or set the File name for keyword output containing new tolls(STAGE=3 only,DEFAULT=lstool)
        """ # nopep8
        return self._cards[2].get_value("outname")

    @outname.setter
    def outname(self, value: str) -> None:
        self._cards[2].set_value("outname", value)

