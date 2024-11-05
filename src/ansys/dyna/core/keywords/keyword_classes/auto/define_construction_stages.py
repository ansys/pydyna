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

class DefineConstructionStages(KeywordBase):
    """DYNA DEFINE_CONSTRUCTION_STAGES keyword"""

    keyword = "DEFINE"
    subkeyword = "CONSTRUCTION_STAGES"
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
                        "istage",
                        int,
                        0,
                        10,
                        kwargs.get("istage")
                    ),
                    Field(
                        "ats",
                        float,
                        10,
                        10,
                        kwargs.get("ats", 0.0)
                    ),
                    Field(
                        "ate",
                        float,
                        20,
                        10,
                        kwargs.get("ate", 0.0)
                    ),
                    Field(
                        "atr",
                        float,
                        30,
                        10,
                        kwargs.get("atr")
                    ),
                    Field(
                        "rts",
                        float,
                        40,
                        10,
                        kwargs.get("rts", 0.0)
                    ),
                    Field(
                        "rte",
                        float,
                        50,
                        10,
                        kwargs.get("rte", 0.0)
                    ),
                    Field(
                        "unused",
                        float,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "idynain",
                        int,
                        70,
                        10,
                        kwargs.get("idynain", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineConstructionStages.option_specs[0],
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
    def istage(self) -> typing.Optional[int]:
        """Get or set the Stage ID.
        """ # nopep8
        return self._cards[0].get_value("istage")

    @istage.setter
    def istage(self, value: int) -> None:
        self._cards[0].set_value("istage", value)

    @property
    def ats(self) -> float:
        """Get or set the Analysis time at start of stage
        """ # nopep8
        return self._cards[0].get_value("ats")

    @ats.setter
    def ats(self, value: float) -> None:
        self._cards[0].set_value("ats", value)

    @property
    def ate(self) -> float:
        """Get or set the Analysis time at end of stage.
        """ # nopep8
        return self._cards[0].get_value("ate")

    @ate.setter
    def ate(self, value: float) -> None:
        self._cards[0].set_value("ate", value)

    @property
    def atr(self) -> typing.Optional[float]:
        """Get or set the Analysis time duration of ramp.
        """ # nopep8
        return self._cards[0].get_value("atr")

    @atr.setter
    def atr(self, value: float) -> None:
        self._cards[0].set_value("atr", value)

    @property
    def rts(self) -> float:
        """Get or set the Real time at start of stage.
        """ # nopep8
        return self._cards[0].get_value("rts")

    @rts.setter
    def rts(self, value: float) -> None:
        self._cards[0].set_value("rts", value)

    @property
    def rte(self) -> float:
        """Get or set the Real time at end of stage.
        """ # nopep8
        return self._cards[0].get_value("rte")

    @rte.setter
    def rte(self, value: float) -> None:
        self._cards[0].set_value("rte", value)

    @property
    def idynain(self) -> int:
        """Get or set the Flag to control output of dynain file at the end of the stage
        EQ.0:	write dynain file
        EQ.1:	do not write dynain file .
        """ # nopep8
        return self._cards[0].get_value("idynain")

    @idynain.setter
    def idynain(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""idynain must be one of {0,1}""")
        self._cards[0].set_value("idynain", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

