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

class DefineSetAdaptive(KeywordBase):
    """DYNA DEFINE_SET_ADAPTIVE keyword"""

    keyword = "DEFINE"
    subkeyword = "SET_ADAPTIVE"
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
                        "setid",
                        int,
                        0,
                        10,
                        kwargs.get("setid")
                    ),
                    Field(
                        "stype",
                        int,
                        10,
                        10,
                        kwargs.get("stype", 1)
                    ),
                    Field(
                        "adplvl",
                        int,
                        20,
                        10,
                        kwargs.get("adplvl")
                    ),
                    Field(
                        "adpsize",
                        float,
                        30,
                        10,
                        kwargs.get("adpsize")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineSetAdaptive.option_specs[0],
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
    def setid(self) -> typing.Optional[int]:
        """Get or set the Element(shell) set ID or part set ID
        """ # nopep8
        return self._cards[0].get_value("setid")

    @setid.setter
    def setid(self, value: int) -> None:
        self._cards[0].set_value("setid", value)

    @property
    def stype(self) -> int:
        """Get or set the Set type for SETID:   1-element set(shell)   2-part set
        """ # nopep8
        return self._cards[0].get_value("stype")

    @stype.setter
    def stype(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""stype must be one of {1,2}""")
        self._cards[0].set_value("stype", value)

    @property
    def adplvl(self) -> typing.Optional[int]:
        """Get or set the Adaptive refinement level for all elements in SETID set.
        """ # nopep8
        return self._cards[0].get_value("adplvl")

    @adplvl.setter
    def adplvl(self, value: int) -> None:
        self._cards[0].set_value("adplvl", value)

    @property
    def adpsize(self) -> typing.Optional[float]:
        """Get or set the Minimum element size to be adapted based on element edge length for all elements in SETID set.
        """ # nopep8
        return self._cards[0].get_value("adpsize")

    @adpsize.setter
    def adpsize(self, value: float) -> None:
        self._cards[0].set_value("adpsize", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[1].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[1].cards[0].set_value("title", value)

