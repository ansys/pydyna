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

class SectionSolidPeri(KeywordBase):
    """DYNA SECTION_SOLID_PERI keyword"""

    keyword = "SECTION"
    subkeyword = "SOLID_PERI"
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
                        "secid",
                        int,
                        0,
                        10,
                        kwargs.get("secid")
                    ),
                    Field(
                        "elform",
                        int,
                        10,
                        10,
                        kwargs.get("elform", 48)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dr",
                        float,
                        0,
                        10,
                        kwargs.get("dr", 1.01)
                    ),
                    Field(
                        "ptype",
                        int,
                        10,
                        10,
                        kwargs.get("ptype", 0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SectionSolidPeri.option_specs[0],
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
    def secid(self) -> typing.Optional[int]:
        """Get or set the Section ID. SECID is referenced on the *PART card and must be unique.
        """ # nopep8
        return self._cards[0].get_value("secid")

    @secid.setter
    def secid(self, value: int) -> None:
        self._cards[0].set_value("secid", value)

    @property
    def elform(self) -> int:
        """Get or set the Element formulation:
        mut be set as 48.
        """ # nopep8
        return self._cards[0].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        self._cards[0].set_value("elform", value)

    @property
    def dr(self) -> float:
        """Get or set the the ration of radius of the support zone over the element characteristic length.Normally set DR between: 0.8~1.2.
        """ # nopep8
        return self._cards[1].get_value("dr")

    @dr.setter
    def dr(self, value: float) -> None:
        self._cards[1].set_value("dr", value)

    @property
    def ptype(self) -> int:
        """Get or set the 0: bond based peridynamic (Default)
        .1: state based peridynamic (Future using, not support now.).
        """ # nopep8
        return self._cards[1].get_value("ptype")

    @ptype.setter
    def ptype(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ptype must be one of {0,1}""")
        self._cards[1].set_value("ptype", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

