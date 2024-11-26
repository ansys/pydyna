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

class SectionAle1D(KeywordBase):
    """DYNA SECTION_ALE1D keyword"""

    keyword = "SECTION"
    subkeyword = "ALE1D"
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
                        "aleform",
                        int,
                        10,
                        10,
                        kwargs.get("aleform", 11)
                    ),
                    Field(
                        "aet",
                        int,
                        20,
                        10,
                        kwargs.get("aet", 4)
                    ),
                    Field(
                        "elform",
                        int,
                        30,
                        10,
                        kwargs.get("elform", 7)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "thick",
                        float,
                        0,
                        10,
                        kwargs.get("thick")
                    ),
                    Field(
                        "thick",
                        float,
                        10,
                        10,
                        kwargs.get("thick")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SectionAle1D.option_specs[0],
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
    def aleform(self) -> int:
        """Get or set the ALE formulation:
        EQ.6  : Single material Eulerian formulation
        EQ.7  : Single material Ambient Eulerian formulation
        EQ.11: Multi-Material ALE formulation

        """ # nopep8
        return self._cards[0].get_value("aleform")

    @aleform.setter
    def aleform(self, value: int) -> None:
        self._cards[0].set_value("aleform", value)

    @property
    def aet(self) -> int:
        """Get or set the Ambient Element Type:
        EQ.4: pressure inflow
        .
        """ # nopep8
        return self._cards[0].get_value("aet")

    @aet.setter
    def aet(self, value: int) -> None:
        self._cards[0].set_value("aet", value)

    @property
    def elform(self) -> int:
        """Get or set the Element formulation:
        EQ.7:plane strain (x-y plane ; element volume= 1*dx*thick)
        EQ.8: : cylindric  (y-axis of symmetry ; element volume= x*dx*thick)
        EQ.-8: spherical (element volume = x*x*dx)
        """ # nopep8
        return self._cards[0].get_value("elform")

    @elform.setter
    def elform(self, value: int) -> None:
        if value not in [7, 8, -8]:
            raise Exception("""elform must be one of {7,8,-8}""")
        self._cards[0].set_value("elform", value)

    @property
    def thick(self) -> typing.Optional[float]:
        """Get or set the Thickness
        """ # nopep8
        return self._cards[1].get_value("thick")

    @thick.setter
    def thick(self, value: float) -> None:
        self._cards[1].set_value("thick", value)

    @property
    def thick(self) -> typing.Optional[float]:
        """Get or set the Thickness
        """ # nopep8
        return self._cards[1].get_value("thick")

    @thick.setter
    def thick(self, value: float) -> None:
        self._cards[1].set_value("thick", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

