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

class DefineCpmChamber(KeywordBase):
    """DYNA DEFINE_CPM_CHAMBER keyword"""

    keyword = "DEFINE"
    subkeyword = "CPM_CHAMBER"
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
                        "id",
                        int,
                        0,
                        10,
                        kwargs.get("id")
                    ),
                    Field(
                        "nchm",
                        int,
                        10,
                        10,
                        kwargs.get("nchm", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sid1",
                        int,
                        0,
                        10,
                        kwargs.get("sid1")
                    ),
                    Field(
                        "sid2",
                        int,
                        10,
                        10,
                        kwargs.get("sid2", 0)
                    ),
                    Field(
                        "ninter",
                        int,
                        20,
                        10,
                        kwargs.get("ninter", 0)
                    ),
                    Field(
                        "chm_id",
                        int,
                        30,
                        10,
                        kwargs.get("chm_id", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "sid3",
                        int,
                        0,
                        10,
                        kwargs.get("sid3")
                    ),
                    Field(
                        "itype3",
                        int,
                        10,
                        10,
                        kwargs.get("itype3", 0)
                    ),
                    Field(
                        "tochm",
                        int,
                        20,
                        10,
                        kwargs.get("tochm")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineCpmChamber.option_specs[0],
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
    def id(self) -> typing.Optional[int]:
        """Get or set the Unique ID for this card
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def nchm(self) -> int:
        """Get or set the Number of chambers defined
        """ # nopep8
        return self._cards[0].get_value("nchm")

    @nchm.setter
    def nchm(self, value: int) -> None:
        self._cards[0].set_value("nchm", value)

    @property
    def sid1(self) -> typing.Optional[int]:
        """Get or set the Part set defining the chamber (normals pointed inward)
        """ # nopep8
        return self._cards[1].get_value("sid1")

    @sid1.setter
    def sid1(self, value: int) -> None:
        self._cards[1].set_value("sid1", value)

    @property
    def sid2(self) -> int:
        """Get or set the Part set defining the chamber (normals pointed outward)
        """ # nopep8
        return self._cards[1].get_value("sid2")

    @sid2.setter
    def sid2(self, value: int) -> None:
        self._cards[1].set_value("sid2", value)

    @property
    def ninter(self) -> int:
        """Get or set the Number of vent hole definition for chamber interaction
        """ # nopep8
        return self._cards[1].get_value("ninter")

    @ninter.setter
    def ninter(self, value: int) -> None:
        self._cards[1].set_value("ninter", value)

    @property
    def chm_id(self) -> int:
        """Get or set the Chamber ID
        """ # nopep8
        return self._cards[1].get_value("chm_id")

    @chm_id.setter
    def chm_id(self, value: int) -> None:
        self._cards[1].set_value("chm_id", value)

    @property
    def sid3(self) -> typing.Optional[int]:
        """Get or set the Set defining interaction between chambers
        """ # nopep8
        return self._cards[2].get_value("sid3")

    @sid3.setter
    def sid3(self, value: int) -> None:
        self._cards[2].set_value("sid3", value)

    @property
    def itype3(self) -> int:
        """Get or set the Set type EQ.0: Part
        EQ.1: Part set
        """ # nopep8
        return self._cards[2].get_value("itype3")

    @itype3.setter
    def itype3(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""itype3 must be one of {0,1}""")
        self._cards[2].set_value("itype3", value)

    @property
    def tochm(self) -> typing.Optional[int]:
        """Get or set the The chamber ID of the connected chamber
        """ # nopep8
        return self._cards[2].get_value("tochm")

    @tochm.setter
    def tochm(self, value: int) -> None:
        self._cards[2].set_value("tochm", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

