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

class DefineMultiDrawbeadsIges(KeywordBase):
    """DYNA DEFINE_MULTI_DRAWBEADS_IGES keyword"""

    keyword = "DEFINE"
    subkeyword = "MULTI_DRAWBEADS_IGES"
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
                        "filename",
                        str,
                        0,
                        80,
                        kwargs.get("filename")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "dbid",
                        int,
                        0,
                        10,
                        kwargs.get("dbid")
                    ),
                    Field(
                        "vid",
                        int,
                        10,
                        10,
                        kwargs.get("vid")
                    ),
                    Field(
                        "pid",
                        int,
                        20,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "blkid",
                        int,
                        30,
                        10,
                        kwargs.get("blkid")
                    ),
                    Field(
                        "ncur",
                        int,
                        40,
                        10,
                        kwargs.get("ncur")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "crvid",
                        int,
                        0,
                        10,
                        kwargs.get("crvid")
                    ),
                    Field(
                        "bforce",
                        float,
                        10,
                        10,
                        kwargs.get("bforce", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineMultiDrawbeadsIges.option_specs[0],
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
    def filename(self) -> typing.Optional[str]:
        """Get or set the IGES file that has the draw bead curve segment definitions
        """ # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        self._cards[0].set_value("filename", value)

    @property
    def dbid(self) -> typing.Optional[int]:
        """Get or set the Draw bead set ID, which may consists many draw bead segments.
        """ # nopep8
        return self._cards[1].get_value("dbid")

    @dbid.setter
    def dbid(self, value: int) -> None:
        self._cards[1].set_value("dbid", value)

    @property
    def vid(self) -> typing.Optional[int]:
        """Get or set the Vector ID, as defined by *DEFINE_VECTOR. This vector is used to
        project the supplied curves to the rigid tool, defined by variable PID.
        """ # nopep8
        return self._cards[1].get_value("vid")

    @vid.setter
    def vid(self, value: int) -> None:
        self._cards[1].set_value("vid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the rigid tool to which the curves are projected and attached.
        """ # nopep8
        return self._cards[1].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[1].set_value("pid", value)

    @property
    def blkid(self) -> typing.Optional[int]:
        """Get or set the Part set ID of the blank.
        """ # nopep8
        return self._cards[1].get_value("blkid")

    @blkid.setter
    def blkid(self, value: int) -> None:
        self._cards[1].set_value("blkid", value)

    @property
    def ncur(self) -> typing.Optional[int]:
        """Get or set the Number of draw bead curve segments (in the IGES file) to be defined.
        """ # nopep8
        return self._cards[1].get_value("ncur")

    @ncur.setter
    def ncur(self, value: int) -> None:
        self._cards[1].set_value("ncur", value)

    @property
    def crvid(self) -> typing.Optional[int]:
        """Get or set the IGES curve ID for each segment.
        """ # nopep8
        return self._cards[2].get_value("crvid")

    @crvid.setter
    def crvid(self, value: int) -> None:
        self._cards[2].set_value("crvid", value)

    @property
    def bforce(self) -> float:
        """Get or set the Draw bead force for each segment.
        """ # nopep8
        return self._cards[2].get_value("bforce")

    @bforce.setter
    def bforce(self, value: float) -> None:
        self._cards[2].set_value("bforce", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

