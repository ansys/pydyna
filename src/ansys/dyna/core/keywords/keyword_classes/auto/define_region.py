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

class DefineRegion(KeywordBase):
    """DYNA DEFINE_REGION keyword"""

    keyword = "DEFINE"
    subkeyword = "REGION"
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
                        "title",
                        str,
                        10,
                        70,
                        kwargs.get("title")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "type",
                        int,
                        0,
                        10,
                        kwargs.get("type", 0)
                    ),
                    Field(
                        "cid",
                        int,
                        10,
                        10,
                        kwargs.get("cid")
                    ),
                    Field(
                        "move",
                        int,
                        20,
                        10,
                        kwargs.get("move", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "xmn",
                        float,
                        0,
                        10,
                        kwargs.get("xmn", 0.0)
                    ),
                    Field(
                        "xmx",
                        float,
                        10,
                        10,
                        kwargs.get("xmx", 0.0)
                    ),
                    Field(
                        "ymn",
                        float,
                        20,
                        10,
                        kwargs.get("ymn", 0.0)
                    ),
                    Field(
                        "ymx",
                        float,
                        30,
                        10,
                        kwargs.get("ymx", 0.0)
                    ),
                    Field(
                        "zmn",
                        float,
                        40,
                        10,
                        kwargs.get("zmn", 0.0)
                    ),
                    Field(
                        "zmx",
                        float,
                        50,
                        10,
                        kwargs.get("zmx", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = DefineRegion.option_specs[0],
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
        """Get or set the Region ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        self._cards[0].set_value("id", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Title for this region.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[0].set_value("title", value)

    @property
    def type(self) -> int:
        """Get or set the Region type:
        EQ.0: Box
        EQ.1: Sphere or spherical shell
        EQ.2: Cylinder or cylindrical shell, infinite or finite in length
        EQ.3: Ellipsoid.
        """ # nopep8
        return self._cards[1].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        if value not in [0, 1, 2, 3]:
            raise Exception("""type must be one of {0,1,2,3}""")
        self._cards[1].set_value("type", value)

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Optional local coordinate system ID. If given, all the following
        input parameters will be interpreted in this coordinate system.
        """ # nopep8
        return self._cards[1].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[1].set_value("cid", value)

    @property
    def move(self) -> int:
        """Get or set the Flag to specify whether the region moves:
        EQ.0:	Region is stationary.
        EQ.1 : Region moves to follow the local origin and rotates with the local coordinate system(see CID)..
        """ # nopep8
        return self._cards[1].get_value("move")

    @move.setter
    def move(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""move must be one of {0,1}""")
        self._cards[1].set_value("move", value)

    @property
    def xmn(self) -> float:
        """Get or set the Lower x limit of box.
        """ # nopep8
        return self._cards[2].get_value("xmn")

    @xmn.setter
    def xmn(self, value: float) -> None:
        self._cards[2].set_value("xmn", value)

    @property
    def xmx(self) -> float:
        """Get or set the Upper x limit of box.
        """ # nopep8
        return self._cards[2].get_value("xmx")

    @xmx.setter
    def xmx(self, value: float) -> None:
        self._cards[2].set_value("xmx", value)

    @property
    def ymn(self) -> float:
        """Get or set the Lower y limit of box.
        """ # nopep8
        return self._cards[2].get_value("ymn")

    @ymn.setter
    def ymn(self, value: float) -> None:
        self._cards[2].set_value("ymn", value)

    @property
    def ymx(self) -> float:
        """Get or set the Upper y limit of box.
        """ # nopep8
        return self._cards[2].get_value("ymx")

    @ymx.setter
    def ymx(self, value: float) -> None:
        self._cards[2].set_value("ymx", value)

    @property
    def zmn(self) -> float:
        """Get or set the Lower z limit of box.
        """ # nopep8
        return self._cards[2].get_value("zmn")

    @zmn.setter
    def zmn(self, value: float) -> None:
        self._cards[2].set_value("zmn", value)

    @property
    def zmx(self) -> float:
        """Get or set the Upper z limit of box.
        """ # nopep8
        return self._cards[2].get_value("zmx")

    @zmx.setter
    def zmx(self, value: float) -> None:
        self._cards[2].set_value("zmx", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

