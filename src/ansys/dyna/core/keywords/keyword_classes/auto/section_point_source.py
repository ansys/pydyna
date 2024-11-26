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

class SectionPointSource(KeywordBase):
    """DYNA SECTION_POINT_SOURCE keyword"""

    keyword = "SECTION"
    subkeyword = "POINT_SOURCE"
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
                        "lcidt",
                        int,
                        10,
                        10,
                        kwargs.get("lcidt")
                    ),
                    Field(
                        "lcidvolr",
                        int,
                        20,
                        10,
                        kwargs.get("lcidvolr")
                    ),
                    Field(
                        "lcidvel",
                        int,
                        30,
                        10,
                        kwargs.get("lcidvel")
                    ),
                    Field(
                        "nlc001",
                        int,
                        40,
                        10,
                        kwargs.get("nlc001")
                    ),
                    Field(
                        "nlc002",
                        int,
                        50,
                        10,
                        kwargs.get("nlc002")
                    ),
                    Field(
                        "nlc003",
                        int,
                        60,
                        10,
                        kwargs.get("nlc003")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "nodeid",
                        int,
                        0,
                        10,
                        kwargs.get("nodeid")
                    ),
                    Field(
                        "vecid",
                        int,
                        10,
                        10,
                        kwargs.get("vecid")
                    ),
                    Field(
                        "orifarea",
                        float,
                        20,
                        10,
                        kwargs.get("orifarea")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SectionPointSource.option_specs[0],
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
    def lcidt(self) -> typing.Optional[int]:
        """Get or set the Temperature load curve ID.
        """ # nopep8
        return self._cards[0].get_value("lcidt")

    @lcidt.setter
    def lcidt(self, value: int) -> None:
        self._cards[0].set_value("lcidt", value)

    @property
    def lcidvolr(self) -> typing.Optional[int]:
        """Get or set the Relative volume load curve ID.
        """ # nopep8
        return self._cards[0].get_value("lcidvolr")

    @lcidvolr.setter
    def lcidvolr(self, value: int) -> None:
        self._cards[0].set_value("lcidvolr", value)

    @property
    def lcidvel(self) -> typing.Optional[int]:
        """Get or set the Inlet flow velocity load curve ID.
        """ # nopep8
        return self._cards[0].get_value("lcidvel")

    @lcidvel.setter
    def lcidvel(self, value: int) -> None:
        self._cards[0].set_value("lcidvel", value)

    @property
    def nlc001(self) -> typing.Optional[int]:
        """Get or set the Node ID defining a local coordinate system. If defined, the vectors defining the inlet flow direction follow the rotation of this system.
        """ # nopep8
        return self._cards[0].get_value("nlc001")

    @nlc001.setter
    def nlc001(self, value: int) -> None:
        self._cards[0].set_value("nlc001", value)

    @property
    def nlc002(self) -> typing.Optional[int]:
        """Get or set the Node ID defining a local coordinate system. If defined, the vectors defining the inlet flow direction follow the rotation of this system.
        """ # nopep8
        return self._cards[0].get_value("nlc002")

    @nlc002.setter
    def nlc002(self, value: int) -> None:
        self._cards[0].set_value("nlc002", value)

    @property
    def nlc003(self) -> typing.Optional[int]:
        """Get or set the Node ID defining a local coordinate system. If defined, the vectors defining the inlet flow direction follow the rotation of this system.
        """ # nopep8
        return self._cards[0].get_value("nlc003")

    @nlc003.setter
    def nlc003(self, value: int) -> None:
        self._cards[0].set_value("nlc003", value)

    @property
    def nodeid(self) -> typing.Optional[int]:
        """Get or set the Node ID defining the location of the point source.
        """ # nopep8
        return self._cards[1].get_value("nodeid")

    @nodeid.setter
    def nodeid(self, value: int) -> None:
        self._cards[1].set_value("nodeid", value)

    @property
    def vecid(self) -> typing.Optional[int]:
        """Get or set the Vector ID defining the inlet flow direction in a local coordinate system defined by NID1-NID3.  If NID1-NID3 are not defined, the vector is assumed to be defined in the global coordinate system.
        """ # nopep8
        return self._cards[1].get_value("vecid")

    @vecid.setter
    def vecid(self, value: int) -> None:
        self._cards[1].set_value("vecid", value)

    @property
    def orifarea(self) -> typing.Optional[float]:
        """Get or set the Point source orifice area
        """ # nopep8
        return self._cards[1].get_value("orifarea")

    @orifarea.setter
    def orifarea(self, value: float) -> None:
        self._cards[1].set_value("orifarea", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

