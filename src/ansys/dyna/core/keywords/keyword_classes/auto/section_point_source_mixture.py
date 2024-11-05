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

class SectionPointSourceMixture(KeywordBase):
    """DYNA SECTION_POINT_SOURCE_MIXTURE keyword"""

    keyword = "SECTION"
    subkeyword = "POINT_SOURCE_MIXTURE"
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
                        "notused",
                        int,
                        20,
                        10,
                        kwargs.get("notused")
                    ),
                    Field(
                        "lcidvel",
                        int,
                        30,
                        10,
                        kwargs.get("lcidvel")
                    ),
                    Field(
                        "nidlc001",
                        int,
                        40,
                        10,
                        kwargs.get("nidlc001")
                    ),
                    Field(
                        "nidlc002",
                        int,
                        50,
                        10,
                        kwargs.get("nidlc002")
                    ),
                    Field(
                        "nidlc003",
                        int,
                        60,
                        10,
                        kwargs.get("nidlc003")
                    ),
                    Field(
                        "idir",
                        int,
                        70,
                        10,
                        kwargs.get("idir")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lcmdot1",
                        int,
                        0,
                        10,
                        kwargs.get("lcmdot1", 0)
                    ),
                    Field(
                        "lcmdot2",
                        int,
                        10,
                        10,
                        kwargs.get("lcmdot2", 0)
                    ),
                    Field(
                        "lcmdot3",
                        int,
                        20,
                        10,
                        kwargs.get("lcmdot3", 0)
                    ),
                    Field(
                        "lcmdot4",
                        int,
                        30,
                        10,
                        kwargs.get("lcmdot4", 0)
                    ),
                    Field(
                        "lcmdot5",
                        int,
                        40,
                        10,
                        kwargs.get("lcmdot5", 0)
                    ),
                    Field(
                        "lcmdot6",
                        int,
                        50,
                        10,
                        kwargs.get("lcmdot6", 0)
                    ),
                    Field(
                        "lcmdot7",
                        int,
                        60,
                        10,
                        kwargs.get("lcmdot7", 0)
                    ),
                    Field(
                        "lcmdot8",
                        int,
                        70,
                        10,
                        kwargs.get("lcmdot8", 0)
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
                        kwargs.get("nodeid", 0)
                    ),
                    Field(
                        "vecid",
                        int,
                        10,
                        10,
                        kwargs.get("vecid", 0)
                    ),
                    Field(
                        "orifarea",
                        float,
                        20,
                        10,
                        kwargs.get("orifarea", 0.0)
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = SectionPointSourceMixture.option_specs[0],
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
        """Get or set the Section ID Number
        """ # nopep8
        return self._cards[0].get_value("secid")

    @secid.setter
    def secid(self, value: int) -> None:
        self._cards[0].set_value("secid", value)

    @property
    def lcidt(self) -> typing.Optional[int]:
        """Get or set the Inflator gas mixture average stagnation temperature load curve ID.
        """ # nopep8
        return self._cards[0].get_value("lcidt")

    @lcidt.setter
    def lcidt(self, value: int) -> None:
        self._cards[0].set_value("lcidt", value)

    @property
    def notused(self) -> typing.Optional[int]:
        """Get or set the Not used.
        """ # nopep8
        return self._cards[0].get_value("notused")

    @notused.setter
    def notused(self, value: int) -> None:
        self._cards[0].set_value("notused", value)

    @property
    def lcidvel(self) -> typing.Optional[int]:
        """Get or set the inflator gas mixture average velocity load curve ID..  If LCIDVEL=0 or blank, LSDYNA will estimate the inlet gas velocity.
        """ # nopep8
        return self._cards[0].get_value("lcidvel")

    @lcidvel.setter
    def lcidvel(self, value: int) -> None:
        self._cards[0].set_value("lcidvel", value)

    @property
    def nidlc001(self) -> typing.Optional[int]:
        """Get or set the The 1st Node ID defining a local coordinate system.
        """ # nopep8
        return self._cards[0].get_value("nidlc001")

    @nidlc001.setter
    def nidlc001(self, value: int) -> None:
        self._cards[0].set_value("nidlc001", value)

    @property
    def nidlc002(self) -> typing.Optional[int]:
        """Get or set the The 2nd Node ID defining a local coordinate system.
        """ # nopep8
        return self._cards[0].get_value("nidlc002")

    @nidlc002.setter
    def nidlc002(self, value: int) -> None:
        self._cards[0].set_value("nidlc002", value)

    @property
    def nidlc003(self) -> typing.Optional[int]:
        """Get or set the the 3rd Node ID defining a local coordinate system.
        """ # nopep8
        return self._cards[0].get_value("nidlc003")

    @nidlc003.setter
    def nidlc003(self, value: int) -> None:
        self._cards[0].set_value("nidlc003", value)

    @property
    def idir(self) -> typing.Optional[int]:
        """Get or set the A flag for constraining the nodal velocity of the nodes of the ALE element containing a point source.  If IDIR=0 (default), then the ALE nodes behind the point source (relative position of nodes based on the vector direction of flow of point source) will have zero velocity.  If IDIR=1, then all ALE nodes will have velocity distributed based on energy conservation.  The latter option seems to be more robust in airbag modeling.
        """ # nopep8
        return self._cards[0].get_value("idir")

    @idir.setter
    def idir(self, value: int) -> None:
        self._cards[0].set_value("idir", value)

    @property
    def lcmdot1(self) -> int:
        """Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
        """ # nopep8
        return self._cards[1].get_value("lcmdot1")

    @lcmdot1.setter
    def lcmdot1(self, value: int) -> None:
        self._cards[1].set_value("lcmdot1", value)

    @property
    def lcmdot2(self) -> int:
        """Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
        """ # nopep8
        return self._cards[1].get_value("lcmdot2")

    @lcmdot2.setter
    def lcmdot2(self, value: int) -> None:
        self._cards[1].set_value("lcmdot2", value)

    @property
    def lcmdot3(self) -> int:
        """Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
        """ # nopep8
        return self._cards[1].get_value("lcmdot3")

    @lcmdot3.setter
    def lcmdot3(self, value: int) -> None:
        self._cards[1].set_value("lcmdot3", value)

    @property
    def lcmdot4(self) -> int:
        """Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
        """ # nopep8
        return self._cards[1].get_value("lcmdot4")

    @lcmdot4.setter
    def lcmdot4(self, value: int) -> None:
        self._cards[1].set_value("lcmdot4", value)

    @property
    def lcmdot5(self) -> int:
        """Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
        """ # nopep8
        return self._cards[1].get_value("lcmdot5")

    @lcmdot5.setter
    def lcmdot5(self, value: int) -> None:
        self._cards[1].set_value("lcmdot5", value)

    @property
    def lcmdot6(self) -> int:
        """Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
        """ # nopep8
        return self._cards[1].get_value("lcmdot6")

    @lcmdot6.setter
    def lcmdot6(self, value: int) -> None:
        self._cards[1].set_value("lcmdot6", value)

    @property
    def lcmdot7(self) -> int:
        """Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
        """ # nopep8
        return self._cards[1].get_value("lcmdot7")

    @lcmdot7.setter
    def lcmdot7(self, value: int) -> None:
        self._cards[1].set_value("lcmdot7", value)

    @property
    def lcmdot8(self) -> int:
        """Get or set the The mass flow rate load curve ID of the 1st gas in the mixture.
        """ # nopep8
        return self._cards[1].get_value("lcmdot8")

    @lcmdot8.setter
    def lcmdot8(self, value: int) -> None:
        self._cards[1].set_value("lcmdot8", value)

    @property
    def nodeid(self) -> int:
        """Get or set the The node ID(s) defining the point source(s).
        """ # nopep8
        return self._cards[2].get_value("nodeid")

    @nodeid.setter
    def nodeid(self, value: int) -> None:
        self._cards[2].set_value("nodeid", value)

    @property
    def vecid(self) -> int:
        """Get or set the The vector ID defining the direction of flow at each point source.
        """ # nopep8
        return self._cards[2].get_value("vecid")

    @vecid.setter
    def vecid(self, value: int) -> None:
        self._cards[2].set_value("vecid", value)

    @property
    def orifarea(self) -> float:
        """Get or set the The orifice area at each point source
        """ # nopep8
        return self._cards[2].get_value("orifarea")

    @orifarea.setter
    def orifarea(self, value: float) -> None:
        self._cards[2].set_value("orifarea", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[3].cards[0].set_value("title", value)

