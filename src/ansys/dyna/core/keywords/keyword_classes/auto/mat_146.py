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

class Mat146(KeywordBase):
    """DYNA MAT_146 keyword"""

    keyword = "MAT"
    subkeyword = "146"
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
                        "mid",
                        int,
                        0,
                        10,
                        kwargs.get("mid")
                    ),
                    Field(
                        "ro",
                        float,
                        10,
                        10,
                        kwargs.get("ro")
                    ),
                    Field(
                        "k",
                        float,
                        20,
                        10,
                        kwargs.get("k")
                    ),
                    Field(
                        "c",
                        float,
                        30,
                        10,
                        kwargs.get("c")
                    ),
                    Field(
                        "scln1",
                        float,
                        40,
                        10,
                        kwargs.get("scln1", 1.0)
                    ),
                    Field(
                        "scln2",
                        float,
                        50,
                        10,
                        kwargs.get("scln2", 1.0)
                    ),
                    Field(
                        "dofn1",
                        int,
                        60,
                        10,
                        kwargs.get("dofn1")
                    ),
                    Field(
                        "dofn2",
                        int,
                        70,
                        10,
                        kwargs.get("dofn2")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cid1",
                        int,
                        0,
                        10,
                        kwargs.get("cid1")
                    ),
                    Field(
                        "cid2",
                        int,
                        10,
                        10,
                        kwargs.get("cid2")
                    ),
                ],
            ),
            OptionCardSet(
                option_spec = Mat146.option_specs[0],
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
    def mid(self) -> typing.Optional[int]:
        """Get or set the Material identification. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("mid")

    @mid.setter
    def mid(self, value: int) -> None:
        self._cards[0].set_value("mid", value)

    @property
    def ro(self) -> typing.Optional[float]:
        """Get or set the Mass density, see also volume in *SECTION_BEAM definition.
        """ # nopep8
        return self._cards[0].get_value("ro")

    @ro.setter
    def ro(self, value: float) -> None:
        self._cards[0].set_value("ro", value)

    @property
    def k(self) -> typing.Optional[float]:
        """Get or set the Spring stiffness.
        """ # nopep8
        return self._cards[0].get_value("k")

    @k.setter
    def k(self, value: float) -> None:
        self._cards[0].set_value("k", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Damping constant.
        """ # nopep8
        return self._cards[0].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        self._cards[0].set_value("c", value)

    @property
    def scln1(self) -> float:
        """Get or set the Scale factor on force at node 1.  Default=1.0.
        """ # nopep8
        return self._cards[0].get_value("scln1")

    @scln1.setter
    def scln1(self, value: float) -> None:
        self._cards[0].set_value("scln1", value)

    @property
    def scln2(self) -> float:
        """Get or set the Scale factor on force at node 2.  Default=1.0.
        """ # nopep8
        return self._cards[0].get_value("scln2")

    @scln2.setter
    def scln2(self, value: float) -> None:
        self._cards[0].set_value("scln2", value)

    @property
    def dofn1(self) -> typing.Optional[int]:
        """Get or set the Active degree-of-freedom at node 1, a number between 1 to 6 where 1 in x-translation and 4 is x-rotation. If this parameter is defined in the SECTION_BEAM definition or on the ELEMENT_BEAM_SCALAR card, then the value here, if defined, is ignored.
        """ # nopep8
        return self._cards[0].get_value("dofn1")

    @dofn1.setter
    def dofn1(self, value: int) -> None:
        self._cards[0].set_value("dofn1", value)

    @property
    def dofn2(self) -> typing.Optional[int]:
        """Get or set the Active degree-of-freedom at node 2, a number between 1 to 6. If this parameter is defined in the SECTION_BEAM definition or on the ELEMENT_BEAM_SCALAR card, then the value here, if defined, is ignored.
        """ # nopep8
        return self._cards[0].get_value("dofn2")

    @dofn2.setter
    def dofn2(self, value: int) -> None:
        self._cards[0].set_value("dofn2", value)

    @property
    def cid1(self) -> typing.Optional[int]:
        """Get or set the Local coordinate system at node 1.  This coordinate system can be overwritten by a local system specified on the *ELEMENT_BEAM or *SECTION_BEAM keyword input.  If no coordinate system is specified, the global system is used.
        """ # nopep8
        return self._cards[1].get_value("cid1")

    @cid1.setter
    def cid1(self, value: int) -> None:
        self._cards[1].set_value("cid1", value)

    @property
    def cid2(self) -> typing.Optional[int]:
        """Get or set the Local coordinate system at node 2.  If CID2 = 0, CID2 = CID1.
        """ # nopep8
        return self._cards[1].get_value("cid2")

    @cid2.setter
    def cid2(self, value: int) -> None:
        self._cards[1].set_value("cid2", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[2].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[2].cards[0].set_value("title", value)

