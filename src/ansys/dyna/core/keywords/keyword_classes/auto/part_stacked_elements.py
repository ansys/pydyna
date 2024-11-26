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
from ansys.dyna.core.lib.keyword_base import KeywordBase

class PartStackedElements(KeywordBase):
    """DYNA PART_STACKED_ELEMENTS keyword"""

    keyword = "PART"
    subkeyword = "STACKED_ELEMENTS"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
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
            Card(
                [
                    Field(
                        "pidref",
                        int,
                        0,
                        10,
                        kwargs.get("pidref", 0)
                    ),
                    Field(
                        "numlay",
                        int,
                        10,
                        10,
                        kwargs.get("numlay", 0)
                    ),
                    Field(
                        "adpopt",
                        int,
                        20,
                        10,
                        kwargs.get("adpopt", 0)
                    ),
                    Field(
                        "inplcmp",
                        int,
                        30,
                        10,
                        kwargs.get("inplcmp", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "pidi",
                        int,
                        0,
                        10,
                        kwargs.get("pidi")
                    ),
                    Field(
                        "sidi",
                        int,
                        10,
                        10,
                        kwargs.get("sidi")
                    ),
                    Field(
                        "midi",
                        int,
                        20,
                        10,
                        kwargs.get("midi")
                    ),
                    Field(
                        "hgidi",
                        int,
                        30,
                        10,
                        kwargs.get("hgidi", 0)
                    ),
                    Field(
                        "tmidi",
                        int,
                        40,
                        10,
                        kwargs.get("tmidi", 0)
                    ),
                    Field(
                        "thki",
                        float,
                        50,
                        10,
                        kwargs.get("thki")
                    ),
                    Field(
                        "nsldi",
                        int,
                        60,
                        10,
                        kwargs.get("nsldi")
                    ),
                ],
            ),
        ]

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Enter title for the datacard.
        """ # nopep8
        return self._cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        self._cards[0].set_value("title", value)

    @property
    def pidref(self) -> int:
        """Get or set the Part ID of reference shell element mesh.
        """ # nopep8
        return self._cards[1].get_value("pidref")

    @pidref.setter
    def pidref(self, value: int) -> None:
        self._cards[1].set_value("pidref", value)

    @property
    def numlay(self) -> int:
        """Get or set the Number of layers.
        """ # nopep8
        return self._cards[1].get_value("numlay")

    @numlay.setter
    def numlay(self, value: int) -> None:
        self._cards[1].set_value("numlay", value)

    @property
    def adpopt(self) -> int:
        """Get or set the Indicates if parts are adapted.
        """ # nopep8
        return self._cards[1].get_value("adpopt")

    @adpopt.setter
    def adpopt(self, value: int) -> None:
        self._cards[1].set_value("adpopt", value)

    @property
    def inplcmp(self) -> int:
        """Get or set the Option for in-plane composed parts:
        EQ.0:	Off
        EQ.1 : On;
        """ # nopep8
        return self._cards[1].get_value("inplcmp")

    @inplcmp.setter
    def inplcmp(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""inplcmp must be one of {0,1}""")
        self._cards[1].set_value("inplcmp", value)

    @property
    def pidi(self) -> typing.Optional[int]:
        """Get or set the Part identification.
        """ # nopep8
        return self._cards[2].get_value("pidi")

    @pidi.setter
    def pidi(self, value: int) -> None:
        self._cards[2].set_value("pidi", value)

    @property
    def sidi(self) -> typing.Optional[int]:
        """Get or set the Section identification for layer i defined in a *SECTION keyword.
        """ # nopep8
        return self._cards[2].get_value("sidi")

    @sidi.setter
    def sidi(self, value: int) -> None:
        self._cards[2].set_value("sidi", value)

    @property
    def midi(self) -> typing.Optional[int]:
        """Get or set the Material identification for layer i defined in a *MAT keyword.
        """ # nopep8
        return self._cards[2].get_value("midi")

    @midi.setter
    def midi(self, value: int) -> None:
        self._cards[2].set_value("midi", value)

    @property
    def hgidi(self) -> int:
        """Get or set the Hourglass identification for layer i defined in a *HOURGLASS keyword.
        """ # nopep8
        return self._cards[2].get_value("hgidi")

    @hgidi.setter
    def hgidi(self, value: int) -> None:
        self._cards[2].set_value("hgidi", value)

    @property
    def tmidi(self) -> int:
        """Get or set the Thermal material identification for layer i defined in a *MAT_THERMAL keyword.
        """ # nopep8
        return self._cards[2].get_value("tmidi")

    @tmidi.setter
    def tmidi(self, value: int) -> None:
        self._cards[2].set_value("tmidi", value)

    @property
    def thki(self) -> typing.Optional[float]:
        """Get or set the Thickness of layer i.
        """ # nopep8
        return self._cards[2].get_value("thki")

    @thki.setter
    def thki(self, value: float) -> None:
        self._cards[2].set_value("thki", value)

    @property
    def nsldi(self) -> typing.Optional[int]:
        """Get or set the Number of through-thickness solid elements for layer i.
        """ # nopep8
        return self._cards[2].get_value("nsldi")

    @nsldi.setter
    def nsldi(self, value: int) -> None:
        self._cards[2].set_value("nsldi", value)

