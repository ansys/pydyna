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

class LoadSeismicSsiAux(KeywordBase):
    """DYNA LOAD_SEISMIC_SSI_AUX keyword"""

    keyword = "LOAD"
    subkeyword = "SEISMIC_SSI_AUX"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
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
                        "ssid",
                        int,
                        0,
                        10,
                        kwargs.get("ssid")
                    ),
                    Field(
                        "gmset",
                        int,
                        10,
                        10,
                        kwargs.get("gmset")
                    ),
                    Field(
                        "sf",
                        float,
                        20,
                        10,
                        kwargs.get("sf", 1.0)
                    ),
                    Field(
                        "birth",
                        float,
                        30,
                        10,
                        kwargs.get("birth", 0.0)
                    ),
                    Field(
                        "death",
                        float,
                        40,
                        10,
                        kwargs.get("death", 1.E+28)
                    ),
                    Field(
                        "isg",
                        int,
                        50,
                        10,
                        kwargs.get("isg", 0)
                    ),
                    Field(
                        "memgm",
                        int,
                        60,
                        10,
                        kwargs.get("memgm", 2500000)
                    ),
                ],
            ),
        ]

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Name of binary file containing recorded motions
        """ # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        self._cards[0].set_value("filename", value)

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Soil-structure interface ID.
        """ # nopep8
        return self._cards[1].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        self._cards[1].set_value("ssid", value)

    @property
    def gmset(self) -> typing.Optional[int]:
        """Get or set the Identifier for set of recorded motions; see *INTERFACE_SSI_AUX or *INTERFACE_SSI_AUX_?EMBEDDED
        """ # nopep8
        return self._cards[1].get_value("gmset")

    @gmset.setter
    def gmset(self, value: int) -> None:
        self._cards[1].set_value("gmset", value)

    @property
    def sf(self) -> float:
        """Get or set the Ground motion scale factor.
        """ # nopep8
        return self._cards[1].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        self._cards[1].set_value("sf", value)

    @property
    def birth(self) -> float:
        """Get or set the Time at which specified ground motion is activated.
        """ # nopep8
        return self._cards[1].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        self._cards[1].set_value("birth", value)

    @property
    def death(self) -> float:
        """Get or set the Time at which specified ground motion is removed.
        """ # nopep8
        return self._cards[1].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        self._cards[1].set_value("death", value)

    @property
    def isg(self) -> int:
        """Get or set the Definition of soil-structure interface:
        EQ.0: SSID is the ID for the soil-structure interface defined by *INTERFACE_SSI_ID for non-matching mesh between soil and structure.For the DECONV keyword option, ISG = 0 additionally flags that the free-field within motion is computed at depth
        EQ.1: SSID is segment set ID identifying soil-structure interface for merged meshes between soil and structure.For the DECONV, ISG = 1 additionally flags that the free-field outcrop motion is computed at depth.
        """ # nopep8
        return self._cards[1].get_value("isg")

    @isg.setter
    def isg(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""isg must be one of {0,1}""")
        self._cards[1].set_value("isg", value)

    @property
    def memgm(self) -> int:
        """Get or set the Size in words of buffer allocated to read in recorded motions
        """ # nopep8
        return self._cards[1].get_value("memgm")

    @memgm.setter
    def memgm(self, value: int) -> None:
        self._cards[1].set_value("memgm", value)

