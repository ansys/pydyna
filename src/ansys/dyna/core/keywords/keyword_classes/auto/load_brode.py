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

"""Module providing the LoadBrode class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class LoadBrode(KeywordBase):
    """DYNA LOAD_BRODE keyword"""

    keyword = "LOAD"
    subkeyword = "BRODE"

    def __init__(self, **kwargs):
        """Initialize the LoadBrode class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "yld",
                        float,
                        0,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "bht",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "xbo",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "ybo",
                        float,
                        30,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "zbo",
                        float,
                        40,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "tbo",
                        float,
                        50,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "talc",
                        int,
                        60,
                        10,
                        0,
                        **kwargs,
                    ),
                    Field(
                        "sflc",
                        int,
                        70,
                        10,
                        0,
                        **kwargs,
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cfl",
                        float,
                        0,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "cft",
                        float,
                        10,
                        10,
                        0.0,
                        **kwargs,
                    ),
                    Field(
                        "cfp",
                        float,
                        20,
                        10,
                        0.0,
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def yld(self) -> float:
        """Get or set the Yield (Kt, equivalent tons of TNT).
        """ # nopep8
        return self._cards[0].get_value("yld")

    @yld.setter
    def yld(self, value: float) -> None:
        """Set the yld property."""
        self._cards[0].set_value("yld", value)

    @property
    def bht(self) -> float:
        """Get or set the Height of burst.
        """ # nopep8
        return self._cards[0].get_value("bht")

    @bht.setter
    def bht(self, value: float) -> None:
        """Set the bht property."""
        self._cards[0].set_value("bht", value)

    @property
    def xbo(self) -> float:
        """Get or set the x-coordinates of Brode origin.
        """ # nopep8
        return self._cards[0].get_value("xbo")

    @xbo.setter
    def xbo(self, value: float) -> None:
        """Set the xbo property."""
        self._cards[0].set_value("xbo", value)

    @property
    def ybo(self) -> float:
        """Get or set the y-coordinates of Brode origin.
        """ # nopep8
        return self._cards[0].get_value("ybo")

    @ybo.setter
    def ybo(self, value: float) -> None:
        """Set the ybo property."""
        self._cards[0].set_value("ybo", value)

    @property
    def zbo(self) -> float:
        """Get or set the z-coordinates of Brode origin.
        """ # nopep8
        return self._cards[0].get_value("zbo")

    @zbo.setter
    def zbo(self, value: float) -> None:
        """Set the zbo property."""
        self._cards[0].set_value("zbo", value)

    @property
    def tbo(self) -> float:
        """Get or set the Time offset of Brode origin.
        """ # nopep8
        return self._cards[0].get_value("tbo")

    @tbo.setter
    def tbo(self, value: float) -> None:
        """Set the tbo property."""
        self._cards[0].set_value("tbo", value)

    @property
    def talc(self) -> int:
        """Get or set the Load curve number giving time of arrival versus range relative to Brode origin (space, time), see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[0].get_value("talc")

    @talc.setter
    def talc(self, value: int) -> None:
        """Set the talc property."""
        self._cards[0].set_value("talc", value)

    @property
    def sflc(self) -> int:
        """Get or set the Load curve number giving yield scaling versus scaled time , see *DEFINE_ CURVE.
        """ # nopep8
        return self._cards[0].get_value("sflc")

    @sflc.setter
    def sflc(self, value: int) -> None:
        """Set the sflc property."""
        self._cards[0].set_value("sflc", value)

    @property
    def cfl(self) -> float:
        """Get or set the Conversion factor - kft to LS-DYNA length.
        """ # nopep8
        return self._cards[1].get_value("cfl")

    @cfl.setter
    def cfl(self, value: float) -> None:
        """Set the cfl property."""
        self._cards[1].set_value("cfl", value)

    @property
    def cft(self) -> float:
        """Get or set the Conversion factor - milliseconds to LS-DYNA time units.
        """ # nopep8
        return self._cards[1].get_value("cft")

    @cft.setter
    def cft(self, value: float) -> None:
        """Set the cft property."""
        self._cards[1].set_value("cft", value)

    @property
    def cfp(self) -> float:
        """Get or set the Conversion factor - psi to LS-DYNA pressure units.
        """ # nopep8
        return self._cards[1].get_value("cfp")

    @cfp.setter
    def cfp(self, value: float) -> None:
        """Set the cfp property."""
        self._cards[1].set_value("cfp", value)

