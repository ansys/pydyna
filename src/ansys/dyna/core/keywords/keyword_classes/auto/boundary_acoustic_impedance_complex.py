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

class BoundaryAcousticImpedanceComplex(KeywordBase):
    """DYNA BOUNDARY_ACOUSTIC_IMPEDANCE_COMPLEX keyword"""

    keyword = "BOUNDARY"
    subkeyword = "ACOUSTIC_IMPEDANCE_COMPLEX"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
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
                        "zr",
                        float,
                        10,
                        10,
                        kwargs.get("zr", 0.0)
                    ),
                    Field(
                        "zi",
                        float,
                        20,
                        10,
                        kwargs.get("zi", 0.0)
                    ),
                    Field(
                        "lcidr",
                        int,
                        30,
                        10,
                        kwargs.get("lcidr", 0)
                    ),
                    Field(
                        "lcidi",
                        int,
                        40,
                        10,
                        kwargs.get("lcidi", 0)
                    ),
                ],
            ),
        ]

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID of an acoustic surface.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        self._cards[0].set_value("ssid", value)

    @property
    def zr(self) -> float:
        """Get or set the Real part of the boundary impedance Zr.
        """ # nopep8
        return self._cards[0].get_value("zr")

    @zr.setter
    def zr(self, value: float) -> None:
        self._cards[0].set_value("zr", value)

    @property
    def zi(self) -> float:
        """Get or set the Imaginary part of the boundary impedance Zi.
        """ # nopep8
        return self._cards[0].get_value("zi")

    @zi.setter
    def zi(self, value: float) -> None:
        self._cards[0].set_value("zi", value)

    @property
    def lcidr(self) -> int:
        """Get or set the Frequency dependence of Zr.
        """ # nopep8
        return self._cards[0].get_value("lcidr")

    @lcidr.setter
    def lcidr(self, value: int) -> None:
        self._cards[0].set_value("lcidr", value)

    @property
    def lcidi(self) -> int:
        """Get or set the Frequency dependence of Zi.
        """ # nopep8
        return self._cards[0].get_value("lcidi")

    @lcidi.setter
    def lcidi(self, value: int) -> None:
        self._cards[0].set_value("lcidi", value)

