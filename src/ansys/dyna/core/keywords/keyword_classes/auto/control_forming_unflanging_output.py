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

class ControlFormingUnflangingOutput(KeywordBase):
    """DYNA CONTROL_FORMING_UNFLANGING_OUTPUT keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_UNFLANGING_OUTPUT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "thmx",
                        float,
                        0,
                        10,
                        kwargs.get("thmx", 1.0E+20)
                    ),
                    Field(
                        "thmn",
                        float,
                        10,
                        10,
                        kwargs.get("thmn", 0.0)
                    ),
                    Field(
                        "epsmx",
                        float,
                        20,
                        10,
                        kwargs.get("epsmx", 1.0E+20)
                    ),
                ],
            ),
        ]

    @property
    def thmx(self) -> float:
        """Get or set the Maximum thickness beyond which elements are deleted; this is
        useful in removing wrinkling areas of the flange (shrink flange).
        """ # nopep8
        return self._cards[0].get_value("thmx")

    @thmx.setter
    def thmx(self, value: float) -> None:
        self._cards[0].set_value("thmx", value)

    @property
    def thmn(self) -> float:
        """Get or set the Minimum thickness below which elements are deleted; this is useful
        in removing overly thinned areas of the flange (stretch flange).
        """ # nopep8
        return self._cards[0].get_value("thmn")

    @thmn.setter
    def thmn(self, value: float) -> None:
        self._cards[0].set_value("thmn", value)

    @property
    def epsmx(self) -> float:
        """Get or set the Maximum effective plastic strain beyond which elements are deleted,
        this is useful in removing flange areas with high effective plastic strains (stretch flange).
        """ # nopep8
        return self._cards[0].get_value("epsmx")

    @epsmx.setter
    def epsmx(self, value: float) -> None:
        self._cards[0].set_value("epsmx", value)

