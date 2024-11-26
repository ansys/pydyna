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

class DualceseReactionRateIgReduced(KeywordBase):
    """DYNA DUALCESE_REACTION_RATE_IG_REDUCED keyword"""

    keyword = "DUALCESE"
    subkeyword = "REACTION_RATE_IG_REDUCED"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "react_id",
                        int,
                        0,
                        10,
                        kwargs.get("react_id")
                    ),
                    Field(
                        "grow1",
                        float,
                        10,
                        10,
                        kwargs.get("grow1")
                    ),
                    Field(
                        "cc",
                        float,
                        20,
                        10,
                        kwargs.get("cc")
                    ),
                    Field(
                        "dd",
                        float,
                        30,
                        10,
                        kwargs.get("dd")
                    ),
                    Field(
                        "yy",
                        float,
                        40,
                        10,
                        kwargs.get("yy")
                    ),
                    Field(
                        "ph10",
                        float,
                        50,
                        10,
                        kwargs.get("ph10")
                    ),
                ],
            ),
        ]

    @property
    def react_id(self) -> typing.Optional[int]:
        """Get or set the ID of reaction rate law
        """ # nopep8
        return self._cards[0].get_value("react_id")

    @react_id.setter
    def react_id(self, value: int) -> None:
        self._cards[0].set_value("react_id", value)

    @property
    def grow1(self) -> typing.Optional[float]:
        """Get or set the Reaction growth term parameter
        """ # nopep8
        return self._cards[0].get_value("grow1")

    @grow1.setter
    def grow1(self, value: float) -> None:
        self._cards[0].set_value("grow1", value)

    @property
    def cc(self) -> typing.Optional[float]:
        """Get or set the Reaction growth term parameter
        """ # nopep8
        return self._cards[0].get_value("cc")

    @cc.setter
    def cc(self, value: float) -> None:
        self._cards[0].set_value("cc", value)

    @property
    def dd(self) -> typing.Optional[float]:
        """Get or set the Reaction growth term parameter
        """ # nopep8
        return self._cards[0].get_value("dd")

    @dd.setter
    def dd(self, value: float) -> None:
        self._cards[0].set_value("dd", value)

    @property
    def yy(self) -> typing.Optional[float]:
        """Get or set the Reaction growth term parameter
        """ # nopep8
        return self._cards[0].get_value("yy")

    @yy.setter
    def yy(self, value: float) -> None:
        self._cards[0].set_value("yy", value)

    @property
    def ph10(self) -> typing.Optional[float]:
        """Get or set the Additional parameter to account for the non-zero amount of reaction when the mass fraction of the products, is zero
        """ # nopep8
        return self._cards[0].get_value("ph10")

    @ph10.setter
    def ph10(self, value: float) -> None:
        self._cards[0].set_value("ph10", value)

