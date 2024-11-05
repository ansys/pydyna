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

class InitialFatigueDamageRatioD3Plot(KeywordBase):
    """DYNA INITIAL_FATIGUE_DAMAGE_RATIO_D3PLOT keyword"""

    keyword = "INITIAL"
    subkeyword = "FATIGUE_DAMAGE_RATIO_D3PLOT"

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
                        "nstate",
                        int,
                        0,
                        10,
                        kwargs.get("nstate")
                    ),
                    Field(
                        "neiphd",
                        int,
                        10,
                        10,
                        kwargs.get("neiphd")
                    ),
                    Field(
                        "neipsd",
                        int,
                        20,
                        10,
                        kwargs.get("neipsd")
                    ),
                ],
            ),
        ]

    @property
    def filename(self) -> typing.Optional[str]:
        """Get or set the Path and name of existing binary database for fatigue information.
        """ # nopep8
        return self._cards[0].get_value("filename")

    @filename.setter
    def filename(self, value: str) -> None:
        self._cards[0].set_value("filename", value)

    @property
    def nstate(self) -> typing.Optional[int]:
        """Get or set the State ID in binary database (e.g. d3plot) for reading damage variables.
        """ # nopep8
        return self._cards[1].get_value("nstate")

    @nstate.setter
    def nstate(self, value: int) -> None:
        self._cards[1].set_value("nstate", value)

    @property
    def neiphd(self) -> typing.Optional[int]:
        """Get or set the ID of additional integration point history variable which saves the damage for solid elements.
        """ # nopep8
        return self._cards[1].get_value("neiphd")

    @neiphd.setter
    def neiphd(self, value: int) -> None:
        self._cards[1].set_value("neiphd", value)

    @property
    def neipsd(self) -> typing.Optional[int]:
        """Get or set the ID of additional integration point history variable which saves the damage for shell and thick shell elements.
        """ # nopep8
        return self._cards[1].get_value("neipsd")

    @neipsd.setter
    def neipsd(self, value: int) -> None:
        self._cards[1].set_value("neipsd", value)

