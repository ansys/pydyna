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

class ConstrainedFemPeriTieBreak(KeywordBase):
    """DYNA CONSTRAINED_FEM_PERI_TIE_BREAK keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "FEM_PERI_TIE_BREAK"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "cid",
                        int,
                        0,
                        10,
                        kwargs.get("cid")
                    ),
                    Field(
                        "msid",
                        int,
                        10,
                        10,
                        kwargs.get("msid")
                    ),
                    Field(
                        "ssid",
                        int,
                        20,
                        10,
                        kwargs.get("ssid")
                    ),
                    Field(
                        "ft",
                        int,
                        30,
                        10,
                        kwargs.get("ft", 100000000000000000000)
                    ),
                    Field(
                        "fs",
                        int,
                        40,
                        10,
                        kwargs.get("fs", 100000000000000000000)
                    ),
                ],
            ),
        ]

    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Contact ID.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        self._cards[0].set_value("cid", value)

    @property
    def msid(self) -> typing.Optional[int]:
        """Get or set the The FEM part ID.
        """ # nopep8
        return self._cards[0].get_value("msid")

    @msid.setter
    def msid(self, value: int) -> None:
        self._cards[0].set_value("msid", value)

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the The peridynamic part ID.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        self._cards[0].set_value("ssid", value)

    @property
    def ft(self) -> int:
        """Get or set the The tensile pressure to break the tie.
        """ # nopep8
        return self._cards[0].get_value("ft")

    @ft.setter
    def ft(self, value: int) -> None:
        self._cards[0].set_value("ft", value)

    @property
    def fs(self) -> int:
        """Get or set the The shear pressure to break the tie.
        """ # nopep8
        return self._cards[0].get_value("fs")

    @fs.setter
    def fs(self, value: int) -> None:
        self._cards[0].set_value("fs", value)

