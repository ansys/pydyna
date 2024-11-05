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

class LoadFaceXyz(KeywordBase):
    """DYNA LOAD_FACE_XYZ keyword"""

    keyword = "LOAD"
    subkeyword = "FACE_XYZ"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "fxyzid",
                        int,
                        0,
                        10,
                        kwargs.get("fxyzid")
                    ),
                    Field(
                        "lcid",
                        int,
                        10,
                        10,
                        kwargs.get("lcid")
                    ),
                    Field(
                        "sf",
                        float,
                        20,
                        10,
                        kwargs.get("sf", 1.0)
                    ),
                    Field(
                        "at",
                        float,
                        30,
                        10,
                        kwargs.get("at", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def fxyzid(self) -> typing.Optional[int]:
        """Get or set the Physical face ID for the SET keyword option ; see *IGA_FACE_XYZ
        """ # nopep8
        return self._cards[0].get_value("fxyzid")

    @fxyzid.setter
    def fxyzid(self, value: int) -> None:
        self._cards[0].set_value("fxyzid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID (see *DEFINE_CURVE). The load curve must provide pressure as a function of time
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Load curve scale factor
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        self._cards[0].set_value("sf", value)

    @property
    def at(self) -> float:
        """Get or set the Arrival or birth time of pressure
        """ # nopep8
        return self._cards[0].get_value("at")

    @at.setter
    def at(self, value: float) -> None:
        self._cards[0].set_value("at", value)

