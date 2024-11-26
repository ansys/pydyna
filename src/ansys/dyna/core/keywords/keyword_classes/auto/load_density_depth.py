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

class LoadDensityDepth(KeywordBase):
    """DYNA LOAD_DENSITY_DEPTH keyword"""

    keyword = "LOAD"
    subkeyword = "DENSITY_DEPTH"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "psid",
                        int,
                        0,
                        10,
                        kwargs.get("psid", 0)
                    ),
                    Field(
                        "gc",
                        float,
                        10,
                        10,
                        kwargs.get("gc", 0.0)
                    ),
                    Field(
                        "dir",
                        int,
                        20,
                        10,
                        kwargs.get("dir", 1)
                    ),
                    Field(
                        "lcid",
                        int,
                        30,
                        10,
                        kwargs.get("lcid")
                    ),
                ],
            ),
        ]

    @property
    def psid(self) -> int:
        """Get or set the Part set ID, see *SET_PART.
        EQ.0: all parts are initialized.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[0].set_value("psid", value)

    @property
    def gc(self) -> float:
        """Get or set the Gravitational acceleration value.
        """ # nopep8
        return self._cards[0].get_value("gc")

    @gc.setter
    def gc(self, value: float) -> None:
        self._cards[0].set_value("gc", value)

    @property
    def dir(self) -> int:
        """Get or set the Direction of loading:
        EQ.1: global x (default),
        EQ.2: global y,
        EQ.3: global z.
        """ # nopep8
        return self._cards[0].get_value("dir")

    @dir.setter
    def dir(self, value: int) -> None:
        if value not in [1, 2, 3]:
            raise Exception("""dir must be one of {1,2,3}""")
        self._cards[0].set_value("dir", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining density versus depth, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        self._cards[0].set_value("lcid", value)

