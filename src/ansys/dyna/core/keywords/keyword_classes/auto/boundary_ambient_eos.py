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

class BoundaryAmbientEos(KeywordBase):
    """DYNA BOUNDARY_AMBIENT_EOS keyword"""

    keyword = "BOUNDARY"
    subkeyword = "AMBIENT_EOS"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "pid",
                        int,
                        0,
                        10,
                        kwargs.get("pid")
                    ),
                    Field(
                        "lc1",
                        int,
                        10,
                        10,
                        kwargs.get("lc1")
                    ),
                    Field(
                        "lc2",
                        int,
                        20,
                        10,
                        kwargs.get("lc2")
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the The ambient Part ID for which the thermodynamic state is being defined.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def lc1(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for internal energy per unit reference specific volume (or a temperature load curve ID if *EOS_IDEAL_GAS is being used).
        """ # nopep8
        return self._cards[0].get_value("lc1")

    @lc1.setter
    def lc1(self, value: int) -> None:
        self._cards[0].set_value("lc1", value)

    @property
    def lc2(self) -> typing.Optional[int]:
        """Get or set the Load curve ID for relative volume, see *DEFINE_CURVE.
        """ # nopep8
        return self._cards[0].get_value("lc2")

    @lc2.setter
    def lc2(self, value: int) -> None:
        self._cards[0].set_value("lc2", value)

