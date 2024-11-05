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

class EfGrid(KeywordBase):
    """DYNA EF_GRID keyword"""

    keyword = "EF"
    subkeyword = "GRID"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ngx",
                        int,
                        0,
                        10,
                        kwargs.get("ngx")
                    ),
                    Field(
                        "ngy",
                        int,
                        10,
                        10,
                        kwargs.get("ngy")
                    ),
                    Field(
                        "ngz",
                        int,
                        20,
                        10,
                        kwargs.get("ngz")
                    ),
                ],
            ),
        ]

    @property
    def ngx(self) -> typing.Optional[int]:
        """Get or set the The mathematical algorithm underlying the ray tracer, involves gridding the enclosure.  NGX specify the number of grid divisions along the x axis.  This parameter does not affect LS-DYNA’s ability to obtain a solution, but it does affect the amount of CPU time consumed to process each photon.  There is no fixed rule for picking NGX, NGY, and NGZ, however for large geometries involving 1,000 to 15,000 surfaces NGX = NGY = NGZ = 25 is often optimal.  For smaller geometries smaller values are recommended
        """ # nopep8
        return self._cards[0].get_value("ngx")

    @ngx.setter
    def ngx(self, value: int) -> None:
        self._cards[0].set_value("ngx", value)

    @property
    def ngy(self) -> typing.Optional[int]:
        """Get or set the Specifies the number of grid divisions along the y-axis.
        """ # nopep8
        return self._cards[0].get_value("ngy")

    @ngy.setter
    def ngy(self, value: int) -> None:
        self._cards[0].set_value("ngy", value)

    @property
    def ngz(self) -> typing.Optional[int]:
        """Get or set the Specifies the number of grid divisions along the z-axis
        """ # nopep8
        return self._cards[0].get_value("ngz")

    @ngz.setter
    def ngz(self, value: int) -> None:
        self._cards[0].set_value("ngz", value)

