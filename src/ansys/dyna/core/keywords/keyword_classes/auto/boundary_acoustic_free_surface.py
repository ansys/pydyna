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

class BoundaryAcousticFreeSurface(KeywordBase):
    """DYNA BOUNDARY_ACOUSTIC_FREE_SURFACE keyword"""

    keyword = "BOUNDARY"
    subkeyword = "ACOUSTIC_FREE_SURFACE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ssids",
                        int,
                        0,
                        10,
                        kwargs.get("ssids")
                    ),
                    Field(
                        "ssidf",
                        int,
                        10,
                        10,
                        kwargs.get("ssidf")
                    ),
                ],
            ),
        ]

    @property
    def ssids(self) -> typing.Optional[int]:
        """Get or set the Segment set ID for the structural faces.
        """ # nopep8
        return self._cards[0].get_value("ssids")

    @ssids.setter
    def ssids(self, value: int) -> None:
        self._cards[0].set_value("ssids", value)

    @property
    def ssidf(self) -> typing.Optional[int]:
        """Get or set the Segment set ID for the fluid faces.
        """ # nopep8
        return self._cards[0].get_value("ssidf")

    @ssidf.setter
    def ssidf(self, value: int) -> None:
        self._cards[0].set_value("ssidf", value)

