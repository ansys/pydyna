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

class FrequencyDomainModeLoadProjectionExclude(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_MODE_LOAD_PROJECTION_EXCLUDE keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_MODE_LOAD_PROJECTION_EXCLUDE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nmsort",
                        int,
                        0,
                        10,
                        kwargs.get("nmsort")
                    ),
                    Field(
                        "dskip",
                        float,
                        10,
                        10,
                        kwargs.get("dskip")
                    ),
                ],
            ),
        ]

    @property
    def nmsort(self) -> typing.Optional[int]:
        """Get or set the Number of modes to be retained which have the largest load projection ratios.
        """ # nopep8
        return self._cards[0].get_value("nmsort")

    @nmsort.setter
    def nmsort(self, value: int) -> None:
        self._cards[0].set_value("nmsort", value)

    @property
    def dskip(self) -> typing.Optional[float]:
        """Get or set the The threshold load projection ratio. All modes with load projection ratio less than this value will be skipped.
        """ # nopep8
        return self._cards[0].get_value("dskip")

    @dskip.setter
    def dskip(self, value: float) -> None:
        self._cards[0].set_value("dskip", value)

