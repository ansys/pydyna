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

class BoundaryUsaSurface(KeywordBase):
    """DYNA BOUNDARY_USA_SURFACE keyword"""

    keyword = "BOUNDARY"
    subkeyword = "USA_SURFACE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ssid",
                        int,
                        0,
                        10,
                        kwargs.get("ssid")
                    ),
                    Field(
                        "wetdry",
                        int,
                        10,
                        10,
                        kwargs.get("wetdry", 0)
                    ),
                    Field(
                        "nbeam",
                        int,
                        20,
                        10,
                        kwargs.get("nbeam", 0)
                    ),
                ],
            ),
        ]

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID, see *SET_SEGMENT.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        self._cards[0].set_value("ssid", value)

    @property
    def wetdry(self) -> int:
        """Get or set the Wet surface flag:
        EQ.0: dry, no coupling (default),
        EQ.1: wet, coupled with USA.
        """ # nopep8
        return self._cards[0].get_value("wetdry")

    @wetdry.setter
    def wetdry(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""wetdry must be one of {0,1}""")
        self._cards[0].set_value("wetdry", value)

    @property
    def nbeam(self) -> int:
        """Get or set the The number of nodes touched by USA Surface-of-Revolution (SOR) elements. It is not necessary that the LS-DYNA model has beams where USA has beams (i.e., SOR elements), merely that the LS-DYNA model has nodes to receive the forces that USA will return.
        """ # nopep8
        return self._cards[0].get_value("nbeam")

    @nbeam.setter
    def nbeam(self, value: int) -> None:
        self._cards[0].set_value("nbeam", value)

