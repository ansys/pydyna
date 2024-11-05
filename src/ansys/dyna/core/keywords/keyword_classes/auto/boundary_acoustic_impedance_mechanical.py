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

class BoundaryAcousticImpedanceMechanical(KeywordBase):
    """DYNA BOUNDARY_ACOUSTIC_IMPEDANCE_MECHANICAL keyword"""

    keyword = "BOUNDARY"
    subkeyword = "ACOUSTIC_IMPEDANCE_MECHANICAL"

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
                        "mparea",
                        float,
                        10,
                        10,
                        kwargs.get("mparea", 0.0)
                    ),
                    Field(
                        "cparea",
                        float,
                        20,
                        10,
                        kwargs.get("cparea", 0.0)
                    ),
                    Field(
                        "kparea",
                        int,
                        30,
                        10,
                        kwargs.get("kparea", 0)
                    ),
                ],
            ),
        ]

    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID of an acoustic surface.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        self._cards[0].set_value("ssid", value)

    @property
    def mparea(self) -> float:
        """Get or set the Mass per unit area m.
        """ # nopep8
        return self._cards[0].get_value("mparea")

    @mparea.setter
    def mparea(self, value: float) -> None:
        self._cards[0].set_value("mparea", value)

    @property
    def cparea(self) -> float:
        """Get or set the Damping per unit area c.
        """ # nopep8
        return self._cards[0].get_value("cparea")

    @cparea.setter
    def cparea(self, value: float) -> None:
        self._cards[0].set_value("cparea", value)

    @property
    def kparea(self) -> int:
        """Get or set the Stiffness per unit area k.
        """ # nopep8
        return self._cards[0].get_value("kparea")

    @kparea.setter
    def kparea(self, value: int) -> None:
        self._cards[0].set_value("kparea", value)

