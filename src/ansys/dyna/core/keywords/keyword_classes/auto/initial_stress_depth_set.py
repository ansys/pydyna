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

class InitialStressDepthSet(KeywordBase):
    """DYNA INITIAL_STRESS_DEPTH_SET keyword"""

    keyword = "INITIAL"
    subkeyword = "STRESS_DEPTH_SET"

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
                        kwargs.get("psid")
                    ),
                    Field(
                        "ro_g",
                        float,
                        10,
                        10,
                        kwargs.get("ro_g")
                    ),
                    Field(
                        "zdatum",
                        float,
                        20,
                        10,
                        kwargs.get("zdatum")
                    ),
                    Field(
                        "kfact",
                        float,
                        30,
                        10,
                        kwargs.get("kfact", 0.0)
                    ),
                    Field(
                        "lc",
                        int,
                        40,
                        10,
                        kwargs.get("lc")
                    ),
                    Field(
                        "lch",
                        int,
                        50,
                        10,
                        kwargs.get("lch")
                    ),
                    Field(
                        "lck0",
                        int,
                        60,
                        10,
                        kwargs.get("lck0")
                    ),
                ],
            ),
        ]

    @property
    def psid(self) -> typing.Optional[int]:
        """Get or set the Part Set ID.
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: int) -> None:
        self._cards[0].set_value("psid", value)

    @property
    def ro_g(self) -> typing.Optional[float]:
        """Get or set the Stress per unit elevation above datum (usually = density x gravity)
        """ # nopep8
        return self._cards[0].get_value("ro_g")

    @ro_g.setter
    def ro_g(self, value: float) -> None:
        self._cards[0].set_value("ro_g", value)

    @property
    def zdatum(self) -> typing.Optional[float]:
        """Get or set the Z-coordinate of datum
        """ # nopep8
        return self._cards[0].get_value("zdatum")

    @zdatum.setter
    def zdatum(self, value: float) -> None:
        self._cards[0].set_value("zdatum", value)

    @property
    def kfact(self) -> float:
        """Get or set the X- and Y-stress = KFACT x Z-stress
        """ # nopep8
        return self._cards[0].get_value("kfact")

    @kfact.setter
    def kfact(self, value: float) -> None:
        self._cards[0].set_value("kfact", value)

    @property
    def lc(self) -> typing.Optional[int]:
        """Get or set the Optional curve of stress vs z-coordinate (ZDATUM is ignored with this option)
        """ # nopep8
        return self._cards[0].get_value("lc")

    @lc.setter
    def lc(self, value: int) -> None:
        self._cards[0].set_value("lc", value)

    @property
    def lch(self) -> typing.Optional[int]:
        """Get or set the Optional curve of horizontal stress versus z-coordinate (KFACT is ignored with this option)
        """ # nopep8
        return self._cards[0].get_value("lch")

    @lch.setter
    def lch(self, value: int) -> None:
        self._cards[0].set_value("lch", value)

    @property
    def lck0(self) -> typing.Optional[int]:
        """Get or set the Optional curve of K0 (ratio of horizontal_stress/vertical_stress) versus coordinate. KFACT and LCH are ignored with this option. The axis of the curve is the coordinate, the axis is K0.)
        """ # nopep8
        return self._cards[0].get_value("lck0")

    @lck0.setter
    def lck0(self, value: int) -> None:
        self._cards[0].set_value("lck0", value)

