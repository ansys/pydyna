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

class ControlFormingShellToTshell(KeywordBase):
    """DYNA CONTROL_FORMING_SHELL_TO_TSHELL keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_SHELL_TO_TSHELL"

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
                        "thick",
                        float,
                        10,
                        10,
                        kwargs.get("thick")
                    ),
                    Field(
                        "midsf",
                        float,
                        20,
                        10,
                        kwargs.get("midsf", 0)
                    ),
                    Field(
                        "idsegb",
                        float,
                        30,
                        10,
                        kwargs.get("idsegb")
                    ),
                    Field(
                        "idsegt",
                        float,
                        40,
                        10,
                        kwargs.get("idsegt")
                    ),
                ],
            ),
        ]

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the thin shell elements.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        self._cards[0].set_value("pid", value)

    @property
    def thick(self) -> typing.Optional[float]:
        """Get or set the Thickness of the thick shell elements.
        """ # nopep8
        return self._cards[0].get_value("thick")

    @thick.setter
    def thick(self, value: float) -> None:
        self._cards[0].set_value("thick", value)

    @property
    def midsf(self) -> float:
        """Get or set the TSHELL’s mid-plane position definition (see Figure 0-1 and Remark 4):
        EQ.0:	Mid - plane is at thin shell surface.
        EQ.1 : Mid - plane is at one half of THICK above thin shell surface.
        EQ. - 1 : Mid - plane is at one half of THICK below thin shell surface.
        """ # nopep8
        return self._cards[0].get_value("midsf")

    @midsf.setter
    def midsf(self, value: float) -> None:
        if value not in [0, 1, -1]:
            raise Exception("""midsf must be one of {0,1,-1}""")
        self._cards[0].set_value("midsf", value)

    @property
    def idsegb(self) -> typing.Optional[float]:
        """Get or set the Set ID of the segments to be generated at the bottom layer of the TSHELLs, which can be used for segment-based contact.  The bottom layer of the TSHELLs has an outward normal that points in the opposite direction to the positive normal side of thin shells
        """ # nopep8
        return self._cards[0].get_value("idsegb")

    @idsegb.setter
    def idsegb(self, value: float) -> None:
        self._cards[0].set_value("idsegb", value)

    @property
    def idsegt(self) -> typing.Optional[float]:
        """Get or set the Set ID of the segments to be generated at the top layer of the TSHELLs, which can be used for segment-based contact.  The top side of a TSHELL has an outward normal that points in the same direction as the positive normal side of the thin shells
        """ # nopep8
        return self._cards[0].get_value("idsegt")

    @idsegt.setter
    def idsegt(self, value: float) -> None:
        self._cards[0].set_value("idsegt", value)

