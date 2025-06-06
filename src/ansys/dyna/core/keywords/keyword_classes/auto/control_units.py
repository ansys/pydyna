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

"""Module providing the ControlUnits class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.keyword_base import KeywordBase

class ControlUnits(KeywordBase):
    """DYNA CONTROL_UNITS keyword"""

    keyword = "CONTROL"
    subkeyword = "UNITS"

    def __init__(self, **kwargs):
        """Initialize the ControlUnits class."""
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "length",
                        str,
                        0,
                        10,
                        "m",
                        **kwargs,
                    ),
                    Field(
                        "time",
                        str,
                        10,
                        10,
                        "sec",
                        **kwargs,
                    ),
                    Field(
                        "mass",
                        str,
                        20,
                        10,
                        "kg",
                        **kwargs,
                    ),
                    Field(
                        "temp",
                        str,
                        30,
                        10,
                        "k",
                        **kwargs,
                    ),
                ],
            ),
        ]

    @property
    def length(self) -> str:
        """Get or set the Length unit.
        m: meter
        mm: millimeter
        cm: centimeter
        mil: mile
        in: inch
        ft: foot
        yd: yard
        """ # nopep8
        return self._cards[0].get_value("length")

    @length.setter
    def length(self, value: str) -> None:
        """Set the length property."""
        if value not in ["m", "mm", "cm", "mil", "in", "ft", "yd", None]:
            raise Exception("""length must be `None` or one of {"m","mm","cm","mil","in","ft","yd"}.""")
        self._cards[0].set_value("length", value)

    @property
    def time(self) -> str:
        """Get or set the Time unit.
        sec: second
        min: minute
        hr: hour
        ms: msec, millisec
        micro_s: microsec.
        """ # nopep8
        return self._cards[0].get_value("time")

    @time.setter
    def time(self, value: str) -> None:
        """Set the time property."""
        if value not in ["sec", "min", "hr", "ms", "micro_s", None]:
            raise Exception("""time must be `None` or one of {"sec","min","hr","ms","micro_s"}.""")
        self._cards[0].set_value("time", value)

    @property
    def mass(self) -> str:
        """Get or set the Mass unit.
        kg: kilogram
        g: gram
        oz: ounce
        lb: pound
        ton: ton
        ntrc_ton
        """ # nopep8
        return self._cards[0].get_value("mass")

    @mass.setter
    def mass(self, value: str) -> None:
        """Set the mass property."""
        if value not in ["kg", "g", "oz", "lb", "ton", "mtrc_ton", None]:
            raise Exception("""mass must be `None` or one of {"kg","g","oz","lb","ton","mtrc_ton"}.""")
        self._cards[0].set_value("mass", value)

    @property
    def temp(self) -> str:
        """Get or set the Temperature unit.
        k: kelvin
        c: celsius
        f: fahrenheit
        r: rankine
        """ # nopep8
        return self._cards[0].get_value("temp")

    @temp.setter
    def temp(self, value: str) -> None:
        """Set the temp property."""
        if value not in ["k", "c", "f", "r", None]:
            raise Exception("""temp must be `None` or one of {"k","c","f","r"}.""")
        self._cards[0].set_value("temp", value)

