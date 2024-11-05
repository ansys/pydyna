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

class ControlUnits(KeywordBase):
    """DYNA CONTROL_UNITS keyword"""

    keyword = "CONTROL"
    subkeyword = "UNITS"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "length",
                        str,
                        0,
                        10,
                        kwargs.get("length", "m")
                    ),
                    Field(
                        "time",
                        str,
                        10,
                        10,
                        kwargs.get("time", "sec")
                    ),
                    Field(
                        "mass",
                        str,
                        20,
                        10,
                        kwargs.get("mass", "kg")
                    ),
                    Field(
                        "temp",
                        str,
                        30,
                        10,
                        kwargs.get("temp", "k")
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
        if value not in ["m", "mm", "cm", "mil", "in", "ft", "yd"]:
            raise Exception("""length must be one of {"m","mm","cm","mil","in","ft","yd"}""")
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
        if value not in ["sec", "min", "hr", "ms", "micro_s"]:
            raise Exception("""time must be one of {"sec","min","hr","ms","micro_s"}""")
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
        if value not in ["kg", "g", "oz", "lb", "ton", "mtrc_ton"]:
            raise Exception("""mass must be one of {"kg","g","oz","lb","ton","mtrc_ton"}""")
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
        if value not in ["k", "c", "f", "r"]:
            raise Exception("""temp must be one of {"k","c","f","r"}""")
        self._cards[0].set_value("temp", value)

