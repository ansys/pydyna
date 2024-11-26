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

class IcfdControlConj(KeywordBase):
    """DYNA ICFD_CONTROL_CONJ keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_CONJ"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ctype",
                        int,
                        0,
                        10,
                        kwargs.get("ctype", 0)
                    ),
                    Field(
                        "unused",
                        int,
                        10,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        20,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        40,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "unused",
                        int,
                        60,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "tsf",
                        float,
                        70,
                        10,
                        kwargs.get("tsf")
                    ),
                ],
            ),
        ]

    @property
    def ctype(self) -> int:
        """Get or set the Indicates the thermal coupling type.
        EQ.0: Robust and accurate monolithic coupling where the temperature field are solved simultaneously between the fluid and the structure.
        EQ.1: Weak thermal coupling. The fluid passes the heat flux to the solid at the fluid-structure interface and the solid returns the temperature which is applied as a Dirichlet condition.
        """ # nopep8
        return self._cards[0].get_value("ctype")

    @ctype.setter
    def ctype(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""ctype must be one of {0,1}""")
        self._cards[0].set_value("ctype", value)

    @property
    def tsf(self) -> typing.Optional[float]:
        """Get or set the Thermal Speedup Factor. This factor multiplies all thermal parameters present in the heat equation with  units  of  time  in  the  denominator  (e.g.,  thermal  conductivity,  convection  heat  transfer  coefficients).    It  is  used  to artificially  time  scale  the thermal problem. A negative value will refer to a time dependent load curve.
        """ # nopep8
        return self._cards[0].get_value("tsf")

    @tsf.setter
    def tsf(self, value: float) -> None:
        self._cards[0].set_value("tsf", value)

