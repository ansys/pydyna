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

class ControlImplicitForming(KeywordBase):
    """DYNA CONTROL_IMPLICIT_FORMING keyword"""

    keyword = "CONTROL"
    subkeyword = "IMPLICIT_FORMING"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ioption",
                        int,
                        0,
                        10,
                        kwargs.get("ioption", 1)
                    ),
                    Field(
                        "nsmin",
                        int,
                        10,
                        10,
                        kwargs.get("nsmin")
                    ),
                    Field(
                        "nsmax",
                        int,
                        20,
                        10,
                        kwargs.get("nsmax", 2)
                    ),
                    Field(
                        "birth",
                        float,
                        30,
                        10,
                        kwargs.get("birth", 0.0)
                    ),
                    Field(
                        "death",
                        float,
                        40,
                        10,
                        kwargs.get("death", 1.e+20)
                    ),
                    Field(
                        "penchk",
                        float,
                        50,
                        10,
                        kwargs.get("penchk", 0.0)
                    ),
                    Field(
                        "dt0",
                        float,
                        60,
                        10,
                        kwargs.get("dt0")
                    ),
                ],
            ),
        ]

    @property
    def ioption(self) -> int:
        """Get or set the Solution type:
        EQ.1: Gravity loading simulation, see remarks below.
        EQ.2: Binder closing and flanging simulation, see remarks below
        """ # nopep8
        return self._cards[0].get_value("ioption")

    @ioption.setter
    def ioption(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""ioption must be one of {1,2}""")
        self._cards[0].set_value("ioption", value)

    @property
    def nsmin(self) -> typing.Optional[int]:
        """Get or set the Minimum number of implicit steps for IOPTION = 2.
        """ # nopep8
        return self._cards[0].get_value("nsmin")

    @nsmin.setter
    def nsmin(self, value: int) -> None:
        self._cards[0].set_value("nsmin", value)

    @property
    def nsmax(self) -> int:
        """Get or set the Maximum number of implicit steps for IOPTION = 2
        """ # nopep8
        return self._cards[0].get_value("nsmax")

    @nsmax.setter
    def nsmax(self, value: int) -> None:
        self._cards[0].set_value("nsmax", value)

    @property
    def birth(self) -> float:
        """Get or set the Birth time to activate this feature
        """ # nopep8
        return self._cards[0].get_value("birth")

    @birth.setter
    def birth(self, value: float) -> None:
        self._cards[0].set_value("birth", value)

    @property
    def death(self) -> float:
        """Get or set the Death time to activate this feature
        """ # nopep8
        return self._cards[0].get_value("death")

    @death.setter
    def death(self, value: float) -> None:
        self._cards[0].set_value("death", value)

    @property
    def penchk(self) -> float:
        """Get or set the Relative allowed penetration with respect to the part thickness in contact for IOPTION = 2
        """ # nopep8
        return self._cards[0].get_value("penchk")

    @penchk.setter
    def penchk(self, value: float) -> None:
        self._cards[0].set_value("penchk", value)

    @property
    def dt0(self) -> typing.Optional[float]:
        """Get or set the Initial time step size that overrides the DT0 field defined in *CONTROL_IMPLICIT_GENERAL
        """ # nopep8
        return self._cards[0].get_value("dt0")

    @dt0.setter
    def dt0(self, value: float) -> None:
        self._cards[0].set_value("dt0", value)

