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

class LoadVibroAcoustic(KeywordBase):
    """DYNA LOAD_VIBRO_ACOUSTIC keyword"""

    keyword = "LOAD"
    subkeyword = "VIBRO_ACOUSTIC"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "nmode",
                        float,
                        0,
                        10,
                        kwargs.get("nmode")
                    ),
                    Field(
                        "texpos",
                        float,
                        10,
                        10,
                        kwargs.get("texpos", 1.0)
                    ),
                    Field(
                        "tscale",
                        float,
                        20,
                        10,
                        kwargs.get("tscale", 0.0)
                    ),
                    Field(
                        "temper",
                        float,
                        30,
                        10,
                        kwargs.get("temper")
                    ),
                    Field(
                        "dampro",
                        float,
                        40,
                        10,
                        kwargs.get("dampro")
                    ),
                    Field(
                        "damptype",
                        float,
                        50,
                        10,
                        kwargs.get("damptype")
                    ),
                    Field(
                        "spltype",
                        float,
                        60,
                        10,
                        kwargs.get("spltype")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "lddamp",
                        int,
                        0,
                        10,
                        kwargs.get("lddamp")
                    ),
                    Field(
                        "ldspl",
                        int,
                        10,
                        10,
                        kwargs.get("ldspl")
                    ),
                    Field(
                        "ldvel",
                        int,
                        20,
                        10,
                        kwargs.get("ldvel")
                    ),
                    Field(
                        "ldflw",
                        int,
                        30,
                        10,
                        kwargs.get("ldflw")
                    ),
                    Field(
                        "ldspn",
                        int,
                        40,
                        10,
                        kwargs.get("ldspn")
                    ),
                ],
            ),
        ]

    @property
    def nmode(self) -> typing.Optional[float]:
        """Get or set the Number of normal vibration modes employed for coupling with excitation pressure field
        """ # nopep8
        return self._cards[0].get_value("nmode")

    @nmode.setter
    def nmode(self, value: float) -> None:
        self._cards[0].set_value("nmode", value)

    @property
    def texpos(self) -> float:
        """Get or set the Esposure time
        """ # nopep8
        return self._cards[0].get_value("texpos")

    @texpos.setter
    def texpos(self, value: float) -> None:
        self._cards[0].set_value("texpos", value)

    @property
    def tscale(self) -> float:
        """Get or set the Time scale
        """ # nopep8
        return self._cards[0].get_value("tscale")

    @tscale.setter
    def tscale(self, value: float) -> None:
        self._cards[0].set_value("tscale", value)

    @property
    def temper(self) -> typing.Optional[float]:
        """Get or set the Temperature
        """ # nopep8
        return self._cards[0].get_value("temper")

    @temper.setter
    def temper(self, value: float) -> None:
        self._cards[0].set_value("temper", value)

    @property
    def dampro(self) -> typing.Optional[float]:
        """Get or set the Damping ratio
        """ # nopep8
        return self._cards[0].get_value("dampro")

    @dampro.setter
    def dampro(self, value: float) -> None:
        self._cards[0].set_value("dampro", value)

    @property
    def damptype(self) -> typing.Optional[float]:
        """Get or set the Type of damping (=1, broadband; =2, modal damping)
        """ # nopep8
        return self._cards[0].get_value("damptype")

    @damptype.setter
    def damptype(self, value: float) -> None:
        self._cards[0].set_value("damptype", value)

    @property
    def spltype(self) -> typing.Optional[float]:
        """Get or set the Type of SPL input (=1, prs; =2, spl)
        """ # nopep8
        return self._cards[0].get_value("spltype")

    @spltype.setter
    def spltype(self, value: float) -> None:
        self._cards[0].set_value("spltype", value)

    @property
    def lddamp(self) -> typing.Optional[int]:
        """Get or set the Load curve for damping ratio (if non-constant)
        """ # nopep8
        return self._cards[1].get_value("lddamp")

    @lddamp.setter
    def lddamp(self, value: int) -> None:
        self._cards[1].set_value("lddamp", value)

    @property
    def ldspl(self) -> typing.Optional[int]:
        """Get or set the Load curve for PSD or SPL value vs. frequency
        """ # nopep8
        return self._cards[1].get_value("ldspl")

    @ldspl.setter
    def ldspl(self, value: int) -> None:
        self._cards[1].set_value("ldspl", value)

    @property
    def ldvel(self) -> typing.Optional[int]:
        """Get or set the Load curve for phase velocity
        """ # nopep8
        return self._cards[1].get_value("ldvel")

    @ldvel.setter
    def ldvel(self, value: int) -> None:
        self._cards[1].set_value("ldvel", value)

    @property
    def ldflw(self) -> typing.Optional[int]:
        """Get or set the Load curve for exponential decay for TBL in flow-wise direction
        """ # nopep8
        return self._cards[1].get_value("ldflw")

    @ldflw.setter
    def ldflw(self, value: int) -> None:
        self._cards[1].set_value("ldflw", value)

    @property
    def ldspn(self) -> typing.Optional[int]:
        """Get or set the Load curve for exponential decay for TBL in span-wise direction
        """ # nopep8
        return self._cards[1].get_value("ldspn")

    @ldspn.setter
    def ldspn(self, value: int) -> None:
        self._cards[1].set_value("ldspn", value)

