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

class CeseEosInflator1(KeywordBase):
    """DYNA CESE_EOS_INFLATOR1 keyword"""

    keyword = "CESE"
    subkeyword = "EOS_INFLATOR1"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eosid",
                        int,
                        0,
                        10,
                        kwargs.get("eosid")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cp0",
                        float,
                        0,
                        10,
                        kwargs.get("cp0", 0.0)
                    ),
                    Field(
                        "cp1",
                        float,
                        10,
                        10,
                        kwargs.get("cp1", 0.0)
                    ),
                    Field(
                        "cp2",
                        float,
                        20,
                        10,
                        kwargs.get("cp2", 0.0)
                    ),
                    Field(
                        "cp3",
                        float,
                        30,
                        10,
                        kwargs.get("cp3", 0.0)
                    ),
                    Field(
                        "cp4",
                        float,
                        40,
                        10,
                        kwargs.get("cp4", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cv0",
                        float,
                        0,
                        10,
                        kwargs.get("cv0", 0.0)
                    ),
                    Field(
                        "cv1",
                        float,
                        10,
                        10,
                        kwargs.get("cv1", 0.0)
                    ),
                    Field(
                        "cv2",
                        float,
                        20,
                        10,
                        kwargs.get("cv2", 0.0)
                    ),
                    Field(
                        "cv3",
                        float,
                        30,
                        10,
                        kwargs.get("cv3", 0.0)
                    ),
                    Field(
                        "cv4",
                        float,
                        40,
                        10,
                        kwargs.get("cv4", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state identifier for the CESE solver.
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        self._cards[0].set_value("eosid", value)

    @property
    def cp0(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant pressure.
        """ # nopep8
        return self._cards[1].get_value("cp0")

    @cp0.setter
    def cp0(self, value: float) -> None:
        self._cards[1].set_value("cp0", value)

    @property
    def cp1(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant pressure.
        """ # nopep8
        return self._cards[1].get_value("cp1")

    @cp1.setter
    def cp1(self, value: float) -> None:
        self._cards[1].set_value("cp1", value)

    @property
    def cp2(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant pressure.
        """ # nopep8
        return self._cards[1].get_value("cp2")

    @cp2.setter
    def cp2(self, value: float) -> None:
        self._cards[1].set_value("cp2", value)

    @property
    def cp3(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant pressure.
        """ # nopep8
        return self._cards[1].get_value("cp3")

    @cp3.setter
    def cp3(self, value: float) -> None:
        self._cards[1].set_value("cp3", value)

    @property
    def cp4(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant pressure.
        """ # nopep8
        return self._cards[1].get_value("cp4")

    @cp4.setter
    def cp4(self, value: float) -> None:
        self._cards[1].set_value("cp4", value)

    @property
    def cv0(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volume.
        """ # nopep8
        return self._cards[2].get_value("cv0")

    @cv0.setter
    def cv0(self, value: float) -> None:
        self._cards[2].set_value("cv0", value)

    @property
    def cv1(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volume.
        """ # nopep8
        return self._cards[2].get_value("cv1")

    @cv1.setter
    def cv1(self, value: float) -> None:
        self._cards[2].set_value("cv1", value)

    @property
    def cv2(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volume.
        """ # nopep8
        return self._cards[2].get_value("cv2")

    @cv2.setter
    def cv2(self, value: float) -> None:
        self._cards[2].set_value("cv2", value)

    @property
    def cv3(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volume.
        """ # nopep8
        return self._cards[2].get_value("cv3")

    @cv3.setter
    def cv3(self, value: float) -> None:
        self._cards[2].set_value("cv3", value)

    @property
    def cv4(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volume.
        """ # nopep8
        return self._cards[2].get_value("cv4")

    @cv4.setter
    def cv4(self, value: float) -> None:
        self._cards[2].set_value("cv4", value)

