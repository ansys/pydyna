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

class CeseEosInflator2(KeywordBase):
    """DYNA CESE_EOS_INFLATOR2 keyword"""

    keyword = "CESE"
    subkeyword = "EOS_INFLATOR2"

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
                        "cp1_0",
                        float,
                        0,
                        10,
                        kwargs.get("cp1_0", 0.0)
                    ),
                    Field(
                        "cp1_1",
                        float,
                        10,
                        10,
                        kwargs.get("cp1_1", 0.0)
                    ),
                    Field(
                        "cp1_2",
                        float,
                        20,
                        10,
                        kwargs.get("cp1_2", 0.0)
                    ),
                    Field(
                        "cp1_3",
                        float,
                        30,
                        10,
                        kwargs.get("cp1_3", 0.0)
                    ),
                    Field(
                        "cp1_4",
                        float,
                        40,
                        10,
                        kwargs.get("cp1_4", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cp2_0",
                        float,
                        0,
                        10,
                        kwargs.get("cp2_0", 0.0)
                    ),
                    Field(
                        "cp2_1",
                        float,
                        10,
                        10,
                        kwargs.get("cp2_1", 0.0)
                    ),
                    Field(
                        "cp2_2",
                        float,
                        20,
                        10,
                        kwargs.get("cp2_2", 0.0)
                    ),
                    Field(
                        "cp2_3",
                        float,
                        30,
                        10,
                        kwargs.get("cp2_3", 0.0)
                    ),
                    Field(
                        "cp2_4",
                        float,
                        40,
                        10,
                        kwargs.get("cp2_4", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cv1_0",
                        float,
                        0,
                        10,
                        kwargs.get("cv1_0", 0.0)
                    ),
                    Field(
                        "cv1_1",
                        float,
                        10,
                        10,
                        kwargs.get("cv1_1", 0.0)
                    ),
                    Field(
                        "cv1_2",
                        float,
                        20,
                        10,
                        kwargs.get("cv1_2", 0.0)
                    ),
                    Field(
                        "cv1_3",
                        float,
                        30,
                        10,
                        kwargs.get("cv1_3", 0.0)
                    ),
                    Field(
                        "cv1_4",
                        float,
                        40,
                        10,
                        kwargs.get("cv1_4", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cv2_0",
                        float,
                        0,
                        10,
                        kwargs.get("cv2_0", 0.0)
                    ),
                    Field(
                        "cv2_1",
                        float,
                        10,
                        10,
                        kwargs.get("cv2_1", 0.0)
                    ),
                    Field(
                        "cv2_2",
                        float,
                        20,
                        10,
                        kwargs.get("cv2_2", 0.0)
                    ),
                    Field(
                        "cv2_3",
                        float,
                        30,
                        10,
                        kwargs.get("cv2_3", 0.0)
                    ),
                    Field(
                        "cv2_4",
                        float,
                        40,
                        10,
                        kwargs.get("cv2_4", 0.0)
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
    def cp1_0(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant pressure valid for T<1000K.
        """ # nopep8
        return self._cards[1].get_value("cp1_0")

    @cp1_0.setter
    def cp1_0(self, value: float) -> None:
        self._cards[1].set_value("cp1_0", value)

    @property
    def cp1_1(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant pressure valid for T<1000K..
        """ # nopep8
        return self._cards[1].get_value("cp1_1")

    @cp1_1.setter
    def cp1_1(self, value: float) -> None:
        self._cards[1].set_value("cp1_1", value)

    @property
    def cp1_2(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant pressure valid for T<1000K..
        """ # nopep8
        return self._cards[1].get_value("cp1_2")

    @cp1_2.setter
    def cp1_2(self, value: float) -> None:
        self._cards[1].set_value("cp1_2", value)

    @property
    def cp1_3(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant pressure valid for T<1000K..
        """ # nopep8
        return self._cards[1].get_value("cp1_3")

    @cp1_3.setter
    def cp1_3(self, value: float) -> None:
        self._cards[1].set_value("cp1_3", value)

    @property
    def cp1_4(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant pressure valid for T<1000K..
        """ # nopep8
        return self._cards[1].get_value("cp1_4")

    @cp1_4.setter
    def cp1_4(self, value: float) -> None:
        self._cards[1].set_value("cp1_4", value)

    @property
    def cp2_0(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant pressure valid for T>1000K.
        """ # nopep8
        return self._cards[2].get_value("cp2_0")

    @cp2_0.setter
    def cp2_0(self, value: float) -> None:
        self._cards[2].set_value("cp2_0", value)

    @property
    def cp2_1(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant pressure valid for T>1000K.
        """ # nopep8
        return self._cards[2].get_value("cp2_1")

    @cp2_1.setter
    def cp2_1(self, value: float) -> None:
        self._cards[2].set_value("cp2_1", value)

    @property
    def cp2_2(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant pressure valid for T>1000K.
        """ # nopep8
        return self._cards[2].get_value("cp2_2")

    @cp2_2.setter
    def cp2_2(self, value: float) -> None:
        self._cards[2].set_value("cp2_2", value)

    @property
    def cp2_3(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant pressure valid for T>1000K.
        """ # nopep8
        return self._cards[2].get_value("cp2_3")

    @cp2_3.setter
    def cp2_3(self, value: float) -> None:
        self._cards[2].set_value("cp2_3", value)

    @property
    def cp2_4(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant pressure valid for T>1000K.
        """ # nopep8
        return self._cards[2].get_value("cp2_4")

    @cp2_4.setter
    def cp2_4(self, value: float) -> None:
        self._cards[2].set_value("cp2_4", value)

    @property
    def cv1_0(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volume valid for T<1000K.
        """ # nopep8
        return self._cards[3].get_value("cv1_0")

    @cv1_0.setter
    def cv1_0(self, value: float) -> None:
        self._cards[3].set_value("cv1_0", value)

    @property
    def cv1_1(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volume valid for T<1000K.
        """ # nopep8
        return self._cards[3].get_value("cv1_1")

    @cv1_1.setter
    def cv1_1(self, value: float) -> None:
        self._cards[3].set_value("cv1_1", value)

    @property
    def cv1_2(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volume valid for T<1000K.
        """ # nopep8
        return self._cards[3].get_value("cv1_2")

    @cv1_2.setter
    def cv1_2(self, value: float) -> None:
        self._cards[3].set_value("cv1_2", value)

    @property
    def cv1_3(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volume valid for T<1000K.
        """ # nopep8
        return self._cards[3].get_value("cv1_3")

    @cv1_3.setter
    def cv1_3(self, value: float) -> None:
        self._cards[3].set_value("cv1_3", value)

    @property
    def cv1_4(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volume valid for T<1000K.
        """ # nopep8
        return self._cards[3].get_value("cv1_4")

    @cv1_4.setter
    def cv1_4(self, value: float) -> None:
        self._cards[3].set_value("cv1_4", value)

    @property
    def cv2_0(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volume valid for T>1000K.
        """ # nopep8
        return self._cards[4].get_value("cv2_0")

    @cv2_0.setter
    def cv2_0(self, value: float) -> None:
        self._cards[4].set_value("cv2_0", value)

    @property
    def cv2_1(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volume valid for T>1000K.
        """ # nopep8
        return self._cards[4].get_value("cv2_1")

    @cv2_1.setter
    def cv2_1(self, value: float) -> None:
        self._cards[4].set_value("cv2_1", value)

    @property
    def cv2_2(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volume valid for T>1000K.
        """ # nopep8
        return self._cards[4].get_value("cv2_2")

    @cv2_2.setter
    def cv2_2(self, value: float) -> None:
        self._cards[4].set_value("cv2_2", value)

    @property
    def cv2_3(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volume valid for T>1000K.
        """ # nopep8
        return self._cards[4].get_value("cv2_3")

    @cv2_3.setter
    def cv2_3(self, value: float) -> None:
        self._cards[4].set_value("cv2_3", value)

    @property
    def cv2_4(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volume valid for T>1000K.
        """ # nopep8
        return self._cards[4].get_value("cv2_4")

    @cv2_4.setter
    def cv2_4(self, value: float) -> None:
        self._cards[4].set_value("cv2_4", value)

