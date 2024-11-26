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

class DualceseEosInflator2(KeywordBase):
    """DYNA DUALCESE_EOS_INFLATOR2 keyword"""

    keyword = "DUALCESE"
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
                        "cp10",
                        float,
                        0,
                        10,
                        kwargs.get("cp10", 0.0)
                    ),
                    Field(
                        "cp11",
                        float,
                        10,
                        10,
                        kwargs.get("cp11", 0.0)
                    ),
                    Field(
                        "cp12",
                        float,
                        20,
                        10,
                        kwargs.get("cp12", 0.0)
                    ),
                    Field(
                        "cp13",
                        float,
                        30,
                        10,
                        kwargs.get("cp13", 0.0)
                    ),
                    Field(
                        "cp14",
                        float,
                        40,
                        10,
                        kwargs.get("cp14", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cp20",
                        float,
                        0,
                        10,
                        kwargs.get("cp20", 0.0)
                    ),
                    Field(
                        "cp21",
                        float,
                        10,
                        10,
                        kwargs.get("cp21", 0.0)
                    ),
                    Field(
                        "cp22",
                        float,
                        20,
                        10,
                        kwargs.get("cp22", 0.0)
                    ),
                    Field(
                        "cp23",
                        float,
                        30,
                        10,
                        kwargs.get("cp23", 0.0)
                    ),
                    Field(
                        "cp24",
                        float,
                        40,
                        10,
                        kwargs.get("cp24", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cv10",
                        float,
                        0,
                        10,
                        kwargs.get("cv10", 0.0)
                    ),
                    Field(
                        "cv11",
                        float,
                        10,
                        10,
                        kwargs.get("cv11", 0.0)
                    ),
                    Field(
                        "cv12",
                        float,
                        20,
                        10,
                        kwargs.get("cv12", 0.0)
                    ),
                    Field(
                        "cv13",
                        float,
                        30,
                        10,
                        kwargs.get("cv13", 0.0)
                    ),
                    Field(
                        "cv14",
                        float,
                        40,
                        10,
                        kwargs.get("cv14", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "cv20",
                        float,
                        0,
                        10,
                        kwargs.get("cv20", 0.0)
                    ),
                    Field(
                        "cv21",
                        float,
                        10,
                        10,
                        kwargs.get("cv21", 0.0)
                    ),
                    Field(
                        "cv22",
                        float,
                        20,
                        10,
                        kwargs.get("cv22", 0.0)
                    ),
                    Field(
                        "cv23",
                        float,
                        30,
                        10,
                        kwargs.get("cv23", 0.0)
                    ),
                    Field(
                        "cv24",
                        float,
                        40,
                        10,
                        kwargs.get("cv24", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state identifier
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        self._cards[0].set_value("eosid", value)

    @property
    def cp10(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant pressureCp1_(T) = Cp1_0 + Cp1_1 T + Cp1_2 T2 + Cp1_3 T3 + Cp1_4 T4
        """ # nopep8
        return self._cards[1].get_value("cp10")

    @cp10.setter
    def cp10(self, value: float) -> None:
        self._cards[1].set_value("cp10", value)

    @property
    def cp11(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant pressureCp1_(T) = Cp1_0 + Cp1_1 T + Cp1_2 T2 + Cp1_3 T3 + Cp1_4 T4
        """ # nopep8
        return self._cards[1].get_value("cp11")

    @cp11.setter
    def cp11(self, value: float) -> None:
        self._cards[1].set_value("cp11", value)

    @property
    def cp12(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant pressureCp1_(T) = Cp1_0 + Cp1_1 T + Cp1_2 T2 + Cp1_3 T3 + Cp1_4 T4
        """ # nopep8
        return self._cards[1].get_value("cp12")

    @cp12.setter
    def cp12(self, value: float) -> None:
        self._cards[1].set_value("cp12", value)

    @property
    def cp13(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant pressureCp1_(T) = Cp1_0 + Cp1_1 T + Cp1_2 T2 + Cp1_3 T3 + Cp1_4 T4
        """ # nopep8
        return self._cards[1].get_value("cp13")

    @cp13.setter
    def cp13(self, value: float) -> None:
        self._cards[1].set_value("cp13", value)

    @property
    def cp14(self) -> float:
        """Get or set the Coefficients of temperature - dependent specific heat at constant pressureCp1_(T) = Cp1_0 + Cp1_1 T + Cp1_2 T2 + Cp1_3 T3 + Cp1_4 T4
        """ # nopep8
        return self._cards[1].get_value("cp14")

    @cp14.setter
    def cp14(self, value: float) -> None:
        self._cards[1].set_value("cp14", value)

    @property
    def cp20(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volumeCp2_(T) = Cp2_0 + Cp2_1 T + Cp2_2 T2 + Cp2_3 T3 + Cp2_4 T4
        """ # nopep8
        return self._cards[2].get_value("cp20")

    @cp20.setter
    def cp20(self, value: float) -> None:
        self._cards[2].set_value("cp20", value)

    @property
    def cp21(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volumeCp2_(T) = Cp2_0 + Cp2_1 T + Cp2_2 T2 + Cp2_3 T3 + Cp2_4 T4
        """ # nopep8
        return self._cards[2].get_value("cp21")

    @cp21.setter
    def cp21(self, value: float) -> None:
        self._cards[2].set_value("cp21", value)

    @property
    def cp22(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volumeCp2_(T) = Cp2_0 + Cp2_1 T + Cp2_2 T2 + Cp2_3 T3 + Cp2_4 T4
        """ # nopep8
        return self._cards[2].get_value("cp22")

    @cp22.setter
    def cp22(self, value: float) -> None:
        self._cards[2].set_value("cp22", value)

    @property
    def cp23(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volumeCp2_(T) = Cp2_0 + Cp2_1 T + Cp2_2 T2 + Cp2_3 T3 + Cp2_4 T4
        """ # nopep8
        return self._cards[2].get_value("cp23")

    @cp23.setter
    def cp23(self, value: float) -> None:
        self._cards[2].set_value("cp23", value)

    @property
    def cp24(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volumeCp2_(T) = Cp2_0 + Cp2_1 T + Cp2_2 T2 + Cp2_3 T3 + Cp2_4 T4
        """ # nopep8
        return self._cards[2].get_value("cp24")

    @cp24.setter
    def cp24(self, value: float) -> None:
        self._cards[2].set_value("cp24", value)

    @property
    def cv10(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv1_(T) = Cv1_0 + Cv1_1 T + Cv1_2 T2 + Cv1_3 T3 + Cv1_4 T4
        """ # nopep8
        return self._cards[3].get_value("cv10")

    @cv10.setter
    def cv10(self, value: float) -> None:
        self._cards[3].set_value("cv10", value)

    @property
    def cv11(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv1_(T) = Cv1_0 + Cv1_1 T + Cv1_2 T2 + Cv1_3 T3 + Cv1_4 T4
        """ # nopep8
        return self._cards[3].get_value("cv11")

    @cv11.setter
    def cv11(self, value: float) -> None:
        self._cards[3].set_value("cv11", value)

    @property
    def cv12(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv1_(T) = Cv1_0 + Cv1_1 T + Cv1_2 T2 + Cv1_3 T3 + Cv1_4 T4
        """ # nopep8
        return self._cards[3].get_value("cv12")

    @cv12.setter
    def cv12(self, value: float) -> None:
        self._cards[3].set_value("cv12", value)

    @property
    def cv13(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv1_(T) = Cv1_0 + Cv1_1 T + Cv1_2 T2 + Cv1_3 T3 + Cv1_4 T4
        """ # nopep8
        return self._cards[3].get_value("cv13")

    @cv13.setter
    def cv13(self, value: float) -> None:
        self._cards[3].set_value("cv13", value)

    @property
    def cv14(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv1_(T) = Cv1_0 + Cv1_1 T + Cv1_2 T2 + Cv1_3 T3 + Cv1_4 T4
        """ # nopep8
        return self._cards[3].get_value("cv14")

    @cv14.setter
    def cv14(self, value: float) -> None:
        self._cards[3].set_value("cv14", value)

    @property
    def cv20(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv2_(T) = Cv2_0 + Cv2_1 T + Cv2_2 T2 + Cv2_3 T3 + Cv2_4 T4
        """ # nopep8
        return self._cards[4].get_value("cv20")

    @cv20.setter
    def cv20(self, value: float) -> None:
        self._cards[4].set_value("cv20", value)

    @property
    def cv21(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv2_(T) = Cv2_0 + Cv2_1 T + Cv2_2 T2 + Cv2_3 T3 + Cv2_4 T4
        """ # nopep8
        return self._cards[4].get_value("cv21")

    @cv21.setter
    def cv21(self, value: float) -> None:
        self._cards[4].set_value("cv21", value)

    @property
    def cv22(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv2_(T) = Cv2_0 + Cv2_1 T + Cv2_2 T2 + Cv2_3 T3 + Cv2_4 T4
        """ # nopep8
        return self._cards[4].get_value("cv22")

    @cv22.setter
    def cv22(self, value: float) -> None:
        self._cards[4].set_value("cv22", value)

    @property
    def cv23(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv2_(T) = Cv2_0 + Cv2_1 T + Cv2_2 T2 + Cv2_3 T3 + Cv2_4 T4
        """ # nopep8
        return self._cards[4].get_value("cv23")

    @cv23.setter
    def cv23(self, value: float) -> None:
        self._cards[4].set_value("cv23", value)

    @property
    def cv24(self) -> float:
        """Get or set the Coefficients of temperature-dependent specific heat at constant volumeCv2_(T) = Cv2_0 + Cv2_1 T + Cv2_2 T2 + Cv2_3 T3 + Cv2_4 T4
        """ # nopep8
        return self._cards[4].get_value("cv24")

    @cv24.setter
    def cv24(self, value: float) -> None:
        self._cards[4].set_value("cv24", value)

