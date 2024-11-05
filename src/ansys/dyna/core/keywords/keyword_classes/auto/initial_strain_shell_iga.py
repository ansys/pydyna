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

class InitialStrainShellIga(KeywordBase):
    """DYNA INITIAL_STRAIN_SHELL_IGA keyword"""

    keyword = "INITIAL"
    subkeyword = "STRAIN_SHELL_IGA"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "eid",
                        int,
                        0,
                        10,
                        kwargs.get("eid")
                    ),
                    Field(
                        "nplane",
                        int,
                        10,
                        10,
                        kwargs.get("nplane")
                    ),
                    Field(
                        "nthk",
                        int,
                        20,
                        10,
                        kwargs.get("nthk", 0)
                    ),
                    Field(
                        "large",
                        int,
                        30,
                        10,
                        kwargs.get("large", 0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "r",
                        float,
                        0,
                        10,
                        kwargs.get("r")
                    ),
                    Field(
                        "s",
                        float,
                        10,
                        10,
                        kwargs.get("s")
                    ),
                    Field(
                        "t",
                        float,
                        20,
                        10,
                        kwargs.get("t")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "epsxx",
                        float,
                        0,
                        10,
                        kwargs.get("epsxx", 0.0)
                    ),
                    Field(
                        "epsyy",
                        float,
                        10,
                        10,
                        kwargs.get("epsyy", 0.0)
                    ),
                    Field(
                        "epszz",
                        float,
                        20,
                        10,
                        kwargs.get("epszz", 0.0)
                    ),
                    Field(
                        "epsxy",
                        float,
                        30,
                        10,
                        kwargs.get("epsxy", 0.0)
                    ),
                    Field(
                        "epsyz",
                        float,
                        40,
                        10,
                        kwargs.get("epsyz", 0.0)
                    ),
                    Field(
                        "epszx",
                        float,
                        50,
                        10,
                        kwargs.get("epszx", 0.0)
                    ),
                    Field(
                        "thki",
                        float,
                        60,
                        10,
                        kwargs.get("thki", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the iga element ID.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        self._cards[0].set_value("eid", value)

    @property
    def nplane(self) -> typing.Optional[int]:
        """Get or set the Number of in-plane integration points being output.
        """ # nopep8
        return self._cards[0].get_value("nplane")

    @nplane.setter
    def nplane(self, value: int) -> None:
        self._cards[0].set_value("nplane", value)

    @property
    def nthk(self) -> int:
        """Get or set the Flag for initialization of thicknesses at in-plane IPs:
        EQ.0:	o
        ffEQ.1 : on.
        """ # nopep8
        return self._cards[0].get_value("nthk")

    @nthk.setter
    def nthk(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""nthk must be one of {0,1}""")
        self._cards[0].set_value("nthk", value)

    @property
    def large(self) -> int:
        """Get or set the Large format flag:
        EQ.0:	off
        EQ.1 : on.Each strain field is twice as long for higher precision.
        """ # nopep8
        return self._cards[0].get_value("large")

    @large.setter
    def large(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""large must be one of {0,1}""")
        self._cards[0].set_value("large", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Parametric r-coordinate of location of in-plane integration point (with respect to iga shell definition)
        """ # nopep8
        return self._cards[1].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        self._cards[1].set_value("r", value)

    @property
    def s(self) -> typing.Optional[float]:
        """Get or set the Parametric s-coordinate of location of in-plane integration point (with respect to iga shell definition)
        """ # nopep8
        return self._cards[1].get_value("s")

    @s.setter
    def s(self, value: float) -> None:
        self._cards[1].set_value("s", value)

    @property
    def t(self) -> typing.Optional[float]:
        """Get or set the Parametric coordinate of through thickness integration point between -1 and 1 inclusive.
        """ # nopep8
        return self._cards[1].get_value("t")

    @t.setter
    def t(self, value: float) -> None:
        self._cards[1].set_value("t", value)

    @property
    def epsxx(self) -> float:
        """Get or set the Define the xx strain component. The strains are defined in the GLOBAL Cartesian system.
        """ # nopep8
        return self._cards[2].get_value("epsxx")

    @epsxx.setter
    def epsxx(self, value: float) -> None:
        self._cards[2].set_value("epsxx", value)

    @property
    def epsyy(self) -> float:
        """Get or set the Define the yy strain component.The strains are defined in the GLOBAL Cartesian system.
        """ # nopep8
        return self._cards[2].get_value("epsyy")

    @epsyy.setter
    def epsyy(self, value: float) -> None:
        self._cards[2].set_value("epsyy", value)

    @property
    def epszz(self) -> float:
        """Get or set the Define the zz strain component.The strains are defined in the GLOBAL Cartesian system.
        """ # nopep8
        return self._cards[2].get_value("epszz")

    @epszz.setter
    def epszz(self, value: float) -> None:
        self._cards[2].set_value("epszz", value)

    @property
    def epsxy(self) -> float:
        """Get or set the Define the xy strain component.The strains are defined in the GLOBAL Cartesian system.
        """ # nopep8
        return self._cards[2].get_value("epsxy")

    @epsxy.setter
    def epsxy(self, value: float) -> None:
        self._cards[2].set_value("epsxy", value)

    @property
    def epsyz(self) -> float:
        """Get or set the Define the yz strain component.The strains are defined in the GLOBAL Cartesian system.
        """ # nopep8
        return self._cards[2].get_value("epsyz")

    @epsyz.setter
    def epsyz(self, value: float) -> None:
        self._cards[2].set_value("epsyz", value)

    @property
    def epszx(self) -> float:
        """Get or set the Define the zx strain componentThe strains are defined in the GLOBAL Cartesian system.
        """ # nopep8
        return self._cards[2].get_value("epszx")

    @epszx.setter
    def epszx(self, value: float) -> None:
        self._cards[2].set_value("epszx", value)

    @property
    def thki(self) -> float:
        """Get or set the The thickness value at in-plane integration point
        """ # nopep8
        return self._cards[2].get_value("thki")

    @thki.setter
    def thki(self, value: float) -> None:
        self._cards[2].set_value("thki", value)

