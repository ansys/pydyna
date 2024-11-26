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

class InitialStressIgaShell(KeywordBase):
    """DYNA INITIAL_STRESS_IGA_SHELL keyword"""

    keyword = "INITIAL"
    subkeyword = "STRESS_IGA_SHELL"

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
                        kwargs.get("nplane", 0)
                    ),
                    Field(
                        "nthick",
                        int,
                        20,
                        10,
                        kwargs.get("nthick", 0)
                    ),
                    Field(
                        "nhisv",
                        int,
                        30,
                        10,
                        kwargs.get("nhisv", 0)
                    ),
                    Field(
                        "large",
                        int,
                        40,
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
                        "sigxx",
                        float,
                        0,
                        10,
                        kwargs.get("sigxx", 0.0)
                    ),
                    Field(
                        "sigyy",
                        float,
                        10,
                        10,
                        kwargs.get("sigyy", 0.0)
                    ),
                    Field(
                        "sigzz",
                        float,
                        20,
                        10,
                        kwargs.get("sigzz", 0.0)
                    ),
                    Field(
                        "sigxy",
                        float,
                        30,
                        10,
                        kwargs.get("sigxy", 0.0)
                    ),
                    Field(
                        "sigyz",
                        float,
                        40,
                        10,
                        kwargs.get("sigyz", 0.0)
                    ),
                    Field(
                        "sigzx",
                        float,
                        50,
                        10,
                        kwargs.get("sigzx", 0.0)
                    ),
                    Field(
                        "eps",
                        float,
                        60,
                        10,
                        kwargs.get("eps", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "hisv1",
                        float,
                        0,
                        10,
                        kwargs.get("hisv1")
                    ),
                    Field(
                        "hisv2",
                        float,
                        10,
                        10,
                        kwargs.get("hisv2")
                    ),
                    Field(
                        "hisv3",
                        float,
                        20,
                        10,
                        kwargs.get("hisv3")
                    ),
                    Field(
                        "hisv4",
                        float,
                        30,
                        10,
                        kwargs.get("hisv4")
                    ),
                    Field(
                        "hisv5",
                        float,
                        40,
                        10,
                        kwargs.get("hisv5")
                    ),
                    Field(
                        "hisv6",
                        float,
                        50,
                        10,
                        kwargs.get("hisv6")
                    ),
                    Field(
                        "hisv7",
                        float,
                        60,
                        10,
                        kwargs.get("hisv7")
                    ),
                    Field(
                        "hisv8",
                        float,
                        70,
                        10,
                        kwargs.get("hisv8")
                    ),
                ],
            ),
        ]

    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the IGA shell element ID.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        self._cards[0].set_value("eid", value)

    @property
    def nplane(self) -> int:
        """Get or set the Number of in plane integration points being output.
        """ # nopep8
        return self._cards[0].get_value("nplane")

    @nplane.setter
    def nplane(self, value: int) -> None:
        self._cards[0].set_value("nplane", value)

    @property
    def nthick(self) -> int:
        """Get or set the Number of through thickness integration points.
        """ # nopep8
        return self._cards[0].get_value("nthick")

    @nthick.setter
    def nthick(self, value: int) -> None:
        self._cards[0].set_value("nthick", value)

    @property
    def nhisv(self) -> int:
        """Get or set the Number of additional history variables.
        """ # nopep8
        return self._cards[0].get_value("nhisv")

    @nhisv.setter
    def nhisv(self, value: int) -> None:
        self._cards[0].set_value("nhisv", value)

    @property
    def large(self) -> int:
        """Get or set the Format size (0:off or 1:on).
        """ # nopep8
        return self._cards[0].get_value("large")

    @large.setter
    def large(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""large must be one of {0,1}""")
        self._cards[0].set_value("large", value)

    @property
    def r(self) -> typing.Optional[float]:
        """Get or set the Parametric r-coordinate of location of in-plane integration point (with IGA 2D NURBS-patch definition)
        """ # nopep8
        return self._cards[1].get_value("r")

    @r.setter
    def r(self, value: float) -> None:
        self._cards[1].set_value("r", value)

    @property
    def s(self) -> typing.Optional[float]:
        """Get or set the Parametric s-coordinate of location of in-plane integration point (with IGA 2D NURBS-patch definition)
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
    def sigxx(self) -> float:
        """Get or set the Define the xx stress component. The stresses are defined in the GLOBAL cartesian system.
        """ # nopep8
        return self._cards[2].get_value("sigxx")

    @sigxx.setter
    def sigxx(self, value: float) -> None:
        self._cards[2].set_value("sigxx", value)

    @property
    def sigyy(self) -> float:
        """Get or set the Define the yy stress component. The stresses are defined in the GLOBAL cartesian system.
        """ # nopep8
        return self._cards[2].get_value("sigyy")

    @sigyy.setter
    def sigyy(self, value: float) -> None:
        self._cards[2].set_value("sigyy", value)

    @property
    def sigzz(self) -> float:
        """Get or set the Define the zz stress component. The stresses are defined in the GLOBAL cartesian system.
        """ # nopep8
        return self._cards[2].get_value("sigzz")

    @sigzz.setter
    def sigzz(self, value: float) -> None:
        self._cards[2].set_value("sigzz", value)

    @property
    def sigxy(self) -> float:
        """Get or set the Define the xy stress component. The stresses are defined in the GLOBAL cartesian system.
        """ # nopep8
        return self._cards[2].get_value("sigxy")

    @sigxy.setter
    def sigxy(self, value: float) -> None:
        self._cards[2].set_value("sigxy", value)

    @property
    def sigyz(self) -> float:
        """Get or set the Define the yz stress component. The stresses are defined in the GLOBAL cartesian system.
        """ # nopep8
        return self._cards[2].get_value("sigyz")

    @sigyz.setter
    def sigyz(self, value: float) -> None:
        self._cards[2].set_value("sigyz", value)

    @property
    def sigzx(self) -> float:
        """Get or set the Define the zx stress component. The stresses are defined in the GLOBAL cartesian system.
        """ # nopep8
        return self._cards[2].get_value("sigzx")

    @sigzx.setter
    def sigzx(self, value: float) -> None:
        self._cards[2].set_value("sigzx", value)

    @property
    def eps(self) -> float:
        """Get or set the Effective plastic strain.
        """ # nopep8
        return self._cards[2].get_value("eps")

    @eps.setter
    def eps(self, value: float) -> None:
        self._cards[2].set_value("eps", value)

    @property
    def hisv1(self) -> typing.Optional[float]:
        """Get or set the Define the nth history variable. The stresses are defined in the GLOBAL cartesian system
        """ # nopep8
        return self._cards[3].get_value("hisv1")

    @hisv1.setter
    def hisv1(self, value: float) -> None:
        self._cards[3].set_value("hisv1", value)

    @property
    def hisv2(self) -> typing.Optional[float]:
        """Get or set the Define the nth history variable. The stresses are defined in the GLOBAL cartesian system
        """ # nopep8
        return self._cards[3].get_value("hisv2")

    @hisv2.setter
    def hisv2(self, value: float) -> None:
        self._cards[3].set_value("hisv2", value)

    @property
    def hisv3(self) -> typing.Optional[float]:
        """Get or set the Define the nth history variable. The stresses are defined in the GLOBAL cartesian system
        """ # nopep8
        return self._cards[3].get_value("hisv3")

    @hisv3.setter
    def hisv3(self, value: float) -> None:
        self._cards[3].set_value("hisv3", value)

    @property
    def hisv4(self) -> typing.Optional[float]:
        """Get or set the Define the nth history variable. The stresses are defined in the GLOBAL cartesian system
        """ # nopep8
        return self._cards[3].get_value("hisv4")

    @hisv4.setter
    def hisv4(self, value: float) -> None:
        self._cards[3].set_value("hisv4", value)

    @property
    def hisv5(self) -> typing.Optional[float]:
        """Get or set the Define the nth history variable. The stresses are defined in the GLOBAL cartesian system
        """ # nopep8
        return self._cards[3].get_value("hisv5")

    @hisv5.setter
    def hisv5(self, value: float) -> None:
        self._cards[3].set_value("hisv5", value)

    @property
    def hisv6(self) -> typing.Optional[float]:
        """Get or set the Define the nth history variable. The stresses are defined in the GLOBAL cartesian system
        """ # nopep8
        return self._cards[3].get_value("hisv6")

    @hisv6.setter
    def hisv6(self, value: float) -> None:
        self._cards[3].set_value("hisv6", value)

    @property
    def hisv7(self) -> typing.Optional[float]:
        """Get or set the Define the nth history variable. The stresses are defined in the GLOBAL cartesian system
        """ # nopep8
        return self._cards[3].get_value("hisv7")

    @hisv7.setter
    def hisv7(self, value: float) -> None:
        self._cards[3].set_value("hisv7", value)

    @property
    def hisv8(self) -> typing.Optional[float]:
        """Get or set the Define the nth history variable. The stresses are defined in the GLOBAL cartesian system
        """ # nopep8
        return self._cards[3].get_value("hisv8")

    @hisv8.setter
    def hisv8(self, value: float) -> None:
        self._cards[3].set_value("hisv8", value)

