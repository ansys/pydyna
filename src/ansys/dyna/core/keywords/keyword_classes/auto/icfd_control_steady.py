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

class IcfdControlSteady(KeywordBase):
    """DYNA ICFD_CONTROL_STEADY keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_STEADY"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "its",
                        int,
                        0,
                        10,
                        kwargs.get("its", 1000000)
                    ),
                    Field(
                        "tol1",
                        float,
                        10,
                        10,
                        kwargs.get("tol1", 1.e-3)
                    ),
                    Field(
                        "tol2",
                        float,
                        20,
                        10,
                        kwargs.get("tol2", 1.e-3)
                    ),
                    Field(
                        "tol3",
                        float,
                        30,
                        10,
                        kwargs.get("tol3", 1.e-3)
                    ),
                    Field(
                        "rel1",
                        float,
                        40,
                        10,
                        kwargs.get("rel1", 0.3)
                    ),
                    Field(
                        "rel2",
                        float,
                        50,
                        10,
                        kwargs.get("rel2", 0.7)
                    ),
                    Field(
                        "urel",
                        float,
                        60,
                        10,
                        kwargs.get("urel", 1.)
                    ),
                    Field(
                        "order",
                        int,
                        70,
                        10,
                        kwargs.get("order", 0)
                    ),
                ],
            ),
        ]

    @property
    def its(self) -> int:
        """Get or set the Maximum number of iterations to reach convergence.
        """ # nopep8
        return self._cards[0].get_value("its")

    @its.setter
    def its(self, value: int) -> None:
        self._cards[0].set_value("its", value)

    @property
    def tol1(self) -> float:
        """Get or set the Tolerance limits for the momentum pressure and temperature equations respectfully.
        """ # nopep8
        return self._cards[0].get_value("tol1")

    @tol1.setter
    def tol1(self, value: float) -> None:
        self._cards[0].set_value("tol1", value)

    @property
    def tol2(self) -> float:
        """Get or set the Tolerance limits for the momentum pressure and temperature equations respectfully.
        """ # nopep8
        return self._cards[0].get_value("tol2")

    @tol2.setter
    def tol2(self, value: float) -> None:
        self._cards[0].set_value("tol2", value)

    @property
    def tol3(self) -> float:
        """Get or set the Tolerance limits for the momentum pressure and temperature equations respectfully.
        """ # nopep8
        return self._cards[0].get_value("tol3")

    @tol3.setter
    def tol3(self, value: float) -> None:
        self._cards[0].set_value("tol3", value)

    @property
    def rel1(self) -> float:
        """Get or set the Relaxation parameters for the velocity and pressure respectfully. Decreasing those values may add stability but more iterations may be needed to reach convergence.
        """ # nopep8
        return self._cards[0].get_value("rel1")

    @rel1.setter
    def rel1(self, value: float) -> None:
        self._cards[0].set_value("rel1", value)

    @property
    def rel2(self) -> float:
        """Get or set the Relaxation parameters for the velocity and pressure respectfully. Decreasing those values may add stability but more iterations may be needed to reach convergence.
        """ # nopep8
        return self._cards[0].get_value("rel2")

    @rel2.setter
    def rel2(self, value: float) -> None:
        self._cards[0].set_value("rel2", value)

    @property
    def urel(self) -> float:
        """Get or set the Under relaxation parameter. Lowering this value may improve the final accuracy of the solution but more iterations may be needed to achieve convergence.
        """ # nopep8
        return self._cards[0].get_value("urel")

    @urel.setter
    def urel(self, value: float) -> None:
        self._cards[0].set_value("urel", value)

    @property
    def order(self) -> int:
        """Get or set the Analysis order :
        EQ.0:	Second order. More accurate but more time consuming.
        EQ.1:	First order: More stable and faster but may be less accurate.
        """ # nopep8
        return self._cards[0].get_value("order")

    @order.setter
    def order(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""order must be one of {0,1}""")
        self._cards[0].set_value("order", value)

