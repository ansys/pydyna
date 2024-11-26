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

class EmControlSolution(KeywordBase):
    """DYNA EM_CONTROL_SOLUTION keyword"""

    keyword = "EM"
    subkeyword = "CONTROL_SOLUTION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "ncylfem",
                        int,
                        0,
                        10,
                        kwargs.get("ncylfem", 5000)
                    ),
                    Field(
                        "ncylbem",
                        int,
                        10,
                        10,
                        kwargs.get("ncylbem", 5000)
                    ),
                    Field(
                        "autofem",
                        int,
                        20,
                        10,
                        kwargs.get("autofem", 0)
                    ),
                    Field(
                        "autobem",
                        int,
                        30,
                        10,
                        kwargs.get("autobem", 0)
                    ),
                    Field(
                        "tol1fem",
                        float,
                        40,
                        10,
                        kwargs.get("tol1fem", 0.3)
                    ),
                    Field(
                        "tol2fem",
                        float,
                        50,
                        10,
                        kwargs.get("tol2fem", 0.1)
                    ),
                    Field(
                        "tol1bem",
                        float,
                        60,
                        10,
                        kwargs.get("tol1bem", 0.3)
                    ),
                    Field(
                        "tol2bem",
                        float,
                        70,
                        10,
                        kwargs.get("tol2bem", 0.1)
                    ),
                ],
            ),
        ]

    @property
    def ncylfem(self) -> int:
        """Get or set the Number of electromagnetism cycles between the recalculation of FEM matrices.If a negative value is entered, then the absolute value will refer to a load curve giving NCYCLFEM function of time.
        """ # nopep8
        return self._cards[0].get_value("ncylfem")

    @ncylfem.setter
    def ncylfem(self, value: int) -> None:
        self._cards[0].set_value("ncylfem", value)

    @property
    def ncylbem(self) -> int:
        """Get or set the Number of electromagnetism cycles between the recalculation of BEM matrices.If a negative value is entered, then the absolute value will refer to a load curve giving NCYCLBEM function of time.
        """ # nopep8
        return self._cards[0].get_value("ncylbem")

    @ncylbem.setter
    def ncylbem(self, value: int) -> None:
        self._cards[0].set_value("ncylbem", value)

    @property
    def autofem(self) -> int:
        """Get or set the In addition to NCYLFEM, this triggers an automatic recomputation of the FEM matrices based on an error calculation of the conductors' relative deformation and electrical conductivity changes.
        EQ.0:Autorecomputation off.
        EQ.1:Autorecomputation on.
        """ # nopep8
        return self._cards[0].get_value("autofem")

    @autofem.setter
    def autofem(self, value: int) -> None:
        self._cards[0].set_value("autofem", value)

    @property
    def autobem(self) -> int:
        """Get or set the In addition to NCYLBEM, this triggers an automatic recomputation of the BEM matrices based on an error calculation of the conductors' relative displacements.
        EQ.0:Autorecomputation off.
        EQ.1:Autorecomputation on.
        """ # nopep8
        return self._cards[0].get_value("autobem")

    @autobem.setter
    def autobem(self, value: int) -> None:
        self._cards[0].set_value("autobem", value)

    @property
    def tol1fem(self) -> float:
        """Get or set the If a conducting element sees a deformation or a conductivity change that reaches an error higher than TOL1FEM, then the FEM matrices will be reassembled.If a negative value is entered, then the absolute value will refer to a load curve giving TOL1FEM function of time.
        """ # nopep8
        return self._cards[0].get_value("tol1fem")

    @tol1fem.setter
    def tol1fem(self, value: float) -> None:
        self._cards[0].set_value("tol1fem", value)

    @property
    def tol2fem(self) -> float:
        """Get or set the If TOL2FEM*Number-of-conducting-elements see a deformation or a conductivity change that reaches an error higher than TOL2FEM, then the FEM matrices will be recomputed.If a negative value is entered, then the absolute value will refer to a load curve giving TOL2FEM function of time.
        """ # nopep8
        return self._cards[0].get_value("tol2fem")

    @tol2fem.setter
    def tol2fem(self, value: float) -> None:
        self._cards[0].set_value("tol2fem", value)

    @property
    def tol1bem(self) -> float:
        """Get or set the If a conducting element sees a displacement that reaches an error higher than TOL1BEM, then the BEM matrices will be reassembled.If a negative value is entered, then the absolute value will refer to a load curve giving TOL1BEM function of time.
        """ # nopep8
        return self._cards[0].get_value("tol1bem")

    @tol1bem.setter
    def tol1bem(self, value: float) -> None:
        self._cards[0].set_value("tol1bem", value)

    @property
    def tol2bem(self) -> float:
        """Get or set the If TOL2BEM*Number-of-conducting-elements see a displacement that reaches an error higher than TOL2BEM, then the BEM matrices will be recomputed.If a negative value is entered, then the absolute value will refer to a load curve giving TOL2BEM function of time.
        """ # nopep8
        return self._cards[0].get_value("tol2bem")

    @tol2bem.setter
    def tol2bem(self, value: float) -> None:
        self._cards[0].set_value("tol2bem", value)

