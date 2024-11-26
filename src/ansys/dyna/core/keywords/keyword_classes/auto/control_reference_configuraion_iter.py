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

class ControlReferenceConfiguraionIter(KeywordBase):
    """DYNA CONTROL_REFERENCE_CONFIGURAION_ITER keyword"""

    keyword = "CONTROL"
    subkeyword = "REFERENCE_CONFIGURAION_ITER"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "maxiter",
                        int,
                        0,
                        10,
                        kwargs.get("maxiter")
                    ),
                    Field(
                        "target",
                        str,
                        10,
                        70,
                        kwargs.get("target")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "method",
                        int,
                        0,
                        10,
                        kwargs.get("method")
                    ),
                    Field(
                        "step",
                        float,
                        10,
                        10,
                        kwargs.get("step", 1.0)
                    ),
                    Field(
                        "tol",
                        float,
                        20,
                        10,
                        kwargs.get("tol", 0.0)
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "iter",
                        str,
                        0,
                        80,
                        kwargs.get("iter")
                    ),
                ],
            ),
        ]

    @property
    def maxiter(self) -> typing.Optional[int]:
        """Get or set the The maximum number of iterations in the iterative method
        """ # nopep8
        return self._cards[0].get_value("maxiter")

    @maxiter.setter
    def maxiter(self, value: int) -> None:
        self._cards[0].set_value("maxiter", value)

    @property
    def target(self) -> typing.Optional[str]:
        """Get or set the File containing all nodes of the target geometry
        """ # nopep8
        return self._cards[0].get_value("target")

    @target.setter
    def target(self, value: str) -> None:
        self._cards[0].set_value("target", value)

    @property
    def method(self) -> typing.Optional[int]:
        """Get or set the Iterative method
        EQ.0 : Sellier’s method
        EQ.1 : Rausch’s method
        EQ.3:Rausch’s method with an additional line search.
        """ # nopep8
        return self._cards[1].get_value("method")

    @method.setter
    def method(self, value: int) -> None:
        self._cards[1].set_value("method", value)

    @property
    def step(self) -> float:
        """Get or set the The step size used in the iterations to update the current approximate reference geometry for Sellier’s method. It must be > 0.
        """ # nopep8
        return self._cards[1].get_value("step")

    @step.setter
    def step(self, value: float) -> None:
        self._cards[1].set_value("step", value)

    @property
    def tol(self) -> float:
        """Get or set the The tolerance used to determining convergence of the iterative method.This is given in the unit of length
        """ # nopep8
        return self._cards[1].get_value("tol")

    @tol.setter
    def tol(self, value: float) -> None:
        self._cards[1].set_value("tol", value)

    @property
    def iter(self) -> typing.Optional[str]:
        """Get or set the Base name of files containing initial guess of the approximate stress free reference geometry as well as algorithmic parameters. This is used to start, or restart, the iterative method. It is optional, but if ITER is not supplied, TARGET is used as an initial guess. The file name of the initial guess is ITER.guess and it has the same format as the TARGET file, while the file name for algorithmic data is ITER.algo and uses an internal format
        """ # nopep8
        return self._cards[2].get_value("iter")

    @iter.setter
    def iter(self, value: str) -> None:
        self._cards[2].set_value("iter", value)

