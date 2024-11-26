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

class ControlMppDecompositionAdaptive(KeywordBase):
    """DYNA CONTROL_MPP_DECOMPOSITION_ADAPTIVE keyword"""

    keyword = "CONTROL"
    subkeyword = "MPP_DECOMPOSITION_ADAPTIVE"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "freq",
                        float,
                        0,
                        10,
                        kwargs.get("freq")
                    ),
                    Field(
                        "defgeo",
                        int,
                        10,
                        10,
                        kwargs.get("defgeo", 1)
                    ),
                    Field(
                        "cweight",
                        float,
                        20,
                        10,
                        kwargs.get("cweight", 1.0)
                    ),
                    Field(
                        "unused",
                        float,
                        30,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "stime",
                        float,
                        40,
                        10,
                        kwargs.get("stime", 0.0)
                    ),
                ],
            ),
        ]

    @property
    def freq(self) -> typing.Optional[float]:
        """Get or set the Determines the number of redecompositions during the solution.
        LT.0:	|FREQ| rounded to the nearest integer is the number of redecompositions during the solution.
        GT.0:	FREQ is the time interval between redecompositions.
        """ # nopep8
        return self._cards[0].get_value("freq")

    @freq.setter
    def freq(self, value: float) -> None:
        self._cards[0].set_value("freq", value)

    @property
    def defgeo(self) -> int:
        """Get or set the Geometry for decomposition:
        EQ.1:	Use current geometry for decomposition.When applied to a model containing SPH, deactivated SPH elements are not considered in the partition.This will give better load balancing if SPH elements are deleted during the simulation.
        EQ.2 : Use current geometry for decomposition(same as 1 if applied to a non - SPH model).When applied to a model containing SPH, all SPH elements are considered in the partition.This will give better load balancing if SPH elements are reactivated during the simulation.
        """ # nopep8
        return self._cards[0].get_value("defgeo")

    @defgeo.setter
    def defgeo(self, value: int) -> None:
        if value not in [1, 2]:
            raise Exception("""defgeo must be one of {1,2}""")
        self._cards[0].set_value("defgeo", value)

    @property
    def cweight(self) -> float:
        """Get or set the Element cost scale factor for an element in contact or an element that has undergone plastic strain.  If the element is under contact and has plastic strain, the weight will be doubled.  Since the element cost is measured from calculated quantities, the results will remain consistent between runs with the same input and decomposition
        """ # nopep8
        return self._cards[0].get_value("cweight")

    @cweight.setter
    def cweight(self, value: float) -> None:
        self._cards[0].set_value("cweight", value)

    @property
    def stime(self) -> float:
        """Get or set the Start time for redecomposition
        """ # nopep8
        return self._cards[0].get_value("stime")

    @stime.setter
    def stime(self, value: float) -> None:
        self._cards[0].set_value("stime", value)

