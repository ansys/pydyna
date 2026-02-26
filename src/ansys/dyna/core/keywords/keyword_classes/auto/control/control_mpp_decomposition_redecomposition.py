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

"""Module providing the ControlMppDecompositionRedecomposition class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLMPPDECOMPOSITIONREDECOMPOSITION_CARD0 = (
    FieldSchema("freq", float, 0, 10, None),
    FieldSchema("defgeo", int, 10, 10, 1),
    FieldSchema("weight", float, 20, 10, 1.0),
    FieldSchema("remsph", int, 30, 10, 0),
    FieldSchema("stime", float, 40, 10, 0.0),
    FieldSchema("sampt_", float, 50, 10, None, "sampt "),
)

class ControlMppDecompositionRedecomposition(KeywordBase):
    """DYNA CONTROL_MPP_DECOMPOSITION_REDECOMPOSITION keyword"""

    keyword = "CONTROL"
    subkeyword = "MPP_DECOMPOSITION_REDECOMPOSITION"

    def __init__(self, **kwargs):
        """Initialize the ControlMppDecompositionRedecomposition class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLMPPDECOMPOSITIONREDECOMPOSITION_CARD0,
                **kwargs,
            ),        ]
    @property
    def freq(self) -> typing.Optional[float]:
        """Get or set the Determines the number of redecompositions during the solution.
        LT.0:	|FREQ| rounded to the nearest integer is the number of redecompositions during the solution.
        GT.0:	FREQ is the time interval between redecompositions.
        """ # nopep8
        return self._cards[0].get_value("freq")

    @freq.setter
    def freq(self, value: float) -> None:
        """Set the freq property."""
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
        """Set the defgeo property."""
        if value not in [1, 2, None]:
            raise Exception("""defgeo must be `None` or one of {1,2}.""")
        self._cards[0].set_value("defgeo", value)

    @property
    def weight(self) -> float:
        """Get or set the Element cost scale factor for element in contact or with plastic strain.
        If the element is under contact and has plastic strain, the weight will be doubled.
        Since the element cost is measured from calculated quantities, the results will remain consistent between runs with the same input and decomposition unlike using SAMPT option
        """ # nopep8
        return self._cards[0].get_value("weight")

    @weight.setter
    def weight(self, value: float) -> None:
        """Set the weight property."""
        self._cards[0].set_value("weight", value)

    @property
    def remsph(self) -> int:
        """Get or set the Flag to remove deactived SPH particles:
        EQ 0. Keep deactivated particles
        EQ 1. Remove deactivated particles
        """ # nopep8
        return self._cards[0].get_value("remsph")

    @remsph.setter
    def remsph(self, value: int) -> None:
        """Set the remsph property."""
        if value not in [0, 1, None]:
            raise Exception("""remsph must be `None` or one of {0,1}.""")
        self._cards[0].set_value("remsph", value)

    @property
    def stime(self) -> float:
        """Get or set the Start time for redecomposition
        """ # nopep8
        return self._cards[0].get_value("stime")

    @stime.setter
    def stime(self, value: float) -> None:
        """Set the stime property."""
        self._cards[0].set_value("stime", value)

    @property
    def sampt_(self) -> typing.Optional[float]:
        """Get or set the Time interval for collecting element cost profile to use in the next REDECOMP step.GT.0:	Sampling from beginning of each redecomposition for length SAMPT(t to t + SAMPT).If SAMPT ≥ FREQ, then the sampling will occur for the entire time interval, FREQ.LT.0 : Sampling from before ending of each redecomposition through to the next redecomposition(t + FREQ - SAMPT to t + FREQ
        """ # nopep8
        return self._cards[0].get_value("sampt_")

    @sampt_.setter
    def sampt_(self, value: float) -> None:
        """Set the sampt_ property."""
        self._cards[0].set_value("sampt_", value)

