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

class EmControlEp(KeywordBase):
    """DYNA EM_CONTROL_EP keyword"""

    keyword = "EM"
    subkeyword = "CONTROL_EP"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "solvetype",
                        int,
                        0,
                        10,
                        kwargs.get("solvetype")
                    ),
                    Field(
                        "numspliti",
                        int,
                        10,
                        10,
                        kwargs.get("numspliti")
                    ),
                    Field(
                        "actusigma",
                        int,
                        20,
                        10,
                        kwargs.get("actusigma")
                    ),
                ],
            ),
        ]

    @property
    def solvetype(self) -> typing.Optional[int]:
        """Get or set the ElectroPhysiology solver sector: eq. 11: monodomain, eq. 12: bidomain, eq.13 mono+bidomain
        """ # nopep8
        return self._cards[0].get_value("solvetype")

    @solvetype.setter
    def solvetype(self, value: int) -> None:
        self._cards[0].set_value("solvetype", value)

    @property
    def numspliti(self) -> typing.Optional[int]:
        """Get or set the Split ratio between the ionic cell model time step and the monodomain time step. (we will do “numplit” cell model time steps for each diffusion time step)
        """ # nopep8
        return self._cards[0].get_value("numspliti")

    @numspliti.setter
    def numspliti(self, value: int) -> None:
        self._cards[0].set_value("numspliti", value)

    @property
    def actusigma(self) -> typing.Optional[int]:
        """Get or set the Time period at which the electrical conductivity is updated.
        """ # nopep8
        return self._cards[0].get_value("actusigma")

    @actusigma.setter
    def actusigma(self, value: int) -> None:
        self._cards[0].set_value("actusigma", value)

