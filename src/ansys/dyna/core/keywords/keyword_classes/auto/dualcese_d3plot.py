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

class DualceseD3Plot(KeywordBase):
    """DYNA DUALCESE_D3PLOT keyword"""

    keyword = "DUALCESE"
    subkeyword = "D3PLOT"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "flow_var",
                        str,
                        0,
                        80,
                        kwargs.get("flow_var")
                    ),
                ],
            ),
        ]

    @property
    def flow_var(self) -> typing.Optional[str]:
        """Get or set the Name of a flow variable to output to the d3plot file. The currently supported variables are:
        DENSITY
        VELOCITY
        MOMENTUM
        VORTICITY
        TOTAL_ENERGY
        INTERNAL_ENERGY
        PRESSURE
        TEMPERATURE
        ENTROPY
        ENTHALPY
        SCHLIEREN_NUMBER
        VOID_FRACTION
        VOLUME_FRACTION
        REACTANT_MASS_FRACTION
        """ # nopep8
        return self._cards[0].get_value("flow_var")

    @flow_var.setter
    def flow_var(self, value: str) -> None:
        self._cards[0].set_value("flow_var", value)

