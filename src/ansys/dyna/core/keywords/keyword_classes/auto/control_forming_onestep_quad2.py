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

class ControlFormingOnestepQuad2(KeywordBase):
    """DYNA CONTROL_FORMING_ONESTEP_QUAD2 keyword"""

    keyword = "CONTROL"
    subkeyword = "FORMING_ONESTEP_QUAD2"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "option",
                        int,
                        0,
                        10,
                        kwargs.get("option", 6)
                    ),
                    Field(
                        "tsclmax",
                        float,
                        10,
                        10,
                        kwargs.get("tsclmax", 1.0)
                    ),
                    Field(
                        "autobd",
                        float,
                        20,
                        10,
                        kwargs.get("autobd", 0.3)
                    ),
                    Field(
                        "tsclmin",
                        float,
                        30,
                        10,
                        kwargs.get("tsclmin", 1.0)
                    ),
                    Field(
                        "epsmax",
                        float,
                        40,
                        10,
                        kwargs.get("epsmax", 1.0)
                    ),
                    Field(
                        "unused",
                        int,
                        50,
                        10,
                        kwargs.get("unused")
                    ),
                    Field(
                        "lcsdg",
                        int,
                        60,
                        10,
                        kwargs.get("lcsdg")
                    ),
                    Field(
                        "dmgexp",
                        float,
                        70,
                        10,
                        kwargs.get("dmgexp")
                    ),
                ],
            ),
            Card(
                [
                    Field(
                        "flatname",
                        str,
                        0,
                        256,
                        kwargs.get("flatname")
                    ),
                ],
            ),
        ]

    @property
    def option(self) -> int:
        """Get or set the One-step solution method: EQ.7: Invokes a one-step solution with blank unfolding that accounts for part undercut.
        """ # nopep8
        return self._cards[0].get_value("option")

    @option.setter
    def option(self, value: int) -> None:
        self._cards[0].set_value("option", value)

    @property
    def tsclmax(self) -> float:
        """Get or set the If not zero, it defines a thickness scale factor limiting the maximum thickness in the part.
        For example, if the maximum thickness allowed is 0.8mm for a blank with initial thickness of 0.75mm TSCLMAX can be set to 1.0667.  All thicknesses that are computed as more than 0.8mm in the sheet blank will be reset to 0.8mm.  The scale factor is useful in advance feasibility analysis where part design and stamping process have not been finalized and could potentially cause large splits or severe wrinkles during unfolding, rendering the forming results unusable for crash/safety simulation..
        """ # nopep8
        return self._cards[0].get_value("tsclmax")

    @tsclmax.setter
    def tsclmax(self, value: float) -> None:
        self._cards[0].set_value("tsclmax", value)

    @property
    def autobd(self) -> float:
        """Get or set the Apply a fraction of a fully locked bead force along the entire periphery of the blank.  The fully locked bead force is automatically calculated based on a material hardening curve input.  AUTOBD can be increased to easily introduce more thinning and effective plastic strain in the part.
        LT.0.0:	Turns off the “auto-bead” feature.
        EQ.0.0:	Automatically applies 30% of fully locked force.
        GT.0.0:	Fraction input will be used to scale the fully locked force.
        """ # nopep8
        return self._cards[0].get_value("autobd")

    @autobd.setter
    def autobd(self, value: float) -> None:
        self._cards[0].set_value("autobd", value)

    @property
    def tsclmin(self) -> float:
        """Get or set the If not zero, it defines a thickness scale factor limiting the maximum thickness reduction.
        For example, if the minimum thickness allowed is 0.6mm for a blank with initial thickness of 0.75mm TSCLMIN can be set to 0.8.  All thicknesses that are computed as less than 0.6mm in the sheet blank will be reset to 0.6mm.  The scale factor is useful in advance feasibility analysis where part design and stamping process have not been finalized and could potentially cause large splits or severe wrinkles during unfolding, rendering the forming results unusable for crash/safety simulation.
        """ # nopep8
        return self._cards[0].get_value("tsclmin")

    @tsclmin.setter
    def tsclmin(self, value: float) -> None:
        self._cards[0].set_value("tsclmin", value)

    @property
    def epsmax(self) -> float:
        """Get or set the If not zero, it defines the maximum effective plastic strain allowed. All computed effective plastic strains that are greater than this value in the blank will be set to this value.
        """ # nopep8
        return self._cards[0].get_value("epsmax")

    @epsmax.setter
    def epsmax(self, value: float) -> None:
        self._cards[0].set_value("epsmax", value)

    @property
    def lcsdg(self) -> typing.Optional[int]:
        """Get or set the Load curve ID defining equivalent plastic strain to failure vs. stress triaxiality, see *MAT_ADD_EROSION.
        """ # nopep8
        return self._cards[0].get_value("lcsdg")

    @lcsdg.setter
    def lcsdg(self, value: int) -> None:
        self._cards[0].set_value("lcsdg", value)

    @property
    def dmgexp(self) -> typing.Optional[float]:
        """Get or set the Exponent for nonlinear damage accumulation, see *MAT_ADD_EROSION.  Damage accumulation is written as history variable #6 in the file onestepresult.
        """ # nopep8
        return self._cards[0].get_value("dmgexp")

    @dmgexp.setter
    def dmgexp(self, value: float) -> None:
        self._cards[0].set_value("dmgexp", value)

    @property
    def flatname(self) -> typing.Optional[str]:
        """Get or set the File name of the initial unfolded blank by LS-PrePost (see remarks). This is needed only for the OPTION=6.  Leave a blank line for OPTION=7.
        """ # nopep8
        return self._cards[1].get_value("flatname")

    @flatname.setter
    def flatname(self, value: str) -> None:
        self._cards[1].set_value("flatname", value)

