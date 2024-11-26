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

class ControlSolution(KeywordBase):
    """DYNA CONTROL_SOLUTION keyword"""

    keyword = "CONTROL"
    subkeyword = "SOLUTION"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "soln",
                        int,
                        0,
                        10,
                        kwargs.get("soln", 0)
                    ),
                    Field(
                        "nlq",
                        int,
                        10,
                        10,
                        kwargs.get("nlq")
                    ),
                    Field(
                        "isnan",
                        int,
                        20,
                        10,
                        kwargs.get("isnan", 0)
                    ),
                    Field(
                        "lcint",
                        int,
                        30,
                        10,
                        kwargs.get("lcint", 100)
                    ),
                    Field(
                        "lcacc",
                        int,
                        40,
                        10,
                        kwargs.get("lcacc", 0)
                    ),
                    Field(
                        "ncdcf",
                        int,
                        50,
                        10,
                        kwargs.get("ncdcf", 1)
                    ),
                    Field(
                        "nocop",
                        int,
                        60,
                        10,
                        kwargs.get("nocop", 0)
                    ),
                ],
            ),
        ]

    @property
    def soln(self) -> int:
        """Get or set the Analysis solution procedure:
        EQ.0: Structural analysis only,
        EQ.1: Thermal analysis only,
        EQ.2: Coupled structural thermal analysis.
        """ # nopep8
        return self._cards[0].get_value("soln")

    @soln.setter
    def soln(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""soln must be one of {0,1,2}""")
        self._cards[0].set_value("soln", value)

    @property
    def nlq(self) -> typing.Optional[int]:
        """Get or set the Define the vector length used in solution.  This value must not exceed the vector length of the system which varies based on the machine manufacturer.  The default vector length is printed at termination in the MESSAG file.
        """ # nopep8
        return self._cards[0].get_value("nlq")

    @nlq.setter
    def nlq(self, value: int) -> None:
        self._cards[0].set_value("nlq", value)

    @property
    def isnan(self) -> int:
        """Get or set the Flag to check for a NaN in the force and moment arrays after the assembly of these arrays is completed.  This option can be useful for debugging purposes.  A cost overhead of approximately 2% is incurred when this option is active.
        EQ.0: No checking,
        EQ.1: Checking is active..
        """ # nopep8
        return self._cards[0].get_value("isnan")

    @isnan.setter
    def isnan(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""isnan must be one of {0,1}""")
        self._cards[0].set_value("isnan", value)

    @property
    def lcint(self) -> int:
        """Get or set the Number of equally spaced points used in curve (*DEFINE_CURVE) rediscretization. A minimum number of LCINT=100 is always used, i.e., only larger input values are possible. Curve rediscretization applies only to curves used in material models.  Curves defining loads, motion, etc. are not rediscretized.
        """ # nopep8
        return self._cards[0].get_value("lcint")

    @lcint.setter
    def lcint(self, value: int) -> None:
        self._cards[0].set_value("lcint", value)

    @property
    def lcacc(self) -> int:
        """Get or set the Flag to truncate curves to 6 significant figures for single precision and 13 significant figures for double precision. The truncation is done after applying the offset and scale factors specified in *DEFINE_CURVE.  Truncation is intended to prevent curve values from deviating from the input value, e.g., 0.7 being stored as 0.69999999.  This small deviation was seen to have an adverse effect in a particular analysis using *MAT_083.  In general, curve truncation is not necessary and is unlikely to have any effect on results.
        EQ.0:	No truncation.
        NE.0:	Truncate.
        """ # nopep8
        return self._cards[0].get_value("lcacc")

    @lcacc.setter
    def lcacc(self, value: int) -> None:
        self._cards[0].set_value("lcacc", value)

    @property
    def ncdcf(self) -> int:
        """Get or set the Global option to evaluate *DEFINE_CURVE_FUNCTION every NCDCF:th cycle..
        """ # nopep8
        return self._cards[0].get_value("ncdcf")

    @ncdcf.setter
    def ncdcf(self, value: int) -> None:
        self._cards[0].set_value("ncdcf", value)

    @property
    def nocop(self) -> int:
        """Get or set the Avoid copying of material history variables to temporary buffers for constitutive evaluations.
        EQ.0:	Not active
        EQ.1:	Active
        """ # nopep8
        return self._cards[0].get_value("nocop")

    @nocop.setter
    def nocop(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""nocop must be one of {0,1}""")
        self._cards[0].set_value("nocop", value)

