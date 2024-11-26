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

class ControlBulkViscosity(KeywordBase):
    """DYNA CONTROL_BULK_VISCOSITY keyword"""

    keyword = "CONTROL"
    subkeyword = "BULK_VISCOSITY"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._cards = [
            Card(
                [
                    Field(
                        "q1",
                        float,
                        0,
                        10,
                        kwargs.get("q1", 1.5)
                    ),
                    Field(
                        "q2",
                        float,
                        10,
                        10,
                        kwargs.get("q2", 0.06)
                    ),
                    Field(
                        "type",
                        int,
                        20,
                        10,
                        kwargs.get("type", 1)
                    ),
                    Field(
                        "btype",
                        int,
                        30,
                        10,
                        kwargs.get("btype", 0)
                    ),
                    Field(
                        "tstype",
                        int,
                        40,
                        10,
                        kwargs.get("tstype", 0)
                    ),
                ],
            ),
        ]

    @property
    def q1(self) -> float:
        """Get or set the Default quadratic viscosity coefficient (default = 1.5).
        """ # nopep8
        return self._cards[0].get_value("q1")

    @q1.setter
    def q1(self, value: float) -> None:
        self._cards[0].set_value("q1", value)

    @property
    def q2(self) -> float:
        """Get or set the Default linear viscosity coefficient (default = 0.06).
        """ # nopep8
        return self._cards[0].get_value("q2")

    @q2.setter
    def q2(self, value: float) -> None:
        self._cards[0].set_value("q2", value)

    @property
    def type(self) -> int:
        """Get or set the Default bulk viscosity type, IBQ (default = 1):
        EQ.-2: same as - 1 but the internal energy dissipated by the viscosity in the shell elements is computed and included in the overall energy balance.
        EQ.-1: same as 1 but also includes viscosity in shell formulations 2, 4, 10, 16,and 17.  The internal energy is not computed in the shell elements.
        EQ.1: standard bulk viscosity.Solid elements only and internal energy is always computed and included in the overall energy balance.
        EQ.2: Richards - Wilkins bulk viscosity.Two - dimensional plane strain and axisymmetric solid elements only.
        Internal energy is always computed and included in the overall energy balance.
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        if value not in [1, -1, -2, 2]:
            raise Exception("""type must be one of {1,-1,-2,2}""")
        self._cards[0].set_value("type", value)

    @property
    def btype(self) -> int:
        """Get or set the Beam bulk viscosity type (Default=0)
        EQ. 0: The bulk viscosity is turned off for beams.
        EQ. 1: The bulk viscosity is turned on for beam types 1 and 11. The energy contribution is not included in the overall energy balance.
        EQ. 2: The bulk viscosity is turned on for beam type 1 and 11.  The energy contribution is included in the overall energy balance.
        """ # nopep8
        return self._cards[0].get_value("btype")

    @btype.setter
    def btype(self, value: int) -> None:
        if value not in [0, 1, 2]:
            raise Exception("""btype must be one of {0,1,2}""")
        self._cards[0].set_value("btype", value)

    @property
    def tstype(self) -> int:
        """Get or set the Beam bulk viscosity for thick shells (default = 0):
        EQ.0:	The bulk viscosity is turned off for thick shells.
        EQ.1:	The bulk viscosity is turned on for thick shells forms 5, 6 and 7
        """ # nopep8
        return self._cards[0].get_value("tstype")

    @tstype.setter
    def tstype(self, value: int) -> None:
        if value not in [0, 1]:
            raise Exception("""tstype must be one of {0,1}""")
        self._cards[0].set_value("tstype", value)

