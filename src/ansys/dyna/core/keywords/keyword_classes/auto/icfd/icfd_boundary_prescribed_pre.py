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

"""Module providing the IcfdBoundaryPrescribedPre class."""
import typing
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.keyword_base import KeywordBase

class IcfdBoundaryPrescribedPre(KeywordBase):
    """DYNA ICFD_BOUNDARY_PRESCRIBED_PRE keyword"""

    keyword = "ICFD"
    subkeyword = "BOUNDARY_PRESCRIBED_PRE"

    def __init__(self, **kwargs):
        """Initialize the IcfdBoundaryPrescribedPre class."""
        super().__init__(**kwargs)
        self._cards = [
            TableCard(
                [
                    Field("pid", int, 0, 10, None),
                    Field("lcid", int, 10, 10, None),
                    Field("sf", float, 20, 10, 1.0),
                    Field("death", float, 30, 10, 1e+28),
                    Field("birth", float, 40, 10, 0.0),
                    Field("iso", int, 50, 10, 0),
                ],
                None,
                name="boundaries",
                **kwargs,
            ),
        ]
    @property
    def boundaries(self) -> pd.DataFrame:
        """Get the table of boundaries."""
        return self._cards[0].table

    @boundaries.setter
    def boundaries(self, df: pd.DataFrame):
        """Set boundaries from the dataframe df"""
        self._cards[0].table = df

    @property
    def pid(self) -> pd.Series:
        """Get or set the PID for a fluid surface.
        """ # nopep8
        return self._cards[0].get_column("pid")

    @pid.setter
    def pid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the pid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("pid", value)

    @property
    def lcid(self) -> pd.Series:
        """Get or set the Load curve ID to describe the pressure value as a function of time. See *DEFINE_CURVE, *DEFINE_CURVE_FUNCTION, or *DEFINE_FUNCTION. If using a *DEFINE_FUNCTION, the following parameters are allowed: f(x, y, z, vx, vy, vz, temp, pres, time).
        """ # nopep8
        return self._cards[0].get_column("lcid")

    @lcid.setter
    def lcid(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the lcid column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("lcid", value)

    @property
    def sf(self) -> pd.Series:
        """Get or set the Load curve scale factor.  The default is =1.0.
        """ # nopep8
        return self._cards[0].get_column("sf")

    @sf.setter
    def sf(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the sf column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("sf", value)

    @property
    def death(self) -> pd.Series:
        """Get or set the Time imposed motion/constraint is removed. EQ.0.0: Default set to 10E28
        """ # nopep8
        return self._cards[0].get_column("death")

    @death.setter
    def death(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the death column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("death", value)

    @property
    def birth(self) -> pd.Series:
        """Get or set the Time imposed motion/constraint is activated starting from the initial abscissa value of the curve.
        """ # nopep8
        return self._cards[0].get_column("birth")

    @birth.setter
    def birth(self, value: typing.Union[float, typing.Sequence[float], pd.Series]) -> None:
        """Set the birth column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("birth", value)

    @property
    def iso(self) -> pd.Series:
        """Get or set the Adds an additional isovelocity condition to the system on top of the user defined imposed pressure (See Remark 1):
        EQ.0: Off
        EQ.1: On
        """ # nopep8
        return self._cards[0].get_column("iso")

    @iso.setter
    def iso(self, value: typing.Union[int, typing.Sequence[int], pd.Series]) -> None:
        """Set the iso column from a scalar, a Series, or an array-like."""
        self._cards[0].set_column("iso", value)

