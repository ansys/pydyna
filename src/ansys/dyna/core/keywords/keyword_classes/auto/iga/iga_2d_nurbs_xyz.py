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

"""Module providing the Iga2DNurbsXyz class."""
import typing
import math
import pandas as pd

from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.table_card import TableCard
from ansys.dyna.core.lib.keyword_base import KeywordBase

_IGA2DNURBSXYZ_CARD0 = (
    FieldSchema("nid", int, 0, 10, None),
    FieldSchema("nr", int, 10, 10, None),
    FieldSchema("ns", int, 20, 10, None),
    FieldSchema("pr", int, 30, 10, None),
    FieldSchema("ps", int, 40, 10, None),
)

_IGA2DNURBSXYZ_CARD1 = (
    FieldSchema("unir", int, 0, 10, 0),
    FieldSchema("unis", int, 10, 10, 0),
)

_IGA2DNURBSXYZ_CARD3 = (
    FieldSchema("rfirst", float, 0, 20, None),
    FieldSchema("rlast", float, 20, 20, None),
)

_IGA2DNURBSXYZ_CARD5 = (
    FieldSchema("sfirst", float, 0, 20, None),
    FieldSchema("slast", float, 20, 20, None),
)

class Iga2DNurbsXyz(KeywordBase):
    """DYNA IGA_2D_NURBS_XYZ keyword"""

    keyword = "IGA"
    subkeyword = "2D_NURBS_XYZ"

    def __init__(self, **kwargs):
        """Initialize the Iga2DNurbsXyz class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _IGA2DNURBSXYZ_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _IGA2DNURBSXYZ_CARD1,
                **kwargs,
            ),
            TableCard(
                [
                    Field("r1", float, 0, 20, None),
                    Field("r2", float, 20, 20, None),
                    Field("r3", float, 40, 20, None),
                    Field("r4", float, 60, 20, None),
                ],
                lambda: math.ceil((self.nr+self.pr+1)/4),
                lambda: self.unir==0,
                name="r_knots",
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _IGA2DNURBSXYZ_CARD3,
                active_func=lambda: self.unir!=0,
                **kwargs,
            ),
            TableCard(
                [
                    Field("s1", float, 0, 20, None),
                    Field("s2", float, 20, 20, None),
                    Field("s3", float, 40, 20, None),
                    Field("s4", float, 60, 20, None),
                ],
                lambda: math.ceil((self.ns+self.ps+1)/4),
                lambda: self.unis==0,
                name="s_knots",
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _IGA2DNURBSXYZ_CARD5,
                active_func=lambda: self.unis!=0,
                **kwargs,
            ),
            TableCard(
                [
                    Field("x", float, 0, 20, None),
                    Field("y", float, 20, 20, None),
                    Field("z", float, 40, 20, None),
                    Field("wgt", float, 60, 20, 1.0),
                ],
                lambda: self.nr*self.ns,
                name="points",
                **kwargs,
            ),
        ]
    @property
    def nid(self) -> typing.Optional[int]:
        """Get or set the Physical bivariate NURBS ID. A unique number must be chosen.
        """ # nopep8
        return self._cards[0].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[0].set_value("nid", value)

    @property
    def nr(self) -> typing.Optional[int]:
        """Get or set the Number of control points in the local r-direction.
        """ # nopep8
        return self._cards[0].get_value("nr")

    @nr.setter
    def nr(self, value: int) -> None:
        """Set the nr property."""
        self._cards[0].set_value("nr", value)

    @property
    def ns(self) -> typing.Optional[int]:
        """Get or set the Number of control points in the local s-direction.
        """ # nopep8
        return self._cards[0].get_value("ns")

    @ns.setter
    def ns(self, value: int) -> None:
        """Set the ns property."""
        self._cards[0].set_value("ns", value)

    @property
    def pr(self) -> typing.Optional[int]:
        """Get or set the Polynomial degree of the basis in the local r-direction.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: int) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def ps(self) -> typing.Optional[int]:
        """Get or set the Polynomial degree of the basis in the local s-direction.
        """ # nopep8
        return self._cards[0].get_value("ps")

    @ps.setter
    def ps(self, value: int) -> None:
        """Set the ps property."""
        self._cards[0].set_value("ps", value)

    @property
    def unir(self) -> int:
        """Get or set the Knot vector type in the local r-direction.
        EQ.0: Specify the entire knot vector in the local r - direction.
        EQ.1 : Uniform open knot vector in the local r - direction.
        EQ.2 : Uniform periodic knot vector in the local r - direction.
        """ # nopep8
        return self._cards[1].get_value("unir")

    @unir.setter
    def unir(self, value: int) -> None:
        """Set the unir property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""unir must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("unir", value)

    @property
    def unis(self) -> int:
        """Get or set the Knot vector type in the local s-direction.
        EQ.0: Specify the entire knot vector in the local s - direction.
        EQ.1 : Uniform open knot vector in the local s - direction.
        EQ.2 : Uniform periodic knot vector in the local s - direction.
        """ # nopep8
        return self._cards[1].get_value("unis")

    @unis.setter
    def unis(self, value: int) -> None:
        """Set the unis property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""unis must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("unis", value)

    @property
    def r_knots(self) -> pd.DataFrame:
        """Get the table of r_knots."""
        return self._cards[2].table

    @r_knots.setter
    def r_knots(self, df: pd.DataFrame):
        """Set r_knots from the dataframe df"""
        self._cards[2].table = df

    @property
    def rfirst(self) -> typing.Optional[float]:
        """Get or set the First knot value in the local r-direction.
        """ # nopep8
        return self._cards[3].get_value("rfirst")

    @rfirst.setter
    def rfirst(self, value: float) -> None:
        """Set the rfirst property."""
        self._cards[3].set_value("rfirst", value)

    @property
    def rlast(self) -> typing.Optional[float]:
        """Get or set the Last knot value in the local r-direction.
        """ # nopep8
        return self._cards[3].get_value("rlast")

    @rlast.setter
    def rlast(self, value: float) -> None:
        """Set the rlast property."""
        self._cards[3].set_value("rlast", value)

    @property
    def s_knots(self) -> pd.DataFrame:
        """Get the table of s_knots."""
        return self._cards[4].table

    @s_knots.setter
    def s_knots(self, df: pd.DataFrame):
        """Set s_knots from the dataframe df"""
        self._cards[4].table = df

    @property
    def sfirst(self) -> typing.Optional[float]:
        """Get or set the First knot value in the local s-direction.
        """ # nopep8
        return self._cards[5].get_value("sfirst")

    @sfirst.setter
    def sfirst(self, value: float) -> None:
        """Set the sfirst property."""
        self._cards[5].set_value("sfirst", value)

    @property
    def slast(self) -> typing.Optional[float]:
        """Get or set the Last knot value in the local s-direction.
        """ # nopep8
        return self._cards[5].get_value("slast")

    @slast.setter
    def slast(self, value: float) -> None:
        """Set the slast property."""
        self._cards[5].set_value("slast", value)

    @property
    def points(self) -> pd.DataFrame:
        """Get the table of points."""
        return self._cards[6].table

    @points.setter
    def points(self, df: pd.DataFrame):
        """Set points from the dataframe df"""
        self._cards[6].table = df

