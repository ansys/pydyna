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

"""Module providing the ElementSolidNurbsPatch class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ELEMENTSOLIDNURBSPATCH_CARD0 = (
    FieldSchema("npeid", int, 0, 10, None),
    FieldSchema("pid", int, 10, 10, None),
    FieldSchema("npr", int, 20, 10, None),
    FieldSchema("pr", int, 30, 10, None),
    FieldSchema("nps", int, 40, 10, None),
    FieldSchema("ps", int, 50, 10, None),
    FieldSchema("npt", int, 60, 10, None),
    FieldSchema("pt", int, 70, 10, None),
)

_ELEMENTSOLIDNURBSPATCH_CARD1 = (
    FieldSchema("wfl", int, 0, 10, 0),
    FieldSchema("nisr", int, 10, 10, None),
    FieldSchema("niss", int, 20, 10, None),
    FieldSchema("nist", int, 30, 10, None),
    FieldSchema("imass", int, 40, 10, 0),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("unused", int, 60, 10, None),
    FieldSchema("idfne", int, 70, 10, 0),
)

_ELEMENTSOLIDNURBSPATCH_CARD2 = (
    FieldSchema("rk1", float, 0, 10, None),
    FieldSchema("rk2", float, 10, 10, None),
    FieldSchema("rk3", float, 20, 10, None),
    FieldSchema("rk4", float, 30, 10, None),
    FieldSchema("rk5", float, 40, 10, None),
    FieldSchema("rk6", float, 50, 10, None),
    FieldSchema("rk7", float, 60, 10, None),
    FieldSchema("rk8", float, 70, 10, None),
)

_ELEMENTSOLIDNURBSPATCH_CARD3 = (
    FieldSchema("sk1", float, 0, 10, None),
    FieldSchema("sk2", float, 10, 10, None),
    FieldSchema("sk3", float, 20, 10, None),
    FieldSchema("sk4", float, 30, 10, None),
    FieldSchema("sk5", float, 40, 10, None),
    FieldSchema("sk6", float, 50, 10, None),
    FieldSchema("sk7", float, 60, 10, None),
    FieldSchema("sk8", float, 70, 10, None),
)

_ELEMENTSOLIDNURBSPATCH_CARD4 = (
    FieldSchema("tk1", float, 0, 10, None),
    FieldSchema("tk2", float, 10, 10, None),
    FieldSchema("tk3", float, 20, 10, None),
    FieldSchema("tk4", float, 30, 10, None),
    FieldSchema("tk5", float, 40, 10, None),
    FieldSchema("tk6", float, 50, 10, None),
    FieldSchema("tk7", float, 60, 10, None),
    FieldSchema("tk8", float, 70, 10, None),
)

_ELEMENTSOLIDNURBSPATCH_CARD5 = (
    FieldSchema("n1", int, 0, 10, None),
    FieldSchema("n2", int, 10, 10, None),
    FieldSchema("n3", int, 20, 10, None),
    FieldSchema("n4", int, 30, 10, None),
    FieldSchema("n5", int, 40, 10, None),
    FieldSchema("n6", int, 50, 10, None),
    FieldSchema("n7", int, 60, 10, None),
    FieldSchema("n8", int, 70, 10, None),
)

_ELEMENTSOLIDNURBSPATCH_CARD6 = (
    FieldSchema("w1", float, 0, 10, None),
    FieldSchema("w2", float, 10, 10, None),
    FieldSchema("w3", float, 20, 10, None),
    FieldSchema("w4", float, 30, 10, None),
    FieldSchema("w5", float, 40, 10, None),
    FieldSchema("w6", float, 50, 10, None),
    FieldSchema("w7", float, 60, 10, None),
    FieldSchema("w8", float, 70, 10, None),
)

class ElementSolidNurbsPatch(KeywordBase):
    """DYNA ELEMENT_SOLID_NURBS_PATCH keyword"""

    keyword = "ELEMENT"
    subkeyword = "SOLID_NURBS_PATCH"

    def __init__(self, **kwargs):
        """Initialize the ElementSolidNurbsPatch class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ELEMENTSOLIDNURBSPATCH_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSOLIDNURBSPATCH_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSOLIDNURBSPATCH_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSOLIDNURBSPATCH_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSOLIDNURBSPATCH_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSOLIDNURBSPATCH_CARD5,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSOLIDNURBSPATCH_CARD6,
                **kwargs,
            ),        ]
    @property
    def npeid(self) -> typing.Optional[int]:
        """Get or set the Nurbs-Patch Element ID.  A unique number has to be chosen.
        """ # nopep8
        return self._cards[0].get_value("npeid")

    @npeid.setter
    def npeid(self, value: int) -> None:
        """Set the npeid property."""
        self._cards[0].set_value("npeid", value)

    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID, see *PART.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def npr(self) -> typing.Optional[int]:
        """Get or set the Number of control points in local r-direction.
        """ # nopep8
        return self._cards[0].get_value("npr")

    @npr.setter
    def npr(self, value: int) -> None:
        """Set the npr property."""
        self._cards[0].set_value("npr", value)

    @property
    def pr(self) -> typing.Optional[int]:
        """Get or set the Order of polynomial of univariate nurbs basis functions in local r-direction.
        """ # nopep8
        return self._cards[0].get_value("pr")

    @pr.setter
    def pr(self, value: int) -> None:
        """Set the pr property."""
        self._cards[0].set_value("pr", value)

    @property
    def nps(self) -> typing.Optional[int]:
        """Get or set the Number of control points in local s-direction.
        """ # nopep8
        return self._cards[0].get_value("nps")

    @nps.setter
    def nps(self, value: int) -> None:
        """Set the nps property."""
        self._cards[0].set_value("nps", value)

    @property
    def ps(self) -> typing.Optional[int]:
        """Get or set the Order of polynomial of univariate nurbs basis functions in local s-direction.
        """ # nopep8
        return self._cards[0].get_value("ps")

    @ps.setter
    def ps(self, value: int) -> None:
        """Set the ps property."""
        self._cards[0].set_value("ps", value)

    @property
    def npt(self) -> typing.Optional[int]:
        """Get or set the Number of control points in local t-direction.
        """ # nopep8
        return self._cards[0].get_value("npt")

    @npt.setter
    def npt(self, value: int) -> None:
        """Set the npt property."""
        self._cards[0].set_value("npt", value)

    @property
    def pt(self) -> typing.Optional[int]:
        """Get or set the Order of polynomial of univariate nurbs basis functions in local t-direction.
        """ # nopep8
        return self._cards[0].get_value("pt")

    @pt.setter
    def pt(self, value: int) -> None:
        """Set the pt property."""
        self._cards[0].set_value("pt", value)

    @property
    def wfl(self) -> int:
        """Get or set the Flag for weighting factors of the control points
        EQ.0: all weights at the control points are set to 1.0 (B-spline basis) and no optional cards e are allowed.
        NE.0: the weights at the control points are defined in optional cards E which must be defined after cards D.
        """ # nopep8
        return self._cards[1].get_value("wfl")

    @wfl.setter
    def wfl(self, value: int) -> None:
        """Set the wfl property."""
        self._cards[1].set_value("wfl", value)

    @property
    def nisr(self) -> typing.Optional[int]:
        """Get or set the Number of (automatically created) Interpolation Shell elements in local r-direction per created Nurbs-element for visualization (postprocessing) and contact.
        """ # nopep8
        return self._cards[1].get_value("nisr")

    @nisr.setter
    def nisr(self, value: int) -> None:
        """Set the nisr property."""
        self._cards[1].set_value("nisr", value)

    @property
    def niss(self) -> typing.Optional[int]:
        """Get or set the Number of (automatically created) Interpolation Shell elements in local s-direction per created Nurbs-element for visualization (postprocessing) and contact.
        """ # nopep8
        return self._cards[1].get_value("niss")

    @niss.setter
    def niss(self, value: int) -> None:
        """Set the niss property."""
        self._cards[1].set_value("niss", value)

    @property
    def nist(self) -> typing.Optional[int]:
        """Get or set the Number of (automatically created) Interpolation Shell elements in local t-direction per created Nurbs-element for visualization (postprocessing) and contact.
        """ # nopep8
        return self._cards[1].get_value("nist")

    @nist.setter
    def nist(self, value: int) -> None:
        """Set the nist property."""
        self._cards[1].set_value("nist", value)

    @property
    def imass(self) -> int:
        """Get or set the Option for lumping of mass matrix:
        EQ.0: row sum.
        EQ.1: diagonal weighting.
        """ # nopep8
        return self._cards[1].get_value("imass")

    @imass.setter
    def imass(self, value: int) -> None:
        """Set the imass property."""
        self._cards[1].set_value("imass", value)

    @property
    def idfne(self) -> int:
        """Get or set the Element ID of first NURBS-Element within this NURBS-Patch definition.
        """ # nopep8
        return self._cards[1].get_value("idfne")

    @idfne.setter
    def idfne(self, value: int) -> None:
        """Set the idfne property."""
        self._cards[1].set_value("idfne", value)

    @property
    def rk1(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in r-direction defined
        """ # nopep8
        return self._cards[2].get_value("rk1")

    @rk1.setter
    def rk1(self, value: float) -> None:
        """Set the rk1 property."""
        self._cards[2].set_value("rk1", value)

    @property
    def rk2(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in r-direction defined
        """ # nopep8
        return self._cards[2].get_value("rk2")

    @rk2.setter
    def rk2(self, value: float) -> None:
        """Set the rk2 property."""
        self._cards[2].set_value("rk2", value)

    @property
    def rk3(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in r-direction defined
        """ # nopep8
        return self._cards[2].get_value("rk3")

    @rk3.setter
    def rk3(self, value: float) -> None:
        """Set the rk3 property."""
        self._cards[2].set_value("rk3", value)

    @property
    def rk4(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in r-direction defined
        """ # nopep8
        return self._cards[2].get_value("rk4")

    @rk4.setter
    def rk4(self, value: float) -> None:
        """Set the rk4 property."""
        self._cards[2].set_value("rk4", value)

    @property
    def rk5(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in r-direction defined
        """ # nopep8
        return self._cards[2].get_value("rk5")

    @rk5.setter
    def rk5(self, value: float) -> None:
        """Set the rk5 property."""
        self._cards[2].set_value("rk5", value)

    @property
    def rk6(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in r-direction defined
        """ # nopep8
        return self._cards[2].get_value("rk6")

    @rk6.setter
    def rk6(self, value: float) -> None:
        """Set the rk6 property."""
        self._cards[2].set_value("rk6", value)

    @property
    def rk7(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in r-direction defined
        """ # nopep8
        return self._cards[2].get_value("rk7")

    @rk7.setter
    def rk7(self, value: float) -> None:
        """Set the rk7 property."""
        self._cards[2].set_value("rk7", value)

    @property
    def rk8(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in r-direction defined
        """ # nopep8
        return self._cards[2].get_value("rk8")

    @rk8.setter
    def rk8(self, value: float) -> None:
        """Set the rk8 property."""
        self._cards[2].set_value("rk8", value)

    @property
    def sk1(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in s-direction defined
        """ # nopep8
        return self._cards[3].get_value("sk1")

    @sk1.setter
    def sk1(self, value: float) -> None:
        """Set the sk1 property."""
        self._cards[3].set_value("sk1", value)

    @property
    def sk2(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in s-direction defined
        """ # nopep8
        return self._cards[3].get_value("sk2")

    @sk2.setter
    def sk2(self, value: float) -> None:
        """Set the sk2 property."""
        self._cards[3].set_value("sk2", value)

    @property
    def sk3(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in s-direction defined
        """ # nopep8
        return self._cards[3].get_value("sk3")

    @sk3.setter
    def sk3(self, value: float) -> None:
        """Set the sk3 property."""
        self._cards[3].set_value("sk3", value)

    @property
    def sk4(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in s-direction defined
        """ # nopep8
        return self._cards[3].get_value("sk4")

    @sk4.setter
    def sk4(self, value: float) -> None:
        """Set the sk4 property."""
        self._cards[3].set_value("sk4", value)

    @property
    def sk5(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in s-direction defined
        """ # nopep8
        return self._cards[3].get_value("sk5")

    @sk5.setter
    def sk5(self, value: float) -> None:
        """Set the sk5 property."""
        self._cards[3].set_value("sk5", value)

    @property
    def sk6(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in s-direction defined
        """ # nopep8
        return self._cards[3].get_value("sk6")

    @sk6.setter
    def sk6(self, value: float) -> None:
        """Set the sk6 property."""
        self._cards[3].set_value("sk6", value)

    @property
    def sk7(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in s-direction defined
        """ # nopep8
        return self._cards[3].get_value("sk7")

    @sk7.setter
    def sk7(self, value: float) -> None:
        """Set the sk7 property."""
        self._cards[3].set_value("sk7", value)

    @property
    def sk8(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in s-direction defined
        """ # nopep8
        return self._cards[3].get_value("sk8")

    @sk8.setter
    def sk8(self, value: float) -> None:
        """Set the sk8 property."""
        self._cards[3].set_value("sk8", value)

    @property
    def tk1(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in t-direction defined
        """ # nopep8
        return self._cards[4].get_value("tk1")

    @tk1.setter
    def tk1(self, value: float) -> None:
        """Set the tk1 property."""
        self._cards[4].set_value("tk1", value)

    @property
    def tk2(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in t-direction defined
        """ # nopep8
        return self._cards[4].get_value("tk2")

    @tk2.setter
    def tk2(self, value: float) -> None:
        """Set the tk2 property."""
        self._cards[4].set_value("tk2", value)

    @property
    def tk3(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in t-direction defined
        """ # nopep8
        return self._cards[4].get_value("tk3")

    @tk3.setter
    def tk3(self, value: float) -> None:
        """Set the tk3 property."""
        self._cards[4].set_value("tk3", value)

    @property
    def tk4(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in t-direction defined
        """ # nopep8
        return self._cards[4].get_value("tk4")

    @tk4.setter
    def tk4(self, value: float) -> None:
        """Set the tk4 property."""
        self._cards[4].set_value("tk4", value)

    @property
    def tk5(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in t-direction defined
        """ # nopep8
        return self._cards[4].get_value("tk5")

    @tk5.setter
    def tk5(self, value: float) -> None:
        """Set the tk5 property."""
        self._cards[4].set_value("tk5", value)

    @property
    def tk6(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in t-direction defined
        """ # nopep8
        return self._cards[4].get_value("tk6")

    @tk6.setter
    def tk6(self, value: float) -> None:
        """Set the tk6 property."""
        self._cards[4].set_value("tk6", value)

    @property
    def tk7(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in t-direction defined
        """ # nopep8
        return self._cards[4].get_value("tk7")

    @tk7.setter
    def tk7(self, value: float) -> None:
        """Set the tk7 property."""
        self._cards[4].set_value("tk7", value)

    @property
    def tk8(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector in t-direction defined
        """ # nopep8
        return self._cards[4].get_value("tk8")

    @tk8.setter
    def tk8(self, value: float) -> None:
        """Set the tk8 property."""
        self._cards[4].set_value("tk8", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Control point i to define the control grid
        """ # nopep8
        return self._cards[5].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[5].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Control point i to define the control grid
        """ # nopep8
        return self._cards[5].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[5].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Control point i to define the control grid
        """ # nopep8
        return self._cards[5].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        """Set the n3 property."""
        self._cards[5].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Control point i to define the control grid
        """ # nopep8
        return self._cards[5].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        """Set the n4 property."""
        self._cards[5].set_value("n4", value)

    @property
    def n5(self) -> typing.Optional[int]:
        """Get or set the Control point i to define the control grid
        """ # nopep8
        return self._cards[5].get_value("n5")

    @n5.setter
    def n5(self, value: int) -> None:
        """Set the n5 property."""
        self._cards[5].set_value("n5", value)

    @property
    def n6(self) -> typing.Optional[int]:
        """Get or set the Control point i to define the control grid
        """ # nopep8
        return self._cards[5].get_value("n6")

    @n6.setter
    def n6(self, value: int) -> None:
        """Set the n6 property."""
        self._cards[5].set_value("n6", value)

    @property
    def n7(self) -> typing.Optional[int]:
        """Get or set the Control point i to define the control grid
        """ # nopep8
        return self._cards[5].get_value("n7")

    @n7.setter
    def n7(self, value: int) -> None:
        """Set the n7 property."""
        self._cards[5].set_value("n7", value)

    @property
    def n8(self) -> typing.Optional[int]:
        """Get or set the Control point i to define the control grid
        """ # nopep8
        return self._cards[5].get_value("n8")

    @n8.setter
    def n8(self, value: int) -> None:
        """Set the n8 property."""
        self._cards[5].set_value("n8", value)

    @property
    def w1(self) -> typing.Optional[float]:
        """Get or set the Weighting factor of control point i defined
        """ # nopep8
        return self._cards[6].get_value("w1")

    @w1.setter
    def w1(self, value: float) -> None:
        """Set the w1 property."""
        self._cards[6].set_value("w1", value)

    @property
    def w2(self) -> typing.Optional[float]:
        """Get or set the Weighting factor of control point i defined
        """ # nopep8
        return self._cards[6].get_value("w2")

    @w2.setter
    def w2(self, value: float) -> None:
        """Set the w2 property."""
        self._cards[6].set_value("w2", value)

    @property
    def w3(self) -> typing.Optional[float]:
        """Get or set the Weighting factor of control point i defined
        """ # nopep8
        return self._cards[6].get_value("w3")

    @w3.setter
    def w3(self, value: float) -> None:
        """Set the w3 property."""
        self._cards[6].set_value("w3", value)

    @property
    def w4(self) -> typing.Optional[float]:
        """Get or set the Weighting factor of control point i defined
        """ # nopep8
        return self._cards[6].get_value("w4")

    @w4.setter
    def w4(self, value: float) -> None:
        """Set the w4 property."""
        self._cards[6].set_value("w4", value)

    @property
    def w5(self) -> typing.Optional[float]:
        """Get or set the Weighting factor of control point i defined
        """ # nopep8
        return self._cards[6].get_value("w5")

    @w5.setter
    def w5(self, value: float) -> None:
        """Set the w5 property."""
        self._cards[6].set_value("w5", value)

    @property
    def w6(self) -> typing.Optional[float]:
        """Get or set the Weighting factor of control point i defined
        """ # nopep8
        return self._cards[6].get_value("w6")

    @w6.setter
    def w6(self, value: float) -> None:
        """Set the w6 property."""
        self._cards[6].set_value("w6", value)

    @property
    def w7(self) -> typing.Optional[float]:
        """Get or set the Weighting factor of control point i defined
        """ # nopep8
        return self._cards[6].get_value("w7")

    @w7.setter
    def w7(self, value: float) -> None:
        """Set the w7 property."""
        self._cards[6].set_value("w7", value)

    @property
    def w8(self) -> typing.Optional[float]:
        """Get or set the Weighting factor of control point i defined
        """ # nopep8
        return self._cards[6].get_value("w8")

    @w8.setter
    def w8(self, value: float) -> None:
        """Set the w8 property."""
        self._cards[6].set_value("w8", value)

