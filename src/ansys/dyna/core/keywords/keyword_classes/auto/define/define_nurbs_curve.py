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

"""Module providing the DefineNurbsCurve class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DEFINENURBSCURVE_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("n", int, 10, 10, None),
    FieldSchema("p", int, 20, 10, None),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("type", int, 40, 10, 0),
    FieldSchema("wfl", int, 50, 10, 0),
)

_DEFINENURBSCURVE_CARD1 = (
    FieldSchema("k1", float, 0, 10, None),
    FieldSchema("k2", float, 10, 10, None),
    FieldSchema("k3", float, 20, 10, None),
    FieldSchema("k4", float, 30, 10, None),
    FieldSchema("k5", float, 40, 10, None),
    FieldSchema("k6", float, 50, 10, None),
    FieldSchema("k7", float, 60, 10, None),
    FieldSchema("k8", float, 70, 10, None),
)

_DEFINENURBSCURVE_CARD2 = (
    FieldSchema("x", float, 0, 10, None),
    FieldSchema("y", float, 10, 10, None),
    FieldSchema("z", float, 20, 10, None),
    FieldSchema("w", float, 30, 10, None),
)

class DefineNurbsCurve(KeywordBase):
    """DYNA DEFINE_NURBS_CURVE keyword"""

    keyword = "DEFINE"
    subkeyword = "NURBS_CURVE"
    option_specs = [
        OptionSpec("TITLE", -1, 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the DefineNurbsCurve class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DEFINENURBSCURVE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINENURBSCURVE_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _DEFINENURBSCURVE_CARD2,
                **kwargs,
            ),            OptionCardSet(
                option_spec = DefineNurbsCurve.option_specs[0],
                cards = [
                    Card(
                        [
                            Field(
                                "title",
                                str,
                                0,
                                80,
                                kwargs.get("title")
                            ),
                        ],
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Curve ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def n(self) -> typing.Optional[int]:
        """Get or set the Number of control points.
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: int) -> None:
        """Set the n property."""
        self._cards[0].set_value("n", value)

    @property
    def p(self) -> typing.Optional[int]:
        """Get or set the Polynomial degree.
        """ # nopep8
        return self._cards[0].get_value("p")

    @p.setter
    def p(self, value: int) -> None:
        """Set the p property."""
        self._cards[0].set_value("p", value)

    @property
    def type(self) -> int:
        """Get or set the Coordinate type.
        EQ.0:Spatial.
        EQ.1:Parametric.
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        """Set the type property."""
        if value not in [0, 1, None]:
            raise Exception("""type must be `None` or one of {0,1}.""")
        self._cards[0].set_value("type", value)

    @property
    def wfl(self) -> int:
        """Get or set the Flag for user defined control weights.
        EQ.0: Control weights are assumed to be uniform and positive.
        EQ.1: Control weights are defined on the forth entry of cards B.
        """ # nopep8
        return self._cards[0].get_value("wfl")

    @wfl.setter
    def wfl(self, value: int) -> None:
        """Set the wfl property."""
        if value not in [0, 1, None]:
            raise Exception("""wfl must be `None` or one of {0,1}.""")
        self._cards[0].set_value("wfl", value)

    @property
    def k1(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector.
        """ # nopep8
        return self._cards[1].get_value("k1")

    @k1.setter
    def k1(self, value: float) -> None:
        """Set the k1 property."""
        self._cards[1].set_value("k1", value)

    @property
    def k2(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector.
        """ # nopep8
        return self._cards[1].get_value("k2")

    @k2.setter
    def k2(self, value: float) -> None:
        """Set the k2 property."""
        self._cards[1].set_value("k2", value)

    @property
    def k3(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector.
        """ # nopep8
        return self._cards[1].get_value("k3")

    @k3.setter
    def k3(self, value: float) -> None:
        """Set the k3 property."""
        self._cards[1].set_value("k3", value)

    @property
    def k4(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector.
        """ # nopep8
        return self._cards[1].get_value("k4")

    @k4.setter
    def k4(self, value: float) -> None:
        """Set the k4 property."""
        self._cards[1].set_value("k4", value)

    @property
    def k5(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector.
        """ # nopep8
        return self._cards[1].get_value("k5")

    @k5.setter
    def k5(self, value: float) -> None:
        """Set the k5 property."""
        self._cards[1].set_value("k5", value)

    @property
    def k6(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector.
        """ # nopep8
        return self._cards[1].get_value("k6")

    @k6.setter
    def k6(self, value: float) -> None:
        """Set the k6 property."""
        self._cards[1].set_value("k6", value)

    @property
    def k7(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector.
        """ # nopep8
        return self._cards[1].get_value("k7")

    @k7.setter
    def k7(self, value: float) -> None:
        """Set the k7 property."""
        self._cards[1].set_value("k7", value)

    @property
    def k8(self) -> typing.Optional[float]:
        """Get or set the Values of the univariate knot vector.
        """ # nopep8
        return self._cards[1].get_value("k8")

    @k8.setter
    def k8(self, value: float) -> None:
        """Set the k8 property."""
        self._cards[1].set_value("k8", value)

    @property
    def x(self) -> typing.Optional[float]:
        """Get or set the Spatial coordinates in the global X direction.
        """ # nopep8
        return self._cards[2].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        """Set the x property."""
        self._cards[2].set_value("x", value)

    @property
    def y(self) -> typing.Optional[float]:
        """Get or set the Spatial coordinates in the global Y direction.
        """ # nopep8
        return self._cards[2].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[2].set_value("y", value)

    @property
    def z(self) -> typing.Optional[float]:
        """Get or set the Spatial coordinates in the global Z direction.
        """ # nopep8
        return self._cards[2].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        """Set the z property."""
        self._cards[2].set_value("z", value)

    @property
    def w(self) -> typing.Optional[float]:
        """Get or set the Control weights.
        """ # nopep8
        return self._cards[2].get_value("w")

    @w.setter
    def w(self, value: float) -> None:
        """Set the w property."""
        self._cards[2].set_value("w", value)

    @property
    def title(self) -> typing.Optional[str]:
        """Get or set the Additional title line
        """ # nopep8
        return self._cards[3].cards[0].get_value("title")

    @title.setter
    def title(self, value: str) -> None:
        """Set the title property."""
        self._cards[3].cards[0].set_value("title", value)

        if value:
            self.activate_option("TITLE")

