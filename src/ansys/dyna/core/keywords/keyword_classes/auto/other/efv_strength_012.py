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

"""Module providing the EfvStrength012 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVSTRENGTH012_CARD0 = (
    FieldSchema("strid", int, 0, 10, None),
    FieldSchema("g", float, 10, 10, None),
    FieldSchema("fc", float, 20, 10, None),
    FieldSchema("ft", float, 30, 10, None),
    FieldSchema("fs", float, 40, 10, None),
    FieldSchema("a", float, 50, 10, None),
    FieldSchema("n", float, 60, 10, None),
    FieldSchema("q", float, 70, 10, None),
)

_EFVSTRENGTH012_CARD1 = (
    FieldSchema("b2d", float, 0, 10, None),
    FieldSchema("gratio", float, 10, 10, None),
    FieldSchema("tensrat", float, 20, 10, None),
    FieldSchema("compra", float, 30, 10, None),
    FieldSchema("b", float, 40, 10, None),
    FieldSchema("m", float, 50, 10, None),
    FieldSchema("alpha", float, 60, 10, None),
    FieldSchema("delta", float, 70, 10, None),
)

_EFVSTRENGTH012_CARD2 = (
    FieldSchema("start", float, 0, 10, None),
    FieldSchema("cap", int, 10, 10, 2),
)

class EfvStrength012(KeywordBase):
    """DYNA EFV_STRENGTH_012 keyword"""

    keyword = "EFV"
    subkeyword = "STRENGTH_012"

    def __init__(self, **kwargs):
        """Initialize the EfvStrength012 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTH012_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTH012_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTH012_CARD2,
                **kwargs,
            ),
        ]
    @property
    def strid(self) -> typing.Optional[int]:
        """Get or set the Strength model identification. A unique number or label must be used (see Remark Error! Reference source not found. in *EFV_MAT)...
        """ # nopep8
        return self._cards[0].get_value("strid")

    @strid.setter
    def strid(self, value: int) -> None:
        """Set the strid property."""
        self._cards[0].set_value("strid", value)

    @property
    def g(self) -> typing.Optional[float]:
        """Get or set the Shear modulus, G
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[0].set_value("g", value)

    @property
    def fc(self) -> typing.Optional[float]:
        """Get or set the Compressive Strength (fc)
        """ # nopep8
        return self._cards[0].get_value("fc")

    @fc.setter
    def fc(self, value: float) -> None:
        """Set the fc property."""
        self._cards[0].set_value("fc", value)

    @property
    def ft(self) -> typing.Optional[float]:
        """Get or set the Relative Tensile Strength (ft/fc)
        """ # nopep8
        return self._cards[0].get_value("ft")

    @ft.setter
    def ft(self, value: float) -> None:
        """Set the ft property."""
        self._cards[0].set_value("ft", value)

    @property
    def fs(self) -> typing.Optional[float]:
        """Get or set the Relative Shear Strength (fs/fc)
        """ # nopep8
        return self._cards[0].get_value("fs")

    @fs.setter
    def fs(self, value: float) -> None:
        """Set the fs property."""
        self._cards[0].set_value("fs", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Intact Failure Surface Constant A
        """ # nopep8
        return self._cards[0].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[0].set_value("a", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Intact Failure Surface Exponent N
        """ # nopep8
        return self._cards[0].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[0].set_value("n", value)

    @property
    def q(self) -> typing.Optional[float]:
        """Get or set the Tens./Comp. Meridian Ratio (Q)
        """ # nopep8
        return self._cards[0].get_value("q")

    @q.setter
    def q(self, value: float) -> None:
        """Set the q property."""
        self._cards[0].set_value("q", value)

    @property
    def b2d(self) -> typing.Optional[float]:
        """Get or set the Brittle to Ductile Transition
        """ # nopep8
        return self._cards[1].get_value("b2d")

    @b2d.setter
    def b2d(self, value: float) -> None:
        """Set the b2d property."""
        self._cards[1].set_value("b2d", value)

    @property
    def gratio(self) -> typing.Optional[float]:
        """Get or set the Ratio between the original shear modulus and the corresponding value after the elastic limit is passed
        """ # nopep8
        return self._cards[1].get_value("gratio")

    @gratio.setter
    def gratio(self, value: float) -> None:
        """Set the gratio property."""
        self._cards[1].set_value("gratio", value)

    @property
    def tensrat(self) -> typing.Optional[float]:
        """Get or set the Ratio between the elastic tensile limit and the tensile strength
        """ # nopep8
        return self._cards[1].get_value("tensrat")

    @tensrat.setter
    def tensrat(self, value: float) -> None:
        """Set the tensrat property."""
        self._cards[1].set_value("tensrat", value)

    @property
    def compra(self) -> typing.Optional[float]:
        """Get or set the Ratio between the elastic compressive limit and the compressive strength
        """ # nopep8
        return self._cards[1].get_value("compra")

    @compra.setter
    def compra(self, value: float) -> None:
        """Set the compra property."""
        self._cards[1].set_value("compra", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Fractured strength constant, B
        """ # nopep8
        return self._cards[1].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[1].set_value("b", value)

    @property
    def m(self) -> typing.Optional[float]:
        """Get or set the Fractured strength exponent, m
        """ # nopep8
        return self._cards[1].get_value("m")

    @m.setter
    def m(self, value: float) -> None:
        """Set the m property."""
        self._cards[1].set_value("m", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Compressive strain rate dependence exponent
        """ # nopep8
        return self._cards[1].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[1].set_value("alpha", value)

    @property
    def delta(self) -> typing.Optional[float]:
        """Get or set the Tensile strain rate dependence exponent
        """ # nopep8
        return self._cards[1].get_value("delta")

    @delta.setter
    def delta(self, value: float) -> None:
        """Set the delta property."""
        self._cards[1].set_value("delta", value)

    @property
    def start(self) -> typing.Optional[float]:
        """Get or set the Maximum Fracture Strength Ratio
        """ # nopep8
        return self._cards[2].get_value("start")

    @start.setter
    def start(self, value: float) -> None:
        """Set the start property."""
        self._cards[2].set_value("start", value)

    @property
    def cap(self) -> int:
        """Get or set the Apply a cap on the elastic surface to ensure that the elastic surface closes at high pressure:
        EQ.1: Yes
        EQ.2: No
        """ # nopep8
        return self._cards[2].get_value("cap")

    @cap.setter
    def cap(self, value: int) -> None:
        """Set the cap property."""
        if value not in [2, 1, None]:
            raise Exception("""cap must be `None` or one of {2,1}.""")
        self._cards[2].set_value("cap", value)

