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

"""Module providing the EfvStrength011 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVSTRENGTH011_CARD0 = (
    FieldSchema("strid", int, 0, 10, None),
    FieldSchema("g", float, 10, 10, None),
    FieldSchema("istrtyp", int, 20, 10, 0),
)

_EFVSTRENGTH011_CARD1 = (
    FieldSchema("sifmshel", float, 0, 10, None),
    FieldSchema("a", float, 10, 10, None),
    FieldSchema("n", float, 20, 10, None),
    FieldSchema("c", float, 30, 10, None),
    FieldSchema("b", float, 40, 10, None),
    FieldSchema("m", float, 50, 10, None),
    FieldSchema("sigmfmx", float, 60, 10, None),
)

_EFVSTRENGTH011_CARD2 = (
    FieldSchema("sifmshel", float, 0, 10, None),
    FieldSchema("s1", float, 10, 10, None),
    FieldSchema("p1", float, 20, 10, None),
    FieldSchema("s2", float, 30, 10, None),
    FieldSchema("p2", float, 40, 10, None),
    FieldSchema("c", float, 50, 10, None),
    FieldSchema("sfmax", float, 60, 10, None),
    FieldSchema("alpha", float, 70, 10, None),
)

class EfvStrength011(KeywordBase):
    """DYNA EFV_STRENGTH_011 keyword"""

    keyword = "EFV"
    subkeyword = "STRENGTH_011"

    def __init__(self, **kwargs):
        """Initialize the EfvStrength011 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTH011_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTH011_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVSTRENGTH011_CARD2,
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
        """Get or set the Shear modulus
        """ # nopep8
        return self._cards[0].get_value("g")

    @g.setter
    def g(self, value: float) -> None:
        """Set the g property."""
        self._cards[0].set_value("g", value)

    @property
    def istrtyp(self) -> int:
        """Get or set the Strength model type:
        EQ.0: Continuous, JH2(see Remark 1)
        EQ.1: Segmented, JH1(see Remark 2
        """ # nopep8
        return self._cards[0].get_value("istrtyp")

    @istrtyp.setter
    def istrtyp(self, value: int) -> None:
        """Set the istrtyp property."""
        if value not in [0, 1, None]:
            raise Exception("""istrtyp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("istrtyp", value)

    @property
    def sifmshel(self) -> typing.Optional[float]:
        """Get or set the Hugoniot elastic limit, o_HEL
        """ # nopep8
        return self._cards[1].get_value("sifmshel")

    @sifmshel.setter
    def sifmshel(self, value: float) -> None:
        """Set the sifmshel property."""
        self._cards[1].set_value("sifmshel", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Intact strength constant, A
        """ # nopep8
        return self._cards[1].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[1].set_value("a", value)

    @property
    def n(self) -> typing.Optional[float]:
        """Get or set the Intact strength exponent, n
        """ # nopep8
        return self._cards[1].get_value("n")

    @n.setter
    def n(self, value: float) -> None:
        """Set the n property."""
        self._cards[1].set_value("n", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Strain rate constant, C
        """ # nopep8
        return self._cards[1].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[1].set_value("c", value)

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
    def sigmfmx(self) -> typing.Optional[float]:
        """Get or set the Maximum fracture strength ratio, o_F,max
        """ # nopep8
        return self._cards[1].get_value("sigmfmx")

    @sigmfmx.setter
    def sigmfmx(self, value: float) -> None:
        """Set the sigmfmx property."""
        self._cards[1].set_value("sigmfmx", value)

    @property
    def sifmshel(self) -> typing.Optional[float]:
        """Get or set the Hugoniot elastic limit, o_HEL
        """ # nopep8
        return self._cards[2].get_value("sifmshel")

    @sifmshel.setter
    def sifmshel(self, value: float) -> None:
        """Set the sifmshel property."""
        self._cards[2].set_value("sifmshel", value)

    @property
    def s1(self) -> typing.Optional[float]:
        """Get or set the Effective stress at P1, S_1
        """ # nopep8
        return self._cards[2].get_value("s1")

    @s1.setter
    def s1(self, value: float) -> None:
        """Set the s1 property."""
        self._cards[2].set_value("s1", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the Pressure point 1 for intact material, P_1
        """ # nopep8
        return self._cards[2].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        """Set the p1 property."""
        self._cards[2].set_value("p1", value)

    @property
    def s2(self) -> typing.Optional[float]:
        """Get or set the Effective stress at P2, S_2
        """ # nopep8
        return self._cards[2].get_value("s2")

    @s2.setter
    def s2(self, value: float) -> None:
        """Set the s2 property."""
        self._cards[2].set_value("s2", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the Pressure point 2 for intact material, P_2
        """ # nopep8
        return self._cards[2].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        """Set the p2 property."""
        self._cards[2].set_value("p2", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Strain rate constant, C
        """ # nopep8
        return self._cards[2].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[2].set_value("c", value)

    @property
    def sfmax(self) -> typing.Optional[float]:
        """Get or set the Maximum fracture strength, S_(F,max )
        """ # nopep8
        return self._cards[2].get_value("sfmax")

    @sfmax.setter
    def sfmax(self, value: float) -> None:
        """Set the sfmax property."""
        self._cards[2].set_value("sfmax", value)

    @property
    def alpha(self) -> typing.Optional[float]:
        """Get or set the Initial slope of the failed material strength curve. See Figure
        """ # nopep8
        return self._cards[2].get_value("alpha")

    @alpha.setter
    def alpha(self, value: float) -> None:
        """Set the alpha property."""
        self._cards[2].set_value("alpha", value)

