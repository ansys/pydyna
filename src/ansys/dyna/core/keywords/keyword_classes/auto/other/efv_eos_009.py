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

"""Module providing the EfvEos009 class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVEOS009_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("a", float, 10, 10, None),
    FieldSchema("b", float, 20, 10, None),
    FieldSchema("r1", float, 30, 10, None),
    FieldSchema("r2", float, 40, 10, None),
    FieldSchema("omega", float, 50, 10, None),
    FieldSchema("dcj", float, 60, 10, None),
    FieldSchema("ecj", float, 70, 10, None),
)

_EFVEOS009_CARD1 = (
    FieldSchema("pcj", float, 0, 10, None),
    FieldSchema("bcj", float, 10, 10, None),
    FieldSchema("kbk", float, 20, 10, None),
    FieldSchema("c", float, 30, 10, None),
    FieldSchema("to8", int, 40, 10, 0),
    FieldSchema("add", int, 50, 10, 0),
)

_EFVEOS009_CARD2 = (
    FieldSchema("eadd", float, 0, 10, None),
    FieldSchema("econ", float, 10, 10, None),
    FieldSchema("eexp", float, 20, 10, None),
    FieldSchema("pexp", float, 30, 10, None),
)

_EFVEOS009_CARD3 = (
    FieldSchema("qt", float, 0, 10, None),
    FieldSchema("t1", float, 10, 10, None),
    FieldSchema("t2", float, 20, 10, None),
)

class EfvEos009(KeywordBase):
    """DYNA EFV_EOS_009 keyword"""

    keyword = "EFV"
    subkeyword = "EOS_009"

    def __init__(self, **kwargs):
        """Initialize the EfvEos009 class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVEOS009_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS009_CARD1,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS009_CARD2,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _EFVEOS009_CARD3,
                **kwargs,
            ),
        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state identification. A unique number or label must be used.(see Remark Error! Reference source not found. in *EFV_MAT).
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def a(self) -> typing.Optional[float]:
        """Get or set the Product JWL parameter A, see Remarks
        """ # nopep8
        return self._cards[0].get_value("a")

    @a.setter
    def a(self, value: float) -> None:
        """Set the a property."""
        self._cards[0].set_value("a", value)

    @property
    def b(self) -> typing.Optional[float]:
        """Get or set the Product JWL parameter B, see Remarks
        """ # nopep8
        return self._cards[0].get_value("b")

    @b.setter
    def b(self, value: float) -> None:
        """Set the b property."""
        self._cards[0].set_value("b", value)

    @property
    def r1(self) -> typing.Optional[float]:
        """Get or set the Product JWL parameter R1, see Remarks
        """ # nopep8
        return self._cards[0].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        """Set the r1 property."""
        self._cards[0].set_value("r1", value)

    @property
    def r2(self) -> typing.Optional[float]:
        """Get or set the Product JWL parameter R2, see Remarks
        """ # nopep8
        return self._cards[0].get_value("r2")

    @r2.setter
    def r2(self, value: float) -> None:
        """Set the r2 property."""
        self._cards[0].set_value("r2", value)

    @property
    def omega(self) -> typing.Optional[float]:
        """Get or set the Product JWL parameter OMEGA, see Remarks
        """ # nopep8
        return self._cards[0].get_value("omega")

    @omega.setter
    def omega(self, value: float) -> None:
        """Set the omega property."""
        self._cards[0].set_value("omega", value)

    @property
    def dcj(self) -> typing.Optional[float]:
        """Get or set the Chapman-Jouget detonation velocity for the product, D_CJ,p
        """ # nopep8
        return self._cards[0].get_value("dcj")

    @dcj.setter
    def dcj(self, value: float) -> None:
        """Set the dcj property."""
        self._cards[0].set_value("dcj", value)

    @property
    def ecj(self) -> typing.Optional[float]:
        """Get or set the Chapman-Jouget energy per unit mass for the product
        """ # nopep8
        return self._cards[0].get_value("ecj")

    @ecj.setter
    def ecj(self, value: float) -> None:
        """Set the ecj property."""
        self._cards[0].set_value("ecj", value)

    @property
    def pcj(self) -> typing.Optional[float]:
        """Get or set the Chapman-Jouget pressure, p_CJ,
        """ # nopep8
        return self._cards[1].get_value("pcj")

    @pcj.setter
    def pcj(self, value: float) -> None:
        """Set the pcj property."""
        self._cards[1].set_value("pcj", value)

    @property
    def bcj(self) -> typing.Optional[float]:
        """Get or set the Burn on compression fraction BCJ
        """ # nopep8
        return self._cards[1].get_value("bcj")

    @bcj.setter
    def bcj(self, value: float) -> None:
        """Set the bcj property."""
        self._cards[1].set_value("bcj", value)

    @property
    def kbk(self) -> typing.Optional[float]:
        """Get or set the Pre-burn bulk modulus KBK
        """ # nopep8
        return self._cards[1].get_value("kbk")

    @kbk.setter
    def kbk(self, value: float) -> None:
        """Set the kbk property."""
        self._cards[1].set_value("kbk", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Adiabatic constant
        """ # nopep8
        return self._cards[1].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[1].set_value("c", value)

    @property
    def to8(self) -> int:
        """Get or set the Auto-convert to ideal gas:
        EQ.0: Yes
        EQ.1: No
        """ # nopep8
        return self._cards[1].get_value("to8")

    @to8.setter
    def to8(self, value: int) -> None:
        """Set the to8 property."""
        if value not in [0, 1, None]:
            raise Exception("""to8 must be `None` or one of {0,1}.""")
        self._cards[1].set_value("to8", value)

    @property
    def add(self) -> int:
        """Get or set the Additional Options:
        EQ.0: None
        EQ.1: Miller extension
        EQ.2: Additional energy
        """ # nopep8
        return self._cards[1].get_value("add")

    @add.setter
    def add(self, value: int) -> None:
        """Set the add property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""add must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("add", value)

    @property
    def eadd(self) -> typing.Optional[float]:
        """Get or set the Additional specific energy
        """ # nopep8
        return self._cards[2].get_value("eadd")

    @eadd.setter
    def eadd(self, value: float) -> None:
        """Set the eadd property."""
        self._cards[2].set_value("eadd", value)

    @property
    def econ(self) -> typing.Optional[float]:
        """Get or set the Energy release constant
        """ # nopep8
        return self._cards[2].get_value("econ")

    @econ.setter
    def econ(self, value: float) -> None:
        """Set the econ property."""
        self._cards[2].set_value("econ", value)

    @property
    def eexp(self) -> typing.Optional[float]:
        """Get or set the Energy release exponent
        """ # nopep8
        return self._cards[2].get_value("eexp")

    @eexp.setter
    def eexp(self, value: float) -> None:
        """Set the eexp property."""
        self._cards[2].set_value("eexp", value)

    @property
    def pexp(self) -> typing.Optional[float]:
        """Get or set the Pressure exponent
        """ # nopep8
        return self._cards[2].get_value("pexp")

    @pexp.setter
    def pexp(self, value: float) -> None:
        """Set the pexp property."""
        self._cards[2].set_value("pexp", value)

    @property
    def qt(self) -> typing.Optional[float]:
        """Get or set the Additional internal energy per unit mass released. See Remark 3.
        """ # nopep8
        return self._cards[3].get_value("qt")

    @qt.setter
    def qt(self, value: float) -> None:
        """Set the qt property."""
        self._cards[3].set_value("qt", value)

    @property
    def t1(self) -> typing.Optional[float]:
        """Get or set the Start time of additional energy release. See Remark 3.
        """ # nopep8
        return self._cards[3].get_value("t1")

    @t1.setter
    def t1(self, value: float) -> None:
        """Set the t1 property."""
        self._cards[3].set_value("t1", value)

    @property
    def t2(self) -> typing.Optional[float]:
        """Get or set the End time of additional energy release. See Remark 3.
        """ # nopep8
        return self._cards[3].get_value("t2")

    @t2.setter
    def t2(self, value: float) -> None:
        """Set the t2 property."""
        self._cards[3].set_value("t2", value)

