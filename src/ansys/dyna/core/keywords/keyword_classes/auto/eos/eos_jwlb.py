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

"""Module providing the EosJwlb class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EOSJWLB_CARD0 = (
    FieldSchema("eosid", int, 0, 10, None),
    FieldSchema("a1", float, 10, 10, 0.0),
    FieldSchema("a2", float, 20, 10, 0.0),
    FieldSchema("a3", float, 30, 10, 0.0),
    FieldSchema("a4", float, 40, 10, 0.0),
    FieldSchema("a5", float, 50, 10, 0.0),
)

_EOSJWLB_CARD1 = (
    FieldSchema("r1", float, 0, 10, 0.0),
    FieldSchema("r2", float, 10, 10, 0.0),
    FieldSchema("r3", float, 20, 10, 0.0),
    FieldSchema("r4", float, 30, 10, 0.0),
    FieldSchema("r5", float, 40, 10, 0.0),
)

_EOSJWLB_CARD2 = (
    FieldSchema("al1", float, 0, 10, 0.0),
    FieldSchema("al2", float, 10, 10, 0.0),
    FieldSchema("al3", float, 20, 10, 0.0),
    FieldSchema("al4", float, 30, 10, 0.0),
    FieldSchema("al5", float, 40, 10, 0.0),
)

_EOSJWLB_CARD3 = (
    FieldSchema("bl1", float, 0, 10, 0.0),
    FieldSchema("bl2", float, 10, 10, 0.0),
    FieldSchema("bl3", float, 20, 10, 0.0),
    FieldSchema("bl4", float, 30, 10, 0.0),
    FieldSchema("bl5", float, 40, 10, 0.0),
)

_EOSJWLB_CARD4 = (
    FieldSchema("rl1", float, 0, 10, 0.0),
    FieldSchema("rl2", float, 10, 10, 0.0),
    FieldSchema("rl3", float, 20, 10, 0.0),
    FieldSchema("rl4", float, 30, 10, 0.0),
    FieldSchema("rl5", float, 40, 10, 0.0),
)

_EOSJWLB_CARD5 = (
    FieldSchema("c", float, 0, 10, None),
    FieldSchema("omega", float, 10, 10, None),
    FieldSchema("e", float, 20, 10, None),
    FieldSchema("v0", float, 30, 10, None),
)

class EosJwlb(KeywordBase):
    """DYNA EOS_JWLB keyword"""

    keyword = "EOS"
    subkeyword = "JWLB"

    def __init__(self, **kwargs):
        """Initialize the EosJwlb class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EOSJWLB_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EOSJWLB_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EOSJWLB_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EOSJWLB_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EOSJWLB_CARD4,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _EOSJWLB_CARD5,
                **kwargs,
            ),        ]
    @property
    def eosid(self) -> typing.Optional[int]:
        """Get or set the Equation of state label.
        """ # nopep8
        return self._cards[0].get_value("eosid")

    @eosid.setter
    def eosid(self, value: int) -> None:
        """Set the eosid property."""
        self._cards[0].set_value("eosid", value)

    @property
    def a1(self) -> float:
        """Get or set the Equation of state coefficient.
        """ # nopep8
        return self._cards[0].get_value("a1")

    @a1.setter
    def a1(self, value: float) -> None:
        """Set the a1 property."""
        self._cards[0].set_value("a1", value)

    @property
    def a2(self) -> float:
        """Get or set the Equation of state coefficient.
        """ # nopep8
        return self._cards[0].get_value("a2")

    @a2.setter
    def a2(self, value: float) -> None:
        """Set the a2 property."""
        self._cards[0].set_value("a2", value)

    @property
    def a3(self) -> float:
        """Get or set the Equation of state coefficient.
        """ # nopep8
        return self._cards[0].get_value("a3")

    @a3.setter
    def a3(self, value: float) -> None:
        """Set the a3 property."""
        self._cards[0].set_value("a3", value)

    @property
    def a4(self) -> float:
        """Get or set the Equation of state coefficient.
        """ # nopep8
        return self._cards[0].get_value("a4")

    @a4.setter
    def a4(self, value: float) -> None:
        """Set the a4 property."""
        self._cards[0].set_value("a4", value)

    @property
    def a5(self) -> float:
        """Get or set the Equation of state coefficient.
        """ # nopep8
        return self._cards[0].get_value("a5")

    @a5.setter
    def a5(self, value: float) -> None:
        """Set the a5 property."""
        self._cards[0].set_value("a5", value)

    @property
    def r1(self) -> float:
        """Get or set the Equation of state coefficient.
        """ # nopep8
        return self._cards[1].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        """Set the r1 property."""
        self._cards[1].set_value("r1", value)

    @property
    def r2(self) -> float:
        """Get or set the Equation of state coefficient.
        """ # nopep8
        return self._cards[1].get_value("r2")

    @r2.setter
    def r2(self, value: float) -> None:
        """Set the r2 property."""
        self._cards[1].set_value("r2", value)

    @property
    def r3(self) -> float:
        """Get or set the Equation of state coefficient.
        """ # nopep8
        return self._cards[1].get_value("r3")

    @r3.setter
    def r3(self, value: float) -> None:
        """Set the r3 property."""
        self._cards[1].set_value("r3", value)

    @property
    def r4(self) -> float:
        """Get or set the Equation of state coefficient.
        """ # nopep8
        return self._cards[1].get_value("r4")

    @r4.setter
    def r4(self, value: float) -> None:
        """Set the r4 property."""
        self._cards[1].set_value("r4", value)

    @property
    def r5(self) -> float:
        """Get or set the Equation of state coefficient.
        """ # nopep8
        return self._cards[1].get_value("r5")

    @r5.setter
    def r5(self, value: float) -> None:
        """Set the r5 property."""
        self._cards[1].set_value("r5", value)

    @property
    def al1(self) -> float:
        """Get or set the A-lambda1, equation of state coefficient.
        """ # nopep8
        return self._cards[2].get_value("al1")

    @al1.setter
    def al1(self, value: float) -> None:
        """Set the al1 property."""
        self._cards[2].set_value("al1", value)

    @property
    def al2(self) -> float:
        """Get or set the A-lambda2, equation of state coefficient.
        """ # nopep8
        return self._cards[2].get_value("al2")

    @al2.setter
    def al2(self, value: float) -> None:
        """Set the al2 property."""
        self._cards[2].set_value("al2", value)

    @property
    def al3(self) -> float:
        """Get or set the A-lambda3, equation of state coefficient.
        """ # nopep8
        return self._cards[2].get_value("al3")

    @al3.setter
    def al3(self, value: float) -> None:
        """Set the al3 property."""
        self._cards[2].set_value("al3", value)

    @property
    def al4(self) -> float:
        """Get or set the A-lambda4, equation of state coefficient.
        """ # nopep8
        return self._cards[2].get_value("al4")

    @al4.setter
    def al4(self, value: float) -> None:
        """Set the al4 property."""
        self._cards[2].set_value("al4", value)

    @property
    def al5(self) -> float:
        """Get or set the A-lambda5, equation of state coefficient.
        """ # nopep8
        return self._cards[2].get_value("al5")

    @al5.setter
    def al5(self, value: float) -> None:
        """Set the al5 property."""
        self._cards[2].set_value("al5", value)

    @property
    def bl1(self) -> float:
        """Get or set the B-lambda1, equation of state coefficient.
        """ # nopep8
        return self._cards[3].get_value("bl1")

    @bl1.setter
    def bl1(self, value: float) -> None:
        """Set the bl1 property."""
        self._cards[3].set_value("bl1", value)

    @property
    def bl2(self) -> float:
        """Get or set the B-lambda2, equation of state coefficient.
        """ # nopep8
        return self._cards[3].get_value("bl2")

    @bl2.setter
    def bl2(self, value: float) -> None:
        """Set the bl2 property."""
        self._cards[3].set_value("bl2", value)

    @property
    def bl3(self) -> float:
        """Get or set the B-lambda3, equation of state coefficient.
        """ # nopep8
        return self._cards[3].get_value("bl3")

    @bl3.setter
    def bl3(self, value: float) -> None:
        """Set the bl3 property."""
        self._cards[3].set_value("bl3", value)

    @property
    def bl4(self) -> float:
        """Get or set the B-lambda4, equation of state coefficient.
        """ # nopep8
        return self._cards[3].get_value("bl4")

    @bl4.setter
    def bl4(self, value: float) -> None:
        """Set the bl4 property."""
        self._cards[3].set_value("bl4", value)

    @property
    def bl5(self) -> float:
        """Get or set the B-lambda5, equation of state coefficient.
        """ # nopep8
        return self._cards[3].get_value("bl5")

    @bl5.setter
    def bl5(self, value: float) -> None:
        """Set the bl5 property."""
        self._cards[3].set_value("bl5", value)

    @property
    def rl1(self) -> float:
        """Get or set the R-lambda1, equation of state coefficient.
        """ # nopep8
        return self._cards[4].get_value("rl1")

    @rl1.setter
    def rl1(self, value: float) -> None:
        """Set the rl1 property."""
        self._cards[4].set_value("rl1", value)

    @property
    def rl2(self) -> float:
        """Get or set the R-lambda2, equation of state coefficient.
        """ # nopep8
        return self._cards[4].get_value("rl2")

    @rl2.setter
    def rl2(self, value: float) -> None:
        """Set the rl2 property."""
        self._cards[4].set_value("rl2", value)

    @property
    def rl3(self) -> float:
        """Get or set the R-lambda3, equation of state coefficient.
        """ # nopep8
        return self._cards[4].get_value("rl3")

    @rl3.setter
    def rl3(self, value: float) -> None:
        """Set the rl3 property."""
        self._cards[4].set_value("rl3", value)

    @property
    def rl4(self) -> float:
        """Get or set the R-lambda4, equation of state coefficient.
        """ # nopep8
        return self._cards[4].get_value("rl4")

    @rl4.setter
    def rl4(self, value: float) -> None:
        """Set the rl4 property."""
        self._cards[4].set_value("rl4", value)

    @property
    def rl5(self) -> float:
        """Get or set the R-lambda5, equation of state coefficient.
        """ # nopep8
        return self._cards[4].get_value("rl5")

    @rl5.setter
    def rl5(self, value: float) -> None:
        """Set the rl5 property."""
        self._cards[4].set_value("rl5", value)

    @property
    def c(self) -> typing.Optional[float]:
        """Get or set the Equation of state coefficient.
        """ # nopep8
        return self._cards[5].get_value("c")

    @c.setter
    def c(self, value: float) -> None:
        """Set the c property."""
        self._cards[5].set_value("c", value)

    @property
    def omega(self) -> typing.Optional[float]:
        """Get or set the Equation of state coefficient.
        """ # nopep8
        return self._cards[5].get_value("omega")

    @omega.setter
    def omega(self, value: float) -> None:
        """Set the omega property."""
        self._cards[5].set_value("omega", value)

    @property
    def e(self) -> typing.Optional[float]:
        """Get or set the Energy density per unit initial volume.
        """ # nopep8
        return self._cards[5].get_value("e")

    @e.setter
    def e(self, value: float) -> None:
        """Set the e property."""
        self._cards[5].set_value("e", value)

    @property
    def v0(self) -> typing.Optional[float]:
        """Get or set the Initial realtive volume.
        """ # nopep8
        return self._cards[5].get_value("v0")

    @v0.setter
    def v0(self, value: float) -> None:
        """Set the v0 property."""
        self._cards[5].set_value("v0", value)

