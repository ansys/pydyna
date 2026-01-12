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

"""Module providing the ContactAddWear class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTACTADDWEAR_CARD0 = (
    FieldSchema("cid", int, 0, 10, None),
    FieldSchema("wtype", int, 10, 10, 0),
    FieldSchema("p1", float, 20, 10, None),
    FieldSchema("p2", float, 30, 10, None),
    FieldSchema("p3", float, 40, 10, None),
    FieldSchema("p4", float, 50, 10, None),
    FieldSchema("p5", float, 60, 10, None),
    FieldSchema("p6", float, 70, 10, None),
)

_CONTACTADDWEAR_CARD1 = (
    FieldSchema("w1", float, 0, 10, None),
    FieldSchema("w2", float, 10, 10, None),
    FieldSchema("w3", float, 20, 10, None),
    FieldSchema("w4", float, 30, 10, None),
    FieldSchema("w5", float, 40, 10, None),
    FieldSchema("w6", float, 50, 10, None),
    FieldSchema("w7", float, 60, 10, None),
    FieldSchema("w8", float, 70, 10, None),
)

class ContactAddWear(KeywordBase):
    """DYNA CONTACT_ADD_WEAR keyword"""

    keyword = "CONTACT"
    subkeyword = "ADD_WEAR"

    def __init__(self, **kwargs):
        """Initialize the ContactAddWear class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTACTADDWEAR_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONTACTADDWEAR_CARD1,
                **kwargs,
            ),        ]
    @property
    def cid(self) -> typing.Optional[int]:
        """Get or set the Contact interface ID, see *CONTACT_...
        LT.0:	Perturb contact surface according to wear values (see Remark 5).
        GT.0:	Calculate wear properties for post - processing only.
        """ # nopep8
        return self._cards[0].get_value("cid")

    @cid.setter
    def cid(self, value: int) -> None:
        """Set the cid property."""
        self._cards[0].set_value("cid", value)

    @property
    def wtype(self) -> int:
        """Get or set the Wear law:
        LT.0: user defined wear law, value denotes type used in subroutine
        EQ.0 Archard's wear law. d(wear_depth)/dt = K*contact_pressure*sliding_velocity/H
        """ # nopep8
        return self._cards[0].get_value("wtype")

    @wtype.setter
    def wtype(self, value: int) -> None:
        """Set the wtype property."""
        self._cards[0].set_value("wtype", value)

    @property
    def p1(self) -> typing.Optional[float]:
        """Get or set the First wear parameter.
        WTYPE.EQ.0: Dimensionless parameter, K. If negative the absolute value denotes table ID with K = K(p, d) as a function of contact pressure p >= 0 and relative sliding velocity d >= 0.
        WTYPE.LT.0: Number of user wear parameters for this interface.
        """ # nopep8
        return self._cards[0].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        """Set the p1 property."""
        self._cards[0].set_value("p1", value)

    @property
    def p2(self) -> typing.Optional[float]:
        """Get or set the Second wear parameter.
        WTYPE.EQ.0: SURFA surface hardness parameter, Hs. If negative the absolute value denotes curve ID with Hs = Hs(Ts) as function of SURFA node temperature Ts.
        WTYPE.LT.0: Number of user wear history variables per contact node.
        """ # nopep8
        return self._cards[0].get_value("p2")

    @p2.setter
    def p2(self, value: float) -> None:
        """Set the p2 property."""
        self._cards[0].set_value("p2", value)

    @property
    def p3(self) -> typing.Optional[float]:
        """Get or set the Third wear parameter.
        WTYPE.EQ.0: SURFB surface hardness parameter, Hm. If negative the absolute value denotes curve ID with Hm = Hm(Tm) as function of SURFB node temperature Tm.
        WTYPE.LT.0: Not used
        """ # nopep8
        return self._cards[0].get_value("p3")

    @p3.setter
    def p3(self, value: float) -> None:
        """Set the p3 property."""
        self._cards[0].set_value("p3", value)

    @property
    def p4(self) -> typing.Optional[float]:
        """Get or set the Not used
        """ # nopep8
        return self._cards[0].get_value("p4")

    @p4.setter
    def p4(self, value: float) -> None:
        """Set the p4 property."""
        self._cards[0].set_value("p4", value)

    @property
    def p5(self) -> typing.Optional[float]:
        """Get or set the Not used
        """ # nopep8
        return self._cards[0].get_value("p5")

    @p5.setter
    def p5(self, value: float) -> None:
        """Set the p5 property."""
        self._cards[0].set_value("p5", value)

    @property
    def p6(self) -> typing.Optional[float]:
        """Get or set the Not used
        """ # nopep8
        return self._cards[0].get_value("p6")

    @p6.setter
    def p6(self, value: float) -> None:
        """Set the p6 property."""
        self._cards[0].set_value("p6", value)

    @property
    def w1(self) -> typing.Optional[float]:
        """Get or set the User defined wear parameter
        """ # nopep8
        return self._cards[1].get_value("w1")

    @w1.setter
    def w1(self, value: float) -> None:
        """Set the w1 property."""
        self._cards[1].set_value("w1", value)

    @property
    def w2(self) -> typing.Optional[float]:
        """Get or set the User defined wear parameter
        """ # nopep8
        return self._cards[1].get_value("w2")

    @w2.setter
    def w2(self, value: float) -> None:
        """Set the w2 property."""
        self._cards[1].set_value("w2", value)

    @property
    def w3(self) -> typing.Optional[float]:
        """Get or set the User defined wear parameter
        """ # nopep8
        return self._cards[1].get_value("w3")

    @w3.setter
    def w3(self, value: float) -> None:
        """Set the w3 property."""
        self._cards[1].set_value("w3", value)

    @property
    def w4(self) -> typing.Optional[float]:
        """Get or set the User defined wear parameter
        """ # nopep8
        return self._cards[1].get_value("w4")

    @w4.setter
    def w4(self, value: float) -> None:
        """Set the w4 property."""
        self._cards[1].set_value("w4", value)

    @property
    def w5(self) -> typing.Optional[float]:
        """Get or set the User defined wear parameter
        """ # nopep8
        return self._cards[1].get_value("w5")

    @w5.setter
    def w5(self, value: float) -> None:
        """Set the w5 property."""
        self._cards[1].set_value("w5", value)

    @property
    def w6(self) -> typing.Optional[float]:
        """Get or set the User defined wear parameter
        """ # nopep8
        return self._cards[1].get_value("w6")

    @w6.setter
    def w6(self, value: float) -> None:
        """Set the w6 property."""
        self._cards[1].set_value("w6", value)

    @property
    def w7(self) -> typing.Optional[float]:
        """Get or set the User defined wear parameter
        """ # nopep8
        return self._cards[1].get_value("w7")

    @w7.setter
    def w7(self, value: float) -> None:
        """Set the w7 property."""
        self._cards[1].set_value("w7", value)

    @property
    def w8(self) -> typing.Optional[float]:
        """Get or set the User defined wear parameter
        """ # nopep8
        return self._cards[1].get_value("w8")

    @w8.setter
    def w8(self, value: float) -> None:
        """Set the w8 property."""
        self._cards[1].set_value("w8", value)

