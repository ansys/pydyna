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

"""Module providing the EmControlContact class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EMCONTROLCONTACT_CARD0 = (
    FieldSchema("emct", int, 0, 10, 0),
    FieldSchema("cconly", int, 10, 10, 0),
    FieldSchema("ctype", int, 20, 10, 0),
    FieldSchema("cotype", int, 30, 10, 0),
    FieldSchema("eps1", float, 40, 10, 0.3),
    FieldSchema("eps2", float, 50, 10, 0.3),
    FieldSchema("eps3", float, 60, 10, 0.3),
    FieldSchema("d0", float, 70, 10, None),
)

class EmControlContact(KeywordBase):
    """DYNA EM_CONTROL_CONTACT keyword"""

    keyword = "EM"
    subkeyword = "CONTROL_CONTACT"

    def __init__(self, **kwargs):
        """Initialize the EmControlContact class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMCONTROLCONTACT_CARD0,
                **kwargs,
            ),        ]
    @property
    def emct(self) -> int:
        """Get or set the EM contact activation flag:
        EQ.0: no contact detection
        EQ.1: contact detection
        """ # nopep8
        return self._cards[0].get_value("emct")

    @emct.setter
    def emct(self, value: int) -> None:
        """Set the emct property."""
        if value not in [0, 1, None]:
            raise Exception("""emct must be `None` or one of {0,1}.""")
        self._cards[0].set_value("emct", value)

    @property
    def cconly(self) -> int:
        """Get or set the Determines on which parts of the model the EM contact should be activated.
        EQ.0: Contact detection between all active parts associated with a conducting material.
        EQ.1: Only look for EM contact between parts associated through the EM_CONTACT card.In some cases this option can reduce the calculation time.
        """ # nopep8
        return self._cards[0].get_value("cconly")

    @cconly.setter
    def cconly(self, value: int) -> None:
        """Set the cconly property."""
        if value not in [0, 1, None]:
            raise Exception("""cconly must be `None` or one of {0,1}.""")
        self._cards[0].set_value("cconly", value)

    @property
    def ctype(self) -> int:
        """Get or set the Contact type :
        EQ. - 1:	Node to node contact based on constraints on the scalar potential.See Remark 1.
        EQ.0 : Node to node penalty based contact on the scalar potential.
        EQ.1 : Discrete mortar penalty contact on the scalar potential.
        EQ.2 : Continuous mortar penalty contact on the scalar potential and the vector potential(when active).
        """ # nopep8
        return self._cards[0].get_value("ctype")

    @ctype.setter
    def ctype(self, value: int) -> None:
        """Set the ctype property."""
        if value not in [0, -1, 1, 2, None]:
            raise Exception("""ctype must be `None` or one of {0,-1,1,2}.""")
        self._cards[0].set_value("ctype", value)

    @property
    def cotype(self) -> int:
        """Get or set the Type of EM contact. If *EM_CONTACT is not defined, the solver will look for global contact options in *EM_CONTROL_CONTACT.
        EQ.0: Contact type 0.
        EQ.1: Contact type 1.
        """ # nopep8
        return self._cards[0].get_value("cotype")

    @cotype.setter
    def cotype(self, value: int) -> None:
        """Set the cotype property."""
        if value not in [0, 1, None]:
            raise Exception("""cotype must be `None` or one of {0,1}.""")
        self._cards[0].set_value("cotype", value)

    @property
    def eps1(self) -> float:
        """Get or set the Global contact coefficients used if the equivalent fields in *EM_CONTACT are empty.
        """ # nopep8
        return self._cards[0].get_value("eps1")

    @eps1.setter
    def eps1(self, value: float) -> None:
        """Set the eps1 property."""
        self._cards[0].set_value("eps1", value)

    @property
    def eps2(self) -> float:
        """Get or set the Global contact coefficients used if the equivalent fields in *EM_CONTACT are empty.
        """ # nopep8
        return self._cards[0].get_value("eps2")

    @eps2.setter
    def eps2(self, value: float) -> None:
        """Set the eps2 property."""
        self._cards[0].set_value("eps2", value)

    @property
    def eps3(self) -> float:
        """Get or set the Global contact coefficients used if the equivalent fields in *EM_CONTACT are empty.
        """ # nopep8
        return self._cards[0].get_value("eps3")

    @eps3.setter
    def eps3(self, value: float) -> None:
        """Set the eps3 property."""
        self._cards[0].set_value("eps3", value)

    @property
    def d0(self) -> typing.Optional[float]:
        """Get or set the Global contact condition 3 value when COTYPE = 1
        """ # nopep8
        return self._cards[0].get_value("d0")

    @d0.setter
    def d0(self, value: float) -> None:
        """Set the d0 property."""
        self._cards[0].set_value("d0", value)

