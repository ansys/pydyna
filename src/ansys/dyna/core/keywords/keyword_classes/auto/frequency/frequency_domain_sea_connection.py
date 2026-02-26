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

"""Module providing the FrequencyDomainSeaConnection class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_FREQUENCYDOMAINSEACONNECTION_CARD0 = (
    FieldSchema("conid", int, 0, 10, None),
    FieldSchema("ctype", int, 10, 10, 1),
    FieldSchema("nsub", int, 20, 10, None),
    FieldSchema("ibeam", int, 30, 10, 0),
)

_FREQUENCYDOMAINSEACONNECTION_CARD1 = (
    FieldSchema("sub1", int, 0, 10, None),
    FieldSchema("sub2", int, 10, 10, None),
    FieldSchema("sub3", int, 20, 10, None),
    FieldSchema("sub4", int, 30, 10, None),
    FieldSchema("sub5", int, 40, 10, None),
    FieldSchema("sub6", int, 50, 10, None),
    FieldSchema("sub7", int, 60, 10, None),
    FieldSchema("sub8", int, 70, 10, None),
)

_FREQUENCYDOMAINSEACONNECTION_CARD2 = (
    FieldSchema("ang1", float, 0, 10, None),
    FieldSchema("ang2", float, 10, 10, None),
    FieldSchema("ang3", float, 20, 10, None),
    FieldSchema("ang4", float, 30, 10, None),
    FieldSchema("ang5", float, 40, 10, None),
    FieldSchema("ang6", float, 50, 10, None),
    FieldSchema("ang7", float, 60, 10, None),
    FieldSchema("ang8", float, 70, 10, None),
)

_FREQUENCYDOMAINSEACONNECTION_CARD3 = (
    FieldSchema("length", float, 0, 10, 0.0),
)

_FREQUENCYDOMAINSEACONNECTION_CARD4 = (
    FieldSchema("absorb", float, 0, 10, 0.0),
    FieldSchema("thick", float, 10, 10, 0.0),
)

class FrequencyDomainSeaConnection(KeywordBase):
    """DYNA FREQUENCY_DOMAIN_SEA_CONNECTION keyword"""

    keyword = "FREQUENCY"
    subkeyword = "DOMAIN_SEA_CONNECTION"

    def __init__(self, **kwargs):
        """Initialize the FrequencyDomainSeaConnection class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINSEACONNECTION_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINSEACONNECTION_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINSEACONNECTION_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINSEACONNECTION_CARD3,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _FREQUENCYDOMAINSEACONNECTION_CARD4,
                **kwargs,
            ),        ]
    @property
    def conid(self) -> typing.Optional[int]:
        """Get or set the Connection ID.
        """ # nopep8
        return self._cards[0].get_value("conid")

    @conid.setter
    def conid(self, value: int) -> None:
        """Set the conid property."""
        self._cards[0].set_value("conid", value)

    @property
    def ctype(self) -> int:
        """Get or set the Type of connection
        EQ.1: plate-plate
        EQ.2: plate-cavity
        EQ.3: plate-cavity-cavity
        EQ.4: plate-beam
        .
        """ # nopep8
        return self._cards[0].get_value("ctype")

    @ctype.setter
    def ctype(self, value: int) -> None:
        """Set the ctype property."""
        if value not in [1, 2, 3, 4, None]:
            raise Exception("""ctype must be `None` or one of {1,2,3,4}.""")
        self._cards[0].set_value("ctype", value)

    @property
    def nsub(self) -> typing.Optional[int]:
        """Get or set the Number of subsystems in this connection.
        """ # nopep8
        return self._cards[0].get_value("nsub")

    @nsub.setter
    def nsub(self, value: int) -> None:
        """Set the nsub property."""
        self._cards[0].set_value("nsub", value)

    @property
    def ibeam(self) -> int:
        """Get or set the Flag for plate connected to plate
        EQ.0:	plate - plate connection.
        EQ.1 : plate - plate - beam connection.
        """ # nopep8
        return self._cards[0].get_value("ibeam")

    @ibeam.setter
    def ibeam(self, value: int) -> None:
        """Set the ibeam property."""
        if value not in [0, 1, None]:
            raise Exception("""ibeam must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ibeam", value)

    @property
    def sub1(self) -> typing.Optional[int]:
        """Get or set the ID of the ith subsystem.
        """ # nopep8
        return self._cards[1].get_value("sub1")

    @sub1.setter
    def sub1(self, value: int) -> None:
        """Set the sub1 property."""
        self._cards[1].set_value("sub1", value)

    @property
    def sub2(self) -> typing.Optional[int]:
        """Get or set the ID of the ith subsystem.
        """ # nopep8
        return self._cards[1].get_value("sub2")

    @sub2.setter
    def sub2(self, value: int) -> None:
        """Set the sub2 property."""
        self._cards[1].set_value("sub2", value)

    @property
    def sub3(self) -> typing.Optional[int]:
        """Get or set the ID of the ith subsystem.
        """ # nopep8
        return self._cards[1].get_value("sub3")

    @sub3.setter
    def sub3(self, value: int) -> None:
        """Set the sub3 property."""
        self._cards[1].set_value("sub3", value)

    @property
    def sub4(self) -> typing.Optional[int]:
        """Get or set the ID of the ith subsystem.
        """ # nopep8
        return self._cards[1].get_value("sub4")

    @sub4.setter
    def sub4(self, value: int) -> None:
        """Set the sub4 property."""
        self._cards[1].set_value("sub4", value)

    @property
    def sub5(self) -> typing.Optional[int]:
        """Get or set the ID of the ith subsystem.
        """ # nopep8
        return self._cards[1].get_value("sub5")

    @sub5.setter
    def sub5(self, value: int) -> None:
        """Set the sub5 property."""
        self._cards[1].set_value("sub5", value)

    @property
    def sub6(self) -> typing.Optional[int]:
        """Get or set the ID of the ith subsystem.
        """ # nopep8
        return self._cards[1].get_value("sub6")

    @sub6.setter
    def sub6(self, value: int) -> None:
        """Set the sub6 property."""
        self._cards[1].set_value("sub6", value)

    @property
    def sub7(self) -> typing.Optional[int]:
        """Get or set the ID of the ith subsystem.
        """ # nopep8
        return self._cards[1].get_value("sub7")

    @sub7.setter
    def sub7(self, value: int) -> None:
        """Set the sub7 property."""
        self._cards[1].set_value("sub7", value)

    @property
    def sub8(self) -> typing.Optional[int]:
        """Get or set the ID of the ith subsystem.
        """ # nopep8
        return self._cards[1].get_value("sub8")

    @sub8.setter
    def sub8(self, value: int) -> None:
        """Set the sub8 property."""
        self._cards[1].set_value("sub8", value)

    @property
    def ang1(self) -> typing.Optional[float]:
        """Get or set the Connection angle of the plate i.
        """ # nopep8
        return self._cards[2].get_value("ang1")

    @ang1.setter
    def ang1(self, value: float) -> None:
        """Set the ang1 property."""
        self._cards[2].set_value("ang1", value)

    @property
    def ang2(self) -> typing.Optional[float]:
        """Get or set the Connection angle of the plate i.
        """ # nopep8
        return self._cards[2].get_value("ang2")

    @ang2.setter
    def ang2(self, value: float) -> None:
        """Set the ang2 property."""
        self._cards[2].set_value("ang2", value)

    @property
    def ang3(self) -> typing.Optional[float]:
        """Get or set the Connection angle of the plate i.
        """ # nopep8
        return self._cards[2].get_value("ang3")

    @ang3.setter
    def ang3(self, value: float) -> None:
        """Set the ang3 property."""
        self._cards[2].set_value("ang3", value)

    @property
    def ang4(self) -> typing.Optional[float]:
        """Get or set the Connection angle of the plate i.
        """ # nopep8
        return self._cards[2].get_value("ang4")

    @ang4.setter
    def ang4(self, value: float) -> None:
        """Set the ang4 property."""
        self._cards[2].set_value("ang4", value)

    @property
    def ang5(self) -> typing.Optional[float]:
        """Get or set the Connection angle of the plate i.
        """ # nopep8
        return self._cards[2].get_value("ang5")

    @ang5.setter
    def ang5(self, value: float) -> None:
        """Set the ang5 property."""
        self._cards[2].set_value("ang5", value)

    @property
    def ang6(self) -> typing.Optional[float]:
        """Get or set the Connection angle of the plate i.
        """ # nopep8
        return self._cards[2].get_value("ang6")

    @ang6.setter
    def ang6(self, value: float) -> None:
        """Set the ang6 property."""
        self._cards[2].set_value("ang6", value)

    @property
    def ang7(self) -> typing.Optional[float]:
        """Get or set the Connection angle of the plate i.
        """ # nopep8
        return self._cards[2].get_value("ang7")

    @ang7.setter
    def ang7(self, value: float) -> None:
        """Set the ang7 property."""
        self._cards[2].set_value("ang7", value)

    @property
    def ang8(self) -> typing.Optional[float]:
        """Get or set the Connection angle of the plate i.
        """ # nopep8
        return self._cards[2].get_value("ang8")

    @ang8.setter
    def ang8(self, value: float) -> None:
        """Set the ang8 property."""
        self._cards[2].set_value("ang8", value)

    @property
    def length(self) -> float:
        """Get or set the Length of the edge in connection.
        """ # nopep8
        return self._cards[3].get_value("length")

    @length.setter
    def length(self, value: float) -> None:
        """Set the length property."""
        self._cards[3].set_value("length", value)

    @property
    def absorb(self) -> float:
        """Get or set the Absorption coefficient.
        """ # nopep8
        return self._cards[4].get_value("absorb")

    @absorb.setter
    def absorb(self, value: float) -> None:
        """Set the absorb property."""
        self._cards[4].set_value("absorb", value)

    @property
    def thick(self) -> float:
        """Get or set the Thickness of the plate.
        """ # nopep8
        return self._cards[4].get_value("thick")

    @thick.setter
    def thick(self, value: float) -> None:
        """Set the thick property."""
        self._cards[4].set_value("thick", value)

