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

"""Module providing the ElementShellThicknessMcidOffset class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ELEMENTSHELLTHICKNESSMCIDOFFSET_CARD0 = (
    FieldSchema("eid", int, 0, 8, None),
    FieldSchema("pid", int, 8, 8, None),
    FieldSchema("n1", int, 16, 8, None),
    FieldSchema("n2", int, 24, 8, None),
    FieldSchema("n3", int, 32, 8, None),
    FieldSchema("n4", int, 40, 8, None),
    FieldSchema("n5", int, 48, 8, None),
    FieldSchema("n6", int, 56, 8, None),
    FieldSchema("n7", int, 64, 8, None),
    FieldSchema("n8", int, 72, 8, None),
)

_ELEMENTSHELLTHICKNESSMCIDOFFSET_CARD1 = (
    FieldSchema("thic1", float, 0, 16, 0.0),
    FieldSchema("thic2", float, 16, 16, 0.0),
    FieldSchema("thic3", float, 32, 16, 0.0),
    FieldSchema("thic4", float, 48, 16, 0.0),
    FieldSchema("mcid", int, 64, 16, 0),
)

_ELEMENTSHELLTHICKNESSMCIDOFFSET_CARD2 = (
    FieldSchema("thic5", float, 0, 16, 0.0),
    FieldSchema("thic6", float, 16, 16, 0.0),
    FieldSchema("thic7", float, 32, 16, 0.0),
    FieldSchema("thic8", float, 48, 16, 0.0),
)

_ELEMENTSHELLTHICKNESSMCIDOFFSET_CARD3 = (
    FieldSchema("offset", float, 0, 16, 0.0),
)

class ElementShellThicknessMcidOffset(KeywordBase):
    """DYNA ELEMENT_SHELL_THICKNESS_MCID_OFFSET keyword"""

    keyword = "ELEMENT"
    subkeyword = "SHELL_THICKNESS_MCID_OFFSET"

    def __init__(self, **kwargs):
        """Initialize the ElementShellThicknessMcidOffset class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ELEMENTSHELLTHICKNESSMCIDOFFSET_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSHELLTHICKNESSMCIDOFFSET_CARD1,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSHELLTHICKNESSMCIDOFFSET_CARD2,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ELEMENTSHELLTHICKNESSMCIDOFFSET_CARD3,
                **kwargs,
            ),        ]
    @property
    def eid(self) -> typing.Optional[int]:
        """Get or set the Element ID. A unique number has to be used.
        """ # nopep8
        return self._cards[0].get_value("eid")

    @eid.setter
    def eid(self, value: int) -> None:
        """Set the eid property."""
        self._cards[0].set_value("eid", value)

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
    def n1(self) -> typing.Optional[int]:
        """Get or set the Nodal point 1.
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[0].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Nodal point 2.
        """ # nopep8
        return self._cards[0].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[0].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Nodal point 3.
        """ # nopep8
        return self._cards[0].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        """Set the n3 property."""
        self._cards[0].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Nodal point 4.
        """ # nopep8
        return self._cards[0].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        """Set the n4 property."""
        self._cards[0].set_value("n4", value)

    @property
    def n5(self) -> typing.Optional[int]:
        """Get or set the Mid side nodal point 5.
        """ # nopep8
        return self._cards[0].get_value("n5")

    @n5.setter
    def n5(self, value: int) -> None:
        """Set the n5 property."""
        self._cards[0].set_value("n5", value)

    @property
    def n6(self) -> typing.Optional[int]:
        """Get or set the Mid side nodal point 6.
        """ # nopep8
        return self._cards[0].get_value("n6")

    @n6.setter
    def n6(self, value: int) -> None:
        """Set the n6 property."""
        self._cards[0].set_value("n6", value)

    @property
    def n7(self) -> typing.Optional[int]:
        """Get or set the Mid side nodal point 7.
        """ # nopep8
        return self._cards[0].get_value("n7")

    @n7.setter
    def n7(self, value: int) -> None:
        """Set the n7 property."""
        self._cards[0].set_value("n7", value)

    @property
    def n8(self) -> typing.Optional[int]:
        """Get or set the Mid side nodal point 8.
        """ # nopep8
        return self._cards[0].get_value("n8")

    @n8.setter
    def n8(self, value: int) -> None:
        """Set the n8 property."""
        self._cards[0].set_value("n8", value)

    @property
    def thic1(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[1].get_value("thic1")

    @thic1.setter
    def thic1(self, value: float) -> None:
        """Set the thic1 property."""
        self._cards[1].set_value("thic1", value)

    @property
    def thic2(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[1].get_value("thic2")

    @thic2.setter
    def thic2(self, value: float) -> None:
        """Set the thic2 property."""
        self._cards[1].set_value("thic2", value)

    @property
    def thic3(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[1].get_value("thic3")

    @thic3.setter
    def thic3(self, value: float) -> None:
        """Set the thic3 property."""
        self._cards[1].set_value("thic3", value)

    @property
    def thic4(self) -> float:
        """Get or set the 
        """ # nopep8
        return self._cards[1].get_value("thic4")

    @thic4.setter
    def thic4(self, value: float) -> None:
        """Set the thic4 property."""
        self._cards[1].set_value("thic4", value)

    @property
    def mcid(self) -> int:
        """Get or set the Material coordinate system ID. The angle BETA ia determined by the projection of the X axis of the local system onto the shell element.
        """ # nopep8
        return self._cards[1].get_value("mcid")

    @mcid.setter
    def mcid(self, value: int) -> None:
        """Set the mcid property."""
        self._cards[1].set_value("mcid", value)

    @property
    def thic5(self) -> float:
        """Get or set the Shell thickness at node 5.
        """ # nopep8
        return self._cards[2].get_value("thic5")

    @thic5.setter
    def thic5(self, value: float) -> None:
        """Set the thic5 property."""
        self._cards[2].set_value("thic5", value)

    @property
    def thic6(self) -> float:
        """Get or set the Shell thickness at node 6.
        """ # nopep8
        return self._cards[2].get_value("thic6")

    @thic6.setter
    def thic6(self, value: float) -> None:
        """Set the thic6 property."""
        self._cards[2].set_value("thic6", value)

    @property
    def thic7(self) -> float:
        """Get or set the Shell thickness at node 7.
        """ # nopep8
        return self._cards[2].get_value("thic7")

    @thic7.setter
    def thic7(self, value: float) -> None:
        """Set the thic7 property."""
        self._cards[2].set_value("thic7", value)

    @property
    def thic8(self) -> float:
        """Get or set the Shell thickness at node 8.
        """ # nopep8
        return self._cards[2].get_value("thic8")

    @thic8.setter
    def thic8(self, value: float) -> None:
        """Set the thic8 property."""
        self._cards[2].set_value("thic8", value)

    @property
    def offset(self) -> float:
        """Get or set the The offset distance from the nodal points to the reference surface of the shell in the direction of the normal vector to the shell
        """ # nopep8
        return self._cards[3].get_value("offset")

    @offset.setter
    def offset(self, value: float) -> None:
        """Set the offset property."""
        self._cards[3].set_value("offset", value)

