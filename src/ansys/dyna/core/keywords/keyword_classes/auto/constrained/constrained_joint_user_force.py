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

"""Module providing the ConstrainedJointUserForce class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONSTRAINEDJOINTUSERFORCE_CARD0 = (
    FieldSchema("fid", int, 0, 10, None),
    FieldSchema("jid", int, 10, 10, None),
    FieldSchema("nhisv", int, 20, 10, 0),
)

_CONSTRAINEDJOINTUSERFORCE_CARD1 = (
    FieldSchema("const1", int, 0, 10, None),
    FieldSchema("const2", int, 10, 10, None),
    FieldSchema("const3", int, 20, 10, None),
    FieldSchema("const4", int, 30, 10, None),
    FieldSchema("const5", int, 40, 10, None),
    FieldSchema("const6", int, 50, 10, None),
    FieldSchema("const7", int, 60, 10, None),
    FieldSchema("const8", int, 70, 10, None),
)

class ConstrainedJointUserForce(KeywordBase):
    """DYNA CONSTRAINED_JOINT_USER_FORCE keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "JOINT_USER_FORCE"

    def __init__(self, **kwargs):
        """Initialize the ConstrainedJointUserForce class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDJOINTUSERFORCE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDJOINTUSERFORCE_CARD1,
                **kwargs,
            ),        ]
    @property
    def fid(self) -> typing.Optional[int]:
        """Get or set the Joint user force ID.
        """ # nopep8
        return self._cards[0].get_value("fid")

    @fid.setter
    def fid(self, value: int) -> None:
        """Set the fid property."""
        self._cards[0].set_value("fid", value)

    @property
    def jid(self) -> typing.Optional[int]:
        """Get or set the Joint ID for which this user force input applies.
        """ # nopep8
        return self._cards[0].get_value("jid")

    @jid.setter
    def jid(self, value: int) -> None:
        """Set the jid property."""
        self._cards[0].set_value("jid", value)

    @property
    def nhisv(self) -> int:
        """Get or set the Number of history variables required for this definition.  An array NHISV long is allocated and passed into the user subroutine.  This array is updated in the user subroutine.
        """ # nopep8
        return self._cards[0].get_value("nhisv")

    @nhisv.setter
    def nhisv(self, value: int) -> None:
        """Set the nhisv property."""
        self._cards[0].set_value("nhisv", value)

    @property
    def const1(self) -> typing.Optional[int]:
        """Get or set the A constant which is passed into the user subroutine.
        """ # nopep8
        return self._cards[1].get_value("const1")

    @const1.setter
    def const1(self, value: int) -> None:
        """Set the const1 property."""
        self._cards[1].set_value("const1", value)

    @property
    def const2(self) -> typing.Optional[int]:
        """Get or set the A constant which is passed into the user subroutine.
        """ # nopep8
        return self._cards[1].get_value("const2")

    @const2.setter
    def const2(self, value: int) -> None:
        """Set the const2 property."""
        self._cards[1].set_value("const2", value)

    @property
    def const3(self) -> typing.Optional[int]:
        """Get or set the A constant which is passed into the user subroutine.
        """ # nopep8
        return self._cards[1].get_value("const3")

    @const3.setter
    def const3(self, value: int) -> None:
        """Set the const3 property."""
        self._cards[1].set_value("const3", value)

    @property
    def const4(self) -> typing.Optional[int]:
        """Get or set the A constant which is passed into the user subroutine.
        """ # nopep8
        return self._cards[1].get_value("const4")

    @const4.setter
    def const4(self, value: int) -> None:
        """Set the const4 property."""
        self._cards[1].set_value("const4", value)

    @property
    def const5(self) -> typing.Optional[int]:
        """Get or set the A constant which is passed into the user subroutine.
        """ # nopep8
        return self._cards[1].get_value("const5")

    @const5.setter
    def const5(self, value: int) -> None:
        """Set the const5 property."""
        self._cards[1].set_value("const5", value)

    @property
    def const6(self) -> typing.Optional[int]:
        """Get or set the A constant which is passed into the user subroutine.
        """ # nopep8
        return self._cards[1].get_value("const6")

    @const6.setter
    def const6(self, value: int) -> None:
        """Set the const6 property."""
        self._cards[1].set_value("const6", value)

    @property
    def const7(self) -> typing.Optional[int]:
        """Get or set the A constant which is passed into the user subroutine.
        """ # nopep8
        return self._cards[1].get_value("const7")

    @const7.setter
    def const7(self, value: int) -> None:
        """Set the const7 property."""
        self._cards[1].set_value("const7", value)

    @property
    def const8(self) -> typing.Optional[int]:
        """Get or set the A constant which is passed into the user subroutine.
        """ # nopep8
        return self._cards[1].get_value("const8")

    @const8.setter
    def const8(self, value: int) -> None:
        """Set the const8 property."""
        self._cards[1].set_value("const8", value)

