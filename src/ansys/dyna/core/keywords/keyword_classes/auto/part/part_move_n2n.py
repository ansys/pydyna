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

"""Module providing the PartMoveN2N class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_PARTMOVEN2N_CARD0 = (
    FieldSchema("pid", int, 0, 8, None),
    FieldSchema("n1", int, 8, 8, None),
    FieldSchema("n1", int, 16, 8, None),
    FieldSchema("ifset", int, 24, 8, 0),
    FieldSchema("donow", int, 36, 8, 0),
)

class PartMoveN2N(KeywordBase):
    """DYNA PART_MOVE_N2N keyword"""

    keyword = "PART"
    subkeyword = "MOVE_N2N"
    _link_fields = {
        "n1": LinkType.NODE,
        "n1": LinkType.NODE,
    }

    def __init__(self, **kwargs):
        """Initialize the PartMoveN2N class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _PARTMOVEN2N_CARD0,
                **kwargs,
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part id or Part Set id
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Node ID giving the starting point for the vector that defines the motion
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[0].set_value("n1", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Node ID giving the ending point for the vector that defines the motion
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[0].set_value("n1", value)

    @property
    def ifset(self) -> int:
        """Get or set the Indicate if a part set ID is used in the PID definition
        EQ.0: Part ID is used;
        EQ.1: Part set ID is used
        """ # nopep8
        return self._cards[0].get_value("ifset")

    @ifset.setter
    def ifset(self, value: int) -> None:
        """Set the ifset property."""
        if value not in [0, 1, None]:
            raise Exception("""ifset must be `None` or one of {0,1}.""")
        self._cards[0].set_value("ifset", value)

    @property
    def donow(self) -> int:
        """Get or set the Flag specifying when the part is moved during keyword processing:
        EQ.0 : The part is moved after all input cards have been processes.Thus the movement is based on the transformed coordinates of N1 and N2.
        EQ.1 : The part is moved based on the coordinates at the time this keyword is read.Therefore, nodes N1 and N2 must be defined before *PART_MOVE_N2N is processed.
        Note that the difference between DONOW = 0 and 1 can be significant if N1 and N2 are affected by * INCLUDE_TRANSFORM or *NODE_TRANSFORM
        """ # nopep8
        return self._cards[0].get_value("donow")

    @donow.setter
    def donow(self, value: int) -> None:
        """Set the donow property."""
        if value not in [0, 1, None]:
            raise Exception("""donow must be `None` or one of {0,1}.""")
        self._cards[0].set_value("donow", value)

    @property
    def n1_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n1."""
        return self._get_link_by_attr("NODE", "nid", self.n1, "parts")

    @property
    def n1_link(self) -> typing.Optional[KeywordBase]:
        """Get the NODE keyword containing the given n1."""
        return self._get_link_by_attr("NODE", "nid", self.n1, "parts")

