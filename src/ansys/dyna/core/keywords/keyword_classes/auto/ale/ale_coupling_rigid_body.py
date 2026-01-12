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

"""Module providing the AleCouplingRigidBody class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ALECOUPLINGRIGIDBODY_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("esid", int, 10, 10, None),
)

_ALECOUPLINGRIGIDBODY_CARD1 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("idtype", int, 10, 10, 0),
    FieldSchema("ictype", int, 20, 10, 1),
    FieldSchema("iexcle", int, 30, 10, None),
)

class AleCouplingRigidBody(KeywordBase):
    """DYNA ALE_COUPLING_RIGID_BODY keyword"""

    keyword = "ALE"
    subkeyword = "COUPLING_RIGID_BODY"

    def __init__(self, **kwargs):
        """Initialize the AleCouplingRigidBody class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ALECOUPLINGRIGIDBODY_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _ALECOUPLINGRIGIDBODY_CARD1,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Rigid body part ID.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def esid(self) -> typing.Optional[int]:
        """Get or set the Node set ID defining ALE boundary nodes to follow rigidRigid body motion.
        """ # nopep8
        return self._cards[0].get_value("esid")

    @esid.setter
    def esid(self, value: int) -> None:
        """Set the esid property."""
        self._cards[0].set_value("esid", value)

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Set ID defining a part, part set or segment set ID of the ALE coupling interface.
        """ # nopep8
        return self._cards[1].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[1].set_value("id", value)

    @property
    def idtype(self) -> int:
        """Get or set the Type of set ID:
        EQ.0:	Partpart set ID(PSID)).
        EQ.1:	Partpart ID(PID)).
        EQ.2:	Segmentsegment set ID(SGSID)).
        """ # nopep8
        return self._cards[1].get_value("idtype")

    @idtype.setter
    def idtype(self, value: int) -> None:
        """Set the idtype property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""idtype must be `None` or one of {0,1,2}.""")
        self._cards[1].set_value("idtype", value)

    @property
    def ictype(self) -> int:
        """Get or set the Constraint type:EQ.1:	No flow through all directions.
        EQ.2 : No flow through normal direction. (slip condition)
        """ # nopep8
        return self._cards[1].get_value("ictype")

    @ictype.setter
    def ictype(self, value: int) -> None:
        """Set the ictype property."""
        if value not in [1, 2, None]:
            raise Exception("""ictype must be `None` or one of {1,2}.""")
        self._cards[1].set_value("ictype", value)

    @property
    def iexcle(self) -> typing.Optional[int]:
        """Get or set the Segment Set ID to be excluded from applying ALE essential boundary condition. For example, inlet/outlet segments.
        """ # nopep8
        return self._cards[1].get_value("iexcle")

    @iexcle.setter
    def iexcle(self, value: int) -> None:
        """Set the iexcle property."""
        self._cards[1].set_value("iexcle", value)

