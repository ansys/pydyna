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

"""Module providing the ChangeRigidBodyConstraint class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_CHANGERIGIDBODYCONSTRAINT_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("tc", int, 10, 10, 0),
    FieldSchema("rc", int, 20, 10, 0),
)

class ChangeRigidBodyConstraint(KeywordBase):
    """DYNA CHANGE_RIGID_BODY_CONSTRAINT keyword"""

    keyword = "CHANGE"
    subkeyword = "RIGID_BODY_CONSTRAINT"
    _link_fields = {
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the ChangeRigidBodyConstraint class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CHANGERIGIDBODYCONSTRAINT_CARD0,
                **kwargs,
            ),        ]
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
    def tc(self) -> int:
        """Get or set the Translational constraint:
        EQ.0: no constraints,
        EQ.1: constrained x displacement,
        EQ.2: constrained y displacement,
        EQ.3: constrained z displacement,
        EQ.4: constrained x and y displacements,
        EQ.5: constrained y and z displacements,
        EQ.6: constrained z and x displacements,
        EQ.7: constrained x, y, and z displacements.
        """ # nopep8
        return self._cards[0].get_value("tc")

    @tc.setter
    def tc(self, value: int) -> None:
        """Set the tc property."""
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, None]:
            raise Exception("""tc must be `None` or one of {0,1,2,3,4,5,6,7}.""")
        self._cards[0].set_value("tc", value)

    @property
    def rc(self) -> int:
        """Get or set the Rotational constraint:
        EQ.0: no constraints,
        EQ.1: constrained x rotation,
        EQ.2: constrained y rotation,
        EQ.3: constrained z rotation,
        EQ.4: constrained x and y rotations,
        EQ.5: constrained y and z rotations,
        EQ.6: constrained z and x rotations,
        EQ.7: constrained x, y, and z rotations.
        """ # nopep8
        return self._cards[0].get_value("rc")

    @rc.setter
    def rc(self, value: int) -> None:
        """Set the rc property."""
        if value not in [0, 1, 2, 3, 4, 5, 6, 7, None]:
            raise Exception("""rc must be `None` or one of {0,1,2,3,4,5,6,7}.""")
        self._cards[0].set_value("rc", value)

    @property
    def pid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

