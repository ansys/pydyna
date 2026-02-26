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

"""Module providing the RigidDeformableControl class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_RIGIDDEFORMABLECONTROL_CARD0 = (
    FieldSchema("nrbf", int, 0, 10, 0),
    FieldSchema("ncsf", int, 10, 10, 0),
    FieldSchema("rwf", int, 20, 10, 0),
    FieldSchema("dtmax", float, 30, 10, None),
)

class RigidDeformableControl(KeywordBase):
    """DYNA RIGID_DEFORMABLE_CONTROL keyword"""

    keyword = "RIGID"
    subkeyword = "DEFORMABLE_CONTROL"

    def __init__(self, **kwargs):
        """Initialize the RigidDeformableControl class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _RIGIDDEFORMABLECONTROL_CARD0,
                **kwargs,
            ),        ]
    @property
    def nrbf(self) -> int:
        """Get or set the Flag to delete or activate nodal rigid bodies. If nodal rigid bodies or generalized, weld definitions are active in the deformable bodies that are switched to rigid, then the definitions should be deleted to avoid instabilities:
        EQ.0: no change,
        EQ.1: delete,
        EQ.2: activate.
        """ # nopep8
        return self._cards[0].get_value("nrbf")

    @nrbf.setter
    def nrbf(self, value: int) -> None:
        """Set the nrbf property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""nrbf must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("nrbf", value)

    @property
    def ncsf(self) -> int:
        """Get or set the Flag to delete or activate nodal constraint set. If nodal constraint/spotweld definitions are active in the deformable bodies that are switched to rigid, then the definitions should be deleted to avoid instabilities:
        EQ.0: no change,
        EQ.1: delete,
        EQ.2: activate.
        """ # nopep8
        return self._cards[0].get_value("ncsf")

    @ncsf.setter
    def ncsf(self, value: int) -> None:
        """Set the ncsf property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""ncsf must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("ncsf", value)

    @property
    def rwf(self) -> int:
        """Get or set the Flag to delete or activate rigid walls:
        EQ.0: no change,
        EQ.1: delete,
        EQ.2: activate.
        """ # nopep8
        return self._cards[0].get_value("rwf")

    @rwf.setter
    def rwf(self, value: int) -> None:
        """Set the rwf property."""
        if value not in [0, 1, 2, None]:
            raise Exception("""rwf must be `None` or one of {0,1,2}.""")
        self._cards[0].set_value("rwf", value)

    @property
    def dtmax(self) -> typing.Optional[float]:
        """Get or set the Maximum permitted time step size after restart.
        """ # nopep8
        return self._cards[0].get_value("dtmax")

    @dtmax.setter
    def dtmax(self, value: float) -> None:
        """Set the dtmax property."""
        self._cards[0].set_value("dtmax", value)

