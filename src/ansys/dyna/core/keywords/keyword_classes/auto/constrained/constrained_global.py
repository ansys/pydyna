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

"""Module providing the ConstrainedGlobal class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONSTRAINEDGLOBAL_CARD0 = (
    FieldSchema("tc", int, 0, 10, 0),
    FieldSchema("rc", int, 10, 10, 0),
    FieldSchema("dir", int, 20, 10, 0),
    FieldSchema("x", float, 30, 10, 0.0),
    FieldSchema("y", float, 40, 10, 0.0),
    FieldSchema("z", float, 50, 10, 0.0),
    FieldSchema("tol", float, 60, 10, 0.0),
)

class ConstrainedGlobal(KeywordBase):
    """DYNA CONSTRAINED_GLOBAL keyword"""

    keyword = "CONSTRAINED"
    subkeyword = "GLOBAL"

    def __init__(self, **kwargs):
        """Initialize the ConstrainedGlobal class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONSTRAINEDGLOBAL_CARD0,
                **kwargs,
            ),        ]
    @property
    def tc(self) -> int:
        """Get or set the Translational constraint:
        EQ.0: no constraints added,
        EQ.1: constrained x-translation,
        EQ.2: constrained y-translation,
        EQ.3: constrained z-translation,
        EQ.4: constrained x and y translations,
        EQ.5: constrained y and z translations,
        EQ.6: constrained x and z translations,
        EQ.7: constrained x, y, and z translations.
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
        EQ.0: no constraints added,
        EQ.1: constrained x-rotation,
        EQ.2: constrained y-rotation,
        EQ.3: constrained z-rotation,
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
    def dir(self) -> int:
        """Get or set the Direction of normal
        EQ.0: no constraints added,
        EQ.1: global x,
        EQ.2: global y,
        EQ.3: global z.
        """ # nopep8
        return self._cards[0].get_value("dir")

    @dir.setter
    def dir(self, value: int) -> None:
        """Set the dir property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""dir must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("dir", value)

    @property
    def x(self) -> float:
        """Get or set the x-offset coordinate.
        """ # nopep8
        return self._cards[0].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        """Set the x property."""
        self._cards[0].set_value("x", value)

    @property
    def y(self) -> float:
        """Get or set the y-offset coordinate.
        """ # nopep8
        return self._cards[0].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[0].set_value("y", value)

    @property
    def z(self) -> float:
        """Get or set the z-offset coordinate.
        """ # nopep8
        return self._cards[0].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        """Set the z property."""
        self._cards[0].set_value("z", value)

    @property
    def tol(self) -> float:
        """Get or set the User-defined tolerance in length units. If non-zero, the internal mesh-size dependent tolerance gets replaced by this value.
        """ # nopep8
        return self._cards[0].get_value("tol")

    @tol.setter
    def tol(self, value: float) -> None:
        """Set the tol property."""
        self._cards[0].set_value("tol", value)

