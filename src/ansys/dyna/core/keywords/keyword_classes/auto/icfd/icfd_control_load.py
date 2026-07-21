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

"""Module providing the IcfdControlLoad class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDCONTROLLOAD_CARD0 = (
    FieldSchema("abl", int, 0, 10, 1),
    FieldSchema("dir", int, 10, 10, 0),
    FieldSchema("val", float, 20, 10, None),
)

class IcfdControlLoad(KeywordBase):
    """DYNA ICFD_CONTROL_LOAD keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_LOAD"

    def __init__(self, **kwargs):
        """Initialize the IcfdControlLoad class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLLOAD_CARD0,
                **kwargs,
            ),
        ]
    @property
    def abl(self) -> int:
        """Get or set the Flag to reset the body load in the fluid:
        EQ.0:	The body load provided in * LOAD_?BODY is reset to zero only for the fluid analysis. In addition, by specifying DIR and VAL, the body load on the fluid can be specified.
        EQ.1 : Do not reset the body load provided in * LOAD_?BODY.
        """ # nopep8
        return self._cards[0].get_value("abl")

    @abl.setter
    def abl(self, value: int) -> None:
        """Set the abl property."""
        if value not in [1, 0, None]:
            raise Exception("""abl must be `None` or one of {1,0}.""")
        self._cards[0].set_value("abl", value)

    @property
    def dir(self) -> int:
        """Get or set the Global direction in which gravity is applied to the fluid. See Remark 1. DIR is available if ABL = 0.
        EQ.0:	No body load specified for the fluid
        EQ.1:	X - axis
        EQ.2 : Y - axis
        EQ.3 : Z - axis
        """ # nopep8
        return self._cards[0].get_value("dir")

    @dir.setter
    def dir(self, value: int) -> None:
        """Set the dir property."""
        if value not in [0, 1, 2, 3, None]:
            raise Exception("""dir must be `None` or one of {0,1,2,3}.""")
        self._cards[0].set_value("dir", value)

    @property
    def val(self) -> typing.Optional[float]:
        """Get or set the Constant value of gravity (for example, g=9.81 m/s^2 ). VAL is available if ABL = 0.
        """ # nopep8
        return self._cards[0].get_value("val")

    @val.setter
    def val(self, value: float) -> None:
        """Set the val property."""
        self._cards[0].set_value("val", value)

