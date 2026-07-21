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

"""Module providing the DualcesePorousSpecifyJump class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_DUALCESEPOROUSSPECIFYJUMP_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("thicknss", float, 10, 10, None),
    FieldSchema("eqid", float, 20, 10, None),
)

class DualcesePorousSpecifyJump(KeywordBase):
    """DYNA DUALCESE_POROUS_SPECIFY_JUMP keyword"""

    keyword = "DUALCESE"
    subkeyword = "POROUS_SPECIFY_JUMP"
    _link_fields = {
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the DualcesePorousSpecifyJump class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESEPOROUSSPECIFYJUMP_CARD0,
                **kwargs,
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Structural part ID of the porous medium
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def thicknss(self) -> typing.Optional[float]:
        """Get or set the Thickness of the medium
        """ # nopep8
        return self._cards[0].get_value("thicknss")

    @thicknss.setter
    def thicknss(self, value: float) -> None:
        """Set the thicknss property."""
        self._cards[0].set_value("thicknss", value)

    @property
    def eqid(self) -> typing.Optional[float]:
        """Get or set the ID of the equation used to model the porous medium (see *DUALCESE_DARCY-FORCHHEIMER_EQ)
        """ # nopep8
        return self._cards[0].get_value("eqid")

    @eqid.setter
    def eqid(self, value: float) -> None:
        """Set the eqid property."""
        self._cards[0].set_value("eqid", value)

    @property
    def pid_link(self) -> typing.Optional[KeywordBase]:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

