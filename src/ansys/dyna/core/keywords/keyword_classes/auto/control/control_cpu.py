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

"""Module providing the ControlCpu class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_CONTROLCPU_CARD0 = (
    FieldSchema("cputim", float, 0, 10, 0.0),
    FieldSchema("iglst", int, 0, 10, 0),
)

class ControlCpu(KeywordBase):
    """DYNA CONTROL_CPU keyword"""

    keyword = "CONTROL"
    subkeyword = "CPU"

    def __init__(self, **kwargs):
        """Initialize the ControlCpu class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _CONTROLCPU_CARD0,
                **kwargs,
            ),        ]
    @property
    def cputim(self) -> float:
        """Get or set the Seconds of cpu time:
        EQ:0.0 no cpu time limit set
        """ # nopep8
        return self._cards[0].get_value("cputim")

    @cputim.setter
    def cputim(self, value: float) -> None:
        """Set the cputim property."""
        self._cards[0].set_value("cputim", value)

    @property
    def iglst(self) -> int:
        """Get or set the Flag for outputting CPU and elapsed times in the glstat file:
        EQ.0:	no
        EQ.1 : yes
        """ # nopep8
        return self._cards[0].get_value("iglst")

    @iglst.setter
    def iglst(self, value: int) -> None:
        """Set the iglst property."""
        if value not in [0, 1, None]:
            raise Exception("""iglst must be `None` or one of {0,1}.""")
        self._cards[0].set_value("iglst", value)

