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

"""Module providing the IcfdControlBackflow class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDCONTROLBACKFLOW_CARD0 = (
    FieldSchema("bfor", int, 0, 10, 0),
    FieldSchema("sf", float, 10, 10, 1.0),
)

class IcfdControlBackflow(KeywordBase):
    """DYNA ICFD_CONTROL_BACKFLOW keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_BACKFLOW"

    def __init__(self, **kwargs):
        """Initialize the IcfdControlBackflow class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLBACKFLOW_CARD0,
                **kwargs,
            ),
        ]
    @property
    def bfor(self) -> int:
        """Get or set the Set the backflow stabilization formulation:
        EQ.0: Default stabilization dependent on spatial velocity gradients.
        EQ.1: The stabilization adds a temporal velocity gradient which could be necessary for added stabilization.
        """ # nopep8
        return self._cards[0].get_value("bfor")

    @bfor.setter
    def bfor(self, value: int) -> None:
        """Set the bfor property."""
        if value not in [0, 1, None]:
            raise Exception("""bfor must be `None` or one of {0,1}.""")
        self._cards[0].set_value("bfor", value)

    @property
    def sf(self) -> float:
        """Get or set the Scale factor to increase the stabilization if needed. A very small value larger than zero minimizes the effect.
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[0].set_value("sf", value)

