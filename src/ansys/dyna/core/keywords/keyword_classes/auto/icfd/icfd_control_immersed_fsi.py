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

"""Module providing the IcfdControlImmersedFsi class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_ICFDCONTROLIMMERSEDFSI_CARD0 = (
    FieldSchema("unused", int, 0, 10, None),
    FieldSchema("bt", int, 10, 10, None),
    FieldSchema("dt", int, 20, 10, 9999999999999999583119736832),
)

class IcfdControlImmersedFsi(KeywordBase):
    """DYNA ICFD_CONTROL_IMMERSED_FSI keyword"""

    keyword = "ICFD"
    subkeyword = "CONTROL_IMMERSED_FSI"

    def __init__(self, **kwargs):
        """Initialize the IcfdControlImmersedFsi class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDCONTROLIMMERSEDFSI_CARD0,
                **kwargs,
            ),
        ]
    @property
    def bt(self) -> typing.Optional[int]:
        """Get or set the Birth time for the FSI coupling. Before BT, the fluid solver does not pass any loads to the structure, but it receives displacements from the structural solver.
        """ # nopep8
        return self._cards[0].get_value("bt")

    @bt.setter
    def bt(self, value: int) -> None:
        """Set the bt property."""
        self._cards[0].set_value("bt", value)

    @property
    def dt(self) -> int:
        """Get or set the Death time for the FSI coupling. After DT, the fluid solver does not transfer any loads to the structural solver, but the fluid continues to deform with the structure.
        """ # nopep8
        return self._cards[0].get_value("dt")

    @dt.setter
    def dt(self, value: int) -> None:
        """Set the dt property."""
        self._cards[0].set_value("dt", value)

