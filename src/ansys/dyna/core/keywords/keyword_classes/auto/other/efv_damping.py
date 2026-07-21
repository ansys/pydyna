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

"""Module providing the EfvDamping class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVDAMPING_CARD0 = (
    FieldSchema("qquad", float, 0, 10, 1.0),
    FieldSchema("qlin", float, 10, 10, 0.2),
    FieldSchema("noqlexp", int, 20, 10, 0),
    FieldSchema("unused", int, 30, 10, None),
    FieldSchema("unused", int, 40, 10, None),
    FieldSchema("relax", float, 50, 10, None),
)

class EfvDamping(KeywordBase):
    """DYNA EFV_DAMPING keyword"""

    keyword = "EFV"
    subkeyword = "DAMPING"

    def __init__(self, **kwargs):
        """Initialize the EfvDamping class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVDAMPING_CARD0,
                **kwargs,
            ),
        ]
    @property
    def qquad(self) -> float:
        """Get or set the Quadratic Bulk Viscosity
        """ # nopep8
        return self._cards[0].get_value("qquad")

    @qquad.setter
    def qquad(self, value: float) -> None:
        """Set the qquad property."""
        self._cards[0].set_value("qquad", value)

    @property
    def qlin(self) -> float:
        """Get or set the Linear Bulk Viscosity
        """ # nopep8
        return self._cards[0].get_value("qlin")

    @qlin.setter
    def qlin(self, value: float) -> None:
        """Set the qlin property."""
        self._cards[0].set_value("qlin", value)

    @property
    def noqlexp(self) -> int:
        """Get or set the Linear Bulk Viscosity in expansion:
        EQ.0: Applied by default
        EQ.1: Reset to 0
        """ # nopep8
        return self._cards[0].get_value("noqlexp")

    @noqlexp.setter
    def noqlexp(self, value: int) -> None:
        """Set the noqlexp property."""
        if value not in [0, 1, None]:
            raise Exception("""noqlexp must be `None` or one of {0,1}.""")
        self._cards[0].set_value("noqlexp", value)

    @property
    def relax(self) -> typing.Optional[float]:
        """Get or set the Static damping coefficient. It activates the dynamic relaxation like IDRFLG = 1 in *CONTROL_DYNAMIC_RELAXATION. The following expression relates RELAX to DRFCTR on *CONTROL_DYNAMIC_RELAXATION:
        RELAX = 1 - t x DRFCTR
        """ # nopep8
        return self._cards[0].get_value("relax")

    @relax.setter
    def relax(self, value: float) -> None:
        """Set the relax property."""
        self._cards[0].set_value("relax", value)

