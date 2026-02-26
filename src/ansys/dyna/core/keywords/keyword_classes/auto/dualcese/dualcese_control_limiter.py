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

"""Module providing the DualceseControlLimiter class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_DUALCESECONTROLLIMITER_CARD0 = (
    FieldSchema("idlmt", int, 0, 10, 0),
    FieldSchema("alfa", float, 10, 10, 0.0),
    FieldSchema("beta", float, 20, 10, 0.0),
    FieldSchema("epsr", float, 30, 10, 0.0),
)

class DualceseControlLimiter(KeywordBase):
    """DYNA DUALCESE_CONTROL_LIMITER keyword"""

    keyword = "DUALCESE"
    subkeyword = "CONTROL_LIMITER"

    def __init__(self, **kwargs):
        """Initialize the DualceseControlLimiter class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _DUALCESECONTROLLIMITER_CARD0,
                **kwargs,
            ),        ]
    @property
    def idlmt(self) -> int:
        """Get or set the Set the stability limiter option (See dual CESE theory manual):
        EQ.0:	limiter format 1 (Re - weighting).
        EQ.1 : limiter format 2 (Relaxing).
        """ # nopep8
        return self._cards[0].get_value("idlmt")

    @idlmt.setter
    def idlmt(self, value: int) -> None:
        """Set the idlmt property."""
        if value not in [0, 1, None]:
            raise Exception("""idlmt must be `None` or one of {0,1}.""")
        self._cards[0].set_value("idlmt", value)

    @property
    def alfa(self) -> float:
        """Get or set the Re-weighting coefficient (See dual CESE theory manual)
        """ # nopep8
        return self._cards[0].get_value("alfa")

    @alfa.setter
    def alfa(self, value: float) -> None:
        """Set the alfa property."""
        self._cards[0].set_value("alfa", value)

    @property
    def beta(self) -> float:
        """Get or set the Numerical viscosity control coefficient (See dual CESE theory manual)
        """ # nopep8
        return self._cards[0].get_value("beta")

    @beta.setter
    def beta(self, value: float) -> None:
        """Set the beta property."""
        self._cards[0].set_value("beta", value)

    @property
    def epsr(self) -> float:
        """Get or set the Stability control coefficient (See dual CESE theory manual)
        """ # nopep8
        return self._cards[0].get_value("epsr")

    @epsr.setter
    def epsr(self, value: float) -> None:
        """Set the epsr property."""
        self._cards[0].set_value("epsr", value)

