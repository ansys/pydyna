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

"""Module providing the EfvFailureAddCrackSoftening class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EFVFAILUREADDCRACKSOFTENING_CARD0 = (
    FieldSchema("crckid", int, 0, 10, None),
    FieldSchema("frnrg", float, 10, 10, None),
    FieldSchema("comp", float, 20, 10, None),
    FieldSchema("rule", int, 30, 10, 2),
)

class EfvFailureAddCrackSoftening(KeywordBase):
    """DYNA EFV_FAILURE_ADD_CRACK_SOFTENING keyword"""

    keyword = "EFV"
    subkeyword = "FAILURE_ADD_CRACK_SOFTENING"

    def __init__(self, **kwargs):
        """Initialize the EfvFailureAddCrackSoftening class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EFVFAILUREADDCRACKSOFTENING_CARD0,
                **kwargs,
            ),
        ]
    @property
    def crckid(self) -> typing.Optional[int]:
        """Get or set the Crack softening ID. A unique number must be used.
        """ # nopep8
        return self._cards[0].get_value("crckid")

    @crckid.setter
    def crckid(self, value: int) -> None:
        """Set the crckid property."""
        self._cards[0].set_value("crckid", value)

    @property
    def frnrg(self) -> typing.Optional[float]:
        """Get or set the Fracture energy, G_f. See Remark 2.
        """ # nopep8
        return self._cards[0].get_value("frnrg")

    @frnrg.setter
    def frnrg(self, value: float) -> None:
        """Set the frnrg property."""
        self._cards[0].set_value("frnrg", value)

    @property
    def comp(self) -> typing.Optional[float]:
        """Get or set the Onset compression after failure
        """ # nopep8
        return self._cards[0].get_value("comp")

    @comp.setter
    def comp(self, value: float) -> None:
        """Set the comp property."""
        self._cards[0].set_value("comp", value)

    @property
    def rule(self) -> int:
        """Get or set the Flow rule (see Remark 3):
        EQ.1: Radial return
        EQ.2: No bulking
        EQ.3: Bulking(associative)
        """ # nopep8
        return self._cards[0].get_value("rule")

    @rule.setter
    def rule(self, value: int) -> None:
        """Set the rule property."""
        if value not in [2, 1, 3, None]:
            raise Exception("""rule must be `None` or one of {2,1,3}.""")
        self._cards[0].set_value("rule", value)

