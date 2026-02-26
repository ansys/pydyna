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

"""Module providing the EmEpEkg class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_EMEPEKG_CARD0 = (
    FieldSchema("ekgid", int, 0, 10, None),
    FieldSchema("psid", float, 10, 10, None),
)

class EmEpEkg(KeywordBase):
    """DYNA EM_EP_EKG keyword"""

    keyword = "EM"
    subkeyword = "EP_EKG"

    def __init__(self, **kwargs):
        """Initialize the EmEpEkg class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _EMEPEKG_CARD0,
                **kwargs,
            ),        ]
    @property
    def ekgid(self) -> typing.Optional[int]:
        """Get or set the Id of the EKG computation.
        """ # nopep8
        return self._cards[0].get_value("ekgid")

    @ekgid.setter
    def ekgid(self, value: int) -> None:
        """Set the ekgid property."""
        self._cards[0].set_value("ekgid", value)

    @property
    def psid(self) -> typing.Optional[float]:
        """Get or set the Point set id containing the list of virtual points on which the pseudo-EKGs are computed
        """ # nopep8
        return self._cards[0].get_value("psid")

    @psid.setter
    def psid(self, value: float) -> None:
        """Set the psid property."""
        self._cards[0].set_value("psid", value)

