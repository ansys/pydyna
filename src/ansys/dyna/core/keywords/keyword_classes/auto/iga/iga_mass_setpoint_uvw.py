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

"""Module providing the IgaMassSetpointUvw class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_IGAMASSSETPOINTUVW_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("tmass", float, 10, 10, 0.0),
    FieldSchema("dmass", float, 20, 10, 0.0),
)

class IgaMassSetpointUvw(KeywordBase):
    """DYNA IGA_MASS_SETPOINT_UVW keyword"""

    keyword = "IGA"
    subkeyword = "MASS_SETPOINT_UVW"

    def __init__(self, **kwargs):
        """Initialize the IgaMassSetpointUvw class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _IGAMASSSETPOINTUVW_CARD0,
                **kwargs,
            ),
        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Parametric point uvw ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def tmass(self) -> float:
        """Get or set the Mass added to IGA entity. See Remark 1
        """ # nopep8
        return self._cards[0].get_value("tmass")

    @tmass.setter
    def tmass(self, value: float) -> None:
        """Set the tmass property."""
        self._cards[0].set_value("tmass", value)

    @property
    def dmass(self) -> float:
        """Get or set the Uniformly distributed mass added to IGA entity. See Remark 1
        """ # nopep8
        return self._cards[0].get_value("dmass")

    @dmass.setter
    def dmass(self, value: float) -> None:
        """Set the dmass property."""
        self._cards[0].set_value("dmass", value)

