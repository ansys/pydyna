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

"""Module providing the InitialIiEosAle class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_INITIALIIEOSALE_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("type", int, 10, 10, None),
    FieldSchema("mmg", int, 20, 10, None),
    FieldSchema("e0", float, 30, 10, 0.0),
    FieldSchema("v0", float, 40, 10, 0.0),
    FieldSchema("p0", float, 50, 10, 0.0),
)

class InitialIiEosAle(KeywordBase):
    """DYNA INITIAL_II_EOS_ALE keyword"""

    keyword = "INITIAL"
    subkeyword = "II_EOS_ALE"

    def __init__(self, **kwargs):
        """Initialize the InitialIiEosAle class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALIIEOSALE_CARD0,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the Part ID or part set ID or element set ID.
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def type(self) -> typing.Optional[int]:
        """Get or set the Type of “ID”:
        EQ.0:	part set ID.
        EQ.1 : part ID.
        EQ.2 : element set ID(*SET_‌BEAM in 1D, *SET_‌SHELL in 2D, *SET_‌SOLID in 3D).
        """ # nopep8
        return self._cards[0].get_value("type")

    @type.setter
    def type(self, value: int) -> None:
        """Set the type property."""
        self._cards[0].set_value("type", value)

    @property
    def mmg(self) -> typing.Optional[int]:
        """Get or set the Specifies the multi-material group.GT.0:	ALE multi - material group.LT.0 : Set ID of ALE multi - material groups defined in * SET_‌MULTI - MATERIAL_‌GROUP.
        """ # nopep8
        return self._cards[0].get_value("mmg")

    @mmg.setter
    def mmg(self, value: int) -> None:
        """Set the mmg property."""
        self._cards[0].set_value("mmg", value)

    @property
    def e0(self) -> float:
        """Get or set the Initial internal energy per reference volume unit (as defined in *EOS).  See Remark 1
        """ # nopep8
        return self._cards[0].get_value("e0")

    @e0.setter
    def e0(self, value: float) -> None:
        """Set the e0 property."""
        self._cards[0].set_value("e0", value)

    @property
    def v0(self) -> float:
        """Get or set the Initial relative volume (as defined in *EOS)
        """ # nopep8
        return self._cards[0].get_value("v0")

    @v0.setter
    def v0(self, value: float) -> None:
        """Set the v0 property."""
        self._cards[0].set_value("v0", value)

    @property
    def p0(self) -> float:
        """Get or set the Initial pressure
        """ # nopep8
        return self._cards[0].get_value("p0")

    @p0.setter
    def p0(self, value: float) -> None:
        """Set the p0 property."""
        self._cards[0].set_value("p0", value)

