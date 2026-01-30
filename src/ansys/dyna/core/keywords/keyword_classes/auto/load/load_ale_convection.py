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

"""Module providing the LoadAleConvection class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType

_LOADALECONVECTION_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("heading", str, 10, 70, None),
)

_LOADALECONVECTION_CARD1 = (
    FieldSchema("lagpid", int, 0, 10, None),
    FieldSchema("lagt", float, 10, 10, None),
    FieldSchema("lagcp", float, 20, 10, None),
    FieldSchema("h", float, 30, 10, None),
    FieldSchema("lagmas", float, 40, 10, None),
)

class LoadAleConvection(KeywordBase):
    """DYNA LOAD_ALE_CONVECTION keyword"""

    keyword = "LOAD"
    subkeyword = "ALE_CONVECTION"
    _link_fields = {
        "lagpid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the LoadAleConvection class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADALECONVECTION_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADALECONVECTION_CARD1,
                **kwargs,
            ),        ]
    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the loading ID
        """ # nopep8
        return self._cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[0].set_value("id", value)

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the A description of the loading.
        """ # nopep8
        return self._cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        """Set the heading property."""
        self._cards[0].set_value("heading", value)

    @property
    def lagpid(self) -> typing.Optional[int]:
        """Get or set the Lagrangian structure PID from a corresponding coupling card which receives the thermal energy in the convection heat transfer
        """ # nopep8
        return self._cards[1].get_value("lagpid")

    @lagpid.setter
    def lagpid(self, value: int) -> None:
        """Set the lagpid property."""
        self._cards[1].set_value("lagpid", value)

    @property
    def lagt(self) -> typing.Optional[float]:
        """Get or set the Initial temperature of this Lagrangian structure part.
        """ # nopep8
        return self._cards[1].get_value("lagt")

    @lagt.setter
    def lagt(self, value: float) -> None:
        """Set the lagt property."""
        self._cards[1].set_value("lagt", value)

    @property
    def lagcp(self) -> typing.Optional[float]:
        """Get or set the Constant-pressure heat capacity of this Lagrangian structure part.  It has a per-mass unit (for example, J/[kg*K]).
        """ # nopep8
        return self._cards[1].get_value("lagcp")

    @lagcp.setter
    def lagcp(self, value: float) -> None:
        """Set the lagcp property."""
        self._cards[1].set_value("lagcp", value)

    @property
    def h(self) -> typing.Optional[float]:
        """Get or set the Convection heat transfer coefficient on this Lagrangian structure part surface.  It is the amount of energy (J) transferred per unit area, per time, and per temperature difference
        """ # nopep8
        return self._cards[1].get_value("h")

    @h.setter
    def h(self, value: float) -> None:
        """Set the h property."""
        self._cards[1].set_value("h", value)

    @property
    def lagmas(self) -> typing.Optional[float]:
        """Get or set the The mass of the Lagrangian structure part receiving the thermal energy.  This is in absolute mass unit
        """ # nopep8
        return self._cards[1].get_value("lagmas")

    @lagmas.setter
    def lagmas(self, value: float) -> None:
        """Set the lagmas property."""
        self._cards[1].set_value("lagmas", value)

    @property
    def lagpid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given lagpid."""
        return self._get_link_by_attr("PART", "pid", self.lagpid, "parts")

