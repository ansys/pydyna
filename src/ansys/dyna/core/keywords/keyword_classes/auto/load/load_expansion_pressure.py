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

"""Module providing the LoadExpansionPressure class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase

_LOADEXPANSIONPRESSURE_CARD0 = (
    FieldSchema("ssid", int, 0, 10, None),
    FieldSchema("lcid", int, 10, 10, None),
    FieldSchema("sf", float, 20, 10, 1.0),
    FieldSchema("at", float, 30, 10, 0.0),
)

_LOADEXPANSIONPRESSURE_CARD1 = (
    FieldSchema("nsid", int, 0, 10, None),
    FieldSchema("xn", float, 10, 10, None),
    FieldSchema("yn", float, 20, 10, None),
    FieldSchema("zn", float, 30, 10, None),
)

class LoadExpansionPressure(KeywordBase):
    """DYNA LOAD_EXPANSION_PRESSURE keyword"""

    keyword = "LOAD"
    subkeyword = "EXPANSION_PRESSURE"

    def __init__(self, **kwargs):
        """Initialize the LoadExpansionPressure class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADEXPANSIONPRESSURE_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _LOADEXPANSIONPRESSURE_CARD1,
                **kwargs,
            ),        ]
    @property
    def ssid(self) -> typing.Optional[int]:
        """Get or set the Segment set ID which specifies the interior of the chamber. As the edge moves, the pressure is applied or could be applied to these segments.
        """ # nopep8
        return self._cards[0].get_value("ssid")

    @ssid.setter
    def ssid(self, value: int) -> None:
        """Set the ssid property."""
        self._cards[0].set_value("ssid", value)

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID that defines the pressure as a function of time.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Load curve scale factor
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[0].set_value("sf", value)

    @property
    def at(self) -> float:
        """Get or set the Activation time which is the time at which the pressure begins to be applied. Before this time, pressure will not be applied to the chamber.
        """ # nopep8
        return self._cards[0].get_value("at")

    @at.setter
    def at(self, value: float) -> None:
        """Set the at property."""
        self._cards[0].set_value("at", value)

    @property
    def nsid(self) -> typing.Optional[int]:
        """Get or set the Node set ID that defines the moving edge/plane of the dynamic chamber.  Note that this node set must include at least 3 nodes to define a plane.
        """ # nopep8
        return self._cards[1].get_value("nsid")

    @nsid.setter
    def nsid(self, value: int) -> None:
        """Set the nsid property."""
        self._cards[1].set_value("nsid", value)

    @property
    def xn(self) -> typing.Optional[float]:
        """Get or set the X component of the initial outward normal of the moving plane/edge.
        """ # nopep8
        return self._cards[1].get_value("xn")

    @xn.setter
    def xn(self, value: float) -> None:
        """Set the xn property."""
        self._cards[1].set_value("xn", value)

    @property
    def yn(self) -> typing.Optional[float]:
        """Get or set the Y component of the initial outward normal of the moving plane/edge
        """ # nopep8
        return self._cards[1].get_value("yn")

    @yn.setter
    def yn(self, value: float) -> None:
        """Set the yn property."""
        self._cards[1].set_value("yn", value)

    @property
    def zn(self) -> typing.Optional[float]:
        """Get or set the Z component of the initial outward normal of the moving plane/edge .
        """ # nopep8
        return self._cards[1].get_value("zn")

    @zn.setter
    def zn(self, value: float) -> None:
        """Set the zn property."""
        self._cards[1].set_value("zn", value)

