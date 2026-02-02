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

"""Module providing the InitialDetonation class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.node.node import Node

_INITIALDETONATION_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("x", float, 10, 10, 0.0),
    FieldSchema("y", float, 20, 10, 0.0),
    FieldSchema("z", float, 30, 10, 0.0),
    FieldSchema("lt", float, 40, 10, 0.0),
    FieldSchema("unused", int, 50, 10, None),
    FieldSchema("mmgset", int, 60, 10, None),
)

_INITIALDETONATION_CARD1 = (
    FieldSchema("peak", float, 0, 10, None),
    FieldSchema("decay", float, 10, 10, None),
    FieldSchema("xs", float, 20, 10, 0.0),
    FieldSchema("ys", float, 30, 10, 0.0),
    FieldSchema("zs", float, 40, 10, 0.0),
    FieldSchema("nid", int, 50, 10, 0),
)

class InitialDetonation(KeywordBase):
    """DYNA INITIAL_DETONATION keyword"""

    keyword = "INITIAL"
    subkeyword = "DETONATION"
    _link_fields = {
        "nid": LinkType.NODE,
        "pid": LinkType.PART,
    }

    def __init__(self, **kwargs):
        """Initialize the InitialDetonation class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _INITIALDETONATION_CARD0,
                **kwargs,
            ),            Card.from_field_schemas_with_defaults(
                _INITIALDETONATION_CARD1,
                **kwargs,
            ),        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the Part ID of the high explosive to be lit, except in the case where the high explosive is modeled using an ALE formulation, in which case PID is the part ID of the mesh where the high explosive material to be lit initially resides.  However, two other options are available:
        EQ.-1:	an acoustic boundary, also, *BOUNDARY_USA_SURFACE,
        EQ.0:	all high explosive materials are considered.
        LT.-1:    |PID| is the ID of a part set (*SET_PART)
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def x(self) -> float:
        """Get or set the x-coordinate of detonation point.
        """ # nopep8
        return self._cards[0].get_value("x")

    @x.setter
    def x(self, value: float) -> None:
        """Set the x property."""
        self._cards[0].set_value("x", value)

    @property
    def y(self) -> float:
        """Get or set the y-coordinate of detonation point.
        """ # nopep8
        return self._cards[0].get_value("y")

    @y.setter
    def y(self, value: float) -> None:
        """Set the y property."""
        self._cards[0].set_value("y", value)

    @property
    def z(self) -> float:
        """Get or set the z-coordinate of detonation point.
        """ # nopep8
        return self._cards[0].get_value("z")

    @z.setter
    def z(self, value: float) -> None:
        """Set the z property."""
        self._cards[0].set_value("z", value)

    @property
    def lt(self) -> float:
        """Get or set the Lighting time for detonation point (ignored for accustic boundary).
        """ # nopep8
        return self._cards[0].get_value("lt")

    @lt.setter
    def lt(self, value: float) -> None:
        """Set the lt property."""
        self._cards[0].set_value("lt", value)

    @property
    def mmgset(self) -> typing.Optional[int]:
        """Get or set the ID of *SET_MULTI-MATERIAL_GROUP_LIST selecting the explosive ALE groups to be lit in the mesh defined by PID.
        """ # nopep8
        return self._cards[0].get_value("mmgset")

    @mmgset.setter
    def mmgset(self, value: int) -> None:
        """Set the mmgset property."""
        self._cards[0].set_value("mmgset", value)

    @property
    def peak(self) -> typing.Optional[float]:
        """Get or set the Peak pressure,po ,of incident pressure pulse.
        """ # nopep8
        return self._cards[1].get_value("peak")

    @peak.setter
    def peak(self, value: float) -> None:
        """Set the peak property."""
        self._cards[1].set_value("peak", value)

    @property
    def decay(self) -> typing.Optional[float]:
        """Get or set the Decay constant, tau.
        """ # nopep8
        return self._cards[1].get_value("decay")

    @decay.setter
    def decay(self, value: float) -> None:
        """Set the decay property."""
        self._cards[1].set_value("decay", value)

    @property
    def xs(self) -> float:
        """Get or set the x-coordinate of standoff point.
        """ # nopep8
        return self._cards[1].get_value("xs")

    @xs.setter
    def xs(self, value: float) -> None:
        """Set the xs property."""
        self._cards[1].set_value("xs", value)

    @property
    def ys(self) -> float:
        """Get or set the y-coordinate of standoff point.
        """ # nopep8
        return self._cards[1].get_value("ys")

    @ys.setter
    def ys(self, value: float) -> None:
        """Set the ys property."""
        self._cards[1].set_value("ys", value)

    @property
    def zs(self) -> float:
        """Get or set the z-coordinate of standoff point.
        """ # nopep8
        return self._cards[1].get_value("zs")

    @zs.setter
    def zs(self, value: float) -> None:
        """Set the zs property."""
        self._cards[1].set_value("zs", value)

    @property
    def nid(self) -> int:
        """Get or set the Reference node ID near structure.
        """ # nopep8
        return self._cards[1].get_value("nid")

    @nid.setter
    def nid(self, value: int) -> None:
        """Set the nid property."""
        self._cards[1].set_value("nid", value)

    @property
    def nid_link(self) -> KeywordBase:
        """Get the NODE keyword containing the given nid."""
        return self._get_link_by_attr("NODE", "nid", self.nid, "parts")

    @property
    def pid_link(self) -> KeywordBase:
        """Get the PART keyword containing the given pid."""
        return self._get_link_by_attr("PART", "pid", self.pid, "parts")

