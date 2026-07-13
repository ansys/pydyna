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

"""Module providing the IcfdBoundaryWindkessel class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.keyword_base import KeywordBase
from ansys.dyna.core.lib.keyword_base import LinkType
from ansys.dyna.core.keywords.keyword_classes.auto.define.define_curve import DefineCurve

_ICFDBOUNDARYWINDKESSEL_CARD0 = (
    FieldSchema("pid", int, 0, 10, None),
    FieldSchema("wtype", int, 10, 10, None),
    FieldSchema("r1", float, 20, 10, 0.0),
    FieldSchema("c1", float, 30, 10, 0.0),
    FieldSchema("r2", float, 40, 10, 0.0),
    FieldSchema("unused", int, 50, 10, None),
)

_ICFDBOUNDARYWINDKESSEL_CARD1 = (
    FieldSchema("p2lcid", int, 0, 10, None),
    FieldSchema("c2", float, 10, 10, 0.0),
    FieldSchema("r3", float, 20, 10, 0.0),
    FieldSchema("p0", float, 30, 10, 0.0),
    FieldSchema("p1", float, 40, 10, 0.0),
)

class IcfdBoundaryWindkessel(KeywordBase):
    """DYNA ICFD_BOUNDARY_WINDKESSEL keyword"""

    keyword = "ICFD"
    subkeyword = "BOUNDARY_WINDKESSEL"
    _link_fields = {
        "p2lcid": LinkType.DEFINE_CURVE,
    }

    def __init__(self, **kwargs):
        """Initialize the IcfdBoundaryWindkessel class."""
        super().__init__(**kwargs)
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _ICFDBOUNDARYWINDKESSEL_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _ICFDBOUNDARYWINDKESSEL_CARD1,
                **kwargs,
            ),
        ]
    @property
    def pid(self) -> typing.Optional[int]:
        """Get or set the PID for a fluid surface.
        """ # nopep8
        return self._cards[0].get_value("pid")

    @pid.setter
    def pid(self, value: int) -> None:
        """Set the pid property."""
        self._cards[0].set_value("pid", value)

    @property
    def wtype(self) -> typing.Optional[int]:
        """Get or set the Circuit type. See Remarks and Figures.
        """ # nopep8
        return self._cards[0].get_value("wtype")

    @wtype.setter
    def wtype(self, value: int) -> None:
        """Set the wtype property."""
        self._cards[0].set_value("wtype", value)

    @property
    def r1(self) -> float:
        """Get or set the Parameters (Resistances, inductances, capacities) for the different circuits.
        """ # nopep8
        return self._cards[0].get_value("r1")

    @r1.setter
    def r1(self, value: float) -> None:
        """Set the r1 property."""
        self._cards[0].set_value("r1", value)

    @property
    def c1(self) -> float:
        """Get or set the Parameters (Resistances, inductances, capacities) for the different circuits.
        """ # nopep8
        return self._cards[0].get_value("c1")

    @c1.setter
    def c1(self, value: float) -> None:
        """Set the c1 property."""
        self._cards[0].set_value("c1", value)

    @property
    def r2(self) -> float:
        """Get or set the Parameters (Resistances, inductances, capacities) for the different circuits.
        """ # nopep8
        return self._cards[0].get_value("r2")

    @r2.setter
    def r2(self, value: float) -> None:
        """Set the r2 property."""
        self._cards[0].set_value("r2", value)

    @property
    def p2lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID describing behavior of P2(t) function of time for CV type circuit.
        """ # nopep8
        return self._cards[1].get_value("p2lcid")

    @p2lcid.setter
    def p2lcid(self, value: int) -> None:
        """Set the p2lcid property."""
        self._cards[1].set_value("p2lcid", value)

    @property
    def c2(self) -> float:
        """Get or set the Parameters (Resistances, inductances, capacities) for the different circuits.
        """ # nopep8
        return self._cards[1].get_value("c2")

    @c2.setter
    def c2(self, value: float) -> None:
        """Set the c2 property."""
        self._cards[1].set_value("c2", value)

    @property
    def r3(self) -> float:
        """Get or set the Parameters (Resistances, inductances, capacities) for the different circuits.
        """ # nopep8
        return self._cards[1].get_value("r3")

    @r3.setter
    def r3(self, value: float) -> None:
        """Set the r3 property."""
        self._cards[1].set_value("r3", value)

    @property
    def p0(self) -> float:
        """Get or set the Initial pressures at circuit junctions when applicable.
        """ # nopep8
        return self._cards[1].get_value("p0")

    @p0.setter
    def p0(self, value: float) -> None:
        """Set the p0 property."""
        self._cards[1].set_value("p0", value)

    @property
    def p1(self) -> float:
        """Get or set the Initial pressures at circuit junctions when applicable.
        """ # nopep8
        return self._cards[1].get_value("p1")

    @p1.setter
    def p1(self, value: float) -> None:
        """Set the p1 property."""
        self._cards[1].set_value("p1", value)

    @property
    def p2lcid_link(self) -> typing.Optional[DefineCurve]:
        """Get the DefineCurve object for p2lcid."""
        if self.deck is None:
            return None
        for kwd in self.deck.get_kwds_by_full_type("DEFINE", "CURVE"):
            if kwd.lcid == self.p2lcid:
                return kwd
        return None

    @p2lcid_link.setter
    def p2lcid_link(self, value: DefineCurve) -> None:
        """Set the DefineCurve object for p2lcid."""
        self.p2lcid = value.lcid

