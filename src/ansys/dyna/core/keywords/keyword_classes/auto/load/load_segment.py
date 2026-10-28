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

"""Module providing the LoadSegment class."""
import typing
from ansys.dyna.core.lib.card import Card, Field, Flag
from ansys.dyna.core.lib.field_schema import FieldSchema
from ansys.dyna.core.lib.card_set import CardSet, ensure_card_set_properties
from ansys.dyna.core.lib.cards import Cards
from ansys.dyna.core.lib.option_card import OptionCardSet, OptionSpec
from ansys.dyna.core.lib.keyword_base import KeywordBase

_LOADSEGMENTCARDSET_CARD0 = (
    FieldSchema("lcid", int, 0, 10, None),
    FieldSchema("sf", float, 10, 10, 1.0),
    FieldSchema("at", float, 20, 10, 0.0),
    FieldSchema("n1", int, 30, 10, None),
    FieldSchema("n2", int, 40, 10, None),
    FieldSchema("n3", int, 50, 10, None),
    FieldSchema("n4", int, 60, 10, None),
    FieldSchema("n5", int, 70, 10, None),
)

_LOADSEGMENTCARDSET_CARD1 = (
    FieldSchema("n6", int, 0, 10, None),
    FieldSchema("n7", int, 10, 10, None),
    FieldSchema("n8", int, 20, 10, None),
)

class LoadSegmentCardSet(Cards):
    """ CardSet."""

    def __init__(self, **kwargs):
        """Initialize the LoadSegmentCardSet CardSet."""
        super().__init__(kwargs["keyword"])
        self._parent = kwargs["parent"]
        kwargs["parent"] = self
        self._cards = [
            Card.from_field_schemas_with_defaults(
                _LOADSEGMENTCARDSET_CARD0,
                **kwargs,
            ),
            Card.from_field_schemas_with_defaults(
                _LOADSEGMENTCARDSET_CARD1,
                active_func=lambda: self.n5,
                **kwargs,
            ),
        ]

    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the Load curve ID (see *DEFINE_CURVE) or function ID (see *DEFINE_FUNCTION). For a load curve ID, the load curve gives the pressure as a function of time.
        For a function ID, the function for pressure has 10 arguments: time, the 3 current coordinates, the 3 reference coordinates,
        and the 3 velocities; that is, f(t,x,y,z,x0,y0,z0,vx,vy,vz). See Remark 1.
        """ # nopep8
        return self._cards[0].get_value("lcid")

    @lcid.setter
    def lcid(self, value: int) -> None:
        """Set the lcid property."""
        self._cards[0].set_value("lcid", value)

    @property
    def sf(self) -> float:
        """Get or set the Load curve scale factor.
        """ # nopep8
        return self._cards[0].get_value("sf")

    @sf.setter
    def sf(self, value: float) -> None:
        """Set the sf property."""
        self._cards[0].set_value("sf", value)

    @property
    def at(self) -> float:
        """Get or set the Arrival time for pressure or birth time of pressure.
        """ # nopep8
        return self._cards[0].get_value("at")

    @at.setter
    def at(self, value: float) -> None:
        """Set the at property."""
        self._cards[0].set_value("at", value)

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the Node number of first Node.
        """ # nopep8
        return self._cards[0].get_value("n1")

    @n1.setter
    def n1(self, value: int) -> None:
        """Set the n1 property."""
        self._cards[0].set_value("n1", value)

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the Node number of second Node.
        """ # nopep8
        return self._cards[0].get_value("n2")

    @n2.setter
    def n2(self, value: int) -> None:
        """Set the n2 property."""
        self._cards[0].set_value("n2", value)

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the Node number of third node. Repeat N2 for two dimensional geometries.
        """ # nopep8
        return self._cards[0].get_value("n3")

    @n3.setter
    def n3(self, value: int) -> None:
        """Set the n3 property."""
        self._cards[0].set_value("n3", value)

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the Node number of fourth node. Repeat N2 for two dimensional geometries.
        """ # nopep8
        return self._cards[0].get_value("n4")

    @n4.setter
    def n4(self, value: int) -> None:
        """Set the n4 property."""
        self._cards[0].set_value("n4", value)

    @property
    def n5(self) -> typing.Optional[int]:
        """Get or set the Node number of fourth node. Repeat N2 for two dimensional geometries.
        """ # nopep8
        return self._cards[0].get_value("n5")

    @n5.setter
    def n5(self, value: int) -> None:
        """Set the n5 property."""
        self._cards[0].set_value("n5", value)

    @property
    def n6(self) -> typing.Optional[int]:
        """Get or set the Optional mid-side node ID located between nodes 2 and 3
        """ # nopep8
        return self._cards[1].get_value("n6")

    @n6.setter
    def n6(self, value: int) -> None:
        """Set the n6 property."""
        self._cards[1].set_value("n6", value)

    @property
    def n7(self) -> typing.Optional[int]:
        """Get or set the Optional mid-side node ID located between nodes 3 and 4
        """ # nopep8
        return self._cards[1].get_value("n7")

    @n7.setter
    def n7(self, value: int) -> None:
        """Set the n7 property."""
        self._cards[1].set_value("n7", value)

    @property
    def n8(self) -> typing.Optional[int]:
        """Get or set the Optional mid-side node ID located between nodes 4 and 1. Do not define for siz node quadratic surface segments
        """ # nopep8
        return self._cards[1].get_value("n8")

    @n8.setter
    def n8(self, value: int) -> None:
        """Set the n8 property."""
        self._cards[1].set_value("n8", value)

    @property
    def parent(self) -> KeywordBase:
        """Get the parent keyword."""
        return self._parent

_LOADSEGMENT_OPTION0_CARD0 = (
    FieldSchema("id", int, 0, 10, None),
    FieldSchema("heading", str, 10, 70, None),
)

class LoadSegment(KeywordBase):
    """DYNA LOAD_SEGMENT keyword"""

    keyword = "LOAD"
    subkeyword = "SEGMENT"
    _option_spec_list = [
        OptionSpec("ID", "pre/1", 1),
    ]

    def __init__(self, **kwargs):
        """Initialize the LoadSegment class."""
        super().__init__(**kwargs)
        kwargs["parent"] = self
        kwargs["keyword"] = self
        kwargs["parent"] = self
        self._cards = [
            CardSet(
                LoadSegmentCardSet,
                **kwargs
            ),
            OptionCardSet(
                option_spec = LoadSegment._option_spec_list[0],
                cards = [
                    Card.from_field_schemas_with_defaults(
                        _LOADSEGMENT_OPTION0_CARD0,
                        **kwargs,
                    ),
                ],
                **kwargs
            ),
        ]
    @property
    def lcid(self) -> typing.Optional[int]:
        """Get or set the lcid
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].lcid

    @lcid.setter
    def lcid(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].lcid = value

    @property
    def sf(self) -> float:
        """Get or set the sf
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].sf

    @sf.setter
    def sf(self, value: float) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].sf = value

    @property
    def at(self) -> float:
        """Get or set the at
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].at

    @at.setter
    def at(self, value: float) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].at = value

    @property
    def n1(self) -> typing.Optional[int]:
        """Get or set the n1
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].n1

    @n1.setter
    def n1(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].n1 = value

    @property
    def n2(self) -> typing.Optional[int]:
        """Get or set the n2
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].n2

    @n2.setter
    def n2(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].n2 = value

    @property
    def n3(self) -> typing.Optional[int]:
        """Get or set the n3
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].n3

    @n3.setter
    def n3(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].n3 = value

    @property
    def n4(self) -> typing.Optional[int]:
        """Get or set the n4
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].n4

    @n4.setter
    def n4(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].n4 = value

    @property
    def n5(self) -> typing.Optional[int]:
        """Get or set the n5
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].n5

    @n5.setter
    def n5(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].n5 = value

    @property
    def n6(self) -> typing.Optional[int]:
        """Get or set the n6
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].n6

    @n6.setter
    def n6(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].n6 = value

    @property
    def n7(self) -> typing.Optional[int]:
        """Get or set the n7
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].n7

    @n7.setter
    def n7(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].n7 = value

    @property
    def n8(self) -> typing.Optional[int]:
        """Get or set the n8
        """ # nopep8
        ensure_card_set_properties(self, False)
        return self.sets[0].n8

    @n8.setter
    def n8(self, value: int) -> None:
        ensure_card_set_properties(self, True)
        self.sets[0].n8 = value

    @property
    def sets(self) -> typing.List[LoadSegmentCardSet]:
        """Gets the list of sets."""
        return self._cards[0].items()

    def add_set(self, **kwargs):
        """Adds a set."""
        self._cards[0].add_item(**kwargs)

    @property
    def id(self) -> typing.Optional[int]:
        """Get or set the loading ID
        """ # nopep8
        return self._cards[1].cards[0].get_value("id")

    @id.setter
    def id(self, value: int) -> None:
        """Set the id property."""
        self._cards[1].cards[0].set_value("id", value)

        if value:
            self.activate_option("ID")

    @property
    def heading(self) -> typing.Optional[str]:
        """Get or set the A description of the loading.
        """ # nopep8
        return self._cards[1].cards[0].get_value("heading")

    @heading.setter
    def heading(self, value: str) -> None:
        """Set the heading property."""
        self._cards[1].cards[0].set_value("heading", value)

        if value:
            self.activate_option("HEADING")

